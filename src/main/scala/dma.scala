package dma

import Chisel._
import rocket.{CoreParameters, CoreBundle, TLBPTWIO, RoCCMaxTaggedMemXacts}
import uncore._

case object DMADataBits extends Field[Int]
case object DMAQueueDepth extends Field[Int]
case object DMAMaxXacts extends Field[Int]

trait DMAParameters extends UsesParameters {
  val dmaDataBits = params(DMADataBits)
  val dmaDataBytes = dmaDataBits / 8
  val dmaDataOffset = log2Up(dmaDataBytes)
  val dmaQueueDepth = params(DMAQueueDepth)
  val dmaMaxXacts = params(DMAMaxXacts)
  val dmaXactIdBits = log2Up(dmaMaxXacts)
}

abstract class DMAModule extends Module
  with DMAParameters with CoreParameters with TileLinkParameters

class TileLinkDMACommand extends CoreBundle {
  val src_start = UInt(width = paddrBits)
  val dst_start = UInt(width = paddrBits)
  val nbytes = UInt(width = paddrBits)
}

class TileLinkDMATx extends DMAModule {
  val io = new Bundle {
    val cmd = Decoupled(new TileLinkDMACommand).flip
    val dmem = new ClientUncachedTileLinkIO
    val dptw = new TLBPTWIO
    val net = new ClientUncachedTileLinkIO
    val phys = Bool(INPUT)
    val error = Bool(OUTPUT)
  }

  private val tlBlockOffset = tlBeatAddrBits + tlByteAddrBits
  private val tlBytesPerBlock = tlDataBeats * tlDataBytes
  private val blockPgIdxBits = pgIdxBits - tlBlockOffset
  private val blocksPerPage = (1 << blockPgIdxBits)

  val vpn = Reg(UInt(width = vpnBits))
  val page_idx = Reg(UInt(width = pgIdxBits))

  val src_block = Reg(UInt(width = tlBlockAddrBits))
  val dst_block = Reg(UInt(width = tlBlockAddrBits))
  val bytes_left = Reg(UInt(width = paddrBits))

  val beat_idx = Reg(UInt(width = tlBeatAddrBits))
  val write_half = Reg(Bool())
  val read_half = Reg(Bool())

  val offset = Reg(UInt(width = tlBlockOffset))
  val align = Reg(UInt(width = tlBlockOffset))
  val align_dir = Reg(Bool())

  val buffer = Vec.fill(2 * tlDataBeats) { Reg(Bits(width = tlDataBits)) }
  val read_base = Cat(read_half, beat_idx)
  val beat_data = Bits(width = tlDataBits)

  beat_data := buffer(read_base)
  when (align != UInt(0)) {
    val align_beatoff = align(tlBlockOffset - 1, tlByteAddrBits)
    val align_bitshift = Cat(align(tlByteAddrBits - 1, 0), UInt(0, 3))
    // shifting to the right
    when (align_dir) {
      when (align_bitshift === UInt(0)) {
        // take a beat from the left
        beat_data := buffer(read_base + align_beatoff)
      } .otherwise {
        // take the upper bits of the base beat
        // take the lower bits of the beat one above
        val lower_beat = buffer(read_base + align_beatoff)
        val upper_beat = buffer(read_base + align_beatoff + UInt(1))
        val lower_shifted = lower_beat >> align_bitshift
        val upper_shifted = upper_beat << (UInt(tlDataBits) - align_bitshift)
        val full_mask = Fill(tlDataBits, Bool(true))
        val lower_mask = full_mask >> align_bitshift
        val upper_mask = ~lower_mask
        beat_data := (lower_shifted & lower_mask) | (upper_shifted & upper_mask)
      }
    } .otherwise {
      when (align_bitshift === UInt(0)) {
        // take a beat from the right
        beat_data := buffer(read_base - align_beatoff)
      } .otherwise {
        // take the lower bits of the base beat
        // take the upper bits of the beat one below
        val upper_beat = buffer(read_base - align_beatoff)
        val lower_beat = buffer(read_base - align_beatoff - UInt(1))
        val upper_shifted = upper_beat << align_bitshift
        val lower_shifted = lower_beat >> (UInt(tlDataBits) - align_bitshift)
        val full_mask = Fill(tlDataBits, Bool(true))
        val upper_mask = full_mask << align_bitshift
        val lower_mask = ~upper_mask
        beat_data := (lower_shifted & lower_mask) | (upper_shifted & upper_mask)
      }
    }
  }

  val xact_finished = Vec.fill(dmaMaxXacts) { Reg(init = Bool(true)) }
  val xact_id = Reg(init = UInt(0, dmaXactIdBits))

  when (io.dmem.grant.fire()) {
    val index = Cat(write_half, beat_idx)
    buffer(index) := io.dmem.grant.bits.data
  }

  when (io.net.grant.fire()) {
    val recv_xact_id = io.net.grant.bits.client_xact_id(dmaXactIdBits - 1, 0)
    xact_finished(recv_xact_id) := Bool(true)
  }

  val first_block = Reg(Bool())

  val (s_idle :: s_prepare_read :: s_ptw_req :: s_ptw_resp ::
       s_dmem_acquire :: s_dmem_grant :: s_net_acquire :: s_wait_net ::
       s_wait_done :: Nil) = Enum(Bits(), 9)
  val state = Reg(init = s_idle)

  val full_block = (offset === UInt(0) && bytes_left > UInt(tlBytesPerBlock))
  val wmask = Vec.tabulate(tlDataBytes) { i =>
    val byte_index = Cat(beat_idx, UInt(i, tlByteAddrBits))
    byte_index >= offset && byte_index < bytes_left
  }.toBits

  val error = Reg(init = Bool(false))

  io.cmd.ready := (state === s_idle)
  io.error := error

  io.dmem.grant.ready := (state === s_dmem_grant)
  io.dmem.acquire.valid := (state === s_dmem_acquire)
  io.dmem.acquire.bits := GetBlock(
    client_xact_id = UInt(0),
    addr_block = src_block,
    alloc = Bool(true))
  debug(io.dmem.grant.bits.g_type)

  io.net.grant.ready := Bool(true)
  io.net.acquire.valid := (state === s_net_acquire)
  io.net.acquire.bits := Acquire(
    is_builtin_type = Bool(true),
    a_type = Acquire.putBlockType,
    client_xact_id = xact_id,
    addr_block = dst_block,
    addr_beat = beat_idx,
    data = beat_data,
    // we use the alloc bit to hint to the receiver that we are not sending
    // a full block, so the existing block should be read in before receiving
    union = Cat(wmask, !full_block))

  io.dptw.req.valid := (state === s_ptw_req)
  io.dptw.req.bits.addr := vpn
  io.dptw.req.bits.prv := Bits(0)
  io.dptw.req.bits.store := Bool(false)
  io.dptw.req.bits.fetch := Bool(true)

  switch (state) {
    is (s_idle) {
      when (io.cmd.valid) {
        when (io.phys) {
          src_block := io.cmd.bits.src_start(paddrBits - 1, tlBlockOffset)
          state := s_dmem_acquire
        } .otherwise {
          vpn := io.cmd.bits.src_start(paddrBits - 1, pgIdxBits)
          page_idx := io.cmd.bits.src_start(pgIdxBits - 1, 0)
          state := s_ptw_req
        }
        dst_block := io.cmd.bits.dst_start(paddrBits - 1, tlBlockOffset)

        val dst_off = io.cmd.bits.dst_start(tlBlockOffset - 1, 0)
        val src_off = io.cmd.bits.src_start(tlBlockOffset - 1, 0)

        when (dst_off < src_off) {
          align := src_off - dst_off
          align_dir := Bool(true)
        } .otherwise {
          align := dst_off - src_off
          align_dir := Bool(false)
        }
        // need to tack on the dst offset because
        // we will subtract #bytes in a block after transmission
        bytes_left  := io.cmd.bits.nbytes + dst_off
        offset      := dst_off
        write_half  := Bool(false)
        read_half   := Bool(false)
        first_block := Bool(true)
      }
    }
    is (s_ptw_req) {
      when (io.dptw.req.ready) {
        state := s_ptw_resp
      }
    }
    is (s_ptw_resp) {
      when (io.dptw.resp.valid) {
        when (io.dptw.resp.bits.error) {
          error := Bool(true)
          state := s_idle
        } .otherwise {
          val fullPhysAddr = Cat(io.dptw.resp.bits.pte.ppn, page_idx)
          src_block := fullPhysAddr(paddrBits - 1, tlBlockOffset)
          state := s_dmem_acquire
        }
      }
    }
    is (s_prepare_read) {
      val src_page_idx = src_block(blockPgIdxBits - 1, 0)
      when (align >= bytes_left) {
        // if there are enough bytes already in the buffer (the read shift)
        // we can just keep sending
        state := s_net_acquire
      } .elsewhen (!io.phys && src_page_idx === UInt(0)) {
        vpn := vpn + UInt(1)
        page_idx := UInt(0)
        state := s_ptw_req
      } .otherwise {
        state := s_dmem_acquire
      }
    }
    is (s_dmem_acquire) {
      when (io.dmem.acquire.ready) {
        beat_idx := UInt(0)
        state := s_dmem_grant
      }
    }
    is (s_dmem_grant) {
      when (io.dmem.grant.valid) {
        when (beat_idx === UInt(tlDataBeats - 1)) {
          val bytes_in_buffer = UInt(tlBytesPerBlock) - align
          val needs_more_data = align_dir && first_block &&
                                bytes_in_buffer < bytes_left
          when (needs_more_data) {
            src_block := src_block + UInt(1)
            state := s_prepare_read
          } .otherwise {
            state := s_wait_net
          }
          write_half := !write_half
          first_block := Bool(false)
        }
        beat_idx := beat_idx + UInt(1)
      }
    }
    is (s_wait_net) {
      when (xact_finished(xact_id)) {
        beat_idx := UInt(0)
        xact_finished(xact_id) := Bool(false)
        state := s_net_acquire
      }
    }
    is (s_net_acquire) {
      when (io.net.acquire.ready) {
        when (beat_idx === UInt(tlDataBeats - 1)) {
          dst_block := dst_block + UInt(1)
          src_block := src_block + UInt(1)
          offset := UInt(0)
          when (bytes_left <= UInt(tlBytesPerBlock)) {
            bytes_left := UInt(0)
            state := s_wait_done
          } .otherwise {
            state := s_prepare_read
            bytes_left := bytes_left - UInt(tlBytesPerBlock)
          }
          xact_id := xact_id + UInt(1)
          read_half := !read_half
        }
        beat_idx := beat_idx + UInt(1)
      }
    }
    is (s_wait_done) {
      // wait for all in-flight requests to finish before ending operation
      when (xact_finished.toBits.andR) {
        xact_id := UInt(0)
        state := s_idle
      }
    }
  }
}

class TileLinkDMARx extends DMAModule {
  val io = new Bundle {
    val net = new ClientUncachedTileLinkIO().flip
    val dmem = new ClientUncachedTileLinkIO
    val dptw = new TLBPTWIO
    val phys = Bool(INPUT)
    val idle = Bool(OUTPUT)
    val error = Bool(OUTPUT)
  }

  private val tlBlockOffset = tlBeatAddrBits + tlByteAddrBits
  private val blockPgIdxBits = pgIdxBits - tlBlockOffset

  val addr_block = Reg(init = UInt(0, tlBlockAddrBits))
  val buffer = Mem(Bits(width = tlDataBits), tlDataBeats, seqRead = true)
  val beat_idx = Reg(UInt(width = tlBeatAddrBits))
  val page_idx = Reg(UInt(width = blockPgIdxBits))
  val vpn = Reg(UInt(width = vpnBits))
  val net_xact_id = Reg(UInt(0, dmaXactIdBits))
  val net_acquire = io.net.acquire.bits
  val error = Reg(init = Bool(false))
  val direction = Reg(Bool())

  val (s_idle :: s_recv :: s_ack :: s_prepare_recv ::
       s_get_acquire :: s_get_grant :: s_put_acquire :: s_put_grant ::
       s_ptw_req :: s_ptw_resp :: Nil) = Enum(Bits(), 10)
  val state = Reg(init = s_idle)

  io.error := error
  io.idle := (state === s_idle)

  val net_type = Mux(direction, Grant.putAckType, Grant.getDataBlockType)

  io.net.acquire.ready := (state === s_recv)
  io.net.grant.valid := (state === s_ack)
  io.net.grant.bits := Grant(
    is_builtin_type = Bool(true),
    g_type = net_type,
    client_xact_id = net_xact_id,
    manager_xact_id = UInt(0),
    addr_beat = beat_idx,
    data = buffer(beat_idx))

  val dmem_type = Mux(state === s_get_acquire,
    Acquire.getBlockType, Acquire.putBlockType)
  val dmem_union = Cat(Mux(state === s_get_acquire,
    Cat(MT_Q, M_XRD), Acquire.fullWriteMask), Bool(true))

  io.dmem.acquire.valid := (state === s_get_acquire || state === s_put_acquire)
  io.dmem.acquire.bits := Acquire(
    is_builtin_type = Bool(true),
    a_type = dmem_type,
    client_xact_id = UInt(1),
    addr_block = addr_block,
    addr_beat = beat_idx,
    data = buffer(beat_idx),
    union = dmem_union)
  io.dmem.grant.ready := (state === s_get_grant || state === s_put_grant)
  debug(io.dmem.grant.bits.g_type)

  io.dptw.req.valid := (state === s_ptw_req)
  io.dptw.req.bits.addr := vpn
  io.dptw.req.bits.prv := Bits(0)
  io.dptw.req.bits.store := Bool(false)
  io.dptw.req.bits.fetch := Bool(true)

  switch (state) {
    is (s_idle) {
      when (io.net.acquire.valid) {
        val net_vpn = net_acquire.addr_block(tlBlockAddrBits - 1, blockPgIdxBits)
        val net_page_idx = net_acquire.addr_block(blockPgIdxBits - 1, 0)
        when (io.phys || vpn === net_vpn) {
          addr_block := Mux(io.phys,
            net_acquire.addr_block,
            Cat(addr_block(tlBlockAddrBits - 1, blockPgIdxBits), net_page_idx))
          state := s_prepare_recv
        } .otherwise {
          vpn := net_vpn
          page_idx := net_page_idx
          state := s_ptw_req
        }
        direction := (io.net.acquire.bits.a_type === Acquire.putBlockType)
        net_xact_id := net_acquire.client_xact_id
      }
    }
    is (s_prepare_recv) {
      beat_idx := UInt(0)
      when (direction && net_acquire.union(0).toBool) {
        // if alloc is requested, we need to read in to the buffer
        // before we start receiving
        state := s_get_acquire
      } .otherwise {
        state := s_recv
      }
    }
    is (s_ptw_req) {
      when (io.dptw.req.ready) {
        state := s_ptw_resp
      }
    }
    is (s_ptw_resp) {
      when (io.dptw.resp.valid) {
        when (io.dptw.resp.bits.error) {
          error := Bool(true)
          state := s_idle
        } .otherwise {
          addr_block := Cat(io.dptw.resp.bits.pte.ppn, page_idx)
          state := s_prepare_recv
        }
      }
    }
    is (s_get_acquire) {
      when (io.dmem.acquire.ready) {
        beat_idx := UInt(0)
        state := s_get_grant
      }
    }
    is (s_get_grant) {
      when (io.dmem.grant.valid) {
        buffer(beat_idx) := io.dmem.grant.bits.data
        when (beat_idx === UInt(tlDataBeats - 1)) {
          when (direction) {
            state := s_recv
          } .otherwise {
            state := s_ack
          }
        }
        beat_idx := beat_idx + UInt(1)
      }
    }
    is (s_recv) {
      when (io.net.acquire.valid) {
        when (direction) {
          buffer.write(beat_idx, net_acquire.data, net_acquire.full_wmask())
          when (beat_idx === UInt(tlDataBeats - 1)) {
            state := s_put_acquire
          }
          beat_idx := beat_idx + UInt(1)
        } .otherwise {
          state := s_get_acquire
        }
      }
    }
    is (s_ack) {
      when (io.net.grant.ready) {
        when (direction || beat_idx === UInt(tlDataBeats - 1)) {
          state := s_idle
        }
        beat_idx := beat_idx + UInt(1)
      }
    }
    is (s_put_acquire) {
      when (io.dmem.acquire.ready) {
        when (beat_idx === UInt(tlDataBeats - 1)) {
          state := s_put_grant
        }
        beat_idx := beat_idx + UInt(1)
      }
    }
    is (s_put_grant) {
      when (io.dmem.grant.valid) {
        state := s_ack
      }
    }
  }
}
