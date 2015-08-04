package dma

import Chisel._
import rocket.{CoreParameters, TLBPTWIO}
import uncore._

case object DMADataBits extends Field[Int]
case object DMAQueueDepth extends Field[Int]
case object DMAMaxXacts extends Field[Int]

trait DMAParameters extends UsesParameters {
  val dmaDataBits = params(DMADataBits)
  val dmaDataBytes = dmaDataBits / 8
  val dmaDataOffset = log2Up(dmaDataBytes)
  val dmaMaxXacts = params(DMAMaxXacts)
  val dmaXactIdBits = log2Up(dmaMaxXacts)
  val lnHeaderBits = params(LNHeaderBits)
}

abstract class DMAModule extends Module
  with DMAParameters with CoreParameters with TileLinkParameters

abstract class DMABundle extends Bundle
  with DMAParameters with CoreParameters with TileLinkParameters

object TxErrors {
  val noerror     = Bits("b00")
  val pageFault   = Bits("b01")
  val nack        = Bits("b10")
  val noRoute     = Bits("b11")
}

class TileLinkDMACommand extends DMABundle {
  val src_start = UInt(width = paddrBits)
  val dst_start = UInt(width = paddrBits)
  val nbytes = UInt(width = paddrBits)
  val header = new RemoteHeader
  val xact_id = UInt(width = dmaXactIdBits)
  val direction = Bool()
}

class TileLinkDMATx extends DMAModule {
  val io = new Bundle {
    val cmd = Decoupled(new TileLinkDMACommand).flip
    val dmem = new ClientUncachedTileLinkIO
    val dptw = new TLBPTWIO
    val net = new RemoteTileLinkIO
    val phys = Bool(INPUT)
    val error = TxErrors.noerror.cloneType.asOutput
    val route_error = Bool(INPUT)
  }

  private val tlBlockOffset = tlBeatAddrBits + tlByteAddrBits
  private val tlBytesPerBlock = tlDataBeats * tlDataBytes
  private val blockPgIdxBits = pgIdxBits - tlBlockOffset
  private val blocksPerPage = (1 << blockPgIdxBits)
  private val addrByteOff = tlMemoryOperandSizeBits + tlMemoryOpcodeBits + 1

  val vpn = Reg(UInt(width = vpnBits))
  val page_idx = Reg(UInt(width = pgIdxBits))

  val local_block = Reg(UInt(width = tlBlockAddrBits))
  val remote_block = Reg(UInt(width = tlBlockAddrBits))
  val bytes_left = Reg(UInt(width = paddrBits))
  val direction = Reg(Bool())

  val beat_idx = Reg(UInt(width = tlBeatAddrBits))
  val write_half = Reg(Bool())
  val read_half = Reg(Bool())

  val offset = Reg(UInt(width = tlBlockOffset))
  val align = Reg(UInt(width = tlBlockOffset))
  val align_dir = Reg(Bool())

  val write_buffer = Mem(Bits(width = tlDataBits), tlDataBeats, seqRead = true)
  val send_buffer = Vec.fill(2 * tlDataBeats) { Reg(Bits(width = tlDataBits)) }
  val read_base = Cat(read_half, beat_idx)
  val beat_data = Bits(width = tlDataBits)

  beat_data := send_buffer(read_base)
  when (align != UInt(0)) {
    val align_beatoff = align(tlBlockOffset - 1, tlByteAddrBits)
    val align_bitshift = Cat(align(tlByteAddrBits - 1, 0), UInt(0, 3))
    // shifting to the right
    when (align_dir) {
      when (align_bitshift === UInt(0)) {
        // take a beat from the left
        beat_data := send_buffer(read_base + align_beatoff)
      } .otherwise {
        // take the upper bits of the base beat
        // take the lower bits of the beat one above
        val lower_beat = send_buffer(read_base + align_beatoff)
        val upper_beat = send_buffer(read_base + align_beatoff + UInt(1))
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
        beat_data := send_buffer(read_base - align_beatoff)
      } .otherwise {
        // take the lower bits of the base beat
        // take the upper bits of the beat one below
        val upper_beat = send_buffer(read_base - align_beatoff)
        val lower_beat = send_buffer(read_base - align_beatoff - UInt(1))
        val upper_shifted = upper_beat << align_bitshift
        val lower_shifted = lower_beat >> (UInt(tlDataBits) - align_bitshift)
        val full_mask = Fill(tlDataBits, Bool(true))
        val upper_mask = full_mask << align_bitshift
        val lower_mask = ~upper_mask
        beat_data := (lower_shifted & lower_mask) | (upper_shifted & upper_mask)
      }
    }
  }

  val net_grant = io.net.grant.bits.payload

  val first_block = Reg(Bool())

  val (s_idle :: s_prepare_read :: s_ptw_req :: s_ptw_resp ::
       s_dmem_get_acquire :: s_dmem_get_grant ::
       s_net_put_acquire :: s_net_put_grant ::
       s_net_get_acquire :: s_net_get_grant ::
       s_dmem_put_acquire :: s_dmem_put_grant ::
       s_copy_data :: Nil) = Enum(Bits(), 13)
  val state = Reg(init = s_idle)

  val full_block = (offset === UInt(0) && bytes_left > UInt(tlBytesPerBlock))
  val wmask = Vec.tabulate(tlDataBytes) { i =>
    val byte_index = Cat(beat_idx, UInt(i, tlByteAddrBits))
    byte_index >= offset && byte_index < bytes_left
  }.toBits
  val full_wmask = FillInterleaved(8, wmask)

  val error = Reg(init = TxErrors.noerror)

  io.cmd.ready := (state === s_idle)
  io.error := error

  val get_union = Cat(MT_Q, M_XRD, Bool(true))
  val put_union = Cat(wmask, !full_block)

  val dmem_type = Mux(state === s_dmem_put_acquire,
    Acquire.putBlockType, Acquire.getBlockType)
  val dmem_union = Mux(state === s_dmem_put_acquire,
    Cat(Acquire.fullWriteMask, Bool(true)), get_union)

  val header = Reg(new RemoteHeader)
  val xact_id = Reg(UInt(width = dmaXactIdBits))

  io.dmem.grant.ready := (state === s_dmem_get_grant ||
                          state === s_dmem_put_grant)
  io.dmem.acquire.valid := (state === s_dmem_get_acquire ||
                            state === s_dmem_put_acquire)
  io.dmem.acquire.bits := Acquire(
    is_builtin_type = Bool(true),
    a_type = dmem_type,
    client_xact_id = xact_id,
    addr_block = local_block,
    addr_beat = beat_idx,
    data = write_buffer(beat_idx),
    union = dmem_union)
  debug(io.dmem.grant.bits.g_type)

  val net_type = MuxLookup(state, Acquire.getBlockType,
    (s_net_put_acquire, Acquire.putBlockType) ::
    (s_net_get_acquire, Acquire.getBlockType) :: Nil)

  // we use the alloc bit to hint to the receiver that we are not sending
  // a full block, so the existing block should be read in before receiving
  val net_union = MuxLookup(state, get_union,
    (s_net_put_acquire, put_union) ::
    (s_net_get_acquire, get_union) :: Nil)

  io.net.grant.ready := direction || (state === s_net_get_grant)
  io.net.acquire.valid := (state === s_net_put_acquire) ||
                          (state === s_net_get_acquire)
  io.net.acquire.bits.payload := Acquire(
    is_builtin_type = Bool(true),
    a_type = net_type,
    client_xact_id = UInt(0),
    addr_block = remote_block,
    addr_beat = beat_idx,
    data = beat_data,
    union = net_union)
  io.net.acquire.bits.header := header
  io.net.acquire.bits.last := (bytes_left <= UInt(tlBytesPerBlock))

  io.dptw.req.valid := (state === s_ptw_req)
  io.dptw.req.bits.addr := vpn
  io.dptw.req.bits.prv := Bits(0)
  io.dptw.req.bits.store := Bool(false)
  io.dptw.req.bits.fetch := Bool(true)

  switch (state) {
    is (s_idle) {
      when (io.cmd.valid) {
        val cmd_dir = io.cmd.bits.direction
        val src_start = io.cmd.bits.src_start
        val dst_start = io.cmd.bits.dst_start
        val local_start = Mux(cmd_dir, src_start, dst_start)
        val remote_start = Mux(cmd_dir, dst_start, src_start)

        when (io.phys) {
          beat_idx := UInt(0)
          local_block := local_start(paddrBits - 1, tlBlockOffset)
          state := Mux(cmd_dir, s_dmem_get_acquire, s_net_get_acquire)
        } .otherwise {
          vpn := local_start(paddrBits - 1, pgIdxBits)
          page_idx := local_start(pgIdxBits - 1, 0)
          state := s_ptw_req
        }
        remote_block := remote_start(paddrBits - 1, tlBlockOffset)

        val dst_off = dst_start(tlBlockOffset - 1, 0)
        val src_off = src_start(tlBlockOffset - 1, 0)

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
        header      := io.cmd.bits.header
        xact_id     := io.cmd.bits.xact_id
        write_half  := Bool(false)
        read_half   := Bool(false)
        first_block := Bool(true)
        direction   := cmd_dir
        error       := TxErrors.noerror
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
          error := TxErrors.pageFault
          state := s_idle
        } .otherwise {
          val fullPhysAddr = Cat(io.dptw.resp.bits.pte.ppn, page_idx)
          local_block := fullPhysAddr(paddrBits - 1, tlBlockOffset)
          beat_idx := UInt(0)
          state := Mux(direction, s_dmem_get_acquire, s_net_get_acquire)
        }
      }
    }
    is (s_prepare_read) {
      val src_page_idx = local_block(blockPgIdxBits - 1, 0)
      when (align >= bytes_left) {
        // if there are enough bytes already in the buffer (the read shift)
        // we can just send/write the last block
        state := Mux(direction, s_net_put_acquire, s_dmem_get_acquire)
      } .elsewhen (!io.phys && src_page_idx === UInt(0)) {
        vpn := vpn + UInt(1)
        page_idx := UInt(0)
        state := s_ptw_req
      } .otherwise {
        state := Mux(direction, s_dmem_get_acquire, s_net_get_acquire)
      }
      beat_idx := UInt(0)
    }
    is (s_dmem_get_acquire) {
      when (io.dmem.acquire.ready) {
        state := s_dmem_get_grant
      }
    }
    is (s_dmem_get_grant) {
      when (io.dmem.grant.valid) {
        when (direction) {
          val index = Cat(write_half, beat_idx)
          send_buffer(index) := io.dmem.grant.bits.data
          when (beat_idx === UInt(tlDataBeats - 1)) {
            val bytes_in_buffer = UInt(tlBytesPerBlock) - align
            val needs_more_data = align_dir && first_block &&
                                  bytes_in_buffer < bytes_left
            when (needs_more_data) {
              local_block := local_block + UInt(1)
              state := s_prepare_read
            } .otherwise {
              beat_idx := UInt(0)
              state := s_net_put_acquire
            }
            write_half := !write_half
            first_block := Bool(false)
          }
          beat_idx := beat_idx + UInt(1)
        } .otherwise {
          write_buffer(beat_idx) := io.dmem.grant.bits.data
          when (beat_idx === UInt(tlDataBeats - 1)) {
            beat_idx := offset(tlBlockOffset - 1, tlByteAddrBits)
            state := s_copy_data
          } .otherwise {
            beat_idx := beat_idx + UInt(1)
          }
        }
      }
    }
    is (s_copy_data) {
      write_buffer.write(beat_idx, beat_data, full_wmask)
      when (beat_idx === UInt(tlDataBeats - 1)) {
        read_half := !read_half
        offset := UInt(0)
        state := s_dmem_put_acquire
      }
      beat_idx := beat_idx + UInt(1)
    }
    is (s_dmem_put_acquire) {
      when (io.dmem.acquire.ready) {
        when (beat_idx === UInt(tlDataBeats - 1)) {
          state := s_dmem_put_grant
        }
        beat_idx := beat_idx + UInt(1)
      }
    }
    is (s_dmem_put_grant) {
      when (io.dmem.grant.valid) {
        when (bytes_left <= UInt(tlBytesPerBlock)) {
          bytes_left := UInt(0)
          state := s_idle
        } .otherwise {
          remote_block := remote_block + UInt(1)
          local_block := local_block + UInt(1)
          bytes_left := bytes_left - UInt(tlBytesPerBlock)
          state := s_prepare_read
        }
      }
    }
    is (s_net_get_acquire) {
      when (io.net.acquire.ready) {
        state := s_net_get_grant
      }
    }
    is (s_net_get_grant) {
      when (io.net.grant.valid) {
        when (net_grant.g_type === Grant.nackType) {
          error := TxErrors.nack
          state := s_idle
        } .otherwise {
          val index = Cat(write_half, beat_idx)
          send_buffer(index) := net_grant.data
          when (beat_idx === UInt(tlDataBeats - 1)) {
            val bytes_in_buffer = UInt(tlBytesPerBlock) - align
            val needs_more_data = align_dir && first_block &&
                                  bytes_in_buffer < bytes_left
            when (needs_more_data) {
              remote_block := remote_block + UInt(1)
              state := s_prepare_read
            } .elsewhen (full_block) {
              beat_idx := offset(tlBlockOffset - 1, tlByteAddrBits)
              state := s_copy_data
            } .otherwise {
              beat_idx := UInt(0)
              state := s_dmem_get_acquire
            }
            write_half := !write_half
            first_block := Bool(false)
          } .otherwise {
            beat_idx := beat_idx + UInt(1)
          }
        }
      }
    }
    is (s_net_put_acquire) {
      when (io.route_error) {
        error := TxErrors.noRoute
        state := s_idle
      } .elsewhen (io.net.acquire.ready) {
        when (beat_idx === UInt(tlDataBeats - 1)) {
          state := s_net_put_grant
        }
        beat_idx := beat_idx + UInt(1)
      }
    }
    is (s_net_put_grant) {
      when (io.net.grant.valid) {
        when (net_grant.g_type === Grant.nackType) {
          error := TxErrors.nack
          state := s_idle
        } .elsewhen (bytes_left <= UInt(tlBytesPerBlock)) {
          bytes_left := UInt(0)
          state := s_idle
        } .otherwise {
          remote_block := remote_block + UInt(1)
          local_block := local_block + UInt(1)
          read_half := !read_half
          offset := UInt(0)
          bytes_left := bytes_left - UInt(tlBytesPerBlock)
          state := s_prepare_read
        }
      }
    }
  }
}

class TileLinkDMARx extends DMAModule {
  val io = new Bundle {
    val net = new RemoteTileLinkIO().flip
    val dmem = new ClientUncachedTileLinkIO
    val dptw = new TLBPTWIO
    val phys = Bool(INPUT)
    val local_addr = new RemoteAddress().asInput
    val remote_addr = new RemoteAddress().asOutput
    val route_error = Bool(INPUT)
  }

  private val tlBlockOffset = tlBeatAddrBits + tlByteAddrBits
  private val blockPgIdxBits = pgIdxBits - tlBlockOffset

  val addr_block = Reg(init = UInt(0, tlBlockAddrBits))
  val buffer = Mem(Bits(width = tlDataBits), tlDataBeats, seqRead = true)
  val beat_idx = Reg(UInt(width = tlBeatAddrBits))
  val page_idx = Reg(UInt(width = blockPgIdxBits))
  val vpn = Reg(UInt(width = vpnBits))
  val net_xact_id = Reg(UInt(0, dmaXactIdBits))
  val net_acquire = io.net.acquire.bits.payload
  val direction = Reg(Bool())
  val nack = Reg(Bool())

  val (s_idle :: s_recv :: s_ack :: s_prepare_recv ::
       s_get_acquire :: s_get_grant :: s_put_acquire :: s_put_grant ::
       s_ptw_req :: s_ptw_resp :: s_discard :: Nil) = Enum(Bits(), 11)
  val state = Reg(init = s_idle)

  val remote_addr = Reg(new RemoteAddress)
  val local_addr = Reg(new RemoteAddress)

  io.remote_addr := remote_addr

  val net_type = Mux(nack, Grant.nackType,
                 Mux(direction, Grant.putAckType, Grant.getDataBlockType))

  io.net.acquire.ready := (state === s_recv) || (state === s_discard)
  io.net.grant.valid := (state === s_ack)
  io.net.grant.bits.payload := Grant(
    is_builtin_type = Bool(true),
    g_type = net_type,
    client_xact_id = net_xact_id,
    manager_xact_id = UInt(0),
    addr_beat = beat_idx,
    data = buffer(beat_idx))
  io.net.grant.bits.header.src := local_addr
  io.net.grant.bits.header.dst := remote_addr

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
        direction := (net_acquire.a_type != Acquire.getBlockType)
        remote_addr := io.net.acquire.bits.header.src
        net_xact_id := net_acquire.client_xact_id
      }
      // we don't want this to change in the middle of a transaction
      // so only update when in the idle state
      local_addr := io.local_addr
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
          beat_idx := UInt(0)
          state := s_discard
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
            nack := Bool(false)
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
      when (io.route_error) {
        state := s_idle
      } .elsewhen (io.net.grant.ready) {
        val single_beat = nack || direction
        when (single_beat || beat_idx === UInt(tlDataBeats - 1)) {
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
        nack := Bool(false)
        state := s_ack
      }
    }
    // this request cannot be processed, but we still need to consume
    // all of the acquire beats
    is (s_discard) {
      when (io.net.acquire.valid) {
        when (!direction || beat_idx === UInt(tlDataBeats - 1)) {
          nack := Bool(true)
          state := s_ack
        }
        beat_idx := beat_idx + UInt(1)
      }
    }
  }
}
