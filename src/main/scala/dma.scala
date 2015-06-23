package dma

import Chisel._
import rocket.{CoreParameters, TLBPTWIO}
import uncore._

case object DMADataBits extends Field[Int]
case object DMAQueueDepth extends Field[Int]

trait DMAParameters extends UsesParameters {
  val dmaDataBits = params(DMADataBits)
  val dmaDataBytes = dmaDataBits / 8
  val dmaDataOffset = log2Up(dmaDataBytes)
  val dmaQueueDepth = params(DMAQueueDepth)
}

class DMARequest extends Bundle with DMAParameters with CoreParameters {
  val start_addr = UInt(width = paddrBits)
  val nbytes = UInt(width = paddrBits)
}

class DMATx extends Module with DMAParameters
    with TileLinkParameters with CoreParameters {
  val io = new Bundle {
    val dmem = new ClientUncachedTileLinkIO
    val dptw = new TLBPTWIO
    val req = Decoupled(new DMARequest).flip
    val data = Decoupled(Bits(width = dmaDataBits))
    val phys = Bool(INPUT)
    val error = Bool(OUTPUT)
  }

  private val blockAddrOffset = tlBeatAddrBits + tlByteAddrBits
  private val blockPgIdxBits = pgIdxBits - blockAddrOffset
  private val blocksPerPage = (1 << blockPgIdxBits)
  private val wordAddrBits = tlByteAddrBits - dmaDataOffset
  private val wordsPerBeat = tlDataBits / dmaDataBits

  val addr_block = Reg(UInt(width = tlBlockAddrBits))
  val addr_beat = Reg(UInt(width = tlBeatAddrBits))
  val addr_word = Reg(UInt(width = wordAddrBits))

  val skip_beats = Reg(UInt(width = tlBeatAddrBits))
  val bytes_left = Reg(UInt(width = paddrBits))
  val buffer = Vec.fill(tlDataBeats) { Reg(Bits(width = tlDataBits)) }

  val vpn = Reg(UInt(width = vpnBits))
  val pgIdx = Reg(UInt(width = pgIdxBits))
  val error = Reg(init = Bool(false))

  val bitshift = Cat(addr_word, UInt(0, 3 + dmaDataOffset))

  io.dmem.acquire.bits := GetBlock(
    client_xact_id = UInt(0),
    addr_block = addr_block,
    alloc = Bool(true))

  io.dptw.req.bits.addr := vpn
  io.dptw.req.bits.prv := Bits(0)
  io.dptw.req.bits.store := Bool(false)
  io.dptw.req.bits.fetch := Bool(true)

  io.error := error

  val (s_idle :: s_ptw_req :: s_ptw_resp ::
       s_acquire :: s_grant :: s_send :: Nil) = Enum(Bits(), 6)
  val state = Reg(init = s_idle)

  io.dmem.acquire.valid := (state === s_acquire)
  io.dmem.grant.ready := (state === s_grant)
  io.data.valid := (state === s_send)
  io.data.bits := (buffer(addr_beat) >> bitshift)(dmaDataBits - 1, 0)
  io.req.ready := (state === s_idle)
  io.dptw.req.valid := (state === s_ptw_req)

  switch (state) {
    is (s_idle) {
      when (io.req.valid) {
        error := Bool(false)
        bytes_left := io.req.bits.nbytes
        when (io.phys) {
          addr_block := io.req.bits.start_addr(paddrBits - 1, blockAddrOffset)
          skip_beats := io.req.bits.start_addr(blockAddrOffset - 1, tlByteAddrBits)
          addr_word := io.req.bits.start_addr(tlByteAddrBits - 1, dmaDataOffset)
          state := s_acquire
        } .otherwise {
          vpn := io.req.bits.start_addr(paddrBits - 1, pgIdxBits)
          pgIdx := io.req.bits.start_addr(pgIdxBits - 1, 0)
          state := s_ptw_req
        }
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
          val fullPhysAddr = Cat(io.dptw.resp.bits.pte.ppn, pgIdx)
          addr_block := fullPhysAddr(paddrBits - 1, blockAddrOffset)
          skip_beats := fullPhysAddr(blockAddrOffset - 1, tlByteAddrBits)
          addr_word := fullPhysAddr(tlByteAddrBits - 1, dmaDataOffset)
          state := s_acquire
        }
      }
    }
    is (s_acquire) {
      when (io.dmem.acquire.ready) {
        addr_beat := UInt(0)
        state := s_grant
      }
    }
    is (s_grant) {
      when (io.dmem.grant.valid) {
        buffer(addr_beat) := io.dmem.grant.bits.data
        when (addr_beat === UInt(tlDataBeats - 1)) {
          addr_beat := skip_beats
          state := s_send
        } .otherwise {
          addr_beat := addr_beat + UInt(1)
        }
      }
    }
    is (s_send) {
      when (io.data.ready) {
        when (bytes_left === UInt(dmaDataBytes)) {
          state := s_idle
        } .elsewhen (addr_word === UInt(wordsPerBeat - 1)) {
          val beatPgIdx = addr_block(blockPgIdxBits - 1, 0)
          val endOfPage = beatPgIdx === UInt(blocksPerPage - 1)
          when (addr_beat != UInt(tlDataBeats - 1)) {
            addr_beat := addr_beat + UInt(1)
          } .elsewhen (!io.phys && endOfPage) {
            vpn := vpn + UInt(1)
            pgIdx := UInt(0)
            state := s_ptw_req
          } .otherwise {
            addr_beat := UInt(0)
            addr_block := addr_block + UInt(1)
            skip_beats := UInt(0)
            state := s_acquire
          }
        }
        addr_word := addr_word + UInt(1)
        bytes_left := bytes_left - UInt(dmaDataBytes)
      }
    }
  }
}

class DMARx extends Module with DMAParameters
    with TileLinkParameters with CoreParameters {
  val io = new Bundle {
    val dmem = new ClientUncachedTileLinkIO
    val dptw = new TLBPTWIO
    val req = Decoupled(new DMARequest).flip
    val data = Decoupled(Bits(width = dmaDataBits)).flip
    val phys = Bool(INPUT)
    val error = Bool(OUTPUT)
  }

  private val wordsPerBeat = tlDataBits / dmaDataBits
  private val wordAddrBits = tlByteAddrBits - dmaDataOffset
  private val blockPgIdxBits = pgIdxBits - blockAddrOffset
  private val blocksPerPage = (1 << blockPgIdxBits)
  private val blockAddrOffset = tlBeatAddrBits + tlByteAddrBits

  val addr_block = Reg(UInt(width = tlBlockAddrBits))
  val addr_beat = Reg(UInt(width = tlBeatAddrBits))
  val addr_word = Reg(UInt(width = wordAddrBits))

  val offset = Reg(UInt(width = tlByteAddrBits))
  val first_beat = Reg(UInt(width = tlBeatAddrBits))
  val last_beat = Reg(UInt(width = tlBeatAddrBits))
  val bytes_left = Reg(UInt(width = paddrBits))

  val buffer = Vec.fill(tlDataBeats) {
    Vec.fill(wordsPerBeat) { Reg(Bits(width = dmaDataBits)) }
  }

  val vpn = Reg(UInt(width = vpnBits))
  val pgIdx = Reg(UInt(width = pgIdxBits))
  val error = Reg(init = Bool(false))

  val wmask = Vec.tabulate(tlWriteMaskBits) { i =>
    addr_beat >= first_beat && addr_beat <= last_beat &&
    UInt(i) >= offset && UInt(i / dmaDataBytes) <= addr_word
  }.toBits

  io.dmem.acquire.bits := Acquire(
    is_builtin_type = Bool(true),
    a_type = Acquire.putBlockType,
    client_xact_id = UInt(1),
    addr_block = addr_block,
    addr_beat = addr_beat,
    data = buffer(addr_beat).toBits,
    union = Cat(wmask, Bool(true)))

  io.dptw.req.bits.addr := vpn
  io.dptw.req.bits.prv := Bits(0)
  io.dptw.req.bits.store := Bool(true)
  io.dptw.req.bits.fetch := Bool(false)

  io.error := error

  val (s_idle :: s_ptw_req :: s_ptw_resp ::
       s_recv :: s_acquire :: s_grant :: Nil) = Enum(Bits(), 6)
  val state = Reg(init = s_idle)

  io.dmem.acquire.valid := (state === s_acquire)
  io.dmem.grant.ready := (state === s_grant)
  io.data.ready := (state === s_recv)
  io.req.ready := (state === s_idle)
  io.dptw.req.valid := (state === s_ptw_req)

  switch (state) {
    is (s_idle) {
      when (io.req.valid) {
        error := Bool(false)
        bytes_left := io.req.bits.nbytes
        when (io.phys) {
          addr_block := io.req.bits.start_addr(paddrBits - 1, blockAddrOffset)
          addr_beat := io.req.bits.start_addr(blockAddrOffset - 1, tlByteAddrBits)
          addr_word := io.req.bits.start_addr(tlByteAddrBits - 1, dmaDataOffset)
          first_beat := io.req.bits.start_addr(blockAddrOffset - 1, tlByteAddrBits)
          offset := io.req.bits.start_addr(tlByteAddrBits - 1, 0)
          state := s_recv
        } .otherwise {
          vpn := io.req.bits.start_addr(paddrBits - 1, pgIdxBits)
          pgIdx := io.req.bits.start_addr(pgIdxBits - 1, 0)
          state := s_ptw_req
        }
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
          val fullPhysAddr = Cat(io.dptw.resp.bits.pte.ppn, pgIdx)
          addr_block := fullPhysAddr(paddrBits - 1, blockAddrOffset)
          addr_beat := fullPhysAddr(blockAddrOffset - 1, tlByteAddrBits)
          addr_word := fullPhysAddr(tlByteAddrBits - 1, dmaDataOffset)
          first_beat := fullPhysAddr(blockAddrOffset - 1, tlByteAddrBits)
          offset := fullPhysAddr(tlByteAddrBits - 1, 0)
          state := s_recv
        }
      }
    }
    is (s_recv) {
      when (io.data.valid) {
        buffer(addr_beat)(addr_word) := io.data.bits

        when (bytes_left === UInt(dmaDataBytes)) {
          last_beat := addr_beat
          addr_beat := UInt(0)
          state := s_acquire
        } .elsewhen (addr_word === UInt(wordsPerBeat - 1)) {
          when (addr_beat != UInt(tlDataBeats - 1)) {
            addr_beat := addr_beat + UInt(1)
            addr_word := UInt(0)
          } .otherwise {
            last_beat := addr_beat
            addr_beat := UInt(0)
            state := s_acquire
          }
        } .otherwise {
          addr_word := addr_word + UInt(1)
        }

        bytes_left := bytes_left - UInt(dmaDataBytes)
      }
    }
    is (s_acquire) {
      when (io.dmem.acquire.ready) {
        when (addr_beat === UInt(tlDataBeats - 1)) {
          state := s_grant
        } .elsewhen (addr_beat === first_beat) {
          offset := UInt(0)
        }
        addr_beat := addr_beat + UInt(1)
      }
    }
    is (s_grant) {
      when (io.dmem.grant.valid) {
        when (bytes_left === UInt(0)) {
          state := s_idle
        } .otherwise {
          val blockPgIdx = addr_block(blockPgIdxBits - 1, 0)
          val endOfPage = blockPgIdx === UInt(blocksPerPage - 1)
          when (!io.phys && endOfPage) {
            vpn := vpn + UInt(1)
            pgIdx := UInt(0)
            state := s_ptw_req
          } .otherwise {
            addr_block := addr_block + UInt(1)
            addr_beat := UInt(0)
            addr_word := UInt(0)
            first_beat := UInt(0)
            state := s_recv
          }
        }
      }
    }
  }
}
