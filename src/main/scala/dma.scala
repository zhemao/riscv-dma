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

  private val addressBits = tlBlockAddrBits + tlBeatAddrBits
  private val beatPgIdxBits = pgIdxBits - tlByteAddrBits
  private val beatsPerPage = (1 << beatPgIdxBits)

  val address = Reg(UInt(width = tlBlockAddrBits + tlBeatAddrBits))
  val offset = Reg(UInt(width = tlByteAddrBits))
  val bytes_left = Reg(UInt(width = paddrBits))
  val buffer_bytes_left = Reg(UInt(width = tlByteAddrBits))
  val buffer = Reg(Bits(width = tlDataBits))

  val vpn = Reg(UInt(width = vpnBits))
  val pgIdx = Reg(UInt(width = pgIdxBits))
  val error = Reg(init = Bool(false))

  val bitshift = Cat(offset, UInt(0, 3))

  io.dmem.acquire.bits := Get(
    client_xact_id = UInt(0),
    addr_block = address(addressBits - 1, tlBeatAddrBits),
    addr_beat = address(tlBeatAddrBits - 1, 0),
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
  io.data.bits := buffer(dmaDataBits - 1, 0)
  io.req.ready := (state === s_idle)
  io.dptw.req.valid := (state === s_ptw_req)

  switch (state) {
    is (s_idle) {
      when (io.req.valid) {
        error := Bool(false)
        bytes_left := io.req.bits.nbytes
        when (io.phys) {
          address := io.req.bits.start_addr(paddrBits - 1, tlByteAddrBits)
          offset := io.req.bits.start_addr(tlByteAddrBits - 1, 0)
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
          address := fullPhysAddr(paddrBits - 1, tlByteAddrBits)
          offset := fullPhysAddr(tlByteAddrBits - 1, 0)
          state := s_acquire
        }
      }
    }
    is (s_acquire) {
      when (io.dmem.acquire.ready) {
        state := s_grant
      }
    }
    is (s_grant) {
      when (io.dmem.grant.valid) {
        buffer := io.dmem.grant.bits.data >> bitshift
        buffer_bytes_left := Mux(bytes_left < UInt(tlDataBytes),
          bytes_left(tlByteAddrBits - 1, 0),
          UInt(tlDataBytes) - offset)
        offset := UInt(0)
        state := s_send
      }
    }
    is (s_send) {
      when (io.data.ready) {
        when (bytes_left === UInt(dmaDataBytes)) {
          state := s_idle
        } .elsewhen (buffer_bytes_left === UInt(dmaDataBytes)) {
          val beatPgIdx = address(beatPgIdxBits - 1, 0)
          val endOfPage = beatPgIdx === UInt(beatsPerPage - 1)
          when (!io.phys && endOfPage) {
            vpn := vpn + UInt(1)
            pgIdx := UInt(0)
            state := s_ptw_req
          } .otherwise {
            address := address + UInt(1)
            state := s_acquire
          }
        }
        buffer := buffer >> UInt(dmaDataBits)
        buffer_bytes_left := buffer_bytes_left - UInt(dmaDataBytes)
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

  private val addressBits = tlBlockAddrBits + tlBeatAddrBits
  private val bufferWords = tlDataBits / dmaDataBits
  private val beatPgIdxBits = pgIdxBits - tlByteAddrBits
  private val beatsPerPage = (1 << beatPgIdxBits)

  val address = Reg(UInt(width = tlBlockAddrBits + tlBeatAddrBits))
  val offset = Reg(UInt(width = tlByteAddrBits))
  val bytes_left = Reg(UInt(width = paddrBits))
  val index = Reg(UInt(width = log2Up(bufferWords)))
  val buffer = Vec.fill(bufferWords) { Reg(Bits(width = dmaDataBits)) }

  val vpn = Reg(UInt(width = vpnBits))
  val pgIdx = Reg(UInt(width = pgIdxBits))
  val error = Reg(init = Bool(false))

  val wmask = Vec.tabulate(tlWriteMaskBits) {
    i => UInt(i) >= offset && UInt(i / dmaDataBytes) <= index
  }.toBits

  io.dmem.acquire.bits := Put(
    client_xact_id = UInt(1),
    addr_block = address(addressBits - 1, tlBeatAddrBits),
    addr_beat = address(tlBeatAddrBits - 1, 0),
    data = buffer.toBits,
    wmask = wmask)

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
          address := io.req.bits.start_addr(paddrBits - 1, tlByteAddrBits)
          offset := io.req.bits.start_addr(tlByteAddrBits - 1, 0)
          index := io.req.bits.start_addr(tlByteAddrBits - 1, dmaDataOffset)
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
          address := fullPhysAddr(paddrBits - 1, tlByteAddrBits)
          offset := fullPhysAddr(tlByteAddrBits - 1, 0)
          index := fullPhysAddr(tlByteAddrBits - 1, dmaDataOffset)
          state := s_recv
        }
      }
    }
    is (s_recv) {
      when (io.data.valid) {
        buffer(index) := io.data.bits
        bytes_left := bytes_left - UInt(dmaDataBytes)
        val beat_done = (index === UInt(bufferWords - 1))
        val rx_done = (bytes_left === UInt(dmaDataBytes))
        when (beat_done || rx_done) {
          state := s_acquire
        } .otherwise {
          index := index + UInt(1)
        }
      }
    }
    is (s_acquire) {
      when (io.dmem.acquire.ready) {
        index := UInt(0)
        offset := UInt(0)
        state := s_grant
      }
    }
    is (s_grant) {
      when (io.dmem.grant.valid) {
        when (bytes_left === UInt(0)) {
          state := s_idle
        } .otherwise {
          val beatPgIdx = address(beatPgIdxBits - 1, 0)
          val endOfPage = beatPgIdx === UInt(beatsPerPage - 1)
          when (!io.phys && endOfPage) {
            vpn := vpn + UInt(1)
            pgIdx := UInt(0)
            state := s_ptw_req
          } .otherwise {
            address := address + UInt(1)
            state := s_acquire
          }
          state := s_recv
        }
      }
    }
  }
}
