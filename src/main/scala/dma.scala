package dma

import Chisel._
import rocket.{CoreParameters}
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
    val req = Decoupled(new DMARequest).flip
    val data = Decoupled(Bits(width = dmaDataBits))
  }

  private val addressBits = tlBlockAddrBits + tlBeatAddrBits

  val address = Reg(UInt(width = tlBlockAddrBits + tlBeatAddrBits))
  val offset = Reg(UInt(width = tlByteAddrBits))
  val bytes_left = Reg(UInt(width = paddrBits))
  val buffer_bytes_left = Reg(UInt(width = tlByteAddrBits))
  val buffer = Reg(Bits(width = tlDataBits))

  val bitshift = Cat(offset, UInt(0, 3))

  io.dmem.acquire.bits := Get(
    client_xact_id = UInt(0),
    addr_block = address(addressBits - 1, tlBeatAddrBits),
    addr_beat = address(tlBeatAddrBits - 1, 0),
    alloc = Bool(true))

  val (s_idle :: s_acquire :: s_grant :: s_send :: Nil) = Enum(Bits(), 4)
  val state = Reg(init = s_idle)

  io.dmem.acquire.valid := (state === s_acquire)
  io.dmem.grant.ready := (state === s_grant)
  io.data.valid := (state === s_send)
  io.data.bits := buffer(dmaDataBits - 1, 0)
  io.req.ready := (state === s_idle)

  switch (state) {
    is (s_idle) {
      when (io.req.valid) {
        address := io.req.bits.start_addr(paddrBits - 1, tlByteAddrBits)
        offset := io.req.bits.start_addr(tlByteAddrBits - 1, 0)
        bytes_left := io.req.bits.nbytes
        state := s_acquire
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
          address := address + UInt(1)
          state := s_acquire
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
    val req = Decoupled(new DMARequest).flip
    val data = Decoupled(Bits(width = dmaDataBits)).flip
  }

  private val addressBits = tlBlockAddrBits + tlBeatAddrBits
  private val bufferWords = tlDataBits / dmaDataBits

  val address = Reg(UInt(width = tlBlockAddrBits + tlBeatAddrBits))
  val offset = Reg(UInt(width = tlByteAddrBits))
  val bytes_left = Reg(UInt(width = paddrBits))
  val index = Reg(UInt(width = log2Up(bufferWords)))
  val buffer = Vec.fill(bufferWords) { Reg(Bits(width = dmaDataBits)) }
  val bitshift = Cat(offset, UInt(0, 3))

  val wmask = UInt(Acquire.fullWriteMask) << bitshift

  io.dmem.acquire.bits := Put(
    client_xact_id = UInt(0),
    addr_block = address(addressBits - 1, tlBeatAddrBits),
    addr_beat = address(tlBeatAddrBits - 1, 0),
    data = buffer.toBits,
    wmask = wmask)

  val (s_idle :: s_recv :: s_acquire :: s_grant :: Nil) = Enum(Bits(), 4)
  val state = Reg(init = s_idle)

  io.dmem.acquire.valid := (state === s_acquire)
  io.dmem.grant.ready := (state === s_grant)
  io.data.ready := (state === s_recv)
  io.req.ready := (state === s_idle)

  switch (state) {
    is (s_idle) {
      when (io.req.valid) {
        address := io.req.bits.start_addr(paddrBits - 1, tlByteAddrBits)
        offset := io.req.bits.start_addr(tlByteAddrBits - 1, 0)
        index := io.req.bits.start_addr(tlByteAddrBits - 1, dmaDataOffset)
        bytes_left := io.req.bits.nbytes
        state := s_recv
      }
    }
    is (s_recv) {
      when (io.data.valid) {
        buffer(index) := io.data.bits
        when (index === UInt(bufferWords - 1)) {
          state := s_acquire
        } .otherwise {
          index := index + UInt(1)
          bytes_left := bytes_left - UInt(dmaDataBytes)
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
        when (bytes_left === UInt(0)) {
          state := s_idle
        } .otherwise {
          index := UInt(0)
          offset := UInt(0)
          address := address + UInt(1)
          state := s_recv
        }
      }
    }
  }
}
