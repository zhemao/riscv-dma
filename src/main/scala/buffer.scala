package dma

import Chisel._
import rocket.{RoCCInterface, CoreParameters}
import uncore._

object BufferOp {
  val (read :: write :: set :: Nil) = Enum(Bits(), 3)
}

class BufferCmd extends Bundle with TileLinkParameters with CoreParameters {
  private val tlBlockAddrOffset = tlBeatAddrBits + tlByteAddrBits

  val opcode = Bits(width = 2)
  val addr = UInt(width = paddrBits)
  val index = UInt(width = tlBeatAddrBits)
  val nbytes = UInt(width = tlBlockAddrOffset)
  val byte = UInt(width = 8)
}

class BlockBuffer extends Module with TileLinkParameters with CoreParameters {
  private val tlBlockAddrOffset = tlBeatAddrBits + tlByteAddrBits

  val io = new Bundle {
    val dmem = new ClientUncachedTileLinkIO
    val cmd = Decoupled(new BufferCmd).flip
  }

  val (s_idle :: s_readbeat :: s_readblock ::
    s_writebeat :: s_writeblock :: s_setbytes :: Nil) = Enum(Bits(), 6)
  val state = Reg(init = s_idle)

  val address = Reg(init = UInt(0, paddrBits))
  val index = Reg(init = UInt(0, tlBeatAddrBits))
  val wmask = Reg(init = UInt((1 << tlWriteMaskBits) - 1, tlWriteMaskBits))

  val buffer = Vec.fill(tlDataBeats) { Reg(init = UInt(0, tlDataBits)) }

  val addr_block = address(paddrBits - 1, tlBlockAddrOffset)
  val addr_beat = address(tlBlockAddrOffset - 1, tlByteAddrBits)
  val addr_byte = address(tlByteAddrBits - 1, 0)

  val nbeats_send = Reg(init = UInt(0, tlBeatAddrBits))
  val nbeats_recv = Reg(init = UInt(0, tlBeatAddrBits))

  val acq_type = PriorityMux(
    Seq(state === s_readbeat, state === s_readblock,
        state === s_writebeat, state === s_writeblock),
    Seq(Acquire.getType, Acquire.getBlockType,
        Acquire.putType, Acquire.putBlockType))

  val union = PriorityMux(
    Seq(state === s_readbeat || state === s_readblock,
        state === s_writebeat || state === s_writeblock),
    Seq(Cat(MT_Q, M_XRD, Bool(false)),
        Cat(wmask, Bool(true))))

  io.dmem.acquire.bits := Acquire(
    is_builtin_type = Bool(true),
    a_type = acq_type,
    client_xact_id = index,
    addr_block = address(paddrBits - 1, tlBlockAddrOffset),
    addr_beat = address(tlBlockAddrOffset - 1, tlByteAddrBits),
    data = buffer(index),
    union = union
  )

  val sending = Reg(init = Bool(false))

  io.cmd.ready := (state === s_idle)
  io.dmem.acquire.valid := sending

  switch (state) {
    is (s_idle) {
      when (io.cmd.valid) {
        val blockop = (io.cmd.bits.nbytes === UInt(0))
        address := io.cmd.bits.addr

        when (blockop) {
          index := UInt(0)
          nbeats_send := UInt(0)
          nbeats_recv := UInt(tlDataBeats - 1)
        } .otherwise {
          val cmd_nbeats =
            io.cmd.bits.nbytes(tlBlockAddrOffset - 1, tlByteAddrBits)
          val nbeats = Mux(cmd_nbeats === UInt(0), UInt(0), cmd_nbeats - UInt(1))
          index := io.cmd.bits.index
          nbeats_send := nbeats
          nbeats_recv := nbeats
        }
        when (io.cmd.bits.opcode === BufferOp.read) {
          sending := Bool(true)
          state := Mux(blockop, s_readblock, s_readbeat)  
        } .elsewhen (io.cmd.bits.opcode === BufferOp.write) {
          sending := Bool(true)
          state := Mux(blockop, s_writeblock, s_writebeat)
        }
      }
    }
    is (s_readbeat) {
      when (io.dmem.acquire.ready) {
        when (nbeats_send === UInt(0)) {
          sending := Bool(false)
        } .otherwise {
          nbeats_send := nbeats_send - UInt(1)
          index := index + UInt(1)
          address := address + UInt(1 << tlByteAddrBits)
        }
      }
      when (io.dmem.grant.valid) {
        val resp_tag = io.dmem.grant.bits.client_xact_id
        buffer(resp_tag) := io.dmem.grant.bits.data

        when (nbeats_recv === UInt(0)) {
          state := s_idle
        } .otherwise {
          nbeats_recv := nbeats_recv - UInt(1)
        }
      }
    }
    //is (s_readblock) {
    //  sending := Bool(false)
    //  when (io.dmem.grant.valid) {
    //  }
    //}
    is (s_writebeat) {
      when (io.dmem.acquire.ready) {
        when (nbeats_send === UInt(0)) {
          sending := Bool(false)
        } .otherwise {
          nbeats_send := nbeats_send - UInt(1)
          index := index + UInt(1)
          address := address + UInt(1 << tlByteAddrBits)
        }
      }
      when (io.dmem.grant.valid) {
        when (nbeats_recv === UInt(0)) {
          state := s_idle
        } .otherwise {
          nbeats_recv := nbeats_recv - UInt(1)
        }
      }
    }
  }
}
