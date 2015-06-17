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

  val (s_idle :: s_read :: s_write :: s_setbytes :: Nil) = Enum(Bits(), 4)
  val state = Reg(init = s_idle)

  val do_block = Reg(init = Bool(false))

  val address = Reg(init = UInt(0, paddrBits))
  val index = Reg(init = UInt(0, tlBeatAddrBits))
  val wmask = Reg(init = Acquire.fullWriteMask)
  val wdata = Reg(init = UInt(0, tlDataBits))

  val next_beat_addr = address + UInt(1 << tlByteAddrBits)

  val buffer = Mem(UInt(width = tlDataBits), tlDataBeats, seqRead = true)

  val addr_block = address(paddrBits - 1, tlBlockAddrOffset)
  val addr_beat = address(tlBlockAddrOffset - 1, tlByteAddrBits)
  val addr_byte = address(tlByteAddrBits - 1, 0)

  val nbeats_send = Reg(init = UInt(0, tlBeatAddrBits))
  val nbeats_recv = Reg(init = UInt(0, tlBeatAddrBits))

  val acq_type = Mux(state === s_write,
    Mux(do_block, Acquire.putBlockType, Acquire.putType),
    Mux(do_block, Acquire.getBlockType, Acquire.getType))

  val union = Cat(Mux(state === s_write, wmask, Cat(MT_Q, M_XRD)), Bool(true))

  val xact_id = Reg(init = UInt(0, tlClientXactIdBits))

  io.dmem.acquire.bits := Acquire(
    is_builtin_type = Bool(true),
    a_type = acq_type,
    client_xact_id = xact_id,
    addr_block = address(paddrBits - 1, tlBlockAddrOffset),
    addr_beat = address(tlBlockAddrOffset - 1, tlByteAddrBits),
    data = buffer(index),
    union = union
  )

  val sending = Reg(init = Bool(false))
  val received = Reg(init = Bool(false))

  io.cmd.ready := (state === s_idle)
  io.dmem.acquire.valid := sending
  io.dmem.grant.ready := (state === s_read) || (state === s_write)
  debug(io.dmem.grant.bits.g_type)

  switch (state) {
    is (s_idle) {
      when (io.cmd.valid) {
        val byte_len = io.cmd.bits.nbytes(tlByteAddrBits - 1, 0)
        val byte_off = io.cmd.bits.addr(tlByteAddrBits - 1, 0)
        val beat_off = io.cmd.bits.addr(tlBlockAddrOffset - 1, tlByteAddrBits)

        // make sure to zero out the byte address
        address := Cat(io.cmd.bits.addr(paddrBits - 1, tlByteAddrBits),
                       UInt(0, tlByteAddrBits))

        val cmd_nbeats =
          io.cmd.bits.nbytes(tlBlockAddrOffset - 1, tlByteAddrBits)
        val nbeats = Mux(io.cmd.bits.nbytes === UInt(0),
          // if nbytes is zero, we want a whole block
          UInt(tlDataBeats - 1),
          // otherwise just take whatever nbytes indicates
          Mux(byte_len === UInt(0), cmd_nbeats - UInt(1), UInt(0)))
        xact_id := io.cmd.bits.index
        index := io.cmd.bits.index
        nbeats_send := nbeats
        nbeats_recv := nbeats

        when (io.cmd.bits.opcode === BufferOp.read) {
          state := s_read
        } .elsewhen (io.cmd.bits.opcode === BufferOp.write) {
          state := s_write
          wmask := Mux(byte_len === UInt(0),
            Acquire.fullWriteMask,
            ((UInt(1) << byte_len) - UInt(1)) << byte_off)
        }

        do_block := (io.cmd.bits.nbytes === UInt(0) && beat_off === UInt(0))
        sending := Bool(true)
      }
    }
    is (s_read) {
      when (io.dmem.acquire.ready) {
        when (nbeats_send === UInt(0) || do_block) {
          sending := Bool(false)
        } .otherwise {
          nbeats_send := nbeats_send - UInt(1)
          xact_id := xact_id + UInt(1)
          address := next_beat_addr
        }
      }

      when (io.dmem.grant.valid) {
        when (!do_block) { index := io.dmem.grant.bits.client_xact_id }
        wdata := io.dmem.grant.bits.data
        received := Bool(true)
      } .otherwise {
        received := Bool(false)
      }

      when (received) {
        buffer(index) := wdata
        when (do_block) { index := index + UInt(1) }
        when (nbeats_recv === UInt(0)) {
          state := s_idle
        } .otherwise {
          nbeats_recv := nbeats_recv - UInt(1)
        }
      }
    }
    is (s_write) {
      when (io.dmem.acquire.ready) {
        when (nbeats_send === UInt(0)) {
          sending := Bool(false)
        } .otherwise {
          nbeats_send := nbeats_send - UInt(1)
          index := index + UInt(1)
          address := next_beat_addr
          when (!do_block) { xact_id := xact_id + UInt(1) }
        }
      }
      when (io.dmem.grant.valid) {
        when (nbeats_recv === UInt(0) || do_block) {
          state := s_idle
        } .otherwise {
          nbeats_recv := nbeats_recv - UInt(1)
        }
      }
    }
  }
}
