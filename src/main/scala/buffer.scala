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
  val wmask = Reg(init = UInt((1 << tlWriteMaskBits) - 1, tlWriteMaskBits))
  val wdata = Reg(init = UInt(0, tlDataBits))

  val buffer = Mem(UInt(width = tlDataBits), tlDataBeats, seqRead = true)

  val addr_block = address(paddrBits - 1, tlBlockAddrOffset)
  val addr_beat = address(tlBlockAddrOffset - 1, tlByteAddrBits)
  val addr_byte = address(tlByteAddrBits - 1, 0)

  val nbeats_send = Reg(init = UInt(0, tlBeatAddrBits))
  val nbeats_recv = Reg(init = UInt(0, tlBeatAddrBits))

  val acq_type = Mux(state === s_write,
    Mux(do_block, Acquire.putBlockType, Acquire.putType),
    Mux(do_block, Acquire.getBlockType, Acquire.getType))

  val union = Mux(state === s_write,
    Cat(wmask, Bool(true)),
    Cat(MT_Q, M_XRD, Bool(false)))

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

  switch (state) {
    is (s_idle) {
      when (io.cmd.valid) {
        val blockop = (io.cmd.bits.nbytes === UInt(0))
        val byte_len = io.cmd.bits.nbytes(tlByteAddrBits - 1, 0)
        val byte_off = io.cmd.bits.addr(tlByteAddrBits - 1, 0)

        // make sure to zero out the byte address
        address := io.cmd.bits.addr & ~UInt((1 << tlByteAddrBits) - 1)

        when (blockop) {
          xact_id := UInt(0)
          index := UInt(0)
          nbeats_send := UInt(tlDataBeats - 1)
          nbeats_recv := UInt(tlDataBeats - 1)
        } .otherwise {
          val cmd_nbeats =
            io.cmd.bits.nbytes(tlBlockAddrOffset - 1, tlByteAddrBits)
          val nbeats = Mux(byte_len === UInt(0), cmd_nbeats - UInt(1), UInt(0))
          xact_id := io.cmd.bits.index
          index := io.cmd.bits.index
          nbeats_send := nbeats
          nbeats_recv := nbeats
        }
        when (io.cmd.bits.opcode === BufferOp.read) {
          state := s_read
        } .elsewhen (io.cmd.bits.opcode === BufferOp.write) {
          state := s_write
          wmask := Mux(byte_len === UInt(0),
            UInt((1 << tlWriteMaskBits) - 1),
            ((UInt(1) << byte_len) - UInt(1)) << byte_off)
        }

        do_block := blockop
        sending := Bool(true)
      }
    }
    is (s_read) {
      when (io.dmem.acquire.ready) {
        when (nbeats_send === UInt(0)) {
          sending := Bool(false)
        } .otherwise {
          nbeats_send := nbeats_send - UInt(1)
          when (!do_block) {
            xact_id := xact_id + UInt(1)
            address := address + UInt(1 << tlByteAddrBits)
          }
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
          when (!do_block) {
            xact_id := xact_id + UInt(1)
            address := address + UInt(1 << tlByteAddrBits)
          }
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
