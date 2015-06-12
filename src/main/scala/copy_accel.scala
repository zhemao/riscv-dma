package dma

import Chisel._
import rocket.{RoCC, CoreParameters}

object CustomInstructions {
  val setAddrs = UInt(0)
  val doCopy = UInt(1)
}

class CopyAccelerator extends RoCC with CoreParameters {
  val (s_idle :: s_read :: s_write :: s_wait :: Nil) = Enum(Bits(), 4)
  val state = Reg(init = s_idle)

  val src = Reg(UInt(width = paddrBits))
  val dst = Reg(UInt(width = paddrBits))
  val nbytes = Reg(UInt(width = paddrBits))
  val block_buffer = Module(new BlockBuffer)

  block_buffer.io.dmem <> io.dmem
  block_buffer.io.cmd.valid := (state === s_read) || (state === s_write)
  block_buffer.io.cmd.bits.opcode := Mux(
    state === s_write, BufferOp.write, BufferOp.read)
  block_buffer.io.cmd.bits.addr := Mux(state === s_write, dst, src)
  block_buffer.io.cmd.bits.index := UInt(0)
  block_buffer.io.cmd.bits.nbytes := nbytes
  block_buffer.io.cmd.bits.byte := UInt(0)

  io.cmd.ready := (state === s_idle)

  switch (state) {
    is (s_idle) {
      when (io.cmd.valid) {
        switch (io.cmd.bits.inst.funct) {
          is (CustomInstructions.setAddrs) {
            src := io.cmd.bits.rs1
            dst := io.cmd.bits.rs2
          }
          is (CustomInstructions.doCopy) {
            nbytes := io.cmd.bits.rs1
            state := s_read
          }
        }
      }
    }
    is (s_read) {
      when (block_buffer.io.cmd.ready) {
        state := s_write
      }
    }
    is (s_write) {
      when (block_buffer.io.cmd.ready) {
        state := s_wait
      }
    }
    is (s_wait) {
      when (block_buffer.io.cmd.ready) {
        state := s_idle
      }
    }
  }
}
