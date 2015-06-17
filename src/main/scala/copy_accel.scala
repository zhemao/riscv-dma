package dma

import Chisel._
import rocket.{RoCC, CoreParameters}
import uncore.{TileLinkParameters}

object CustomInstructions {
  val setAddrs = UInt(0)
  val doCopy = UInt(1)
}

class CopyAccelerator extends RoCC
    with CoreParameters with TileLinkParameters {
  private val tlBlockAddrOffset = tlBeatAddrBits + tlByteAddrBits
  private val oneBlock = UInt(1 << tlBlockAddrOffset)

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
  block_buffer.io.cmd.bits.nbytes := Mux(nbytes < oneBlock,
    nbytes(tlBlockAddrOffset - 1, 0), oneBlock)
  block_buffer.io.cmd.bits.byte := UInt(0)

  val cmd = Queue(io.cmd)
  cmd.ready := (state === s_idle)
  io.resp.valid := Bool(false)

  switch (state) {
    is (s_idle) {
      when (cmd.valid) {
        switch (cmd.bits.inst.funct) {
          is (CustomInstructions.setAddrs) {
            src := cmd.bits.rs1
            dst := cmd.bits.rs2
          }
          is (CustomInstructions.doCopy) {
            nbytes := cmd.bits.rs1
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
        when (nbytes > oneBlock) {
          nbytes := nbytes - oneBlock
          src := src + oneBlock
          dst := dst + oneBlock
          state := s_read
        } .otherwise {
          state := s_wait
        }
      }
    }
    is (s_wait) {
      when (block_buffer.io.cmd.ready) {
        state := s_idle
      }
    }
  }

  io.busy := (state != s_idle)
  io.interrupt := Bool(false)

  io.mem.req.valid := Bool(false)
  io.imem.acquire.valid := Bool(false)
  io.imem.grant.ready := Bool(false)
  io.iptw.req.valid := Bool(false)
  io.dptw.req.valid := Bool(false)
  io.pptw.req.valid := Bool(false)
}
