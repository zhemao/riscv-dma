package dma

import Chisel._
import rocket.{RoCC, RoCCResponse, CoreParameters}
import uncore.{TileLinkParameters, ClientUncachedTileLinkIOArbiter}

object CustomInstructions {
  val setAddrs = UInt(0)
  val doCopy = UInt(1)
  val setPhys = UInt(2)
}

class CopyAccelerator extends RoCC with DMAParameters
    with CoreParameters with TileLinkParameters {

  val src = Reg(UInt(width = paddrBits))
  val dst = Reg(UInt(width = paddrBits))
  val nbytes = Reg(UInt(width = paddrBits))
  val phys = Reg(init = Bool(false))

  val (s_idle :: s_req :: s_wait_dma :: s_error :: Nil) = Enum(Bits(), 4)
  val state = Reg(init = s_idle)

  val cmd = Queue(io.cmd)
  cmd.ready := (state === s_idle)

  io.resp.valid := Bool(false)

  val tx = Module(new TileLinkDMATx)
  tx.io.cmd.valid := (state === s_req)
  tx.io.cmd.bits.src_start := src
  tx.io.cmd.bits.dst_start := dst
  tx.io.cmd.bits.nbytes := nbytes
  tx.io.phys := phys

  val rx = Module(new TileLinkDMARx)
  rx.io.phys := phys

  rx.io.net.acquire <> Queue(tx.io.net.acquire, dmaQueueDepth)
  tx.io.net.grant <> Queue(rx.io.net.grant, dmaQueueDepth)

  val dmemArb = Module(new ClientUncachedTileLinkIOArbiter(2))
  dmemArb.io.in(0) <> tx.io.dmem
  dmemArb.io.in(1) <> rx.io.dmem
  dmemArb.io.out <> io.dmem

  val ptwArb = Module(new PTWArbiter(2))
  ptwArb.io.requestors(0) <> tx.io.dptw
  ptwArb.io.requestors(1) <> rx.io.dptw
  ptwArb.io.ptw <> io.dptw

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
            state := s_req
          }
          is (CustomInstructions.setPhys) {
            phys := cmd.bits.rs1(0)
          }
        }
      }
    }
    is (s_req) {
      when (tx.io.cmd.ready) {
        state := s_wait_dma
      }
    }
    is (s_wait_dma) {
      when (rx.io.error || tx.io.error) {
        state := s_error
      } .elsewhen (tx.io.cmd.ready && rx.io.idle) {
        state := s_idle
      }
    }
    is (s_error) {
      // stay here until reset
      state := s_error
    }
  }

  io.busy := (state != s_idle) || cmd.valid
  io.interrupt := Bool(false)

  io.mem.req.valid := Bool(false)
  io.imem.acquire.valid := Bool(false)
  io.imem.grant.ready := Bool(false)
  io.iptw.req.valid := Bool(false)
  io.pptw.req.valid := Bool(false)
}
