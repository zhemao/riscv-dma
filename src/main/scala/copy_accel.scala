package dma

import Chisel._
import rocket.{RoCC, CoreParameters}
import uncore.{TileLinkParameters, ClientUncachedTileLinkIOArbiter}

object CustomInstructions {
  val setAddrs = UInt(0)
  val doCopy = UInt(1)
}

class CopyAccelerator extends RoCC with DMAParameters
    with CoreParameters with TileLinkParameters {

  val src = Reg(UInt(width = paddrBits))
  val dst = Reg(UInt(width = paddrBits))
  val nbytes = Reg(UInt(width = paddrBits))

  val (s_idle :: s_req_tx :: s_req_rx :: s_wait :: Nil) = Enum(Bits(), 4)
  val state = Reg(init = s_idle)

  val cmd = Queue(io.cmd)
  cmd.ready := (state === s_idle)
  io.resp.valid := Bool(false)

  val tx = Module(new DMATx)
  tx.io.req.valid := (state === s_req_tx)
  tx.io.req.bits.start_addr := src
  tx.io.req.bits.nbytes := nbytes

  val rx = Module(new DMARx)
  rx.io.req.valid := (state === s_req_rx)
  rx.io.req.bits.start_addr := dst
  rx.io.req.bits.nbytes := nbytes

  rx.io.data <> Queue(tx.io.data, dmaQueueDepth)

  val arb = Module(new ClientUncachedTileLinkIOArbiter(2))
  arb.io.in(0) <> tx.io.dmem
  arb.io.in(1) <> rx.io.dmem
  arb.io.out <> io.dmem

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
            state := s_req_tx
          }
        }
      }
    }
    is (s_req_tx) {
      when (tx.io.req.ready) {
        state := s_req_rx
      }
    }
    is (s_req_rx) {
      when (rx.io.req.ready) {
        state := s_wait
      }
    }
    is (s_wait) {
      when (rx.io.req.ready) {
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
