package dma

import Chisel._
import rocket.{RoCC, RoCCResponse, CoreParameters}
import uncore.{TileLinkParameters, ClientUncachedTileLinkIOArbiter}

object CustomInstructions {
  val setAddrs = UInt(0)
  val doCopy = UInt(1)
  val ptwLookup = UInt(2)
}

class CopyAccelerator extends RoCC with DMAParameters
    with CoreParameters with TileLinkParameters {

  val src = Reg(UInt(width = paddrBits))
  val dst = Reg(UInt(width = paddrBits))
  val nbytes = Reg(UInt(width = paddrBits))
  val vpn = Reg(UInt(width = vpnBits))
  val ppn = Reg(UInt(width = ppnBits))
  val pgOff = Reg(UInt(width = pgIdxBits))

  val (s_idle :: s_req_tx :: s_req_rx :: s_wait_dma ::
    s_req_ptw :: s_wait_ptw :: s_resp :: Nil) = Enum(Bits(), 7)
  val state = Reg(init = s_idle)

  val cmd = Queue(io.cmd)
  cmd.ready := (state === s_idle)

  val resp = Decoupled(new RoCCResponse)
  val resp_rd = Reg(Bits(width = 5))
  io.resp <> Queue(resp)
  resp.valid := (state === s_resp)
  resp.bits.rd := resp_rd
  resp.bits.data := Cat(ppn, pgOff)

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
          is (CustomInstructions.ptwLookup) {
            vpn := cmd.bits.rs1(vaddrBits - 1, pgIdxBits)
            pgOff := cmd.bits.rs1(pgIdxBits - 1, 0)
            resp_rd := cmd.bits.inst.rd
            state := s_req_ptw
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
        state := s_wait_dma
      }
    }
    is (s_wait_dma) {
      when (rx.io.req.ready) {
        state := s_idle
      }
    }
    is (s_req_ptw) {
      when (io.dptw.req.ready) {
        state := s_wait_ptw
      }
    }
    is (s_wait_ptw) {
      when (io.dptw.resp.valid) {
        when (io.dptw.resp.bits.error) {
          ppn := UInt(0)
          pgOff := UInt(0)
        } .otherwise {
          ppn := io.dptw.resp.bits.pte.ppn
        }
        state := s_resp
      }
    }
    is (s_resp) {
      when (resp.ready) {
        state := s_idle
      }
    }
  }

  io.dptw.req.valid := (state === s_req_ptw)
  io.dptw.req.bits.addr := vpn
  io.dptw.req.bits.prv := Bits(0)
  io.dptw.req.bits.store := Bool(false)
  io.dptw.req.bits.fetch := Bool(true)

  io.busy := (state != s_idle) || cmd.valid
  io.interrupt := Bool(false)

  io.mem.req.valid := Bool(false)
  io.imem.acquire.valid := Bool(false)
  io.imem.grant.ready := Bool(false)
  io.iptw.req.valid := Bool(false)
  io.pptw.req.valid := Bool(false)
}
