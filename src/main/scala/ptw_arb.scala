package dma

import Chisel._
import rocket.{TLBPTWIO, PTWReq}

class PTWArbiter(n: Int) extends Module {
  val io = new Bundle {
    val requestors = Vec.fill(n) { new TLBPTWIO().flip }
    val ptw = new TLBPTWIO
  }

  val (s_ready :: s_send :: s_recv :: Nil) = Enum(Bits(), 3)
  val state = Reg(init = s_ready)

  val arb = Module(new RRArbiter(new PTWReq, n))
  arb.io.in <> io.requestors.map(_.req)
  arb.io.out.ready := state === s_ready

  val r_req = Reg(new PTWReq)
  val r_dest = Reg(Bits(width = log2Up(n)))

  switch (state) {
    is (s_ready) {
      when (arb.io.out.valid) {
        r_req := arb.io.out.bits
        r_dest := arb.io.chosen
        state := s_send
      }
    }
    is (s_send) {
      when (io.ptw.req.ready) {
        state := s_recv
      }
    }
    is (s_recv) {
      when (io.ptw.resp.valid) {
        state := s_ready
      }
    }
  }

  io.ptw.req.bits := r_req
  io.ptw.req.valid := (state === s_send)

  for (i <- 0 until n) {
    io.requestors(i).status := io.ptw.status
    io.requestors(i).invalidate := io.ptw.invalidate
    io.requestors(i).resp.bits := io.ptw.resp.bits
    io.requestors(i).resp.valid := io.ptw.resp.valid && r_dest === UInt(i)
  }
}
