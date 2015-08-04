package dma

import Chisel._
import rocket.{RoCC, RoCCResponse, CoreParameters}
import uncore._

object CustomInstructions {
  val DMA_PUT         = UInt(0)
  val DMA_GET         = UInt(1)
}

import CustomInstructions._

object DMACSRs {
  val SEGMENT_SIZE = 0
  val SRC_STRIDE   = 1
  val DST_STRIDE   = 2
  val NSEGMENTS    = 3
  val LOCAL_ADDR   = 4
  val LOCAL_PORT   = 5
  val REMOTE_ADDR  = 6
  val REMOTE_PORT  = 7
  val SENDER_ADDR  = 8
  val SENDER_PORT  = 9
  val TX_ERROR     = 10
  val PHYS         = 11
}

import DMACSRs._

class DMACSRs extends DMABundle {
  val segment_size = UInt(width = paddrBits)
  val src_stride = UInt(width = paddrBits)
  val dst_stride = UInt(width = paddrBits)
  val nsegments = UInt(width = paddrBits)
  val header = new RemoteHeader
  val phys = Bool()
}

class SegmentSenderCommand extends DMABundle {
  val dst = UInt(width = paddrBits)
  val src = UInt(width = paddrBits)
  val direction = Bool()
}

class SegmentSender extends DMAModule {
  val io = new Bundle {
    val cmd = Decoupled(new SegmentSenderCommand).flip
    val csrs = (new DMACSRs).asInput
    val dma = Decoupled(new TileLinkDMACommand)
    val busy = Bool(OUTPUT)
  }

  val s_idle :: s_req :: s_wait :: Nil = Enum(Bits(), 3)
  val state = Reg(init = s_idle)

  val cmd = Queue(io.cmd)
  cmd.ready := (state === s_idle)

  io.busy := (state != s_idle) || cmd.valid

  val src = Reg(UInt(width = paddrBits))
  val dst = Reg(UInt(width = paddrBits))
  val segments_left = Reg(UInt(width = paddrBits))
  val direction = Reg(Bool())
  val xact_id = Reg(init = UInt(0, dmaXactIdBits))
  val src_step = Reg(UInt(width = paddrBits))
  val dst_step = Reg(UInt(width = paddrBits))

  io.dma.valid := (state === s_req)
  io.dma.bits.src_start := src
  io.dma.bits.dst_start := dst
  io.dma.bits.nbytes := io.csrs.segment_size
  io.dma.bits.direction := direction
  io.dma.bits.header := io.csrs.header
  io.dma.bits.xact_id := xact_id

  val nowork = io.csrs.segment_size === UInt(0) ||
               io.csrs.nsegments === UInt(0)

  switch (state) {
    is (s_idle) {
      when (cmd.valid) {
        dst := cmd.bits.dst
        src := cmd.bits.src
        direction := cmd.bits.direction
        dst_step := io.csrs.segment_size + io.csrs.dst_stride
        src_step := io.csrs.segment_size + io.csrs.src_stride
        segments_left := io.csrs.nsegments

        when (!nowork) { state := s_req }
      }
    }
    is (s_req) {
      when (io.dma.ready) {
        src := src + src_step
        dst := dst + dst_step
        when (segments_left === UInt(1)) {
          state := s_wait
        }
        segments_left := segments_left - UInt(1)
      }
    }
    is (s_wait) {
      when (io.dma.ready) {
        xact_id := xact_id + UInt(1)
        state := s_idle
      }
    }
  }
}

class CopyAccelerator extends RoCC with DMAParameters with TileLinkParameters {

  val (s_idle :: s_req_send ::
    s_req_track :: s_resp :: Nil) = Enum(Bits(), 4)
  val state = Reg(init = s_idle)

  val initCsrs = new DMACSRs
  initCsrs.segment_size := UInt(0)
  initCsrs.dst_stride := UInt(0)
  initCsrs.src_stride := UInt(0)
  initCsrs.nsegments := UInt(0)
  initCsrs.phys := Bool(false)
  initCsrs.header.dst.addr := UInt(0)
  initCsrs.header.dst.port := UInt(0)
  initCsrs.header.src.addr := UInt(0)
  initCsrs.header.src.port := UInt(0)
  val csrs = Reg(init = initCsrs)

  when (io.csrs.wen) {
    switch (io.csrs.waddr) {
      is (UInt(SEGMENT_SIZE)) { csrs.segment_size := io.csrs.wdata }
      is (UInt(SRC_STRIDE))   { csrs.src_stride := io.csrs.wdata }
      is (UInt(DST_STRIDE))   { csrs.dst_stride := io.csrs.wdata }
      is (UInt(NSEGMENTS))    { csrs.nsegments := io.csrs.wdata }
      is (UInt(LOCAL_ADDR))   { csrs.header.src.addr := io.csrs.wdata }
      is (UInt(LOCAL_PORT))   { csrs.header.src.port := io.csrs.wdata }
      is (UInt(REMOTE_ADDR))  { csrs.header.dst.addr := io.csrs.wdata }
      is (UInt(REMOTE_PORT))  { csrs.header.dst.port := io.csrs.wdata }
      is (UInt(PHYS))         { csrs.phys := (io.csrs.wdata != UInt(0)) }
    }
  }

  io.csrs.rdata(SEGMENT_SIZE) := csrs.segment_size
  io.csrs.rdata(SRC_STRIDE)   := csrs.src_stride
  io.csrs.rdata(DST_STRIDE)   := csrs.dst_stride
  io.csrs.rdata(NSEGMENTS)    := csrs.nsegments
  io.csrs.rdata(LOCAL_ADDR)   := csrs.header.src.addr
  io.csrs.rdata(LOCAL_PORT)   := csrs.header.src.port
  io.csrs.rdata(REMOTE_ADDR)  := csrs.header.dst.addr
  io.csrs.rdata(REMOTE_PORT)  := csrs.header.dst.port
  io.csrs.rdata(PHYS)         := csrs.phys

  val src = Reg(UInt(width = paddrBits))
  val dst = Reg(UInt(width = paddrBits))
  val direction = Reg(Bool())

  val sender = Module(new SegmentSender)
  sender.io.csrs := csrs
  sender.io.cmd.valid := (state === s_req_send)
  sender.io.cmd.bits.src := src
  sender.io.cmd.bits.dst := dst
  sender.io.cmd.bits.direction := direction

  val tx = Module(new TileLinkDMATx)
  tx.io.net <> io.net.tx
  tx.io.route_error := io.net.ctrl.route_error(0)
  tx.io.phys := csrs.phys
  tx.io.cmd <> sender.io.dma

  val rx = Module(new TileLinkDMARx)
  rx.io.net <> io.net.rx
  rx.io.route_error := io.net.ctrl.route_error(1)
  rx.io.phys := csrs.phys

  val dmemArb = Module(new ClientUncachedTileLinkIOArbiter(2))
  dmemArb.io.in(0) <> tx.io.dmem
  dmemArb.io.in(1) <> rx.io.dmem
  dmemArb.io.out <> io.dmem

  val ptwArb = Module(new PTWArbiter(2))
  ptwArb.io.requestors(0) <> tx.io.dptw
  ptwArb.io.requestors(1) <> rx.io.dptw
  ptwArb.io.ptw <> io.dptw

  val cmd = Queue(io.cmd)
  cmd.ready := (state === s_idle)

  io.net.ctrl.cur_addr := csrs.header.src
  io.net.ctrl.switch_addr.ready := Bool(false)

  val nowork = csrs.nsegments === UInt(0) || csrs.segment_size === UInt(0)

  io.csrs.rdata(SENDER_ADDR) := rx.io.remote_addr.addr
  io.csrs.rdata(SENDER_PORT) := rx.io.remote_addr.port
  io.csrs.rdata(TX_ERROR)    := tx.io.error

  switch (state) {
    is (s_idle) {
      when (cmd.valid) {
        val funct = cmd.bits.inst.funct
        when (funct(6, 1) === UInt(0)) {
          dst := cmd.bits.rs1
          src := cmd.bits.rs2
          direction := !funct(0)
          state := s_req_send
        }
      }
    }
    is (s_req_send) {
      when (sender.io.cmd.ready) {
        state := s_idle
      }
    }
  }

  io.busy := (state != s_idle) || cmd.valid || sender.io.busy

  io.resp.valid := Bool(false)
  io.mem.req.valid := Bool(false)
  io.imem.acquire.valid := Bool(false)
  io.imem.grant.ready := Bool(false)
  io.iptw.req.valid := Bool(false)
  io.pptw.req.valid := Bool(false)
  io.interrupt := Bool(false)
}
