package dma

import Chisel._
import rocket.{RoCC, RoCCResponse, CoreParameters}
import uncore.{TileLinkParameters, ClientUncachedTileLinkIOArbiter}

object CustomInstructions {
  val DMA_SCATTER_L2R = UInt(0)
  val DMA_GATHER_L2R  = UInt(1)
  val DMA_SCATTER_R2L = UInt(2)
  val DMA_GATHER_R2L  = UInt(3)
}

import CustomInstructions._

class RoCCCSR extends DMABundle {
  val segment_size = UInt(width = paddrBits)
  val stride_size = UInt(width = paddrBits)
  val nsegments = UInt(width = paddrBits)
  val remote_id = UInt(width = hostIdBits)
  val phys = Bool()
}

class CopyAccelerator extends RoCC with DMAParameters with TileLinkParameters {
  val src = Reg(UInt(width = paddrBits))
  val dst = Reg(UInt(width = paddrBits))
  val segments_left = Reg(UInt(width = paddrBits))
  val scatter = Reg(Bool())
  val direction = Reg(Bool())

  val (s_idle :: s_req :: s_wait_dma :: s_resp :: Nil) = Enum(Bits(), 4)
  val state = Reg(init = s_idle)

  val csrs = new RoCCCSR
  csrs.segment_size := io.csrs(0)
  csrs.stride_size := io.csrs(1)
  csrs.nsegments := io.csrs(2)
  csrs.remote_id := io.csrs(3)
  csrs.phys := io.csrs(4) != UInt(0)

  val tx = Module(new TileLinkDMATx)
  tx.io.net <> io.net.tx
  tx.io.cmd.valid := (state === s_req)
  tx.io.cmd.bits.src_start := src
  tx.io.cmd.bits.dst_start := dst
  tx.io.cmd.bits.nbytes := csrs.segment_size
  tx.io.cmd.bits.direction := direction
  tx.io.cmd.bits.remote_id := csrs.remote_id
  tx.io.host_id := io.host_id
  tx.io.phys := csrs.phys

  val rx = Module(new TileLinkDMARx)
  rx.io.net <> io.net.rx
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

  val inst_rd = Reg(Bits(width = 5))
  val resp_wanted = Reg(init = Bool(false))

  val resp = Decoupled(new RoCCResponse)
  io.resp <> Queue(resp)
  resp.valid := (state === s_resp)
  resp.bits.data := tx.io.error
  resp.bits.rd := inst_rd

  val small_step = csrs.segment_size
  val large_step = csrs.segment_size + csrs.stride_size

  switch (state) {
    is (s_idle) {
      when (cmd.valid) {
        val funct = cmd.bits.inst.funct
        src := cmd.bits.rs1
        dst := cmd.bits.rs2
        segments_left := csrs.nsegments
        scatter := !funct(0)
        direction := !funct(1)
        inst_rd := cmd.bits.inst.rd
        resp_wanted := cmd.bits.inst.xd
        // If nsegments or segment_size is 0, there's nothing to do
        when (csrs.nsegments != UInt(0) && csrs.segment_size != UInt(0)) {
          state := s_req
        } .elsewhen (cmd.bits.inst.xd) {
          state := s_resp
        }
      }
    }
    is (s_req) {
      when (tx.io.cmd.ready) {
        when (scatter) {
          src := src + small_step
          dst := dst + large_step
        } .otherwise {
          src := src + large_step
          dst := dst + small_step
        }
        when (segments_left === UInt(1)) {
          state := s_wait_dma
        }
        segments_left := segments_left - UInt(1)
      }
    }
    is (s_wait_dma) {
      when (tx.io.cmd.ready) {
        when (resp_wanted) {
          state := s_resp
        } .otherwise {
          state := s_idle
        }
      }
    }
    is (s_resp) {
      when (resp.ready) {
        state := s_idle
      }
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
