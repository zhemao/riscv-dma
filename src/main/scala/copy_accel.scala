package dma

import Chisel._
import rocket.{RoCC, RoCCResponse, CoreParameters}
import uncore.{TileLinkParameters, ClientUncachedTileLinkIOArbiter}

object CustomInstructions {
  val DMA_SCATTER = UInt(0)
  val DMA_GATHER = UInt(1)
}

import CustomInstructions._

class RoCCCSR extends Bundle with CoreParameters {
  val segment_size = UInt(width = paddrBits)
  val stride_size = UInt(width = paddrBits)
  val nsegments = UInt(width = paddrBits)
  val phys = Bool()
}

class CopyAccelerator extends RoCC with DMAParameters
    with CoreParameters with TileLinkParameters {

  val src = Reg(UInt(width = paddrBits))
  val dst = Reg(UInt(width = paddrBits))
  val segments_left = Reg(UInt(width = paddrBits))
  val scatter = Reg(Bool())

  val (s_idle :: s_req :: s_wait_dma :: s_error :: Nil) = Enum(Bits(), 4)
  val state = Reg(init = s_idle)

  val cmd = Queue(io.cmd)
  cmd.ready := (state === s_idle)

  io.resp.valid := Bool(false)

  val csrs = new RoCCCSR
  csrs.segment_size := io.csrs(0)
  csrs.stride_size := io.csrs(1)
  csrs.nsegments := io.csrs(2)
  csrs.phys := io.csrs(3) != UInt(0)

  val tx = Module(new TileLinkDMATx)
  tx.io.cmd.valid := (state === s_req)
  tx.io.cmd.bits.src_start := src
  tx.io.cmd.bits.dst_start := dst
  tx.io.cmd.bits.nbytes := csrs.segment_size
  tx.io.phys := csrs.phys

  val rx = Module(new TileLinkDMARx)
  rx.io.phys := csrs.phys

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

  val small_step = csrs.segment_size
  val large_step = csrs.segment_size + csrs.stride_size

  switch (state) {
    is (s_idle) {
      when (cmd.valid) {
        val funct = cmd.bits.inst.funct
        when (funct === DMA_SCATTER || funct === DMA_GATHER) {
          src := cmd.bits.rs1
          dst := cmd.bits.rs2
          segments_left := csrs.nsegments
          scatter := (funct === DMA_SCATTER)
          // If nsegments or segment_size is 0, there's nothing to do
          when (csrs.nsegments != UInt(0) && csrs.segment_size != UInt(0)) {
            state := s_req
          }
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
