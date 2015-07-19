package dma

import Chisel._
import rocket.{RoCC, RoCCResponse, CoreParameters}
import uncore._

object CustomInstructions {
  val DMA_SCATTER_L2R = UInt(0)
  val DMA_GATHER_L2R  = UInt(1)
  val DMA_SCATTER_R2L = UInt(2)
  val DMA_GATHER_R2L  = UInt(3)
  val DMA_TRACK_RECV  = UInt(4)
  val DMA_POLL_RECV   = UInt(5)
  val DMA_SEND_IMM    = UInt(6)
}

object IRQCauses {
  val RX_FINISHED = UInt(0)
  val ADDR_SWITCH = UInt(1)

  val ncauses = 2
}

import CustomInstructions._

class IRQStatus extends Bundle {
  val addr_switch = Bool()
  val rx_finished = Bool()
}

class RoCCCSRFile extends DMABundle {
  val segment_size = UInt(width = paddrBits)
  val stride_size = UInt(width = paddrBits)
  val nsegments = UInt(width = paddrBits)
  val header = new RemoteHeader
  val phys = Bool()
  val ip = new IRQStatus
  val ie = new IRQStatus
  val cause = UInt(width = log2Up(IRQCauses.ncauses))
}

class TrackerCmd extends Bundle with CoreParameters {
  val start = UInt(width = paddrBits)
  val nbytes = UInt(width = paddrBits)
  val immediate = Bool()
}

object RxErrors {
  val noerror = Bits("b00")
  val notFinished = Bits("b01")
  val earlyFinish = Bits("b10")
  val rxNack = Bits("b11")
}

class RecvTracker extends DMAModule {
  val io = new Bundle {
    val cmd = Decoupled(new TrackerCmd).flip
    val acquire = Valid(new RemoteNetworkIO(new Acquire)).flip
    val grant = Valid(new RemoteNetworkIO(new Grant)).flip
    val error = Bits(OUTPUT, 2)
    val imm_data = UInt(OUTPUT, paddrBits)
    val src_addr = new RemoteAddress().asOutput
    val finished = Bool(OUTPUT)
    val inprogress = Bool(OUTPUT)
  }

  private val tlBlockOffset = tlBeatAddrBits + tlByteAddrBits

  val (s_idle :: s_wait_first :: s_wait_acquire :: s_wait_grant ::
       s_wait_imm :: Nil) = Enum(Bits(), 5)
  val state = Reg(init = s_idle)

  val acquire = io.acquire.bits.payload
  val grant = io.grant.bits.payload

  val is_put_block = acquire.a_type === Acquire.putBlockType
  val is_immediate = acquire.a_type === Acquire.immediateType
  val is_ack = grant.g_type === Grant.putAckType
  val is_nack = grant.g_type === Grant.nackType

  val first_block = Reg(UInt(width = tlBlockAddrBits))
  val end_block = Reg(UInt(width = tlBlockAddrBits))
  val xact_id = Reg(UInt(width = dmaXactIdBits))

  val right_xact_id = xact_id === acquire.client_xact_id
  val right_addr =
    first_block <= acquire.addr_block &&
    end_block > acquire.addr_block

  val last_grant = Reg(Bool())

  val error = Reg(init = RxErrors.noerror)
  io.error := error

  val imm_data = Reg(init = UInt(0, paddrBits))
  io.imm_data := imm_data

  val src_addr = Reg(new RemoteAddress)
  io.src_addr := src_addr

  switch (state) {
    is (s_idle) {
      when (io.cmd.valid) {
        val end_addr = io.cmd.bits.start + io.cmd.bits.nbytes
        first_block := io.cmd.bits.start(paddrBits - 1, tlBlockOffset)
        end_block := end_addr(paddrBits - 1, tlBlockOffset)
        error := RxErrors.notFinished
        when (io.cmd.bits.immediate) {
          state := s_wait_imm
        } .otherwise {
          state := s_wait_first
        }
      }
    }
    is (s_wait_first) {
      when (io.acquire.valid && is_put_block && right_addr) {
        xact_id := acquire.client_xact_id
        src_addr := io.acquire.bits.header.src
        state := s_wait_acquire
      }
    }
    is (s_wait_acquire) {
      when (io.acquire.valid && is_put_block) {
        when (!right_xact_id) {
          error := RxErrors.earlyFinish
          state := s_idle
        } .otherwise {
          last_grant := io.acquire.bits.last
          state := s_wait_grant
        }
      }
    }
    is (s_wait_grant) {
      when (io.grant.valid) {
        when (is_ack) {
          when (last_grant) {
            error := RxErrors.noerror
            state := s_idle
          } .otherwise {
            state := s_wait_acquire
          }
        } .elsewhen (is_nack) {
          error := RxErrors.rxNack
          state := s_idle
        }
      }
    }
    is (s_wait_imm) {
      when (io.acquire.valid && is_immediate) {
        imm_data := acquire.full_addr()
        src_addr := io.acquire.bits.header.src
        error := RxErrors.noerror
        state := s_idle
      }
    }
  }

  io.cmd.ready := (state === s_idle)

  val last_ready = Reg(next = io.cmd.ready)
  io.finished := io.cmd.ready && !last_ready

  io.inprogress := (state != s_idle) && (state != s_wait_first)
}

class CopyAccelerator extends RoCC with DMAParameters with TileLinkParameters {
  val src = Reg(UInt(width = paddrBits))
  val dst = Reg(UInt(width = paddrBits))
  val segments_left = Reg(UInt(width = paddrBits))
  val scatter = Reg(Bool())
  val direction = Reg(Bool())
  val immediate = Reg(Bool())
  val xact_id = Reg(init = UInt(0, dmaXactIdBits))

  val (s_idle :: s_req_tx :: s_wait_tx ::
    s_req_track :: s_resp :: Nil) = Enum(Bits(), 5)
  val state = Reg(init = s_idle)

  val initCsrs = new RoCCCSRFile
  initCsrs.segment_size := UInt(0)
  initCsrs.stride_size := UInt(0)
  initCsrs.nsegments := UInt(0)
  initCsrs.phys := Bool(false)
  initCsrs.header.dst.addr := UInt(0)
  initCsrs.header.dst.port := UInt(0)
  initCsrs.header.src.addr := UInt(0)
  initCsrs.header.src.port := UInt(0)
  initCsrs.ip.addr_switch := Bool(false)
  initCsrs.ip.rx_finished := Bool(false)
  initCsrs.ie.addr_switch := Bool(false)
  initCsrs.ie.rx_finished := Bool(false)
  initCsrs.cause := UInt(0)
  val csrs = Reg(init = initCsrs)

  when (io.csrs.wen) {
    switch (io.csrs.waddr) {
      is (UInt(0))  { csrs.segment_size := io.csrs.wdata }
      is (UInt(1))  { csrs.stride_size := io.csrs.wdata }
      is (UInt(2))  { csrs.nsegments := io.csrs.wdata }
      is (UInt(3))  { csrs.phys := (io.csrs.wdata != UInt(0)) }
      is (UInt(4))  { csrs.header.dst.addr := io.csrs.wdata }
      is (UInt(5))  { csrs.header.dst.port := io.csrs.wdata }
      is (UInt(6))  { csrs.header.src.addr := io.csrs.wdata }
      is (UInt(7))  { csrs.header.src.port := io.csrs.wdata }
      is (UInt(13)) { csrs.ip := new IRQStatus().fromBits(io.csrs.wdata) }
      is (UInt(14)) { csrs.ie := new IRQStatus().fromBits(io.csrs.wdata) }
    }
  }

  io.csrs.rdata(0) := csrs.segment_size
  io.csrs.rdata(1) := csrs.stride_size
  io.csrs.rdata(2) := csrs.nsegments
  io.csrs.rdata(3) := csrs.phys
  io.csrs.rdata(4) := csrs.header.dst.addr
  io.csrs.rdata(5) := csrs.header.dst.port
  io.csrs.rdata(6) := csrs.header.src.addr
  io.csrs.rdata(7) := csrs.header.src.port
  io.csrs.rdata(13) := csrs.ip.toBits
  io.csrs.rdata(14) := csrs.ie.toBits
  io.csrs.rdata(15) := csrs.cause

  val tx = Module(new TileLinkDMATx)
  tx.io.net <> io.net.tx
  tx.io.cmd.valid := (state === s_req_tx)
  tx.io.cmd.bits.src_start := src
  tx.io.cmd.bits.dst_start := dst
  tx.io.cmd.bits.nbytes := csrs.segment_size
  tx.io.cmd.bits.direction := direction
  tx.io.cmd.bits.header := csrs.header
  tx.io.cmd.bits.xact_id := xact_id
  tx.io.cmd.bits.immediate := immediate
  tx.io.phys := csrs.phys

  val rx = Module(new TileLinkDMARx)
  rx.io.net <> io.net.rx
  rx.io.phys := csrs.phys

  val tracker = Module(new RecvTracker)
  tracker.io.acquire.bits := io.net.rx.acquire.bits
  tracker.io.acquire.valid := io.net.rx.acquire.fire()
  tracker.io.grant.bits := io.net.rx.grant.bits
  tracker.io.grant.valid := io.net.rx.grant.fire()
  tracker.io.cmd.valid := (state === s_req_track)
  tracker.io.cmd.bits.start := dst
  tracker.io.cmd.bits.nbytes := csrs.segment_size
  tracker.io.cmd.bits.immediate := immediate

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

  val resp_rd = Reg(Bits(width = 5))
  val resp_data = Reg(Bits(width = xLen))
  val resp_wanted = Reg(init = Bool(false))

  val resp = Decoupled(new RoCCResponse)
  io.resp <> Queue(resp)
  resp.valid := (state === s_resp)
  resp.bits.data := resp_data
  resp.bits.rd := resp_rd

  val small_step = csrs.segment_size
  val large_step = csrs.segment_size + csrs.stride_size

  io.net.ctrl.cur_addr := csrs.header.src
  io.net.ctrl.switch_addr.ready := !tracker.io.inprogress &&
                                   !rx.io.busy && tx.io.cmd.ready

  val nowork = csrs.nsegments === UInt(0) || csrs.segment_size === UInt(0)

  io.csrs.rdata(8) := io.net.ctrl.switch_addr.bits.addr
  io.csrs.rdata(9) := io.net.ctrl.switch_addr.bits.port
  io.csrs.rdata(10) := tracker.io.src_addr.addr
  io.csrs.rdata(11) := tracker.io.src_addr.port
  io.csrs.rdata(12) := tracker.io.imm_data

  switch (state) {
    is (s_idle) {
      when (cmd.valid) {
        val funct = cmd.bits.inst.funct
        when (funct(6, 2) === UInt(0)) {
          src := cmd.bits.rs1
          dst := cmd.bits.rs2
          segments_left := csrs.nsegments
          scatter := !funct(0)
          direction := !funct(1)
          immediate := Bool(false)
          resp_rd := cmd.bits.inst.rd
          resp_wanted := cmd.bits.inst.xd
          // If nsegments or segment_size is 0, there's nothing to do
          when (!nowork) {
            state := s_req_tx
          } .elsewhen (cmd.bits.inst.xd) {
            state := s_resp
          }
        } .elsewhen (funct === DMA_TRACK_RECV) {
          dst := cmd.bits.rs1
          immediate := cmd.bits.inst.rs2(0)
          state := s_req_track
        } .elsewhen (funct === DMA_POLL_RECV) {
          resp_rd := cmd.bits.inst.rd
          resp_data := tracker.io.error
          state := s_resp
        } .elsewhen (funct === DMA_SEND_IMM) {
          dst := cmd.bits.rs1
          immediate := Bool(true)
          direction := Bool(true)
          resp_rd := cmd.bits.inst.rd
          resp_wanted := cmd.bits.inst.xd
          state := s_req_tx
        }
      }
    }
    is (s_req_tx) {
      when (tx.io.cmd.ready) {
        when (scatter) {
          src := src + small_step
          dst := dst + large_step
        } .otherwise {
          src := src + large_step
          dst := dst + small_step
        }
        when (immediate || segments_left === UInt(1)) {
          state := s_wait_tx
        }
        segments_left := segments_left - UInt(1)
      }
    }
    is (s_wait_tx) {
      when (tx.io.cmd.ready) {
        when (resp_wanted) {
          resp_data := tx.io.error
          state := s_resp
        } .otherwise {
          state := s_idle
        }
        xact_id := xact_id + UInt(1)
      }
    }
    is (s_req_track) {
      when (tracker.io.cmd.ready) {
        state := s_idle
      }
    }
    is (s_resp) {
      when (resp.ready) {
        state := s_idle
      }
    }
  }

  when (tracker.io.finished) {
    csrs.ip.rx_finished := Bool(true)
    csrs.cause := IRQCauses.RX_FINISHED
  }

  when (io.net.ctrl.switch_addr.fire()) {
    csrs.header.src := io.net.ctrl.switch_addr.bits
    csrs.ip.addr_switch := Bool(true)
    csrs.cause := IRQCauses.ADDR_SWITCH
  }

  io.busy := (state != s_idle) || cmd.valid
  io.interrupt := (csrs.ip.toBits & csrs.ie.toBits).orR

  io.mem.req.valid := Bool(false)
  io.imem.acquire.valid := Bool(false)
  io.imem.grant.ready := Bool(false)
  io.iptw.req.valid := Bool(false)
  io.pptw.req.valid := Bool(false)
}
