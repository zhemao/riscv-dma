package dma

import Chisel._
import rocket.{RoCC, RoCCResponse, CoreParameters}
import uncore._

object CustomInstructions {
  val DMA_PUT         = UInt(0)
  val DMA_GET         = UInt(1)
  val DMA_TRACK_RECV  = UInt(2)
  val DMA_POLL_RECV   = UInt(3)
  val DMA_SEND_IMM    = UInt(4)
}

object IRQCauses {
  val RX_FINISHED = UInt(0)
  val ADDR_SWITCH = UInt(1)

  val ncauses = 2
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
  val SWITCH_ADDR  = 10
  val SWITCH_PORT  = 11
  val IMM_DATA     = 12
  val PHYS         = 13
  val IRQ_CAUSE    = 14
}

import DMACSRs._

class DMACSRFile extends DMABundle {
  val segment_size = UInt(width = paddrBits)
  val src_stride = UInt(width = paddrBits)
  val dst_stride = UInt(width = paddrBits)
  val nsegments = UInt(width = paddrBits)
  val header = new RemoteHeader
  val phys = Bool()
  val cause = UInt(width = log2Up(IRQCauses.ncauses))
}

class TrackerCmd extends Bundle with CoreParameters {
  val start = UInt(width = paddrBits)
  val nbytes = UInt(width = paddrBits)
  val immediate = Bool()
}

object RxErrors {
  val noerror = Bits("b000")
  val notStarted = Bits("b001")
  val notFinished = Bits("b010")
  val earlyFinish = Bits("b011")
  val rxNack = Bits("b100")
  val noRoute = Bits("b101")
}

class RecvTracker extends DMAModule {
  val io = new Bundle {
    val cmd = Decoupled(new TrackerCmd).flip
    val acquire = Valid(new RemoteNetworkIO(new Acquire)).flip
    val grant = Valid(new RemoteNetworkIO(new Grant)).flip
    val error = RxErrors.noerror.clone.asOutput
    val route_error = Bool(INPUT)
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
        error := RxErrors.notStarted
        when (io.cmd.bits.immediate) {
          state := s_wait_imm
        } .otherwise {
          state := s_wait_first
        }
      }
    }
    is (s_wait_first) {
      when (io.acquire.fire() && is_put_block && right_addr) {
        xact_id := acquire.client_xact_id
        src_addr := io.acquire.bits.header.src
        error := RxErrors.notFinished
        state := s_wait_grant
      }
    }
    is (s_wait_acquire) {
      when (io.acquire.fire() && is_put_block) {
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
      when (io.route_error) {
        error := RxErrors.noRoute
        state := s_idle
      } .elsewhen (io.grant.fire()) {
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
      when (io.acquire.fire() && is_immediate) {
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
  val direction = Reg(Bool())
  val immediate = Reg(Bool())
  val xact_id = Reg(init = UInt(0, dmaXactIdBits))

  val (s_idle :: s_req_tx :: s_wait_tx ::
    s_req_track :: s_resp :: Nil) = Enum(Bits(), 5)
  val state = Reg(init = s_idle)

  val initCsrs = new DMACSRFile
  initCsrs.segment_size := UInt(0)
  initCsrs.dst_stride := UInt(0)
  initCsrs.src_stride := UInt(0)
  initCsrs.nsegments := UInt(0)
  initCsrs.phys := Bool(false)
  initCsrs.header.dst.addr := UInt(0)
  initCsrs.header.dst.port := UInt(0)
  initCsrs.header.src.addr := UInt(0)
  initCsrs.header.src.port := UInt(0)
  initCsrs.cause := UInt(0)
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
  io.csrs.rdata(IRQ_CAUSE)    := csrs.cause
  io.csrs.rdata(PHYS)         := csrs.phys

  val tx = Module(new TileLinkDMATx)
  tx.io.net <> io.net.tx
  tx.io.route_error := io.net.ctrl.route_error(0)
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
  rx.io.route_error := io.net.ctrl.route_error(1)
  rx.io.phys := csrs.phys

  val tracker = Module(new RecvTracker)
  tracker.io.acquire.bits := io.net.rx.acquire.bits
  tracker.io.acquire.valid := io.net.rx.acquire.fire()
  tracker.io.grant.bits := io.net.rx.grant.bits
  tracker.io.grant.valid := io.net.rx.grant.fire()
  tracker.io.route_error := io.net.ctrl.route_error(1)
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

  io.net.ctrl.cur_addr := csrs.header.src
  io.net.ctrl.switch_addr.ready := !tracker.io.inprogress &&
                                   !rx.io.busy && tx.io.cmd.ready

  val nowork = csrs.nsegments === UInt(0) || csrs.segment_size === UInt(0)

  io.csrs.rdata(SWITCH_ADDR) := io.net.ctrl.switch_addr.bits.addr
  io.csrs.rdata(SWITCH_PORT) := io.net.ctrl.switch_addr.bits.port
  io.csrs.rdata(SENDER_ADDR) := tracker.io.src_addr.addr
  io.csrs.rdata(SENDER_PORT) := tracker.io.src_addr.port
  io.csrs.rdata(IMM_DATA)    := tracker.io.imm_data

  switch (state) {
    is (s_idle) {
      when (cmd.valid) {
        val funct = cmd.bits.inst.funct
        when (funct(6, 1) === UInt(0)) {
          src := cmd.bits.rs1
          dst := cmd.bits.rs2
          segments_left := csrs.nsegments
          direction := !funct(0)
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
        src := src + csrs.src_stride
        dst := dst + csrs.dst_stride
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

  val tracker_irq = tracker.io.finished
  val switch_irq = io.net.ctrl.switch_addr.fire()

  when (tracker_irq) {
    csrs.cause := IRQCauses.RX_FINISHED
  }

  when (switch_irq) {
    csrs.cause := IRQCauses.ADDR_SWITCH
  }

  io.busy := (state != s_idle) || cmd.valid
  io.interrupt := Reg(next = tracker_irq || switch_irq)

  io.mem.req.valid := Bool(false)
  io.imem.acquire.valid := Bool(false)
  io.imem.grant.ready := Bool(false)
  io.iptw.req.valid := Bool(false)
  io.pptw.req.valid := Bool(false)
}
