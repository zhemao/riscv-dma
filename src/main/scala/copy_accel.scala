package dma

import Chisel._
import rocket.{RoCC, RoCCResponse, CoreParameters}
import uncore._

object CustomInstructions {
  val DMA_PUT         = UInt(0)
  val DMA_GET         = UInt(1)
  val DMA_TRACK_RECV  = UInt(2)
  val DMA_ERRCODE     = UInt(3)
  val DMA_SEND_IMM    = UInt(4)
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

class TrackerCmd extends Bundle with CoreParameters {
  val start = UInt(width = paddrBits)
  val nbytes = UInt(width = paddrBits)
  val immediate = Bool()
  val direction = Bool()
}

object RxErrors {
  val noerror = Bits("b00")
  val rxNack = Bits("b01")
  val noRoute = Bits("b10")
}

class RecvTracker extends DMAModule {
  val io = new Bundle {
    val cmd = Decoupled(new TrackerCmd).flip
    val acquire = Valid(new RemoteNetworkIO(new Acquire)).flip
    val grant = Valid(new RemoteNetworkIO(new Grant)).flip
    val error = RxErrors.noerror.cloneType.asOutput
    val route_error = Bool(INPUT)
    val src_addr = new RemoteAddress().asOutput
    val busy = Bool(OUTPUT)
  }

  private val tlBlockOffset = tlBeatAddrBits + tlByteAddrBits

  val (s_idle :: s_wait_first :: s_wait_acquire :: s_wait_grant ::
       s_wait_imm :: Nil) = Enum(Bits(), 5)
  val state = Reg(init = s_idle)

  val cmd = Queue(io.cmd)
  cmd.ready := (state === s_idle)

  val acquire = io.acquire.bits.payload
  val grant = io.grant.bits.payload

  val is_put_block = acquire.a_type === Acquire.putBlockType
  val is_get_block = acquire.a_type === Acquire.getBlockType
  val is_immediate = acquire.a_type === Acquire.immediateType

  val is_ack = grant.g_type === Grant.putAckType ||
               grant.g_type === Grant.getDataBlockType
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

  val src_addr = Reg(new RemoteAddress)
  io.src_addr := src_addr

  val immediate = Reg(Bool())
  val direction = Reg(Bool())

  switch (state) {
    is (s_idle) {
      when (cmd.valid) {
        val end_addr = cmd.bits.start + cmd.bits.nbytes
        first_block := cmd.bits.start(paddrBits - 1, tlBlockOffset)
        end_block := end_addr(paddrBits - 1, tlBlockOffset)
        error := RxErrors.noerror
        immediate := cmd.bits.immediate
        direction := cmd.bits.direction
        state := s_wait_first
      }
    }
    is (s_wait_first) {
      when (io.acquire.fire()) {
        xact_id := acquire.client_xact_id
        src_addr := io.acquire.bits.header.src
        last_grant := io.acquire.bits.last

        val correct_acquire =
          (immediate && is_immediate) ||
          (!immediate && right_addr &&
            (direction && is_put_block || !direction && is_get_block))

        when (correct_acquire) {
          state := s_wait_grant
        }
      }
    }
    is (s_wait_acquire) {
      val correct_acquire = right_xact_id &&
        (direction && is_put_block || !direction && is_get_block)

      when (io.acquire.fire() && correct_acquire) {
        last_grant := io.acquire.bits.last
        state := s_wait_grant
      }
    }
    is (s_wait_grant) {
      when (io.route_error) {
        error := RxErrors.noRoute
        state := s_idle
      } .elsewhen (io.grant.fire()) {
        when (is_ack) {
          when (immediate || last_grant) {
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
  }

  io.busy := (state != s_idle) || cmd.valid
}

class SegmentSenderCommand extends DMABundle {
  val dst = UInt(width = paddrBits)
  val src = UInt(width = paddrBits)
  val direction = Bool()
  val immediate = Bool()
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
  val immediate = Reg(Bool())
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
  io.dma.bits.immediate := immediate

  val nowork = !cmd.bits.immediate &&
    (io.csrs.segment_size === UInt(0) || io.csrs.nsegments === UInt(0))

  switch (state) {
    is (s_idle) {
      when (cmd.valid) {
        dst := cmd.bits.dst
        src := cmd.bits.src
        direction := cmd.bits.direction
        immediate := cmd.bits.immediate
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
        when (immediate || segments_left === UInt(1)) {
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
  val immediate = Reg(Bool())

  val sender = Module(new SegmentSender)
  sender.io.csrs := csrs
  sender.io.cmd.valid := (state === s_req_send)
  sender.io.cmd.bits.src := src
  sender.io.cmd.bits.dst := dst
  sender.io.cmd.bits.direction := direction
  sender.io.cmd.bits.immediate := immediate

  val tx = Module(new TileLinkDMATx)
  tx.io.net <> io.net.tx
  tx.io.route_error := io.net.ctrl.route_error(0)
  tx.io.phys := csrs.phys
  tx.io.cmd <> sender.io.dma

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
  tracker.io.cmd.bits.nbytes := src
  tracker.io.cmd.bits.immediate := immediate
  tracker.io.cmd.bits.direction := direction

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
  io.net.ctrl.switch_addr.ready := Bool(false)

  val nowork = csrs.nsegments === UInt(0) || csrs.segment_size === UInt(0)

  io.csrs.rdata(SWITCH_ADDR) := io.net.ctrl.switch_addr.bits.addr
  io.csrs.rdata(SWITCH_PORT) := io.net.ctrl.switch_addr.bits.port
  io.csrs.rdata(SENDER_ADDR) := tracker.io.src_addr.addr
  io.csrs.rdata(SENDER_PORT) := tracker.io.src_addr.port
  io.csrs.rdata(IMM_DATA)    := rx.io.imm_data

  switch (state) {
    is (s_idle) {
      when (cmd.valid) {
        val funct = cmd.bits.inst.funct
        when (funct(6, 1) === UInt(0)) {
          dst := cmd.bits.rs1
          src := cmd.bits.rs2
          direction := !funct(0)
          immediate := Bool(false)
          state := s_req_send
        } .elsewhen (funct === DMA_SEND_IMM) {
          dst := cmd.bits.rs1
          immediate := Bool(true)
          direction := Bool(true)
          state := s_req_send
        } .elsewhen (funct === DMA_TRACK_RECV) {
          dst := cmd.bits.rs1
          src := cmd.bits.rs2
          direction := !cmd.bits.inst.rd(0)
          immediate := cmd.bits.inst.rd(1)
          state := s_req_track
        } .elsewhen (funct === DMA_ERRCODE) {
          resp_rd := cmd.bits.inst.rd
          // rs1 = 0 means get recv error
          // rs1 = 1 means get send error
          resp_data := Mux(cmd.bits.inst.rs1(0), tx.io.error, tracker.io.error)
          state := s_resp
        }
      }
    }
    is (s_req_send) {
      when (sender.io.cmd.ready) {
        state := s_idle
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

  io.busy := (state != s_idle) || cmd.valid ||
             tracker.io.busy || sender.io.busy

  io.mem.req.valid := Bool(false)
  io.imem.acquire.valid := Bool(false)
  io.imem.grant.ready := Bool(false)
  io.iptw.req.valid := Bool(false)
  io.pptw.req.valid := Bool(false)
  io.interrupt := Bool(false)
}
