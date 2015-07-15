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
  val DMA_READ_IMM    = UInt(7)
}

import CustomInstructions._

class RoCCCSR extends DMABundle {
  val segment_size = UInt(width = paddrBits)
  val stride_size = UInt(width = paddrBits)
  val nsegments = UInt(width = paddrBits)
  val header = new RemoteHeader
  val phys = Bool()
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

class ValidLastIO[T <: Data](dType: T) extends Bundle {
  val bits = dType.clone.asOutput
  val valid = Bool(OUTPUT)
  val last = Bool(OUTPUT)
  override def clone = new ValidLastIO(dType).asInstanceOf[this.type]
}

object ValidLast {
  def apply[T <: Data](dType: T) = new ValidLastIO(dType)
}

class RecvTracker extends DMAModule {
  val io = new Bundle {
    val cmd = Decoupled(new TrackerCmd).flip
    val acquire = ValidLast(new Acquire).flip
    val grant = Valid(new Grant).flip
    val error = Bits(OUTPUT, 2)
    val imm_data = UInt(OUTPUT, paddrBits)
  }

  private val tlBlockOffset = tlBeatAddrBits + tlByteAddrBits

  val (s_idle :: s_wait_first :: s_wait_acquire :: s_wait_grant ::
       s_wait_imm :: Nil) = Enum(Bits(), 5)
  val state = Reg(init = s_idle)

  val is_put_block = io.acquire.bits.a_type === Acquire.putBlockType
  val is_immediate = io.acquire.bits.a_type === Acquire.immediateType
  val is_ack = io.grant.bits.g_type === Grant.putAckType
  val is_nack = io.grant.bits.g_type === Grant.nackType

  val first_block = Reg(UInt(width = tlBlockAddrBits))
  val end_block = Reg(UInt(width = tlBlockAddrBits))
  val xact_id = Reg(UInt(width = dmaXactIdBits))

  val right_xact_id = xact_id === io.acquire.bits.client_xact_id
  val right_addr =
    first_block <= io.acquire.bits.addr_block &&
    end_block > io.acquire.bits.addr_block

  val error = Reg(Bits(width = 2))
  io.error := error

  val imm_data = Reg(UInt(width = paddrBits))
  io.imm_data := imm_data

  val last_grant = Reg(Bool())

  switch (state) {
    is (s_idle) {
      when (io.cmd.valid) {
        val end_addr = io.cmd.bits.start + io.cmd.bits.nbytes
        first_block := io.cmd.bits.start(paddrBits - 1, tlBlockOffset)
        end_block := end_addr(paddrBits - 1, tlBlockOffset)
        error := RxErrors.noerror
        when (io.cmd.bits.immediate) {
          state := s_wait_imm
        } .otherwise {
          state := s_wait_first
        }
      }
    }
    is (s_wait_first) {
      when (io.acquire.valid && is_put_block && right_addr) {
        xact_id := io.acquire.bits.client_xact_id
        state := s_wait_acquire
      }
    }
    is (s_wait_acquire) {
      when (io.acquire.valid && is_put_block) {
        when (!right_xact_id) {
          error := RxErrors.earlyFinish
          state := s_idle
        } .otherwise {
          last_grant := io.acquire.last
          state := s_wait_grant
        }
      }
    }
    is (s_wait_grant) {
      when (io.grant.valid) {
        when (is_ack) {
          when (last_grant) {
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
        imm_data := io.acquire.bits.full_addr()
        state := s_idle
      }
    }
  }

  io.cmd.ready := (state === s_idle)
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

  val csrs = new RoCCCSR
  csrs.segment_size := io.csrs(0)
  csrs.stride_size := io.csrs(1)
  csrs.nsegments := io.csrs(2)
  csrs.header.dst.addr := io.csrs(3)
  csrs.header.dst.port := io.csrs(4)
  csrs.header.src.addr := io.csrs(5)
  csrs.header.src.port := io.csrs(6)
  csrs.phys := io.csrs(7) != UInt(0)

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
  tracker.io.acquire.bits := io.net.rx.acquire.bits.payload
  tracker.io.acquire.valid := io.net.rx.acquire.fire()
  tracker.io.acquire.last := io.net.rx.acquire.bits.last
  tracker.io.grant.bits := io.net.rx.grant.bits.payload
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

  io.addr := csrs.header.src

  val nowork = csrs.nsegments === UInt(0) || csrs.segment_size === UInt(0)

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
          resp_data := Mux(tracker.io.cmd.ready,
            tracker.io.error, RxErrors.notFinished)
          state := s_resp
        } .elsewhen (funct === DMA_SEND_IMM) {
          dst := cmd.bits.rs1
          immediate := Bool(true)
          direction := Bool(true)
          resp_rd := cmd.bits.inst.rd
          resp_wanted := cmd.bits.inst.xd
          state := s_req_tx
        } .elsewhen (funct === DMA_READ_IMM) {
          resp_rd := cmd.bits.inst.rd
          resp_data := tracker.io.imm_data
          state := s_resp
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

  io.busy := (state != s_idle) || cmd.valid
  io.interrupt := Bool(false)

  io.mem.req.valid := Bool(false)
  io.imem.acquire.valid := Bool(false)
  io.imem.grant.ready := Bool(false)
  io.iptw.req.valid := Bool(false)
  io.pptw.req.valid := Bool(false)
}
