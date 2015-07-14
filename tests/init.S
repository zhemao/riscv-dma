.text
.align 4
.global _start, twobeat_src, twobeat_dst

_start:
  // set accelerator to physical mode
  li a0, 1
  csrw 0x807, a0
  // don't proceed if hartid isn't 0
  csrr a0, mhartid
  bnez a0, extra_core
  // enable extensions
  lui a0, 0x4
  csrs sstatus, a0
  // init stack pointer
  li sp, 0x10000000
  // call C code
  jal main
  // write return code to mtohost
  slli a0, a0, 1
  ori a0, a0, 1
_finish:
  csrw mtohost, a0
  j _finish
extra_core:
  j extra_core
