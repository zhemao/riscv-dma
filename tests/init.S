.text
.align 4
.global _start, twobeat_src, twobeat_dst

_start:
  // enable extensions
  lui a0, 0x4
  csrs sstatus, a0
  // set accelerator to physical mode
  li a0, 1
  custom0 0, a0, 0, 2
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
