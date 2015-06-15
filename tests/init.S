.text
.align 4
.global _start, twobeat_src, twobeat_dst

_start:
  // enable extensions
  li a0, 0xC000
  csrs mstatus, a0
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
