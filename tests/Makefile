CC=riscv64-unknown-linux-gnu-gcc
OBJDUMP=riscv64-unknown-linux-gnu-objdump
LDFLAGS=-static -nostdlib -Tlink.ld
CFLAGS=-O2 -Wall

BAREMETAL_TESTS=simple-test
KERNEL_TESTS=matrix-test ipi-test

ELF=$(addsuffix .elf, $(BAREMETAL_TESTS))
HEX=$(addsuffix .hex, $(BAREMETAL_TESTS))
DUMP=$(addsuffix .dump, $(KERNEL_TESTS) $(BAREMETAL_TESTS))

default: $(KERNEL_TESTS) $(ELF) $(HEX) $(DUMP)

%: %.c dma-ext.h
	$(CC) $(CFLAGS) -static $< -o $@

%.dump: %.elf
	$(OBJDUMP) -D $< > $@

%.dump: %
	$(OBJDUMP) -D $< > $@

%.hex: %.elf
	elf2hex 16 32768 $< > $@

%.elf: %.o init.o dma-ext.h
	$(CC) $(LDFLAGS) $< init.o -o $@

%.o: %.c
	$(CC) $(CFLAGS) -c $<

%.o: %.S
	$(CC) $(CFLAGS) -c $<

clean:
	rm -f $(KERNEL_TESTS) *.dump *.elf *.hex *.o
