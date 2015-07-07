CC=riscv64-unknown-linux-gnu-gcc
OBJDUMP=riscv64-unknown-linux-gnu-objdump
BAREMETAL_LDFLAGS=-static -nostdlib -Tlink.ld
KERNEL_LDFLAGS=-static -lpthread
CFLAGS=-O2 -Wall -march=IMAFD -Wa,-march=IMAFDXcustom

BAREMETAL_TESTS=simple-test
KERNEL_TESTS=matrix-test

ELF=$(addsuffix .elf, $(BAREMETAL_TESTS))
HEX=$(addsuffix .hex, $(BAREMETAL_TESTS))
DUMP=$(addsuffix .dump, $(KERNEL_TESTS) $(BAREMETAL_TESTS))

default: $(KERNEL_TESTS) $(ELF) $(HEX) $(DUMP)

%: %.c dma-ext.h
	$(CC) $(CFLAGS) $(KERNEL_LDFLAGS) $< -o $@

%.dump: %.elf
	$(OBJDUMP) -D $< > $@

%.dump: %
	$(OBJDUMP) -D $< > $@

%.hex: %.elf
	elf2hex 16 32768 $< > $@

%.elf: %.o init.o dma-ext.h
	$(CC) $(BAREMETAL_LDFLAGS) $< init.o -o $@

%.o: %.c
	$(CC) $(CFLAGS) -c $<

%.o: %.S
	$(CC) $(CFLAGS) -c $<

clean:
	rm -f $(KERNEL_TESTS) *.dump *.elf *.hex *.o
