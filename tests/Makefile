CC=riscv64-unknown-linux-gnu-gcc
OBJDUMP=riscv64-unknown-linux-gnu-objdump
LDFLAGS=-static -nostdlib -Tlink.ld
CFLAGS=-O2 -Wall

BAREMETAL_TESTS=dma-test
KERNEL_TESTS=dma-test-linux

ELF=$(addsuffix .elf, $(BAREMETAL_TESTS))
HEX=$(addsuffix .hex, $(BAREMETAL_TESTS))
DUMP=$(addsuffix .dump, $(BAREMETAL_TESTS))

default: $(KERNEL_TESTS) $(ELF) $(HEX) $(DUMP)

%: %.c
	$(CC) -static $< -o $@

%.dump: %.elf
	$(OBJDUMP) -D $< > $@

%.hex: %.elf
	elf2hex 16 32768 $< > $@

%.elf: init.o %.o
	$(CC) $(LDFLAGS) $^ -o $@

%.o: %.c
	$(CC) $(CFLAGS) -c $<

%.o: %.S
	$(CC) $(CFLAGS) -c $<

clean:
	rm -f $(BAREMETAL_TESTS) *.dump *.elf *.hex *.o
