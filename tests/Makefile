CC=riscv64-unknown-linux-gnu-gcc
OBJDUMP=riscv64-unknown-linux-gnu-objdump
LDFLAGS=-static -nostdlib -Tlink.ld
CFLAGS=-O2 -Wall

TESTS=dma-test ptw-test

ELF=$(addsuffix .elf, $(TESTS))
HEX=$(addsuffix .hex, $(TESTS))
DUMP=$(addsuffix .dump, $(TESTS))

default: $(ELF) $(HEX) $(DUMP)

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
	rm -f *.dump *.elf *.hex *.o
