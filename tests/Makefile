CC=riscv64-unknown-linux-gnu-gcc
LDFLAGS=-static -nostdlib -Tlink.ld
CFLAGS=-O2 -Wall

TESTS=dma-test accum-test

ELF=$(addsuffix .elf, $(TESTS))
HEX=$(addsuffix .hex, $(TESTS))

default: $(ELF) $(HEX)

%.hex: %.elf
	elf2hex 16 32768 $< > $@

%.elf: init.o %.o
	$(CC) $(LDFLAGS) $^ -o $@

%.o: %.c
	$(CC) $(CFLAGS) -c $<

%.o: %.S
	$(CC) $(CFLAGS) -c $<

clean:
	rm -f *.elf *.hex *.o
