CC=riscv64-unknown-linux-gnu-gcc
OBJDUMP=riscv64-unknown-linux-gnu-objdump
BAREMETAL_LDFLAGS=-static -nostdlib -Tlink.ld
PK_LDFLAGS=-static
LINUX_LDFLAGS=-pthread -lrt
CFLAGS=-O2 -Wall

BAREMETAL_TESTS=simple-test error-test matrix-test
LINUX_TESTS=lnx-matrix-test lnx-simple-test client-test server-test
PK_TESTS=pk-simple-test pk-matrix-test
ALL_TESTS=$(BAREMETAL_TESTS) $(LINUX_TESTS) $(PK_TESTS)

ELF=$(addsuffix .elf, $(BAREMETAL_TESTS))
HEX=$(addsuffix .hex, $(BAREMETAL_TESTS))
DUMP=$(addsuffix .dump, $(BAREMETAL_TESTS))

NOKERN_OBJS=$(addsuffix .o, $(BAREMETAL_TESTS) $(PK_TESTS))
KERNEL_OBJS=$(addsuffix .o, $(LINUX_TESTS))

default: $(LINUX_TESTS) $(PK_TESTS) $(ELF) $(HEX) $(DUMP)

$(LINUX_TESTS): %: %.o barrier.o
	$(CC) $(CFLAGS) $< barrier.o $(LINUX_LDFLAGS) -o $@

$(PK_TESTS): %: %.o
	$(CC) $(CFLAGS) $< $(PK_LDFLAGS) -o $@

$(DUMP): %.dump: %.elf
	$(OBJDUMP) -D $< > $@

$(HEX): %.hex: %.elf
	elf2hex 16 32768 $< > $@

$(ELF): %.elf: %.o init.o dma-ext.h
	$(CC) $(BAREMETAL_LDFLAGS) init.o $< -o $@

$(NOKERN_OBJS): %.o: %.c dma-ext.h
	$(CC) $(CFLAGS) -c $<

$(KERNEL_OBJS): %.o: %.c dma-ext.h dma-syscalls.h
	$(CC) $(CFLAGS) -c $<

%.o: %.c %.h
	$(CC) $(CFLAGS) -c $<

%.o: %.S
	$(CC) $(CFLAGS) -c $<

clean:
	rm -f $(PK_TESTS) $(LINUX_TESTS) *.dump *.elf *.hex *.o
