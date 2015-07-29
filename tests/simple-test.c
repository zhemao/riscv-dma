#include "dma-ext.h"

#define ARR_SIZE  64
#define COPY_SIZE 32
#define SRC_OFF   3
#define DST_OFF   8

int src_array[ARR_SIZE] = {
	0x10, 0x11, 0x12, 0x13, 0x14, 0x15, 0x16, 0x17,
	0x18, 0x19, 0x1A, 0x1B, 0x1C, 0x1D, 0x1E, 0x1F,
	0x20, 0x21, 0x22, 0x23, 0x24, 0x25, 0x26, 0x27,
	0x28, 0x29, 0x2A, 0x2B, 0x2C, 0x2D, 0x2E, 0x2F,
	0x30, 0x31, 0x32, 0x33, 0x34, 0x35, 0x36, 0x37,
	0x38, 0x39, 0x3A, 0x3B, 0x3C, 0x3D, 0x3E, 0x3F,
	0x40, 0x41, 0x42, 0x43, 0x44, 0x45, 0x46, 0x47,
	0x48, 0x49, 0x4A, 0x4B, 0x4C, 0x4D, 0x4E, 0x4F
};
int dst_array[ARR_SIZE];

#define PORT 16
#define IMMEDIATE 108

int main(void)
{
	int *src = src_array + SRC_OFF;
	int *dst = dst_array + DST_OFF;
	int wrong = 0;
	int i, err;
	struct dma_addr addr;

	for (i = 0; i < ARR_SIZE; i++)
		dst_array[i] = 0;

	addr.addr = 0;
	addr.port = PORT;
	dma_raw_bind_addr(&addr);

	dma_track_immediate();
	// test that the sequencing is correct
	err = dma_poll_recv();
	if (err != DMA_RX_NOT_STARTED)
		return err;

	dma_send_immediate(&addr, IMMEDIATE);

	err = dma_raw_wait_recv();
	if (err)
		return 0x10 | err;

	if (dma_read_immediate() != IMMEDIATE)
		return 0x20;

	dma_read_src_addr(&addr);
	if (addr.port != PORT)
		return 0x30 | err;

	// allow a write up to the end of the 
	dma_track_put(dst_array, ARR_SIZE * sizeof(int));

	dma_gather_put(&addr, dst, src, COPY_SIZE * sizeof(int), 0, 1);

	err = dma_raw_wait_recv();
	if (err)
		return 0x40 | err;

	for (i = 0; i < DST_OFF; i++) {
		if (dst_array[i] != 0)
			wrong = 1;
	}

	for (i = 0; i < COPY_SIZE; i++) {
		if (dst[i] != src[i])
			wrong = 1;
	}

	for (i = DST_OFF + COPY_SIZE; i < ARR_SIZE; i++) {
		if (dst_array[i] != 0)
			wrong = 1;
	}

	return wrong;
}
