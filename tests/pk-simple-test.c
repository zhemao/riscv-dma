#include <stdlib.h>
#include <stdio.h>
#include <stdint.h>
#include <string.h>

#include "dma-ext.h"

#define NITEMS 5000
#define PORT 20
#define IMMEDIATE 153

int main(void)
{
	uint8_t *src, *dst;
	int i, err = 0;
	struct dma_addr addr;
	unsigned long imm_data;

	src = malloc(NITEMS);
	dst = malloc(NITEMS);

	for (i = 0; i < NITEMS; i++) {
		src[i] = i;
		dst[i] = 0;
	}

	printf("Starting test\n");

	addr.addr = 0;
	addr.port = PORT;
	dma_bind_addr(&addr);

	dma_send_immediate(&addr, IMMEDIATE);
	dma_fence();
	err = dma_send_error();
	if (err) {
		printf("dma_send_immediate failed %d\n", err);
		return -err;
	}

	imm_data = dma_read_immediate();
	if (imm_data != IMMEDIATE) {
		printf("dma_read_immediate: expected %d, got %lu\n", IMMEDIATE, imm_data);
		return imm_data;
	}

	// do a put to our own CPU
	dma_contig_put(&addr, dst, src, NITEMS);
	dma_fence();
	err = dma_send_error();

	if (err) {
		printf("dma_contig_put failed %d\n", err);
		return -err;
	}

	for (i = 0; i < NITEMS; i++) {
		if (dst[i] != src[i]) {
			printf("Expected %d got %d\n", src[i], dst[i]);
			err = 1;
		}
	}

	if (!err) {
		printf("Test completed without errors\n");
	}

	return 0;
}
