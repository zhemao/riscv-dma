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
	int i, ret, error = 0;
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
	dma_raw_bind_addr(&addr);

	dma_track_immediate();
	dma_send_immediate(&addr, IMMEDIATE);
	error = dma_raw_wait_recv();
	if (error)
		return -error;

	imm_data = dma_read_immediate();
	if (imm_data != IMMEDIATE)
		return imm_data;

	// set up to track a receive
	dma_track_recv(dst, NITEMS);

	// do a put to our own CPU
	ret = dma_contig_put(&addr, dst, src, NITEMS);
	if (ret) {
		printf("Error sending data: %d\n", ret);
		exit(EXIT_FAILURE);
	}

	ret = dma_raw_wait_recv();
	if (ret) {
		printf("Error receiving data: %d\n", ret);
		exit(EXIT_FAILURE);
	}

	for (i = 0; i < NITEMS; i++) {
		if (dst[i] != src[i]) {
			printf("Expected %d got %d\n", src[i], dst[i]);
			error = 1;
		}
	}

	if (!error) {
		printf("Test completed without errors\n");
	}

	return 0;
}
