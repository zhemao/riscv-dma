#include <stdlib.h>
#include <stdio.h>
#include <stdint.h>
#include <string.h>

#include "dma-ext.h"

#define NITEMS 5000

int main(void)
{
	uint8_t *src, *dst;
	int i, ret, error = 0;

	src = malloc(NITEMS);
	dst = malloc(NITEMS);

	for (i = 0; i < NITEMS; i++) {
		src[i] = i;
		dst[i] = 0;
	}

	printf("Starting test\n");

	// do a put to our own CPU
	ret = dma_contig_put(0, dst, src, NITEMS);
	if (ret) {
		printf("Error performing transfer\n");
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
