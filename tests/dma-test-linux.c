#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sys/mman.h>
#include "dma-copy.h"

#define PAGE_SIZE 4096
#define ARRAY_SIZE (2 * PAGE_SIZE)

#define SRC_OFF  5
#define DST_OFF  2
#define COPY_SIZE (PAGE_SIZE + 20)

int main(void)
{
	char *twopage_src, *twopage_dst;
	char *src, *dst;
	int i, error = 0;

	twopage_src = malloc(ARRAY_SIZE);
	twopage_dst = malloc(ARRAY_SIZE);

	if (twopage_src == NULL || twopage_dst == NULL) {
		fprintf(stderr, "Could not allocate memory\n");
		exit(EXIT_FAILURE);
	}

	printf("src (%p), dst (%p)\n", twopage_src, twopage_dst);

	for (i = 0; i < ARRAY_SIZE; i++)
		twopage_src[i] = i;
	memset(twopage_dst, 0, ARRAY_SIZE);

	src = twopage_src + SRC_OFF;
	dst = twopage_dst + DST_OFF;

	printf("copy %d from %p to %p\n", COPY_SIZE, src, dst);

	dma_copy(src, dst, COPY_SIZE);

	for (i = 0; i < DST_OFF; i++) {
		if (twopage_dst[i] != 0) {
			printf("idx %d is %x not 0\n", i, twopage_dst[i]);
			error = 1;
		}
	}

	for (i = 0; i < COPY_SIZE; i++) {
		if (dst[i] != src[i]) {
			printf("idx %d is %x not %x\n",
				i + DST_OFF, dst[i], src[i]);
			error = 1;
		}
	}

	for (i = DST_OFF + COPY_SIZE; i < ARRAY_SIZE; i++) {
		if (twopage_dst[i] != 0) {
			printf("idx %d is %x not 0\n", i, twopage_dst[i]);
			error = 1;
		}
	}

	if (!error)
		printf("Copy completed with no errors.\n");

	free(twopage_src);
	free(twopage_dst);

	return error;
}
