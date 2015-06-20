#include <stdio.h>
#include <stdlib.h>

#define COPY_SIZE 18
#define SRC_OFF  5
#define DST_OFF  2
#define ARR_SIZE 32

int twobeat_src[ARR_SIZE] = {
	0x10, 0x11, 0x12, 0x13, 0x14, 0x15, 0x16, 0x17,
	0x18, 0x19, 0x1A, 0x1B, 0x1C, 0x1D, 0x1E, 0x1F,
	0x20, 0x21, 0x22, 0x23, 0x24, 0x25, 0x26, 0x27,
	0x28, 0x29, 0x2A, 0x2B, 0x2C, 0x2D, 0x2E, 0x2F
};
int twobeat_dst[ARR_SIZE];

static inline void copy_beat(int *src, int *dst, int nbytes) {
	asm volatile ("fence");
	asm volatile ("custom0 0, %[src], %[dst], 0" : :
	              [src] "r" (src), [dst] "r" (dst));
	asm volatile ("custom0 0, %[nbytes], 0, 1" : : [nbytes] "r" (nbytes));
	asm volatile ("fence");
}

int main(void)
{
	int *src = twobeat_src + SRC_OFF;
	int *dst = twobeat_dst + DST_OFF;
	int i;

	copy_beat(src, dst, COPY_SIZE * sizeof(int));

	for (i = 0; i < DST_OFF; i++) {
		if (twobeat_dst[i] != 0)
			printf("idx %d is %x not 0\n", i, twobeat_dst[i]);
	}

	for (i = 0; i < COPY_SIZE; i++) {
		if (dst[i] != src[i])
			printf("idx %d is %x not %x\n",
				i + DST_OFF, dst[i], src[i]);
	}

	for (i = DST_OFF + COPY_SIZE; i < ARR_SIZE; i++) {
		if (twobeat_dst[i] != 0)
			printf("idx %d is %x not 0\n", i, twobeat_dst[i]);
	}

	return 0;
}
