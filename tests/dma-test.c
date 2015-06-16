#define ARR_SIZE 16

int twobeat_src[16] = {0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15};
int twobeat_dst[ARR_SIZE];

#define COPY_ACCEL

#ifdef COPY_ACCEL
static inline void copy_beat(int *src, int *dst, int nbytes) {
	asm volatile ("fence");
	asm volatile ("custom0 0, %[src], %[dst], 0" : :
	              [src] "r" (src), [dst] "r" (dst) : );
	asm volatile ("custom0 0, %[nbytes], 0, 1" : : [nbytes] "r" (nbytes) : );
	asm volatile ("fence");
}
#else
static inline void copy_beat(int *src, int *dst, int nbytes) {
	int nwords = nbytes / sizeof(int);
	int i;

	for (i = 0; i < nwords; i++)
		dst[i] = src[i];
}
#endif

int main(void)
{
	for (int i = 0; i < ARR_SIZE; i++)
		twobeat_src[i] <<= 4;

	copy_beat(twobeat_src, twobeat_dst, sizeof(twobeat_dst));

	for (int i = 0; i < ARR_SIZE; i++) {
		if (twobeat_dst[i] != twobeat_src[i])
			return 1;
	}

	return 0;
}
