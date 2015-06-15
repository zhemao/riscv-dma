int twobeat_src[8] = {0, 1, 2, 3, 4, 5, 6, 7};
int twobeat_dst[8] = {0, 0, 0, 0, 0, 0, 0, 0};

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
	for (int i = 0; i < 8; i++) {
		if (twobeat_src[i] != i)
			return 1;
	}

	copy_beat(twobeat_src, twobeat_dst, sizeof(twobeat_src));

	for (int i = 0; i < 8; i++) {
		if (twobeat_dst[i] != i)
			return 1;
	}

	return 0;
}
