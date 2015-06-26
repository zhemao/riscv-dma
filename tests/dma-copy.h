#ifndef DMA_COPY
#define DMA_COPY

static inline void dma_copy(void *src, void *dst, int nbytes) {
	int stride = 0;
	int nsegments = 1;

	asm volatile ("fence");
	// set size = nbytes, stride = 0, and nsegments = 1
	asm volatile ("csrw 0x800, %[size]" : : [size] "r" (nbytes));
	asm volatile ("csrw 0x801, %[stride]" : : [stride] "r" (stride));
	asm volatile ("csrw 0x802, %[nsegs]" : : [nsegs] "r" (nsegments));
	// scatter
	asm volatile ("custom0 0, %[src], %[dst], 0" : :
	              [src] "r" (src), [dst] "r" (dst));
	asm volatile ("fence");
}

#endif
