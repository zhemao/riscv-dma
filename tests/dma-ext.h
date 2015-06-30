#ifndef DMA_COPY
#define DMA_COPY

typedef unsigned long csr_t;

static inline void setup_csrs(csr_t nsegs, csr_t seg_size, csr_t stride_size)
{
	asm volatile ("csrw 0x800, %[size]" : : [size] "r" (seg_size));
	asm volatile ("csrw 0x801, %[stride]" : : [stride] "r" (stride_size));
	asm volatile ("csrw 0x802, %[nsegs]" : : [nsegs] "r" (nsegs));
}

static inline void dma_scatter_l2r(
	void *src, void *dst, csr_t nsegs, csr_t seg_size, csr_t stride_size)
{
	asm volatile ("fence");
	setup_csrs(nsegs, seg_size, stride_size);
	asm volatile ("custom0 0, %[src], %[dst], 0" : :
	              [src] "r" (src), [dst] "r" (dst));
	asm volatile ("fence");
}

static inline void dma_gather_l2r(
	void *src, void *dst, csr_t nsegs, csr_t seg_size, csr_t stride_size)
{
	asm volatile ("fence");
	setup_csrs(nsegs, seg_size, stride_size);
	asm volatile ("custom0 0, %[src], %[dst], 1" : :
	              [src] "r" (src), [dst] "r" (dst));
	asm volatile ("fence");
}

static inline void dma_scatter_r2l(
	void *src, void *dst, csr_t nsegs, csr_t seg_size, csr_t stride_size)
{
	asm volatile ("fence");
	setup_csrs(nsegs, seg_size, stride_size);
	asm volatile ("custom0 0, %[src], %[dst], 2" : :
	              [src] "r" (src), [dst] "r" (dst));
	asm volatile ("fence");
}

static inline void dma_gather_r2l(
	void *src, void *dst, csr_t nsegs, csr_t seg_size, csr_t stride_size)
{
	asm volatile ("fence");
	setup_csrs(nsegs, seg_size, stride_size);
	asm volatile ("custom0 0, %[src], %[dst], 3" : :
	              [src] "r" (src), [dst] "r" (dst));
	asm volatile ("fence");
}

#endif
