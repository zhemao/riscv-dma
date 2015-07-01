#ifndef DMA_COPY
#define DMA_COPY

#include <riscv-pk/encoding.h>

#define dma_op(funct, rid, src_addr, dst_addr, nsegments, seg_size, stride_size) \
{ \
	write_csr(0x800, seg_size); \
	write_csr(0x801, stride_size); \
	write_csr(0x802, nsegments); \
	write_csr(0x803, rid); \
	asm volatile ("fence"); \
	asm volatile ("custom0 0, %[src], %[dst], " #funct : : \
	              [src] "r" (src_addr), [dst] "r" (dst_addr)); \
	asm volatile ("fence"); \
}

#define dma_scatter_l2r(rid, src, dst, nsegs, seg_size, stride_size) \
	dma_op(0, rid, src, dst, nsegs, seg_size, stride_size)

#define dma_gather_l2r(rid, src, dst, nsegs, seg_size, stride_size) \
	dma_op(1, rid, src, dst, nsegs, seg_size, stride_size)

#define dma_scatter_r2l(rid, src, dst, nsegs, seg_size, stride_size) \
	dma_op(2, rid, src, dst, nsegs, seg_size, stride_size)

#define dma_gather_r2l(rid, src, dst, nsegs, seg_size, stride_size) \
	dma_op(3, rid, src, dst, nsegs, seg_size, stride_size)

#endif
