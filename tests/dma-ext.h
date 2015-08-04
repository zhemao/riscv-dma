#ifndef DMA_EXT
#define DMA_EXT

#include <riscv-pk/encoding.h>

#define DMA_RX_NACK 1
#define DMA_RX_NO_ROUTE 2

#define DMA_TX_PAGEFAULT 1
#define DMA_TX_NACK 2
#define DMA_TX_NOROUTE 3

struct dma_addr {
	unsigned long addr;
	unsigned short port;
};

static inline void setup_dma(struct dma_addr *remote_addr,
		unsigned long segsize,
		unsigned long src_stride,
		unsigned long dst_stride,
		unsigned long nsegments)
{
	write_csr(0x800, segsize);
	write_csr(0x801, src_stride);
	write_csr(0x802, dst_stride);
	write_csr(0x803, nsegments);
	write_csr(0x806, remote_addr->addr);
	write_csr(0x807, remote_addr->port);
}

static inline void dma_put(
		struct dma_addr *remote_addr, void *dst, void *src,
		unsigned long segsize, unsigned long src_stride,
		unsigned long dst_stride, unsigned long nsegments)
{
	setup_dma(remote_addr, segsize, src_stride, dst_stride, nsegments);

	asm volatile ("fence");
	asm volatile ("custom0 0, %[dst], %[src], 0" : :
			[src] "r" (src), [dst] "r" (dst));
}

static inline void dma_scatter_put(
		struct dma_addr *remote_addr, void *dst, void *src,
		unsigned long segsize, unsigned long stride,
		unsigned long nsegments)
{
	dma_put(remote_addr, dst, src, segsize, 0, stride, nsegments);
}

static inline void dma_gather_put(
		struct dma_addr *remote_addr, void *dst, void *src,
		unsigned long segsize, unsigned long stride,
		unsigned long nsegments)
{
	dma_put(remote_addr, dst, src, segsize, stride, 0, nsegments);
}

static inline void dma_contig_put(struct dma_addr *remote_addr,
		void *dst, void *src, unsigned long len)
{
	dma_put(remote_addr, dst, src, len, 0, 0, 1);
}

static inline void dma_get(struct dma_addr *remote_addr, void *dst, void *src,
		unsigned long segsize, unsigned long src_stride,
		unsigned long dst_stride, unsigned long nsegments)
{
	setup_dma(remote_addr, segsize, src_stride, dst_stride, nsegments);

	asm volatile ("fence");
	asm volatile ("custom0 0, %[dst], %[src], 1" : :
			[src] "r" (src), [dst] "r" (dst));
}

static inline void dma_scatter_get(
		struct dma_addr *remote_addr, void *dst, void *src,
		unsigned long segsize, unsigned long stride,
		unsigned long nsegments)
{
	dma_get(remote_addr, dst, src, segsize, 0, stride, nsegments);
}

static inline void dma_gather_get(
		struct dma_addr *remote_addr, void *dst, void *src,
		unsigned long segsize, unsigned long stride,
		unsigned long nsegments)
{
	dma_get(remote_addr, dst, src, segsize, stride, 0, nsegments);
}

static inline void
dma_contig_get(struct dma_addr *remote_addr,
		void *dst, void *src, unsigned long len)
{
	dma_get(remote_addr, dst, src, len, 0, 0, 1);
}

static inline void dma_bind_addr(struct dma_addr *addr)
{
	write_csr(0x804, addr->addr);
	write_csr(0x805, addr->port);
}

static inline int dma_send_error(void)
{
	return read_csr(0x80A);
}

static inline void dma_read_src_addr(struct dma_addr *addr)
{
	addr->addr = read_csr(0x808);
	addr->port = read_csr(0x809);
}


static inline void dma_fence(void)
{
	asm volatile ("fence");
}

#endif
