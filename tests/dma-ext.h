#ifndef DMA_EXT
#define DMA_EXT

#include <riscv-pk/encoding.h>

#define DMA_RX_NOT_STARTED 1
#define DMA_RX_NOT_FINISHED 2
#define DMA_RX_NACK 3
#define DMA_RX_NO_ROUTE 4

#define DMA_TX_NOT_FINISHED 1
#define DMA_TX_PAGEFAULT 2
#define DMA_TX_NACK 3
#define DMA_TX_NOROUTE 4

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

static inline void dma_raw_bind_addr(struct dma_addr *addr)
{
	write_csr(0x804, addr->addr);
	write_csr(0x805, addr->port);
}

static inline void dma_track_put(void *dst, unsigned long nbytes)
{
	asm volatile ("custom0 0, %[dst], %[nbytes], 2" : :
			[dst] "r" (dst), [nbytes] "r" (nbytes));
}

static inline void dma_track_get(void *src, unsigned long nbytes)
{
	asm volatile ("custom0 1, %[src], %[nbytes], 2" : :
			[src] "r" (src), [nbytes] "r" (nbytes));
}

static inline void dma_track_immediate(void)
{
	asm volatile ("custom0 2, zero, zero, 2");
}

static inline int dma_poll_recv(void)
{
	int err;
	asm volatile ("custom0 %[err], 0, 0, 3" : [err] "=r" (err));
	return err;
}

static inline int dma_poll_send(void)
{
	int err;
	asm volatile ("custom0 %[err], 1, 0, 3" : [err] "=r" (err));
	return err;
}

static inline void dma_send_immediate(struct dma_addr *addr, unsigned long imm)
{
	setup_dma(addr, 0, 0, 0, 0);
	asm volatile ("custom0 0, %[imm], 0, 4" : : [imm] "r" (imm));
}

static inline int dma_raw_wait_recv(void)
{
	int err;

	do {
		err = dma_poll_recv();
	} while (err == DMA_RX_NOT_STARTED);

	do {
		err = dma_poll_recv();
	} while (err == DMA_RX_NOT_FINISHED);

	asm volatile ("fence");

	return err;
}

static inline int dma_raw_wait_send(void)
{
	int err;

	do {
		err = dma_poll_send();
	} while (err == DMA_TX_NOT_FINISHED);

	asm volatile ("fence");

	return err;
}

static inline void dma_read_src_addr(struct dma_addr *addr)
{
	addr->addr = read_csr(0x808);
	addr->port = read_csr(0x809);
}


static inline unsigned long dma_read_immediate(void)
{
	return read_csr(0x80C);
}

#endif
