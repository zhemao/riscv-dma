#ifndef DMA_EXT
#define DMA_EXT

#include <riscv-pk/encoding.h>

#define DMA_RX_NOT_FINISHED 1
#define DMA_RX_EARLY_FINISH 2
#define DMA_RX_NACK 3

#define DMA_TX_PAGEFAULT 1
#define DMA_TX_GETNACK 2
#define DMA_TX_PUTNACK 3

struct dma_addr {
	unsigned long addr;
	unsigned short port;
};

static inline void setup_dma(struct dma_addr *remote_addr,
		unsigned long segsize,
		unsigned long stride,
		unsigned long nsegments)
{
	write_csr(0x800, segsize);
	write_csr(0x801, stride);
	write_csr(0x802, nsegments);
	write_csr(0x803, remote_addr->addr);
	write_csr(0x804, remote_addr->port);
}

static inline int dma_scatter_put(struct dma_addr *remote_addr, void *dst, void *src,
		unsigned long segsize, unsigned long stride,
		unsigned long nsegments)
{
	int error;

	setup_dma(remote_addr, segsize, stride, nsegments);

	asm volatile ("fence");
	asm volatile ("custom0 %[error], %[src], %[dst], 0" :
			[error] "=r" (error) :
			[src] "r" (src), [dst] "r" (dst));
	asm volatile ("fence");

	return error;
}

static inline int dma_gather_put(struct dma_addr *remote_addr, void *dst, void *src,
		unsigned long segsize, unsigned long stride,
		unsigned long nsegments)
{
	int error;

	setup_dma(remote_addr, segsize, stride, nsegments);

	asm volatile ("fence");
	asm volatile ("custom0 %[error], %[src], %[dst], 1" :
			[error] "=r" (error) :
			[src] "r" (src), [dst] "r" (dst));
	asm volatile ("fence");

	return error;
}

static inline int dma_scatter_get(struct dma_addr *remote_addr, void *dst, void *src,
		unsigned long segsize, unsigned long stride,
		unsigned long nsegments)
{
	int error;

	setup_dma(remote_addr, segsize, stride, nsegments);

	asm volatile ("fence");
	asm volatile ("custom0 %[error], %[src], %[dst], 2" :
			[error] "=r" (error) :
			[src] "r" (src), [dst] "r" (dst));
	asm volatile ("fence");

	return error;
}

static inline int dma_gather_get(struct dma_addr *remote_addr, void *dst, void *src,
		unsigned long segsize, unsigned long stride,
		unsigned long nsegments)
{
	int error;

	setup_dma(remote_addr, segsize, stride, nsegments);

	asm volatile ("fence");
	asm volatile ("custom0 %[error], %[src], %[dst], 3" :
			[error] "=r" (error) :
			[src] "r" (src), [dst] "r" (dst));
	asm volatile ("fence");

	return error;
}

static inline int
dma_contig_put(struct dma_addr *remote_addr, void *dst, void *src, unsigned long len)
{
	return dma_gather_put(remote_addr, dst, src, len, 0, 1);
}

static inline int
dma_contig_get(struct dma_addr *remote_addr, void *dst, void *src, unsigned long len)
{
	return dma_gather_get(remote_addr, dst, src, len, 0, 1);
}

static inline void dma_bind_addr(struct dma_addr *addr)
{
	write_csr(0x805, addr->addr);
	write_csr(0x806, addr->port);
}

static inline void dma_track_recv(void *dst, unsigned long nbytes)
{
	write_csr(0x800, nbytes);
	asm volatile ("custom0 0, %[dst], 0, 4" : : [dst] "r" (dst));
}

static inline int dma_poll_recv(void)
{
	int err;

	asm volatile ("custom0 %[err], 0, 0, 5" : [err] "=r" (err));

	return err;
}

static inline int dma_send_immediate(struct dma_addr *addr, unsigned long imm)
{
	int err;
	setup_dma(addr, 0, 0, 0);
	asm volatile ("custom0 %[err], %[imm], 0, 6" :
			[err] "=r" (err) : [imm] "r" (imm));
	return err;
}

static inline int dma_wait_recv(void)
{
	int err;

	do {
		err = dma_poll_recv();
	} while (err == DMA_RX_NOT_FINISHED);

	asm volatile ("fence");

	return err;
}

static inline void dma_track_immediate(void)
{
	asm volatile ("custom0 0, zero, 1, 4");
}

static inline unsigned long dma_read_immediate(void)
{
	unsigned long res;

	asm volatile ("custom0 %[res], 0, 0, 7" : [res] "=r" (res));

	return res;
}

#endif
