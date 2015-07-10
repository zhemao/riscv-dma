#ifndef DMA_EXT
#define DMA_EXT

#include <riscv-pk/encoding.h>

#define DMA_RX_NOT_FINISHED 1
#define DMA_RX_EARLY_FINISH 2
#define DMA_RX_NACK 3

#define DMA_TX_PAGEFAULT 1
#define DMA_TX_GETNACK 2
#define DMA_TX_PUTNACK 3

static inline void setup_dma(int remote_port, unsigned long segsize,
		unsigned long stride, unsigned long nsegments)
{
	write_csr(0x800, segsize);
	write_csr(0x801, stride);
	write_csr(0x802, nsegments);
	write_csr(0x803, remote_port);
}

static inline int dma_scatter_put(int remote_port, void *dst, void *src,
		unsigned long segsize, unsigned long stride,
		unsigned long nsegments)
{
	int error;

	setup_dma(remote_port, segsize, stride, nsegments);

	asm volatile ("fence");
	asm volatile ("custom0 %[error], %[src], %[dst], 0" :
			[error] "=r" (error) :
			[src] "r" (src), [dst] "r" (dst));
	asm volatile ("fence");

	return error;
}

static inline int dma_gather_put(int remote_port, void *dst, void *src,
		unsigned long segsize, unsigned long stride,
		unsigned long nsegments)
{
	int error;

	setup_dma(remote_port, segsize, stride, nsegments);

	asm volatile ("fence");
	asm volatile ("custom0 %[error], %[src], %[dst], 1" :
			[error] "=r" (error) :
			[src] "r" (src), [dst] "r" (dst));
	asm volatile ("fence");

	return error;
}

static inline int dma_scatter_get(int remote_port, void *dst, void *src,
		unsigned long segsize, unsigned long stride,
		unsigned long nsegments)
{
	int error;

	setup_dma(remote_port, segsize, stride, nsegments);

	asm volatile ("fence");
	asm volatile ("custom0 %[error], %[src], %[dst], 2" :
			[error] "=r" (error) :
			[src] "r" (src), [dst] "r" (dst));
	asm volatile ("fence");

	return error;
}

static inline int dma_gather_get(int remote_port, void *dst, void *src,
		unsigned long segsize, unsigned long stride,
		unsigned long nsegments)
{
	int error;

	setup_dma(remote_port, segsize, stride, nsegments);

	asm volatile ("fence");
	asm volatile ("custom0 %[error], %[src], %[dst], 3" :
			[error] "=r" (error) :
			[src] "r" (src), [dst] "r" (dst));
	asm volatile ("fence");

	return error;
}

static inline int
dma_contig_put(int remote_port, void *dst, void *src, unsigned long len)
{
	return dma_gather_put(remote_port, dst, src, len, 0, 1);
}

static inline int
dma_contig_get(int remote_port, void *dst, void *src, unsigned long len)
{
	return dma_gather_get(remote_port, dst, src, len, 0, 1);
}

static inline void dma_bind_port(int port)
{
	write_csr(0x804, port);
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

static inline int dma_wait_recv(void)
{
	int err;

	do {
		err = dma_poll_recv();
	} while (err == DMA_RX_NOT_FINISHED);

	asm volatile ("fence");

	return err;
}

#endif
