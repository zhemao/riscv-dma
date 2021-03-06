#include "dma-ext.h"

#define PORT 100

int main(void)
{
	int err;
	struct dma_addr addr;
	int src[12], dst[12];

	addr.addr = 0;
	addr.port = PORT;

	dma_bind_addr(&addr);

	// turn phys back off so we get a page fault
	write_csr(0x80B, 0);
	dma_contig_put(&addr, 0, 0, 1024);
	dma_fence();
	err = dma_send_error();

	if (err != DMA_TX_PAGEFAULT)
		return (0x10 | err);

	// now turn it back on
	write_csr(0x80B, 1);

	addr.port = 102;
	dma_contig_put(&addr, dst, src, 12 * sizeof(int));
	dma_fence();
	err = dma_send_error();
	if (err != DMA_TX_NOROUTE)
		return (0x20 | err);

	return !err;
}
