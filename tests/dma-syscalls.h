#ifndef __DMA_SYSCALLS_H__
#define __DMA_SYSCALLS_H__

#include "dma-ext.h"

#include <unistd.h>
#include <sys/syscall.h>
#include <signal.h>

#define __NR_dma_bind_addr 245
#define __NR_dma_unbind_addr 246
#define __NR_dma_wait_recv 247

#define MAX_WAIT_CYCLES 5000

static inline int dma_bind_addr(struct dma_addr *addr)
{
	return syscall(__NR_dma_bind_addr, addr);
}

static inline int dma_unbind_addr(void)
{
	return syscall(__NR_dma_unbind_addr);
}

static inline int dma_wait_recv(void)
{
	int i, err;

	for (i = 0; i < MAX_WAIT_CYCLES; i++) {
		err = dma_poll_recv();
		if (err != DMA_RX_NOT_STARTED)
			goto started;
	}

	err = syscall(__NR_dma_wait_recv);
	if (err != DMA_RX_NOT_FINISHED)
		return err;

started:
	do {
		err = dma_poll_recv();
	} while (err == DMA_RX_NOT_FINISHED);

	return err;
}

#endif
