#include "dma-ext.h"

int main(void)
{
	int err;

	// turn phys back off
	asm volatile ("csrw 0x804, zero");
	err = dma_scatter_put(0, 0, 0, 64, 0, 1);

	return !err;
}
