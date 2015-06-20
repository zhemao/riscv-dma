#include <stdio.h>
#include <stdlib.h>
#include <stdint.h>

static inline uintptr_t translate_addr(void *virtAddr)
{
	unsigned long physAddr;

	asm volatile ("custom0 %[physAddr], %[virtAddr], 0, 2" :
			[physAddr] "=r" (physAddr) :
			[virtAddr] "r" (virtAddr));

	return physAddr;
}

int main(void)
{
	uint32_t arr[4];

	printf("%p -> %lx\n", arr, translate_addr(arr));

	return 0;
}
