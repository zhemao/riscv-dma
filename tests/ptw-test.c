static inline unsigned long translate_addr(void *virtAddr)
{
	unsigned long physAddr;

	asm volatile ("custom0 %[physAddr], %[virtAddr], 0, 2" :
			[physAddr] "=r" (physAddr) :
			[virtAddr] "r" (virtAddr));

	return physAddr;
}

int main(void)
{
	if (translate_addr(0) != 0)
		return 1;
	return 0;
}
