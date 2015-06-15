unsigned long numbers[4] = {20939L, 92929L, 9393L, 1245442L};

static void accum_set(int addr, unsigned long value)
{
	asm volatile ("custom0 0, %[value], %[addr], 0" : :
			[value] "r" (value), [addr] "r" (addr));
}

static void accum_add(int addr, unsigned long value)
{
	asm volatile ("custom0 0, %[value], %[addr], 3" : :
			[value] "r" (value), [addr] "r" (addr));
}

static unsigned long accum_read(int addr)
{
	unsigned long result;

	asm volatile ("custom0 %[result], x0, %[addr], 1" :
			[result] "=r" (result) : [addr] "r" (addr));

	return result;
}

int main(void)
{
	unsigned long reference = 0, actual;
	int addr = 2;
	int i;

	// reset to zero
	accum_set(addr, 0);

	for (i = 0; i < 4; i++) {
		accum_add(addr, numbers[i]);
		reference += numbers[i];
	}

	actual = accum_read(addr);

	if (reference != actual)
		return 1;

	return 0;
}
