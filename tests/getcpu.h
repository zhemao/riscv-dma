#ifndef __GETCPU_H__
#define __GETCPU_H__

#include <unistd.h>
#include <sys/syscall.h>

int getcpu(unsigned *cpu, unsigned *node)
{
	return syscall(__NR_getcpu, cpu, node, NULL);
}

#endif
