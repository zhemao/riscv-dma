#include <sys/mman.h>
#include <sys/stat.h>
#include <fcntl.h>
#include <stdlib.h>
#include <unistd.h>

#include "barrier.h"

int barrier_init(struct barrier *barrier, const char *name, int n)
{
	barrier->fd = shm_open(name, O_RDWR | O_CREAT, S_IRUSR | S_IWUSR);
	if (barrier->fd < 0)
		return -1;

	if (ftruncate(barrier->fd, sizeof(struct barrier_shared))) {
		shm_unlink(name);
		return -1;
	}

	barrier->shared = mmap(NULL, sizeof(struct barrier_shared),
			PROT_READ | PROT_WRITE, MAP_SHARED, barrier->fd, 0);
	if (barrier->shared == MAP_FAILED) {
		shm_unlink(name);
		return -1;
	}

	sem_init(&barrier->shared->mutex, 1, 1);
	sem_init(&barrier->shared->turnstile, 1, 0);
	barrier->shared->count = 0;
	barrier->shared->cycle = 0;
	barrier->n = n;

	return 0;
}

int barrier_open(struct barrier *barrier, const char *name, int n)
{
	barrier->fd = shm_open(name, O_RDWR, S_IRUSR | S_IWUSR);
	if (barrier->fd < 0)
		return -1;

	barrier->shared = mmap(NULL, sizeof(struct barrier_shared),
			PROT_READ | PROT_WRITE, MAP_SHARED, barrier->fd, 0);
	if (barrier->shared == MAP_FAILED) {
		shm_unlink(name);
		return -1;
	}

	barrier->n = n;

	return 0;
}

static inline int sem_postn(sem_t *sem, int n)
{
	int i;

	for (i = 0; i < n; i++) {
		if (sem_post(sem))
			return -1;
	}
	return 0;
}

int barrier_wait(struct barrier *barrier)
{
	int cycle;
	volatile int *cycle_ptr = &barrier->shared->cycle;

	if (sem_wait(&barrier->shared->mutex))
		return -1;

	cycle = *cycle_ptr;
	// increment the count
	barrier->shared->count++;
	// last thread to arrive switches the cycle and resets count
	if (barrier->shared->count == barrier->n) {
		barrier->shared->count = 0;
		*cycle_ptr = !cycle;
	}

	if (sem_post(&barrier->shared->mutex))
		return -1;

	// busy loop until the last thread switches the cycle
	while (*cycle_ptr == cycle);

	return 0;
}

int barrier_close(struct barrier *barrier)
{
	if (munmap(barrier->shared, sizeof(struct barrier_shared)))
		return -1;

	if (close(barrier->fd))
		return -1;

	return 0;
}
