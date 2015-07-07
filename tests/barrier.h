#ifndef BARRIER_H
#define BARRIER_H

#include <semaphore.h>

struct barrier {
	struct barrier_shared *shared;
	int fd;
	int n;
};

struct barrier_shared {
	sem_t mutex;
	sem_t turnstile;
	int count;
	int cycle;
};

int barrier_init(struct barrier *barrier, const char *name, int n);
int barrier_open(struct barrier *barrier, const char *name, int n);
int barrier_wait(struct barrier *barrier);
int barrier_close(struct barrier *barrier);

#endif
