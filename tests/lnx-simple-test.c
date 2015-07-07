#include <stdlib.h>
#include <stdio.h>
#include <stdint.h>
#include <string.h>

#include <sys/mman.h>
#include <sys/wait.h>

#include "dma-ext.h"
#include "getcpu.h"
#include "barrier.h"
#include "shared-state.h"

#define NITEMS 128

struct unshared_state {
	struct barrier barrier;
	int src[NITEMS];
	int dst[NITEMS];
};

void parent_thread(struct unshared_state *unshared, struct shared_state *shared)
{
	int i, ret;
	struct barrier *barrier = &unshared->barrier;

	getcpu(&shared->parent_cpu, NULL);

	for (i = 0; i < NITEMS; i++)
		unshared->src[i] = i;

	barrier_wait(barrier);

	printf("Put from CPU %d to CPU %d\n",
		shared->parent_cpu, shared->child_cpu);

	ret = dma_contig_put(shared->child_cpu,
			unshared->dst, unshared->src, NITEMS);

	if (ret) {
		fprintf(stderr, "send failed with error code: %d\n", ret);
		exit(EXIT_FAILURE);
	}

	// wait for child to get the data
	barrier_wait(barrier);
}

void child_thread(struct unshared_state *unshared, struct shared_state *shared)
{
	int i, error;
	struct barrier *barrier = &unshared->barrier;

	getcpu(&shared->child_cpu, NULL);

	memset(unshared->dst, 0, NITEMS);

	barrier_wait(barrier);

	// wait for parent to send the data
	barrier_wait(barrier);

	asm volatile ("fence");

	for (i = 0; i < NITEMS; i++) {
		if (unshared->dst[i] != i) {
			printf("Expected %d, got %d\n", i, unshared->dst[i]);
			error = 1;
		}
	}

	if (!error)
		printf("Child received all data with no errors\n");
}

int main(void)
{
	struct shared_state *shared;
	struct unshared_state *unshared;
	int fd;
	pid_t pid;

	shared = open_shared_state("lnx-simple-shared", &fd);
	unshared = malloc(sizeof(struct unshared_state));

	if (barrier_init(&unshared->barrier, "lnx-simple-barrier", 2)) {
		perror("barrier_init");
		exit(EXIT_FAILURE);
	}

	pid = fork();
	if (pid < 0) {
		perror("fork");
		exit(EXIT_FAILURE);
	}

	if (pid == 0) {
		child_thread(unshared, shared);
	} else {
		parent_thread(unshared, shared);
		if (waitpid(pid, NULL, 0) < 0) {
			perror("waitpid");
			exit(EXIT_FAILURE);
		}
	}

	close_shared_state(shared, fd);

	if (barrier_close(&unshared->barrier)) {
		perror("barrier_close");
		exit(EXIT_FAILURE);
	}

	free(unshared);

	return 0;
}
