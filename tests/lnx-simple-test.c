#include <stdlib.h>
#include <stdio.h>
#include <stdint.h>
#include <string.h>

#include <sys/mman.h>
#include <sys/wait.h>
#include <unistd.h>

#include "dma-ext.h"
#include "barrier.h"

#define NITEMS 128

struct unshared_state {
	struct barrier barrier;
	int src[NITEMS];
	int dst[NITEMS];
};

#define PARENT_PORT 100
#define CHILD_PORT 101

void parent_thread(struct unshared_state *unshared)
{
	int i, ret;
	struct barrier *barrier = &unshared->barrier;

	for (i = 0; i < NITEMS; i++)
		unshared->src[i] = i;

	bind_port(PARENT_PORT);

	barrier_wait(barrier);

	ret = dma_contig_put(CHILD_PORT, unshared->dst, unshared->src, NITEMS);

	if (ret) {
		fprintf(stderr, "send failed with error code: %d\n", ret);
		exit(EXIT_FAILURE);
	}

	// wait for child to get the data
	barrier_wait(barrier);
}

void child_thread(struct unshared_state *unshared)
{
	int i, error;
	struct barrier *barrier = &unshared->barrier;

	bind_port(CHILD_PORT);

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
	struct unshared_state *unshared;
	pid_t pid;

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
		child_thread(unshared);
	} else {
		parent_thread(unshared);
		if (waitpid(pid, NULL, 0) < 0) {
			perror("waitpid");
			exit(EXIT_FAILURE);
		}
	}

	if (barrier_close(&unshared->barrier)) {
		perror("barrier_close");
		exit(EXIT_FAILURE);
	}

	free(unshared);

	return 0;
}
