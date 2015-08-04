#include <stdlib.h>
#include <stdio.h>
#include <stdint.h>
#include <string.h>

#include <sys/mman.h>
#include <sys/wait.h>
#include <unistd.h>

#include "barrier.h"
#include "dma-ext.h"

#define NITEMS 5000

struct unshared_state {
	uint8_t src[NITEMS];
	uint8_t dst[NITEMS];
};

#define PARENT_PORT 100
#define CHILD_PORT 101

void parent_thread(struct barrier *barrier, struct unshared_state *unshared)
{
	int i, ret;
	struct dma_addr local_addr, remote_addr;

	local_addr.addr = 0;
	local_addr.port = PARENT_PORT;
	dma_bind_addr(&local_addr);

	for (i = 0; i < NITEMS; i++)
		unshared->src[i] = i & 0xff;

	printf("Sending data\n");

	remote_addr.addr = 0;
	remote_addr.port = CHILD_PORT;
	dma_contig_put(&remote_addr, unshared->dst, unshared->src, NITEMS);
	dma_fence();

	ret = dma_send_error();
	if (ret) {
		fprintf(stderr, "send failed with error code: %d\n", ret);
		exit(EXIT_FAILURE);
	}

	barrier_wait(barrier);

	printf("Data sent\n");
}

void child_thread(struct barrier *barrier, struct unshared_state *unshared)
{
	int i, error = 0;
	struct dma_addr local_addr;

	local_addr.addr = 0;
	local_addr.port = CHILD_PORT;
	dma_bind_addr(&local_addr);

	printf("Waiting for data\n");

	barrier_wait(barrier);

	for (i = 0; i < NITEMS; i++) {
		uint8_t expected = i & 0xff;
		if (unshared->dst[i] != expected) {
			printf("Expected %d, got %d\n",
					expected, unshared->dst[i]);
			error = 1;
		}
	}

	if (!error)
		printf("Child received all data with no errors\n");
}

int main(void)
{
	struct unshared_state *unshared;
	struct barrier barrier;
	pid_t pid;

	unshared = malloc(sizeof(struct unshared_state));

	memset(unshared, 0, sizeof(struct unshared_state));

	if (barrier_init(&barrier, "simple-barrier", 2)) {
		perror("barrier_init");
		return -1;
	}

	pid = fork();
	if (pid < 0) {
		perror("fork");
		exit(EXIT_FAILURE);
	}

	if (pid == 0) {
		child_thread(&barrier, unshared);
	} else {
		parent_thread(&barrier, unshared);
		if (waitpid(pid, NULL, 0) < 0) {
			perror("waitpid");
			exit(EXIT_FAILURE);
		}
	}

	if (barrier_close(&barrier)) {
		perror("barrier_close");
		return -1;
	}

	free(unshared);

	return 0;
}
