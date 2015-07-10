#include <stdlib.h>
#include <stdio.h>
#include <stdint.h>
#include <string.h>

#include <sys/mman.h>
#include <sys/wait.h>
#include <unistd.h>

#include "dma-ext.h"

#define NITEMS 5000

struct unshared_state {
	uint8_t src[NITEMS];
	uint8_t dst[NITEMS];
};

#define PARENT_PORT 100
#define CHILD_PORT 101

void parent_thread(struct unshared_state *unshared)
{
	int i, ret;

	dma_bind_port(PARENT_PORT);

	for (i = 0; i < NITEMS; i++)
		unshared->src[i] = i & 0xff;

	printf("Sending data\n");

	ret = dma_contig_put(CHILD_PORT, unshared->dst, unshared->src, NITEMS);
	if (ret) {
		fprintf(stderr, "send failed with error code: %d\n", ret);
		exit(EXIT_FAILURE);
	}

	printf("Data sent\n");
}

void child_thread(struct unshared_state *unshared)
{
	int i, ret, error = 0;

	dma_bind_port(CHILD_PORT);

	dma_track_recv(unshared->dst, NITEMS);

	printf("Waiting for data\n");

	ret = dma_wait_recv();
	if (ret) {
		fprintf(stderr, "recv failed with error code: %d\n", ret);
		exit(EXIT_FAILURE);
	}

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
	pid_t pid;

	unshared = malloc(sizeof(struct unshared_state));

	memset(unshared, 0, sizeof(struct unshared_state));

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

	free(unshared);

	return 0;
}
