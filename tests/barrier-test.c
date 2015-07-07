#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>
#include <sys/wait.h>
#include <sys/mman.h>

#include "barrier.h"

int main(void)
{
	struct barrier barrier;
	pid_t pid;
	int i;

	if (barrier_init(&barrier, "barrier-test", 2)) {
		perror("barrier_init");
		return -1;
	}

	pid = fork();

	if (pid < 0)
		return -1;

	for (i = 0; i < 10; i++) {
		printf("Iteration %d\n", i);
		if (barrier_wait(&barrier)) {
			perror("barrier_wait");
			return -1;
		}
	}

	if (barrier_close(&barrier)) {
		perror("barrier_close");
		return -1;
	}

	if (pid != 0) {
		if (waitpid(pid, NULL, 0) < 0) {
			perror("waitpid");
			return -1;
		}
		if (shm_unlink("barrier-test")) {
			perror("shm_unlink");
			return -1;
		}
	}

	return 0;
}
