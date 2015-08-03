#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <stdint.h>

#include <sys/wait.h>
#include <unistd.h>

#include "barrier.h"
#include "dma-ext.h"

#define N 128
#define M 32

#define ROW 32
#define COL 64

#define MASTER_PORT 100
#define SLAVE_PORT 101

static int check_matrix(int *mat_a, int *mat_b)
{
	int a, b, i, j, error_count = 0;
	for (i = 0; i < M; i++) {
		for (j = 0; j < M; j++) {
			a = mat_a[(ROW + i) * N + COL + j];
			b = mat_b[i * M + j];
			if (a != b && error_count < 50) {
				printf("expected %d, got %d\n", a, b);
				error_count++;
			}
		}
	}
	return error_count > 0;
}

int master_process(struct barrier *barrier, int *mat_a, int *mat_b)
{
	int *start;
	unsigned long nsegs, seg_size, stride_size;
	struct dma_addr local_addr, remote_addr;
	int i, error = 0, ret;

	for (i = 0; i < N * N; i++)
		mat_a[i] = i;
	memset(mat_b, 0, M * M * sizeof(int));

	local_addr.addr = 0;
	local_addr.port = MASTER_PORT;
	dma_bind_addr(&local_addr);

	remote_addr.addr = 0;
	remote_addr.port = SLAVE_PORT;

	barrier_wait(barrier);

	start = mat_a + ROW * N + COL;
	nsegs = M;
	seg_size = M * sizeof(int);
	stride_size = (N - M) * sizeof(int);

	// send cutout to slave
	dma_gather_put(&remote_addr, mat_b, start,
		seg_size, stride_size, nsegs);
	dma_fence();
	ret = dma_send_error();

	if (ret) {
		fprintf(stderr, "dma_gather_put failed with code %d\n", ret);
		exit(EXIT_FAILURE);
	}

	barrier_wait(barrier);

	// check that matrix b is unchanged
	for (i = 0; i < M * M; i++) {
		if (mat_b[i] != 0) {
			printf("mat_b[%d] = %d\n", i, mat_b[i]);
			error = 1;
		}
	}

	// wait for slave to finish computing new matrix
	barrier_wait(barrier);

	// read doubled data back from slave
	dma_scatter_get(&remote_addr, start, mat_b,
			seg_size, stride_size, nsegs);
	dma_fence();
	ret = dma_send_error();
	if (ret) {
		fprintf(stderr, "dma_scatter_get failed with code %d\n", ret);
		exit(EXIT_FAILURE);
	}

	// wait for slave to send its mat_b to ours
	barrier_wait(barrier);

	printf("master check data from slave\n");
	if (check_matrix(mat_a, mat_b))
		error = 1;

	return error;
}

int slave_process(struct barrier *barrier, int *mat_a, int *mat_b)
{
	int i, error = 0, ret;
	struct dma_addr local_addr, remote_addr;

	for (i = 0; i < N * N; i++)
		mat_a[i] = i;
	memset(mat_b, 0, M * M * sizeof(int));

	local_addr.addr = 0;
	local_addr.port = SLAVE_PORT;
	dma_bind_addr(&local_addr);

	remote_addr.addr = 0;
	remote_addr.port = MASTER_PORT;

	barrier_wait(barrier);

	// wait for master to finish transmission
	barrier_wait(barrier);

	printf("slave check data from master\n");
	if (check_matrix(mat_a, mat_b))
		error = 1;

	for (i = 0; i < M * M; i++)
		mat_b[i] *= 2;

	barrier_wait(barrier);

	// send matrix b back to master
	dma_contig_put(&remote_addr, mat_b, mat_b, M * M * sizeof(int));
	dma_fence();
	ret = dma_send_error();
	if (ret) {
		fprintf(stderr, "dma_contig_put failed with code %d\n", ret);
		exit(EXIT_FAILURE);
	}

	// wait for master to finish reading matrix b back
	barrier_wait(barrier);

	// check that our matrix A is unchanged
	for (i = 0; i < N * N; i++) {
		if (mat_a[i] != i) {
			printf("mat_a[%d] = %d\n", i, mat_a[i]);
			error = 1;
		}
	}

	return error;
}

int main(void)
{
	int *mat_a, *mat_b;
	pid_t id;
	struct barrier barrier;
	int ret, slave_status;

	mat_a = malloc(N * N * sizeof(int));
	mat_b = malloc(M * M * sizeof(int));

	if (barrier_init(&barrier, "matrix-barrier", 2)) {
		perror("barrier_init");
		return -1;
	}

	id = fork();
	if (id < 0)
		abort();
	else if (id == 0) {
		ret = slave_process(&barrier, mat_a, mat_b);
	} else {
		ret = master_process(&barrier, mat_a, mat_b);

		if (waitpid(id, &slave_status, 0) < 0) {
			perror("waitpid");
			return -1;
		}

		if (!ret)
			printf("Master process completed without errors\n");
		if (WEXITSTATUS(slave_status) == 0)
			printf("Slave process completed without errors\n");
	}

	free(mat_a);
	free(mat_b);

	if (barrier_close(&barrier)) {
		perror("barrier_close");
		return -1;
	}

	return ret;
}
