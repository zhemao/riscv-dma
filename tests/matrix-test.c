#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <stdint.h>
#include <sys/mman.h>
#include "dma-ext.h"

#define N 128
#define M 32

#define ROW 32
#define COL 64

static int check_matrix(int *mat_a, int *mat_b)
{
	int a, b, i, j, error = 0;
	for (i = 0; i < M; i++) {
		for (j = 0; j < M; j++) {
			a = mat_a[(ROW + i) * N + COL + j];
			b = mat_b[i * M + j];
			if (a != b) {
				printf("expected %d, got %d\n", a, b);
				error = 1;
			}
		}
	}
	return error;
}

int main(void)
{
	int *mat_a, *mat_b, *start;
	unsigned long nsegs, seg_size, stride_size;
	unsigned long hartid = read_csr(mhartid);
	int i, error = 0;

	if (hartid == 0) {
		mat_a = malloc(N * N * sizeof(int));
		mat_b = malloc(M * M * sizeof(int));

		start = mat_a + ROW * N + COL;
		nsegs = M;
		seg_size = M * sizeof(int);
		stride_size = (N - M) * sizeof(int);

		printf("nsegs: %lu\n", nsegs);
		printf("seg_size: %lu\n", seg_size);
		printf("stride_size: %lu\n", stride_size);
		printf("src offset: %lu\n", ((uintptr_t) start) % 64);
		printf("dst offset: %lu\n", ((uintptr_t) mat_b) % 64);

		for (i = 0; i < N * N; i++)
			mat_a[i] = i;
		memset(mat_b, 0, M * M * sizeof(int));

		dma_gather_l2r(0, start, mat_b, nsegs, seg_size, stride_size);

		if (check_matrix(mat_a, mat_b))
			error = 1;

		for (i = 0; i < M * M; i++)
			mat_b[i] *= 2;

		dma_scatter_r2l(0, mat_b, start, nsegs, seg_size, stride_size);

		if (check_matrix(mat_a, mat_b))
			error = 1;

		if (!error)
			printf("Copies completed with no errors.\n");

		free(mat_a);
		free(mat_b);
	}

	return error;
}
