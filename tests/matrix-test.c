#include "dma-ext.h"

#define PORT 16

#define N 64
#define M 16

#define ROW 32
#define COL 16

int mat_a[N * N];
int mat_b[M * M];

#define MAX_ERRORS 128

static int check_matrix(void)
{
	int a, b, i, j, error_count = 0;
	for (i = 0; i < M; i++) {
		for (j = 0; j < M; j++) {
			a = mat_a[(ROW + i) * N + COL + j];
			b = mat_b[i * M + j];
			if (a != b && error_count < MAX_ERRORS)
				error_count++;
		}
	}
	return error_count > 0;
}

int main(void)
{
	int *start;
	unsigned long nsegs, seg_size, stride_size;
	int i, ret;
	struct dma_addr addr;

	addr.addr = 0;
	addr.port = PORT;
	dma_bind_addr(&addr);

	for (i = 0; i < N * N; i++)
		mat_a[i] = i;

	start = mat_a + ROW * N + COL;
	nsegs = M;
	seg_size = M * sizeof(int);
	stride_size = (N - M) * sizeof(int);

	dma_gather_put(&addr, mat_b, start,
		seg_size, stride_size, nsegs);
	ret = dma_send_error();
	if (ret)
		return 0x10 | ret;

	if (check_matrix())
		return 0x20;

	for (i = 0; i < M * M; i++)
		mat_b[i] *= 2;

	dma_scatter_get(&addr, start, mat_b,
		seg_size, stride_size, nsegs);
	ret = dma_send_error();
	if (ret)
		return 0x30 | ret;

	if (check_matrix())
		return 0x40;

	return 0;
}
