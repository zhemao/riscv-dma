#include <stdio.h>
#include <stdlib.h>

#include "dma-ext.h"

#define SERVER_PORT 1000
#define CLIENT_PORT 1001

#define ARR_SIZE 100

int dst[ARR_SIZE];

int main(void)
{
	struct dma_addr client, server;
	int i, err;
	int *src;

	client.addr = 0;
	client.port = CLIENT_PORT;

	server.addr = 0;
	server.port = SERVER_PORT;

	dma_bind_addr(&client);
	dma_track_immediate();

	printf("Requesting address from server...\n");
	err = dma_send_immediate(&server, 0);
	if (err)
		return -err;

	err = dma_wait_recv();
	if (err)
		return -err;

	src = (int *) dma_read_immediate();

	printf("Received address %p\n", src);

	err = dma_contig_get(&server, dst, src, sizeof(dst));
	if (err)
		return -err;

	for (i = 0; i < ARR_SIZE; i++) {
		if (dst[i] != i * 3)
			printf("[%d] expected %d, got %d\n", i, i * 3, dst[i]);
	}

	return 0;
}
