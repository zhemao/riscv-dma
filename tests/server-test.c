#include <stdio.h>
#include <stdlib.h>

#include "dma-ext.h"

#define SERVER_PORT 1000
#define ARR_SIZE 100

int src[ARR_SIZE];

int main(void)
{
	int i, err;
	struct dma_addr client, server;

	for (i = 0; i < ARR_SIZE; i++)
		src[i] = i * 3;

	server.addr = 0;
	server.port = SERVER_PORT;

	dma_bind_addr(&server);

	for (;;) {
		printf("waiting for client\n");

		dma_track_immediate();
		dma_fence();

		err = dma_recv_error();
		if (err) {
			fprintf(stderr, "wait_recv() got error %d\n", err);
			return -err;
		}

		if (dma_read_immediate())
			break;

		dma_read_src_addr(&client);
		printf("received request from %lx:%u\n",
				client.addr, client.port);

		dma_send_immediate(&client, (unsigned long) src);
		dma_fence();

		err = dma_send_error();
		if (err)
			return -err;

		printf("send address\n");
	}

	return 0;
}
