#include <stdio.h>
#include <stdlib.h>

#include "dma-syscalls.h"

#define SERVER_PORT 1000
#define ARR_SIZE 100

int src[ARR_SIZE];

void quit_handler(int sig)
{
	dma_unbind_addr();
}

int main(void)
{
	int i, err;
	struct dma_addr client, server;

	for (i = 0; i < ARR_SIZE; i++)
		src[i] = i * 3;

	server.addr = 0;
	server.port = SERVER_PORT;

	dma_bind_addr(&server);

	signal(SIGTERM, quit_handler);
	signal(SIGINT, quit_handler);

	for (;;) {
		printf("waiting for client\n");

		dma_track_immediate();
		err = dma_raw_wait_recv();
		if (err) {
			fprintf(stderr, "wait_recv() got error %d\n", err);
			return -err;
		}

		dma_read_src_addr(&client);
		printf("received request from %lx:%u\n",
				client.addr, client.port);

		dma_send_immediate(&client, (unsigned long) src);

		printf("send address\n");
	}

	return 0;
}
