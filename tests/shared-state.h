#ifndef SHARED_STATE
#define SHARED_STATE

#include <unistd.h>
#include <sys/types.h>
#include <sys/mman.h>
#include <sys/stat.h>
#include <fcntl.h>

struct shared_state {
	unsigned parent_cpu;
	unsigned child_cpu;
};

static inline struct shared_state *open_shared_state(const char *name, int *fd)
{
	struct shared_state *shared;

	*fd = shm_open(name, O_RDWR | O_CREAT, S_IRUSR | S_IWUSR);
	if (*fd < 0) {
		perror("shm_open");
		exit(EXIT_FAILURE);
	}

	if (ftruncate(*fd, sizeof(struct shared_state))) {
		perror("ftruncate");
		exit(EXIT_FAILURE);
	}

	shared = mmap(NULL, sizeof(*shared), PROT_READ | PROT_WRITE,
			MAP_SHARED, *fd, 0);
	if (shared == MAP_FAILED) {
		perror("mmap");
		exit(EXIT_FAILURE);
	}

	return shared;
}

static inline void close_shared_state(struct shared_state *shared, int fd)
{
	if (munmap(shared, sizeof(*shared))) {
		perror("munmap");
		exit(EXIT_FAILURE);
	}

	if (close(fd)) {
		perror("close");
		exit(EXIT_FAILURE);
	}
}

#endif
