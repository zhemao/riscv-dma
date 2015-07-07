#include <sys/mman.h>
#include <sys/types.h>
#include <sys/fcntl.h>
#include <sys/wait.h>
#include <unistd.h>
#include <semaphore.h>

#include <stdio.h>
#include <stdlib.h>

struct shared_sem {
	int fd;
	sem_t *sem;
};

void create_sem(struct shared_sem *shsem, const char *name)
{
	printf("creating file\n");

	shsem->fd = shm_open(name, O_RDWR | O_CREAT, S_IWUSR | S_IRUSR);
	if (shsem->fd < 0) {
		perror("shm_open");
		exit(EXIT_FAILURE);
	}

	printf("expanding file to proper size\n");

	if (ftruncate(shsem->fd, sizeof(sem_t))) {
		perror("ftruncate");
		exit(EXIT_FAILURE);
	}

	printf("create: mmap semaphore \n");

	shsem->sem = mmap(NULL, sizeof(sem_t), PROT_READ | PROT_WRITE,
			MAP_SHARED, shsem->fd, 0);
	if (shsem->sem == MAP_FAILED) {
		perror("mmap");
		exit(EXIT_FAILURE);
	}

	printf("init semaphore\n");

	if (sem_init(shsem->sem, 1, 0)) {
		perror("sem_init");
		exit(EXIT_FAILURE);
	}
}

void open_sem(struct shared_sem *shsem, const char *name)
{
	printf("opening file\n");

	shsem->fd = shm_open(name, O_RDWR, S_IWUSR | S_IRUSR);
	if (shsem->fd < 0) {
		perror("shm_open");
		exit(EXIT_FAILURE);
	}

	printf("open: mmap semaphore\n");

	shsem->sem = mmap(NULL, sizeof(sem_t), PROT_READ | PROT_WRITE,
			MAP_SHARED, shsem->fd, 0);
	if (shsem->sem == MAP_FAILED) {
		perror("mmap");
		exit(EXIT_FAILURE);
	}
}

void close_sem(struct shared_sem *shsem)
{
	munmap(shsem->sem, sizeof(sem_t));
	close(shsem->fd);
}

void unlink_sem(struct shared_sem *shsem, const char *name)
{
	sem_destroy(shsem->sem);
	munmap(shsem->sem, sizeof(sem_t));
	close(shsem->fd);
	shm_unlink(name);
}

int main(void)
{
	struct shared_sem shsem;
	pid_t pid;

	printf("opening shared memory\n");

	create_sem(&shsem, "semaphore");

	printf("initializing semaphore\n");


	pid = fork();
	if (pid < 0) {
		perror("fork");
		exit(EXIT_FAILURE);
	}

	if (pid == 0) {
		open_sem(&shsem, "semaphore");
		printf("waiting on semaphore\n");
		if (sem_wait(shsem.sem)) {
			perror("sem_wait");
			exit(EXIT_FAILURE);
		}
		close_sem(&shsem);
	} else {
		printf("posting to semaphore\n");
		if (sem_post(shsem.sem)) {
			perror("sem_post");
			exit(EXIT_FAILURE);
		}
		printf("waiting for child to exit\n");
		if (waitpid(pid, NULL, 0) < 0) {
			perror("waitpid");
			exit(EXIT_FAILURE);
		}
		unlink_sem(&shsem, "semaphore");
		printf("program finished\n");
	}

	return 0;
}
