#include <assert.h>
#include <fcntl.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <sys/mman.h>
#include <unistd.h>

#define DDR_TOTAL_SIZE 0x20000000
#define DDR_BASE_ADDR ((uintptr_t)0x10000000)

void loader(char *imgfile, off_t offset) {
  /* map some devices into the address space of this program */
  int mem_fd = open("/dev/mem", O_RDWR | O_SYNC);
  if (mem_fd == -1) {
    perror("open failed");
    exit(1);
  }
  void *ddr_base = mmap(NULL, DDR_TOTAL_SIZE, PROT_READ | PROT_WRITE,
                        MAP_SHARED, mem_fd, DDR_BASE_ADDR);
  if (ddr_base == NULL) {
    perror("mmap failed");
    close(mem_fd);
    exit(1);
  }

  FILE *fp = fopen(imgfile, "rb");
  assert(fp);

  fseek(fp, 0, SEEK_END);
  long size = ftell(fp);
  printf("image size = %ld\n", size);

  fseek(fp, 0, SEEK_SET);
  size_t nr_read = fread(ddr_base + offset, size, 1, fp);
  printf("payload size %zd\n", nr_read);

  fclose(fp);

  munmap((void *)ddr_base, DDR_TOTAL_SIZE);
  close(mem_fd);
}

int main(int argc, char *argv[]) {

  printf("Load %s at 0x%llx\n", argv[1], strtoll(argv[2], NULL, 0));
  loader(argv[1], strtoll(argv[2], NULL, 0));

  return 0;
}