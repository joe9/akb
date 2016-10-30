
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <errno.h>
#include <stdbool.h>
#include <sys/types.h>
#include <unistd.h>
#include <fcntl.h>

int main()
{
   /*    char readpath[] = "/home/j/dev/apps/durden-arcan/kbdfs/out"; */
   char readpath[] = "/home/j/dev/apps/plan9/test/out";
   int readfd = 0, ret = 0;

   readfd = open(readpath, O_RDONLY);
   if (-1 == readfd) {
      perror("open failed: ");
      printf("Couldn't open file %s for reading.\n", readpath);
      exit(EXIT_FAILURE);
   }

   char buf[16];
   size_t count = sizeof(buf);
   ssize_t bytes_read = 0;

   while (-1 != (bytes_read = read(readfd, buf, count - 1))){
      printf("contents of %s\n",buf);
   }
   perror("read: ");

   ret = close(readfd);
   if (-1 == ret) {
      perror("close failed: ");
      printf("Couldn't close file %s opened for reading.\n", readpath);
      exit(EXIT_FAILURE);
   }

   exit(EXIT_SUCCESS);
}
