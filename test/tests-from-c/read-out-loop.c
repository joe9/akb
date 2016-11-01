
#include <stdio.h>
#include <stdlib.h>
#include <errno.h>
#include <unistd.h>
#include <fcntl.h>
#include <string.h>

/* c man pages
   http://www.iso-9899.info/wiki/Web_resources#Man_pages
   https://www.kernel.org/doc/man-pages/ */
int main() {
   char readpath[] = "/home/j/dev/apps/durden-arcan/kbdfs/out";
   int readfd = 0, ret = 0;

   readfd = open(readpath, O_RDONLY);
   if (-1 == readfd) {
      perror("open failed");
      printf("Couldn't open file %s for reading.\n", readpath);
      exit(EXIT_FAILURE);
   }

   char buf[32];
   size_t count = sizeof(buf);
   ssize_t bytes_read = 0;
   memset (buf,0,count);

   /* count - 1 for ensuring that there a trailing null character at
    * the end */
   while (-1 != (bytes_read = read(readfd, buf, count - 1))) {
      buf[count] = 0;
      printf("contents of %s",buf);
   }
   perror("read");

   ret = close(readfd);
   if (-1 == ret) {
      perror("close failed");
      printf("Couldn't close file %s opened for reading.\n", readpath);
      exit(EXIT_FAILURE);
   }

   exit(EXIT_SUCCESS);
}
