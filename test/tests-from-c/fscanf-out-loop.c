
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <errno.h>
#include <fcntl.h>

/* c man pages
   http://www.iso-9899.info/wiki/Web_resources#Man_pages
   https://www.kernel.org/doc/man-pages/ */
int main() {
   char readPath[] = "/home/j/dev/apps/durden-arcan/kbdfs/out";

   FILE * readFile = fopen(readPath, "r");
   if (NULL == readFile) {
      perror("open failed: ");
      printf("Couldn't open file %s for reading.\n", readPath);
      exit(EXIT_FAILURE);
   }

   ssize_t noOfItems = 0;

   long int t = 100;
   long int ms = 100;
   unsigned short int type = 1;
   unsigned short int code = 30;
   int v = 1;

   /*    keysymbol word32  */
   unsigned int ks = 0;
   unsigned short int modifiers = 0;
   unsigned int utf32 = 0;
   unsigned char utf8[6] = {0,0,0,0,0,0} ;
   memset (utf8,0,6);

   while (noOfItems = fscanf(readFile, "%li,%li,%hu,%hu,%i,%i,%hu,%i,%c\n", &t,&ms,&type,&code,&v,&ks,&modifiers,&utf32,&utf8[0])) {
      if(noOfItems == 9) {
	 printf("from in file: %li,%li,%hu,%hu,%i,%i,%hu,%i,%s\n", t,ms,type,code,v,ks,modifiers,utf32,utf8);
      } else if(errno == 0) {
	 printf("No match.\n");
	 break;
      } else {
	 perror("fscanf: ");
	 break;
      }
   }

   int ret = fclose(readFile);
   if (0 != ret) {
      perror("close failed: ");
      printf("Couldn't close file %s opened for reading.\n", readPath);
      exit(EXIT_FAILURE);
   }

   exit(EXIT_SUCCESS);
}
