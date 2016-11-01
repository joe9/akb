
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <errno.h>
#include <fcntl.h>

/* <joe9> is using fscanf a bad idea? I read that it is ok if you are in control of the data format.
   <cinap_lenrek> YES
   <cinap_lenrek> fscanf is INSANE  [14:56]
   <aiju> joe9: fscanf is a terrible idea pretending to be a library function
   <cinap_lenrek> never use it
   <Aram> fscanf is like doing your tax returns in assembly
   <joe9> https://gist.github.com/7b79b1a6eda771069a0694faaa17bb52 i have this simple program. instead of reading to a line and splitting on comma, using fscanf seemed easier.
   <aiju> %hu,%hu looks like something burnzez would write  [14:57]
   <aiju> %hue%hue
   <joe9> cinap_lenrek: use getline and split it using strtok? or, scanf?
   <aiju> joe9: we already told you scanf is a terrible idea
   <aiju> it just has a tendency to blow up in your fucking face
   <aiju> strtok is also a bad idea mostly  [14:59]
   <joe9> What is the recommended approach?
   <cinap_lenrek> joe9: you could use strtol()
   <aiju> it's called a pointer dad
   <joe9> cinap_lenrek:  ok, Thanks.
   <aiju> in C you don't just string bullshit library functions
   <cinap_lenrek> joe9: which whould stop parsing when it hit the comma
   <aiju> you figure out how to do shit
   <aiju> which usually involves throwing around a few pointers
   <cinap_lenrek> joe9: and it gives you a pointer to the end where it stoped
   <aiju> strtol() is pretty good though
   <aiju> since ^-- what cinap just said
   <aiju> you can then check whether it's pointing at a comma  [15:00]
   <aiju> and then skip the comma and continue :)
   <cinap_lenrek> yeah
   <cinap_lenrek> strtol() also skips over whitespace
   <joe9> fgets vs getline, any preferences? I read warnings about using fgets
   <aiju> fgets() is fine, only gets() is dangerous  [15:01]
   <aiju> but this is all retarded posix crap that we don't really use in plan 9 land
   <aiju> apologies, ansi crap.
   <cinap_lenrek> fgets() seems fine  [15:03]
   <cinap_lenrek> gets() as aiju said is the retarded one
   <cinap_lenrek> and its obvious by looking at the function prototype why */

/* c man pages
   http://www.iso-9899.info/wiki/Web_resources#Man_pages
   https://www.kernel.org/doc/man-pages/ */
int main() {
   char readPath[] = "/home/j/dev/apps/durden-arcan/kbdfs/out";

   FILE * readFile = fopen(readPath, "r");
   if (NULL == readFile) {
      perror("open failed");
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

   while (9 == (noOfItems = fscanf(readFile, "%li,%li,%hu,%hu,%i,%i,%hu,%i,%c\n", &t,&ms,&type,&code,&v,&ks,&modifiers,&utf32,&utf8[0]))) {
      if(noOfItems == 9) {
	 printf("from in file: %li,%li,%hu,%hu,%i,%i,%hu,%i,%s\n", t,ms,type,code,v,ks,modifiers,utf32,utf8);
      }
   }
   if(errno == 0) {
      printf("No match.\n");
      printf("No of items: %zd.\n",noOfItems);
   } else {
      perror("fscanf");
   }

   int ret = fclose(readFile);
   if (0 != ret) {
      perror("close failed");
      printf("Couldn't close file %s opened for reading.\n", readPath);
      exit(EXIT_FAILURE);
   }

   exit(EXIT_SUCCESS);
}
