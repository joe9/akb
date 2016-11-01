
#include <stdlib.h>
#include <limits.h>
#include <stdio.h>
#include <errno.h>
#include <string.h>
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

long int readNumber(char **nptr){
   char *endptr = NULL, *str = *nptr;

   errno = 0;    /* To distinguish success/failure after call */
   long val = strtol(str, &endptr, 10);
   /* Check for various possible errors */
   if ((errno == ERANGE && (val == LONG_MAX || val == LONG_MIN))
       || (errno != 0 && val == 0)) {
      perror("strtol");
      exit(EXIT_FAILURE);
   }
   if (endptr == str) {
      fprintf(stderr, "No digits were found\n");
      exit(EXIT_FAILURE);
   }
   *nptr = endptr;
   return val;
}

int readValues(char *line, long int * t, long int *ms,unsigned short int * type,unsigned short int * code, int * v, unsigned int * ks,unsigned short int * modifiers,unsigned int * utf32,unsigned char * utf80) {
   char **ptr = &line;
   *t = readNumber(ptr);
   *ptr = (*ptr) + sizeof((char)0); 
   *ms = readNumber(ptr);
   *ptr = (*ptr) + sizeof((char)0); 
   *type = (unsigned short int) readNumber(ptr);
   *ptr = (*ptr) + sizeof((char)0); 
   *code = (unsigned short int)readNumber(ptr);
   *ptr = (*ptr) + sizeof((char)0); 
   *v = (int)readNumber(ptr);
   *ptr = (*ptr) + sizeof((char)0); 
   *ks = (unsigned int)readNumber(ptr);
   *ptr = (*ptr) + sizeof((char)0); 
   *modifiers = (unsigned short int)readNumber(ptr);
   *ptr = (*ptr) + sizeof((char)0); 
   *utf32 = (unsigned int)readNumber(ptr);
   *ptr = (*ptr) + sizeof((char)0); 
   *utf80 = (char)readNumber(ptr);

   if (**ptr != '\n')        /* Not necessarily an error... */
      printf("Further characters after number: %s\n", *ptr);
   return 9;
}

/* c man pages
   http://www.iso-9899.info/wiki/Web_resources#Man_pages
   https://www.kernel.org/doc/man-pages/ */
/* gdb -x gdb-loop-on-out-file-contents.ini loop-on-out-file-contents */
int main() {
   char readPath[] = "/home/j/dev/apps/durden-arcan/kbdfs/out";

   FILE * readFile = fopen(readPath, "r");
   if (NULL == readFile) {
      perror("open failed");
      printf("Couldn't open file %s for reading.\n", readPath);
      exit(EXIT_FAILURE);
   }

   char *line = NULL;
   size_t len = 0;
   ssize_t read;

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

   while (-1 != (read = getline(&line, &len, readFile))) {
      printf("Retrieved line of length %zu: \n", read);
      printf("%s", line);

      int n = readValues(line, &t,&ms,&type,&code,&v,&ks,&modifiers,&utf32,&utf8[0]); 
      if(n == 9) {
	 printf("from in file: %li,%li,%hu,%hu,%i,%i,%hu,%i,%s\n", t,ms,type,code,v,ks,modifiers,utf32,utf8);
      } else if(errno == 0) {
	 printf("No match.\n");
	 break;
      } else {
	 perror("fscanf");
	 break;
      }

      /* If we got here, strtol() successfully parsed the numbers */
      printf("read from read_file: %li,%li,%hu,%hu,%i,%i,%hu,%i,%s\n", t,ms,type,code,v,ks,modifiers,utf32,utf8);
   }
   free(line);
   int ret = fclose(readFile);
   if (0 != ret) {
      perror("close failed");
      printf("Couldn't close file %s opened for reading.\n", readPath);
      exit(EXIT_FAILURE);
   }

   exit(EXIT_SUCCESS);
}
