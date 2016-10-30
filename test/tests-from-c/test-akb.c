
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <errno.h>
#include <stdbool.h>

int main()
{
   char *readname = "/home/j/dev/apps/durden-arcan/kbdfs/out";
   FILE *readfile;
   char *echoname = "/home/j/dev/apps/durden-arcan/kbdfs/echo";
   FILE *echofile;
   char *writename = "/home/j/dev/apps/durden-arcan/kbdfs/in";
   FILE *writefile;
   char line_buffer[BUFSIZ];


   readfile = fopen(readname, "r");
   if (!readfile) {
      printf("Couldn't open file %s for reading.\n", readname);
      return 0;
   }

   echofile = fopen(echoname, "r");
   if (!echofile) {
      printf("Couldn't open file %s for echoing.\n", echoname);
      fclose(readfile);
      return 0;
   }

   writefile = fopen(writename, "w");
   if (!writefile) {
      printf("Couldn't open file %s for writing.\n", writename);
      fclose(echofile);
      fclose(readfile);
      return 0;
   }

   long int t = 100;
   long int ms = 100;
   unsigned short int type = 1;
   unsigned short int code = 30;
   int v = 1;
   unsigned char direction[20] = {0};
   memset (direction,0,20);
   /*    keysymbol word32  */
   unsigned int ks = 0;
   unsigned short int modifiers = 0;
   unsigned int utf32 = 0;
   unsigned char utf8[6] = {0,0,0,0,0,0} ;
   memset (utf8,0,6);

   printf("sending to writefile: %li,%li,%hu,%hu,%i\n", t,ms,type,code,v);
   fprintf(writefile,"%li,%li,%hu,%hu,%i\n", t,ms,type,code,v);
   fflush(writefile);

   /* fscanf vs getline/sscanf http://stackoverflow.com/a/22331878 */
   /* use getline instead of fgets */
   int ret = fscanf(echofile,"%li,%li,%hu,%hu,%i\n", &t,&ms,&type,&code,&v);
   if(ret == 5)
      printf("from echo file: %li,%li,%hu,%hu,%i\n", t,ms,type,code,v);
   else if(errno != 0) {
      perror("scanf:");
   } else {
      printf("No match. ret value is %i\n",ret);
   }

   /*    char line[8192]; */
   /*    while (fgets(line, 8192, readfile) != NULL) { */
   /*       printf ("printing line: %s\n",line); */
   /*       printf ("next loop\n"); */
   /*    } */
   /*    perror("fgets:"); */

   ret = fscanf(readfile,"%li,%li,%hu,%hu,%i,%i,%hu,%i,%c\n", &t,&ms,&type,&code,&v,&ks,&modifiers,&utf32,&utf8[0]);
   if(ret == 9)
      printf("from in file: %li,%li,%hu,%hu,%i,%i,%hu,%i,%s\n", t,ms,type,code,v,ks,modifiers,utf32,utf8);
   else if(errno != 0) {
      perror("scanf:");
   } else {
      printf("No match.\n");
   }

   /*    /\*    printf("from in file: %li,%li,%hu,%hu,%i,%s,%i,%hu,%i,%s\n", t,ms,type,code,v,direction,ks,modifiers,utf32,utf8); *\/ */
   /*    printf("from in file: %li,%li,%hu,%hu,%i\n", t,ms,type,code,v); */

   /*    sleep(1); */
   printf("\nagain\n");
   code = 31;
   printf("sending to writefile: %li,%li,%hu,%hu,%i\n", t,ms,type,code,v);
   fprintf(writefile,"%li,%li,%hu,%hu,%i\n", t,ms,type,code,v);
   fflush(writefile);
   /*    sleep(1); */

   /* move back to the beginning of the file */
   fseek(echofile,(long)0,SEEK_SET);
   fseek(readfile,(long)0,SEEK_SET);
   ret = fscanf(echofile,"%li,%li,%hu,%hu,%i\n", &t,&ms,&type,&code,&v);
   if(ret == 5)
      printf("from echo file: %li,%li,%hu,%hu,%i\n", t,ms,type,code,v);
   else if(errno != 0) {
      perror("scanf:");
   } else {
      printf("No match. ret value is %i\n",ret);
   }

   ret = fscanf(readfile,"%li,%li,%hu,%hu,%i,%i,%hu,%i,%c\n", &t,&ms,&type,&code,&v,&ks,&modifiers,&utf32,&utf8[0]);
   if(ret == 9)
      printf("from in file: %li,%li,%hu,%hu,%i,%i,%hu,%i,%s\n", t,ms,type,code,v,ks,modifiers,utf32,utf8);
   else if(errno != 0) {
      perror("scanf:");
   } else {
      printf("No match. ret value is %i\n",ret);
   }
   /*    printf("from in file: %li,%li,%hu,%hu,%i,%s,%i,%hu,%i,%s\n", t,ms,type,code,v,direction,ks,modifiers,utf32,utf8); */
   printf("from in file: %li,%li,%hu,%hu,%i\n", t,ms,type,code,v);
   printf("from in file: %li,%li,%hu,%hu,%i,%i,%hu,%i,%s\n", t,ms,type,code,v,ks,modifiers,utf32,utf8);

   /*    clearerr(readfile); */
   /*    fflush (readfile); */
   /*    while (fgets(line, 8192, readfile) != NULL) { */
   /*       printf ("printing line: %s\n",line); */
   /*       printf ("next loop\n"); */
   /*    } */
   /*    perror("fgets:"); */

   fclose(echofile);
   fclose(readfile);
   fclose(writefile);
   return 0;
}
