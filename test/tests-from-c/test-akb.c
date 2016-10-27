
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

int main()
{
   char *readname = "/home/j/dev/apps/plan9/test1/out";
   FILE *readfile;
   char *echoname = "/home/j/dev/apps/plan9/test1/echo";
   FILE *echofile;
   char *writename = "/home/j/dev/apps/plan9/test1/in";
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
   unsigned short int code = 10;
   int v = 1;
   unsigned char direction[20] = {0};
   memset (direction,0,20);
   /*    keysymbol word32  */
   unsigned int ks = 0;
   unsigned short int modifiers = 0;
   unsigned int utf32 = 0;
   unsigned char utf8[6] = {0,0,0,0,0,0} ;
   memset (utf8,0,6);

   printf("%li,%li,%hu,%hu,%i\n", t,ms,type,code,v);
   fprintf(writefile,"%li,%li,%hu,%hu,%i\n", t,ms,type,code,v);
   fflush(writefile);

   fscanf(echofile,"%li,%li,%hu,%hu,%i\n", &t,&ms,&type,&code,&v);
   printf("from echo file: %li,%li,%hu,%hu,%i\n", t,ms,type,code,v);

   fscanf(readfile,"%li,%li,%hu,%hu,%i,%i,%hu,%i,%s\n", &t,&ms,&type,&code,&v,&ks,&modifiers,&utf32,utf8);
   /*    printf("from in file: %li,%li,%hu,%hu,%i,%s,%i,%hu,%i,%s\n", t,ms,type,code,v,direction,ks,modifiers,utf32,utf8); */
   printf("from in file: %li,%li,%hu,%hu,%i\n", t,ms,type,code,v);
   printf("from in file: %li,%li,%hu,%hu,%i,%i,%hu,%i,%s\n", t,ms,type,code,v,ks,modifiers,utf32,utf8);

   fclose(echofile);
   fclose(readfile);
   fclose(writefile);
   return 0;
}
