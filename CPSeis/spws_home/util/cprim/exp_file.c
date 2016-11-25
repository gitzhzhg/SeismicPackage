/*<license>
 -------------------------------------------------------------------------------
  Copyright (c) 2007 ConocoPhillips Company
 
  Permission is hereby granted, free of charge, to any person obtaining a copy
  of this software and associated documentation files (the "Software"), to deal
  in the Software without restriction, including without limitation the rights
  to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
  copies of the Software, and to permit persons to whom the Software is
  furnished to do so, subject to the following conditions:
 
  The above copyright notice and this permission notice shall be included in all
  copies or substantial portions of the Software.
 
  THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
  IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
  FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
  AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
  LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
  OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
  SOFTWARE.
 -------------------------------------------------------------------------------
 </license>*/
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <assert.h>

#if (!VMS) || defined(_POSIX_SOURCE)
#include <unistd.h>
#endif
/*
 * local headers
 */
#include "cprim.h"



#if (VMS)
  static char file_token= ']';
#elif (DOS)
  static char file_token= '\\';
#else
  static char file_token= '/';
#endif

char *getdir();
static void build(char *fullfile, char *filename);


long exp_file( char *fullfile,
                  char *infile)
{

 char *wrkstr;
 char *tokenptr;
 char *current_dir;
 char savchar;
 long success= True;



 if (strlen(infile) == 0) {
     fullfile[0]= '\0';
 }
 else {

     current_dir= getdir();
     wrkstr= newstr( infile);
     tokenptr= strrchr( wrkstr, file_token);

     if (tokenptr) {
         tokenptr++;
         savchar= *tokenptr;
         *tokenptr= '\0';
  
         if ( chdir(wrkstr) == 0) {
              *tokenptr= savchar;
              build(fullfile, tokenptr);
              chdir(current_dir);
         }
         else {
               success= False;
               strcpy( fullfile, infile);
         }

     }
     else {
         build(fullfile, infile);
     }
     free (current_dir);
     free (wrkstr);
 }

 return success;

}


static void build(char *fullfile, char *filename)
{
 char *new_dir;
 int len;

 new_dir= getdir();
 strcpy(fullfile,new_dir);

 len= strlen(fullfile);
 if (fullfile[len - 1] != file_token) {
     fullfile[len]= file_token;
     fullfile[len+1]= '\0';
 }
            
 strcat(fullfile, filename);

 free (new_dir);
 
}



#define INCSIZE 200
#define NUMTRYS 30

char *getdir()
{

   char *dirptr= NULL;
   char *retstr= NULL;
   char *succ= NULL;
   int size, i;

   for(size= INCSIZE, i=0; ( (!succ)&&(i<NUMTRYS) ); size += INCSIZE, i++) {

       dirptr= (char *)malloc(size);
       assert(dirptr);
    
       succ= getcwd(dirptr, size-1);

       if (!succ) free(dirptr);
   } /* End loop */

   if (succ) {
      retstr= newstr( dirptr);
      free(dirptr);
   } /* End if */

   return (retstr);

}
