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
/*
 * wpSystem is like UNIX system, except it uses lib$spawn in VMS.
 * system works in VMS, but is calls a spawn wait.  wpSystem in
 * VMS looks for a trailing & in the command, and if it finds one
 * does a spawn no wait.  wpSystem in UNIX does a system.
 * Putting wpSystem in wproc isolates the developer from VMS, allowing
 * code to be identical for VMS and UNIX.
 * ehs --- 13May94
 */

#include <stdio.h>
#include <string.h>

#ifdef VMS
#include <stddef.h>
#include <lib$routines.h>
#include <descrip.h>
#include <clidef.h>
static int lastChar(char *s, char c, int *index);
#else
#include <stdlib.h>
#include <sys/types.h>
#include <unistd.h>
#include <assert.h>
#include "cprim.h"
static int doUnixSystem(char *command);
#endif /* VMS */

#ifdef sgi
#define vfork fork
#endif

#define True 1
#define False 0

int wpSystem(char *command)
{
	int stat=True;
#ifndef VMS
	stat= doUnixSystem(command);
#else
	struct dsc$descriptor_s desc = {0, DSC$K_DTYPE_T, DSC$K_CLASS_S, NULL};
	int noWait = CLI$M_NOWAIT;
	int wait   = 0           ;
	int index;

	desc.dsc$w_length  = (int) strlen(command);
	desc.dsc$a_pointer = command;

	if (lastChar(command, '&', &index))
	{
		command[index] = '\0';

		lib$spawn(&desc, NULL, NULL, &noWait, NULL, NULL, NULL, NULL,
			NULL, NULL, NULL, NULL);

		command[index] = '&';
	}
	else
	{
		lib$spawn(&desc, NULL, NULL, &wait  , NULL, NULL, NULL, NULL,
			NULL, NULL, NULL, NULL);
	}
#endif /* VMS */
	return stat;
}

#ifdef VMS

static int lastChar(char *s, char c, int *index)
{
	int retval = 0;
	char *last = strrchr(s, (int) c);

	if (last)
	{
		if (strtok(last + 1, " ") == (char *) NULL)
		{
			*index = (int) (last - s);
			retval = 1;
		}
	}

	return retval;
}

#endif /* VMS */


#ifndef VMS

#define ARRAY_LEN 200

static int doUnixSystem(char *command)
{
  
   char *params[ARRAY_LEN];
   int i, dummy_stat, pid, len;
   int retstat, prog_found= False, wait_for_completion= True; /* bool values */
   char prog_with_path[300];
   char *prog, *path, *curr_ptr, *delim;
   static char *tokens= " \f\n\r\t\v";
   static char *sq_delim= "\'";
   static char *dq_delim= "\"";
   static char *space_delim= " ";
          
   assert(command);

   len= strlen(command);
   for(i=len-1; (i>=0 && (isspace(command[i]) || command[i] == '&')); i--) {
         if (command[i] == '&') wait_for_completion= False;; 
         command[i]= '\0';
   }


   for(i=0; (i<ARRAY_LEN); params[i++]= NULL);

   params[0]= prog= command;
   curr_ptr= strpbrk(command, tokens);
   if (curr_ptr)  {
        *curr_ptr= '\0';
        curr_ptr++;
   } 
   
   /*if (prog) printf("prog= %s\n", prog);*/
  for (i=1; (curr_ptr && i<ARRAY_LEN); i++) {
           for(; isspace(*curr_ptr); curr_ptr++);
           if ((*curr_ptr == '\'') || (*curr_ptr == '\"')) {
               delim= ('\'' == *curr_ptr) ? sq_delim : dq_delim;
               curr_ptr++;
           } 
           else {
               delim= space_delim;
           }
           params[i]= curr_ptr;
           curr_ptr= strpbrk(curr_ptr, delim);
           if (curr_ptr) {
                *curr_ptr= '\0';
                curr_ptr++;
           }
           /*if (curr_ptr) printf("curr_ptr= %s\n", curr_ptr);*/
   }

  assert(i<ARRAY_LEN);

   path= newstr(getenv("PATH"));
   if (path) {
       char *ptr;
       for (ptr = strtok(path, ":"); (ptr && !prog_found); 
                                                ptr = strtok(NULL, ":")){
            sprintf(prog_with_path, "%s/%s", ptr, prog);
            if (access(prog_with_path, R_OK | X_OK) == 0) prog_found= True;
       }
       free(path);
       if (prog_found) params[0]= prog_with_path;
       else if (access(prog, R_OK | X_OK) == 0) prog_found= True;
   }


  /*
   *for(i=0; (params[i] != NULL); i++) {
   *     printf("params[%1d]= %s\n", i, params[i]);
   *}
   */

   retstat= prog_found;
   if (prog_found) {
      if ((pid = vfork()) < 0) {
         perror("fork"); 
         retstat=False;
      }
      else if (pid == 0) {
         execv(params[0], params);
         perror(params[0]);
         exit(1);
      }
      if (wait_for_completion) for(;(wait(&dummy_stat) != pid););
   }

   return retstat;
}

#endif



#if 0

main( int argc, char *argv[] )
{
  doUnixSystem("ls -la");
}


#endif
