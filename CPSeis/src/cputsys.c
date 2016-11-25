/****
!<CPS_v1 type="PRIMITIVE"/>
!<license>
!-------------------------------------------------------------------------------
! Copyright (c) 2007 ConocoPhillips Company
!
! Permission is hereby granted, free of charge, to any person obtaining a copy
! of this software and associated documentation files (the "Software"), to deal
! in the Software without restriction, including without limitation the rights
! to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
! copies of the Software, and to permit persons to whom the Software is
! furnished to do so, subject to the following conditions:
!
! The above copyright notice and this permission notice shall be included in all
! copies or substantial portions of the Software.
!
! THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
! IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
! FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
! AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
! LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
! OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
! SOFTWARE.
!-------------------------------------------------------------------------------
!</license>

!<brief_doc>
!-------------------------------------------------------------------------------
!                        C P S   P R I M I T I V E
!
! Name       : cputsys 
! Category   : miscellaneous
! Written    : 1999-07-01   by: Donna K. Vunderink
! Revised    : 2009-06-12   by: Bill Menger
! Maturity   : production
! Purpose    : Execute a UNIX command from C.
! Portability: No known limitations.
!
!-------------------------------------------------------------------------------
!</brief_doc>

!<descript_doc>
!-------------------------------------------------------------------------------
!                         GENERAL DESCRIPTION
!
! 
! This primitive is a collection of C routines for executing a UNIX command.
!
! Function                       Description
! --------                       -----------
! cputsys_cmd                    Execute a command using the C routine system
! cputsys_env                    Set the value of an environment variable
! cputsys_texec                  Execute a command using an exec to tcsh
!
!-------------------------------------------------------------------------------
!</descript_doc>

!<calling_doc>
!-------------------------------------------------------------------------------
!                      INPUT AND OUTPUT ARGUMENTS
!
!
! For each subroutine or function documented below, each argument is
! flagged as follows:
!      i = value required upon INPUT.
!      o = value set by the routine upon OUTPUT.
!      b = value BOTH required upon input and changed upon output.
!
!-------------------------------------------------------------------------------
!                          CALLING SEQUENCE
!
!
!To execute UNIX command using the C function system
!
!                  o                   i
!                istat = cputsys_cmd (cmd)
!
! char*  cmd   = UNIX command to be executed 
! long   istat = return status
!                0 = no error occurred
!               -1 = an error occurred
!
!
!To set an environment variable
!
!                  o                   i    i
!                istat = cputsys_env (var, val)
!
! char*  var   = name of variable
! char*  value = value of variable
! long   istat = return status
!                0 = no error occurred
!               -1 = an error occurred
!
!
! To execute a UNIX command using an exec to the tcsh shell
!
!                  o                     i
!                istat = cputsys_texec (cmd)
!
! char*  cmd   = UNIX command to be executed 
! long   istat = return status
!                0 = no error occurred
!               -1 = an error occurred
!
!-------------------------------------------------------------------------------
!</calling_doc>

!<advice_doc>
!-------------------------------------------------------------------------------
!                            ADVICE FOR USERS
!
! 
! 
!
!-------------------------------------------------------------------------------
!</advice_doc>

!<history_doc>
!-------------------------------------------------------------------------------
!                           REVISION HISTORY
!
!     Date        Author       Description
!     ----        ------       -----------
!  7. 2009-06-11  Menger       Changed tcsh to sh in cputsys_texec
!  6. 2004-04-27  Goodger      Change /user/local/bin/tcsh to /bin/tcsh.
!  5. 2004-03-15  Goodger      Change vfork to fork. Add RCS ident string.
!  4. 2003-06-05  SMCook       Added cputsys_env.
!  3. 2001-04-30  Vunderink    Updated documentation tags.
!  2. 2000-03-15  Vunderink    Added routine cputsys_texec.
!  1. 1999-07-01  Vunderink    Initial version.
!
!
!
!-------------------------------------------------------------------------------
!</history_doc>

!<portability_doc>
!-------------------------------------------------------------------------------
!                         PORTABILITY LIMITATIONS
!
! No known limitations.
!
!-------------------------------------------------------------------------------
!</portability_doc>

!<compile_doc>
!-------------------------------------------------------------------------------
!                      SPECIAL COMPILING REQUIREMENTS
!
!
!-------------------------------------------------------------------------------
!</compile_doc>

!<algorithm_doc>
!-------------------------------------------------------------------------------
!                   ALGORITHM DESCRIPTION FOR DEVELOPERS
!
!
!-------------------------------------------------------------------------------
!</algorithm_doc>

!<programming_doc>
!-------------------------------------------------------------------------------
!                           PROGRAMMING NOTES
!
! 
! 
!
!-------------------------------------------------------------------------------
!</programming_doc>
****/

#include "cputsys.h"
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>
#include <sys/types.h>
#include <sys/wait.h>

char cputsys_ident[100] = 
             "$Id: cputsys.c,v 1.6 2004/04/27 11:41:28 Goodger prod sps $";

INTEGER cputsys_cmd (char* value)    
{ int i;

  i = system (value); 
  if (i != 0)
    return(-1);
  else
    return(0); 
}

INTEGER cputsys_env (char *var, char *val)
  {
  char *s;
  INTEGER i,len=0;

  i=0; while(var[i] != '\0') { i++; len++; }
  i=0; while(val[i] != '\0') { i++; len++; }

  len+=2;   /* add one for equals sign and one for null terminator char */

  s=(char *)malloc((size_t)len);
  i=0; while(i<len) { s[i] = '\0'; i++; }

  strcpy(s,var);
  strcat(s,"=");
  strcat(s,val);

  if(putenv(s))
    return -1;
  else
    return 0;

/* DON'T FREE THE MEMORY -- PROGRAM WILL NOT WORK IF YOU DO */
/* free(s); */
  }

INTEGER cputsys_texec (char* value)
{

  pid_t pid;
  int status;

  if (value == 0) return -1;

  if ((pid = fork()) < 0) {
      perror("fork");
      return -1;
  }

  if (pid == 0) {
      char *argv[4];
      argv[0] = "sh";
      argv[1] = "-c";
      argv[2] = value;
      argv[3] = 0;
      execvp("/bin/sh", argv);
      perror(value);
      exit(-1);
  }

  if (waitpid(pid, &status, 0) == -1)
      return -1;
  else {
      if (WIFEXITED(status) != 0)
          return 0;
      else
          return -1;
  }
}
