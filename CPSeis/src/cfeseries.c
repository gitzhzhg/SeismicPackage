/***
!<CPS_v1 type="PROGRAM"/>
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
!                       C P S   P R O G R A M
!
! Name       : cfeseries
! Category   : stand-alone
! Written    : 2000-01-24   by: Donna K. Vunderink
! Revised    : 2003-12-23   by: Karen Goodger
! Maturity   : beta
! Purpose    : Submits jobs based on a job series file
! Portability: No known limitations.
!
!-------------------------------------------------------------------------------
!</brief_doc>


!<descript_doc>
!-------------------------------------------------------------------------------
!                         GENERAL DESCRIPTION
!
!   Usage:  cfeseries [-n] [-p] [-s seriesfile] jobname
!
!           -s          Series file name
!           -n          Use NQS (default)
!           -p          Use PBS
!           jobname     Name if CPS job
!
!-------------------------------------------------------------------------------
!</descript_doc>


!<advice_doc>
!-------------------------------------------------------------------------------
!                            ADVICE FOR USERS
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
!  6. 2003-12-23  Goodger      Change series_sub to cfeseries_f to confrom with
!                              standards.
!  5. 2001-04-11  Vunderink    Added RCS ident string and fixed compiler 
!                                warnings.
!  4. 2001-03-15  Vunderink    Added routine cfeseries_sleep
!  3. 2001-01-23  Vunderink    Added options -n and -p to choose between the NQS
!                                and PBS queuing systems.
!  2. 2000-02-06  Vunderink    Modified to pass jobname and to pass seriesfile
!                                as a -s option.
!  1. 2000-01-24  Vunderink    Initial version.
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
! No special requirements.
!
!-------------------------------------------------------------------------------
!</compile_doc>


!<algorithm_doc>
!-------------------------------------------------------------------------------
!                   ALGORITHM DESCRIPTION FOR DEVELOPERS
!
! Reads the .series file from the users directory
! Decrement the count field by 1
! Submit any job with a count of zero
! If all jobs have a count of less than zero, delete the .series file
!
!-------------------------------------------------------------------------------
!</algorithm_doc>


!<programming_doc>
!-------------------------------------------------------------------------------
!                           PROGRAMMING NOTES
!
!
!-------------------------------------------------------------------------------
!</programming_doc>


!!--------------------------- start of code -------------------------------!!
!!--------------------------- start of code -------------------------------!!
!!--------------------------- start of code -------------------------------!!
****/


#include <ctype.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>
#include "c2f_interface.h"

#ifdef NEED_UNDERSCORE
#define cfeseries_f        cfeseries_f_
#define cfeseries_sleep   cfeseries_sleep_
#endif

#ifdef NEED_CAPITALS
#define cfeseries_f        CFESERIES_F
#define cfeseries_sleep   CFESERIES_SLEEP
#endif

void cfeseries_sleep (int *seconds);
void cfeseries_f (char *jobname,char *seriesfile,char *batchsystem,int *istat);

char cfeseries_ident[100] = 
"$Id: cfeseries.c,v 1.2 2008/02/15 15:10:19 mengewm Exp $";

int main(argc,argv)
int argc;
char *argv[];
{

  int  i = 1;
  int  iflag = 0;
  int  istat;
  char *jobname;
  char *seriesfile;
  char batchsystem[4] = "NQS";
  char ctmp[4]        = "   ";


  jobname    = NULL;
  seriesfile = NULL;

  if (argc == 1) {
    printf("Usage:  cfeseries [-n] [-p] [-s seriesfile] jobname\n");
    printf("\n");
    printf("         -n      Use NQS (default)\n");
    printf("         -p      Use PBS\n");
    printf("         -s      Series file name\n");
    printf("\n");
    exit(1);
  }

  while (i < argc) {
    if (argv[i][0] != '-') {
      if (iflag == 1)
        seriesfile = argv[i];
      else
        jobname = argv[i];
      iflag = 0;
    }
    else {
      switch(toupper(argv[i][1])) {
         case 'N':
               strcpy(batchsystem,"NQS");
               break;
         case 'P':
               strcpy(batchsystem,"PBS");
               break;
         case 'S':
               iflag = 1;
               break;
         default:
               printf("Invalid option \n Type 'cfeseries' to see options\n");
               exit(1);
      }
    }
  i++;
  }

  if (jobname == NULL) jobname = ctmp;

  if (seriesfile == NULL) {
    printf("Error no seriesfile specified\n");
    exit(1);
  }

  cfeseries_f (jobname,seriesfile,batchsystem,&istat);

  return 0;
}

void cfeseries_sleep (int *seconds)
{
  sleep(*seconds);
}
