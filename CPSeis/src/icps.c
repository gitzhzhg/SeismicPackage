/*<CPS_v1 type="PROGRAM"/>
!!--------------------------------- icps.c --------------------------------!!
!!--------------------------------- icps.c --------------------------------!!
!!--------------------------------- icps.c --------------------------------!!


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
! Name       : icps              (interactive cps)
! Category   : stand-alone
! Written    : 2003-11-04   by: Tom Stoeckley
! Revised    : 2010-08-17   by: Tom Stoeckley
! Maturity   : beta
! Purpose    : Run a CPS job.
! Portability: No known limitations.
! Parallel   : No.
!-------------------------------------------------------------------------------
!</brief_doc>


!<descript_doc>
!-------------------------------------------------------------------------------
!                          GENERAL DESCRIPTION
!
! Run a CPS job (batch or interactive) using either a workfile or a
! process_parameters file.  Simply run the following from the command line:
!
!                          icps <workfile>
!
! where <workfile> is either a CPS workfile or a process_parameters file.
!
!-------------------------------------------------------------------------------
!</descript_doc>


!<advice_doc>
!-------------------------------------------------------------------------------
!                          ADVICE FOR USERS
!
! 
!-------------------------------------------------------------------------------
!</advice_doc>


!<history_doc>
!-------------------------------------------------------------------------------
!                             REVISION HISTORY
!
!     Date        Author      Description
!     ----        ------      -----------
!  2. 2010-08-17  Stoeckley   Put in comments on how to run icps with gdb.
!  1. 2003-11-04  Stoeckley   Initial Version
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
!                     SPECIAL COMPILING REQUIREMENTS
!
! None required.
!
!-------------------------------------------------------------------------------
!</compile_doc>
*/


/*-------------------------- start of program ------------------------------*/
/*-------------------------- start of program ------------------------------*/
/*-------------------------- start of program ------------------------------*/


char ICPS_IDENT[100] =
"$Id: icps.c,v 1.1 2003/11/04 13:59:04 Stoeckley beta sps $";


#include "c2f_interface.h"
#include "named_constants.h"
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <assert.h>


/*------------------------ fortran spelling adjustments ------------------*/
/*------------------------ fortran spelling adjustments ------------------*/
/*------------------------ fortran spelling adjustments ------------------*/


#if NEED_UNDERSCORE
#define engine_run_from_c    engine_run_from_c_
#elif NEED_CAPITALS
#define engine_run_from_c    ENGINE_RUN_FROM_C
#endif


/*----------------------------- prototypes -------------------------------*/
/*----------------------------- prototypes -------------------------------*/
/*----------------------------- prototypes -------------------------------*/


INTEGER engine_run_from_c (const char *workfile);


/*---------------------------- main program ------------------------------*/
/*---------------------------- main program ------------------------------*/
/*---------------------------- main program ------------------------------*/


int main (int argc, char **argv)
{
  INTEGER istat;
  char filename[222];

  if(argc < 2)
      {
      printf("workfile or parameter_file must be supplied as an argument\n");
      exit(1);
      return 1;
      }

  strcpy(filename, argv[1]);

  if(strcmp(filename,"") == 0)
      {
      printf("workfile or parameter_file must be supplied as an argument\n");
      exit(1);
      return 1;
      }

  istat = engine_run_from_c(filename);
  exit((int)istat);
  return (int)istat;
}


/*------------------------------- end program ------------------------------*/
/*------------------------------- end program ------------------------------*/
/*------------------------------- end program ------------------------------*/

