/*<CPS_v1 type="PRIMITIVE"/>

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
! Name       : ppavo_crou
! Category   : velocity
! Written    : 2003-08-26   by: Bill Lucas
! Revised    : 2005-05-31   by: tom Stoeckley
! Maturity   : production
! Purpose    : AVO and velocity analysis C subroutines.
!-------------------------------------------------------------------------------
!</brief_doc>

!<descript_doc>
!-------------------------------------------------------------------------------
!  These are c support routines for internal use by ppavo.
!-------------------------------------------------------------------------------
!</descript_doc>

!<calling_doc>
!-------------------------------------------------------------------------------
! These programs are meant to be used by only ppavo.
!-------------------------------------------------------------------------------
!</calling_doc>

!<history_doc>
!-------------------------------------------------------------------------------
!                          REVISION HISTORY
!     Date        Author       Description
!     ----        ------       -----------
!  2. 2005-05-31  Stoeckley    Fix to compile with C++.
!  1. 2005-01-03  B. Lucas     Initial version.
!-------------------------------------------------------------------------------
!</history_doc>

!<portability_doc>
!-------------------------------------------------------------------------------
!                        PORTABILITY LIMITATIONS
!
!-------------------------------------------------------------------------------
!</portability_doc>

!<compile_doc>
!-------------------------------------------------------------------------------
!                     SPECIAL COMPILING REQUIREMENTS
!
!-------------------------------------------------------------------------------
!</compile_doc>
*/

/*--------------------------- start of coding ------------------------------*/
/*--------------------------- start of coding ------------------------------*/
/*--------------------------- start of coding ------------------------------*/


char PPAVO_CROU_IDENT[100] =
"$Id: ppavo_crou.c,v 1.2 2005/05/31 13:04:10 Stoeckley prod sps $";

#include "c2f_interface.h"
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <assert.h>
#include <math.h>
#include <sys/types.h>
#include <unistd.h>

extern int signgam;


/*---------------------- fortran spelling adjustments ----------------------*/
/*---------------------- fortran spelling adjustments ----------------------*/
/*---------------------- fortran spelling adjustments ----------------------*/


#ifdef NEED_UNDERSCORE
#define ppavo_crou_getpid    ppavo_crou_getpid_
#define ppavo_crou_gamma     ppavo_crou_gamma_
#elif defined NEED_CAPITALS
#define ppavo_crou_getpid    PPAVO_CROU_GETPID
#define ppavo_crou_gamma     PPAVO_CROU_GAMMA
#endif


#ifdef __cplusplus
extern "C" {
#endif

/*--------------------------- ppavo_getpid --------------------------------*/
/*--------------------------- ppavo_getpid --------------------------------*/
/*--------------------------- ppavo_getpid --------------------------------*/

void ppavo_crou_getpid (INTEGER *pid)
{
   pid_t unipid = getpid();  
   *pid = (int)unipid;
   return;
}


/*--------------------------- ppavo_gamma --------------------------------*/
/*--------------------------- ppavo_gamma --------------------------------*/
/*--------------------------- ppavo_gamma --------------------------------*/

void ppavo_crou_gamma (DOUBLE *in, DOUBLE *out)
{
   double value;
   value = exp(lgamma(*in));
   *out = value * signgam;
   return;
}

#ifdef __cplusplus
}
#endif


/*-------------------------------- end ------------------------------------*/
/*-------------------------------- end ------------------------------------*/
/*-------------------------------- end ------------------------------------*/

