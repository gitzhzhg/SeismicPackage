/****
!<CPS_v1 type=AUXILIARY_FILE"/>
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
!                        C P S  A U X I L I A R Y  F I L E
!
! Name       : ciu_crou
! Category   : stand-alone
! Written    : 2002-04-25   by: Donna K. Vunderink
! Revised    : 2002-04-25   by: Donna K. Vunderink
! Maturity   : production
! Purpose    : Utilities Module.
! Portability: No known limitations.
!
!-------------------------------------------------------------------------------
!</brief_doc>


!<history_doc>
!-------------------------------------------------------------------------------
!                     AUXILIARY FILE REVISION HISTORY
!
!     Date        Author       Description
!     ----        ------       -----------
!  1. 2002-04-25  Vunderink    Initial version.
!
!
!
!-------------------------------------------------------------------------------
!</history_doc>
****/


char ciu_crou_ident[100] = "$Id: ciu_crou.c,v 1.1 2002/04/25 20:49:26 Vunderink prod sps $";



#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <time.h>
#include <unistd.h>
#include <sys/types.h>
#include <sys/stat.h>
#include "c2f_interface.h"


#ifdef __cplusplus
extern "C" {
#endif


#ifdef NEED_UNDERSCORE
#define ciu_library_c           ciu_library_c_
#endif

#ifdef NEED_CAPITALS
#define ciu_library_c           CIU_LIBRARY_C
#endif


#define  CIU_UNKNOWN     0
#define  CIU_PRODLIB     1
#define  CIU_BETALIB     2
#define  CIU_ALPHALIB    3

#ifdef PRODLIB
#define LNKLIB  CIU_PRODLIB
#elif BETALIB
#define LNKLIB  CIU_BETALIB
#elif ALPHALIB
#define LNKLIB  CIU_ALPHALIB
#else
#define LNKLIB  CIU_UNKNOWN
#endif


INTEGER ciu_library_c ()
{
  return (INTEGER)LNKLIB;
}

/*-------------------------------- end ------------------------------------*/
/*-------------------------------- end ------------------------------------*/
/*-------------------------------- end ------------------------------------*/

