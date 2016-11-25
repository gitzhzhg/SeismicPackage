/****
!<CPS_v1 type="HEADER_FILE"/>
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
!                      C P S   H E A D E R   F I L E
!
! Name       : cputsys
! Category   : miscellaneous
! Written    : 2000-04-15   by: Vunderink
! Revised    : 2003-04-21   by: Cook
! Maturity   : production   2003-06-05
! Purpose    : Execute a UNIX command from C.
! Portability: No known limitations.
!
!-------------------------------------------------------------------------------
!</brief_doc>


!<history_doc>
!-------------------------------------------------------------------------------
!                       HEADER FILE REVISION HISTORY
!
!     Date        Author       Description
!     ----        ------       -----------
!  2. 2003-06-05  SMCook       Added cputsys_env.
!  1. 2000-03-15  Vunderink    Initial version.  
!                              Moved to production 2000-03-16.
!
!-------------------------------------------------------------------------------
!</history_doc>
****/


/*--------------------------- start of coding ------------------------------*/
/*--------------------------- start of coding ------------------------------*/
/*--------------------------- start of coding ------------------------------*/


#ifndef _CPUTSYS_H_
#define _CPUTSYS_H_


#ifdef __cplusplus
extern "C" {
#endif


#include "c2f_interface.h"


#ifdef NEED_UNDERSCORE 
#define cputsys_cmd       cputsys_cmd_
#define cputsys_env       cputsys_env_
#define cputsys_texec     cputsys_texec_
#endif

#ifdef NEED_CAPITALS
#define cputsys_cmd       CPUTSYS_CMD
#define cputsys_env       CPUTSYS_ENV
#define cputsys_texec     CPUTSYS_TEXEC
#endif


/*------------------------ start of prototypes ---------------------------*/
/*------------------------ start of prototypes ---------------------------*/
/*------------------------ start of prototypes ---------------------------*/


INTEGER cputsys_cmd      (char *value);
INTEGER cputsys_env      (char *var, char *val);
INTEGER cputsys_texec    (char *value);


/*------------------------- end of information ---------------------------*/
/*------------------------- end of information ---------------------------*/
/*------------------------- end of information ---------------------------*/


#ifdef __cplusplus
}
#endif


#endif


/*-------------------------------- end ------------------------------------*/
/*-------------------------------- end ------------------------------------*/
/*-------------------------------- end ------------------------------------*/

