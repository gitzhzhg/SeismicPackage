/*<CPS_v1 type="HEADER_FILE"/>
!------------------------------- mth.h ----------------------------------
!------------------------------- mth.h ----------------------------------
!------------------------------- mth.h ----------------------------------

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
! Name       : MTH.H
! Category   : math
! Written    : 2002-07-18   by: Charles C. Burch
! Revised    : 2002-07-18   by: Charles C. Burch
! Maturity   : production   2002-07-29
! Purpose    : Provides an interface to math routines in mth_crou.c
! References : These routines are for general use from either c or Fortram 
!-------------------------------------------------------------------------------
!</brief_doc>

!<history_doc>
!-------------------------------------------------------------------------------
!                       HEADER FILE REVISION HISTORY
!     Date        Author          Description
!     ----        ------          -----------
!  1. 2002-07-29  Chuck C. Burch  Initial version.
!-------------------------------------------------------------------------------
!</history_doc>
*/

/*--------------------------- start of coding ------------------------------*/
/*--------------------------- start of coding ------------------------------*/
/*--------------------------- start of coding ------------------------------*/

#ifndef _MTH_H_
#define _MTH_H_

#include "c2f_interface.h"

#ifdef NEED_UNDERSCORE
#define mth_random_number_c      mth_random_number_c_
#define mth_random_numberf_c     mth_random_numberf_c_
#endif

#ifdef NEED_CAPITALS
#define mth_random_number_c      MTH_RANDOM_NUMBER_C
#define mth_random_numberf_c     MTH_RANDOM_NUMBERF_C
#endif


#ifdef __cplusplus
extern "C" {
#endif

/*char *mth_h_ident =*/
/*"$Id: mth.h,v 1.1 2002/07/26 20:25:03 CCBurch prod sps $";*/

/*------------------------ start of prototypes ---------------------------*/
/*------------------------ start of prototypes ---------------------------*/
/*------------------------ start of prototypes ---------------------------*/

/*        function prototypes for mth  routines    */

INTEGER mth_random_number_c(INTEGER*, INTEGER*); 
REAL    mth_random_numberf_c(INTEGER*);

/*------------------------- end of prototypes ---------------------------*/
/*------------------------- end of prototypes ---------------------------*/
/*------------------------- end of prototypes ---------------------------*/


#ifdef __cplusplus
}
#endif

#endif

/*-------------------------------- end ------------------------------------*/
/*-------------------------------- end ------------------------------------*/
/*-------------------------------- end ------------------------------------*/
