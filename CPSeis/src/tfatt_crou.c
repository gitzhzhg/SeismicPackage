/****
!<CPS_v1 type="AUXILIARY_FILE"/>
****/
/*----------------------------- tfatt_crou.c -------------------------------*/
/*----------------------------- tfatt_crou.c -------------------------------*/
/*----------------------------- tfatt_crou.c -------------------------------*/

    /* other files are:  tfatt.f90 */

/****
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
!                         C P S   P R O C E S S
!
! Name       : TFATT_CROU
! Category   : transforms
! Written    : 2001-07-21   by: Randy Selzler
! Revised    : 2005-10-10   by: Tom Stoeckley
! Maturity   : production
! Purpose    : Generate t-f spectra or t-f attributes of input traces.
! Portability: No known limitations.
!
!-------------------------------------------------------------------------------
!</brief_doc>


!<history_doc>
!-------------------------------------------------------------------------------
!                     AUXILIARY FILE REVISION HISTORY 
!
!     Date        Author     Description
!     ----        ------     -----------
!  3. 2005-10-10  Stoeckley  Fix to compile with C++.
!  2. 2001-10-30  Selzler    Added attribute option
!  1. 2001-07-11  Selzler    Initial version.
!
!-------------------------------------------------------------------------------
!</history_doc>
****/

/*--------------------------- start of coding ------------------------------*/
/*--------------------------- start of coding ------------------------------*/
/*--------------------------- start of coding ------------------------------*/

char TFATT_CROU_IDENT[100] =
"$Id: tfatt_crou.c,v 1.3 2005/10/10 11:21:15 Stoeckley prod sps $";

#include "c2f_interface.h"
#include "rice.h"

#ifdef NEED_UNDERSCORE
#define tfatt_crou_rice_timefreq    tfatt_crou_rice_timefreq_
#elif defined NEED_CAPITALS
#define tfatt_crou_rice_timefreq    TFATT_CROU_RICE_TIMEFREQ
#endif

/*------------------------ start of functions ----------------------------*/
/*------------------------ start of functions ----------------------------*/
/*------------------------ start of functions ----------------------------*/

#ifdef __cplusplus
extern "C" {
#endif


int tfatt_crou_rice_timefreq(
  REAL *trace_in,
  const INTEGER *nsamp_in,
  REAL *trace_out,
  REAL *srval,
  INTEGER *num_traces_in,
  char *kernal,
  int *mode_cnt,
  char (*mode)[2*sizeof(int)],
  char *window,
  char *scale,
  char *correct,
  INTEGER *winlen,
  INTEGER *fftlen,
  REAL *dBthr,
  REAL *vol) {

  return rice_timefreq2(
    trace_in,
    *nsamp_in,
    trace_out,
    *srval,
    *num_traces_in,
    kernal,
    *mode_cnt,
    mode,
    window,
    scale,
    correct,
    *winlen,
    *fftlen,
    (double)*dBthr,
    (double)*vol);
}

#ifdef __cplusplus
}
#endif


/*-------------------------------- end ------------------------------------*/
/*-------------------------------- end ------------------------------------*/
/*-------------------------------- end ------------------------------------*/

