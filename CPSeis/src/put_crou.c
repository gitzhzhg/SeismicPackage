/*<CPS_v1 type="PRIMITIVE", pretag="!"/>
!------------------------------ put_crou.c ----------------------------------
!------------------------------ put_crou.c ----------------------------------
!------------------------------ put_crou.c ----------------------------------
! other files are:  put.f90
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
! Name       : put_crou 
! Category   : io
! Written    : 2001-09-06   by: Bill Menger
! Revised    : 2001-09-12   by: Bill Menger
! Maturity   : beta
! Purpose    : Supply the printf function calls to fortran 90.
! Portability: No known limitations.
!
!-------------------------------------------------------------------------------
!</brief_doc>
!<descript_doc>
!-------------------------------------------------------------------------------
!                         GENERAL DESCRIPTION   
  This provides interfaces to the fprintf, printf, sprintf functions from F90
!-------------------------------------------------------------------------------
!</descript_doc>

!<calling_doc>
!-------------------------------------------------------------------------------
!                      INPUT AND OUTPUT ARGUMENTS        
!
! For each subroutine or function documented below, each argument is
! flagged as follows:
!   i = intent(in)    = value required upon INPUT.
!   o = intent(out)   = value set by the routine upon OUTPUT.
!   b = intent(inout) = value BOTH required upon input and changed upon output.
!
! Optional arguments are also flagged as follows:
!   opt = this argument is optional.
!
!-------------------------------------------------------------------------------
!                          CALLING SEQUENCE        
!   o             i       i
! status = put(        variable)
! status = put(format, variable)
!
! integer                     :: status ! 0 = ok, else bad.
! character(len=*),intent(in) :: format ! C format statement within which to
!                                       ! format your "variable"
! character(len=*),intent(in) :: variable
! integer,intent(in)          :: variable
! real   ,intent(in)          :: variable
! double precision,intent(in) :: variable
!
!
!-------------------------------------------------------------------------------
!</calling_doc>

!<history_doc>
!-------------------------------------------------------------------------------
!                             REVISION HISTORY
!
!     Date        Author      Description
!     ----        ------      -----------
!  3. 2001-09-12  Bill Menger moved the includes.
!  2. 2001-09-10  Bill Menger Added more includes.
!  1. 2001-09-06  Bill Menger Initial version.
!
!-------------------------------------------------------------------------------
!</history_doc>

!<portability_doc>
!-------------------------------------------------------------------------------
!                         PORTABILITY LIMITATIONS 
! No known limitations.
!-------------------------------------------------------------------------------
!</portability_doc>
*/


/*-------------------------- start of module ------------------------------*/
/*-------------------------- start of module ------------------------------*/
/*-------------------------- start of module ------------------------------*/


char put_crou_IDENT[100] =
"$Id: put_crou.c,v 1.19 2001/09/12 15:02:58 mengewm Exp $";

#include "named_constants.h"
#include "c2f_interface.h"

#ifdef NEED_CAPITALS
#define put_int_c PUT_INT_C
#define put_flt_c PUT_FLT_C
#define put_dbl_c PUT_DBL_C
#define put_str_c PUT_STR_C
#endif

#ifdef NEED_UNDERSCORE
#define put_int_c put_int_c_
#define put_flt_c put_flt_c_
#define put_dbl_c put_dbl_c_
#define put_str_c put_str_c_
#endif

#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <math.h>
#include <ctype.h>
#include <assert.h>
#include <stdarg.h>

INTEGER status;

/*------------------------- start of functions ----------------------------*/
/*------------------------- start of functions ----------------------------*/
/*------------------------- start of functions ----------------------------*/

INTEGER put_int_c(const char * format, INTEGER * i) {
  status = printf(format, *i);
  fflush(stdout);
  return status;
}

INTEGER put_flt_c(const char * format, REAL * f) {
  status=printf(format, *f);
  fflush(stdout);
  return status;
}

INTEGER put_dbl_c(const char * format, double *d) {
  status=printf(format, *d);
  fflush(stdout);
  return status;
}

INTEGER put_str_c(const char * format, char * c) {
  status=printf(format, c);
  fflush(stdout);
  return status;
}

/*-------------------------------- end ------------------------------------*/
/*-------------------------------- end ------------------------------------*/
/*-------------------------------- end ------------------------------------*/
