/****
!<CPS_v1 type="PRIMITIVE"/>
****/
/*---------------------------- truevalue.c -------------------------------*/
/*---------------------------- truevalue.c -------------------------------*/
/*---------------------------- truevalue.c -------------------------------*/
 
        /* other files are: */
 
/****
!!! --> Delete from the above list any other files which do NOT exist.
!!! --> There must always be a header file.
!!! --> There must NOT be an truevalue.f90 file.
****/


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
!                        C P S   P R I M I T I V E 
!
! Name       : truevalue
! Category   : velocity
! Written    : 2003-07-17   by: Michael Ried
! Revised    : 2005-05-31   by: Tom Stoeckley
! Maturity   : production
! Purpose    : Determine the a true value between points
! Portability: No known limitations.
! Parallel   : No.
!
!-------------------------------------------------------------------------------
!</brief_doc>


!<descript_doc>
!-------------------------------------------------------------------------------
!                         GENERAL DESCRIPTION   
!
!!!  Using a input array, determine the true value between two points of the 
!!!  input array
!
!-------------------------------------------------------------------------------
!</descript_doc>

!<calling_doc>
!-------------------------------------------------------------------------------
!                      INPUT AND OUTPUT ARGUMENTS
!
! For each subroutine or function documented below, each argument is
! flagged as follows:
!        i = value required upon INPUT.
!        o = value set by the routine upon OUTPUT.
!        b = value BOTH required upon input and changed upon output.
!
! For pointers, the flag (i,o,b) refers to the contents pointed to
! by the pointer, not to the value of the pointer itself.  The pointer
! value (the address) is required upon INPUT in all cases.
!
!-------------------------------------------------------------------------------
!                          CALLING SEQUENCE
!
!!!  --> This is a REQUIRED section which CANNOT be omitted.
!!!
!!!  --> This section should list all functions in this primitive, and
!!!  --> information about their arguments, in a format similar to that
!!!  --> illustrated here, customized as appropriate.
!!!
!!!  --> If this primitive contains several groups of related functions which
!!!  --> would be more clearly documented separately, or if this primitive
!!!  --> contains functions which require sufficient documentation to warrant
!!!  --> a separate section, there can be several sections such as this one,
!!!  --> appropriately individually titled.
!
!                                   i   i         i          o
!                void    truevalue (x, shift_in, maxndx_in, y_out)
!
!
!!!  --> The type and description of each argument should be shown in a table
!!!  --> such as that shown below.  
!
! float   *x         = An array of input values
! float   *shift_in  = An index pule a fraction to a shifted value that we
!                      want to interpolate to find a true value for
! int     *maxndx_in = Maximum # of x input values
! float   *y_out     = Output true value 
!
!-------------------------------------------------------------------------------
!</calling_doc>

!<history_doc>
!-------------------------------------------------------------------------------
!                             REVISION HISTORY
!
!     Date        Author        Description
!     ----        ------        -----------
!  2. 2005-05-31  Stoeckley     Fix to compile with C++.
!  1. 2005-01-10  Michael Ried  Put into CPS from Omega
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


!<algorithm_doc>
!-------------------------------------------------------------------------------
!                   ALGORITHM DESCRIPTION FOR DEVELOPERS
!
!!!  --> This routine interpolates between two value in the input array.
!!!  --> The routine outputs only one value per call.
!
!-------------------------------------------------------------------------------
!</algorithm_doc>
****/


/*-------------------------- start of module ------------------------------*/
/*-------------------------- start of module ------------------------------*/
/*-------------------------- start of module ------------------------------*/


char TRUEVALUE_IDENT[100] =
"$Id: truevalue.c,v 1.2 2005/05/31 13:04:11 Stoeckley prod sps $";

/****
!!!
!!! Put your header files here.
!!!
****/

#include "named_constants.h"
#include "c2f_interface.h"
#include <math.h>

#ifdef NEED_UNDERSCORE
#define truevalue  truevalue_
#elif defined NEED_CAPITALS
#define truevalue  TRUEVALUE
#endif

#define SYNC_SZ 17
#define H_SYNC_SZ 8

#ifdef __cplusplus
extern "C" {
#endif

/*------------------------- start of functions ----------------------------*/
/*------------------------- start of functions ----------------------------*/
/*------------------------- start of functions ----------------------------*/


void truevalue(float *x,        /* Array[*maxndx_in] of input x values */
               float *shift_in, /* An index plus a fraction to a shifted */
                                /* value that we want to interpolate to  */
                                /* find a true value for */
               int *maxndx_in,  /* Maximum number of x input values */
               float *y_out)    /* Output true value */
{
  int i, maxndx, nx, nx2;
  float shift, y, fract_nx;
  double angle, xsin;

  y = 0.0;
  shift = *shift_in;
  maxndx = *maxndx_in;

  nx=(int)(shift+0.5);
  fract_nx = shift - nx;
  xsin=sin(M_PI * (-H_SYNC_SZ - fract_nx));

  if (nx < 0 || nx >= maxndx) {
    y = 0.0;
  }
  else if (fabs(fract_nx) < .05) {
    y = x[nx];
  }
  else {
    for (i=-H_SYNC_SZ; i<=H_SYNC_SZ; i++) {
      nx2=nx+i;
      if (nx2 > 0 && nx2 < maxndx) {
        angle = M_PI * (i - fract_nx);
        y += x[nx2] * (xsin / angle) * (0.54 + 0.46 * 
                       cos(angle / H_SYNC_SZ));
        xsin = -xsin;
      }
    }
  }
  *y_out = y;
}

#ifdef __cplusplus
}
#endif

/*-------------------------------- end ------------------------------------*/
/*-------------------------------- end ------------------------------------*/
/*-------------------------------- end ------------------------------------*/
