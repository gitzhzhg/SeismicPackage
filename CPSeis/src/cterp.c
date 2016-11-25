/****
!<CPS_v1 type="PRIMITIVE"/>
***/

/*------------------------------- cterp.c ----------------------------------*/
/*------------------------------- cterp.c ----------------------------------*/
/*------------------------------- cterp.c ----------------------------------*/
 
                      /* other files are:  cterp.h */

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
! Name       : CTERP
! Category   : math
! Written    : 1994-12-03   by: Tom Stoeckley
! Revised    : 2000-07-20   by: Tom Stoeckley
! Maturity   : production   2000-07-26
! Purpose    : Simple C-language interpolation functions.
! Portability: No known limitations.
!
!-------------------------------------------------------------------------------
!</brief_doc>


!<descript_doc>
!-------------------------------------------------------------------------------
!                         GENERAL DESCRIPTION              
!
!  To interpolate within arrays, and do other related
!  functions.  Uses binary search to speed things up.
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
!  For pointers, the flag (i,o,b) refers to the contents pointed to
!  by the pointer, not to the value of the pointer itself.  The pointer
!  value is required upon INPUT in all cases.
!
!-------------------------------------------------------------------------------
!  To interpolate between two points:
!
!                                   i        i         i
!      float cterp_two_point (float x, float xa, float va,
!                                      float xb, float vb)
!
!  Returns interpolated ordinate corresponding to abscissa x.
!  Uses straight line connecting points (xa,va) and (xb,vb).
!  Does sloping extrapolation if abscissa x is outside range.
!
!-------------------------------------------------------------------------------
!  To interpolate within an array:
!
!                              i                 i          i
!    float cterp_floats (float x, const float *xarray, long n,
!                                 const float *varray)
!
!  Returns interpolated ordinate corresponding to abscissa x.
!  Uses line defined by abscissae xarray[] and ordinates varray[].
!  Array xarray[] must be in increasing or decreasing order.
!  Adjacent equal values in xarray[] are allowed, but in such a case,
!    the ordinate value returned may correspond to any one of the
!    matching abscissae.
!  Does flat extrapolation.
!  Returns 0.0 if n <= 0 or xarray == NULL or varray == NULL.
!
!-------------------------------------------------------------------------------
!</calling_doc>


!<history_doc>
!-------------------------------------------------------------------------------
!                              REVISION HISTORY     
!
!     Date        Author     Description
!     ----        ------     -----------
!  5. 2000-07-20  Stoeckley  Fix warning about comment inside comment.
!  4. 1999-11-17  Stoeckley  Add IDENT string for RCS.
!  3. 1999-09-10  Stoeckley  Add reference to other files.
!  2. 1999-08-31  Stoeckley  Converted from old system.
!                              Name changed from interp_util to cterp.
!  1. 1994-12-03  Stoeckley  Initial version.
!
!-------------------------------------------------------------------------------
!</history_doc>
****/


/*-------------------------- start of module ------------------------------*/
/*-------------------------- start of module ------------------------------*/
/*-------------------------- start of module ------------------------------*/


#include "cterp.h"
#include "binary_search.h"
#include <stdlib.h>
#include <assert.h>


char CTERP_IDENT[100] = "$Id: cterp.c,v 1.5 2000/07/25 14:33:30 sps prod sps $";


/*------------------------ cterp two point ------------------------------*/
/*------------------------ cterp two point ------------------------------*/
/*------------------------ cterp two point ------------------------------*/

  /* returns interpolated ordinate corresponding to abscissa x */
  /* uses straight line connecting points (xa,va) and (xb,vb) */
  /* does sloping extrapolation if abscissa x is outside range */

float cterp_two_point(float x, float xa, float va,
                               float xb, float vb)
{
  if(xa == xb) return 0.5 * (va + vb);
  return va + (x - xa) * (vb - va) / (xb - xa);
}



/*--------------------------- cterp floats ------------------------------*/
/*--------------------------- cterp floats ------------------------------*/
/*--------------------------- cterp floats ------------------------------*/

  /* returns interpolated ordinate corresponding to abscissa x */
  /* uses line defined by abscissae xarray[] and ordinates varray[] */
  /* xarray[] must be in increasing or decreasing order */
  /* adjacent equal values in xarray[] are allowed */
  /* does flat extrapolation */
  /* returns 0.0 if n <= 0 */

#define XTOL 0.0

float cterp_floats(float x, const float *xarray, long n, const float *varray)
{
  long ia, ib;
  if(!xarray || !varray || n <= 0) return 0.0;
  binary_search_floats((float*)xarray, x, XTOL, n, &ia, &ib);
  return cterp_two_point(x, xarray[ia], varray[ia],
                            xarray[ib], varray[ib]);
}



/*------------------------------ end ---------------------------------------*/
/*------------------------------ end ---------------------------------------*/
/*------------------------------ end ---------------------------------------*/

