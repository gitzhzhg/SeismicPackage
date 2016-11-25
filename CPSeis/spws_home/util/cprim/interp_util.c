/*<license>
 -------------------------------------------------------------------------------
  Copyright (c) 2007 ConocoPhillips Company
 
  Permission is hereby granted, free of charge, to any person obtaining a copy
  of this software and associated documentation files (the "Software"), to deal
  in the Software without restriction, including without limitation the rights
  to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
  copies of the Software, and to permit persons to whom the Software is
  furnished to do so, subject to the following conditions:
 
  The above copyright notice and this permission notice shall be included in all
  copies or substantial portions of the Software.
 
  THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
  IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
  FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
  AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
  LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
  OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
  SOFTWARE.
 -------------------------------------------------------------------------------
 </license>*/
/*
C      interp_util.c
c        1         2         3         4         5         6         7  |
c23456789012345678901234567890123456789012345678901234567890123456789012|
C\USER DOC
C-----------------------------------------------------------------------
C                     SEISMIC PROCESSING WORKSTATION
C                   EXPLORATION RESEARCH AND SERVICES
C                              CONOCO, INC.
C
C                             U T I L I T Y 
C             written in C -- designed to be called from C
C
C     Utility Name:  interp_util         (interpolation utilities)
C          Written:  94/12/03  by:  Tom Stoeckley
C     Last revised:  94/12/03  by:  Tom Stoeckley
C
C  Purpose:       To interpolate within arrays, and do other related
C                 functions.  Uses binary search to speed things up.
C
C  Related Documentation:
C-----------------------------------------------------------------------
C                            THIS UTILITY
C
C  node:                   pospsv (ultrix)
C  source code directory:  ~spws/util/cprim   (shared)
C  library:                cprim.a            (shared)
C  header file:            cprim.h            (shared)
C  source file:            interp_util.c
C
C  documented functions:   interp_two_point  
C                          interp_floats 
C
C  static functions:       none
C
C  The user should include the above header file in his code.
C-----------------------------------------------------------------------
C                        EXTERNAL REFERENCES
C         (this utility does not reference X, Xt, and Motif)
C              (standard C references not listed here)
C
C  libraries:     cprim.a
C  header files:  cprim.h
C
C  functions:     binary_search_floats
C-----------------------------------------------------------------------
C                           REVISION HISTORY
C
C     Date      Author     Description
C     ----      ------     -----------
C  2.
C  1. 94/12/03  Stoeckley  Initial version.
C-----------------------------------------------------------------------
C                   GENERAL INFORMATION
C
C-----------------------------------------------------------------------
C       THE VARIOUS ROUTINES IN THIS UTILITY ARE LISTED BELOW.
C
C  For each of the following routines, each argument is flagged as
C  follows:  i = value required upon INPUT to the routine.
C            o = value set by the routine upon OUTPUT.
C            b = value BOTH required upon input and changed upon output.
C
C  For pointers, the flag (i,o,b) refers to the contents pointed to 
C  by the pointer, not to the value of the pointer itself.  The pointer
C  value is required upon INPUT in all cases.
C
C  If all of the arguments are INPUT, the flags may be omitted.
C-----------------------------------------------------------------------
C  To interpolate between two points:
C
C      float interp_two_point (float x, float xa, float va,
C                                       float xb, float vb)
C
C  Returns interpolated ordinate corresponding to abscissa x.
C  Uses straight line connecting points (xa,va) and (xb,vb).
C  Does sloping extrapolation if abscissa x is outside range.
C-----------------------------------------------------------------------
C  To interpolate within an array:
C
C    float interp_floats (float x, float *xarray, long n, float *varray)
C
C  Returns interpolated ordinate corresponding to abscissa x.
C  Uses line defined by abscissae xarray[] and ordinates varray[].
C  Array xarray[] must be in increasing or decreasing order.
C  Adjacent equal values in xarray[] are allowed, but in such a case,
C    the ordinate value returned may correspond to any one of the
C    matching abscissae.
C  Does flat extrapolation.
C  Returns 0.0 if n <= 0 or xarray == NULL or varray == NULL.
C
C-----------------------------------------------------------------------
C                                NOTES
C
C  1. 
C
C-----------------------------------------------------------------------
C\END DOC
*/


#include <stdlib.h>
#include <assert.h>
#include "cprim.h"
#include "binary_search.h"



/*------------------- interp two point -------------------------*/
/*------------------- interp two point -------------------------*/
/*------------------- interp two point -------------------------*/

  /* returns interpolated ordinate corresponding to abscissa x */
  /* uses straight line connecting points (xa,va) and (xb,vb) */
  /* does sloping extrapolation if abscissa x is outside range */

float interp_two_point(float x, float xa, float va,
                                float xb, float vb)
{
  if(xa == xb) return 0.5 * (va + vb);
  return va + (x - xa) * (vb - va) / (xb - xa);
}



/*---------------------- interp floats -------------------------*/
/*---------------------- interp floats -------------------------*/
/*---------------------- interp floats -------------------------*/

  /* returns interpolated ordinate corresponding to abscissa x */
  /* uses line defined by abscissae xarray[] and ordinates varray[] */
  /* xarray[] must be in increasing or decreasing order */
  /* adjacent equal values in xarray[] are allowed */
  /* does flat extrapolation */
  /* returns 0.0 if n <= 0 */

#define XTOL 0.0

float interp_floats(float x, float *xarray, long n, float *varray)
{
  long ia, ib;
  if(!xarray || !varray || n <= 0) return 0.0;
  binary_search_floats(xarray, x, XTOL, n, &ia, &ib);
  return interp_two_point(x, xarray[ia], varray[ia],
                                  xarray[ib], varray[ib]);
}



/*------------------------- end ---------------------------------------*/
/*------------------------- end ---------------------------------------*/
/*------------------------- end ---------------------------------------*/

