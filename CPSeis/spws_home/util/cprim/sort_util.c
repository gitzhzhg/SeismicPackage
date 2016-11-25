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
C      sort_util.c
C************************* COPYRIGHT NOTICE ****************************
C*      CONFIDENTIAL AND PROPRIETARY INFORMATION OF CONOCO INC.        *
C*       PROTECTED BY THE COPYRIGHT LAW AS AN UNPUBLISHED WORK         *
C************************* COPYRIGHT NOTICE ****************************
C\USER DOC
C-----------------------------------------------------------------------
C                     SEISMIC PROCESSING WORKSTATION
C                             U T I L I T Y 
C             written in c -- designed to be called from c
C
C     Utility Name:  sort_util    (various small sort utilities)
C          Written:  94/05/19  by:  Tom Stoeckley
C     Last revised:  94/08/10  by:  Tom Stoeckley
C
C  Purpose:     To sort data using a binary sort method.  Several
C               functions are available in this utility, and more
C               will be added as needed.
C
C  Related Documentation:
C-----------------------------------------------------------------------
C                            THIS UTILITY     
C
C  node:                   pospsv (ultrix)
C  source code directory:  ~spws/util/cprim   (shared)
C  library:                cprim.a            (shared)
C  header file:            cprim.h            (shared)
C  source file:            sort_util.c
C
C  static functions:       ffl_fun
C
C  documented functions:   generic_sort  ffl_sort
C
C  The user should include the above header file in his code.
C-----------------------------------------------------------------------
C                        EXTERNAL REFERENCES 
C         (this utility does not reference X, Xt, and Motif)
C
C  libraries:     none
C  header files:  cprim.h
C  functions:     none
C-----------------------------------------------------------------------
C                           REVISION HISTORY
C
C     Date      Author     Description
C     ----      ------     -----------
C  2. 94/08/10  Stoeckley  Changed ff1_sort to fix incorrect order of
C                             primary/secondary sort, and to allow the
C                             last two arguments to be NULL.
C  1. 94/05/19  Stoeckley  Initial version.
C-----------------------------------------------------------------------
C-----------------------------------------------------------------------
C-----------------------------------------------------------------------
C-----------------------------------------------------------------------
C-----------------------------------------------------------------------
C       THE VARIOUS ROUTINES IN THIS UTILITY ARE LISTED BELOW.
C
C  For each of the following routines, each parameter is flagged as
C  follows:   i = value required upon INPUT
C             o = value set by the routine upon OUTPUT
C             b = value BOTH required upon input and changed upon output
C
C  For pointers, the flag (i,o,b) refers to the contents pointed to
C  by the pointer, not to the value of the pointer itself.  The pointer
C  value is required upon INPUT in all cases.
C-----------------------------------------------------------------------
C  To sort anything:
C                             i     i    b
C        void generic_sort (nsort, fun, data)
C
C  long   nsort = number of items to sort.
C  int (*fun)() = user-written function to assist the sort.
C  void   *data = user data containing information to be sorted.
C
C  Prototype of the user-written function:
C
C              switched = fun (data, lo, up)
C
C  void   *data = user data containing information to be sorted.
C  long      lo = first  item number to compare (>= 1 and <  nsort).
C  long      up = second item number to compare (> lo and <= nsort).
C  int switched = TRUE if the two items were switched within this function,
C                   or FALSE otherwise.
C
C  This user-written function must do the following:
C    - Compare item numbers lo and up to see if they need switching.
C        They will need switching if item number up should come before
C        item number lo.  Both lo and up must lie within the range
C        [1,nsort].
C    - Switch the two items within the user data if necessary.
C    - Return TRUE if the items were switched, and FALSE otherwise.
C
C  This routine differes from the ansi-standard function qsort in that
C  the data to be sorted does not have to be arranged in an array of
C  typedefs.
C   - The user-written routine registered with the sorting function
C       qsort needs only to do a compare, and return the result of the
C       compare (+1, -1, or 0).
C   - The user-written routine registered with the sorting function
C       generic_sort needs to do a compare, then switch the the two
C       items if necessary, and return the result of the compare
C       (TRUE or FALSE).
C-----------------------------------------------------------------------
C  To sort three arrays - float, float, and long:
C                           i    b  b  b
C          void ffl_sort (nsort, x, y, r)
C
C  long nsort = number of items to sort.
C  float  x[] = primary sort array.
C  float  y[] = secondary sort array.
C  long   r[] = array which goes along for the ride.
C
C  All three arrays are sorted together.  Array x[] will contain values
C  in ascending order.  For equal values in array x[], array y[] will
C  contain values in ascending order.  The order of items which are
C  equal will not change.
C
C  Array y[] and/or array r[] can be NULL.
C-----------------------------------------------------------------------
C                                NOTES
C
C  1.
C-----------------------------------------------------------------------
C-----------------------------------------------------------------------
C-----------------------------------------------------------------------
C-----------------------------------------------------------------------
C                  FORTRAN SUBROUTINE FOR REFERENCE

      SUBROUTINE TRIPLESORT (Y,X,R,N)
C     SORTS THE 3 LINKED ARRAYS Y(N),X(N),R(N) INTO ASCENDING ORDER.
C     real    array y(n) (e.g. y-coordinate) is the the primary   sort key.
C     real    array x(n) (e.g. x-coordinate) is the the secondary sort key.
C     integer array r(n) (e.g. record number) goes along for the ride.
C     MADE FROM CONSEIS SUBROUTINE SORTER.
      implicit none
      real y(*),x(*),ytemp,xtemp
      integer r(*),n,m,k,j,i,l,rtemp
C------------------------------------------GET STARTED.
      M=N
   20 M=M/2
      IF (M.EQ.0) RETURN
      K=N-M
      DO J=1,K
           I=J+M
   49      L=I
           I=I-M
           if (y(i).gt.y(l).or.
     $        (y(i).eq.y(l).and.x(i).gt.x(l)))then
C------------------------------------------SWITCH INDICES I AND L.
                ytemp=y(i)
                xtemp=x(i)
                rtemp=r(i)
                y(i)=y(l)
                x(i)=x(l)
                r(i)=r(l)
                y(l)=ytemp
                x(l)=xtemp
                r(l)=rtemp
C------------------------------------------KEEP WORKING.
                IF (I.GT.M) GO TO 49
           end if
      END DO
      GO TO 20
      END

C-----------------------------------------------------------------------
C-----------------------------------------------------------------------
C-----------------------------------------------------------------------
C-----------------------------------------------------------------------
C\END DOC
*/


/*---------------------- header files ----------------------------------*/

#include "cprim.h"
  


/*------------------- generic sort -------------------------------------*/

void generic_sort(long nsort,
                  int (*fun)(void *data, long lo, long up), void *data)
{
  long m, k, j, lo, up;

       m = nsort;
  L20: m /= 2;
       if(m == 0) return;
       k = nsort - m;
       for(j = 1; j <= k; j++)
            {
            lo = j + m;
  L49:      up = lo;
            lo -= m;
            if(fun(data, lo, up))
                 {
                 if(lo > m) goto L49;
                 }
             }
      goto L20;
}



/*---------- ffl sort (sort float/float/long arrays) ---------------*/

     /*  float x[] is primary   sort        */
     /*  float y[] is secondary sort        */
     /*  long  r[] goes along for the ride  */

typedef struct _FloatFloatLong
  {
  float *x;
  float *y;
  long  *r;
  } FloatFloatLong;


  /*  lo and up must be >= 1 and <= nsort */

static int ffl_fun(void *data, long lo, long up)
{
  FloatFloatLong *st = (FloatFloatLong*)data;
  float ytemp, xtemp;
  long rtemp;
  long lo2 = lo - 1;
  long up2 = up - 1;

  if(st->x[lo2] > st->x[up2] ||
     (st->x[lo2] == st->x[up2] && st->y && st->y[lo2] > st->y[up2]))
     {
     xtemp = st->x[lo2];  st->x[lo2] = st->x[up2];  st->x[up2] = xtemp;
     if(st->y)
   { ytemp = st->y[lo2];  st->y[lo2] = st->y[up2];  st->y[up2] = ytemp; }
     if(st->r)
   { rtemp = st->r[lo2];  st->r[lo2] = st->r[up2];  st->r[up2] = rtemp; }
     return TRUE;
     }
  return FALSE;
}


void ffl_sort(long nsort, float *x, float *y, long *r)
{
  FloatFloatLong data;

  data.x = x;
  data.y = y;
  data.r = r;
  generic_sort(nsort, ffl_fun, &data);
}


  
/*----------------------------- end ------------------------------------*/
