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
C      math_util.c
C\USER DOC
C-----------------------------------------------------------------------
C                   SEISMIC PROCESSING WORKSTATION
C                         C-LANGUAGE UTILITY
C                 designed to be called from C or C++
C
C     Utility Name:  math_util    (various small math utilities)
C
C     Subdirectory:  cprim         (shared)
C     Library:       cprim.a       (shared)
C     Header file:   cprim.h       (shared)
C     Source file:   math_util.c
C
C     Written:       93/06/11  by:  Tom Stoeckley
C     Last revised:  94/06/24  by:  Tom Stoeckley
C
C  Purpose:     Find array direction, find matching value in an array,
C               convert byte to float and back again, sort arrays,
C               etc.
C
C  Related Documentation:
C-----------------------------------------------------------------------
C                           REVISION HISTORY
C
C     Date      Author     Description
C     ----      ------     -----------
C  5. 94/06/24  Stoeckley  Add regularize_sampling, binary_terp1, and
C                           interpolated_ordinate.  Also, the unfinished
C                           sorting functions were deleted and put into
C                           sort_util.c.
C  4. 93/12/01  Stoeckley  Add several new functions.
C  3. 93/10/29  Stoeckley  Add sorting functions (not yet finished).
C  2. 93/07/20  Stoeckley  Add find_iarray_nearest_match.
C  1. 93/06/11  Stoeckley  Initial version.
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
C  To copy one array to another:
C                                i        o     i
C          void  copy_iarray (iarray1, iarray2, n)
C          void  copy_farray (farray1, farray2, n)
C
C  long  iarray1[] = input array.
C  float farray1[] = input array.
C  long  iarray2[] = output array.
C  float farray2[] = output array.
C  long        n   = number of elements in the arrays.
C-----------------------------------------------------------------------
C  To find the direction of an array:
C            o                                i     i
C        direction = find_iarray_direction (iarray, n)
C        direction = find_farray_direction (farray, n)
C
C  long  iarray[] = array to search through.
C  float farray[] = array to search through.
C  long         n = length of array.
C  long direction = direction of array values. 
C
C  direction =  2 if strictly ascending.
C  direction = -2 if strictly descending.
C  direction =  1 if ascending  (with some adjacent equal values).
C  direction = -1 if descending (with some adjacent equal values).
C  direction =  0 if both ascending and descending, or all equal, or n<=1.
C-----------------------------------------------------------------------
C  To find the minimum and maximum array spacings:
C                                     i     i    o      o
C       void  find_iarray_spacings (iarray, n, &imin, &imax)
C       void  find_farray_spacings (farray, n, &fmin, &fmax)
C
C  long  iarray[] = array to search through.
C  float farray[] = array to search through.
C  long         n = length of array.
C  long      imin = minimum spacing in iarray.
C  long      imax = maximum spacing in iarray.
C  float     fmin = minimum spacing in farray.
C  float     fmax = maximum spacing in farray.
C  
C  The array must be in ascending or descending order.
C-----------------------------------------------------------------------
C  To find bracketing indices and weights in an array:
C                                  i      i     i   o    o    o    o
C    void  find_iarray_brackets (iwant, iarray, n, &ia, &ib, &wa, &wb)
C    void  find_farray_brackets (fwant, farray, n, &ia, &ib, &wa, &wb)
C
C  long  iwant    = value to search for.
C  float fwant    = value to search for.
C  long  iarray[] = array to search through.
C  float farray[] = array to search through.
C  long  ia       = array index immediately preceding value.
C  long  ib       = array index immediately following value.
C  float wa       = weight for location array[ia].
C  float wb       = weight for location array[ib].
C
C  A binary search is conducted on the array.
C  The array must be in ascending or descending order.
C  The returned indices fall in the range from 0 thru n-1.
C  The returned weights are non-negative and add to one.
C
C  If n >= 2 and want falls between array[0] and array[n-1], then
C    this routine returns ia and ib such that:
C       (1) want is between array[ia] and array[ib].
C       (2) ib = ia+1.
C    and this routine returns wa and wb such that:
C         if (array[ia] == array[ib]) then wa = wb = 0.5;
C         otherwise wa = (want - array[ia]) / (array[ib] - array[ia])
C                   wb = (want - array[ib]) / (array[ia] - array[ib]).
C  Otherwise:                         ia   ib   wa   wb 
C    If n <= 1:                        0    0   1.0  0.0
C    If want is closer to array[0]:    0    0   1.0  0.0
C    If want is closer to array[n-1]: n-1  n-1  1.0  0.0
C-----------------------------------------------------------------------
C  To find the nearest matching value in an array:
C
C                                       i    i  i     i          i
C  index = find_iarray_nearest_match (xwant, x, n, direction, adjustment)
C
C  long     xwant  = desired value to find in array x[].
C  long        x[] = array to search through.
C  long         n  = length of array x[].
C  long direction  = direction of array values (see find_iarray_match). 
C  long adjustment = which nearest index to return.
C  long     index  = nearest matching index, such that the absolute
C                      value of (x[index] - xwant) is the smallest
C                      of all indices.
C
C  The returned index is 0 thru n-1 if successful, or -1 if n = 0.
C  adjustment = zero returns index of nearest value to xwant (>, =, or <).
C  adjustment = positive returns index of nearest value >= xwant.
C  adjustment = negative returns index of nearest value <= xwant.
C-----------------------------------------------------------------------
C  To find a matching value in an array:
C
C                                     i    i  i     i
C        index = find_iarray_match (xwant, x, n, direction)
C
C  long     xwant  = desired value to find in array x[].
C  long        x[] = array to search through.
C  long         n  = length of array x[].
C  long direction  = direction of array values. 
C  long     index  = matching index, such that x[index] = xwant.
C
C  The returned index is 0 thru n-1 if successful, or -1 if not.
C  Set direction >=  1 if you know that x[] is increasing.
C  Set direction <= -1 if you know that x[] is decreasing.
C  Set direction ==  0 otherwise (or if you do not know).
C  You can still set direction non-zero if adjacent values in x[] are
C    equal, but in this case you will not know which matching index you
C    get; if you want to guarantee getting the first matching index,
C    set direction = 0.
C  If direction is non-zero, a fast binary search is done.
C  If direction is   zero  , a sequential  search is done.
C-----------------------------------------------------------------------
C  To switch the direction of an array:
C                                         b     i
C        void  switch_iarray_direction (iarray, n)
C        void  switch_farray_direction (farray, n)
C
C  long  iarray[] = array which will be switched backward.
C  float farray[] = array which will be switched backward.
C  long       n   = number of elements in the array.
C-----------------------------------------------------------------------
C  To remove duplicate values from an array:
C                                                b     b
C        void  remove_duplicate_iarray_velues (iarray, n)
C        void  remove_duplicate_farray_velues (farray, n)
C
C  long  iarray[] = array from which duplicate values are to be removed.
C  float farray[] = array from which duplicate values are to be removed.
C  long      *n   = pointer to number of elements in the array.
C
C  The array will be shortened by removing the duplicate values, and
C    the number representing the length of the array will be changed.
C  The array must be sorted in ascending or descending order, so that
C    duplicate values will be adjacent.
C-----------------------------------------------------------------------
C  To convert a byte array to a float array:
C  To convert a float array back to a byte array:
C                                    i      i            o
C        void convert_byte_to_float (n, byte_array , float_array)
C        void return_float_to_byte  (n, float_array, byte_array )
C
C  long                    n  = length of arrays.
C  unsigned char byte_array[] = byte array.
C  float        float_array[] = float array.
C
C  Byte value 128 is actually 0.
C  The smallest byte value is 128 - 127 =   1 (actual value = -127).
C  The largest  byte value is 128 + 127 = 255 (actual value =  127).
C  Byte value 0 is not used.
C  When converting float to byte, it is assumed that the floating
C    values were originally converted from byte values, and that 
C    the magnitudes have not changed significantly.
C-----------------------------------------------------------------------
C  To return the interpolated ordinate between two points:
C                                        i    i   i   i   i
C        yget = interpolated_ordinate (xwant, xa, xb, ya, yb)
C
C  float yget = interpolated ordinate.
C  float xwant = desired abscissa.
C  float xa, ya = one point.
C  float xb, yb = another point.
C
C  Returns yget such that the point (xwant,yget) lies on the straight 
C    line defined by the two points (xa,ya) and (xb,yb).
C  If xa == xb, returns the mean of ya and yb.
C-----------------------------------------------------------------------
C  To do binary-search 1-D interpolation:
C                              i    i  i  i
C        yget = binary_terp1(xwant, x, y, n)
C
C  float yget = interpolated ordinate.
C  float xwant = desired abscissa.
C  float x[n] = array of abscissae.
C  float y[n] = array of ordinates.
C  long  n    = number of (x,y) points described by the arrays.
C
C  Array x must be in ascending or descending order.
C  If n == 0, yget = 0.0 is returned.
C  Does flat extrapolation if xwant is outside the range of array x[n].
C-----------------------------------------------------------------------
C  To regularize the sampling of points:
C                               i  i  i   i     i      i      o
C      void regularize_sampling(x, y, n, xmin, xmax, nbins, array)
C
C  float x[n] = array of abscissae.
C  float y[n] = array of ordinates.
C  long  n    = number of (x,y) points described by the arrays.
C  float xmin = desired abscissa for array[0].
C  float xmax = desired abscissa for array[nbins-1].
C  long  nbins = desired number of points in the output array.
C  float array[nbins] = output array of ordinates.
C
C  Array x must be in ascending or descending order.
C  The output array contains ordinates for equally-spaced abscissae
C    from xmin to xmax.
C  If n == 0, the output array will be filled with 0.0.
C  Does flat extrapolation if xmin and/or xmax lie outside the range
C    of array x[n].
C-----------------------------------------------------------------------
C\END DOC
*/


/*---------------------- header files ----------------------------------*/

#include "cprim.h"
#include "named_constants.h"
#include <assert.h>
  


/*----------------- copy array1 to array2 -----------------------------*/

#define CARR                         \
{                                    \
  int i;                             \
                                     \
  for(i = 0; i < n; i++)             \
       {                             \
       array2[i] = array1[i];        \
       }                             \
}


void copy_iarray(long  *array1, long  *array2, long n)  CARR
void copy_farray(float *array1, float *array2, long n)  CARR



/*----------------- find direction of array ---------------------------*/

#define ARRAY_DIRECTION                                       \
{                                                             \
  int i, ascend = 0, descend = 0, equal = 0;                  \
                                                              \
  for(i = 1; i < n; i++)                                      \
       {                                                      \
       if     (array[i] > array[i - 1]) ascend  = 1;          \
       else if(array[i] < array[i - 1]) descend = 1;          \
       else                             equal   = 1;          \
       }                                                      \
  if(ascend == 1 && descend == 0 && equal == 0) return  2;    \
  if(ascend == 0 && descend == 1 && equal == 0) return -2;    \
  if(ascend == 1 && descend == 0              ) return  1;    \
  if(ascend == 0 && descend == 1              ) return -1;    \
  return 0;                                                   \
}


long find_iarray_direction(long  array[], long n)    ARRAY_DIRECTION
long find_farray_direction(float array[], long n)    ARRAY_DIRECTION



/*-------------- find minimum and maximum array spacings --------------*/
         /* array must be in ascending or descending order */

#define FAS                                             \
  int i;                                                \
                                                        \
  if(n == 0) { *min = *max = 0; return; }               \
  for(i = 1; i < n; i++)                                \
       {  test = AbsoluteValue(array[i] - array[i-1]);  \
          if(i == 1) { *min = *max = test; }            \
          else   { *min = MinimumValue(*min, test);     \
                   *max = MaximumValue(*max, test); }   \
       }


void find_iarray_spacings(long  array[], long n, long  *min, long  *max)
      { long  test; FAS }
void find_farray_spacings(float array[], long n, float *min, float *max)
      { float test; FAS }


/*---------------- find bracketing indices in an array ----------------*/

#define FFBB                                                          \
  long i, direction;                                                  \
                                                                      \
  *ia = 0;   *ib = MaximumValue(n - 1, 0);                            \
  *wa = 1.0; *wb = 0.0;                                               \
  if(n <= 1) return;                                                  \
  if(array[*ib] >= array[*ia]) direction =  1;                        \
  else                         direction = -1;                        \
  if(direction * (array[*ia] - want) >= zero) { *ib = *ia; return; }  \
  if(direction * (array[*ib] - want) <= zero) { *ia = *ib; return; }  \
  while(*ib > *ia + 1)                                                \
       {                                                              \
       i = (*ia + *ib)/2;                                             \
       if(direction * (array[i] - want) > zero) *ib = i;              \
       else                                     *ia = i;              \
       }                                                              \
  if(*ia == *ib) return;                                              \
  if(array[*ia] == array[*ib]) *wa = 0.5;                             \
  else *wa = (want - array[*ia]) / (array[*ib] - array[*ia]);         \
  *wb = 1.0 - *wa;


void find_iarray_brackets(long  want, long  array[], long n,
                            long *ia, long *ib, float *wa, float *wb)
                 { long  zero = 0  ; FFBB }

void find_farray_brackets(float want, float array[], long n,
                            long *ia, long *ib, float *wa, float *wb)
                 { float zero = 0.0; FFBB }



/*---- find index of nearest matching value (in array of type long) ---*/

long find_iarray_nearest_match(long xwant, long x[], long n,
                                          long direction, long adjustment)
{
  int i, la, lb;
  int k, m=0, k2, m2=0;
  long a;

  if(n <= 0)
       {
       return -1;                                          /* array is empty */
       }
  else if(n == 1)
       {
       return 0;                                        /* array of length 1 */
       }
  else if(x[n-1] != x[0])                /* see if calculated location works */
       {
       i = ((xwant - x[0]) * (n-1)) / (x[n-1] - x[0]);
       if(i >= 0 && i < n && x[i] == xwant) return i;   /* calculation works */
       }
  if(direction == 0 || n < 5)                        /* do sequential search */
       {
       k = k2 = -1;
       for(i = 0; i < n; i++)
            {
            if(x[i] == xwant) return i;                       /* found match */
            a = AbsValue(x[i] - xwant);
            if(k == -1 || a < m) { m = a; k = i; }
            if( ( (adjustment > 0 && x[i] >= xwant) ||
                  (adjustment < 0 && x[i] <= xwant) ) &&
                (k2 == -1 || a < m2) ) { m2 = a; k2 = i; }
            }
       if(k2 >= 0) k = k2;
       return k;                                       /* did not find match */
       }
  la = 0;                                                /* do binary search */
  lb = n-1;
  if(direction * (x[la] - xwant) > 0) return   0; /* xwant is off bottom end */
  if(direction * (x[lb] - xwant) < 0) return n-1;    /* xwant is off top end */
  if(x[la] == xwant) return la;                               /* found match */
  if(x[lb] == xwant) return lb;                               /* found match */
  while(lb > la + 1)
       {
       i = (la + lb)/2;
       if(x[i] == xwant) return i;                            /* found match */
       if(direction * (x[i] - xwant) > 0) lb = i;
       else                               la = i;
       }
  if(direction > 0 && adjustment > 0) return lb;       /* did not find match */
  if(direction > 0 && adjustment < 0) return la;       /* did not find match */
  if(direction < 0 && adjustment > 0) return lb;       /* did not find match */
  if(direction < 0 && adjustment < 0) return la;       /* did not find match */
  if(AbsValue(x[lb] - xwant) <= AbsValue(x[la] - xwant)) return lb;
  return la;
}



/*-------- find index of matching value (in array of type long) -------*/

long find_iarray_match(long xwant, long x[], long n, long direction)
{
  int i, la, lb;

  if(n <= 0)
       {
       return -1;                                          /* array is empty */
       }
  else if(n == 1)
       {
       if(x[0] == xwant) return 0;              /* array of length 1 matches */
       return -1;                        /* array of length 1 does not match */
       }
  else if(x[n-1] != x[0])                /* see if calculated location works */
       {
       i = ((xwant - x[0]) * (n-1)) / (x[n-1] - x[0]);
       if(i >= 0 && i < n && x[i] == xwant) return i;   /* calculation works */
       }
  if(direction == 0 || n < 5)                        /* do sequential search */
       {
       for(i = 0; i < n; i++)
            { if(x[i] == xwant) return i; }                   /* found match */
       return -1;                                      /* did not find match */
       }
  la = 0;                                                /* do binary search */
  lb = n-1;
  if(direction * (x[la] - xwant) > 0) return -1;  /* xwant is off bottom end */
  if(direction * (x[lb] - xwant) < 0) return -1;     /* xwant is off top end */
  if(x[la] == xwant) return la;                               /* found match */
  if(x[lb] == xwant) return lb;                               /* found match */
  while(lb > la + 1)
       {
       i = (la + lb)/2;
       if(x[i] == xwant) return i;                            /* found match */
       if(direction * (x[i] - xwant) > 0) lb = i;
       else                               la = i;
       }
  return -1;                                           /* did not find match */
}



/*--------------- switch the direction of an array ---------------------*/

#define SAD                                                       \
  long i, j, n2;                                                  \
                                                                  \
  if(n <= 1) return;                                              \
  n2 = n / 2;                                                     \
  for(i = 0; i < n2; i++)                                         \
       {                                                          \
       j = n - i - 1;                                             \
       temp = array[i]; array[i] = array[j]; array[j] = temp;     \
       }


void switch_iarray_direction(long  *array, long n) { long  temp; SAD }
void switch_farray_direction(float *array, long n) { float temp; SAD }



/*--------------- remove duplicate values in an array ------------------*/

#define RDAV                                                      \
{                                                                 \
  int i, j = 1;                                                   \
                                                                  \
  if(*n <= 1) return;                                             \
  for(i = 1; i < *n; i++)                                         \
       {                                                          \
       if(array[i] != array[j-1]) { array[j] = array[i]; j++; }   \
       }                                                          \
  *n = j;                                                         \
}


void remove_duplicate_iarray_values(long  *array, long *n) RDAV
void remove_duplicate_farray_values(float *array, long *n) RDAV



/*---------- convert byte to float -------------------------------------*/
/*---------- return float to byte --------------------------------------*/

void convert_byte_to_float(long n, unsigned char b[], float f[])
{
  int i;
  for(i = 0; i < n; i++)
       {
       f[i] = (float)((int)b[i] - 128);
       }
}

void return_float_to_byte(long n, float f[], unsigned char b[])
{
  int i;
  float a;
  for(i = 0; i < n; i++)
       {
       a = f[i];
       if     (a >  127.0) a =  127.0;
       else if(a < -127.0) a = -127.0;
       b[i] = (unsigned char)(128.5 + a);
       }
}


/*---------- return interpolated ordinate between two points -------*/

/*
   Returns y such that the point (xwant,y) lies on the straight 
     line defined by the two points (xa,ya) and (xb,yb).
   If xa == xb, returns the mean of ya and yb.

      FUNCTION YROOT (XROOT,XA,XB,YA,YB)
      IF (XA.EQ.XB) GO TO 10
      YROOT=YA+(XROOT-XA)*(YB-YA)/(XB-XA)
      RETURN
10    YROOT=(YA+YB)/2.
      RETURN
      END
*/

float interpolated_ordinate(float xwant,
                       float xa, float xb, float ya, float yb)
{
  if(xa == xb) return ( (ya + yb) / 2.0 );
  return ( ya + (xwant - xa) * (yb - ya) / (xb - xa) );
}



/*----------- routine to do binary 1-D interpolation ---------------*/
                  /* does flat extrapolation */

float binary_terp1(float xwant, float *x, float *y, long n)
{
  long ia, ib;
  float wa, wb;

  assert(n >= 0);
  if(n == 0) return 0.0;
  find_farray_brackets(xwant, x, n, &ia, &ib, &wa, &wb);
  return ( wa * y[ia] + wb * y[ib] );
}


/*---------- routine to regularize sampling ----------------------------*/

      /* xmin (minimum abscissa) is mapped to array[0]       */
      /* xmax (maximum abscissa) is mapped to array[nbins-1] */
 /* fills float array with zeros if there are no picks */
      /* x[npoints] are the abscissae */
      /* y[npoints] are the ordinates */

void regularize_sampling(float *x, float *y, long n,
                         float xmin, float xmax,
                         long nbins, float *array)
{
  int i;
  float dx, xwant;

  if (nbins > 1) dx = (xmax - xmin)/(nbins - 1);
  else           dx = 0.0;

  for(i=0; i<nbins; i++)
       {
       xwant = xmin + i * dx;
       array[i] = binary_terp1(xwant, x, y, n);
       }
}

 
  
/*----------------------------- end ------------------------------------*/
