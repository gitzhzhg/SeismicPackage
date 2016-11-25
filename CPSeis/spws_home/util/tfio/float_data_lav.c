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
#include <stdio.h>
#include <math.h>
#define ZEROBYT 128

long float_data_lavs_( float arr[], long *, long *,
     long *, long *, long *, float lav[], char msg[]);
long float_data_lav_( float arr[], long *, long *,
     long *, float *);

/*------------------------------------------------------------------
C\USER DOC
 *Name   : float_data_lavs_ 
 *Purpose: Given a 2 dimensional array  of floats, return an array 
 *         of Largest Absolute Values(lavs). 
 *Author : R. Day
 *Date   : 04/11/91
 *Revised: 07/17/96  Vunderink
 *
 *Function Definition:        ( Language = C )
 *long float_data_lavs_( float arr[], long *vsize, long *nvecs,
 *   long *ncomp, long *start, long *mode, float lav[], char msg[])
 * arr[]      in    array of floats.
 * *vsize     in    Fast storage dimension size(Vector size).
 * *nvecs     in    Slow storage dimension size(No. of Vectors)
 * *ncomp  in,out   Number of vector components to scan
 * *start     in    Start with this component in each vector.
 *                  The 1st component is = 0.
 * lav[]      out   Float array to contain the lav's.
 * *mode      in    for mode=1 return the lav for each component.
 *                  for mode!=1 return the lav for each vector.
 * msg        out   msk="OK" or an error message.
 * 
 *
 *NOTES:
 * 1. return value = 0 if all is OK. 
 * 2. Size of lav = ncomp+1 for mode=1, and = nvecs+1 for mode!=1
 *    The last lav is the max of all the preceeding lavs
 *
 *Revisions:
 *DATE      WHO         DESCRIPTION
 *--------  --------    --------------------------------------------
 *96/07/17  Vunderink   Inserted into the conlib library.
C\END DOC
 *------------------------------------------------------------------*/
long float_data_lavs_( float arr[], long *vsize, long *nvecs,
     long *ncomp, long *start, long *mode, float lav[], char msg[])
{
  long i;
  register long m,mstrt,mlast,mstride;
  register double big, bigbig;
 
  if(*start < 0 || *start >= *vsize )
    { strcpy(msg,"01:float_data_lavs: bad start value?");
      goto error; }
  *ncomp = (*start + *ncomp >*vsize) ? *vsize-*start : *ncomp;

  bigbig=0.0;
  if(*mode == 1)  /* return lav of each component */
  { for(i= *start;i<*start+*ncomp;i++)
    { big = 0.0;
      mstrt=i;
      mstride = *vsize;
      mlast = mstrt + *vsize*(*nvecs-1);
      for(m=mstrt;m<= mlast;m += mstride)
         { if( fabs((double) arr[m]) > big )
            big = fabs( (double) arr[m]); }
      lav[i-*start] =  big;
      if(big > bigbig) bigbig = big;
    }
    lav[*ncomp] = (float ) bigbig;
  }
  else        /* return lav of each vector*/
  { for(i=0;i< *nvecs;i++)
    { big = 0.0;
      mstrt = *start + i*(*vsize);
      mstride=1;
      mlast = mstrt + *ncomp;
      for(m=mstrt;m<mlast;m += mstride)
         { if( fabs((double) arr[m]) > big )
            big = fabs((double)  arr[m]);}
      lav[i] = (float ) big;
      if(big > bigbig) bigbig = big;
    }
    lav[*nvecs] = (float ) bigbig;
   }

  return 0;
  error:
  return msg[1]-'0';

}

long float_data_lav_( float arr[], long *mstrt, long *mstride,
     long *num, float *lav)
{
  register long m, mlast;
  register double big;
  if(*num < 1 ) return 1;
  if(*mstride ==0 ) return 1;

  big=0.0;
  mlast = *mstrt +  (*num - 1)*( *mstride);
  for(m = *mstrt;m<= mlast;m += *mstride)
    { if( fabs((double)  arr[m]) > big )
            big = fabs((double) arr[m]);
    }
    *lav =  big;

 return 0;
}
