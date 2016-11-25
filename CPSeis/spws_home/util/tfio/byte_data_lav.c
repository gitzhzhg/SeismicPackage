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

long byte_data_lavs_( unsigned char arr[], long *, long *,
     long *, long *, long *, float lav[],
     register unsigned long pop[], char msg[]);
long byte_data_lav_( unsigned char arr[], long *, long *,
     long *, float *lav);
int byte_data_lavsgn( char *arr ,int num, float *lav);

/*------------------------------------------------------------------
C\USER DOC
 *Name   : byte_data_lavs_
 *Purpose: Given a 2 dimensional array  of unsigned chars, return an
 *         array of Largest Absolute Values(lavs).
 *Author : R. Day
 *Date   : 04/11/91
 *Last revised: 99/02/01 Day
 *
 *Function Definition:        ( Language = C )
 *long byte_data_lavs_( char arr[], long *vsize, long *nvecs,
 *   long *ncomp, long *start, long *mode, float lav[], char msg[])
 *
 *   long byte_data_lav_( unsigned char arr[], long *, long *,
 *    long *, float *lav);
 * int byte_data_lavsgn( char *arr ,int num, float *lav);
 *
 * arr[]      in    array of chars.
 * *vsize     in    Fast storage dimension size(Vector size).
 * *nvecs     in    Slow storage dimension size(No. of Vectors)
 * *ncomp  in,out   Number of vector components to scan. use < vsize
 * *start     in    Start with this component in each vector.
 *                  The 1st component is = 0.
 * *mode      in    for mode=1 return the lav for each component.
 *                  for mode!=1 return the lav for each vector.
 * lav[]      out   Float array to contain the lav's.
 * pop[]      out   pop[i]=number of bytes falling into bin value i,
 *                  where 0 <= i < 256 .
 * msg        out   msk="OK" or an error message.
 *
 *
 *NOTES:
 * 1. Return value = 0 if all is OK.
 * 2. Size of lav = ncomp+1 for mode=1, and = nvecs+1 for mode!=1
 *    The last lav is the max of all the preceeding lavs
 *
 *Revisions:
 *DATE      WHO         DESCRIPTION
 *--------  --------    --------------------------------------------
 *99/01/26  Day         Added byte_data_lavsgn for signed bytes.
 *96/07/17  Vunderink   Inserted into the conlib library.
C\END DOC
            if( ic < little ) little = ic;
      printf("%d %d\n",big,little);
 *-----------------------------------------------------------------*/
long byte_data_lavs_( unsigned char arr[], long *vsize, long *nvecs,
     long *ncomp, long *start, long *mode, float lav[],
     register unsigned long pop[], char msg[])
{
  register long i, j ;
  register long m,mstrt,mlast,mstride;
  register unsigned char big, bigbig, ic;

  if(*start < 0 || *start >= *vsize )
    { strcpy(msg,"01:float_data_lavs: bad start value?");
      goto error; }
  *ncomp = (*start + *ncomp >*vsize) ? *vsize-*start : *ncomp;

  for(j=0;j<=255;j++) pop[j]=0; /* Initialize population curve */
  bigbig=0;
  if(*mode == 1)                /* return lav of each component */
  { for(i= *start;i<*start+*ncomp;i++)
    { big = 0;
      mstrt=i;
      mstride = *vsize;
      mlast = mstrt + *vsize*(*nvecs-1);
      for(m=mstrt;m<=mlast;m += mstride)
         {  ic = abs( arr[m] - ZEROBYT) ;
            if( ic > big ) big = ic;
            pop[arr[m]] += 1; 
         }
      lav[i-*start] =  big;
      if(big > bigbig) bigbig = big;
    }
    lav[*ncomp]=bigbig; 
  }
  else                          /* return lav of each vector*/
  { for(i=0;i< *nvecs;i++)
    { big = 0;
      mstrt = *start + i*(*vsize);
      mstride=1;
      mlast = mstrt + *ncomp;
      for(m=mstrt;m<mlast;m += mstride)
         {  ic = abs( arr[m] - ZEROBYT) ;
            if( ic > big ) big = ic; 
            pop[arr[m]] += 1; 
         }
      lav[i] = (float ) big;
      if(big > bigbig) bigbig = big;
     }
     lav[*nvecs]=bigbig; 
   }

  return 0;
  error:
  return msg[1]-'0';

}

long byte_data_lav_( unsigned char arr[], long *mstrt, long *mstride,
     long *num, float *lav)
{
  register long m, mlast;
  register unsigned char ic, big;
  if(*num < 1 ) return 1;
  if(*mstride ==0 ) return 1;

  big=0;
  mlast = *mstrt +  (*num - 1)*( *mstride);
  for(m = *mstrt;m<= mlast;m += *mstride)
    {  ic = abs( arr[m] - ZEROBYT) ;
       if( ic > big ) big = ic;
    }
    *lav =  big;

 return 0;
}

/*
 * arr.....contains signed byte data
 *         HP won't allow signed char!
 */
int byte_data_lavsgn( char *arr ,int num, float *lav)
{
  register int m;
  register unsigned char ic, jc, big;
 
  if(num < 1 ) return 1;
 
  big=0;
  for(m = 0; m< num ; m++) {
      jc = arr[m] ^ '\x80'; /* convert to unsigned */
      ic = abs( jc - 128);  /* shift origin */
      if( ic > big ) big = ic;
  }
  *lav =  big;
 
 return 0;
}

