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
#include <cprim.h>

#define HI 0
#define LO 32
#define HL 64

/*
C      pack32.c 
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
C     Utility Name:  pack32_ & upack32_ (Pack or Unpack a vector)         
C          Written:  93/07/23  by:  R. Day
C     Last revised:  93/07/23  by:  
C
C  Purpose:       Compress Cray Floating point data from 64 to 32 bits
C                 Input is copied to output on nodes other than the Cray.
C
C-----------------------------------------------------------------------
C                            THIS UTILITY     
C
C  node:                   pospsv (ultrix)
C  source code directory:  ~spws/util/cprim   (shared)
C  library:                cprim.a            (shared)
C  header file:            cprim.h            (shared)
C  source file:            pack32.c
C
C-----------------------------------------------------------------------
C                        EXTERNAL REFERENCES 
C         (this utility does not reference X, Xt, and Motif)
C
C  libraries:
C  header files:
C  functions:
C-----------------------------------------------------------------------
C                           REVISION HISTORY
C
C     Date      Author     Description
C     ----      ------     -----------
C  2.
C  1. 93/07/23  R.Day      Initial version.
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
C                               USAGE
C 
C                    i        i          o         i          i       i
C void pack32_(long *in,long *istr,long *out,long *ostr,long *n,long *hl);
C                    i         i           o           i        i
C void upack32_(long *in, long *istr, long *out,  long *n, long *hl);
C
C  in      --> input buffer
C  out     --> output buffer
C  n       --> number of words to pack or number of words to unpack to
C  istr    --> input memory stride
C  ostr    --> output memory stride
C  hl      --> Get or Put HI,LO, or HI&LO 32 bits of packed words
C                          0 32     64
C
C-----------------------------------------------------------------------
C                                NOTES
C
C  1. Uses a small internal buffer of a few thousand words. Will not
C     work for requests with n>2500. Buffer allows packs and unpacks
C     to be done in place.
C-----------------------------------------------------------------------
C\END DOC
*/
void pack32_(long *in, long *istr, long *out, long *ostr, long *n, long *hl)
{ register int  m,npak,i;
  long odd,low,high,vec[PAKLIM];
  long *in0,*out0;
  int num;

  if(*n > PAKLIM)
   { printf("pack32: error %d\n",n); return; }
  high = ~037777777777;
  low  = ~high;
  odd  = 01;
  npak= *n/2 + (*n&odd);
  in0 = in;
  out0= out;
  num = *n;

  if(sizeof(float) <= 4)
   {/* copy data if no packing needed */
    for(m=0;m<*n;m++)
     {*out = in[m];
      out += *ostr;
     }
     return;
   }

  if(*hl == HI)
   {for(m=0;m<num;m++)
     {vec[m]  = (*out & low) | (*in&high);
      in  += *istr;
      out += *ostr;
     }
    for(m=0;m<num;m++) { out0[m] = vec[m]; }
    return;
   }
  else if(*hl== LO)
   {for(m=0;m<num;m++)
     {vec[m]  = (*out & high) | (*in >> sizeof(float) * 4);
      in  += *istr;
      out += *ostr;
     }
    for(m=0;m<num;m++) { out0[m] = vec[m]; }
    return;
   }
  else
   {long istride;
    istride= *ostr;
    i = 0;
    for(m=0;m<npak;m++)
     {
      vec[i] = *in&high; i++;
      in += *istr;
      vec[i] = *in >> sizeof(float) * 4; i++;
      in += *istr;
     }
    if(2*npak != num) vec[*n] = in0[num*(*istr)] & low;
    for(m=0;m<npak;m++)
     {*out = vec[2*m] | vec[2*m+1];
      out += istride;
     }
   }

}

void upack32_(long *in, long *istr, long *out,  long *n, long *hl)
{ register int  m;
  long high,vec[PAKLIM];
  long num;

  num = *n;
  if(num > PAKLIM)
   { printf("upack32: error %d\n",num); return; }
  if(sizeof(float) <= 4)
   {/* copy data if no packing was possible */
    for(m=0;m<num;m++)
     {*out = *in;
      in  += *istr;
      out++;
     }
     return;
   }

  high = ~037777777777;
  if(*hl == HI)
   {for(m=0;m<num;m++)
     {vec[m]  = *in&high;
      in += *istr;
     }
   }
  else if(*hl==LO)
   {for(m=0;m<num;m++)
     {vec[m]= *in << sizeof(float) * 4;
      in += *istr;
     }
   }
  else
   { long npak,odd,istride;
     register int i; 
     istride = *istr;
     odd  = 01;
     npak= num/2 + (num&odd);
     i = 0;
     for(m=0;m<npak;m++)
      {vec[i]  = *in&high; i++;
       vec[i]= *in << sizeof(float) * 4;  i++;
       in += istride;
      }
   }

  for(m=0;m<num;m++)
   {*out = vec[m];
    out++;
   }
}

