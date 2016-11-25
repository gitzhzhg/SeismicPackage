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
#include "c2f_interface.h"

#include "bswap.h"

/*------------------------------------------------------------------
C\USER DOC
 *Name   : bswap_
 *Purpose: swap the bytes in a float array.
 *Author : R. Day
 *Date   : 10/16/91
 *Last revised: 99/02/01  Day
 *
 *Function Definition:        ( Language = C )
 * void bswap_( int  *num, unsigned char ieee[])
 *  num      in         Number of floats in ieee.
 *  ieee[]   in&out     An array of ieee floating point numbers.
 *
 *NOTES:
 * 1. For ieee data concocted on the cray and sent to DEC machines.
 * 2. Assumes a 4byte word size for the ieee floats.
 * 3. bswap is a noop on the Cray.
 * 4. bswap is a noop if the local machine is not byte swapped.
 *
 *Revisions:
 * DATE      WHO         DESCRIPTION
 * --------  --------    --------------------------------------------
 * 99/02/01  Day         Updating Conlib. Added header file for
 *                       prototyping(no code changes).
 * 96/07/17  Vunderink   Inserted into the conlib library.
C\END DOC
  *------------------------------------------------------------------*/
int  bswap_( int  *num,unsigned char ieee[])
{
  unsigned long swaptest = 1;
  unsigned char c1;
  register int i,i1,i2,i3,i4;
#ifdef CRAY
if(*num > 0 ) return 0;
#endif

  if  (*(char *) &swaptest)
  { for (i=0; i<*num; i++)
    {
      /* byte swaping */
      i1 = i*4;
      i2 = i1+1;
      i3 = i1+2;
      i4 = i1+3;
      c1 = ieee[i1];
      ieee[i1] = ieee[i4];
      ieee[i4] = c1;
      c1 = ieee[i2];
      ieee[i2] = ieee[i3];
      ieee[i3] = c1;

    }
  }
  else return 0;

 return *num;
}
