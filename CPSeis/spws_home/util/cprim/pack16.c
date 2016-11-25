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
/*
 * Pack vector of integers into vector of 16 bit values
 * The integers must be less than 2**15
 */


void pack16_(long *in, long *istride, long *out, long *num)
{
 long n,ival;
 long *iloc;
 unsigned char *oloc;

 if(in == NULL) return;
 if(out== NULL) return;
 if(*num<= 0) return;

 iloc = in;
 oloc = (unsigned char *) out;
 for(n=0;n<*num;n++)
  {ival = *iloc;

   *oloc = (unsigned char) (ival & 0377);
   oloc++;
   *oloc = (unsigned char)  ((ival >> 8) &0377);
   oloc++;

   iloc += *istride;
  }
}

/*
 * Unpack vector of 16 bit values to full integer vector
 */
void unpack16_(long *in, long *out, long *ostride, long *num)
{
 long n,ival;
 long *oloc;
 unsigned char *iloc;

 if(in == NULL) return;
 if(out== NULL) return;
 if(*num<= 0) return;

 iloc = (unsigned char *) in;
 oloc = out;
 for(n=0;n<*num;n++)
  {ival = 0;

   ival = (long ) *iloc;
   iloc++;
   ival = ival | (*iloc << 8);
   *oloc = (long )  ival ;
   iloc++;

   oloc += *ostride;
  }
}
 
