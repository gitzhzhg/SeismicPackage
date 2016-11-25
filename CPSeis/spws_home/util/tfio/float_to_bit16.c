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
#include "tfio.h"
#include "tfdefs.h"

/*------------------------------------------------------------------
C\USER DOC
 *Name   : float_to_bit16_
 *Purpose: Converts floating point numbers to 16 bit integers and 
 *         interpolates from nin to nout points.
 *Author : R. Day
 *Date   : 95/10/05
 *Revised: 99/10/04/  Day
 *
 *Function Definition:        ( Language = C )
 * void float_to_bit16_(float *ftr, int  *nin,  short *btr,
 *      int  *nout, float *flav)
 * ftr       in        A floating point trace.
 * nin       in        Number of input trace samples.
 * btr       out       Output byte trace.
 * nout      in        Number of output samples requested.
 * flav      in        flav >= A scaling factor. Normally the largest
 *                     absolute value in the input vector. Divide float
 *                     values by this number.
 *
 *NOTES:
 * 1.  Will clip 16bit values to the range 1-65535.
 *
 *Revisions:
 *DATE      WHO         DESCRIPTION
 *--------  --------    --------------------------------------------
 *99/10/04  Day         Correccted a problem with clips on the
 *                      positive amplitudes(was wrapping to negative)
 *96/07/17  Vunderink   Inserted into the conlib library.
C\END DOC
 *** Convert floating point values to unsigned character data ***
 *** Interpolate the samples as we compress to unsigned char  ***
 *** flav........Largest Absolute Value in the float trace.   ***
 *** FirstColor= First color level to employ.                 ***
 *** NumColors = Number of levels of color.                   ***
 *** CPS byte files use FirstColor=1 & NumColors=255          ***
 *------------------------------------------------------------------*/

void float_to_bit16_(float *ftr, int  *nin,  char *btr, float *flav)
{int    i;
 unsigned long swaptest=1;
 int    l;
 int    maxval;
 int    zeropt; 
 int    FirstColor, NumColors, LastColor;
 char c;
 int    zero=0, two=2;
 float sc;

 union {
  char cv[8];
  int  lv;
 } u;

 FirstColor = 1;
 NumColors  = 65536;
 FirstColor = - ( NumColors-1)/2;
 LastColor  = FirstColor + NumColors -1; 

 zeropt = FirstColor + (NumColors -1)/2;
 maxval = (NumColors-1)/2;
 sc = 0.0;
 if(*flav != 0.) sc = maxval/(*flav);

 l = sizeof(int );
    for(i=0;i<*nin;i++) 
     {
      u.lv = zeropt + (int ) (ftr[i]*sc+0.5);
      if(u.lv < FirstColor) u.lv=FirstColor;
      if(u.lv >= LastColor) u.lv=LastColor-1;

      if  (*(char *) &swaptest)
       {c = u.cv[0];
        u.cv[0] = u.cv[1];
        u.cv[1] = c;
       }

#ifdef CRAY
      memcpy(&btr[ 2*i], &u.cv[l-2],2);
#else
      memcpy(&btr[ 2*i], &u.cv[0],2);
#endif
     }

}
