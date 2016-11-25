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
 *Name   : float_to_byt_ & float_to_bytsgn
 *Purpose: Converts float numbers to unsigned or signed chars.
 *Author : R. Day
 *Last revised: 99/02/01
 *
 *Function Definition:        ( Language = C )
 * void float_to_byt_(float ftr[], int  *nin,  unsigned char btr[],
 *      int  *nout, float *flav)
 * void float_to_bytsgn(float *ftr, int  *nin,  char *btr,
 *      float *flav)
 * ftr       in        A floating point trace.
 * nin       in        Number of input trace samples.
 * btr       out       Output byte trace.
 * nout      in        Number of output samples requested.
 * flav      in        flav >= A scaling factor. Normally the largest
 *                     absolute value in the input vector. Divide float
 *                     values by this number.
 *
 *NOTES:
 * 1.  Will clip unsigned byte values to the range 1 - 255.
 * 2.  Will clip signed byte values to the range -127 to 127
 *
 *Revisions:
 *DATE      WHO         DESCRIPTION
 *--------  --------    --------------------------------------------
 *99/02/01  R.S.Day     removed signed chars, changed algorithm in
 *                      float_to_bytsgn
 *99/02/01  R.S.Day     Updating Conlib
 *99/01/31  R.S.Day     Added function float_to_bytsgn
 *97/04/04  R.S.Day     Will clip in a more consistent manner
 *95/10/05  R.S.Day     float_to_bit16 added
 *92/04/08  R.S.Day     Changed unsigned char cast to char(Cray )
C\END DOC
 *** Convert floating point values to unsigned character data ***
 *** Interpolate the samples as we compress to unsigned char  ***
 *** flav........Largest Absolute Value in the float trace.   ***
 *** FirstColor= First color level to employ.                 ***
 *** NumColors = Number of levels of color.                   ***
 *** CPS byte files use FirstColor=1 & NumColors=255          ***
 *------------------------------------------------------------------*/
void float_to_byt_(float ftr[], int  *nin,  unsigned char btr[],
     int  *nout, float *flav)
{int   i, maxval, index, ival;
 int   zeropt; 
 int   FirstColor, NumColors;
 float sc;
 float factor, r, fraction, scintval;
 float round;

 FirstColor = 1;
 NumColors  = 255;
 zeropt = FirstColor + (NumColors -1)/2;
 maxval = (NumColors-1)/2;
 sc = 0.0;
 if(*flav != 0.) sc = maxval/(*flav);
 factor = (*(float *) nin  -1.)/ ( *(float *) nout -1.);

 if( *nin == *nout) { 
    for(i=0;i<*nout;i++)  {
     ival= (zeropt + (int) (ftr[i]*sc+0.5));
     if(ival < 1) ival=1;
     if(ival > 255) ival=255;
     btr[i] = (unsigned char)ival;
    }
 } else { 
    for(i=0;i<*nout;i++) {
     r = i*factor;
     index = r;
     if(index > *nin-2) { index = *nin -2; r=index; }
     fraction = r - index;
     scintval = sc*(ftr[index] + fraction*(ftr[index+1]-ftr[index]));
     round = ( scintval > 0 ) ? 0.5 : -0.5;
     scintval = scintval + round;
     ival= (zeropt + (int) (ftr[i]*sc+0.5));
     if(ival < 1) ival=1;
     if(ival > 255) ival=255;
     btr[i] = (unsigned char)ival;
    }
 }

 return;
}

void float_to_bytsgn(float *ftr, int  *nin,unsigned  char *btr,
     float *flav)
{int   i,n= *nin, ival;
 float sc=0.0;
 unsigned char by;
 if(*flav != 0.) sc = 127/(*flav);

 for(i=0;i<n;i++)  {
    if(ftr[i]>=0)
     ival= (int) (128 + (int) (ftr[i]*sc + 0.1));
    else
     ival= (int) (128 + (int) (ftr[i]*sc - 0.1));
    if(ival < 1) ival=1;
    if(ival > 255) ival=255;
    by = (unsigned char) ival;
    btr[i] = by ^ '\x80';
 }

}

/*
void float_to_bytsgn(float *ftr, int  *nin,  signed char *btr,
     float *flav)
{int   i,n= *nin, ival;
 float sc=0.0;
 if(*flav != 0.) sc = 127/(*flav);
 
 for(i=0;i<n;i++)  {
    ival= (int) (ftr[i]*sc+0.5);
    if(ival < -127) ival=-127;
    if(ival >  127) ival=127;
    btr[i] = (signed char) ival;
 }
 
}
*/

