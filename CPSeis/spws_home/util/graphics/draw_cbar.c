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
#include "gplot.h"

/*
 * nlevel = number of color bar levels
 * luts   = starting color index
 * vals   = vlaue for color index luts
 * vale   = value for color index luts + nlevel - 1
 * corner points specified in normal coordinates
 */
void draw_cbar( long *nlevel,long *luts, float *vals, float *vale,
                float *xnl, float *xnr, float *ynt, float *ynb)
{
 float     xl,xr,yt,yb,dn,annot,xa,ya,xo,yo;
 long      lut,dir,npix,iwk;
 char      string[16];
 int      i,nv,slen;

 if(*nlevel < 2) return;

 xl = *xnl; xr = *xnr;
 dn = (*ynt - *ynb)/(*nlevel);
 if(dn < 0) dn = - dn;
 yb = (*ynb < *ynt) ? *ynb : *ynt;
 yt = yb + dn;
 for(i=0; i< *nlevel; i++)
  {
   lut = *luts + i;
   gsfaci(&lut);
   Xgfr(&xl,&xr,&yt,&yb);
   annot = *vals + i*(*vale - *vals)/(*nlevel - 1);
   sprintf(string,"%g",annot);
   xa = *xnr + .0025;
   ya = (yb + yt)*0.5;
   gsncwc(&xa,&ya,&xo,&yo);
   dir = 0;
   npix = 8;
   draw_str_(string, &xo,&yo,&npix, &dir);
   yt += dn;
   yb += dn;
  }

}

