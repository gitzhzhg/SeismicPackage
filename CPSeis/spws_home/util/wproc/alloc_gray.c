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
#include "cenv.h"
#include "wproc.h"
#include <X11/Xlib.h>
#include <X11/Xutil.h>
#include <math.h>

long alloc_gray_colors( Display *dpy,
                        struct COLOR_INFO  *col)

{
  XColor cdef[256];
  XColor cshr;
  long    stat, i;


  for(i=0; (i<24); col->pmsk[i++]= 0);
  for(i=0; (i<500);col->pix[i++]= 0);
  if (NOT(col->shared) ) {
         stat= XAllocColorCells( dpy,       col->cmap, True, 
                                 col->pmsk, col->numplanes, 
                                 col->pix,  col->cnum);
         if (stat) {
             for(i=0;( i<col->cnum ); i++)  {
                  cdef[i].pixel= col->pix[i];
                  cdef[i].red=cdef[i].blue=cdef[i].green= 
                                         (col->cnum-i) * (65535/col->cnum); 
                  cdef[i].flags= DoRed|DoBlue|DoGreen;
             ENDloop
             XStoreColors( dpy, col->cmap, cdef, col->cnum); 
         ENDif
  ENDif
  else {
         for (i=0,stat=True; ( (i<col->cnum) AND (stat) ); i++ ) {
                  cshr.red=cshr.blue=cshr.green=
                           (col->cnum-i) * (65535/col->cnum);
                  stat= XAllocColor( dpy, col->cmap, &cshr );
                  col->pix[i]= cshr.pixel;
         ENDloop
         if (!stat)
                  XFreeColors(dpy, col->cmap, col->pix, i-1, 0);
  ENDelse


  col->colorsafe= stat;
  return (stat);
}
