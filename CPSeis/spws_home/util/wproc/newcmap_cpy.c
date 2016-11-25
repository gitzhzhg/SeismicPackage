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
#include <X11/Xlib.h>
#include <X11/Xutil.h>
#include <X11/Intrinsic.h>

#define PMAX 256

Colormap newcmap_andcpy_onscreen( Display *dpy,
                                  Screen  *scr,
                                  long     copy_colors )
{
 Window  wind;
 Colormap cmap, dcmap;

 XColor cdefs[PMAX];
 unsigned long pixels[PMAX];
 unsigned long pmsk[20];
 long i, stat;
 Boolean may_do_store= True;


  cmap= 0;
  if (copy_colors < PMAX) {
      dcmap= DefaultColormapOfScreen(scr);
      wind= RootWindowOfScreen(scr);
      cmap=XCreateColormap( dpy, wind, DefaultVisualOfScreen(scr), AllocNone );

      if (dcmap!= cmap) {
         for(i=0;(i<copy_colors);i++) cdefs[i].pixel= i;

         XQueryColors(dpy, dcmap, cdefs, copy_colors );
         stat= XAllocColorCells(dpy, cmap, True, pmsk, 0, pixels, copy_colors);
         if (stat) {
             for (i=0; (i<copy_colors && may_do_store); i++) {
                    may_do_store= (cdefs[i].pixel == pixels[i]);
             } /* end loop */
             if (may_do_store) XStoreColors( dpy, cmap, cdefs, copy_colors); 
         }  /* end if */
      } /* end if */
  } /* end if */

  return (cmap);
}

Colormap newcmap_andcpy( Widget w,
                         long   copy_colors )


{
  Colormap cmap= 0;
  if (copy_colors < PMAX) {
      cmap= newcmap_andcpy_onscreen(XtDisplay(w), XtScreen(w), copy_colors);
  } /* ENDif */
  return (cmap);
}



