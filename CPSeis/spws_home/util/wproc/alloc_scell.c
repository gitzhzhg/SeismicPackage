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
/*
 * Name        : alloc_scell
 * File        : alloc_scell.c
 * Library     : wproc
 * Author      : Trey Roby
 * Date        : 10/1/92
 *
 * Allocate a some shared color cells on failure return black.
 *
 */

#include <stdio.h>
#include <X11/Xlib.h>
#include <X11/Xutil.h>
#include "cenv.h"
#include "wproc.h"


#define BLACKSTR "black"
#define WHITESTR "white"


unsigned long alloc_scell( Display            *dpy,
                           Colormap           cmap,
                           struct COLOR_SCELL *sc,
                           Boolean            printerr,
                           XColor             *cdef,    /*returned- if !NULL */
                           long               *ret_stat)/*returned- if !NULL */


{

  Screen        *scr;
  long          cflags;
  long          class;
  char          *defstr;
  unsigned long defpix;
  unsigned long retpix=0;
  XColor        *cols, actdef, rgbdef;
  long          stat_holder, *stat;



  scr= DefaultScreenOfDisplay(dpy);
  cflags= test_vis( dpy, &class, NULL);

  /*
   * if the user passed me a XColor struct then it will be populated
   * with the result of XAllocNamedColor
   * otherwise he passed a NULL and does not get the result
   * Most the time they will pass a NULL.
   */
  if (cdef)
      cols= cdef;
  else
      cols= &actdef;

  /*
   * if the user is interested in the status of the routine.
   * if he passed a NULL for ret_stat I just put the stat into a dummy
   * variable.  It is not necessary for the caller to know the stat since
   * this routine will always return a usable pixel value.
   */
  if (ret_stat)
       stat= ret_stat;
  else
       stat= &stat_holder;
  
  
  /*
   * The last ditch default will either be black or white depending on
   * whether defblack is set to true or false.
   */
  if (sc->defblack) {
          defstr= BLACKSTR;
          defpix= BlackPixelOfScreen(scr);
  ENDif
  else {
          defstr= WHITESTR;
          defpix= WhitePixelOfScreen(scr);
  ENDelse
  



  /*
   * if I only have a black and white screen
   */
  if (cflags & MUST_BW) {
      retpix= defpix;
      *stat= SCELL_BW;
  } /* end if */

  else if (cflags & MAYONLY_GSLINE) {
       if (strlen (sc->gsname) == 0) {
               retpix= defpix;
               *stat= SCELL_BW;
       } /* end if */
       else if (XAllocNamedColor(dpy, cmap, sc->gsname, cols, &rgbdef)){
               retpix= cols->pixel;
               *stat= SCELL_GCOLOR;
       } /* end else if */
       else {
               retpix=  defpix;
               *stat= SCELL_BW;
               if (printerr)
                   fprintf(stderr,
                           "Warning: Cannot allocate color %s - using %s\n",
                           sc->gsname, defstr );
       } /* end  else */
  } /* end  if */

  else if (cflags & MAY_CGS_LINE) {
       if (strlen (sc->cname) == 0) {
               retpix= defpix;
               *stat= SCELL_BW;
       } /* end  if */
       else if (XAllocNamedColor(dpy, cmap, sc->cname, cols, &rgbdef)){
               retpix= cols->pixel;
               *stat= SCELL_COLOR;
       } /* end else if */
       else {
               retpix=  defpix;
               *stat= SCELL_BW;
               if (printerr)
                   fprintf(stderr, 
                           "Warning: Cannot allocate color %s - using %s\n",
                           sc->cname, defstr );
       } /* end else */


  ENDelse



  sc->pixel=  retpix;
  return ( retpix);





}
