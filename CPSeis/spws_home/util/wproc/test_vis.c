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
 * Name        : test_vis
 * File        : test_vis.c
 * Executable  : -
 * Author      : Trey Roby
 * Date        : 2/1/91
 *
 *
 */

/* -------------------------------------------------------------------------*/

#include "cenv.h"
#include "wproc.h"
#include <stdio.h>
#include <math.h>
#include <X11/Xlib.h>
#include <X11/Xutil.h>
#include <X11/Intrinsic.h>
#include <Xm/Xm.h>


long test_vis( Display       *dpy,
               long          *visclass,
               long          *tot_col )


{
 Visual *vis;
 Screen *scr;
 long depth;
 long cflags= 0;


  scr=   DefaultScreenOfDisplay(dpy);
  vis=   DefaultVisualOfScreen( scr);
  depth= DefaultDepthOfScreen( scr);


  if (tot_col)
         *tot_col= (depth<32) ? pow((double)2,(double)depth) :  
                                pow((double)2,(double)31);


    /* !!!! WARNING the Visual structure not should be accessed
     *      but I'm doing it anyway  (but only in one place) !!!!!
     */
  *visclass= vis->class;

   
  switch (*visclass) {
      case StaticGray:
                   if (depth==1)
                        cflags= MUST_BW|MUST_SHR;
                   else if ( (depth>1)AND(depth<=4) )
                        cflags= MAYONLY_GSLINE|MUST_SHR;
                   else
                        cflags= MAYONLY_GSLINE|MAY_GS|MUST_SHR;
                   break;

      case GrayScale:
                   if (depth<=4)
                        cflags= MAYONLY_GSLINE;
                   else
                        cflags= MAYONLY_GSLINE|MAY_GS;
                   break;

      case PseudoColor:
      case DirectColor:
                   if (depth<=4)
                        cflags= MAY_CGS_LINE;
                   else
                        cflags= MAY_CGS_LINE|MAY_GS|MAY_COL;
                   break;
      case StaticColor:
      case TrueColor:
                   if (depth<=4)
                        cflags= MAY_CGS_LINE|MUST_SHR;
                   else
                        cflags= MAY_CGS_LINE|MAY_GS|MAY_COL|MUST_SHR;
                   break;
  ENDswitch


  return ( cflags );



}

