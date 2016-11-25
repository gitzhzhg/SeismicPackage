#ifndef _Scale_
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
#define _Scale_

#include <X11/StringDefs.h>
#include <X11/Intrinsic.h>
#include <Xm/Xm.h>
#include <stdlib.h>
#include "wproc.h"
#include "ez_win.h"
#include "gplot.h"

/* User data structure for windows */
struct UD_scale
{
 float XN1;
 float XU1;
 char  XNAME[16];
 float XN2;
 float XU2;
 float YN1;
 float YU1;
 char  YNAME[16];
 float YN2;
 float YU2;
 long  NTRAN;
 Gwindow *gwin;
 Widget shell;       /* optional dialog shell               */
 Widget form;        /* form widget for the glimits display */
 struct EZwin *Cwin;
 struct HELPCTX *Hctx;};
 
/*****************
 *  GUI building declarations */
Widget get_scale_parm_(Widget parent,int opt, struct HELPCTX *Hctx,
       void **data, Gwindow *gwin);
Widget *scale(int opt, Widget P ,char help_file[],
       char help_title[], struct UD_scale *udat);
Widget scale1(int opt, Widget parent,struct UD_scale *udat, long *sid);

void scale_update(Gwindow *gwin);

/*****************
 *  Declaration of ezed trap names */
Boolean scale_uc(struct UD_scale *);
Boolean scale_nc(struct UD_scale *);
void scale_ntran_(struct UD_scale *);

void scale_trap(Widget W, struct UD_scale *udat, caddr_t b);
void scale_init(struct UD_scale *udat);

#endif
