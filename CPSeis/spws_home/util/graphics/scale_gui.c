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
#include "ez_win.h"

Widget ScaleGUI(Gwindow *gwin, Widget parent, struct HELPCTX *Hctx)
{ErsCoordInf *coords;
 struct EZwin *ezwin;
 char  title[16];
 Arg   arglist[15];
 long  i,n,wkid;
 long  ierr,ntran;
 float wndw[4],vprt[4];
 void  *udata;
 extern  Widget get_scale_parm_();

 Widget  shell,form;
 Widget  PBOK,PBCAN;
 Widget  rcvp,rcww;
 Widget  textw[16],labw[16];

 if(gwin == NULL) return;
 wkid = gwin->wkstn;
 if(wkid <=0) { return NULL; }
 gsactive(&wkid);
 gqcntn(&ierr,&ntran);
 gqnt(&ntran,&ierr,wndw,vprt);
/*************************************************
 * Create GUI showing viewport & world coord    */
 form = get_scale_parm_(parent,0, Hctx, &udata, gwin);
/*************************************************
 * Set viewport & window. Clobbered by resources*/
 gsvp(&ntran,&vprt[0],&vprt[1],&vprt[2],&vprt[3]);
 gswn(&ntran,&wndw[0],&wndw[1],&wndw[3],&wndw[2]);
/*************************************************
 * Force a screen refresh to update the display */

 
 return form;
}

