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
#include <stdio.h>
#include "cenv.h"
#include "wproc.h"
#include <X11/Xlib.h>
#include <X11/Xutil.h>
#include <X11/Intrinsic.h>
#include <Xm/Xm.h>
#include <Xm/RowColumn.h>
#include <Xm/CascadeB.h>




Widget create_cas_pdown( struct Cas_PullW *cs,
                         boolean          ishelp) 


{
   Arg      arglist[4];
   long     n;



   cs->pdown= XmCreatePulldownMenu( cs->parent, cs->pdown_name, NULL, 0);


   n=0;
   XtSetArg (arglist[n], XmNsubMenuId, cs->pdown ); n++;
   cs->cascade= XtCreateManagedWidget( cs->cas_name, xmCascadeButtonWidgetClass,
                                 cs->parent, arglist, n);


   if (ishelp) {
         n=0;
         XtSetArg (arglist[n], XmNmenuHelpWidget, cs->cascade ); n++;
         XtSetValues( cs->parent, arglist, n);
   ENDif


   return (cs->pdown);
}
