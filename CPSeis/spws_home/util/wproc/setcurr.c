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
#include "wproc.h"
#include <X11/Xlib.h>
#include <X11/Xutil.h>
#include <X11/Intrinsic.h>
#include <Xm/Xm.h>
#include <Xm/Text.h>


void setcurr(Widget w, 
             struct CURRFLD_INFO *curr,
             XmAnyCallbackStruct *cbinfo )
{
 int n;
 Arg arglist[3];
 long wconst;
 long len;
 char *str;


 /*
  * get the widget constant out of user data of the widget  
  */
 n=0;
 XtSetArg (arglist[n], XmNuserData, &wconst ); n++;
 XtGetValues( w, arglist, n);

 curr->curr_textw= w;           /* info is pointer to a where the current
                              *  widget is stored 
                              */
 curr->curr_text= wconst;    /* fldptr is pointing to where the current 
                              * widget constant is kept
                              */

 /*
  * this next code sets the widget to be selected when it is entered
  */

 n=0;
 str= XmTextGetString(w);
 len=strlen(str);
 XtFree(str);
 XmTextSetSelection( w, 0, len, CurrentTime);
}

