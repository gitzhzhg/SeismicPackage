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




Widget cre_opm( wunion             *wu,
                Widget             parent,
                char               *menu_name,
                char               *rc_name,
                char               *label_str,
                long               wconst)




{
   Arg      arglist[3];
   long     n;
   XmString str;


   wu->opm.rc= XmCreatePulldownMenu( parent, rc_name, NULL, 0);
   wu->opm.wconst= wconst;
   wu->opm.wclass= OPTION_MENU;
   str= XmStringCreateSimple( label_str );

   n=0;
   XtSetArg (arglist[n], XmNsubMenuId, wu->opm.rc ); n++;
   XtSetArg (arglist[n], XmNlabelString, str ); n++;
   wu->opm.menuw= XmCreateOptionMenu( parent, menu_name, arglist, n);
   XtManageChild( wu->opm.menuw);


   XmStringFree(str);

   return ( wu->opm.menuw );
}
