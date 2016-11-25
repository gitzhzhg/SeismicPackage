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
#include <X11/Intrinsic.h>
#include <Xm/Xm.h>
#include <Xm/ToggleB.h>


#define Get_toggle_value(w)     ( XtSetArg(arglist[0],XmNset, &isset),  \
                                 XtGetValues ((w), arglist, 1),isset )



Widget create_toggle( struct ToggleW   *ts)

{
      Arg           arglist[4];
      long           n;
      Boolean       isset;


/*
 *  Create Toggle Widget
 */
 
       n= 0;
       XtSetArg (arglist[n], XmNuserData, ts->wconst); n++;
       ts->toggle= XtCreateManagedWidget( ts->toggle_name, 
                                        xmToggleButtonWidgetClass,
                                        ts->parent, 
                                        arglist, n);
/*
 * Add Callback
 */
       if ( ts->valchange_cb != NULL ) {
          XtAddCallback( ts->toggle, XmNvalueChangedCallback, 
                         ts->valchange_cb, ts->valchange_info);
       ENDif


       ts->initval= Get_toggle_value(ts->toggle);

       if (ts->target)  {
              if ( (ts->type == TYPE_RADIO) && (ts->initval) )
                      *(long *)ts->target= ts->wconst;
              if ( (ts->type == TYPE_TOGGLE) )
                      *(long *)ts->target= ts->initval;
       ENDif
       return (ts->toggle);
 }
