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
#include <Xm/Xm.h>
#include <Xm/ScrollBar.h>
#include "cenv.h"
#include "wproc.h"



Widget create_sb( struct sbW   *sb)

{
      Arg           arglist[4];
      int           n;


/*
 *  Create Scroll Bar Widget
 */
 
  n= 0;
  XtSetArg (arglist[n], XmNuserData, sb->wconst); n++;
  sb->scrollb= XtCreateManagedWidget( sb->sb_name, 
                                      xmScrollBarWidgetClass,
                                      sb->parent, 
                                      arglist, n);
/*
 * Add Callback
 */
  if ( sb->cbs != NULL ) {
/*
    XtAddCallback( sb->scrollb, XmNdecrementCallback, sb->cbs, sb->cbs_info);
    XtAddCallback( sb->scrollb, XmNincrementCallback, sb->cbs, sb->cbs_info);
    XtAddCallback( sb->scrollb, XmNpageDecrementCallback, sb->cbs,sb->cbs_info);
    XtAddCallback( sb->scrollb, XmNpageIncrementCallback, sb->cbs,sb->cbs_info);
    XtAddCallback( sb->scrollb, XmNtoBottomCallback,  sb->cbs, sb->cbs_info);
    XtAddCallback( sb->scrollb, XmNtoTopCallback,     sb->cbs, sb->cbs_info);
*/
    XtAddCallback( sb->scrollb, XmNdragCallback,      sb->cbs, sb->cbs_info);
    XtAddCallback( sb->scrollb, XmNvalueChangedCallback, sb->cbs, sb->cbs_info);
  ENDif

  return(sb->scrollb);
}
