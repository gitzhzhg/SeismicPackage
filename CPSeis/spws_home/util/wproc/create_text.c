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
#include <Xm/Text.h>
#include <Xm/Label.h>
#include <assert.h>

extern chk_reset();

Widget create_text (struct TextW *ts)
{
  Arg arglist[15];
  int n;
/*
 *  Create Label Widget
 */
  if (strlen(ts->label_name) > 0) {
    n= 0;
    ts->label = XtVaCreateManagedWidget (ts->label_name, xmLabelWidgetClass,
      ts->parent, NULL);
  }
/*
 *  Create Text Widget
 */
  n = 0;
  XtSetArg (arglist[n], XmNuserData, ts->wconst); n++;
  ts->text = XtCreateManagedWidget (ts->text_name, xmTextWidgetClass,
    ts->parent, arglist, n);
/*
 * Add Callbacks
 */
  if (ts->exit_cb != NULL) {
    XtAddCallback (ts->text, XmNlosingFocusCallback, ts->exit_cb,
      ts->exit_info);
    XtAddCallback (ts->text, XmNactivateCallback, ts->exit_cb, ts->exit_info);
  }
  if (ts->entry_cb != NULL) {
    XtAddCallback (ts->text, XmNfocusCallback, ts->entry_cb, ts->entry_info);
  }
/*
 * tempory - and experimental
 */
/*
 *   XtAddCallback (ts->text, XmNmodifyVerifyCallback, chk_reset,
 *     ts->entry_info);
*/

/*
 * Get Initial Value
 */
  ts->initval = XmTextGetString (ts->text);

/*
 *   set variable to initial value
 */
  if (ts->target) {
    switch (ts->type) {
    case TYPE_UCHAR : 
      if (!check_uchar(ts->initval,ts->target)) {
	*(unsigned char *)ts->target = 0;
      }
      break;
    case TYPE_SINT : 
      if (!check_sint(ts->initval,ts->target)) {
	*(short *)ts->target = 0;
      }
      break;
    case TYPE_INT : 
      if (!check_int(ts->initval,ts->target)) {
	*(int *)ts->target = 0;
      }
      break;
    case TYPE_LONG :
      if (!check_long(ts->initval,ts->target)) {
	*(long *)ts->target = 0;
      }
      break;
    case TYPE_FLT  : 
    case TYPE_GFLT : 
      if (!check_flt(ts->initval,ts->target)) {
	*(float *)ts->target = 0;
      }
      break;
    case TYPE_DBL  : 
    case TYPE_GDBL : 
      if (!check_dbl(ts->initval,ts->target)) {
	*(double *)ts->target = 0;
      }
      break;
    case TYPE_FILE :
      if (strcmp(ts->initval,"NONE") == 0) {
	strcpy ((char *)ts->target, "");
      }
      else {
	strcpy ((char *)ts->target, ts->initval);
      }
      break;
    case TYPE_STR :
      strcpy ((char *)ts->target, ts->initval );
      break;
    default:
      assert (0);
      break;
    }
  }
  return ts->text;
}
