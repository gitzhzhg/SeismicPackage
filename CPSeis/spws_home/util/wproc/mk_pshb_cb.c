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
#include <Xm/PushB.h>
#include <Xm/Separator.h>
#include <Xm/ArrowB.h>
#include <stdarg.h>
#include <string.h>
#include <stdlib.h>
#include "cenv.h"
#include "wproc.h"



Widget mk_pshb_cb( wunion             wu[],
                   struct  CB         cb[],
                   Widget             parent,
                   XtCallbackProc     act_cb,
                   struct CB          *example_cb,
                   long               cnt,      /*this parameter must be here*/
                   ... )
/*
 * parameters: wu         - widget array which will have elements set
 *             cb         - CB array which will have elements set
 *             parent     - parent widget
 *             act_cb     - activate callback
 *             example_cb - struct CB that will be used to set the specific
 *                          elements in the cb[] array.  The following elements
 *                          are set from example_cb: info, popbox, 
 *                                                   more_func, and fldptr
 *                          if NULL no these elements are not set in the cb[]
 *                          array.
 *             cnt        - number of widgets to create.
 *             ...        - the following parameter are repeated in pairs.
 *                          the number of pairs that should be past is
 *                          specified by the cnt parameter.
 *        int  ele        - element of the wu & cb array that is set, it
 *                          is also set as the wconst of the CB[] array and
 *                          the user data of the widget.
 *       char  *wname     - the widget name of the push button widget.
 *                          If the widget name is of the form
 *                          "name::SEPARATOR" we will create a separator 
 *                          widget instead of a push button.
 */

{
      Arg           arglist[4];
      int           n, i;
      va_list       args;
      int           ele;
      char          *wname, *sepstrloc;
      char          sepname[100];
      struct CB     tmpcb;
      Widget        w;
      long          wclass;

#define ISSEP_FLAG_STR "::SEPARATOR"


  va_start( args, cnt);

  if (example_cb)
      memcpy( &tmpcb, example_cb, sizeof (struct CB) );

  for(i=0; (i<cnt); i++) {

      ele=   va_arg(args, int);
      wname= va_arg(args, char* );

      sepstrloc= strstr(wname, ISSEP_FLAG_STR  );
      if (sepstrloc) {
             strncpy( sepname, wname, sepstrloc-wname );
             sepname[sepstrloc-wname+1]= '\0';
             /*
              *  Create Separator Widget
              */
             n= 0;
             XtSetArg (arglist[n], XmNuserData, (XtArgVal)ele); n++;
             w= XtCreateManagedWidget( sepname, 
                                                   xmSeparatorWidgetClass,
                                                   parent, arglist, n);
             wclass= SEPARATOR;
      ENDif
      else  {
            if (example_cb) {
                set_CB(cb, ele, tmpcb.info, tmpcb.popbox,
                       tmpcb.fldptr, TYPE_NONE );
                cb[ele].more_func= tmpcb.more_func;
            ENDif
            else {
                set_CB(cb, ele, NULL, NULL, NULL, TYPE_NONE );
                cb[ele].more_func= NULL;
            ENDelse
 
            /*
             *  Create Push Button Widget
             */
 
            n= 0;
            XtSetArg (arglist[n], XmNuserData, (XtArgVal)ele); n++;
            w= XtCreateManagedWidget( wname, xmPushButtonWidgetClass,
                                      parent, arglist, n);
            /*
             * Add Callback
             */
            if ( act_cb != NULL ) {
               XtAddCallback( w, XmNactivateCallback, act_cb, &cb[ele]);
            ENDif
      
             wclass= PUSH_BUTTON;
     ENDelse
     if (ele != WPROC_NONE) {
              wu[ele].any.wclass= wclass;
              wu[ele].any.w= w;
     ENDif
  ENDloop


  va_end(args);

  return NULL;
}
