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
#include <stdarg.h>
#include <X11/Xlib.h>
#include <X11/Xutil.h>
#include <X11/Intrinsic.h>
#include <Xm/Xm.h>
#include <Xm/RowColumn.h>
#include <Xm/CascadeB.h>




Widget mk_cas_pd( wunion             wu[],
                  Widget             parent,
                  long               cnt,      /*this parameter must be here*/
                  ... )

/*
 * parameters: wu         - widget array which will have elements set
 *             parent     - parent widget
 *             cnt        - number of widgets to create.
 *             ...        - the following parameter are repeated in pairs.
 *                          the number of pairs that should be past is
 *                          specified by the cnt parameter.
 *        int  ele        - element of the wu array.
 *       char  *cas_name  - the widget name of cascade.
 *       char  *pd_name   - the widget name of row column
 */


{
      Arg           arglist[4];
      int           n, i;
      va_list       args;
      int           ele;
      char          *pd_name, *cas_name;



  va_start( args, cnt);



  for(i=0; (i<cnt); i++) {

      ele     =  va_arg(args, int);
      cas_name= va_arg(args, char* );
      pd_name = va_arg(args, char* );


      wu[ele].cpw.pdown= XmCreatePulldownMenu( parent, pd_name, NULL, 0);

      n=0;
      XtSetArg (arglist[n], XmNsubMenuId, wu[ele].cpw.pdown ); n++;
      wu[ele].cpw.cascade= XtCreateManagedWidget( cas_name, xmCascadeButtonWidgetClass,
                                                  parent, arglist, n);


      wu[ele].any.wclass= CAS_PULL;
  ENDloop



  va_end(args);

  return NULL;
}
