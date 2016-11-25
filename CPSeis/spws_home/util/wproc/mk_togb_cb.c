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
#include <Xm/ToggleB.h>
#include <stdarg.h>
#include "cenv.h"
#include "wproc.h"



#define Get_toggle_value(w)     ( XtSetArg(arglist[0],XmNset, &isset),  \
                                 XtGetValues ((w), arglist, 1),isset )



Widget mk_togb_cb( wunion             wu[],
                   struct  CB         cb[],
                   Widget             parent,
                   XtCallbackProc     valc_cb,
                   struct CB          *example_cb,
                   long               t_type,
                   long               cnt,      /*this parameter must be here*/
                   ... )
/*
 * parameters: wu         - widget array which will have elements set
 *             cb         - CB array which will have elements set
 *             parent     - parent widget
 *             act_cb     - activate callback
 *             example_cb - struct CB that will be used to set the specific
 *                          elements in the cb[] array. The following elements
 *                          are set from example_cb: info, popbox, 
 *                                                   more_func and
 *                                                   fldptr in the 
 *                                                   TYPE_RADIO case
 *                          if NULL no these elements are not set in the cb[]
 *                          array.
 *             t_type     - toggle type (TYPE_TOGGLE or TYPE_RADIO)
 *             cnt        - number of widgets to create.
 *             ...        - the following parameter are repeated in pairs.
 *                          the number of pairs that should be past is
 *                          specified by the cnt parameter.
 *       int   ele        - element of the wu & cb array that is set, it
 *                          is also set as the wconst of the CB[] array and
 *                          the user data of the widget.
 *       char  *wname     - the widget name of the push button widget 
 *            target      - target int where value of toggle is stored.
 */

{
      Arg           arglist[4];
      int           n, i;
      va_list       args;
      int           ele;
      char          *wname;
      Boolean       isset;
      struct CB     tmpcb;
      long          *target;



  va_start( args, cnt);

  if (t_type == TYPE_RADIO) {
          n=0;
          XtSetArg (arglist[n], XmNradioBehavior, True) ; n++;
          XtSetArg (arglist[n], XmNpacking, XmPACK_COLUMN) ; n++;
          XtSetValues( parent, arglist, n);
  ENDif

  if (example_cb)
      memcpy( &tmpcb, example_cb, sizeof (struct CB) );


  for(i=0; (i<cnt); i++) {

      ele=   va_arg(args, int);
      wname= va_arg(args, char* );
      target= va_arg(args, long* );
      
      if (example_cb) {

          if (t_type == TYPE_RADIO) {
                   set_CB(cb, ele, tmpcb.info, tmpcb.popbox, NULL, t_type);
                   cb[ele].more_func= tmpcb.more_func;
                   cb[ele].fldptr= tmpcb.fldptr;  
                   target= tmpcb.fldptr;  
          ENDif
          else if (t_type == TYPE_TOGGLE) {
                   set_CB(cb, ele, tmpcb.info, tmpcb.popbox, target, t_type);
                   cb[ele].more_func= tmpcb.more_func;
          ENDif

      ENDif
      else {
          set_CB(cb, ele, NULL, NULL, NULL, t_type );
          cb[ele].more_func= NULL;
      ENDelse
 
      /*
       *  Create Toggle Button Widget
       */
 
      n= 0;
      XtSetArg (arglist[n], XmNuserData, (XtArgVal)ele); n++;
      wu[ele].any.w= XtCreateManagedWidget( wname, 
                                            xmToggleButtonWidgetClass,
                                            parent, arglist, n);


      /*
       * Add Callback
       */
      if ( valc_cb != NULL ) {
         XtAddCallback( wu[ele].any.w, XmNvalueChangedCallback, 
                        valc_cb, &cb[ele]);
      ENDif


      wu[ele].togw.initval= Get_toggle_value( wu[ele].any.w);
      wu[ele].togw.type= t_type;

       /*
        * target exist
        */
       if (target)  {
              /*
               * if this is a radio with it value set to true
               * initialze the target field it indicate that button
               * is set.
               */
              if ( (t_type == TYPE_RADIO) AND (wu[ele].togw.initval == True) )
                      *target= (long)ele;
              /*
               * if this is a radio with it value set to true
               * initialze the target field it indicate that button
               * is set.
               */
              if ( (t_type == TYPE_TOGGLE) )
                      *target=  wu[ele].togw.initval;
       ENDif



       wu[ele].any.wclass= TOGGLE_BUTTON;

  ENDloop



  va_end(args);

  return NULL;
}
