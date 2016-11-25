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
/*
 * Name        : show_msg
 * File        : show_msg.c
 * Executable  : -
 * Author      : Trey Roby
 * Date        : 11/1/91
 *
 * This functions sets the message string for a message box widget and
 * manages the message box.
 * This function is very useful since message boxes are begin displayed 
 * all the time.
 *
 * PARAMETERS:
 *             Widget     w   - the widget id of the message box.
 *             char     str[] - string to be display in the message box.
 *
 *             RETURNS  none
 *
 * 
 * CHANGES:
 */

/* -------------------------------------------------------------------------*/

#include <stdio.h>
#include <X11/Intrinsic.h>
#include <X11/IntrinsicP.h>
#include <Xm/Xm.h>
#include <Xm/MessageB.h>
#include <Xm/Text.h>
#include <Xm/TextF.h>
#include <Xm/Scale.h>
#include <Xm/Label.h>
#include <Xm/LabelG.h>
#include <Xm/PushB.h>
#include <Xm/ToggleB.h>
#include <Xm/ToggleBG.h>
#include <Xm/CascadeB.h>
#include <Xm/CascadeBG.h>
#include <Xm/PushBG.h>
#include <stdarg.h>
#include "wproc.h"



/*
 * ------------------------------ FUNCTION ----------------------------------
 */

Widget wprocGetShell(Widget w)
{
  if (XtIsShell(w)) return(w);

  while (!XtIsShell(w)) w= XtParent(w);

  return(w);
}






/*
 * ------------------------------ FUNCTION ----------------------------------
 */
void wprocShowMsg( Widget w,
                   char   str[] )

{
  long n;
  XmString Xmstr;
  Arg arglist[3];
  Display *dpy;
  Window shell_win;

  if (w) {
     dpy= XtDisplay(w);
     if ( XtClass(w) == xmMessageBoxWidgetClass) {
        Widget shell_p;
        n=0;
        /*
         * a complex line that sets the resource to the passed string
         */
        XtSetArg (arglist[n], XmNmessageString, Xmstr= XmStringCreateLtoR(str, 
                  XmSTRING_DEFAULT_CHARSET ) ); n++;
        XtSetValues (w, arglist, n);
      
        shell_p=  XtParent( wprocGetShell(w) );
        shell_win= XtWindow(XtParent(w));
        /*
        The following would raise an error popup with no message and
        no remove button because it would not have a managed window
        to work with. Replaced this code with the following on 03/2000 MLS
        if (shell_p) {
            if ( XtIsManaged( get_shell_child( shell_p) ) && 
                 ( XtWindow(shell_p) ) ) 
                    XtManageChild(w);      
                    if (shell_win)  {
                        XMapWindow( dpy, shell_win );
                        XRaiseWindow(dpy, shell_win);
                    }
        } 
        */

        /*New if block 03/2000 MLS */
        if (shell_p) {
            if ( XtIsManaged( get_shell_child( shell_p) ) && 
                 ( XtWindow(shell_p) ) ) 
              {
                    XtManageChild(w);      
                    if (shell_win)  {
                        XMapWindow( dpy, shell_win );
                        XRaiseWindow(dpy, shell_win);
                    }
              }
        }
        else {
             XtManageChild(w);            /*  manage the message box */
             if ( shell_win ) XMapWindow( dpy, shell_win );
        }

        XmStringFree( Xmstr);        /*  free memory */
     } /* END if */
   
     else if ( XtClass(w) == xmTextFieldWidgetClass) {
             XmTextFieldSetString( w, str);
     }
   
   
     else if ( XtClass(w) == xmTextWidgetClass) {
             XmTextSetString( w, str);
     }
   
     else if ( XtClass(w) == xmScaleWidgetClass) {
        n=0;
        XtSetArg (arglist[n], XmNtitleString, Xmstr= XmStringCreateLtoR(str, 
                  XmSTRING_DEFAULT_CHARSET ) ); n++;
        XtSetValues (w, arglist, n);
        XmStringFree( Xmstr);        /*  free memory */
             
     }

     else if ( ( XtClass(w) == xmLabelWidgetClass) ||
               ( XtClass(w) == xmLabelGadgetClass) ) {
             set_label( w, str);
     }
     else if ( ( XtClass(w) == xmPushButtonWidgetClass)   ||
               ( XtClass(w) == xmPushButtonGadgetClass)   ||
               ( XtClass(w) == xmToggleButtonWidgetClass) ||
               ( XtClass(w) == xmToggleButtonGadgetClass) ||
               ( XtClass(w) == xmCascadeButtonWidgetClass) ||
               ( XtClass(w) == xmCascadeButtonGadgetClass) ) {
             set_label( w, str);
     }
     else  {
             fprintf( stderr, "Bad Widget class passed to show_msg.\n" );
     }
 } /* END if */
 else 
     fprintf( stderr, "NULL Widget passed to show_msg.\n" );
   
}


/*
 * ------------------------------ FUNCTION ----------------------------------
 */
char *wprocGetMsg( Widget w )
{
 char *retstr= NULL;

 if (w) {
     if ( XtClass(w) == xmTextFieldWidgetClass) {
             retstr= XmTextFieldGetString( w);
     }

     else if ( XtClass(w) == xmTextWidgetClass) {
             retstr= XmTextGetString( w);
     }

     else if ( XtClass(w) == xmLabelWidgetClass) {
             retstr= get_simp_labelstrptr (w);
     }

     else {
             fprintf( stderr, "Bad Widget class passed to wprocGetMsg.\n" );
     }

 } /* End if (w) */

 return ( retstr);
}


/*
 * ------------------------------ FUNCTION ----------------------------------
 */
void show_msg( Widget w,
               char   str[] )

{
   wprocShowMsg(w, str); 
}



/*
 * ------------------------------ FUNCTION ----------------------------------
 */
void wprocVAShowMsg( Widget w, char *format, ...)
{
#define BUFLEN 2000
 char str[BUFLEN];
 va_list   args;
 int cnt;

 va_start(args, format);
 cnt= vsprintf(str, format, args);
 if (cnt >= BUFLEN) printf("Warning: wprocVAShowMsg: Buffer overflow\n");
 wprocShowMsg(w,str);
 va_end(args);
}
