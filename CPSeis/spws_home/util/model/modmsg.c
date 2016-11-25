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
#include <string.h>
#include <X11/Xlib.h>
#include <X11/IntrinsicP.h>
#include <X11/Shell.h>
#include <Xm/MessageB.h>
#include <Xm/XmP.h>

#ifdef __hpux
#include <spws_hpux.h>
#endif

/* procedures defined in this file */
void   MODMsgBox(Widget , char *);
void   MODMsgDestroy(Widget W,caddr_t , caddr_t );

static Widget warning_dialog;
static Widget warning_box;

/************************************************************************
 *
 *  - MODMsgBox() -
 *    Display warning messages.
 *
 ***********************************************************************/
void MODMsgBox(Widget widget, char *msg)
{
  XmString xmsg;
  Widget cancel, help;
  char   title[64];
  Arg arg[4];
  int n;

  if (warning_dialog == NULL)
   { /* build warning panel */
    n = 0;
    XtSetArg(arg[n], XtNallowShellResize, True); n++;
    sprintf(title,"Model Message");
    XtSetArg (arg[n], XmNtitle, title); n++;
    warning_dialog = (Widget)
            XmCreateDialogShell(XutilGetShell(widget), "MODMsg", arg, n);

    n = 0;
    XtSetArg(arg[n], XmNdialogType, XmDIALOG_ERROR); n++;

    warning_box = XtCreateWidget("MODMsgBox",
         xmMessageBoxWidgetClass, warning_dialog, arg, n);

    cancel = XmMessageBoxGetChild(warning_box,
                              XmDIALOG_CANCEL_BUTTON);
    XtUnmanageChild(cancel);
    help = XmMessageBoxGetChild(warning_box,
                                XmDIALOG_HELP_BUTTON);
    XtUnmanageChild(help);
    XtAddCallback(warning_dialog,XmNdestroyCallback,MODMsgDestroy,NULL);
  }


  xmsg = XmStringCreateLtoR(msg,XmSTRING_DEFAULT_CHARSET);
  n = 0;
  XtSetArg(arg[n], XmNmessageString, xmsg); n++;
  XtSetValues(warning_box, arg, n);
  XBell(XtDisplay(widget), 0);
  XMapWindow(XtDisplay(warning_dialog),XtWindow(warning_dialog));
  XtManageChild(warning_box);
  XmStringFree(xmsg);
}

void MODMsgDestroy(Widget W,caddr_t a, caddr_t b)
{
 warning_dialog=NULL;
 warning_box=NULL;
}
