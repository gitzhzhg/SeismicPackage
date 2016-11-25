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
#include <stdlib.h>
#include <X11/StringDefs.h>
#include <X11/Intrinsic.h>
#include <Xm/Xm.h>
#include <Xm/DialogS.h>
#include <Xm/Form.h>
#include <Xm/PushB.h>
#include <Xm/Separator.h>
#include "wproc.h"
#include "file_choice.h"
#include "sl_cvm_app.hh"
#include "opencvm.h"


#ifdef __cplusplus
extern "C" {                 // for C++
#endif

void cvm_filei(Widget W, void *cvmapp,caddr_t b);

#ifdef __cplusplus
}                   // for C++
#endif

/* PROTOTYPES FOR PRIVATE METHODS */
static void ControlCB (Widget , OpenData *, caddr_t );
static void KillCB(Widget W, OpenData *, caddr_t b);

Widget OpenGUI(int option, Widget parent, struct HELPCTX *Hctx,
       void *cvmapp)
{/* set option = 0 for return widget to be a form in a dialog */
 /* set option != 0 for return widget to be a form */
 OpenData *opdata;
 char  title[24];
 Arg   arglist[25];
 int   i,n,flags;


 Widget  shell,form,close;
 Widget  fcw,sep;
 
/***************************************************
 * Do some sanity checking                        */
 if(parent  == NULL) return NULL;

/*************************************************
 * Create a popup shell                         */
 i=0;
 opdata = (OpenData *) calloc(1,sizeof(OpenData));
 opdata->shell = NULL;
 if(option == 0)
  {
   sprintf(title,"Open Old CVM File");
   XtSetArg (arglist[i], XmNtitle, title); i++;
   shell = XtCreatePopupShell("opshell",xmDialogShellWidgetClass,
           parent,arglist,i);
   opdata->shell = shell;
  }
/*************************************************
 * Create a Form Widget */
 i = 0;
 form = NULL;
 if(option == 0)
  {XtSetArg (arglist[i], XmNautoUnmanage,False); i++;
   form = XmCreateForm(shell, "form", arglist, i);
  }
 else
  form = XmCreateForm(parent, "form", arglist, i);
 opdata->form = form;
 XtAddCallback(form,XmNdestroyCallback,(XtCallbackProc) KillCB,opdata);
/*************************************************
 * Add CLOSE pushbutton under form              */
 n= 0;
 XtSetArg (arglist[n], XmNleftAttachment, XmATTACH_POSITION); n++;
 XtSetArg (arglist[n], XmNleftPosition, 45); n++;
 XtSetArg (arglist[n], XmNrightAttachment, XmATTACH_NONE); n++;
 XtSetArg (arglist[n], XmNbottomAttachment, XmATTACH_FORM); n++;
 XtSetArg (arglist[n], XmNbottomOffset, 5) ; n++;
 close= XtCreateWidget( "CLOSE", xmPushButtonWidgetClass,
                                 form, arglist,n);
 XtAddCallback(close,XmNactivateCallback,(XtCallbackProc) ControlCB,opdata);
 opdata->close = close;
 if(Hctx != NULL) add_HELP(close, helper, Hctx);
/*************************************************
 * Create a Separator                           */
 n= 0;
 XtSetArg (arglist[n], XmNleftAttachment, XmATTACH_FORM) ; n++;
 XtSetArg (arglist[n], XmNrightAttachment, XmATTACH_FORM) ; n++;
 XtSetArg (arglist[n], XmNbottomAttachment, XmATTACH_WIDGET) ; n++;
 XtSetArg (arglist[n], XmNbottomWidget, close) ; n++;
 sep = XtCreateWidget( "sep", xmSeparatorWidgetClass,form, arglist,n);
/*************************************************
 * Add a FileChoice widget under the form.      **
 * cvm_filei reads file and plots it.           */
 n = 0;
 flags = ~0777777777777;
 flags |= (wprocSqueezeBlanksMask|wprocMustExistMask);
/*
 flags = ~(wprocMustExistMask|wprocExpandFileMask|
           wprocMsgBlkMask| wprocOverWriteWarnMask|wprocWritableMask);
           */
 XtSetArg (arglist[n], XmNfileExtension, "pck"); n++;
 XtSetArg (arglist[n], XmNfileFlags, flags ); n++;
 XtSetArg (arglist[n], XmNlabelString, "CVM Input...") ; n++;
 XtSetArg (arglist[n], XmNtopAttachment, XmATTACH_NONE); n++;
 XtSetArg (arglist[n], XmNy, 45); n++;
 XtSetArg (arglist[n], XmNleftAttachment, XmATTACH_FORM); n++;
 XtSetArg (arglist[n], XmNleftOffset, 20); n++;
 XtSetArg (arglist[n], XmNrightAttachment, XmATTACH_FORM); n++;
 XtSetArg (arglist[n], XmNrightOffset, 20); n++;
 XtSetArg (arglist[n], XmNbottomAttachment, XmATTACH_WIDGET) ; n++;
 XtSetArg (arglist[n], XmNbottomWidget, sep) ; n++;
 XtSetArg (arglist[n], XmNbottomOffset, 10) ; n++;
 fcw = XtCreateWidget( "FCWIN1",fileChoiceWidgetClass,
       form, arglist,n);
 XtAddCallback(fcw,XmNsuccessCallback,(XtCallbackProc) cvm_filei,cvmapp);
 if(Hctx != NULL) add_HELP(fcw, helper, Hctx);
 opdata->fcw = fcw;

 XtManageChild(close);
 XtManageChild(sep);
 XtManageChild(fcw);
 XtManageChild(form);

 return form;
}

static void ControlCB(Widget W, OpenData *opdat, caddr_t )
{ /* destroy the open window */

 if(opdat == NULL) return;
 if(W == opdat->close )
   XtUnmanageChild(opdat->form);
/*
 XtDestroyWidget(opdat->shell);
 */
 return;
}

static void KillCB(Widget , OpenData *dat, caddr_t )
{ /* Close down everything */
 if(dat == NULL) return;
 dat->form  = NULL;
 dat->shell= NULL;
 free(dat);
 return;
}
