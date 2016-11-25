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
#include <stdlib.h>
#include <X11/StringDefs.h>
#include <X11/Intrinsic.h>
#include <Xm/Xm.h>
#include <Xm/DialogS.h>
#include <Xm/Form.h>
#include <Xm/PushB.h>
#include <Xm/RowColumn.h>
#include <Xm/ToggleB.h>
#include <Xm/Separator.h>
#include "wproc.h"
#include "sl_cvm_app.hh"
#include "vec_list_util.hh"
#include "dispopt.h"


/* PROTOTYPES FOR PRIVATE METHODS */
#ifdef __cplusplus
extern "C" {                 // for C++
#endif
static void ControlCB (Widget , dispcvm *, caddr_t );
static void DispCB(Widget , dispcvm *, caddr_t b);
static void KillCB(Widget W, dispcvm *, caddr_t b);
#ifdef __cplusplus
			   }                   // for C++
#endif

Widget DispGUI(int option, Widget parent, struct HELPCTX *Hctx,
       void *cvmapp)
{/* set option = 0 for return widget to be a form in a dialog */
 /* set option != 0 for return widget to be a form */
 dispcvm *disp_data;
 char  title[24];
 Arg   arglist[25];
 int   i,n;


 static String tbnames[4]= {"tb1","tb2","tb3","tb4"};
 static String tblabs[4] = {"Structure","Velocity", "Cells","CellPointer"};

 XmString  xstr;
 Widget  shell,form,disprc,ok,cancel;
 Widget  sep;
 Widget  togpb[4];
 
/***************************************************
 * Do some sanity checking                        */
 if(parent  == NULL) return NULL;
 if(cvmapp  == NULL) return NULL;

/*************************************************
 * Create a popup shell                         */
 i=0;
 disp_data = (dispcvm *) calloc(1,sizeof(dispcvm));
 disp_data->shell = NULL;
 disp_data->appdata= (void *) cvmapp;
 for(i=0;i<XtNumber(disp_data->ObjData);i++)
  {  disp_data->ObjData[i].lwidth=2;
     disp_data->ObjData[i].mwidth=1;
     disp_data->ObjData[i].msize=7;
  }
 if(option == 0)
  {i= 0;
   sprintf(title,"Model Display Options");
   XtSetArg (arglist[i], XmNtitle, title); i++;
   shell = XtCreatePopupShell("dispshell",xmDialogShellWidgetClass,
           parent,arglist,i);
   disp_data->shell = shell;
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
 disp_data->form = form;
 XtAddCallback(form,XmNdestroyCallback,(XtCallbackProc) KillCB,disp_data);
/*************************************************
 * Add OK pushbutton under form                 */
 n= 0;
 XtSetArg (arglist[n], XmNleftAttachment, XmATTACH_FORM); n++;
 XtSetArg (arglist[n], XmNleftOffset, 5); n++;
 XtSetArg (arglist[n], XmNrightAttachment, XmATTACH_NONE); n++;
 XtSetArg (arglist[n], XmNbottomAttachment, XmATTACH_FORM); n++;
 XtSetArg (arglist[n], XmNbottomOffset, 5) ; n++;
 ok= XtCreateWidget( "OK", xmPushButtonWidgetClass,
                                 form, arglist,n);
 XtAddCallback(ok,XmNactivateCallback,(XtCallbackProc) ControlCB,disp_data);
 disp_data->ok = ok;
 if(Hctx != NULL) add_HELP(ok, helper, Hctx);
/*************************************************
 * Add CANCEL pushbutton under form             */
 n= 0;
 XtSetArg (arglist[n], XmNleftAttachment, XmATTACH_NONE) ; n++;
 XtSetArg (arglist[n], XmNrightAttachment, XmATTACH_FORM) ; n++;
 XtSetArg (arglist[n], XmNrightOffset, 5) ; n++;
 XtSetArg (arglist[n], XmNbottomAttachment, XmATTACH_FORM) ; n++;
 XtSetArg (arglist[n], XmNbottomOffset, 5) ; n++;
 cancel= XtCreateWidget( "CANCEL", xmPushButtonWidgetClass,
                                 form, arglist,n);
 XtAddCallback(cancel,XmNactivateCallback,(XtCallbackProc) ControlCB,disp_data);
 disp_data->cancel = cancel;
 if(Hctx != NULL) add_HELP(cancel, helper, Hctx);
/*************************************************
 * Create a Separator                           */
 n= 0;
 XtSetArg (arglist[n], XmNleftAttachment, XmATTACH_FORM) ; n++;
 XtSetArg (arglist[n], XmNrightAttachment, XmATTACH_FORM) ; n++;
 XtSetArg (arglist[n], XmNbottomAttachment, XmATTACH_WIDGET) ; n++;
 XtSetArg (arglist[n], XmNbottomWidget, ok) ; n++;
 sep = XtCreateWidget( "sep", xmSeparatorWidgetClass,form, arglist,n);
/*************************************************
 * Add a RowColumn under the form               */
 n=0;
 XtSetArg (arglist[n], XmNradioBehavior,False); n++;
 XtSetArg (arglist[n], XmNpacking,XmPACK_COLUMN); n++;
 XtSetArg (arglist[n], XmNleftAttachment, XmATTACH_FORM); n++;
 XtSetArg (arglist[n], XmNleftOffset, 20); n++;
 XtSetArg (arglist[n], XmNrightAttachment, XmATTACH_NONE); n++;
 XtSetArg (arglist[n], XmNtopOffset, 50); n++;
 XtSetArg (arglist[n], XmNbottomAttachment, XmATTACH_WIDGET) ; n++;
 XtSetArg (arglist[n], XmNbottomWidget, sep) ; n++;
 XtSetArg (arglist[n], XmNbottomOffset, 10) ; n++;
 disprc= XtCreateWidget("disprc", xmRowColumnWidgetClass,
                               form, arglist,n);
/************************************************
 * Create toggle buttons for selection topics  */
 for(i=0;i<XtNumber(togpb);i++)
  {
   xstr = XmStringCreateSimple(tblabs[i]);
   n=0;
   XtSetArg(arglist[n], XmNlabelString, xstr); n++;
   XtSetArg(arglist[n], XmNset, False); n++;
   togpb[i] =  XtCreateWidget(tbnames[i], xmToggleButtonWidgetClass,
                              disprc, arglist,n);
   XmStringFree(xstr);
   if(Hctx != NULL) add_HELP(togpb[i], helper, Hctx);
   XtAddCallback(togpb[i],XmNvalueChangedCallback,(XtCallbackProc) DispCB,disp_data);
   disp_data->ObjData[i].widget = togpb[i];
   disp_data->ObjData[i].show = False;
  }
 XmToggleButtonSetState(togpb[0],True,False);
 disp_data->ObjData[0].show = True;
 XmToggleButtonSetState(togpb[1],False,False);
 XmToggleButtonSetState(togpb[2],False,False);
 XmToggleButtonSetState(togpb[3],False,False);

 XtManageChild(ok);
 XtManageChild(cancel);
 XtManageChild(sep);
 XtManageChildren(togpb, XtNumber(togpb));
 XtManageChild(disprc);
 XtManageChild(form);

 return form;
}

static void ControlCB(Widget W, dispcvm *disp_data, caddr_t )
{ /* Unmanage the graphics window */
 Widget widget;

 CvmApp *cvmdata;
 int lorm;
 void *vls;

 if(disp_data == NULL) return;
 cvmdata = (CvmApp *) disp_data->appdata;
 if(cvmdata == NULL) return;
 vls = (void *) cvmdata->cvm_get_vll(CvmAppBase::Structure);
 if(W == disp_data->ok )
  {
   lorm = 0;
   if(disp_data->ObjData[0].show == True) lorm = 3;
   VectorListSetVis(vls,(int) lorm);
  }
 widget = disp_data->shell;
 if(widget == NULL) widget = disp_data->form;
 if(W == disp_data->ok || W == disp_data->cancel) XtDestroyWidget(widget);
 return;
}

static void DispCB(Widget W, dispcvm *disp_data, caddr_t/* b*/)
{ /* Close down everything */
 Arg args[4];
 int i;
 Boolean state;
 CvmApp   *cvmdata;
 int lorm;
 void *vls; /* a SeisVectLinkedList object */

 if(disp_data == NULL) return;
 cvmdata = (CvmApp *) disp_data->appdata;
 i = 0;
 XtSetArg (args[i], XmNset, &state); i++;
 XtGetValues(W, args, i);
 if(cvmdata == NULL) return;
 if(W == disp_data->ObjData[0].widget )
  {lorm = 0;
   if(state==True) lorm = 3;
   disp_data->ObjData[0].show=state;
   vls = (void *) cvmdata->cvm_get_vll(CvmAppBase::Structure);
   VectorListSetVis(vls,lorm);
  }
 if(W == disp_data->ObjData[1].widget)
  {lorm = 0;
   if(state==True) lorm = 1;
   disp_data->ObjData[1].show=state;
   vls = (void *) cvmdata->cvm_get_vll(CvmAppBase::Materials);
   VectorListSetVis(vls,lorm);
  }
 if(W == disp_data->ObjData[2].widget)
  {lorm = 0;
   if(state==True) lorm = 1;
   disp_data->ObjData[2].show=state;
   vls = (void *) cvmdata->cvm_get_vll(CvmAppBase::Boundarys);
   VectorListSetVis(vls,lorm);
  }
 if(W == disp_data->ObjData[3].widget)
  {lorm = 0;
   if(state==True) lorm = 1;
   disp_data->ObjData[2].show=state;
   vls = (void *) cvmdata->cvm_get_vll(CvmAppBase::Cpointers);
   VectorListSetVis(vls,lorm);
  }

 return;
}

static void KillCB(Widget , dispcvm *dat, caddr_t )
{ /* Close down everything */
 if(dat == NULL) return;
 dat->form=NULL;
 dat->shell=NULL;
 dat->appdata=NULL;
 free(dat);
 return;
}

