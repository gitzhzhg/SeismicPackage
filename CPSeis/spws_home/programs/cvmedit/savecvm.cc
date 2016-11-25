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
#include "file_choice.h"
#include "tfio.h"
#include "model_io.h"
#include "model_desc.hh"
#include "sl_cvm_app.hh"
#include "savecvm.h"


/* PROTOTYPES FOR PRIVATE METHODS */
#ifdef __cplusplus
extern "C" {                 // for C++
#endif

static void ControlCB (Widget , savecvm *, caddr_t );
static void StyleCB(Widget , savecvm *, caddr_t b);
static void FileCB(Widget W, savecvm *, caddr_t b);
static void KillCB(Widget W, savecvm *, caddr_t b);
void   CVMMsgBox(Widget , char *);

#ifdef __cplusplus
}                   // for C++
#endif
 

Widget SaveGUI(int option, void *data, HelpCtx Hctx)
{/* set option = 0 for return widget to be a form in a dialog */
 /* set option != 0 for return widget to be a form */
 CvmAppBase *cvmapp = (CvmAppBase *) data;
 Widget parent;
 savecvm *SAVE_DATA;
 char  title[24];
 Arg   arglist[25];
 int   i,n,num;


 static String tbnames[3]= {"tb1","tb2","tb3"};
 static String tblabs[3] = {"CVM","GWS", "Gocad++"};

 XmString  xstr;
 Widget  shell,form,savrc,ok,cancel;
 Widget  fcw,sep;
 Widget  togpb[3];
 
/***************************************************
 * Do some sanity checking                        */
 parent = cvmapp->mainWindow();
 if(parent  == NULL) return NULL;

/*************************************************
 * Create a popup shell                         */
 i=0;
 SAVE_DATA = (savecvm *) calloc(1,sizeof(savecvm));
 SAVE_DATA->shell = NULL;
 SAVE_DATA->cvmapp = (void *) cvmapp;
 if(option == 0)
  {
   sprintf(title,"Save CVM Data");
   XtSetArg (arglist[i], XmNtitle, title); i++;
   shell = XtCreatePopupShell("savshell",xmDialogShellWidgetClass,
           parent,arglist,i);
   SAVE_DATA->shell = shell;
  }
 strcpy(SAVE_DATA->file,"NONE");
 SAVE_DATA->can_write = NO_WRITE;
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
 SAVE_DATA->form = form;
 XtAddCallback(form,XmNdestroyCallback,(XtCallbackProc) KillCB,SAVE_DATA);
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
 XtAddCallback(ok,XmNactivateCallback,(XtCallbackProc) ControlCB,SAVE_DATA);
 SAVE_DATA->ok = ok;
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
 XtAddCallback(cancel,XmNactivateCallback,(XtCallbackProc) ControlCB,SAVE_DATA);
 SAVE_DATA->cancel = cancel;
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
 num = XtNumber(togpb);
 XtSetArg (arglist[n], XmNradioBehavior,True); n++;
 XtSetArg (arglist[n], XmNpacking,XmPACK_COLUMN); n++;
 XtSetArg (arglist[n], XmNleftAttachment, XmATTACH_FORM); n++;
 XtSetArg (arglist[n], XmNleftOffset, 20); n++;
 XtSetArg (arglist[n], XmNrightAttachment, XmATTACH_NONE); n++;
 XtSetArg (arglist[n], XmNtopOffset, 50); n++;
 XtSetArg (arglist[n], XmNbottomAttachment, XmATTACH_WIDGET) ; n++;
 XtSetArg (arglist[n], XmNbottomWidget, sep) ; n++;
 XtSetArg (arglist[n], XmNbottomOffset, 10) ; n++;
 savrc= XtCreateWidget("savrc", xmRowColumnWidgetClass,
                               form, arglist,n);
/*************************************************
 * Add a FileChoice widget under the form       */
 n = 0;
 XtSetArg (arglist[n], XmNfileExtension, "pck"); n++;
 XtSetArg (arglist[n], XmNfileFlags, wprocWritableMask|
           wprocOverWriteWarnMask); n++;
 XtSetArg (arglist[n], XmNlabelString,"CVM Output") ; n++;
 XtSetArg (arglist[n], XmNannoType,wprocLabel) ; n++;
 XtSetArg (arglist[n], XmNtopAttachment, XmATTACH_NONE); n++;
 XtSetArg (arglist[n], XmNy, 35); n++;
 XtSetArg (arglist[n], XmNleftAttachment, XmATTACH_FORM); n++;
 XtSetArg (arglist[n], XmNleftOffset, 20); n++;
 XtSetArg (arglist[n], XmNrightAttachment, XmATTACH_FORM); n++;
 XtSetArg (arglist[n], XmNrightOffset, 20); n++;
 XtSetArg (arglist[n], XmNbottomAttachment, XmATTACH_WIDGET) ; n++;
 XtSetArg (arglist[n], XmNbottomWidget, savrc) ; n++;
 XtSetArg (arglist[n], XmNbottomOffset, 10) ; n++;
 fcw = XtCreateWidget( "FCWINO",fileChoiceWidgetClass,
       form, arglist,n);
 XtAddCallback(fcw,XmNsuccessCallback,(XtCallbackProc) FileCB,SAVE_DATA);
/************************************************
 * Create toggle buttons for selection topics  */
 for(i=0;i<XtNumber(togpb);i++)
  {
   xstr = XmStringCreateSimple(tblabs[i]);
   n=0;
   XtSetArg(arglist[n], XmNlabelString, xstr); n++;
   XtSetArg(arglist[n], XmNset, False); n++;
   togpb[i] =  XtCreateWidget(tbnames[i], xmToggleButtonWidgetClass,
                              savrc, arglist,n);
   XmStringFree(xstr);
   if(Hctx != NULL) add_HELP(togpb[i], helper, Hctx);
   XtAddCallback(togpb[i],XmNvalueChangedCallback,(XtCallbackProc) StyleCB,SAVE_DATA);
  }
 SAVE_DATA->cvm=togpb[0];
 SAVE_DATA->gws=togpb[1];
 SAVE_DATA->goc=togpb[2];
 XmToggleButtonSetState(togpb[0],True,False);
 XmToggleButtonSetState(togpb[1],False,False);
 XmToggleButtonSetState(togpb[2],False,False);
 SAVE_DATA->style=LAYER_TYPE;

 XtManageChild(ok);
 XtManageChild(cancel);
 XtManageChild(sep);
 XtManageChildren(togpb, XtNumber(togpb));
 XtManageChild(savrc);
 XtManageChild(fcw);
 XtManageChild(form);

 return form;
}

static void ControlCB(Widget W, savecvm *sav, caddr_t )
{ /* Unmanage the graphics window */
 Widget widget;
 char msg[240];
 int  ierr;
 if(sav == NULL) return;
 if(W == sav->ok )
  { if(sav->can_write == DO_WRITE)
     {ierr = savecvm_out(sav->cvmapp,sav->file,(long *) &sav->style,msg);
      if(ierr == 0)
       {strcat(msg,"- FAILED TO WRITE");
        CVMMsgBox(sav->form,msg);
        return;
       }
     }
    else
     { sprintf(msg,"ControlCB: no file written");
       CVMMsgBox(sav->form,msg);
       return;
     }
  }
 widget = sav->shell;
 if(widget == NULL) widget = sav->form;
 if(W == sav->ok || W == sav->cancel) XtDestroyWidget(widget);
 return;
}

static void StyleCB(Widget W, savecvm *sav, caddr_t )
{ /* Close down everything */
 Arg args[4];
 int i;
 Boolean state;
 if(sav == NULL) return;
 sav->style=LAYER_TYPE;
 i = 0;
 XtSetArg (args[i], XmNset, &state); i++;
 XtGetValues(W, args, i);
 if(W == sav->gws && state==True) sav->style=GWS_TYPE;
 if(W == sav->cvm && state==True) sav->style=LAYER_TYPE;
 if(W == sav->goc && state==True) sav->style=GOCAD_TYPE;
/* printf("style=%d\n",sav->style); */
 return;
}

static void FileCB(Widget W, savecvm *sav, caddr_t )
{
 char fileo[96];
 if(sav == NULL) return;
 strcpy(sav->file,"NONE");
 if(!wprocFileChoiceGetFileByStr(W,fileo) ) return;
 strcpy(sav->file,fileo);
 sav->can_write = DO_WRITE;
 return;
}

static void KillCB(Widget , savecvm *dat, caddr_t )
{ /* Close down everything */
 if(dat == NULL) return;
 free(dat);
 return;
}

int savecvm_out(void *data, char *fileo,long *otyp,char *msg)
{CvmAppBase *cvmapp = (CvmAppBase *) data;
 ErsModel     *model=NULL;
 ModelDesc    *mod=NULL;

 strcpy(msg,"OK");
 if(fileo == NULL) return 0;
 if(strncmp(fileo," "   ,1)==0) return 0;
 if(strncmp(fileo,"NONE",4)==0) return 0;
 if(strncmp(fileo,"none",4)==0) return 0;
 mod = cvmapp->getModelDesc();
 if(!mod) return 0;
// Create a temporary ErsModel from a ModelDesc
 model = mod->toErsModel();
 if(!model)
  { strcpy(msg,"savecvm_out: NULL model found for output\n");
    return 0;
  }

/*  Save the data in a GWS, CVM or Gocad file format */
 if(*otyp == GWS_TYPE || *otyp == LAYER_TYPE || *otyp == GOCAD_TYPE)
  { model_settype(model,"LAYER");
    model_settfile(model,"SAME");
    if(pcardwr(model,fileo,*otyp))
     { destroy_model(model); return 1; }
    else
     { strcpy(msg,"Failure: Model file not written out\n");
       destroy_model(model); 
       return 0;
     }
  }

 if(model) destroy_model(model); 
 return 0;
}
