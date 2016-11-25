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
#include <wproc.h>
#include "file_choice.h"
#include <Xm/Separator.h>
#include <mod_pop.h>
#include "tfio.h"

typedef struct _savecvm
 { Widget shell,form,fcw;
   Widget cvm,gws,intw,sierra;
   Widget ok,cancel;
   ErsModel *model;
   long *style;
   char *file;
 } savecvm;


void SaveAsCB(Widget W, ModInfo *modgui, caddr_t b);
Widget SaveModGUI(Widget parent,int option,char *file, long *style,
                  ErsModel *model, struct HELPCTX *Hctx,Widget *fchoice);

/* PROTOTYPES FOR PRIVATE METHODS */
static void ControlCB (Widget , savecvm *, caddr_t );
static void StyleCB(Widget , savecvm *, caddr_t b);
static void FileCB(Widget W, savecvm *, caddr_t b);
static void KillCB(Widget W, savecvm *, caddr_t b);
static int SaveMod(ErsModel *model, char *fileo,long *otyp,char *msg);

/* interface to the model gui */
void SaveAsCB(Widget W, ModInfo *modgui, caddr_t b)
{Widget fcw;
 Arg   arglist[5];
 savecvm *save;
 int n;
 if(modgui == NULL) return;
 if(modgui->save_form == NULL)
  {modgui->save_form= 
     SaveModGUI(modgui->ctl_form,0,modgui->outfile,&modgui->outtyp,
                modgui->model, modgui->helpctx, &fcw);
   modgui->mwig[OFIL].w = fcw; /* save the filechoice widget */
  }
 else
  {if(strlen(modgui->outfile)== 0) strcpy(modgui->outfile,"NONE");
   wprocFileChoiceSetFile(modgui->mwig[OFIL].w,modgui->outfile,True);
   n = 0;
   XtSetArg (arglist[n], XmNuserData,&save); n++;
   XtGetValues(modgui->save_form,arglist,n);
   if(save != NULL) save->model = modgui->model;
  }
 XtManageChild(modgui->save_form);
}

Widget SaveModGUI(Widget parent,int option,char *file,long *style,
       ErsModel *model, struct HELPCTX *Hctx, Widget *fchoice)
{/* set option = 0 for return widget to be a form in a dialog */
 /* set option != 0 for return widget to be a form */
 savecvm *SAVE_DATA;
 extern void helper();
 char  title[24],tfile[200];
 Arg   arglist[25];
 long i,n,num;
 int  nobj, oid, otyp;

 static String tbnames[2]= {"cvm","gws"};
 static String tblabs[2] = {"CVM","GWS"};

 XmString  xstr;
 Widget  shell,form,savrc,ok,cancel;
 Widget  fcw,sep;
 Widget  togpb[2];
 
/***************************************************
 * Do some sanity checking                        */
 if(parent == NULL) return NULL;
 if(model  == NULL) return NULL;
 if(file == NULL || style == NULL) return NULL;

/*************************************************
 * Create a popup shell                         */
 i=0;
 SAVE_DATA = (savecvm *) calloc(1,sizeof(savecvm));
 SAVE_DATA->model = model;
 SAVE_DATA->file  = file;
 SAVE_DATA->style = style;
 SAVE_DATA->shell = NULL;
 strcpy(tfile,file);
 if(option == 0)
  {
   sprintf(title,"Save Model Data");
   XtSetArg (arglist[i], XmNtitle, title); i++;
   shell = XtCreatePopupShell("savshell",xmDialogShellWidgetClass,
           parent,arglist,i);
   SAVE_DATA->shell = shell;
  }
/*************************************************
 * Create a Form Widget */
 i = 0;
 XtSetArg (arglist[i], XmNuserData, SAVE_DATA); i++;
 form = NULL;
 if(option == 0)
  {XtSetArg (arglist[i], XmNautoUnmanage,False); i++;
   form = XmCreateForm(shell, "form", arglist, i);
  }
 else
  form = XmCreateForm(parent, "form", arglist, i);
 SAVE_DATA->form = form;
 XtAddCallback(form,XmNdestroyCallback,KillCB,SAVE_DATA);
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
 XtAddCallback(ok,XmNactivateCallback,ControlCB,SAVE_DATA);
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
 XtAddCallback(cancel,XmNactivateCallback,ControlCB,SAVE_DATA);
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
 XtSetArg (arglist[n], XmNorientation, XmHORIZONTAL);
 XtSetArg (arglist[n], XmNnumColumns, num); n++;
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
 XtSetArg (arglist[n], XmNfileTargetAddr, file); n++;
 fcw = XtCreateWidget( "ofil",fileChoiceWidgetClass,
       form, arglist,n);
 *fchoice = fcw;
 SAVE_DATA->fcw = fcw;
 XtAddCallback(fcw,XmNsuccessCallback,FileCB,SAVE_DATA);
 if(strlen(tfile)==0) strcpy(tfile,"NONE");
 wprocFileChoiceSetFile(SAVE_DATA->fcw,tfile,True);
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
   XtAddCallback(togpb[i],XmNvalueChangedCallback,StyleCB,SAVE_DATA);
  }
 SAVE_DATA->cvm=togpb[0];
 SAVE_DATA->gws=togpb[1];
 XmToggleButtonSetState(togpb[0],False,False);
 XmToggleButtonSetState(togpb[1],False,False);
 *SAVE_DATA->style=LAYER_TYPE;

 XtManageChild(ok);
 XtManageChild(cancel);
 XtManageChild(sep);
 XtManageChildren(togpb, XtNumber(togpb));
 XtManageChild(savrc);
 XtManageChild(fcw);
 XtManageChild(form);

 if(*style == LAYER_TYPE)
  XmToggleButtonSetState(SAVE_DATA->cvm,True,False);
 else
  XmToggleButtonSetState(SAVE_DATA->cvm,False,False);
 if(*style == GWS_TYPE)
  XmToggleButtonSetState(SAVE_DATA->gws,True,False);

 return form;
}

static void ControlCB(Widget W, savecvm *sav, caddr_t b)
{ /* Unmanage the Save As... window */
 Widget widget;
 char msg[240];
 int  ierr;
 if(sav == NULL) return;
 if(W == sav->ok )
  { ierr = SaveMod(sav->model,sav->file,sav->style,msg);
    if(ierr == 0)
     {strcat(msg,"-WRITE FAILURE");
      MODMsgBox(sav->form,msg);
      return;
     }
  }
 widget = sav->form;
 if(W == sav->ok || W == sav->cancel)
  {if(widget != NULL)  XtUnmanageChild(widget); }
 return;
}

static void StyleCB(Widget W, savecvm *sav, caddr_t b)
{ /* Close down everything */
 Arg args[4];
 int i;
 Boolean state;
 if(sav == NULL) return;
 *sav->style=LAYER_TYPE;
 i = 0;
 XtSetArg (args[i], XmNset, &state); i++;
 XtGetValues(W, args, i);
 if(W == sav->gws && state==True) *sav->style=GWS_TYPE;
 if(W == sav->cvm && state==True) *sav->style=LAYER_TYPE;
 return;
}

static void FileCB(Widget W, savecvm *sav, caddr_t b)
{
 Widget form;
 char fileo[120];
 if(sav == NULL) return;
 strcpy(sav->file,"NONE");
 if(!wprocFileChoiceGetFileByStr(W,fileo) ) return;
 strcpy(sav->file,fileo);
 XtSetSensitive(sav->ok,True);
 return;
}

static void KillCB(Widget W, savecvm *dat, caddr_t b)
{ /* Close down everything */
 if(dat == NULL) return;
 free(dat);
 return;
}

static int SaveMod(ErsModel *model,char *fileo,long *otyp,char *msg)
{
 if(fileo == NULL) return 0;
 if(strncmp(fileo," "   ,1)==0) return 0;
 if(strncmp(fileo,"NONE",4)==0) return 0;
 if(strncmp(fileo,"none",4)==0) return 0;
 if(model == NULL)
  { strcpy(msg,"SaveMod: NULL model found for output");
    return 0;}

 if(*otyp == SRATYP)
  {sprintf(msg,"SaveMod: Sierra output not supported");
   return 0;
  }

/*  Save the data in a GWS or an CVM file format */
 if(*otyp == GWS_TYPE || *otyp == LAYER_TYPE )
  { int ierr;
    if(pcardwr(model,fileo,*otyp))
     { return 1; }
    else
     { strcpy(msg,"SaveMod: pcardwr write failure");
       return 0;
     }
  }

 return 0;
}



