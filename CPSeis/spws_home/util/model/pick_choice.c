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
#include <X11/Xlib.h>
#include <X11/Intrinsic.h>
#include <Xm/Xm.h>
#include <Xm/DialogS.h>
#include <Xm/ToggleB.h>
#include <Xm/Text.h>
#include <Xm/PushB.h>
#include <Xm/RowColumn.h>
#include <Xm/Separator.h>
#include <Xm/Form.h>
#include <mod_pop.h>
#include <pcard.h>


typedef struct _pikchoice
 { 
   int control;
   int style;
   Widget shell,form;
   Widget modpik,velpik;
   Widget ok;
 } pikchoice;

static pikchoice *DATA;

void   PickComponentCB(Widget W, ModInfo *modgui, caddr_t b);
Widget PickComponentGUI(Widget parent,int option, ModInfo *modgui);
static void PickCompTBCB(Widget W, ModInfo *modgui, caddr_t b);
static void KillCB(Widget W, pikchoice *dat, caddr_t b);
static void ControlCB(Widget W, pikchoice *data, caddr_t b);

void PickComponentCB(Widget W, ModInfo *modgui, caddr_t b)
{
 if(modgui == NULL) return;
 if(modgui->comp_form == NULL)
  { modgui->comp_form = 
     PickComponentGUI(modgui->ctl_form, 0, modgui);
  }
 /* Set toggles to indicate the current state */
 if(modgui->Spik_or_Vpik == MODPICK)
  {XmToggleButtonSetState(DATA->modpik,True,False);
   XmToggleButtonSetState(DATA->velpik,False,False);
   DATA->style=MODPICK;
  }
 if(modgui->Spik_or_Vpik == VELPICK)
  {XmToggleButtonSetState(DATA->velpik,True,False);
   XmToggleButtonSetState(DATA->modpik,False,False);
   DATA->style=VELPICK;
  }
 XtManageChild(modgui->comp_form);
}

/****
 * Build the popup for making a coice in picking style
 ***/
Widget PickComponentGUI(Widget parent,int option, ModInfo *modgui)
{/* set option = 0 for return widget to be a form in a dialog */
 /* set option != 0 for return widget to be a form */

 extern void helper();
 HelpCtx  Hctx;
 char  title[24];
 Arg   arglist[25];
 long i,n,num;
 int  otyp;

 static String tbnames[2]= {"structureh","velocityh"};
 static String tblabs[2] = {"Structural Horizons",
                            "Velocity Horizons"};

 XmString  xstr;
 Widget  shell,form,rc,ok,cancel;
 Widget  fcw,sep;
 Widget  togpb[2];

/***************************************************
 * Do some sanity checking                        */
 if(parent  == NULL) return NULL;
 if(modgui  == NULL) return NULL;
 Hctx = modgui->helpctx;

/*************************************************
 * Create a popup shell                         */
 i=0;
 DATA = (pikchoice *) calloc(1,sizeof(pikchoice));
 DATA->shell = NULL;
 if(option == 0)
  {
   sprintf(title,"Model Picking Choice");
   XtSetArg (arglist[i], XmNtitle, title); i++;
   shell = XtCreatePopupShell("pchshell",xmDialogShellWidgetClass,
           parent,arglist,i);
   DATA->shell = shell;
  }

/*************************************************
 * Create a Form Widget */
 i = 0;
 form = NULL;
 if(option == 0)
  form = XmCreateForm(shell, "form", arglist, i);
 else
  form = XmCreateForm(parent, "form", arglist, i);
 DATA->form = form;
 XtAddCallback(form,XmNdestroyCallback,KillCB,DATA);

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
 XtAddCallback(ok,XmNactivateCallback,ControlCB,DATA);
 DATA->ok = ok;
 if(Hctx != NULL) add_HELP(ok, helper, Hctx);

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
 num = 1;
 XtSetArg (arglist[n], XmNradioBehavior,True); n++;
 XtSetArg (arglist[n], XmNorientation, XmVERTICAL);
 XtSetArg (arglist[n], XmNnumColumns, num); n++;
 XtSetArg (arglist[n], XmNpacking, XmPACK_COLUMN); n++;
 XtSetArg (arglist[n], XmNleftAttachment, XmATTACH_FORM); n++;
 XtSetArg (arglist[n], XmNleftOffset, 25); n++;
 XtSetArg (arglist[n], XmNrightAttachment, XmATTACH_NONE); n++;
 XtSetArg (arglist[n], XmNbottomAttachment, XmATTACH_WIDGET) ; n++;
 XtSetArg (arglist[n], XmNbottomWidget, sep) ; n++;
 XtSetArg (arglist[n], XmNbottomOffset, 10) ; n++;
 rc= XtCreateWidget("pikchrc", xmRowColumnWidgetClass,
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
                              rc, arglist,n);
   XmStringFree(xstr);
   if(Hctx != NULL) add_HELP(togpb[i], helper, Hctx);
  }
 DATA->modpik=togpb[0];
 DATA->velpik=togpb[1];
 if(modgui->Spik_or_Vpik == MODPICK)
   XmToggleButtonSetState(togpb[0],True,False);
 if(modgui->Spik_or_Vpik == VELPICK)
   XmToggleButtonSetState(togpb[1],True,False);
 XtAddCallback(togpb[0],XmNvalueChangedCallback,PickCompTBCB,modgui);
 XtAddCallback(togpb[1],XmNvalueChangedCallback,PickCompTBCB,modgui);
 DATA->style=modgui->Spik_or_Vpik;

 XtManageChild(ok);
 XtManageChild(sep);
 XtManageChildren(togpb, XtNumber(togpb));
 XtManageChild(rc);
 XtManageChild(form);

 return form;


}

static void PickCompTBCB(Widget W, ModInfo *modgui, caddr_t b)
{ /* Close down everything */
 XmToggleButtonCallbackStruct *tbs;
 Arg args[4];
 int i,type;
 Boolean state;
 tbs = (XmToggleButtonCallbackStruct *) b;
/* return if no picking is not active yet */
 if(modgui == NULL) return;
 if(DATA   == NULL) return;
 if(modgui->pikrec == NULL || modgui->model == NULL) return;
 if(tbs->event == NULL) return;
 if(tbs->event->type != ButtonPress &&
    tbs->event->type != ButtonRelease) return;
 i = 0;
 XtSetArg (args[i], XmNset, &state); i++;
 XtGetValues(W, args, i);
 if(W == DATA->modpik )
  {if(state == True) type = MODPICK;
   else type = VELPICK;
  }
 if(W == DATA->velpik)
  {if(state == True) type = VELPICK;
   else type = MODPICK;
  }
 if(type != modgui->Spik_or_Vpik) ModOrVelPick((void *) modgui, type);
 if(modgui->Spik_or_Vpik == MODPICK)
  {XmToggleButtonSetState(DATA->modpik,True,False);
   XmToggleButtonSetState(DATA->velpik,False,False);
   DATA->style=MODPICK;
  }
 if(modgui->Spik_or_Vpik == VELPICK)
  {XmToggleButtonSetState(DATA->velpik,True,False);
   XmToggleButtonSetState(DATA->modpik,False,False);
   DATA->style=VELPICK;
  }
 return;
}

static void KillCB(Widget W, pikchoice *dat, caddr_t b)
{ /* Close down everything */
 if(dat == NULL) return;
 free(dat);
 DATA = NULL;
 return;
}

static void ControlCB(Widget W, pikchoice *data, caddr_t b)
{ /* Unmanage the graphics window */
 Widget widget;
 char msg[240];
 int  ierr;
 if(data == NULL) return;
 widget = data->shell;
 widget = data->form;
 XtUnmanageChild(widget);
 return;
}

