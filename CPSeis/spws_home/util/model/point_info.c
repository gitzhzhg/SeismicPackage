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
#include <Xm/DialogS.h>
#include <Xm/PushB.h>
#include <Xm/Form.h>
#include <Xm/Separator.h>
#include <Xm/RowColumn.h>
#include <Xm/Label.h>
#include <Xm/Text.h>
#include <string.h>
#include <stdlib.h>
#include <pick.h>


typedef struct _PtInfo
 { Widget text;
   Widget idtext;
   Widget mltext;
   Widget shell,form;
   ErsPoint *point;
   ErsHorizon *horizon;
   PR_ *pr;
 } PtInfo;

Widget PointInfoGUI(Widget parent);
void   PointInfoShow(Widget w, PR_ *pikrec, ErsPoint *point);

static PtInfo Info;
static void PointInfoDes(Widget W, PtInfo *data, caddr_t b);
static void Pt_gettxt(Widget W, PtInfo *data, caddr_t b);
static void OKcb(Widget W, Widget form, caddr_t b);

Widget PointInfoGUI(Widget parent)
{
 Arg      arglist[20];
 XmString  xstr;
 Widget  shell,form,sep,ok,temp;
 Widget  idlab,lab,idtext,text,mltext,rc;
 int     n,num;
 char    title[80];

 n = 0;
 XmCreateDialogShell(parent,"ShowPt",arglist,n);
 n=0;
 sprintf(title,"%s","Point Information");
 XtSetArg(arglist[n], XmNtitle, title); n++;
 XtSetArg(arglist[n], XtNallowShellResize, True); n++;
 shell = XtCreatePopupShell("infoshell",xmDialogShellWidgetClass,
         parent,arglist,n);

 Info.shell = shell;
 n=0;
 form = XmCreateForm(shell, "form", arglist, n);
 XtAddCallback(form,XmNdestroyCallback,PointInfoDes,&Info);
 
/*************************************************
 * Add OK pushbutton under form                 */
 n= 0;
 XtSetArg (arglist[n], XmNleftAttachment, XmATTACH_POSITION) ; n++;
 XtSetArg (arglist[n], XmNleftPosition, 45) ; n++;
 XtSetArg (arglist[n], XmNbottomAttachment, XmATTACH_FORM) ; n++;
 XtSetArg (arglist[n], XmNbottomOffset, 5) ; n++;
 ok= XtCreateWidget( "OK", xmPushButtonWidgetClass,
                                 form, arglist,n);
 XtAddCallback(ok,XmNactivateCallback,OKcb,form);

 n= 0;
 XtSetArg (arglist[n], XmNrightAttachment, XmATTACH_FORM) ; n++;
 XtSetArg (arglist[n], XmNleftAttachment, XmATTACH_FORM) ; n++;
 XtSetArg (arglist[n], XmNbottomAttachment, XmATTACH_WIDGET) ; n++;
 XtSetArg (arglist[n], XmNbottomWidget, ok) ; n++;
 XtSetArg (arglist[n], XmNbottomOffset, 5) ; n++;
 sep = XtCreateWidget( "sep", xmSeparatorWidgetClass,
                                 form, arglist,n);


/*************************************************
 * Add a RowColumn under the form               */
 n=0;
 num = 2;
 XtSetArg (arglist[n], XmNorientation, XmHORIZONTAL);
 XtSetArg (arglist[n], XmNnumColumns, num); n++;
 XtSetArg (arglist[n], XmNpacking, XmPACK_COLUMN); n++;
 XtSetArg (arglist[n], XmNleftAttachment, XmATTACH_FORM); n++;
 XtSetArg (arglist[n], XmNleftOffset, 25); n++;
 XtSetArg (arglist[n], XmNrightAttachment, XmATTACH_NONE); n++;
 XtSetArg (arglist[n], XmNbottomAttachment, XmATTACH_WIDGET) ; n++;
 XtSetArg (arglist[n], XmNbottomWidget, sep) ; n++;
 XtSetArg (arglist[n], XmNbottomOffset, 10) ; n++;
 rc= XtCreateWidget("ptrc", xmRowColumnWidgetClass,
                               form, arglist,n);

/*************************************************
 * Add a label and text widgets                 */
 n= 0;
 xstr = (XmString) XmStringCreateLtoR("Point\nUser Data",
                       XmSTRING_DEFAULT_CHARSET);
 XtSetArg(arglist[n], XmNlabelString, xstr); n++;
 lab = XtCreateWidget( "ptlabel", xmLabelWidgetClass, rc, arglist,n);
 XmStringFree(xstr);

 n= 0;
 XtSetArg (arglist[n], XmNeditable, True); n++;
 XtSetArg (arglist[n], XmNeditMode, XmSINGLE_LINE_EDIT); n++;
 XtSetArg (arglist[n], XmNcolumns, 10) ; n++;
 text = XtCreateWidget( "pttext", xmTextWidgetClass, rc, arglist,n);
 XtAddCallback(text,XmNactivateCallback, Pt_gettxt,&Info);
 XtAddCallback(text,XmNlosingFocusCallback, Pt_gettxt,&Info);

 n= 0;
 xstr = (XmString) XmStringCreateLtoR("Horizon\nUser Data",
                       XmSTRING_DEFAULT_CHARSET);
 XtSetArg(arglist[n], XmNlabelString, xstr); n++;
 idlab = XtCreateWidget( "ptlabel2", xmLabelWidgetClass, rc, arglist,n);
 XmStringFree(xstr);

 n= 0;
 XtSetArg (arglist[n], XmNeditable, True); n++;
 XtSetArg (arglist[n], XmNeditMode, XmSINGLE_LINE_EDIT); n++;
 XtSetArg (arglist[n], XmNcolumns, 10) ; n++;
 idtext = XtCreateWidget( "pttext2", xmTextWidgetClass, rc, arglist,n);
 XtAddCallback(idtext,XmNactivateCallback, Pt_gettxt,&Info);
 XtAddCallback(idtext,XmNlosingFocusCallback, Pt_gettxt,&Info);

 n= 0;
 XtSetArg (arglist[n], XmNleftAttachment, XmATTACH_FORM) ; n++;
 XtSetArg (arglist[n], XmNrightAttachment, XmATTACH_FORM) ; n++;
 XtSetArg (arglist[n], XmNrightOffset, 25) ; n++;
 XtSetArg (arglist[n], XmNleftOffset, 25) ; n++;
 XtSetArg (arglist[n], XmNbottomAttachment, XmATTACH_WIDGET) ; n++;
 XtSetArg (arglist[n], XmNbottomWidget, rc) ; n++;
 XtSetArg (arglist[n], XmNbottomOffset, 5) ; n++;
 XtSetArg (arglist[n], XmNeditable, False); n++;
 XtSetArg (arglist[n], XmNcolumns, 30) ; n++;
 XtSetArg (arglist[n], XmNrows,6); n++;
 XtSetArg (arglist[n], XmNeditMode, XmMULTI_LINE_EDIT); n++;
 mltext = XtCreateWidget( "ptmltext", xmTextWidgetClass,
                             form, arglist,n);

 XtManageChild(ok);
 XtManageChild(sep);
 XtManageChild(lab);
 XtManageChild(text);
 XtManageChild(idlab);
 XtManageChild(idtext);
 XtManageChild(rc);
 XtManageChild(mltext);
 Info.text = text;
 Info.idtext = idtext;
 Info.mltext = mltext;
 Info.form = form;
 Info.point= NULL;
 Info.horizon=NULL;
 Info.pr   = NULL;

 return form;
}

static void OKcb(Widget W, Widget form, caddr_t b)
{ /* Unmanage the graphics window */
 Widget widget;
 if(form == NULL) return;
 XtUnmanageChild(form);
 return;
}

void PointInfoShow(Widget w, PR_ *pikrec, ErsPoint *point)
{
  Widget built;
  ErsHorizon *horizon;
  XmString cstring;
  char string[256],user_data[16],horizid[8];
  char pval[16],sval[16],tval[16];
  Arg arg[3];
  float time;

  if(w == NULL) return;
  if(Info.form == NULL)
   { Info.form = PointInfoGUI(w);
     if(Info.form == NULL) return; }
  if(pikrec== NULL) return;
  if(point == NULL) return;
  Info.pr = pikrec;

  if(pikrec->Phdr == UNDEFINED_KEY )
   strcpy(pval,"UNDEFINED");
  else
   sprintf(pval,"%f",point->pkey);

  if(pikrec->Shdr == UNDEFINED_KEY )
   strcpy(sval,"UNDEFINED");
  else
   sprintf(sval,"%f",point->skey);

  if(pikrec->Thdr == UNDEFINED_KEY )
   strcpy(tval,"UNDEFINED");
  else
   sprintf(tval,"%f",point->tkey);

  time = point->time;
  sprintf(string,"Pick Time    = %f\n\
Trace Number = %d\n\
Primary   Key= %s\n\
Secondary Key= %s\n\
Tertiary  Key= %s\n", time, point->tn,pval,sval,tval);

 XmTextSetString(Info.mltext,string);
 sprintf(user_data,"%g",point->user_data);
 XmTextSetString(Info.text,user_data);
 sprintf(user_data,"%g",point->user_data);
 horizon = ErsHorizonGetFromPoint(pikrec,point);
 Info.horizon = horizon;

 sprintf(horizid,"%d",UNDEFINED_HID);
 if(horizon != NULL)
  { sprintf(horizid,"%d",horizon->hid); }
 XmTextSetString(Info.idtext,horizid);
  
 Info.point = point;
 XMapWindow(XtDisplay(Info.shell),XtWindow(Info.shell));
 XtManageChild(Info.form);

}


static void PointInfoDes(Widget W, PtInfo *data, caddr_t b)
{ 
 if(data == NULL) return;
 data->text  = NULL;
 data->mltext=NULL;
 data->idtext=NULL;
 data->form  = NULL;
 data->point = NULL;
}

static void Pt_gettxt(Widget W, PtInfo *data, caddr_t b)
{char   msg[80],*str;
 ErsHorizon *horizon;
 int    n,maxl,hid;
 long   lv,wlv,nchars;
 float  fval;
 double dv;

  if(XtClass(W) == xmTextWidgetClass)
   {str = XmTextGetString(W); }
  else return;


  if(strlen(str) == 0)  strcpy(msg,"0");
  else strcpy(msg,str);
  if(strlen(msg) >= 16)  msg[15]='\0';

  if (W == data->text)
   { fval =  (float) atof(msg);
     P_GetUser(data->point) = fval;
     sprintf(msg,"%g",fval);
     XmTextSetString(data->text, msg);
   }
  else if(W == data->idtext)
   { hid = (int) atoi(msg);
     horizon = ErsHorizonGetFromPoint(data->pr,data->point);
     ErsHorizonChangeHID(data->pr,  horizon, hid);
   }
     
  XtFree(str);
}
