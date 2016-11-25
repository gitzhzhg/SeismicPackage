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
#include <X11/Xlib.h>
#include <X11/Xutil.h>
#include <X11/Intrinsic.h>
#include <Xm/Xm.h>
#include <X11/Shell.h>
#include <Xm/Label.h>
#include <Xm/PushB.h>
#include <Xm/Text.h>
#include <Xm/Form.h>
#include <Xm/List.h>
#include "file_choice.h"
#include <wproc.h>
#include "sl/sl_def_pop.hh"
#include "sl/sl_radio_box.hh"
#include "sl/sl_tog_box.hh"
#include "sl/sl_text_box.hh"
#include "sl/sl_scale.hh"
#include "sl/sl_app.hh"
#include "sl/sl_form.hh"
#include "sl/sl_pull_pop.hh"
#include "sl/error_handler.hh"
#include "sl/sl_option_menu.hh"
#include "sl/psuedo_widget.hh"



static void *context;
static struct Data  *cbyt_data;
static struct GLBL  *G;

extern "C" {

extern Widget pickfile();


void cb (Widget w,
         struct HELPCTX *helpctx,
         XmAnyCallbackStruct *CBdata );

void changeit (Widget w,
               struct HELPCTX *helpctx,
               XmAnyCallbackStruct *CBdata );

void fcb (Widget w,
         struct HELPCTX                    *helpctx,
         wprocFileChoiceSuccCallbackStruct *CBdata );
}

void ack1( void *data, long ident);

long t1, t2, t3; float t4, t5;

enum{ T1, T2, T3, T4, T5};

static SLText texts[]  = {
               { "t1", "range:1 9",                &t1, SLType_int,   T1 },
               { "t2", "range:10 30,default:11",   &t2, SLType_int,   T2 },
               { "t3", NULL,                       &t3, SLType_int,   T3 },
               { "t4", "range:-2.0 2.0,default:0.00", &t4, SLType_float, T4 },
               { "t5", "range:-10.0 1.0,default:0.0", &t5, SLType_float, T5 },
                };



static SLPush pushes[] = {
         { "p1",  T1 },
         { "p2",  T2 },
         { "p3",  T3 },
         { "p4",  T4 },
};

static SLRadio rads[]  = {
         { "test1",       T1 },
         { "test2",       T2 },
         { "test3",       T3 },
         { "test4",       T4 },
      };

static SLTog togs[]  = {
         { "tog1",    NULL, T1 },
         { "tog2",    NULL, T2 },
       };


static Widget toplevel, form, push, text, file, in, togrc, cs, 
              same1, same2, num;
static SLFormPop *formpop;
static SLOptionMenu *opm;
static SLTextBox *textbox;
//static SLArrowScale *as;
static SLTogBox *rb;
static SLScaleDrag *scl;
static SLApp *application;
static SLPullPop *p1;
static SLPullPop *cas;
static SLForm *slform;
static Display *dpy;

main( int argc, char *argv[] )


{

 int n;
 Arg arglist[5];
 long flags;
 static  char str[50];
 static void  *helpdata=NULL;
 struct HELPCTX *helpctx;
 Widget w;
 SLPullPop *c1;



   /*
    * initialize Xt and create toplevel widget
    */
/*
   n=0;
   XtSetArg (arglist[n], XtNallowShellResize, TRUE) ; n++;
   toplevel= XtInitialize( "tcxx", "Tcxx", arglist, n, &argc, argv );

*/

   application= new SLApp( "tcxx", "Tcxx", argc, argv);
   dpy= XtDisplay(application->W());

   p1= new SLPullPop("File", NULL, application);
   p1->addPush("quit", 1);
   p1->addPush("another ", 2);
   cas= new SLPullPop("File", p1);
   cas->addPush("cas 1", 1);
   cas->addPush("cas 2", 2);
   p1->addPush("formpop ", 3);
   p1->addPush("formpop_private ", 4);
   p1->setAltPushAction(ack1, NULL);

   //form= XtCreateManagedWidget( "form", xmFormWidgetClass, 
   //                             application->mainWindow(), NULL, 0);
   slform= new SLForm(application->mainWindow(), "test", NULL, False, False);
   slform->make();
   form= slform->W();
   application->setWorkArea(slform->W());


   push= XtCreateManagedWidget("push2", xmPushButtonWidgetClass, form, NULL, 0);
   w= XtCreateManagedWidget("push", xmListWidgetClass, form, NULL, 0);
   XtAddCallback( push, XmNactivateCallback, (XtCallbackProc)cb, 
                      (XtPointer)NULL);

   opm= new SLOptionMenu(form, "opm", NULL, pushes, XtNumber(pushes), NULL);

   textbox= new SLTextBox( form, "textbox", NULL, texts,  XtNumber(texts), 
                     False, 1, False, False );
   rb= new SLTogBox( form, "rb", NULL, togs,  XtNumber(togs), 
                     True, False, False );
   formpop= new SLFPopSep( form, "formpop", FP_DOALL, NULL, True, 
                           False, True, 260);
   p1->addPushUp("delay make ", formpop);
   c1= new SLPullPop(formpop, "cascade");
   c1->addPush("test1", 1);
   c1->addPush("test2", 2);
   //----------





   //----------

   scl=     new SLScaleDrag( form, "scl", NULL, NULL, False);

   printf("tog1= %d, tog2= %d\n", rb->IsSelected(T1),rb->IsSelected(T2));
   printf("t1= %d, t2= %d, t3= %d, t4= %f, t5= %f\n", t1, t2, t3 ,t4, t5);
   printf("scale value= %d\n", scl->getScaleValue() );


/*
   PsuedoWidget pw(application->mainWindow());
   XmStringTable tab;
   tab= pw.itemsDef();
*/
   


   application->realize();
   application->loop();
}


void ack1( void *data, long ident)
{
  if (ident == 1) exit(0);
  else if (ident == 2) {
          SLApp *newapp;
          SLPullPop *newp;
          newapp= new SLApp(dpy, "new", "New",1);
          newp= new SLPullPop("File", NULL, newapp);
          newp->addPush("quit", 1);
          newp->addPush("another ", 2);
          newp->setAltPushAction(ack1, NULL);
          newapp->realize();
  } // end if
  else if (ident == 3) {
          static SLFPopSep *fp= NULL;
          if (!fp) {
              fp= new SLFPopSep( form, "other", FP_DOALL, NULL, True, 
                                  True, True, 0, MayNotIcon, UseResource);
          }
          fp->makeAndManage();
  } // end if
  else if (ident == 4) {
          static SLFPopSep *fpp= NULL;
          if (!fpp) {
              fpp= new SLFPopSep( form, "other_private", 
                                  FP_DOALL, NULL, True, 
                                  True, True, 200, MayIcon, UseResource);
          }
          fpp->makeAndManage();
  } // end if


}
void ack( void *data)
{
 puts("got an ack");
}

void cb (Widget w,
         struct HELPCTX *helpctx,
         XmAnyCallbackStruct *CBdata )

{
/*
 static morebutton= 10;
 int t1;
 SLTextBox *tmp;
 ErrorHandler eh(w);
 char aname[50];
 rb->make(form);
 textbox->make(form);
 scl->make(form);
 printf("rb made= %d\n", rb->made() );
 //t1= textbox[T1];
 printf("\n\nt1= \n", t1);
 textbox[(int)T2]= t1;
 t1= textbox[(int)1];
 printf("printing []=%d\n", textbox[(int)1]);
 eh.setAltErrorAck(ack, NULL);
 eh.deliverError("bad stuff");
 opm->setButton(T3); 
 sprintf(aname, "TMORE%d", morebutton);
 opm->delButton(morebutton-1 );
 opm->addButton(aname, morebutton++ );
*/
 formpop->makeAndManage();
}
