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
#include "cenv.h"
#include "wproc.h"
#include <X11/Xlib.h>
#include <X11/Xutil.h>
#include <X11/Intrinsic.h>
#include <X11/cursorfont.h>
#include <Xm/Xm.h>
#include <Xm/Form.h>
#include <Xm/BulletinB.h>

/* macro for determining pixels in an inch */
#define PixelsPerInch(dsp, scrn) (int) (((float) DisplayWidth(dsp, scrn)) / \
                                 (0.0394 * DisplayWidthMM(dsp, scrn)))




static void disbuttons( long               numb,
                        struct   Form_popup *fmp )


{

 Arg arglist[3];
 long n, i, j;
 Dimension wid;
 Position nx;
 long start, qsize;
 Dimension bw[4];
 Widget abut[4];
 Widget but[4];
 
 

 abut[0]= fmp->okb;
 abut[1]= fmp->appb;
 abut[2]= fmp->canb;
 abut[3]= fmp->helpb;

 n=0;
 XtSetArg( arglist[n], XmNwidth, &wid ); n++;
 XtGetValues( fmp->button_ctl, arglist, n);

 
 for(i=0, j=0; (i<4); i++) {
    if (abut[i]) {
         n=0;
         XtSetArg( arglist[n], XmNwidth, &bw[j] ); n++;
         XtGetValues( abut[i], arglist, n);
         but[j]= abut[i];
         j++;
    ENDif
 ENDloop

 
 qsize= wid/numb;
 for (i=0, start= 0; (i<numb); i++) {

      nx= ((qsize - bw[i] )/2) + start;
      start+= qsize;
      n=0;
      XtSetArg( arglist[n], XmNx, nx ); n++;
      XtSetValues( but[i], arglist, n);
      

 ENDloop
    

}


static void cntrl_resize( Widget             w,
                          struct  Form_popup *fmp,
                          XConfigureEvent    *ev )

{

 if (ev->type == ConfigureNotify) {

    disbuttons( fmp->numb, fmp); 

 ENDif
  
}

static void start_resize( Widget              w,
                          struct  Form_popup  *fmp,
                          XmAnyCallbackStruct *cbs )

{

    disbuttons( fmp->numb, fmp); 
    XtRemoveCallback( w, XmNmapCallback, 
                        (XtCallbackProc)start_resize, (XtPointer)fmp);
  
}


static void dodestroy( Widget              w,
                       struct  Form_popup  *fmp,
                       XmAnyCallbackStruct *cbs )

{
  free(fmp);
}



Widget cre_frm_pop( wunion              *wu,
                    Boolean             reduce_on_small,
                    struct   HELPCTX    *helpctx )

{
  struct   Form_popup *fmp= (struct   Form_popup *)wu;
  Widget tempw;
  wunion wig[4];
  Arg arglist[20];
  long n;
  Display *dpy;
  long screen;
  long dpi;
  Dimension wid, height;
  int min_wid, min_height;

  fmp= (struct Form_popup *)malloc( sizeof (struct Form_popup));

  memcpy( fmp, wu, sizeof (struct Form_popup) );


  dpy= XtDisplay(fmp->parent);
  screen= DefaultScreen(dpy);
  dpi= PixelsPerInch( dpy, screen);
  fmp->numb= 0;
   
  if (fmp->not_a_pop)
      fmp->form= XtCreateManagedWidget( fmp->form_name, xmFormWidgetClass,
                                         fmp->parent, NULL, 0);
  else
      fmp->form= XmCreateFormDialog( fmp->parent, fmp->form_name, NULL, 0);
  n=0;
  XtSetArg (arglist[n], XmNresizePolicy, XmRESIZE_NONE ) ; n++;
  XtSetValues(fmp->form, arglist, n);

  
  fmp->button_ctl= tempw= XtVaCreateManagedWidget( "button_control", 
                        xmBulletinBoardWidgetClass, fmp->form,
                        XmNmarginHeight,     0,
                        XmNmarginWidth,      0,
                        XmNtopAttachment,    XmATTACH_NONE,
                        XmNbottomAttachment, XmATTACH_FORM,
                        XmNrightAttachment,  XmATTACH_FORM,
                        XmNleftAttachment,   XmATTACH_FORM, NULL );

  if ( (dpi<80)AND(reduce_on_small)) {
      n=0;
      XtSetArg (arglist[n], XmNwidth, &wid ) ; n++;
      XtSetArg (arglist[n], XmNheight, &height ) ; n++;
      XtGetValues(fmp->form, arglist, n);
      height*= .85;
      wid*=   .8;
      n=0;
      XtSetArg (arglist[n], XmNwidth, wid ) ; n++;
      XtSetArg (arglist[n], XmNheight, height ) ; n++;
      XtSetValues(fmp->form, arglist, n);


      n=0;
      XtSetArg (arglist[n], XmNminWidth, &min_wid ) ; n++;
      XtSetArg (arglist[n], XmNminHeight, &min_height ) ; n++;
      XtGetValues(XtParent(fmp->form), arglist, n);
      if ( (min_wid<3000)AND(min_wid>100) ) {
            min_wid*=   .8;
            n=0;
            XtSetArg (arglist[n], XmNminWidth, min_wid ) ; n++;
            XtSetValues(XtParent(fmp->form), arglist, n);
      ENDif
      if ( (min_height<3000)AND(min_height>100) ) {
            min_height*= .85;
            n=0;
            XtSetArg (arglist[n], XmNminHeight, min_height ) ; n++;
            XtSetValues(XtParent(fmp->form), arglist, n);
      ENDif

  ENDif
  if (fmp->whichb & FP_DOOK) {
      set_PUSH(wig, "OK", fmp->active_cb, fmp->active_info, tempw, FP_OK );
      fmp->okb= create_push( &wig[FP_OK] );
      if (helpctx) add_HELP( fmp->okb, helper, helpctx);
      fmp->numb++;
  ENDif
  else
      fmp->okb= NULL;

  if (fmp->whichb & FP_DOAPPLY) {
      set_PUSH(wig, "Apply", fmp->active_cb,fmp->active_info,tempw, FP_APPLY );
      fmp->appb= create_push( &wig[FP_APPLY] );
      if (helpctx) add_HELP( fmp->appb, helper, helpctx);
      fmp->numb++;
  ENDif
  else
      fmp->appb= NULL;

  if (fmp->whichb & FP_DOCANCEL) {
      set_PUSH(wig,"Cancel", fmp->active_cb,fmp->active_info,tempw, FP_CANCEL );
      fmp->canb= create_push( &wig[FP_CANCEL] );
      if (helpctx) add_HELP( fmp->canb, helper, helpctx);
      fmp->numb++;
  ENDif
  else
      fmp->canb= NULL;

  if (fmp->whichb & FP_DOHELP) {
      set_PUSH(wig, "Help", fmp->active_cb, fmp->active_info, tempw, FP_HELP );
      fmp->helpb= create_push( &wig[FP_HELP] ); 
      if (helpctx) add_HELP( fmp->helpb, helper, helpctx);
      fmp->numb++;
  ENDif
  else
      fmp->helpb= NULL;

  XtAddEventHandler( tempw,  StructureNotifyMask, False, 
                     (XtEventHandler)cntrl_resize, (XtPointer)fmp );
  XtAddCallback( fmp->form, XmNmapCallback, 
                     (XtCallbackProc)start_resize, (XtPointer)fmp);
  XtAddCallback( fmp->button_ctl, XmNdestroyCallback, 
                     (XtCallbackProc)dodestroy, (XtPointer)fmp);

  memcpy( wu, fmp, sizeof (struct Form_popup) );
  return (fmp->form);

  
}
