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
// Author Michael L. Sherrill 10/93
// Installs ovjd picking class and creates menu to be used with SeisPlot
#include <Xm/Label.h>
#include <Xm/Frame.h>
#include <Xm/Separator.h>
#include <stdio.h>
#include "vu/seis_ovjd_pop.hh"
#include "vu/seis_ovjd.hh"
#include "sp/seis_plot.hh"
#include "sl/sl_push_box.hh"
#include "sl/sl_text_box.hh"
#include <math.h>


#define    CHANGE_SOURCEX   0.0F
#define    CHANGE_SOURCEY   0.0F
#define    CHANGE_RECEIVERX 0.0F
#define    CHANGE_RECEIVERY 0.0F
#define    CHANGE_VELOCITY  5000L


static String defres[]= {
    "*sop_popup.title:               Color Processing Menu",
    ".height:                        500",
    ".width:                         375",
    "*ovjdvelocitylab.labelString:   Offset Velocity",
    "*modifyhdrlab.labelString:      Offset Headers",
    "*changelab.labelString:         Change( + - )",
    "*vellab.labelString:            Velocity",
    "*hdrvallab.labelString:         Trace ID:                 ",
    "*sourcexlab.labelString:        Source X:                 ",
    "*sourceylab.labelString:        Source Y:                 ",
    "*receiverxlab.labelString:      Receiver X:                 ",
    "*receiverylab.labelString:      Receiver Y:                 ",
    "*header_box.leftOffset:         225",
    "*header_box.topOffset:          220",
    "*velocity_box.leftOffset:       175",
    "*velocity_box.topOffset:        60",
    "*select_trace.labelString:      Select Trace",
    "*original_headers.labelString:  Original Headers",
    "*sep_1.topPosition:             25",
    "*sep_1.leftOffset:              10",
    "*sep_1.rightOffset:             10",
    "*sep_1.bottomOffset:            10",
    "*ovjdvelocitylab.fontList:      -*-*-bold-r-*-*-*-180-*-*-p-*-*-*",
    "*modifyhdrlab.fontList:         -*-*-bold-r-*-*-*-180-*-*-p-*-*-*",
    "*change_source_x.value:         0.0",
    "*change_source_y.value:         0.0",
    "*change_receiver_x.value:       0.0",
    "*change_receiver_y.value:       0.0",
    NULL};


enum {CHANGE_SOURCE_X, CHANGE_SOURCE_Y,CHANGE_RECEIVER_X, CHANGE_RECEIVER_Y};
enum {VELOCITY};
enum {SELECT_TRACE};
enum {ORIGINAL_HEADERS};



static char *picking_mode = "Mode: Pick OVJD Trace";
static const char * const help_token = "OVJD";
static char *ovjdhelp= "mouse*OVJD:  BTN#1: Select location to modify, \
BTN#2: None, BTN#3: None";


#define ParentClass SLFPopSep


SeisOvjdPop::SeisOvjdPop( Widget            p,
                          char              *name,
                          SeisOvjd          *so,
                          HelpCtx           hctx) 

       : SLFPopSep(p,name,FP_DOALL,hctx,True,False),
                            _plot_on_doaciton(False), _new_file(True),
                            _use_file_defaults(False), _new_appdefaults(True),
                            _so(so), 
                            _new_velocity(so->getVelocity()),
                            _first_time(True)

{

  static SLText textb1[]  = {
    {"change_source_x", NULL, NULL,   SLType_float, CHANGE_SOURCE_X},
    {"change_source_y", NULL, NULL,   SLType_float, CHANGE_SOURCE_Y},
    {"change_receiver_x", NULL, NULL, SLType_float, CHANGE_RECEIVER_X},
    {"change_receiver_y", NULL, NULL, SLType_float, CHANGE_RECEIVER_Y},
  };
  textb1[0].target= &_change_source_x;
  textb1[1].target= &_change_source_y;
  textb1[2].target= &_change_receiver_x;
  textb1[3].target= &_change_receiver_y;


  static SLText textb2[]  = {
    {"new_velocity", NULL, NULL, SLType_float, VELOCITY},
  };
  textb2[0].target= &_new_velocity;

  static SLPush pushb[]  = {
    { "select_trace",   SELECT_TRACE },
  };

  static SLPush pushb2[]  = {
    { "original_headers",   ORIGINAL_HEADERS },
  };

   setDefaultResources( p, name, defres);

   
   _header_box= new SLTextBox( this, "header_box", getHelpCtx(),
                              textb1,XtNumber(textb1), False, 1, False, False );

   _velocity_box= new SLTextBox( this, "velocity_box", getHelpCtx(),
                              textb2,XtNumber(textb2), False, 1, False, False );


   _select_trace= new SLPushBox(this,"select_trace",NULL, pushb, 
                                XtNumber(pushb), False,False,False );

   _select_trace->setAltPushAction( (SLPushButtonfunc)selected, this);

   _original_headers= new SLPushBox(this,"original_headers",NULL, pushb2,
                                XtNumber(pushb2), False,False,False );

   _original_headers->setAltPushAction( (SLPushButtonfunc)original, this);


   _trace_selected = False;
   _tindex = 0;
   _so->_sop = this;
   _pick_ovjd = NULL;

   DoAction();     
}



//Following done to allow customization of widget creation
SeisOvjdPop::SeisOvjdPop( Widget            p,
                          char              *name,
                          SeisOvjd          *so,
                          HelpCtx           hctx,
                          Boolean           /*custom_widgets*/) 

       : SLFPopSep(p,name,FP_DOALL,hctx,True,False),
                            _plot_on_doaciton(False), _new_file(True),
                            _use_file_defaults(False), _new_appdefaults(True),
                            _so(so), 
                            _new_velocity(so->getVelocity()),
                            _first_time(True)
{

}



SeisOvjdPop::~SeisOvjdPop()
{

}


Widget SeisOvjdPop::make(Widget p)
{

   if ( made() ) return topWidget();
   SLFPopSep::make(p);


   XtVaSetValues( _header_box->W(), XmNleftAttachment, XmATTACH_POSITION,
                                  XmNtopAttachment,  XmATTACH_POSITION, NULL);

 
   XtVaSetValues( _velocity_box->W(), XmNleftAttachment, XmATTACH_POSITION,
                                    XmNtopAttachment,  XmATTACH_POSITION, NULL);


   XtVaSetValues( _select_trace->W(), XmNtopAttachment, XmATTACH_FORM,
                                   XmNtopOffset,        370,
                                   XmNleftAttachment,   XmATTACH_FORM, 
                                   XmNleftOffset,       50, NULL);

   XtVaSetValues( _original_headers->W(), XmNtopAttachment, XmATTACH_FORM,
                                   XmNtopOffset,        370,
                                   XmNleftAttachment,   XmATTACH_FORM,
                                   XmNleftOffset,       175, NULL);


   _hdrvallab = XtVaCreateManagedWidget("hdrvallab",xmLabelWidgetClass,
                              topWidget(), 
                              XmNleftAttachment,   XmATTACH_FORM,
                              XmNleftOffset,       25,
                              XmNtopAttachment,    XmATTACH_FORM,
                              XmNtopOffset,        190, NULL);

   Widget changelab = XtVaCreateManagedWidget("changelab",xmLabelWidgetClass,
                              topWidget(), 
                              XmNleftAttachment,   XmATTACH_FORM,
                              XmNleftOffset,       220,
                              XmNtopAttachment,    XmATTACH_FORM,
                              XmNtopOffset,        190, NULL);

   Widget vellab = XtVaCreateManagedWidget("vellab",xmLabelWidgetClass,
                              topWidget(), 
                              XmNleftAttachment,   XmATTACH_FORM,
                              XmNleftOffset,       108,
                              XmNtopAttachment,    XmATTACH_FORM,
                              XmNtopOffset,        64, NULL);

   _sourcexlab= XtVaCreateManagedWidget("sourcexlab",xmLabelWidgetClass,
                              topWidget(), 
                              XmNleftAttachment,   XmATTACH_FORM,
                              XmNleftOffset,       25,
                              XmNtopAttachment,    XmATTACH_FORM,
                              XmNtopOffset,        228, NULL);


   _sourceylab= XtVaCreateManagedWidget("sourceylab",xmLabelWidgetClass,
                             topWidget(), 
                             XmNtopAttachment,     XmATTACH_WIDGET,
                             XmNtopWidget,         _sourcexlab, 
                             XmNleftAttachment,    XmATTACH_FORM,
                             XmNleftOffset,        25,
                             XmNtopOffset,         7, NULL);


   _receiverxlab=XtVaCreateManagedWidget("receiverxlab",
                             xmLabelWidgetClass,   topWidget(),
                             XmNtopAttachment,     XmATTACH_WIDGET,
                             XmNtopWidget,         _sourceylab,
                             XmNleftAttachment,    XmATTACH_FORM,
                             XmNleftOffset,        25,
                             XmNtopOffset,         7, NULL);

   _receiverylab= XtVaCreateManagedWidget("receiverylab",
                             xmLabelWidgetClass,   topWidget(),
                             XmNtopAttachment,     XmATTACH_WIDGET,
                             XmNtopWidget,         _receiverxlab, 
                             XmNleftAttachment,    XmATTACH_FORM,
                             XmNleftOffset,        25,
                             XmNtopOffset,         7, NULL);
       

   Widget modifyhdrlab= XtVaCreateManagedWidget(
                            "modifyhdrlab",     xmLabelWidgetClass, topWidget(),
                            XmNtopAttachment,    XmATTACH_FORM,
                            XmNtopOffset,        150,
                            XmNleftAttachment,   XmATTACH_FORM,
                            XmNleftOffset,       100, NULL );

   Widget ovjdvelocitylab= XtVaCreateManagedWidget(
                            "ovjdvelocitylab",  xmLabelWidgetClass, topWidget(),
                            XmNleftAttachment,   XmATTACH_FORM,
                            XmNleftOffset,       100, 
                            XmNtopAttachment,    XmATTACH_FORM,
                            XmNtopOffset,        10,NULL );

   Widget sep_1 = XtVaCreateManagedWidget("sep_1", xmSeparatorWidgetClass,
                             topWidget(),
                             XmNbottomAttachment, XmATTACH_WIDGET,
                             XmNbottomWidget,     modifyhdrlab,
                             XmNleftAttachment,   XmATTACH_FORM,
                             XmNrightAttachment,  XmATTACH_FORM, NULL);

   return topWidget();

}


Boolean SeisOvjdPop::ValidInput()
{
 Boolean stat;


  if (made())
     stat= ParentClass::ValidInput();
  else
     stat= True;

  return (stat); 
}


void SeisOvjdPop::UndoInput()
{
  if(_so->getCurrentSP()->imageIsDisplayed())_so->makeInvisible();
  SLFormPop::UndoInput();
  _so->getCurrentSP()->redraw();
  _trace_selected = False;
}

void SeisOvjdPop::DoAction()
{
 ParentClass::DoAction();
 Boolean replot = False;

  if(!_so->getCurrentSP()->imageIsDisplayed())return; 

  if(_change_source_x   != _last_source_x   || 
     _change_source_y   != _last_source_y   ||
     _change_receiver_x != _last_receiver_x || 
     _change_receiver_y != _last_receiver_y)  replot = True;

  if(_so->getVelocity() != _new_velocity) 
     {
     _so->changeVelocity(_new_velocity);
     replot = True;
     }

  if(!_first_time)
     {
     switch (whichButton())
        {
        case FP_OK:
        case FP_APPLY:
                   if(_so->getCurrentSP()->imageIsDisplayed())
                      {
                      _so->makeVisible();
                      replot = True;
		      }
                   break;
        }
     }


  if(replot) 
     {
     changeHeaders();
     _so->getCurrentSP()->redraw();
     _so->post();
     }

}




void SeisOvjdPop::manage()
{

  if(_first_time) 
     {
     set_label( _hdrvallab,    "Trace ID:                 " );
     set_label( _sourcexlab,   "Source X:                 " );
     set_label( _sourceylab,   "Source Y:                 " );
     set_label( _receiverxlab, "Receiver X:                 " );
     set_label( _receiverylab, "Receiver Y:                 " );
     _header_box->SetValue(CHANGE_SOURCE_X,   CHANGE_SOURCEX);
     _header_box->SetValue(CHANGE_SOURCE_Y,   CHANGE_SOURCEY);
     _header_box->SetValue(CHANGE_RECEIVER_X, CHANGE_RECEIVERX);
     _header_box->SetValue(CHANGE_RECEIVER_Y, CHANGE_RECEIVERY);
     _velocity_box->SetValue(VELOCITY, _so->getVelocity());
     _last_source_x   = _change_source_x;
     _last_source_y   = _change_source_y;
     _last_receiver_x = _change_receiver_x;
     _last_receiver_y = _change_receiver_y;
     }

  XtManageChild(topWidget()); 

  _first_time = False;

}

void SeisOvjdPop::reloadDefaults(Boolean)
{
  SLFPopSep::reloadDefaults();

  _header_box->reloadDefaults();
  _velocity_box->reloadDefaults();
  DoAction();
}


void SeisOvjdPop::reloadSystemDefaults(Boolean do_method)
{
  SLFPopSep::reloadSystemDefaults(do_method);
  _header_box->SetValue(CHANGE_SOURCE_X, CHANGE_SOURCEX);
  _header_box->SetValue(CHANGE_SOURCE_Y, CHANGE_SOURCEY);
  _header_box->SetValue(CHANGE_RECEIVER_X, CHANGE_RECEIVERX);   
  _header_box->SetValue(CHANGE_RECEIVER_Y, CHANGE_RECEIVERY);
  _velocity_box->SetValue(VELOCITY, CHANGE_VELOCITY);
  _last_source_x   = _change_source_x;
  _last_source_y   = _change_source_y;
  _last_receiver_x = _change_receiver_x;
  _last_receiver_y = _change_receiver_y;
  _trace_selected  = False;
  DoAction();
}





void SeisOvjdPop::original( void *data, long /*button*/)
{
 SeisOvjdPop *obj = (SeisOvjdPop *)data;

  obj->_header_box->SetValue(CHANGE_SOURCE_X, CHANGE_SOURCEX);
  obj->_header_box->SetValue(CHANGE_SOURCE_Y, CHANGE_SOURCEY);
  obj->_header_box->SetValue(CHANGE_RECEIVER_X, CHANGE_RECEIVERX);
  obj->_header_box->SetValue(CHANGE_RECEIVER_Y, CHANGE_RECEIVERY);
  obj->_last_source_x   = obj->_change_source_x;
  obj->_last_source_y   = obj->_change_source_y;
  obj->_last_receiver_x = obj->_change_receiver_x;
  obj->_last_receiver_y = obj->_change_receiver_y;
  obj->_so->loadHeaders();
  obj->_so->getCurrentSP()->redraw();
  obj->_so->post();
  obj->_so->_headers_modified = False;
}

void SeisOvjdPop::newData()
{


  if(!_so->getCurrentSP()->imageIsDisplayed())return;
  if(_first_time) return;

  _header_box->SetValue(CHANGE_SOURCE_X, CHANGE_SOURCEX);
  _header_box->SetValue(CHANGE_SOURCE_Y, CHANGE_SOURCEY);
  _header_box->SetValue(CHANGE_RECEIVER_X, CHANGE_RECEIVERX);
  _header_box->SetValue(CHANGE_RECEIVER_Y, CHANGE_RECEIVERY);
  _last_source_x   = _change_source_x;
  _last_source_y   = _change_source_y;
  _last_receiver_x = _change_receiver_x;
  _last_receiver_y = _change_receiver_y;
  _tindex = 0;
  set_label( _hdrvallab,  "Trace ID:                 " );
  set_label( _sourcexlab, "Source X:                 " );
  set_label( _sourceylab, "Source Y:                 " );
  set_label( _receiverxlab, "Receiver X:                 " );
  set_label( _receiverylab, "Receiver Y:                 " );
  _so->_headers_modified = False;
  _trace_selected = False;
 
}



void SeisOvjdPop::changeHeaders()
{
 Boolean *trace_ary;
 const float *sphd = _so->getCurrentSP()->headers();
 float sx,rx,sy,ry;
 float sourcex,sourcey;
 float receiverx,receivery;
 double squareone, squaretwo;
 long i;

  if(!_trace_selected) 
    {
    _so->loadHeaders();
    return;
    }

  trace_ary = (Boolean *)calloc(1, (unsigned int)(_so->getCurrentSP()->plottedNplt() 
                                   * sizeof(Boolean)));
  if(trace_ary == NULL) {printf("handle trace_ary error here\n"); return;}

  _so->loadHeaders();
  sourcex   = _so->_hd[_tindex+SXHDR];
  sourcey   = _so->_hd[_tindex+SYHDR];
  receiverx = _so->_hd[_tindex+RXHDR];
  receivery = _so->_hd[_tindex+RYHDR];

  for(i = 0; i < _so->getCurrentSP()->plottedNplt(); i++)
     {
     if(sphd[i*_so->getCurrentSP()->numHeaders()+SXHDR] == sourcex) 
       _so->_hd[i*_so->getCurrentSP()->numHeaders()+SXHDR] =
                   sphd[i*_so->getCurrentSP()->numHeaders()+SXHDR] +
                   _change_source_x;
     if(sphd[i*_so->getCurrentSP()->numHeaders()+SYHDR] == sourcey)
       _so->_hd[i*_so->getCurrentSP()->numHeaders()+SYHDR] = 
                   sphd[i*_so->getCurrentSP()->numHeaders()+SYHDR] +
                   _change_source_y;
     if(sphd[i*_so->getCurrentSP()->numHeaders()+RXHDR] == receiverx)
       _so->_hd[i*_so->getCurrentSP()->numHeaders()+RXHDR] =
                   sphd[i*_so->getCurrentSP()->numHeaders()+RXHDR] +
                   _change_receiver_x;
     if(sphd[i*_so->getCurrentSP()->numHeaders()+RYHDR] == receivery)
       _so->_hd[i*_so->getCurrentSP()->numHeaders()+RYHDR] = 
                   sphd[i*_so->getCurrentSP()->numHeaders()+RYHDR] +
                   _change_receiver_y;
     }

  for(i = 0; i < _so->getCurrentSP()->plottedNplt(); i++) 
     {
     sx = _so->_hd[i*_so->getCurrentSP()->numHeaders()+SXHDR];
     sy = _so->_hd[i*_so->getCurrentSP()->numHeaders()+SYHDR];
     rx = _so->_hd[i*_so->getCurrentSP()->numHeaders()+RXHDR];
     ry = _so->_hd[i*_so->getCurrentSP()->numHeaders()+RYHDR];
     squareone = ( sx - rx ) * ( sx - rx );
     squaretwo = ( sy - ry ) * ( sy - ry );
     _so->_hd[i*_so->getCurrentSP()->numHeaders()+OFFSETHDR] = 
                                               sqrt( (squareone + squaretwo) );
    }


  _so->_headers_modified = True;
  _last_source_x   = _change_source_x;
  _last_source_y   = _change_source_y;
  _last_receiver_x = _change_receiver_x;
  _last_receiver_y = _change_receiver_y;

 free(trace_ary);
}


void SeisOvjdPop::selected( void *data, long /*button*/)
{
  SeisOvjdPop *obj = (SeisOvjdPop *)data;
  if(obj->_pick_ovjd == NULL) 
     obj->_pick_ovjd = new PickOvjd(obj->_so->getCurrentSP(), obj);
}


//******************** Ovjd's PickBase ********************************

PickOvjd::PickOvjd (SeisPlot    *sp, 
                    SeisOvjdPop *sop)
                    : PickBase(sp, picking_mode, help_token, ovjdhelp,
                                   XC_tcross, allow, True), _sop(sop)
{

}

void PickOvjd::buttonAny(int ev_x1, int /*ev_x2*/, int /*ev_y1*/, 
                         int /*ev_y2*/, int button, Action action, 
                         Modifier /*modifier*/)
{
 long trace;
 char lstr[30];
 const float *sphd;


 if(!_sop->_so->getCurrentSP()->imageIsDisplayed())return;
 sphd = _sop->_so->getCurrentSP()->headers();
 long THDR = _sop->_so->getCurrentSP()->header1() - 1;


 if(button == 1 && action == press)
    {
    //trace = _sop->_so->getCurrentSP()->getTraceFromPixel(ev_x1);
    trace = (long)(_sop->_so->getCurrentSP()->xWC(ev_x1) + .5);
    if(trace < 1) trace = 1;
    if(trace > _sop->_so->getCurrentSP()->plottedNplt())
       trace = _sop->_so->getCurrentSP()->plottedNplt(); 
    _sop->_tindex = (trace-1) * _sop->_so->getCurrentSP()->numHeaders();
    _sop->_trace_selected = True;
    sprintf(lstr,"Trace ID: %10.3f",  sphd[_sop->_tindex+THDR]);
    set_label( _sop->_hdrvallab, lstr );
    sprintf(lstr,"Source X: %10.3f",sphd[_sop->_tindex+SXHDR]);
    set_label( _sop->_sourcexlab, lstr );
    sprintf(lstr,"Source Y: %10.3f",sphd[_sop->_tindex+SYHDR]);
    set_label( _sop->_sourceylab, lstr );
    sprintf(lstr,"Receiver X: %10.3f",sphd[_sop->_tindex+RXHDR]);
    set_label( _sop->_receiverxlab, lstr );
    sprintf(lstr,"Receiver Y: %10.3f",sphd[_sop->_tindex+RYHDR]);
    set_label( _sop->_receiverylab, lstr ); 
    _sop->_pick_ovjd = NULL;  
    delete this;
    }

}






