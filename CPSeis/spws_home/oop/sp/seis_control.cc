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
#include "sp/seis_loc_out.hh"
#include "sp/seis_scan.hh"
#include "sp/seis_movie.hh"
#include "sp/seis_control.hh"
#include "sp/seis_inform.hh"
#include "sp/seis_zoomer.hh"
#include <Xm/Xm.h>
#include <Xm/RowColumn.h>
#include <Xm/Separator.h>
#include <Xm/PushB.h>
#include <Xm/Text.h>
#include <stdio.h>


class SeisControlPushBox : public SLPushBox {
    protected:
           virtual void  pushAction( long ident);
           SeisControl   *_contrl;
    public:
           SeisControlPushBox( const Widget       p,
                               char               *name,
                               const HelpCtx      hctx,
                               const SLPushAry    pushary,
                               const unsigned int arycnt,
                               SeisControl        *contrl) :
           SLPushBox(p,name,hctx,pushary,arycnt), _contrl(contrl) {}

};

enum Buttons { ZABORT= 9998899, OTHERS=1000 };

static SLPush pushes[]  = {
 { "zoom_abort", ZABORT },
};


static String  defres[]= {
    "*zoom_abort.background:   red",
    "*zoom_abort.labelString:  Abort Zoom",
    "*rc.orientation:          HORIZONTAL",
    "*rc.packing:              PACK_COLUMN",
    "*rc.numColumns:           1",
    NULL };





SeisControl::SeisControl( Widget   p,
                          char     *name,
                          SeisPlot *sp,
                          HelpCtx  hctx,  
                          Boolean  doscan,
                          Boolean  domovie)
           : SLForm(p, name, hctx), SeisInform(sp),
             _altPushAction(NULL), _altPushData(NULL),
             _loc(NULL), _scan(NULL), _movie(NULL),
             _alternate(NULL) 

{ 

  setFallbackResources( (const char**)defres);

  _loc= new SeisLocOut( topWidget(), "xyloc", sp, hctx); 
  if (doscan)  {
       _scan= new SeisScan( topWidget(), "scan", sp, hctx); 
       _scan->unmanage();
  }
  if (domovie) {
       _movie= new SeisMovie( topWidget(), "movie", sp, hctx); 
       _movie->unmanage();
  }

  Widget sep= XtVaCreateManagedWidget( "sep", xmSeparatorWidgetClass,
                                     topWidget(), 
                            XmNtopAttachment,   XmATTACH_FORM,
                            XmNrightAttachment, XmATTACH_FORM,
                            XmNleftAttachment,  XmATTACH_FORM,
                            XmNrightOffset,     5,
                            XmNleftOffset,      5, NULL );

  XtVaSetValues( _loc->W(), XmNtopAttachment,    XmATTACH_WIDGET, 
                            XmNtopWidget,        sep, 
                            XmNtopOffset,        7, 
                            XmNrightAttachment,  XmATTACH_FORM,
                            XmNrightOffset,      4,
                            NULL );

  if (domovie)
       XtVaSetValues( _movie->W(), XmNbottomAttachment, XmATTACH_FORM,
                                   XmNleftAttachment,   XmATTACH_FORM,
                                   XmNbottomOffset,     2,
                                   XmNleftOffset,       2,
                                   NULL );

  if (domovie)
       XtVaSetValues( _scan->W(),  XmNbottomAttachment, XmATTACH_FORM,
                                   XmNleftAttachment,   XmATTACH_FORM,
                                   XmNbottomOffset,     5,
                                   XmNleftOffset,       2,
                                   NULL );

  if (_alternate)
       XtVaSetValues( _alternate->W(),  XmNbottomAttachment, XmATTACH_FORM,
                                        XmNleftAttachment,   XmATTACH_FORM,
                                        XmNbottomOffset,     5,
                                        XmNleftOffset,       2,
                                        NULL );

  _push_box= new SeisControlPushBox( topWidget(), "rc", hctx,  
                                     pushes, XtNumber(pushes), this );
  XtVaSetValues( _push_box->topWidget(),
                 XmNrightAttachment,  XmATTACH_WIDGET,
                 XmNrightWidget,      _loc->W(),
                 //                 XmNbottomAttachment, XmATTACH_FORM,
                 //                 XmNbottomOffset,     2,
                 XmNbottomAttachment, XmATTACH_NONE,
                 XmNtopAttachment,    XmATTACH_OPPOSITE_WIDGET,
                 XmNtopWidget,        _loc->W(), NULL);

  
  XtUnmanageChild( _push_box->pushW(ZABORT) );

}

SeisControl::~SeisControl() 
{ 
  delete _loc; 
  if (_scan) delete _scan; 
  if (_movie) delete _movie;
  delete _push_box;
}


void SeisControl::setAlternate(SLDelay *alt)
{
   if (_alternate)
        XtVaSetValues( _alternate->W(),  XmNbottomAttachment, XmATTACH_NONE,
                                         XmNtopAttachment,    XmATTACH_NONE,
                                         XmNleftAttachment,   XmATTACH_NONE,
                                         XmNbottomOffset,     5,
                                         XmNleftOffset,       2,
                                         NULL );
   _alternate= alt;
   XtVaSetValues( _alternate->W(),  XmNbottomAttachment, XmATTACH_FORM,
                                    XmNtopAttachment,    XmATTACH_FORM,
                                    XmNleftAttachment,   XmATTACH_FORM,
                                    XmNbottomOffset,     5,
                                    XmNleftOffset,       2,
                                    XmNtopOffset,        2,
                                    NULL );
  if (_movie) _movie->unmanage();
  if (_scan)  _scan->unmanage();
}


void SeisControl::addPush(char *name, int ident)
{
 _push_box->addButton(name,ident); 
}

void SeisControl::delPush(int ident)
{
 _push_box->delButton(ident); 
}

Boolean SeisControl::pushExist(int ident)
{
  return _push_box->pushExist(ident);
}

Widget SeisControl::getPush(int ident)
{
  return _push_box->pushW(ident);
}

void SeisControlPushBox::pushAction( long ident)
{
  switch (ident) {
       case ZABORT: if(_contrl->_zoomer)
                        _contrl->_zoomer->zoomAbort(); break;
       default:     if (_contrl->_altPushAction) 
                         _contrl->_altPushAction(_contrl->_altPushData,ident);
                      _contrl->callNotifyComplex((int)ident);
                    break;
  }  // End Switch
}

void SeisControl::newPlot(SeisPlot *)
{

  Boolean one_is_movie= False;
  SeisPlot *q;
  void *x;
  for(q= top(&x); (q); q= next(&x)) {
      if (q->movie() && q->isPlotDisplayed() && q->isCurrentInWindow()) 
                       one_is_movie = True;
  }
  if (one_is_movie && _movie) {
        _movie->manage();
        if (_scan) XtVaSetValues( _scan->W(),  
                              XmNbottomAttachment, XmATTACH_FORM,
                              XmNleftAttachment,   XmATTACH_WIDGET,
                              XmNleftWidget,       _movie->W(),
                              XmNbottomOffset,     2,
                              XmNleftOffset,       5,
                              NULL );
  }
  else {
        if (_movie) _movie->unmanage();
        if (_scan) XtVaSetValues( _scan->W(),  
                              XmNbottomAttachment, XmATTACH_FORM,
                              XmNleftAttachment,   XmATTACH_FORM,
                              XmNbottomOffset,     2,
                              XmNleftOffset,       5,
                              NULL ); 
  }
  if (_scan) _scan->manage();

  XtUnmanageChild( _push_box->pushW(ZABORT) );
}

void SeisControl::noPlotDisplayed(SeisPlot *)
{
  SeisPlot *q;
  void *x;
  Boolean all_unmanaged= True;

  for(q= top(&x); ((q)&&(all_unmanaged)); q= next(&x)) {
      if (all_unmanaged) 
            all_unmanaged= !(q->isPlotDisplayed() && q->isCurrentInWindow());
  }
  if (all_unmanaged) {
      if (_scan)  _scan->unmanage();
      if (_movie) _movie->unmanage();
  }
}


void SeisControl::preZoom(SeisPlot *, SeisZoomer *zoomer, 
                            SeisPlot::ZoomDir dir)
{
 if ((dir == SeisPlot::Up) || (dir == SeisPlot::UpSeparateWin)) {
     _zoomer= zoomer;
     XtManageChild( _push_box->pushW(ZABORT) );
 }
}


void SeisControl::zoomInProgress(SeisPlot *, SeisPlot::ZoomDir dir)
{
 if ((dir == SeisPlot::Up) || (dir == SeisPlot::UpSeparateWin)) {
     XtUnmanageChild( _push_box->pushW(ZABORT) );
 }
}


void SeisControl::postZoom(SeisPlot *, SeisPlot::ZoomDir dir)
{
 if (dir == SeisPlot::Up || dir == SeisPlot::Abort || 
     dir == SeisPlot::UpSeparateWin) {
         XtUnmanageChild( _push_box->pushW(ZABORT) );
         _zoomer= NULL;
 }
}


void SeisControl::newSeisPlotCreatedInWindow(SeisPlot *newsp)
{
  addSeisPlot(newsp);
}

void SeisControl::notCurrentInWindow(SeisPlot *oldsp)
{
  SeisPlot *sp= oldsp->currentSPInWindow();
  newPlot(sp);
}



void SeisControl::addControl(SeisPlot *sp)
{
  if (_movie) _movie->addControl(sp);
  if (_scan) _scan->addControl(sp);
  _loc->addControl(sp);
  addSeisPlot(sp);
}

void SeisControl::addXYControlOnly(SeisPlot *sp)
{
  _loc->addControl(sp); 
}

void SeisControl::removeControl(SeisPlot *sp)
{
  if (_movie) _movie->addControl(sp);
  if (_scan) _scan->removeControl(sp);
  _loc->removeControl(sp);
}


void SeisControl::ifTieShowTime(Boolean doit) 
{
  _loc->ifTieShowTime(doit); 
}

Widget SeisControl::getPushButton(int which)
{
  return _push_box->pushW(which);
}
