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
//author Michael L. Sherrill 12/93
//menu to control the plotting of underlay data

#include "sp/seis_under_pop.hh"
#include "sp/seis_plot.hh"
#include "sp/seis_plot_under.hh"
#include "sp/seis_inform.hh"
#include "sp/trace_selector_pop.hh"
#include "sp/trace_selector_ndo_pop.hh"
#include "sp/seis_color.hh"
#include "sl/shell_stat_msg.hh"
#include "sl/sl_push_box.hh"
#include "sl/sl_text_box.hh"
#include "sl/sl_tog_box.hh"
#include "sl/sl_radio_box.hh"
#include "sl/sl_smart_form.hh"
#include "sl/slp_file.hh"


static String  defres[]= {
    "_popup.title:                   Underlay Plot",
    "*doumovie.labelString:          Movie - Number Frames:",
    "*mheaderL.labelString:          Match Header:",
    "*mheader.value:                 1",
    "*umovie_form_Frame.leftOffset:  4",
    NULL};

enum { UFRAMES, USFRAMES, DOUMOVIE, MHEADER };


class UnderInform : public SeisInform {
  private:
     SeisUnderPop *_under_pop;
  public:
     UnderInform( SeisUnderPop *under_pop, SeisPlot *sp) 
              : SeisInform(sp), _under_pop(under_pop) {};
     virtual void newPlot(SeisPlot *sp);
     virtual void noPlotDisplayed(SeisPlot *sp);
};



SeisUnderPop::SeisUnderPop (Widget p, char *name, HelpCtx hctx,
  SeisPlot *underlay_sp, SeisPlot *overlay_sp, Boolean allow_selector) :
  SeisSelect (p,name,hctx,underlay_sp,overlay_sp, allow_selector),
  _overlay_sp  (overlay_sp)
{
  static SLText umovie_text[]  = {
    { "uframes",        NULL,  NULL,       SLType_int,   UFRAMES },
  };
  umovie_text[0].target= &_uframes;

  static SLText header_text[]  = {
    { "mheader",  "range:1 99999999,default:1",  NULL,   SLType_int, MHEADER },
  };
  header_text[0].target= &_mheader;

  static SLTog mtogs[]  = {
    { "doumovie", NULL,   DOUMOVIE },
  };
  mtogs[0].target= &_doumovie;
  setDefaultResources( p, name, defres);
  _umovie_form= new SLForm(this,"umovie_form",hctx,True);
  _ifumovie= new SLTogBox(_umovie_form, "ifumovie", getHelpCtx(),
                          mtogs,XtNumber(mtogs));
  _umovie= new SLTextBox( _umovie_form,"umovie",hctx,
                          umovie_text,XtNumber(umovie_text), False);
  _headers= new SLTextBox( this,"headers",hctx,
                          header_text,XtNumber(header_text));

  _inform= new UnderInform(this, overlay_sp);
  addSeisPlot(overlay_sp);
}



SeisUnderPop::~SeisUnderPop()
{
  delete _umovie;
  delete _ifumovie;
  delete _umovie_form;
  delete _inform;
  delete _selector_pop;
  delete _selector_ndo_pop;
}



Widget SeisUnderPop::make(Widget p)
{
  if ( made() ) return topWidget();
  p= p ? p : wParent();
  ShellStatMsg  bld_info(p,"Building Underlay Popup...");
  SeisSelect::make(p);


  if (_allow_selector) {
    _selector_pop = new TraceSelectorPop(topWidget(), "Selector",
      getHelpCtx(), this);
    _selector_ndo_pop = new TraceSelectorNDoPop(topWidget(), "Selector",
      getHelpCtx(), this);
  }



  unmanageCT();
  unmanagePTYPE();
  unmanageScale();

  XtVaSetValues( _headers->W(), XmNtopAttachment,  XmATTACH_WIDGET,
                                XmNtopWidget,      _but->W(),
                                XmNleftAttachment, XmATTACH_OPPOSITE_WIDGET,
                                XmNleftWidget,     _but->W(), NULL );

  XtVaSetValues( _umovie_form->W(), XmNtopAttachment,    XmATTACH_WIDGET,
                                    XmNleftAttachment, XmATTACH_OPPOSITE_WIDGET,
                                    XmNbottomAttachment, XmATTACH_WIDGET,
                                    XmNrightAttachment,  XmATTACH_FORM,
                                    XmNrightOffset,      10,
                                    XmNtopOffset,        10,
                                    XmNbottomOffset,     10,
                                    XmNbottomWidget,     bottomSeparator(),
                                    XmNtopWidget,        _headers->W(),
                                    XmNleftWidget,       _headers->W(), NULL );

  XtVaSetValues( _umovie->W(), XmNleftAttachment,  XmATTACH_WIDGET,
                               XmNrightAttachment, XmATTACH_FORM,
                               XmNrightOffset,     5,
                               XmNtopAttachment,   XmATTACH_OPPOSITE_WIDGET,
                               XmNtopWidget,       _ifumovie->W(),
                               XmNleftWidget,      _ifumovie->W(), NULL );

  XtVaSetValues( _umovie->TxtW(UFRAMES),XmNshadowThickness, 0, NULL );

  XtVaSetValues( _but->W(), XmNtopAttachment,  XmATTACH_WIDGET,
                            XmNtopWidget,      _traceparams->W(),
                            XmNleftAttachment, XmATTACH_OPPOSITE_WIDGET,
                            XmNleftWidget,     _traceparams->W(),
                            XmNnumColumns,     2, NULL);

  return topWidget();
}

void SeisUnderPop::preManage()
{
  if(!_overlay_sp->movie()) 
     {
     _umovie->clear(UFRAMES);
     _ifumovie->SetTog( DOUMOVIE, False );
     XtSetSensitive( _ifumovie->W(), False); 
     }
  else
     {
     _umovie->SetValue(UFRAMES, _overlay_sp->frames()); 
     XtSetSensitive( _ifumovie->W(), True); 
     }
  XtSetSensitive( _umovie->W(), False);
  setFromOther(_overlay_sp);
  _selector_ndo_pop->checkMovieState (_overlay_sp->movie(),
    _overlay_sp->frames());

}
void SeisUnderPop::manage()
{
  preManage();
  SeisSelect::manage();
  setFromOther(_overlay_sp);
}

Boolean SeisUnderPop::ValidInput()
{
  Boolean stat= True;
  if (stat) stat= SeisSelect::ValidInput();
  if (stat) stat= _umovie->validate();
  if (stat) stat= _headers->validate();
  return stat;
}

void SeisUnderPop::DoAction()
{
  _sp->setFrames( _uframes);
  _sp->setMovie((Boolean)_doumovie);
  _sp->setMatchHeader((int)_mheader);
  _overlay_sp->setMatchHeader((int)_mheader);
  if (_sp->canScaleToFile())  _norm_type->SetRadio(PlotImage::FILENORM);
  else                        _norm_type->SetRadio(PlotImage::PANELNORM);
  ///////////////// new //////////////////////
  // this appeared to be a long standing bug 12/09/04
  _overlay_sp->getSeisColor()->loadToSeisPlot (_sp);
  ///////////////// new //////////////////////
  SeisSelect::DoAction();
}

void SeisUnderPop::reloadDefaults(Boolean do_method)
{
  SeisSelect::reloadDefaults(do_method);
  _umovie->reloadDefaults();
}

void SeisUnderPop::reloadSystemDefaults(Boolean do_method)
{
  SeisSelect::reloadSystemDefaults(do_method);
  _umovie->SetValue(UFRAMES,  10L);
  _umovie->SetValue(USFRAMES, 0L );
}

void UnderInform::newPlot(SeisPlot *sp)
{
 if (_under_pop->made()) {
    if ( (sp->plottedPlotType() == PlotImage::PlotWFILL) ||
         (sp->plottedPlotType() == PlotImage::PlotWONLY) ) {
           _under_pop->preManage();
    }
 }
}



void UnderInform::noPlotDisplayed(SeisPlot *)
{
 if (_under_pop->made()) {
     // _under_pop->unmanage();
     _under_pop->preManage();
 }
}

SeisPlot *SeisUnderPop::getSP()
{
   return _sp;
}

void SeisUnderPop::notCurrentInWindow(SeisPlot *sp)
{
  SeisPlot *oldsp= _sp;
  if (sp == _overlay_sp) {
        if (sp->currentSPInWindow()->getChainedSP()) 
                _sp=  sp->currentSPInWindow()->getChainedSP();
        else {
                assert(0);
        }
        _overlay_sp= sp->currentSPInWindow();
        _inform->addSeisPlot(_sp);
        if (!find(_overlay_sp)) {
              addSeisPlot(_overlay_sp);
              setFromOther(oldsp);

        }
        else if (made() && XtIsManaged(W())) {
              setFromOther(_sp);
        }
        _overlay_sp->setMatchHeader( 
                   ((SeisPlotUnder*)oldsp)->overlaySP()->matchHeader() );
        if (_infile) _infile->setFilename (_sp->filename());
  }

}

void SeisUnderPop::setFromOther(SeisPlot *othersp)
{
  SeisPlot *sp= othersp ? othersp : _sp;
  _sp->setFrames( sp->frames() );
  _sp->setMovie( sp->movie() );
  _sp->setMatchHeader( sp->matchHeader() );
  SeisSelect::setFromOther(sp);
  if (_selector_ndo_pop && made()) {
    _selector_ndo_pop->checkMovieState (sp->movie(), sp->frames());
  }
}

void SeisUnderPop::getTracePattern (long *nplt, long *iskp, long *ndo,
 long *nskp, long *frames, long *domovie)
{
  *nplt    = _nplt;
  *iskp    = _iskp;
  *ndo     = _ndo;
  *nskp    = _nskp;
  *frames  = _uframes;
  *domovie = _doumovie;
}

void SeisUnderPop::setNumMovieFrames(long num_frames)
{
  _umovie->SetValue(UFRAMES, num_frames);
}

void SeisUnderPop::setMovie (Boolean domovie)
{
  _ifumovie->SetTog (DOUMOVIE, domovie);
}
