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
//========================= COPYRIGHT NOTICE ================================
//====  CONFIDENTIAL AND PROPRIETARY INFORMATION OF CONOCO INC.      ========
//====   PROTECTED BY THE COPYRIGHT LAW AS AN UNPUBLISHED WORK       ========
//======================== COPYRIGHT NOTICE =================================


//=========================================================================
//========== Cmp plot gui class                                  ==========
//========== Author Michael L. Sherrill 09/97                    ==========
//=========================================================================

// $Id: va_cmp_gui.cc,v 1.2 2004/04/05 18:12:11 wjdone Exp $
// $Name:  $

#include <Xm/Label.h>
#include <Xm/Frame.h>
#include <Xm/Xm.h>
#include "sl/sl_scale.hh"
#include "sl/sl_text_box.hh"
#include "sl/sl_tog_box.hh"
#include "sl/sl_radio_box.hh"
#include "sl/psuedo_widget.hh"
#include "sl/sl_form.hh"
#include "sp/seis_plot.hh"
#include "vaplots/va_plot.hh"
#include "vaplots/va_cmp_plot.hh"
#include "vaplots/va_cmp_gui.hh"
#include "vaplots/va_common_params.hh"
#include "vaplots/va_common_params_gui.hh"
#include "vaplots/va_common_movie_gui.hh"
#include "sl/sl_push_box.hh"
#include "sp/seis_color_pop.hh"
#include "sp/seis_anno_pop.hh"


static String  defres[]= {
    "_popup.title:                    CMP Menu",
    "*cmptiL.labelString:             Traces / Inch:",
    "*isL.labelString:                Inches  /  Sec :",
    "*ctL.labelString:                Ct:",
    "*tminL.labelString:              Tmin:",
    "*tmaxL.labelString:              Tmax:", 
    "*CMPptype.Wonly.labelString:     Wiggle Only",
    "*CMPptype.Wfill.labelString:     Variable Area Pos",
    "*CMPptype.WfillN.labelString:    Variable Area Neg",
    "*CMPptype.Color.labelString:     Variable Density",
    "*cmprp.labelString:              Reverse Polarity",
    "*cmp_rtol.labelString:           Right to Left",
    "*cmpftL.labelString:             First Trace:",
    "*panelnorm.labelString:          Scale To Panel",
    "*normalize.labelString:          Normalize",
    "*filenorm.labelString:           Scale to File",
    "*ext.labelString:                Scale to Amp",
    "*cmpnskpL.labelString:           Nskp:",
    "*cmptpL.labelString:             Traces in Panel:",
    "*displab.labelString:            Display Parameters",
    "*displab.fontList:               -*-*-bold-r-*-*-*-180-*-*-p-*-*-*",
    "*cmpft.value:                    1",
    "*cmpnskp.value:                  0",
    "*cmptp.value:                    1",
    "*cmpti.value:                    20.0",
    "*ct.value:                       4",
    "*Wfill.set:                      True",
    "*color.labelString:              Color Options...",
    "*domovie.labelString:            Set Movie Parameters",
    "*first_panelL.labelString:       First Panel:",
    "*skip_panelsL.labelString:       Panels To Skip:",
    "*total_panelsL.labelString:      Total Panels:",
    "*automatic_nmc.labelString:      Apply NMC on scan",
    "*anno.labelString:               Annotation...",
    NULL, };



enum {CMPrp, CMP_LtoR};
enum {CMPft, CMPnskp, CMPtp};
enum {NEGATIVE_FILL};
enum {CMPti, CT};
enum {ANNO,  COLOR, AUTOMATIC_NMC};

//=========================================================================
//====================== Constructor ======================================
//=========================================================================
VaCmpGui::VaCmpGui( Widget            p,
                    char              *name,
                    HelpCtx           hctx,
                    VaCmpPlot         *plot)
                : SLFPopSep(p,name,FP_DOALL,hctx,False,False),
                  _plot(plot),_sp(plot->SP()), _apply_automatic_nmc(0),
                  _external_amp(1.0), _annotation_changed(False)
{
 
static SLRadio rads[]  = 
  {
    { "Color", PlotImage::PlotCOLOR },
    { "Wonly", PlotImage::PlotWONLY },
    { "Wfill", PlotImage::PlotWFILL },
    { "WfillN",NEGATIVE_FILL },
  };

static SLTog CMPtogs[]  = 
  {
    { "cmprp",    NULL, CMPrp },
    { "cmp_rtol", NULL, CMP_LtoR },
  };
CMPtogs[0].target= &_cmprp;
CMPtogs[1].target= &_rtol;

static SLText CmpText[]  = 
  {
    { "cmpft",   NULL,  NULL, SLType_int, CMPft },
    { "cmpnskp", NULL,  NULL, SLType_int, CMPnskp },
    { "cmptp",   NULL,  NULL, SLType_int, CMPtp },
  };
CmpText[0].target = &_cmpft;
CmpText[1].target = &_cmpnskp;
CmpText[2].target = &_cmptp;

static SLText plottexts[]  = 
  {
    {"cmpti", "range:0.1 1000.0,default:4.0",   NULL, SLType_float, CMPti },
    {"ct",    "range:0.1 1000.0,default:4.0",   NULL, SLType_float, CT },
  };
plottexts[0].target = &_cmpti;
plottexts[1].target = &_ct;


static SLRadio norm_rads[]  = 
  {
   { "panelnorm",  PlotImage::PANELNORM },
   { "normalize",  PlotImage::NORM },
   { "filenorm",   PlotImage::FILENORM },
   { "ext",        PlotImage::EXTERNALNORM },
  };

static SLText amp_text[]  = {
 { "amp",   NULL,  NULL,  SLType_float,   PlotImage::EXTERNALNORM },
};


static SLPush buttons[]  = {
 { "color",   COLOR},
};

static SLPush anno_button[]  = {
 { "anno",    ANNO },
};


static SLTog automatic_nmc_tog[]  = {
         { "automatic_nmc", NULL, AUTOMATIC_NMC },
};
automatic_nmc_tog[0].target = &_apply_automatic_nmc;


  setDefaultResources( p, name, defres);


  _first_time = True;
  _ok_to_retry = False;

  _plot_params= new SLTextBox( this, "p_params", getHelpCtx(),
                            plottexts, XtNumber(plottexts), True, 1, True );


  _common_gui_box = new VaCommonParamsGui(this, "common", getHelpCtx(),
                                          _plot->getCommonParams(),
                                          VaPlot::CMP);
  _plot->getCommonParams()->registerCommonGui(VaPlot::CMP,
                                              _common_gui_box);

  _common_movie_gui_box = new VaCommonMovieGui(this, "common_movie",
                                          getHelpCtx(),
                                          _plot->getCommonParams(),
                                          VaPlot::CMP);
  _plot->getCommonParams()->registerCommonMovieGui(VaPlot::CMP,
                                           _common_movie_gui_box);


  _plot->getCommonParams()->registerCmpPlotGui(this);

  _plot_type= new SLRadioBox( this, "CMPptype", getHelpCtx(), rads, 
                             XtNumber(rads), NULL, True, False );

  _display= new SLTextBox(this,"CMPdisp",getHelpCtx(), 
                           CmpText,XtNumber(CmpText), True, 1, True);
  _display->setAltLosingAction( (SLTextfunc)setTP, this);

  _rp= new SLTogBox( this, "CMPop", getHelpCtx(),
                        CMPtogs, XtNumber(CMPtogs), True, False );


  _norm_type= new SLRadioBox( this, "cmp_norm_type", getHelpCtx(),
                             norm_rads, XtNumber(norm_rads),NULL,True,False);
  _ext_amp= new SLTextBox( this,"_ext_amp",getHelpCtx(),
                           amp_text,XtNumber(amp_text), False);
  _ext_amp->setComplexNotify(this);

  _color_anno_buttons = new SLPushBox(this, "color_anno_buttons", getHelpCtx(),
                                buttons, XtNumber(buttons));
  _color_anno_buttons->setComplexNotify(this);

  _color_pop = NULL;

  _automatic_nmc_tog= new SLTogBox( this, "automatic_nmc",getHelpCtx(),
                          automatic_nmc_tog, XtNumber(automatic_nmc_tog),False,
                          False );

  make(p);

  _anno_but = new SLPushBox(this, "anno", getHelpCtx(),
                                anno_button, XtNumber(anno_button));
  _anno_but->setComplexNotify(this);
  _anno_pop = new SeisAnnoPop(topWidget(), "Cmp Annotation", getHelpCtx(),
                              _sp);
  _anno_pop->plotOnOK(False);
  _anno_pop->setPrimaryTimingLine(1.0F);
  _anno_pop->setSecondaryTimingLine(0.5F);
  _anno_pop->setPrimaryHeader(6);
  _anno_pop->setSecondaryHeader(_plot->getActiveXheader());

}


//=========================================================================
//===================== Destructor ========================================
//=========================================================================
VaCmpGui::~VaCmpGui()
{
  if(_plot_params)          delete _plot_params;
  if(_common_gui_box)       delete _common_gui_box;
  if(_common_movie_gui_box) delete _common_movie_gui_box;
  if(_plot_type)            delete _plot_type;
  if(_display)              delete _display;
  if(_rp)                   delete _rp;
  if(_norm_type)            delete _norm_type;
  if(_color_pop)            delete _color_pop;
  delete _automatic_nmc_tog;
}


//=========================================================================
//========================= Make  =========================================
//=========================================================================
Widget VaCmpGui::make(Widget p)
{

  if (made()) return topWidget ();

  SLFPopSep::make (p);
  p = wParent ();



   Widget _displab= XtVaCreateManagedWidget( "displab", xmLabelWidgetClass, 
                                topWidget(),
                                XmNtopAttachment,   XmATTACH_FORM,
                                XmNtopOffset,       15, 
                                XmNleftAttachment,  XmATTACH_FORM,
                                XmNleftOffset,      120, NULL );

   XtVaSetValues( _plot_params->W(),
                               XmNleftAttachment,  XmATTACH_FORM,
                               XmNleftOffset,      5,
                               XmNtopAttachment,   XmATTACH_WIDGET,
                               XmNtopWidget,       _displab, NULL);

   
   XtVaSetValues( _plot_type->W(), 
                                XmNtopAttachment,   XmATTACH_WIDGET,
                                XmNtopWidget,       _displab,
                                XmNleftAttachment,  XmATTACH_WIDGET,
                                XmNleftWidget,      _display->W(),
                                XmNleftOffset,      25,
                                XmNrightAttachment, XmATTACH_FORM,
                                XmNrightOffset,     5,
                                NULL);

   XtVaSetValues(_rp->W(),      XmNtopAttachment,   XmATTACH_WIDGET,
                                XmNtopOffset,       10,
                                XmNtopWidget,       _plot_type->W(),
                                XmNleftAttachment,  XmATTACH_WIDGET,
                                XmNleftWidget,      _display->W(),
                                XmNleftOffset,      25,
                                XmNrightAttachment, XmATTACH_FORM,
                                XmNrightOffset,     5,
                                NULL);
   

   XtVaSetValues(_norm_type->W(), 
                                XmNtopAttachment,   XmATTACH_WIDGET,
                                XmNtopOffset,       10,
                                XmNtopWidget,       _rp->W(),
                                XmNleftAttachment,  XmATTACH_WIDGET,
                                XmNleftWidget,      _display->W(),
                                XmNleftOffset,      25,
                                XmNrightAttachment, XmATTACH_FORM,
                                XmNrightOffset,     5,
                                NULL);
   _norm_type->SetRadio(PlotImage::PANELNORM);

   XtVaSetValues( _ext_amp->W(),   XmNtopAttachment,   XmATTACH_WIDGET,
                                   XmNtopWidget,       _norm_type->W(),
                                   XmNleftAttachment,  XmATTACH_OPPOSITE_WIDGET,
                                   XmNleftWidget,      _norm_type->W(),
                                   XmNrightAttachment, XmATTACH_FORM,
                                   NULL);
   _ext_amp->SetValue(PlotImage::EXTERNALNORM, (float)_external_amp );
   _ext_amp->SetSensitive(PlotImage::EXTERNALNORM, False);

   XtVaSetValues(_common_gui_box->W(), 
                                XmNleftAttachment,  XmATTACH_FORM,
                                XmNleftOffset,      5,
                                XmNtopAttachment,   XmATTACH_WIDGET,
                                XmNtopWidget,       _plot_params->W(), 
                                XmNtopOffset,       25,
                                NULL);


   XtVaSetValues(_display->W(), 
                               XmNleftAttachment,  XmATTACH_FORM,
                               XmNleftOffset,      5,
                               XmNtopAttachment,   XmATTACH_WIDGET,
                               XmNtopWidget,       _common_gui_box->W(), 
                               XmNtopOffset,       25,
                               NULL);


   XtVaSetValues( _color_anno_buttons->W(),
                               XmNleftAttachment,  XmATTACH_WIDGET,
                               XmNleftWidget,      _display->W(),
                               XmNleftOffset,      35,
                               XmNtopAttachment,   XmATTACH_WIDGET,
                               XmNtopWidget,       _norm_type->W(), 
                               XmNtopOffset,       60, NULL);
   
   XtVaSetValues( _automatic_nmc_tog->W(),
                               XmNleftAttachment,  XmATTACH_WIDGET,
                               XmNleftWidget,      _display->W(),
                               XmNleftOffset,      25,
                               XmNtopAttachment,   XmATTACH_WIDGET,
                               XmNtopWidget,       _color_anno_buttons->W(),
                               XmNtopOffset,       40, NULL);

   _common_movie_gui_box->setMovieOption(False);
   _common_movie_gui_box->setSensitive(False); 

   XtVaSetValues( _common_movie_gui_box->W(),
                    XmNtopAttachment,    XmATTACH_WIDGET,
                    XmNtopWidget,        _display->W(),
                    XmNtopOffset,        10,
                    XmNleftAttachment,   XmATTACH_FORM,
                    XmNleftOffset,       5,
                    NULL); 

   Widget tmp=  XtVaCreateManagedWidget( "", xmLabelWidgetClass, topWidget(),
                               XmNtopAttachment,   XmATTACH_WIDGET,
                               XmNtopWidget,  _common_movie_gui_box->W(),
                               XmNtopOffset,       3,
                               XmNbottomAttachment,XmATTACH_WIDGET,
                               XmNbottomWidget,    bottomSeparator(),
                               XmNbottomOffset,    25,
                               XmNleftAttachment,  XmATTACH_FORM,
                               XmNleftOffset,      5,
                               NULL);


   if(!_plot->_numcolors)
     {
     XtSetSensitive(_color_anno_buttons->W(),False);
     XtSetSensitive(_plot_type->GetRadioWidget(PlotImage::PlotCOLOR),False);
     }
   else
     {
     _color_pop = new SeisColorPop(topWidget(), "colorpop", _sp, 
                                   getHelpCtx());
     }


   

   return topWidget();
}



//=========================================================================
//============== The Seisplot in the window has changed ===================
//=========================================================================
void VaCmpGui::seisPlotChanged()
{
  printf("need to test this gui's seisPlotChanged method\n");


  _common_gui_box->setTmin(_sp->tmin());
  _common_gui_box->setTmax(_sp->tmax());
  _common_gui_box->notifyCommon();

  _rp->SetTog(CMPrp,    (Boolean)_sp->rp());
  _rp->SetTog(CMP_LtoR, (Boolean)_sp->rToL());


  if(_sp->plottedPlotType() == PlotImage::PlotWFILL && _sp->negativeFill())
     _plot_type->SetRadio(NEGATIVE_FILL);
  else
     _plot_type->SetRadio(_sp->plottedPlotType());

  _norm_type->SetRadio(_sp->norm());


  _display->SetValue(CMPft,  _sp->plottedISkp() + 1);
  _display->SetValue(CMPnskp,_sp->plottedNskp());
  _display->SetValue(CMPtp,  _sp->plottedNplt());

  _plot_params->SetValue(CMPti, _sp->originalTI());
  _plot_params->SetValue(CT,    _sp->originalIS());

  _common_movie_gui_box->setMovieOption(_sp->movie());

  //these need more work
  _common_movie_gui_box->setFirstMovie(1L);
  _common_movie_gui_box->setSkipMovies(_sp->skipFrames());
  _common_movie_gui_box->setTotalMovies(_sp->plottedFrames());
  
}




//=========================================================================
//========================== Handle notifies       ========================
//=========================================================================
Boolean VaCmpGui::notifyComplex(SLDelay *obj, int ident)
{

  if(obj == _color_anno_buttons) 
    {
    if(_color_pop) _color_pop->dontPlotYet(True);
    if(_color_pop)_color_pop->makeAndManage();
    _plot_type->SetRadio(PlotImage::PlotCOLOR);
    //_norm_type->SetRadio(PlotImage::FILENORM);
    //XtSetSensitive(_norm_type->W(),False);
    }

  else if(obj == _plot_type)
     {
     if(_plot_type->WhichSelected() == PlotImage::PlotCOLOR)
       {
       //_norm_type->SetRadio(PlotImage::FILENORM);
       //XtSetSensitive(_norm_type->W(), False);
       }
     else
       {
       //XtSetSensitive(_norm_type->W(), True);
       }
     }

  else if (obj == _norm_type) 
     {
     if (ident ==  PlotImage::EXTERNALNORM) 
       {
       _ext_amp->SetSensitive(PlotImage::EXTERNALNORM, True);
       _external_amp= _ext_amp->GetFloat(PlotImage::EXTERNALNORM);
       _ext_amp->SetValue(PlotImage::EXTERNALNORM, (float)_external_amp );
       }
     else 
       {
       _ext_amp->SetSensitive(PlotImage::EXTERNALNORM, False);
       _ext_amp->clear(PlotImage::EXTERNALNORM); 
       }
     }

  else if (obj == _ext_amp) 
     {
     _external_amp= _ext_amp->GetFloat(PlotImage::EXTERNALNORM);
     _ext_amp->SetValue(PlotImage::EXTERNALNORM, (float)_external_amp );
     }

  else if (obj == _anno_but)
     {
     _anno_pop->setSeisPlot(_sp);
     _anno_pop->makeAndManage(XtParent(W()));
     _annotation_changed = True;
     //Use the following to disable depth options
     //_anno_pop->setTimeDepthState(False);
     }

  return True;
}


//=========================================================================
//========================== Check for valid input ========================
//=========================================================================
Boolean VaCmpGui::ValidInput()
{
 Boolean stat = True;
 
 if (made()) 
   {
   if (stat) stat= _plot_params->validate();
   if (stat) stat= _display->validate();
   }

 return (stat);

}


//=========================================================================
//===================== Set traces per panel  =============================
//=========================================================================
void VaCmpGui::setTP(void *data, long which)
{
VaCmpGui *obj = (VaCmpGui *)data;
long nskp, ft, tp, usertp;
float temp;
long max_per_panel;
long num_panels;


  switch (which) 
  {
  case CMPft:
  case CMPnskp:
    if(!(num_panels = obj->_plot->numberLocationsInFile()) ) break;
    max_per_panel = obj->_sp->totalTraces() / num_panels;
    ft   = obj->_cmpft;
    nskp = obj->_cmpnskp;
    temp = (float((max_per_panel - ft))) / (float((nskp + 1.0))) + 1.0;
    tp = int(temp);
    obj->_display->SetValue(CMPtp, tp);
    break;       

  case CMPtp:
    if(!(num_panels = obj->_plot->numberLocationsInFile()) ) break;
    max_per_panel = obj->_sp->totalTraces() / num_panels;
    ft   = obj->_cmpft;
    nskp = obj->_cmpnskp;
    temp = (float((max_per_panel - ft))) / (float((nskp + 1.0))) + 1.0;
    tp = int(temp);
    usertp = obj->_cmptp;
    tp = (tp < usertp) ? tp : usertp;
    obj->_display->SetValue(CMPtp, tp);
    break;

   } // End 
}

//=========================================================================
//===================== Take action on Ok Apply etc========================
//=========================================================================
void VaCmpGui::DoAction()
{
int stat;

  SLFPopSep::DoAction();

  setParameters();

  if(_color_pop)_color_pop->dontPlotYet(True);
  if(_color_pop)_color_pop->make();
  if(_color_pop)_color_pop->dontPlotYet(False);

  if(_annotation_changed)
    {
      _common_gui_box->annotationChanged(VaPlot::CMP,
                                         _anno_pop->getPrimaryTimingLine(),
                                         _anno_pop->getSecondaryTimingLine(),
                                         _anno_pop->getDepthOption());
      _annotation_changed = False;
    }


  if( strlen(_sp->filename()) )
      {
      //if we attempted to plot before and it failed the isPlotDisplayed
      //method returns a false, so we will retry if it is ok 
      if(_sp->isPlotDisplayed() || _ok_to_retry ) 
        {
        stat = _plot->plot();
        if(stat != PlotImage::PlotSuccess)
          _ok_to_retry = True;
        else
          _ok_to_retry = False;
        }
      }

  _common_gui_box->replotAllPlots(VaPlot::CMP);
}

//=========================================================================
//============= Common gui changes calls this for a replot      ===========
//=========================================================================
void VaCmpGui::commonReplot()
{
  if(!_sp->isPlotDisplayed()) return;
  setParameters();
  _plot->plot();
}

//=========================================================================
//============= Set first panel to display                      ===========
//=========================================================================
void VaCmpGui::setFirstPanelToPlot(long first_panel)
{
  _common_movie_gui_box->setFirstMovie(first_panel);
  setParameters();
}

//=========================================================================
//============= Set skip movie panels                           ===========
//=========================================================================
long VaCmpGui::getSkipMoviePanels()
{
  return _common_movie_gui_box->getSkipMovies();
}


//=========================================================================
//============= Set first movie panel                           ===========
//=========================================================================
long VaCmpGui::getFirstMoviePanel()
{
  return _common_movie_gui_box->getFirstMovie();
}


//=========================================================================
//===================== Set parameters for plotting =======================
//=========================================================================
void VaCmpGui::setParameters()
{
long trace_skip = 0;


  if(!_sp->usingSelector())
    {
    trace_skip = (_common_movie_gui_box->getFirstMovie() - 1) 
               * _plot->tracesPerGroup();
    }
  else
    {
    for(int i = 0; i <_common_movie_gui_box->getFirstMovie(); i++)
      trace_skip += _sp->getSelectorNumTraces(i);
    }


  _sp->setHeaders(_anno_pop->getPrimaryHeader(), 
                  _anno_pop->getSecondaryHeader());
  _sp->setCT(_ct);
  _sp->setRP(_cmprp);
  _sp->setRtoL(_rtol);
  _sp->setTimingLines(_anno_pop->getPrimaryTimingLine(), 
                      _anno_pop->getSecondaryTimingLine());
  _sp->setDepth(_anno_pop->getDepthOption());
  _sp->setNorm(_norm_type->WhichSelected());
  if (_norm_type->WhichSelected() == PlotImage::EXTERNALNORM)
      _sp->setExternalAmp(_external_amp);
  _sp->setTI(_cmpti);
  _sp->setIS(_common_gui_box->getIS()); 
  _sp->setTminTmax(_common_gui_box->getTmin(), _common_gui_box->getTmax());
  _sp->setISkp(trace_skip);
  _sp->setNPlt(_cmptp);
  if(_plot_type->WhichSelected() == NEGATIVE_FILL)
    {
    _sp->setPlotType(PlotImage::PlotWFILL);
    _sp->setNegativeFill(True);
    }
  else
    {
    _sp->setPlotType( _plot_type->WhichSelected() );
    _sp->setNegativeFill(False);
    }

  if(_cmpft > 1 && _cmpnskp == 0)
    _sp->setNSkp( _cmpft-1);
  else
    _sp->setNSkp(_cmpnskp);     

  if(_cmpnskp == 0)
    _sp->setNdo(_cmptp);
  else
    _sp->setNdo(1L);

  if(_common_movie_gui_box->getMovieOption())
    {
    if(_common_movie_gui_box->getTotalMovies() > 1)
      {
      _sp->setMovie(True);
      _sp->setFrames(_common_movie_gui_box->getTotalMovies());
      _sp->setSkipFrames(_common_movie_gui_box->getSkipMovies());
      }
    else
      {
      _sp->setMovie(False);
      }
    }  
  else
    {
    _sp->setMovie(False);
    _sp->setSkipFrames(0);
    _sp->setFrames(1);
    }

  //Reset parameters for irregular data
  if(_sp->usingSelector())
    {
    long max_traces = 0;

    for(int i = 0; i < _plot->numberLocationsInFile(); i++)
       if(max_traces < _sp->getSelectorNumTraces(i))
          max_traces = _sp->getSelectorNumTraces(i);

    _sp->setNPlt(max_traces);
    _sp->setCurrentTraceSelectorPanel(
                       _common_movie_gui_box->getFirstMovie() - 1);

    }


}

//=========================================================================
//==================== Reset when new file is read ========================
//=========================================================================
void VaCmpGui::setToFileDefaults()
{

}


//=========================================================================
//====================== Manage ===========================================
//=========================================================================
void VaCmpGui::manage()
{

 assert(_plot);

 SLBase::manage();

 if (_first_time) 
   {
   XtVaSetValues( _anno_but->W(), XmNtopAttachment,  XmATTACH_WIDGET,
                             XmNtopWidget,      _color_anno_buttons->W(),
                             XmNleftAttachment, XmATTACH_OPPOSITE_WIDGET, 
                             XmNleftWidget,     _color_anno_buttons->W(),
                             XmNtopOffset,      5, NULL);
   }

 _first_time= False;
 
 XtManageChild(topWidget()); 
}


//=========================================================================
//==================== Reset to system defaults ===========================
//=========================================================================
void VaCmpGui::reloadSystemDefaults(Boolean /*do_method*/)
{

  _rp->SetTog(CMPrp, False);
  _rp->SetTog(CMP_LtoR, False);


  _plot_type->SetRadio(PlotImage::PlotWFILL);

  _norm_type->SetRadio(PlotImage::PANELNORM);


  _display->SetValue(CMPft, 1L);
  _display->SetValue(CMPnskp, 0L);
  _display->SetValue(CMPtp, 1L);

  _plot_params->SetValue(CMPti, 20.0F);
  _plot_params->SetValue(CT,     4.0F);

  _common_movie_gui_box->setMovieOption(False);

  _common_movie_gui_box->setFirstMovie(1L);
  _common_movie_gui_box->setSkipMovies(0L);
  _common_movie_gui_box->setTotalMovies(1L);


}



//=========================================================================
//====================  Update public method    ===========================
//====================  Called on new file      ===========================
//=========================================================================
void VaCmpGui::updateParams(Boolean update_file_limits)
{
long possible_movies;
long first_panel_index;


  _display->SetValue(CMPft,_plot->firstTrace());
  _display->SetValue(CMPtp,_plot->tracesPerGroup());
  _display->SetValue(CMPnskp,0L);
  _sp->setNPlt(_plot->tracesPerGroup());
  _sp->setNdo(1);
  _sp->setNSkp(0);
  _sp->setTdec(1);

  if(!_anno_pop->made())
    {
    _anno_pop->setPrimaryHeader(6);
    _anno_pop->setSecondaryHeader(_plot->getActiveXheader());
    }
  
  first_panel_index = (_plot->firstTrace() - 1) / _plot->tracesPerGroup();
  possible_movies = _plot->numberLocationsInFile() - first_panel_index; 
  if(_common_movie_gui_box->getSkipMovies())
     possible_movies = possible_movies / _common_movie_gui_box->getSkipMovies();
  if(possible_movies <= 1 || _common_movie_gui_box->getTotalMovies() <= 1)
    _common_movie_gui_box->setMovieOption(False);
  else
    possible_movies = min(possible_movies, 
                          _common_movie_gui_box->getTotalMovies());
  _common_movie_gui_box->setFirstMovie(first_panel_index + 1);
  _common_movie_gui_box->setTotalMovies(possible_movies);
  _common_movie_gui_box->setFileMaxMovies(VaPlot::CMP, 
                                           _plot->numberLocationsInFile());

  

   _sp->setISkp((_common_movie_gui_box->getFirstMovie() - 1) *
                 _plot->tracesPerGroup());

    //set common parameters for gvs,cmp and semblance
  if(update_file_limits)
    {
    _common_gui_box->setFileTmin(VaPlot::CMP,_plot->minTime());
    _common_gui_box->setFileTmax(VaPlot::CMP,_plot->maxTime());
    _common_gui_box->haveFileLimits(VaPlot::CMP, True);
    _common_gui_box->setTmin(_common_gui_box->getFileTminLimit());
    _common_gui_box->setTmax(_common_gui_box->getFileTmaxLimit());
    }

  _common_gui_box->setIS(_common_gui_box->getIS());
  _common_gui_box->notifyCommon();

}

void VaCmpGui::annotationChanged(float primary, float secondary, Boolean depth)
{
  _anno_pop->setPrimaryTimingLine(primary);
  _anno_pop->setSecondaryTimingLine(secondary);
  _anno_pop->setDepthOption(depth);
}
