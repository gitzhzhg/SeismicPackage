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
//========== Gvs plot gui class                                  ==========
//========== Author Michael L. Sherrill 09/97                    ==========
//=========================================================================

// $Id: va_gvs_gui.cc,v 1.2 2004/04/05 18:12:11 wjdone Exp $
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
#include "vaplots/va_gvs_plot.hh"
#include "vaplots/va_gvs_gui.hh"
#include "vaplots/va_common_params_gui.hh"
#include "vaplots/va_common_params.hh"
#include "vaplots/va_iso_plotter.hh"
#include "sl/sl_push_box.hh"
#include "sp/seis_color_pop.hh"
#include "sp/seis_plot_under.hh"
#include "sp/seis_anno_pop.hh"


static String  defres[]= {
    "_popup.title:                    GVS Menu",
    "*gvstiL.labelString:             Traces / Inch:",
    "*isL.labelString:                Inches  /  Sec :",
    "*ctL.labelString:                Ct:",
    "*tminL.labelString:              Tmin:",
    "*tmaxL.labelString:              Tmax:", 
    "*GVSptype.Wonly.labelString:     Wiggle Only",
    "*GVSptype.Wfill.labelString:     Variable Area Pos",
    "*GVSptype.WfillN.labelString:    Variable Area Neg",
    "*GVSptype.Color.labelString:     Variable Density",
    "*gvsrp.labelString:              Reverse Polarity",
    "*gvs_rtol.labelString:           Right to Left",
    "*gvsftL.labelString:             First Trace:",
    "*panelnorm.labelString:          Scale To Panel",
    "*normalize.labelString:          Normalize",
    "*ext.labelString:                Scale to Amp",
    "*filenorm.labelString:           Scale to File",
    "*gvsnskpL.labelString:           Nskp:",
    "*gvstpL.labelString:             Traces in Panel:",
    "*displab.labelString:            Display Parameters",
    "*displab.fontList:               -*-*-bold-r-*-*-*-180-*-*-p-*-*-*",
    "*gvsft.value:                    1",
    "*gvsnskp.value:                  0",
    "*gvstp.value:                    1",
    "*gvsti.value:                    20.0",
    "*ct.value:                       4",
    "*Wfill.set:                      True",
    "*color.labelString:              Color Options...",
    "*domovie.labelString:            Set Movie Parameters",
    "*first_panelL.labelString:       First Panel:",
    "*skip_panelsL.labelString:       Panels To Skip :",
    "*total_panelsL.labelString:      Total Panels:",
    "*first_panel.value:              1",
    "*skip_panels.value:              0",
    "*total_panels.value:             1",
    "*anno.labelString:               Annotation...",
    NULL, };



enum {GVSrp, GVS_LtoR};
enum {GVSft, GVSnskp, GVStp};
enum {GVSti, CT};
enum {ANNO,  COLOR};
enum {NEGATIVE_FILL, DO_MOVIE};
enum {FIRST_PANEL, SKIP_PANELS, TOTAL_PANELS};

//=========================================================================
//====================== Constructor ======================================
//=========================================================================
VaGvsGui::VaGvsGui( Widget            p,
                    char              *name,
                    HelpCtx           hctx,
                    VaGvsPlot         *plot)
                : SLFPopSep(p,name,FP_DOALL,hctx,False,False),
                  _plot(plot),_sp(plot->SP()), _external_amp(1.0),
                  _annotation_changed(False)
{

  _sp_under = _plot->underSP();
 
static SLRadio rads[]  = 
  {
    { "Color", PlotImage::PlotCOLOR },
    { "Wonly", PlotImage::PlotWONLY },
    { "Wfill", PlotImage::PlotWFILL },
    { "WfillN",NEGATIVE_FILL },
  };

static SLTog GVStogs[]  = 
  {
    { "gvsrp",    NULL, GVSrp },
    { "gvs_rtol", NULL, GVS_LtoR },
  };
GVStogs[0].target= &_gvsrp;
GVStogs[1].target= &_rtol;

static SLText GvsText[]  = 
  {
    { "gvsft",   NULL,  NULL, SLType_int, GVSft },
    { "gvsnskp", NULL,  NULL, SLType_int, GVSnskp },
    { "gvstp",   NULL,  NULL, SLType_int, GVStp },
  };
GvsText[0].target = &_gvsft;
GvsText[1].target = &_gvsnskp;
GvsText[2].target = &_gvstp;

static SLText plottexts[]  = 
  {
    {"gvsti", "range:0.1 1000.0,default:4.0",   NULL, SLType_float, GVSti },
    {"ct",    "range:0.1 1000.0,default:4.0",   NULL, SLType_float, CT },
  };
plottexts[0].target = &_gvsti;
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

static SLTog movietog[]  = {
         { "domovie", NULL, DO_MOVIE },
};
movietog[0].target = &_do_movie;

static SLText movietexts[]  = {
 {"first_panel",  NULL, NULL, SLType_int,   FIRST_PANEL},
 {"skip_panels",  NULL, NULL, SLType_int,   SKIP_PANELS},
 {"total_panels", NULL, NULL, SLType_int,   TOTAL_PANELS},
};
movietexts[0].target = &_first_panel;
movietexts[1].target = &_skip_panels;
movietexts[2].target = &_total_panels;


static SLPush anno_button[]  = {
 { "anno",    ANNO },
};




  setDefaultResources( p, name, defres);

  _first_time  = True;
  _ok_to_retry = False;

  _plot_params= new SLTextBox( this, "p_params", getHelpCtx(),
                            plottexts, XtNumber(plottexts), True, 1, True );


  _common_gui_box = new VaCommonParamsGui(this, "common", getHelpCtx(),
                                          _plot->getCommonParams(),
                                          VaPlot::GVS);
  _plot->getCommonParams()->registerCommonGui(VaPlot::GVS,
                                              _common_gui_box); 
  _plot->getCommonParams()->registerGvsPlotGui(this);


  _plot_type= new SLRadioBox( this, "GVSptype", getHelpCtx(), rads, 
                             XtNumber(rads), NULL, True, False );
  _plot_type->setComplexNotify(this);

  _display= new SLTextBox(this,"GVSdisp",getHelpCtx(), 
                           GvsText,XtNumber(GvsText), True, 1, True);
  _display->setAltLosingAction( (SLTextfunc)setTP, this);

  _rp= new SLTogBox( this, "GVSop", getHelpCtx(),
                        GVStogs, XtNumber(GVStogs), True, False );


  _norm_type= new SLRadioBox( this, "gvs_norm_type", getHelpCtx(),
                             norm_rads, XtNumber(norm_rads),NULL,True,False);
  _norm_type->setComplexNotify(this);

  _ext_amp= new SLTextBox( this,"_ext_amp",getHelpCtx(),
                           amp_text,XtNumber(amp_text), False);
  _ext_amp->setComplexNotify(this);


  _color_anno_buttons = new SLPushBox(this, "color_anno_buttons", getHelpCtx(),
                                      buttons, XtNumber(buttons));
  _color_anno_buttons->setComplexNotify(this);

  _color_pop = NULL;

  _movie_tog= new SLTogBox( this, "movie_tog",getHelpCtx(),
                           movietog, XtNumber(movietog),False, False );
  _movie_tog->setComplexNotify(this);

  _movie_box = new SLTextBox( this, "movie_box", getHelpCtx(), 
                              movietexts, XtNumber(movietexts), True, 1, True);
  _movie_box->setComplexNotify(this);

  make(p);

  _anno_but = new SLPushBox(this, "anno", getHelpCtx(),
                                anno_button, XtNumber(anno_button));
  _anno_but->setComplexNotify(this);
  _anno_pop = new SeisAnnoPop(topWidget(), "Gvs Annotation", getHelpCtx(),
                              _sp);
  _anno_pop->plotOnOK(False);
  _anno_pop->setPrimaryTimingLine(1.0F);
  _anno_pop->setSecondaryTimingLine(0.5F);
  _anno_pop->setPrimaryHeader(_plot->getActiveXheader());
  _anno_pop->setSecondaryHeader(63);
}


//=========================================================================
//===================== Destructor ========================================
//=========================================================================
VaGvsGui::~VaGvsGui()
{
  if(_plot_params)    delete _plot_params;
  if(_common_gui_box) delete _common_gui_box;
  if(_plot_type)      delete _plot_type;
  if(_display)        delete _display;
  if(_rp)             delete _rp;
  if(_norm_type)      delete _norm_type;
  if(_ext_amp)        delete _ext_amp;
  if(_color_pop)      delete _color_pop;
}


//=========================================================================
//============== The Seisplot in the window has changed ===================
//=========================================================================
void VaGvsGui::seisPlotChanged()
{
  printf("need to test this gui's seisPlotChanged method\n");


  _common_gui_box->setTmin(_sp->tmin());
  _common_gui_box->setTmax(_sp->tmax());
  _common_gui_box->notifyCommon();

  _rp->SetTog(GVSrp,    (Boolean)_sp->rp());
  _rp->SetTog(GVS_LtoR, (Boolean)_sp->rToL());


  if(_sp->plottedPlotType() == PlotImage::PlotWFILL && _sp->negativeFill())
     _plot_type->SetRadio(NEGATIVE_FILL);
  else
     _plot_type->SetRadio(_sp->plottedPlotType());

  _norm_type->SetRadio(_sp->norm());


  _display->SetValue(GVSft,  _sp->plottedISkp() + 1);
  _display->SetValue(GVSnskp,_sp->plottedNskp());
  _display->SetValue(GVStp,  _sp->plottedNplt());

  _plot_params->SetValue(GVSti, _sp->originalTI());
  _plot_params->SetValue(CT,    _sp->originalIS());

  _movie_tog->SetTog(DO_MOVIE,  _sp->movie());

  //these need more work
 _movie_box->SetValue(FIRST_PANEL,  1.0F);
 _movie_box->SetValue(SKIP_PANELS,  _sp->skipFrames());
 _movie_box->SetValue(TOTAL_PANELS, _sp->plottedFrames());
  
}

//=========================================================================
//========================= Make  =========================================
//=========================================================================
Widget VaGvsGui::make(Widget p)
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
   
   XtVaSetValues( _movie_tog->W(),
                               XmNtopAttachment,    XmATTACH_WIDGET,
                               XmNtopWidget,        _display->W(),
                               XmNleftAttachment,   XmATTACH_FORM,
                               XmNleftOffset,       30,
                               NULL ); 
   _movie_tog->SetTog( DO_MOVIE, False );


   XtVaSetValues( _movie_box->W(),
                    XmNtopAttachment,    XmATTACH_WIDGET,
                    XmNtopWidget,        _movie_tog->W(),
                    XmNleftAttachment,   XmATTACH_OPPOSITE_WIDGET,
                    XmNleftWidget,       _display->W(),
                    NULL); 

   Widget tmp=  XtVaCreateManagedWidget( "", xmLabelWidgetClass, topWidget(),
                               XmNtopAttachment,   XmATTACH_WIDGET,
                               XmNtopWidget,       _movie_box->W(),
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
//========================== Handle notifies       ========================
//=========================================================================
Boolean VaGvsGui::notifyComplex(SLDelay *obj, int ident)
{

  if(obj == _color_anno_buttons) 
    {
    if(_color_pop) _color_pop->dontPlotYet(True);
    if(_color_pop) _color_pop->makeAndManage();
    _plot_type->SetRadio(PlotImage::PlotCOLOR);
    //_norm_type->SetRadio(PlotImage::FILENORM);
    //XtSetSensitive(_norm_type->W(),False);
    if (_sp->canScaleToFile()) 
      _norm_type->SetRadio(PlotImage::FILENORM);
    else
      _norm_type->SetRadio(PlotImage::PANELNORM);
    }

  else if(obj == _movie_tog)
     {
     if(_movie_tog->IsSelected(DO_MOVIE))
       XtSetSensitive(_movie_box->W(), True);
     else
       XtSetSensitive(_movie_box->W(), False);
     }

  else if(obj == _plot_type)
     {
     if(_plot_type->WhichSelected() == PlotImage::PlotCOLOR)
       {
       _iso_plotter->setColorOptions(VaPlot::ISO,False);  
       //_norm_type->SetRadio(PlotImage::FILENORM);
       //XtSetSensitive(_norm_type->W(), False);
       }
     else
       {
       _iso_plotter->setColorOptions(VaPlot::ISO,True);
       //XtSetSensitive(_norm_type->W(), True);
       }
     }

   else if(obj == _movie_box)
     {
     if(_movie_tog->IsSelected(DO_MOVIE)) setMovieParameters(ident);
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
     //Use the following to disable the time depth options
     //_anno_pop->setTimeDepthState(False);
     }


  return True;
}


//=========================================================================
//========================== Check for valid input ========================
//=========================================================================
Boolean VaGvsGui::ValidInput()
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
void VaGvsGui::setTP(void *data, long which)
{
VaGvsGui *obj = (VaGvsGui *)data;
long nskp, ft, tp, usertp;
float temp;
long max_per_panel;
long num_panels;


  switch (which) 
  {
  case GVSft:
  case GVSnskp:
    if(!(num_panels = obj->_plot->numberLocationsInFile()) ) break;
    max_per_panel = obj->_sp->totalTraces() / num_panels;
    ft   = obj->_gvsft;
    nskp = obj->_gvsnskp;
    temp = (float((max_per_panel - ft))) / (float((nskp + 1.0))) + 1.0;
    tp = int(temp);
    obj->_display->SetValue(GVStp, tp);
    break;       

  case GVStp:
    if(!(num_panels = obj->_plot->numberLocationsInFile()) ) break;
    max_per_panel = obj->_sp->totalTraces() / num_panels;
    ft   = obj->_gvsft;
    nskp = obj->_gvsnskp;
    temp = (float((max_per_panel - ft))) / (float((nskp + 1.0))) + 1.0;
    tp = int(temp);
    usertp = obj->_gvstp;
    tp = (tp < usertp) ? tp : usertp;
    obj->_display->SetValue(GVStp, tp);
    break;

   } // End 
}


//=========================================================================
//===================== Set color options based on underlay usage  ========
//=========================================================================
void VaGvsGui::setColorOptions(Boolean turn_on)
{
  if(!made()) return;
  if(turn_on)
    {
    XtSetSensitive(_plot_type->GetRadioWidget(PlotImage::PlotCOLOR),True);
    XtSetSensitive(_color_anno_buttons->pushW(COLOR),True);
    }
  else
    {
    _plot_type->SetRadio(PlotImage::PlotWFILL);
    XtSetSensitive(_plot_type->GetRadioWidget(PlotImage::PlotCOLOR),False);
    XtSetSensitive(_color_anno_buttons->pushW(COLOR),False);
    }
}

//=========================================================================
//===================== Take action on Ok Apply etc========================
//=========================================================================
void VaGvsGui::DoAction()
{
int stat;

  SLFPopSep::DoAction();

  setParameters();

  if(_color_pop) _color_pop->dontPlotYet(True);
  if(_color_pop) _color_pop->make();
  if(_color_pop) _color_pop->dontPlotYet(False);

  if(_annotation_changed)
    {
      _common_gui_box->annotationChanged(VaPlot::GVS,
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

  _common_gui_box->replotAllPlots(VaPlot::GVS);
}

//=========================================================================
//============= Common gui changes calls this for a replot      ===========
//=========================================================================
void VaGvsGui::commonReplot()
{
  if(!_sp->isPlotDisplayed()) return;
  setParameters();
  _plot->plot();
}
//=========================================================================
//===================== Set parameters for plotting =======================
//=========================================================================
void VaGvsGui::setParameters()
{
long trace_skip = 0;

  if(_do_movie)
    {
    if(!_sp->usingSelector())
      {
      trace_skip = (_first_panel - 1) * _plot->tracesPerGroup();
      }
    else
      {
      for(int i = 0; i < _first_panel; i++)
        trace_skip += _sp->getSelectorNumTraces(i);
      }
    }

  _sp->setHeaders(_anno_pop->getPrimaryHeader(), 
                  _anno_pop->getSecondaryHeader());
  _sp->setDepth(_anno_pop->getDepthOption());
  _sp->setCT(_ct);
  _sp->setRP(_gvsrp);
  _sp->setRtoL(_rtol);
  _sp->setTimingLines(_anno_pop->getPrimaryTimingLine(), 
                      _anno_pop->getSecondaryTimingLine());
  _sp->setNorm(_norm_type->WhichSelected());
  if (_norm_type->WhichSelected() == PlotImage::EXTERNALNORM)
      _sp->setExternalAmp(_external_amp);
  _sp->setTI(_gvsti);
  _sp_under->setTI(_gvsti);
  _sp->setIS(_common_gui_box->getIS());
  _sp_under->setIS(_common_gui_box->getIS());
  _sp->setTminTmax(_common_gui_box->getTmin(), _common_gui_box->getTmax());
  _sp->setISkp(trace_skip);
  _sp->setNPlt(_gvstp);
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

  if(_gvsft > 1 && _gvsnskp == 0)
    _sp->setNSkp( _gvsft-1);
  else
    _sp->setNSkp(_gvsnskp);     

  if(_gvsnskp == 0)
    _sp->setNdo(_gvstp);
  else
    _sp->setNdo(1L);

  if(_movie_tog->IsSelected(DO_MOVIE))
    {
    if(_total_panels > 1)
      {
      _sp->setMovie(True);
      _sp->setFrames(_total_panels);
      _sp->setSkipFrames(_skip_panels);
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

}

//=========================================================================
//==================== Reset when new file is read ========================
//=========================================================================
void VaGvsGui::setToFileDefaults()
{

}


//=========================================================================
//====================== Manage ===========================================
//=========================================================================
void VaGvsGui::manage()
{

  assert(_plot);

  SLBase::manage();

  if (_first_time) 
   {
   XtSetSensitive(_movie_box->W(), False); 
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
void VaGvsGui::reloadSystemDefaults(Boolean /*do_method*/)
{

  _rp->SetTog(GVSrp, False);
  _rp->SetTog(GVS_LtoR, False);


  _plot_type->SetRadio(PlotImage::PlotWFILL);

  _norm_type->SetRadio(PlotImage::PANELNORM);


  _display->SetValue(GVSft, 1L);
  _display->SetValue(GVSnskp, 0L);
  _display->SetValue(GVStp, 1L);

  _plot_params->SetValue(GVSti, 20.0F);
  _plot_params->SetValue(CT,     4.0F);

  _movie_tog->SetTog(DO_MOVIE,   False);

  _movie_box->SetValue(FIRST_PANEL,  1L);
  _movie_box->SetValue(SKIP_PANELS,  0L);
  _movie_box->SetValue(TOTAL_PANELS, 1L);


}

//=========================================================================
//==================== Set movie parameters     ===========================
//=========================================================================
void VaGvsGui::setMovieParameters(int ident)
{
long max_possible;
long current_maxpnl;
float temp;

  max_possible = _plot->numberLocationsInFile();

  temp = ( (float)max_possible - _first_panel ) / (_skip_panels + 1.0) + 1.0;
  current_maxpnl = (long)temp;


  switch(ident)
    {
    case FIRST_PANEL:
      if(_first_panel >= 1)
        {
        _total_panels = current_maxpnl;
        _movie_box->SetValue(TOTAL_PANELS, current_maxpnl);
        }
      if(_first_panel >= max_possible)
        {
        _first_panel  = max_possible;
        _skip_panels  = 0;
        _total_panels = 1;
        _movie_box->SetValue(FIRST_PANEL,  _first_panel);
        _movie_box->SetValue(SKIP_PANELS,  _skip_panels);
        _movie_box->SetValue(TOTAL_PANELS, _total_panels);
      }
      break;


    case SKIP_PANELS:
      if(_skip_panels)
        {
        temp = ((float)max_possible-_first_panel ) / (_skip_panels+1.0) + 1.0;
        current_maxpnl = (long)temp;
        _movie_box->SetValue(TOTAL_PANELS, current_maxpnl);
        if( _skip_panels >= (max_possible - _first_panel) )
          {
          _skip_panels  = 0;
          temp = ( (float)max_possible - _first_panel )
                      / (_skip_panels + 1.0) + 1.0;
          current_maxpnl = (long)temp;
          _total_panels   = (int)(max_possible - _first_panel + 1);
          _movie_box->SetValue(SKIP_PANELS,  _skip_panels);
          _movie_box->SetValue(TOTAL_PANELS, _total_panels);
          }
        }
      else
        {
        _total_panels = current_maxpnl;
        _movie_box->SetValue(TOTAL_PANELS, _total_panels);
        }
      break;


    case TOTAL_PANELS:
      if(_total_panels > current_maxpnl)
         _total_panels = current_maxpnl;
      _movie_box->SetValue(TOTAL_PANELS, _total_panels);
      break;
    }//end switch

}

//=========================================================================
//====================  Update public method    ===========================
//====================  Called on new file      ===========================
//=========================================================================
void VaGvsGui::updateParams(Boolean update_file_limits)
{
long possible_movies;
long first_panel;

  _display->SetValue(GVSft,_plot->firstTrace());
  _display->SetValue(GVStp,_plot->tracesPerGroup());
  _display->SetValue(GVSnskp,0L);
  _sp->setNPlt(_plot->tracesPerGroup());
  _sp->setISkp(_plot->firstTrace() - 1);
  _sp->setNdo(1);
  _sp->setNSkp(0);
  _sp->setTdec(1);

  if(!_anno_pop->made())
    {
    _anno_pop->setPrimaryHeader(_plot->getActiveXheader());
    _anno_pop->setSecondaryHeader(63);
    }

  first_panel = (_plot->firstTrace() - 1) / _plot->tracesPerGroup();
  possible_movies = _plot->numberLocationsInFile() - first_panel; 
  if(_skip_panels) possible_movies = possible_movies / _skip_panels;
  if(possible_movies <= 1 || _total_panels <= 1)
    _movie_tog->SetTog( DO_MOVIE, False );
  else
    possible_movies = min(possible_movies, _total_panels);
  _movie_box->SetValue(TOTAL_PANELS, possible_movies);

    //set common parameters for gvs,cmp and semblance
  if(update_file_limits)
    {
    _common_gui_box->setFileTmin(VaPlot::GVS,_plot->minTime());
    _common_gui_box->setFileTmax(VaPlot::GVS,_plot->maxTime());
    _common_gui_box->haveFileLimits(VaPlot::GVS, True);
    _common_gui_box->setTmin(_common_gui_box->getFileTminLimit());
    _common_gui_box->setTmax(_common_gui_box->getFileTmaxLimit());
    }


  _common_gui_box->setIS(_common_gui_box->getIS());
  
  _common_gui_box->notifyCommon();
}


void VaGvsGui::annotationChanged(float primary, float secondary, Boolean depth)
{
  _anno_pop->setPrimaryTimingLine(primary);
  _anno_pop->setSecondaryTimingLine(secondary);
  _anno_pop->setDepthOption(depth);
}
