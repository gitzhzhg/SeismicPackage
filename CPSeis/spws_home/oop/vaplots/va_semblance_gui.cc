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
//========== Semblance plot gui class                            ==========
//========== Author Michael L. Sherrill 08/97                    ==========
//=========================================================================

// $Id: va_semblance_gui.cc,v 1.4 2004/07/21 16:50:36 wjdone Exp $
// $Name:  $

#include <Xm/Label.h>
#include <Xm/Frame.h>
#include <Xm/Xm.h>
#include "sl/sl_text_box.hh"
#include "sl/sl_tog_box.hh"
#include "sl/sl_radio_box.hh"
#include "sl/psuedo_widget.hh"
#include "sp/seis_cbar.hh"
#include "sp/seis_plot.hh"
#include "vf/vf_manager.hh"
#include "vf/vf_utilities.hh"
#include "vf/vf_update.hh"
#include "vaplots/va_semblance_plot.hh"
#include "vaplots/va_semblance_gui.hh"
#include "vaplots/va_common_params.hh"
#include "sp/seis_ctype.hh"
#include "vaplots/va_common_params_gui.hh"
#include "vaplots/va_common_movie_gui.hh"
#include "sp/seis_anno_pop.hh"
#include "sl/sl_push_box.hh"

#define    PWIDTH_DEF       4.0F
#define    IS_DEF           2.0F
#define    CNUM_DEF         1L
#define    CONNUM_DEF       8L
#define    TMIN_DEF         0.0F
#define    TMAX_DEF         1.0F
#define    VMIN_DEF         4000.0F
#define    VMAX_DEF         20000.0F
#define    PDMIN_DEF        .05F
#define    PDMAX_DEF        .98F
#define    GRADE_VERT_DEF   True
#define    GRADE_HORZ_DEF   True
#define    PLOT_CON_DEF     False
#define    AUTO_COPY_DEF    False
#define    SNAP_PICK_DEF    False
#define    FIRST_PANEL_DEF  1L
#define    SKIP_PANELS_DEF  0L
#define    TOTAL_PANELS_DEF 1L

static String  defres[]= {
    "_popup.title:               Semblance Menu",
    "*Cbar.labelString:          Color Bar  --->",
    "*Usedef.labelString:        Use Defined -->",
    "*pwidthL.labelString:       Plot Width:",
    "*pwidth.value:              4.0",
    "*isL.labelString:           Inches / Sec:",
    "*is.value:                  2.0",
    "*connumL.labelString:       # of Contours:",
    "*connum.value:              8",
    "*tminL.labelString:         Tmin:",
    "*tmin.value:                0.0",
    "*tmaxL.labelString:         Tmax:",
    "*tmax.value:                1.0",
    "*vminL.labelString:         Vmin:",
    "*vmin.value:                4000.0",
    "*vmaxL.labelString:         Vmax:",
    "*vmax.value:                20000.0",
    "*snapvelL.labelString:      Velocity:",
    "*snapvel.value:             500.0",
    "*snaptimeL.labelString:     Time:",
    "*snaptime.value:            0.10",
    "*pdminL.labelString:        Pdmin:",
    "*pdmin.value:               .052",
    "*pdmaxL.labelString:        Pdmax:",
    "*pdmax.value:               .98",
    "*startvelL.labelString:     Start Vel:",
    "*bot_timeL.labelString:     Bottom Time:",
    "*gradvert.labelString:      Grade Vertical",
    "*gradhor.labelString:       Grade Horizontal",
    "*gradvert.set:              True",
    "*gradhor.set:               True",
    "*plot_con.labelString:      Plot Contours Only",
    "*plot_con.set:              False",
    "*auto_copy.labelString:     Auto Copy Previous",
    "*auto_copy.set:             False",
    "*snap_pick.labelString:     Snap Pick",
    "*snap_pick.set:             False",
    "*plotlab.labelString:       Semblance Plot Parameters",
    "*domovie.labelString:       Set Movie Parameters",
    "*first_panelL.labelString:  First Panel:",
    "*first_panel.value:         1",
    "*skip_panelsL.labelString:  Panels To Skip:",
    "*skip_panels.value:         0",
    "*total_panelsL.labelString: Total Panels:",
    "*total_panels.value:        1",
    "*anno.labelString:         Annotation...",
    NULL, };


enum {PWIDTH, CONNUM,     VMIN,       VMAX,     PDMIN,       PDMAX,
      GRADE_VERT,         GRADE_HORZ, PLOT_CON, STARTVEL,    BOT_TIME,
      CBAR_FILE,          PREDEFINED, DO_MOVIE, FIRST_PANEL, SKIP_PANELS,
      TOTAL_PANELS,       ANNO,       AUTO_COPY,SNAP_PICK,   SNAP_VEL,
      SNAP_TIME};
//=========================================================================
//====================== Constructor ======================================
//=========================================================================
VaSemblanceGui::VaSemblanceGui( Widget            p,
                                char              *name,
                                HelpCtx           hctx,
                                VaSemblancePlot   *plot)
                : SLFPopSep(p,name,FP_DOALL,hctx,False,False),
                  _plot(plot),_sp(plot->SP()), _annotation_changed(False)

{

static SLText texts[]  = {
 {"vmin",  "range:0.00 9999999.00,default:4000.00", NULL,SLType_float,VMIN },
 {"vmax",  "range:0.00 9999999.00,default:20000.00",NULL,SLType_float,VMAX },
 {"pdmin", "range:0.00 1.00,default:0.05",          NULL,SLType_float,PDMIN },
 {"pdmax", "range:0.01 1.00,default:0.98",          NULL,SLType_float,PDMAX },
 {"connum","range:0 20,default:8",                  NULL,SLType_int,  CONNUM },
 {"pwidth","range:.1 200.0,default:4.00",           NULL,SLType_float,PWIDTH },
 {"startvel","range:0.0 9999999.00,default:5000.0", NULL,SLType_float,STARTVEL},
 {"bot_time","range:0.0 5000.0,default:1.000",      NULL,SLType_float,BOT_TIME},
};
 texts[0].target = &_vmin;
 texts[1].target = &_vmax;
 texts[2].target = &_pdmin;
 texts[3].target = &_pdmax;
 texts[4].target = &_connum;
 texts[5].target = &_pwidth;
 texts[6].target = &_startvel;
 texts[7].target= &_bot_time;

static SLText snap_texts[]  = {
 {"snapvel",  "range:0.00 9999999.00,default:500.00", NULL,SLType_float,SNAP_VEL },
 {"snaptime", "range:0.00    5000.00,default:.10",    NULL,SLType_float,SNAP_TIME},
};
 snap_texts[0].target = &_snap_vel;
 snap_texts[1].target = &_snap_time;


static SLTog togs[]  = {
         { "gradvert",  NULL, GRADE_VERT },
         { "gradhor",   NULL, GRADE_HORZ },
         { "plot_con",  NULL, PLOT_CON },
};
togs[0].target = &_grade_vert;
togs[1].target = &_grade_horz;
togs[2].target = &_plot_con;

static SLTog pick_togs[]  = {
         { "auto_copy", NULL, AUTO_COPY },
         { "snap_pick", NULL, SNAP_PICK },
};
pick_togs[0].target = &_auto_copy;
pick_togs[1].target = &_snap_pick;


static SLPush anno_button[]  = {
 { "anno",    ANNO },
};


  _first_time = True;

  setDefaultResources( p, name, defres);

  _params_box= new SLTextBox( this, "params_box", getHelpCtx(), 
                              texts, XtNumber(texts), True, 1, True );
  _params_box->setComplexNotify(this);

  _common_gui_box = new VaCommonParamsGui(this, "common", getHelpCtx(),
                                          _plot->getCommonParams(),
                                          VaPlot::SEMBLANCE);
  _plot->getCommonParams()->registerCommonGui(VaPlot::SEMBLANCE,
                                              _common_gui_box);

  _common_movie_gui_box = new VaCommonMovieGui(this, "common_movie",
                                          getHelpCtx(),
                                          _plot->getCommonParams(),
                                          VaPlot::SEMBLANCE);
  _plot->getCommonParams()->registerCommonMovieGui(VaPlot::SEMBLANCE,
                                           _common_movie_gui_box);

  
  _plot->getCommonParams()->registerSemblancePlotGui(this);

  _grade_box= new SLTogBox( this, "grade", getHelpCtx(), togs, XtNumber(togs),
                           False, False );

  make(p);

  _colorbar = new SeisCbar(topWidget(), "cbar", _sp, False, False, True);
   
  _color_control = new SeisCtype(topWidget(), "color_control", getHelpCtx(),
                                 _sp, _colorbar, True);
  _colorbar->loadToSeisPlot(); 


  _anno_but = new SLPushBox(this, "anno", getHelpCtx(),
                                anno_button, XtNumber(anno_button));
  _anno_but->setComplexNotify(this);
  _anno_pop = new SeisAnnoPop(topWidget(), "Semblance Annotation", getHelpCtx(),
                              _sp);
  _anno_pop->plotOnOK(False);
  _anno_pop->setPrimaryTimingLine(1.0F);
  _anno_pop->setSecondaryTimingLine(0.5F);
  _anno_pop->setPrimaryHeader(_plot->getActiveXheader());
  _anno_pop->setSecondaryHeader(_plot->getActiveYheader());

  _pick_box = new SLTogBox(this, "pick", getHelpCtx(), pick_togs,
                           XtNumber(pick_togs), False, False);
  _pick_box->setComplexNotify(this);

  _snap_box= new SLTextBox(this, "snap_box", getHelpCtx(), 
                           snap_texts, XtNumber(snap_texts), True, 1, True);
  _snap_box->setComplexNotify(this);
  _snap_box->setAltFocusAction(&VaSemblanceGui::handleSnapTextFields,
                               (void *)this);
  _snap_box->setAltLosingAction(&VaSemblanceGui::handleSnapTextFields,
                                (void *)this);
}


//=========================================================================
//===================== Destructor ========================================
//=========================================================================
VaSemblanceGui::~VaSemblanceGui()
{
  if(_params_box)           delete _params_box;
  if(_snap_box)             delete _snap_box;
  if(_common_gui_box)       delete _common_gui_box;
  if(_grade_box)            delete _grade_box;
  if(_pick_box)             delete _pick_box;
  if(_colorbar)             delete _colorbar;
  if(_color_control)        delete _color_control;
  if(_common_movie_gui_box) delete _common_movie_gui_box;
}


//=========================================================================
//============== The Seisplot in the window has changed ===================
//=========================================================================
void VaSemblanceGui::seisPlotChanged()
{

 printf("need to test this gui's seisPlotChanged method\n");  

 _params_box->SetValue(PWIDTH, _sp->gridWidth());
 _params_box->SetValue(CONNUM, _sp->contours());
 _params_box->SetValue(VMIN,   _sp->minImageVel());
 _params_box->SetValue(VMAX,   _sp->maxImageVel());
 _params_box->SetValue(PDMIN,  _sp->minP());
 _params_box->SetValue(PDMAX,  _sp->maxP());
   
 _grade_box->SetTog(GRADE_VERT, (Boolean)_sp->gradeVert());
 _grade_box->SetTog(GRADE_HORZ, (Boolean)_sp->gradeHorz());

 //next if needs work
 if(_sp->contours())
   _grade_box->SetTog(PLOT_CON,   True);
 else
   _grade_box->SetTog(PLOT_CON,   False);

  
  _common_movie_gui_box->setMovieOption(_sp->movie());
 //these need more work
  _common_movie_gui_box->setFirstMovie(1L);
  _common_movie_gui_box->setSkipMovies(_sp->skipFrames());
  _common_movie_gui_box->setTotalMovies(_sp->plottedFrames()); 

}


//=========================================================================
//========================= Make  =========================================
//=========================================================================
Widget VaSemblanceGui::make(Widget p)
{

   if (made()) return topWidget();
   SLFPopSep::make (p);
   return topWidget();

}


Boolean VaSemblanceGui::notifyComplex(SLDelay * obj, int ident)
{
   
   if(obj == _params_box)
     {
     if(ident == VMIN)
       {
       if(_params_box->GetFloat(VMIN) < _plot->minVelocity())
         {
         _params_box->SetValue(VMIN, _plot->minVelocity());
         _params_box->popError( 
                    "Vmin is less than the file contains");
         }
       if(_params_box->GetFloat(VMIN) >= _params_box->GetFloat(VMAX))
         {
         _params_box->SetValue(VMIN, _plot->minVelocity());
         _params_box->SetValue(VMAX, _plot->maxVelocity());
         _params_box->popError( 
                    "Vmin is greater than or equal to Vmax");
         }
       }
     
      if(ident == VMAX)
       {
       if(_params_box->GetFloat(VMAX) > _plot->maxVelocity())
         {
         _params_box->SetValue(VMAX, _plot->maxVelocity());
         _params_box->popError( 
                     "Vmax specified is greater than the file contains");
         }
       if(_params_box->GetFloat(VMAX) <= _params_box->GetFloat(VMIN))
         {
         _params_box->SetValue(VMIN, _plot->minVelocity());
         _params_box->SetValue(VMAX, _plot->maxVelocity());
         _params_box->popError( 
                    "Vmax is less than or equal to Vmin");
         }
       }    

     }
   else if(obj == _anno_but)
     {
     _anno_pop->setSeisPlot(_sp);
     _anno_pop->makeAndManage(XtParent(W()));
     _annotation_changed = True;
     //Use the following to disable or enable header word and depth options
     _anno_pop->setTextState(SeisAnnoPop::PRIMH, False);
     _anno_pop->setTextState(SeisAnnoPop::SECH, False);
     _anno_pop->setTextState(SeisAnnoPop::START_TRACE, False);
     _anno_pop->setTextState(SeisAnnoPop::TRACE_INC, False);
     //_anno_pop->setTimeDepthState(False);
     }
   else if(obj == _pick_box) {
     if(ident == SNAP_PICK) {
         if (isSnapPickSelected()) {
             _snap_box->SetSensitive(SNAP_VEL, True);
             _snap_box->SetSensitive(SNAP_TIME, True);

             // the time and velocity parameters must be known by the
             // picks class. it and this gui class don't know about each
             // other, but this class knows about the plot class,
             // which in turn knows about the pick class.
             // use the plot class to pass the info to the picks class.
             _plot->activateSnapMode(getSnapVelocity(), getSnapTime(),
                                     _common_gui_box->getTmin(),
                                     _common_gui_box->getTmax(),
                                     _vmin, _vmax);
         }
         else {
             // have plot class notify picks class of deactivation
             _snap_box->SetSensitive(SNAP_VEL, False);
             _snap_box->SetSensitive(SNAP_TIME, False);
             _plot->deactivateSnapMode();
         }
     }
   }


   return True;
}



//=========================================================================
//========================== Check for valid input ========================
//=========================================================================
Boolean VaSemblanceGui::ValidInput()
{
 Boolean stat;

 if (made()) 
   {
    stat= SLFPopSep::ValidInput();
    if (stat)
         stat= _params_box->validate();

    if ( (_vmin >= _vmax) && (stat) ) 
      {
      _params_box->popError( "vmax must be greater than vmin");
      stat= False;
      }

    if(_vmin < _plot->minVelocity())
      {
      _params_box->popError( "Vmin is less than the file contains");
      stat= False;
      }

    if(_vmax > _plot->maxVelocity())
      {
      _params_box->popError( "Vmax is greater than the file contains");
      stat= False;
      }

    if ( (_pdmin >= _pdmax) && (stat) ) 
      {
      _params_box->popError( "pdmax must be greater than pdmin");
      stat= False;
      }
   }
 else//not made yet
   { 
   stat= True;
   }

 return (stat); 
}


//=========================================================================
//===================== Take action on Ok Apply etc========================
//=========================================================================
void VaSemblanceGui::DoAction()
{
  SLFPopSep::DoAction();

  if(_annotation_changed)
    {
      _common_gui_box->annotationChanged(VaPlot::SEMBLANCE,
                                         _anno_pop->getPrimaryTimingLine(),
                                         _anno_pop->getSecondaryTimingLine(),
                                         _anno_pop->getDepthOption());
      _annotation_changed = False;
    }

  setParameters();

  if( strlen(_sp->filename())  && _sp->isPlotDisplayed()  )
    _plot->plot();

  _common_gui_box->replotAllPlots(VaPlot::SEMBLANCE);
}

//=========================================================================
//============= Common gui changes calls this for a replot      ===========
//=========================================================================
void VaSemblanceGui::commonReplot()
{
  if(!_sp->isPlotDisplayed()) return;
  setParameters();
  _plot->plot();
}


//=========================================================================
//============= Set first panel to display                      ===========
//=========================================================================
void VaSemblanceGui::setFirstPanelToPlot(long first_panel)
{
  _common_movie_gui_box->setFirstMovie(first_panel);
  setParameters();
}

//=========================================================================
//============= Set first movie panel                           ===========
//=========================================================================
long VaSemblanceGui::getFirstMoviePanel()
{
  return _common_movie_gui_box->getFirstMovie();
}

//=========================================================================
//============= Set skip movie panels                           ===========
//=========================================================================
long VaSemblanceGui::getSkipMoviePanels()
{
  return _common_movie_gui_box->getSkipMovies();
}

//=========================================================================
//===================== Set parameters for plotting =======================
//=========================================================================
void VaSemblanceGui::setParameters()
{
VfUtilities *vfu = _plot->getVfManager()->utilities();
VfUpdate *update = vfu->update();
long trace_skip = 0;

  if(!_sp->usingSelector())
    {
    trace_skip = (_common_movie_gui_box->getFirstMovie() - 1) 
               * _plot->tracesPerGroup();
    }
  else
    {
    for(int i = 0; i <_common_movie_gui_box->getFirstMovie() ; i++)
      trace_skip += _sp->getSelectorNumTraces(i);
    }


  if(!_first_time)_colorbar->loadToSeisPlot();
  _sp->setMatchHeader(6);//velocity header used for zooming etc.
  _sp->setHeaders(_anno_pop->getPrimaryHeader(), 
                  _anno_pop->getSecondaryHeader());
  _sp->setDepth(_anno_pop->getDepthOption());
  _sp->setCT(4.0);
  _sp->setRP(False);
  _sp->setRtoL(False);
  _sp->setTimingLines(_anno_pop->getPrimaryTimingLine(), 
                      _anno_pop->getSecondaryTimingLine());
  _sp->setNorm(False);
  _sp->setTI(_pwidth);
  _sp->setContours(min(_connum,_plot->_numcontourcolors));
  _sp->setIS(_common_gui_box->getIS()); 
  _sp->setTminTmax(_common_gui_box->getTmin(), _common_gui_box->getTmax());
  _sp->setMinMaxVel(_vmin, _vmax);
  _sp->setGridXYS(_vmin, _vmax, _common_gui_box->getTmin(),
                  _common_gui_box->getTmax());
  _sp->setMinMaxP(_pdmin, _pdmax);
  _sp->setGradeVert((char)_grade_vert);
  _sp->setGradeHorz((char)_grade_horz);
   
  if (_plot_con)
    {
    _sp->setPlotType(PlotImage::PlotCONTOUR);   
    }
  else
    {
    _sp->setPlotType(PlotImage::PlotSEMB);
    }

  _sp->setContoursOnly((int)_plot_con);
  _sp->setContourIncrement((_vmax - _vmin) / (_sp->contours() - 1));

  if(_common_movie_gui_box->getMovieOption())
    {
    if(_common_movie_gui_box->getTotalMovies() > 1)
      {
      _sp->setMovie(True);
      _sp->setSkipFrames(_common_movie_gui_box->getSkipMovies());
      _sp->setFrames(_common_movie_gui_box->getTotalMovies());
      _sp->setISkp(trace_skip);
      }
    else
      {
      _sp->setMovie(False);
      }
    }  
  else
    {
    _sp->setISkp(trace_skip);
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


  update->setStartvel(_startvel);
  update->setMaxtime(_bot_time);

}

//=========================================================================
//==================== Reset when new file is read ========================
//=========================================================================
void VaSemblanceGui::setToFileDefaults()
{

}


//=========================================================================
//====================== Manage ===========================================
//=========================================================================
void VaSemblanceGui::manage()
{
char textstr[100];

  //have to create color bar and set attachments here to prevent it
  //from grabbing other colors
  if(_first_time)
   {
   XtVaSetValues( _color_control->W(),
                    XmNleftAttachment,  XmATTACH_FORM,
                    XmNleftOffset,      5,
                    XmNtopAttachment,   XmATTACH_FORM,
                    XmNtopOffset,       5,
                    XmNrightAttachment, XmATTACH_FORM,
                    XmNrightOffset,     160,
                    NULL );  

   XtVaSetValues( _colorbar->W(),       
                    XmNleftAttachment,   XmATTACH_WIDGET,
                    XmNleftWidget,       _grade_box->W(),
                    XmNleftOffset,       15,
                    XmNtopAttachment,    XmATTACH_FORM, 
                    XmNrightAttachment,  XmATTACH_FORM,
                    XmNbottomAttachment, XmATTACH_WIDGET,
                    XmNbottomWidget,     bottomSeparator(),
                    XmNbottomOffset,     5,
                    XmNwidth,            150,
                    NULL);

   XtVaSetValues( _grade_box->W(), 
                    XmNbottomAttachment, XmATTACH_OPPOSITE_WIDGET,
                    XmNbottomWidget,     _params_box->W(),
                    XmNleftAttachment,   XmATTACH_WIDGET,
                    XmNleftWidget,       _params_box->W(),
                    XmNleftOffset,       50,
                    NULL);

   _plotlab= XtVaCreateManagedWidget(
                    "plotlab", xmLabelWidgetClass, topWidget(),
                    XmNleftAttachment,   XmATTACH_FORM,
                    XmNleftOffset,       10,
                    XmNtopAttachment,    XmATTACH_WIDGET,
                    XmNtopWidget,        _color_control->W(),
                    XmNtopOffset,        10,
                    NULL );

   XtVaSetValues( _params_box->W(),
                    XmNleftAttachment,   XmATTACH_FORM,
                    XmNleftOffset,       5,
                    XmNtopAttachment,    XmATTACH_WIDGET, 
                    XmNtopWidget,        _plotlab,
                    NULL);

   XtVaSetValues(_common_gui_box->W(), 
                    XmNleftAttachment,  XmATTACH_FORM,
                    XmNleftOffset,      5,
                    XmNrightAttachment, XmATTACH_OPPOSITE_WIDGET,
                    XmNrightWidget,     _params_box->W(),
                    XmNtopAttachment,   XmATTACH_WIDGET,
                    XmNtopWidget,       _params_box->W(), 
                    XmNtopOffset,       5,
                    NULL);

   XtVaSetValues( _common_movie_gui_box->W(),
                    XmNtopAttachment,    XmATTACH_WIDGET,
                    XmNtopWidget,        _common_gui_box->W(),
                    XmNtopOffset,        5,
                    XmNleftAttachment,   XmATTACH_FORM,
                    XmNleftOffset,       5,
                    XmNrightAttachment,  XmATTACH_OPPOSITE_WIDGET,
                    XmNrightWidget,      _common_gui_box->W(),
                    NULL); 
   _common_movie_gui_box->setMovieOption(False);
   _common_movie_gui_box->setSensitive(False);

   _timerangelab= XtVaCreateManagedWidget(
                    "rangelab", xmLabelWidgetClass, topWidget(),
                    XmNleftAttachment,   XmATTACH_FORM,
                    XmNleftOffset,       5,
                    XmNtopAttachment,    XmATTACH_WIDGET,
                    XmNtopWidget,        _common_movie_gui_box->W(),
                    XmNtopOffset,        10, NULL );

   _velrangelab  = XtVaCreateManagedWidget(
                    "vellab",   xmLabelWidgetClass, topWidget(),
                    XmNleftAttachment,   XmATTACH_FORM,
                    XmNleftOffset,       5,
                    XmNtopAttachment,    XmATTACH_WIDGET,
                    XmNtopWidget,        _timerangelab, 
                    NULL);

   Widget tmp=  XtVaCreateManagedWidget( "", 
                                   xmLabelWidgetClass, topWidget(),
                                   XmNtopAttachment,   XmATTACH_WIDGET,
                                   XmNtopWidget,       _velrangelab,
                                   XmNleftAttachment,  XmATTACH_FORM,
                                   XmNbottomAttachment,XmATTACH_WIDGET,
                                   XmNbottomWidget,    bottomSeparator(),
                                   XmNtopOffset,      5,
                                   XmNleftOffset,     5,
                                   XmNbottomOffset,   25,
                                   NULL);

   


   wprocShowMsg(_timerangelab, "Time min and max:");
   wprocShowMsg(_velrangelab,  "Velocity min and max:");


   XtVaSetValues( _anno_but->W(), XmNtopAttachment,  XmATTACH_WIDGET,
                             XmNtopWidget,      _grade_box->W(),
                             XmNleftAttachment, XmATTACH_OPPOSITE_WIDGET, 
                             XmNleftWidget,     _grade_box->W(), 
                             XmNtopOffset,      5, NULL);

   XtVaSetValues( _pick_box->W(), 
                    XmNtopAttachment,  XmATTACH_WIDGET,
                    XmNtopWidget,      _anno_but->W(),
                    XmNleftAttachment, XmATTACH_OPPOSITE_WIDGET, 
                    XmNleftWidget,     _grade_box->W(), 
                    XmNtopOffset,      5,
                    NULL);

   XtVaSetValues( _snap_box->W(), 
                    XmNtopAttachment,   XmATTACH_WIDGET,
                    XmNtopWidget,       _pick_box->W(),
                    XmNleftAttachment,  XmATTACH_OPPOSITE_WIDGET, 
                    XmNleftWidget,      _grade_box->W(), 
                    XmNrightAttachment, XmATTACH_WIDGET,
                    XmNrightWidget,     _colorbar->W(),
                    XmNtopOffset,      5,
                    XmNrightOffset,    5,
                    NULL);
   if (isSnapPickSelected()) {
       _snap_box->SetSensitive(SNAP_VEL, True);
       _snap_box->SetSensitive(SNAP_TIME, True);
   }
   else {
       _snap_box->SetSensitive(SNAP_VEL, False);
       _snap_box->SetSensitive(SNAP_TIME, False);
   }


   defaultButton(FP_DOAPPLY, False);
   //following lines removed...will over-ride any save defaults the user uses
   //_color_control->setColorType(SeisCtype::USEDEF);
   //_color_control->setPredef(PlotImage::STANDARD);
  }//end first time

  
  SLBase::manage();

  if (_first_time) 
    {
    setToFileDefaults();
    _first_time = False;
    _common_gui_box->notifyCommon();
    } 


  XtManageChild(topWidget());

  sprintf(textstr, "Time min and max:  %4.3f  to  %4.3f",
                     _plot->minTime(),_plot->maxTime());
  wprocShowMsg(_timerangelab, textstr);

  sprintf(textstr, "Velocity min and max:  %4.2f  to  %4.2f",
                     _plot->minVelocity(),_plot->maxVelocity());
  wprocShowMsg(_velrangelab, textstr); 

}


//=========================================================================
//==================== Reset to system defaults ===========================
//=========================================================================
void VaSemblanceGui::reloadSystemDefaults(Boolean /*do_method*/)
{

 _params_box->SetValue(PWIDTH, PWIDTH_DEF);
 _params_box->SetValue(CONNUM, CONNUM_DEF);
 _params_box->SetValue(VMIN,   VMIN_DEF);
 _params_box->SetValue(VMAX,   VMAX_DEF);
 _params_box->SetValue(PDMIN,  PDMIN_DEF);
 _params_box->SetValue(PDMAX,  PDMAX_DEF);
   
 _grade_box->SetTog(GRADE_VERT, GRADE_VERT_DEF);
 _grade_box->SetTog(GRADE_HORZ, GRADE_HORZ_DEF);
 _grade_box->SetTog(PLOT_CON,   PLOT_CON_DEF);

 _pick_box->SetTog(AUTO_COPY,  AUTO_COPY_DEF);
 _pick_box->SetTog(SNAP_PICK,  SNAP_PICK_DEF);

 _common_movie_gui_box->setMovieOption(False);

 _common_movie_gui_box->setFirstMovie(1L);
 _common_movie_gui_box->setSkipMovies(0L);
 _common_movie_gui_box->setTotalMovies(1L);

}


//=========================================================================
//====================                          ===========================
//=========================================================================
void VaSemblanceGui::UndoInput()
{
}

//=========================================================================
//====================  Update public method    ===========================
//=========================================================================
void VaSemblanceGui::updateParams(Boolean update_file_limits)
{
char textstr[100];
long possible_movies;
long first_panel_index;
VfUtilities *vfu = _plot->getVfManager()->utilities();
VfUpdate *update = vfu->update();


  //This method should be called when a file is read in or changed

  if(!_anno_pop->made())
    {
    _anno_pop->setPrimaryHeader(_plot->getActiveXheader());
    _anno_pop->setSecondaryHeader(_plot->getActiveYheader());
    }


  if(update_file_limits)
    {
    _params_box->SetValue( STARTVEL, _plot->minVelocity());
    _params_box->SetValue( BOT_TIME, _plot->maxTime());
     _params_box->SetValue( VMIN, _plot->minVelocity() );
    _params_box->SetValue( VMAX, _plot->maxVelocity() );
    _common_gui_box->setSemblanceMinMaxVelocity(_plot->minVelocity(),
                                              _plot->maxVelocity());
    }

  //Moved following up to update_file_limits if block
  //_params_box->SetValue( VMIN, _plot->minVelocity() );
  //_params_box->SetValue( VMAX, _plot->maxVelocity() );
  //_common_gui_box->setSemblanceMinMaxVelocity(_plot->minVelocity(),
  //                                            _plot->maxVelocity());

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
  _common_movie_gui_box->setFileMaxMovies(VaPlot::SEMBLANCE,
                                          _plot->numberLocationsInFile());

  _sp->setNPlt(_plot->tracesPerGroup());
  _sp->setISkp( (_common_movie_gui_box->getFirstMovie() - 1) * 
                 _plot->tracesPerGroup());
  _sp->setNdo(1);
  _sp->setNSkp(0);
  _sp->setTdec(1);

  //set common parameters for gvs,cmp and semblance
  if(update_file_limits)
    {
    _common_gui_box->setFileTmin(VaPlot::SEMBLANCE,_plot->minTime());
    _common_gui_box->setFileTmax(VaPlot::SEMBLANCE,_plot->maxTime());
    _common_gui_box->haveFileLimits(VaPlot::SEMBLANCE, True);
    _common_gui_box->setTmin(_common_gui_box->getFileTminLimit());
    _common_gui_box->setTmax(_common_gui_box->getFileTmaxLimit());
    }

  _common_gui_box->setIS(_common_gui_box->getIS());
  
  _common_gui_box->notifyCommon();

  //update the range widgets
  if(!_first_time)
    {
    sprintf(textstr, "Time min and max:  %4.3f  to  %4.3f",
                     _plot->minTime(),_plot->maxTime());
    wprocShowMsg(_timerangelab, textstr);

    sprintf(textstr, "Velocity min and max:  %4.2f  to  %4.2f",
                      _plot->minVelocity(),_plot->maxVelocity());
    wprocShowMsg(_velrangelab, textstr);
    }
}



void VaSemblanceGui::annotationChanged(float primary, float secondary, 
                                       Boolean depth)
{
  _anno_pop->setPrimaryTimingLine(primary);
  _anno_pop->setSecondaryTimingLine(secondary);
  _anno_pop->setDepthOption(depth);
}

Boolean VaSemblanceGui::isAutoCopySelected() const {
    return _pick_box->IsSelected(AUTO_COPY);
}

Boolean VaSemblanceGui::isSnapPickSelected() const {
    return _pick_box->IsSelected(SNAP_PICK);
}

float VaSemblanceGui::getSnapVelocity() const {
    return _snap_box->GetFloat(SNAP_VEL);
}

float VaSemblanceGui::getSnapTime() const {
    return _snap_box->GetFloat(SNAP_TIME);
}

void VaSemblanceGui::handleSnapTextFields(void *instance, long ident) {

    if (instance) {
        (static_cast<VaSemblanceGui *> (instance))->handleSnapTextFields(ident);
    }
}

void VaSemblanceGui::handleSnapTextFields(long ident) {

    SLTextBox::TextReason reason = _snap_box->lastNotifyReason();

    switch (reason) {
    case SLTextBox::TextNone:
        break;
    case SLTextBox::GainingFocus:
    case SLTextBox::LosingFocus:
    case SLTextBox::Activate:
        // somewhat wasteful, but following op to activate snap mode
        // is not expensive, so do it for any of these reasons.

        // the time and velocity parameters must be known by the
        // picks class. it and this gui class don't know about each
        // other, but this class knows about the plot class,
        // which in turn knows about the pick class.
        // use the plot class to pass the info to the picks class.
        _plot->activateSnapMode(getSnapVelocity(), getSnapTime(),
                                _common_gui_box->getTmin(),
                                _common_gui_box->getTmax(),
                                _vmin, _vmax);
        break;
    default:
        assert(False);
    }
}
