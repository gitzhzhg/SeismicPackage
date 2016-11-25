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
//========== Iso velocity plot gui class                         ==========
//========== Author Michael L. Sherrill 09/97                    ==========
//=========================================================================

// $Id: va_iso_gui.cc,v 1.2 2004/04/05 18:12:11 wjdone Exp $
// $Name:  $



#include <Xm/Label.h>
#include <Xm/Frame.h>
#include <Xm/Form.h>
#include <stdio.h>
#include "vf/vf_file_base.hh"
#include "vaplots/va_plot_control.hh"
#include "vaplots/va_iso_plot.hh"
#include "vaplots/va_iso_gui.hh"
#include "vaplots/va_iso_plotter.hh"
#include "sp/seis_plot.hh"
#include "sl/sl_text_box.hh"
#include "sl/sl_tog_box.hh"
#include "sl/sl_radio_box.hh"
#include "sp/seis_cbar.hh"
#include "sp/seis_ctype.hh"
#include "sp/contour_gui.hh"
#include "vf/vf_dataset.hh"
#include "vf/vf_manager.hh"
#include "vf/vf_constants.hh"
#include "named_constants.h"


static String  defres[]= {
    "_popup.title:              Iso Velocity Menu",
    "*pwidthL.labelString:      Plot Width:",
    "*pwidth.value:             5",
    "*isL.labelString:          Plot Height:",
    "*is.value:                 5",
    "*connumL.labelString:      # of Contours:",
    "*connum.value:             8",
    "*x1L.labelString:          Left:",
    "*x1.value:                 0",
    "*x2L.labelString:          Right:",
    "*x2.value:                 100",
    "*y1L.labelString:          Top:",
    "*y1.value:                 0",
    "*y2L.labelString:          Bottom:",
    "*y2.value:                 100",
    "*tminL.labelString:        Tmin:",
    "*tmin.value:               0",
    "*tmaxL.labelString:        Tmax:",
    "*tmax.value:               1",
    "*vminL.labelString:        Vmin:",
    "*vmin.value:               5000",
    "*vmaxL.labelString:        Vmax:",
    "*vmax.value:               20000",
    "*x_annoL.labelString   :      X Annot. Incr:",
    "*x_anno.value:             1",
    "*primary_timingL.labelString: Primary Timing:",
    "*primary_timing.value:     1.0",
    "*secondary_timingL.labelString: Secondary Timing:",
    "*secondary_timing.value:   0.5",
    "*gradhor.labelString:      Grade Horizontal",
    "*gradver.labelString:      Grade Interval Velocities Verticaly",
    "*underlay.labelString:     Underlay GVS/CVST Data",
    "*plotlab.labelString:      Plot Parameters",
    "*linelab.labelString:      Line Selection",
    "*ptypebox.T0.labelString:  NMO",
    "*ptypebox.T1.labelString:  RMS",
    "*ptypebox.T2.labelString:  AVG",
    "*ptypebox.T3.labelString:  INT",
    "*timelab.labelString:      Versus Time->",
    "*depthlab.labelString:     Versus Dpth->",
    "*ptypelab.labelString:     Plot Type",
    "*ptypebox.tnmo.labelString: ",
    "*ptypebox.trms.labelString: ",
    "*ptypebox.drms.labelString: ",
    "*ptypebox.tavg.labelString: ",
    "*ptypebox.davg.labelString: ",
    "*ptypebox.tint.labelString: ",
    "*ptypebox.dint.labelString: ",
    "*linebox.inline.labelString:    Inline----->",
    "*linebox.crossline.labelString: Crossline-->",
    "*linebox.timeslice.labelString: Time Slice->",
    "*binbox.ybinL.labelString:      Ybin:",
    "*binbox.xbinL.labelString:      Xbin:",
    "*binbox.timeL.labelString:      Time:",
    "*depthlab.alignment:        ALIGNMENT_BEGINNING",
    "*timelab.alignment:         ALIGNMENT_BEGINNING",
    "*ptypebox.numColumns:       4",
    "*domovie.labelString:       Set Movie Parameters",
    "*first_panelL.labelString:  First Panel:",
    "*first_panel.value:         1",
    "*skip_panelsL.labelString:  Panels To Skip:",
    "*skip_panels.value:         0",
    "*total_panelsL.labelString: Total Panels:",
    "*total_panels.value:        1",
    NULL, };



//=========================================================================
//==================== Constructor                        =================
//=========================================================================
VaIsoGui::VaIsoGui( Widget            p,
                    char              *name,
                    HelpCtx           hctx,
                    VaIsoPlot        *plot)
                : SLFPopSep(p,name,FP_DOALL,hctx,False,False),
                  _plot(plot),_sp(plot->SP())

{
static SLText texts[]  = {
 {"pwidth", "range:0.1 1000.0,default:8.0",NULL, SLType_float,PWIDTH},
 {"is",     "range:0.1 1000.0,default:4.0", NULL, SLType_float, PHEIGHT},
 {"x1",   NULL, NULL, SLType_float, X1 },
 {"x2",   NULL, NULL, SLType_float, X2 },
 {"y1",   NULL, NULL, SLType_float, Y1 },
 {"y2",   NULL, NULL, SLType_float, Y2 },
 {"tmin", NULL, NULL, SLType_float, TMIN },
 {"tmax", NULL, NULL, SLType_float, TMAX },
 {"vmin", NULL, NULL, SLType_float, VMIN },
 {"vmax", NULL, NULL, SLType_float, VMAX },
 {"x_anno", NULL, NULL, SLType_int,   X_ANNO },
 {"primary_timing", NULL, NULL, SLType_float, PRIMARY_TIMING },
 {"secondary_timing", NULL, NULL, SLType_float, SECONDARY_TIMING },
};
texts[0].target = &_pwidth;
texts[1].target = &_pheight;
texts[2].target = &_x1;
texts[3].target = &_x2;
texts[4].target = &_y1;
texts[5].target = &_y2;
texts[6].target = &_tmin;
texts[7].target = &_tmax;
texts[8].target = &_vmin;
texts[9].target = &_vmax;
texts[10].target= &_x_annotation_increment;
texts[11].target= &_primary_timing;
texts[12].target= &_secondary_timing;


static SLText bins[]  = {
  {"ybin", NULL, NULL, SLType_float, YBIN },
  {"xbin", NULL, NULL, SLType_float, XBIN },
  {"time", NULL, NULL, SLType_float, TIME },
};
bins[0].target = &_ybin;
bins[1].target = &_xbin;
bins[2].target = &_time;

static SLTog togs[]  = {
         { "gradhor",  NULL,  GRADEH },
         { "gradver",  NULL,  GRADEV },
         { "underlay", NULL,  UNDERLAY},         
       };
togs[0].target = &_grad_hor;
togs[1].target = &_grad_ver;
togs[2].target = &_underlay;

static SLRadio rads[]  = {
  { "tnmo",       VTNM },//time nmo
  { "SLBlankSpace", -1 },//blank radio
  { "trms",       VTRM },//time rms
  { "drms",       VZRM },//depth rms
  { "tavg",       VTAV },//time average
  { "davg",       VZAV },//depth average
  { "tint",       VTIN },//time interval
  { "dint",       VZIN },//depth interval
       };

static SLRadio lines[]  = {
  { "inline",       VaIsoPlot::INLINE    },
  { "crossline",    VaIsoPlot::CROSSLINE },
  { "timeslice",    VaIsoPlot::TIMESLICE },
};

static SLTog movietog[]  = {
         { "domovie", NULL, DO_MOVIE },
};
movietog[0].target = &_do_movie;


static SLText movietexts[]  = {
 {"first_panel",  NULL, NULL, SLType_float,   FIRST_PANEL},
 {"skip_panels",  NULL, NULL, SLType_float,   SKIP_PANELS},
 {"total_panels", NULL, NULL, SLType_int,     TOTAL_PANELS},
};
 movietexts[0].target = &_first_panel;
 movietexts[1].target = &_skip_panels;
 movietexts[2].target = &_total_panels;


  _first_time            = True;
  _plot_type             = VTNM;
  _plotted_vel_type      = VTNM;
  _number_y_lines        = 0;
  _vel_request_list      = NULL;
  _vel_ylines_list       = NULL;
  _functions_in_frame    = NULL;
  _time_of_frame = (float *)calloc(1,((int)( sizeof(float))));
  _vfd = _plot->vfManager()->activeDataset(); 
  _time                  = 0.0;
  _was_depth             = False;
  _was_time              = False;
  _oldtval               = 0.0;
  _first_panel           = 1.0;
  _skip_panels           = 0.0;
  
  
  _sp->setDoPercent(False);
  setDefaultResources( p, name, defres);

  _params= new SLTextBox( this, "params", getHelpCtx(), 
                            texts, XtNumber(texts), True, 1, True, False );
  _params->setAltLosingAction( (SLTextfunc)paramLosingFocusAction, this );
  _params->setAltFocusAction ( (SLTextfunc)paramFocusAction, this );

  _ptypebox= new SLRadioBox( this, "ptypebox", getHelpCtx(), 
                             rads, XtNumber(rads), NULL, True, True );
  _ptypebox->setAltChoiceAction( (SLRadioButtonfunc)plotTypeAction, this );


  _grade= new SLTogBox( this, "grade", getHelpCtx(), togs, XtNumber(togs), 
                        False, False );
  _grade->setComplexNotify(this);

  _linebox= new SLRadioBox( this, "linebox", getHelpCtx(), 
                            lines, XtNumber(lines), NULL, True, False );
  _linebox->setAltChoiceAction( (SLRadioButtonfunc)lineAction, this );
  _linebox->SetRadio(VaIsoPlot::INLINE); 
  _line_type = VaIsoPlot::INLINE;

  _binbox= new SLTextBox( this, "binbox", getHelpCtx(),
                            bins, XtNumber(bins), True, 1, True, False );
  _binbox->setAltLosingAction( (SLTextfunc)binLosingFocusAction, this );
  _binbox->setAltFocusAction ( (SLTextfunc)binFocusAction, this );

  
  _movie_tog= new SLTogBox( this, "movie_tog",getHelpCtx(),
                           movietog, XtNumber(movietog),False, False );
  _movie_tog->setComplexNotify(this);

  _movie_box = new SLTextBox( this, "movie_box", getHelpCtx(), 
                              movietexts, XtNumber(movietexts), True, 1, True);
  _movie_box->setComplexNotify(this);

  _contour_gui = new ContourGui(this, "contour_gui", _sp, getHelpCtx());


  _user_visited     = False;
  _grad_hor         = True;
  _grad_ver         = False;
  _dont_plot_yet    = False;
  _data_initialized = False;
  _x_annotation_increment = 1;
  //make(p);

  _colorbar = new SeisCbar(this, "cbar", _sp, False, False, True);
   
  _color_control = new SeisCtype(this, "color_control", getHelpCtx(),
                                  _sp, _colorbar, True);
   
  _colorbar->setPredef(1);
  _colorbar->loadToSeisPlot();
}


//=========================================================================
//===================== Destructor ========================================
//=========================================================================
VaIsoGui::~VaIsoGui()
{
  if(_params) delete _params;
  if(_ptypebox) delete _ptypebox;
  if(_grade) delete _grade;
  if(_linebox) delete _linebox;
  if(_binbox) delete _binbox;
  if(_colorbar) delete _colorbar;
  if(_color_control) delete _color_control;
}


//=========================================================================
//============== The Seisplot in the window has changed ===================
//=========================================================================
void VaIsoGui::seisPlotChanged()
{
  printf("need to test this gui's seisPlotChanged method\n");
 _params->SetValue(PWIDTH, _sp->gridWidth());
 _params->SetValue(PHEIGHT,_sp->gridHeight());
 _params->SetValue(TMIN,   _sp->tmin());
 _params->SetValue(TMAX,   _sp->tmax());
 _params->SetValue(VMIN,   _sp->minColorAmp());
 _params->SetValue(VMAX,   _sp->maxColorAmp());
 _params->SetValue(X_ANNO, _sp->lblInc());
 _params->SetValue(X1,     _sp->gridX1());
 _params->SetValue(X2,     _sp->gridX2());
 _params->SetValue(Y1,     _sp->gridY1());
 _params->SetValue(Y2,     _sp->gridY2());
 _params->SetValue(PRIMARY_TIMING,   (float)_sp->primTimingLine());
 _params->SetValue(SECONDARY_TIMING, (float)_sp->secTimingLine());

 _grade->SetTog(GRADEH, (Boolean)_sp->gradeHorz());

 //next needs more work
 _ptypebox->SetRadio(VTNM);

 _movie_tog->SetTog(DO_MOVIE, _sp->movie());

 //these need more work
 _movie_box->SetValue(FIRST_PANEL,  1.0F);
 _movie_box->SetValue(SKIP_PANELS,  _sp->skipFrames());
 _movie_box->SetValue(TOTAL_PANELS, _sp->plottedFrames());
}



//=========================================================================
//==================== Make                               =================
//=========================================================================
Widget VaIsoGui::make(Widget p)
{

  if ( made() ) return topWidget();
  SLFPopSep::make (p);
  p = wParent ();


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
                    XmNleftWidget,       _grade->W(),
                    XmNleftOffset,       10,
                    XmNtopAttachment,    XmATTACH_FORM, 
                    XmNrightAttachment,  XmATTACH_FORM,
                    XmNbottomAttachment, XmATTACH_WIDGET,
                    XmNbottomWidget,     bottomSeparator(),
                    XmNbottomOffset,     5,
                    XmNwidth,            150,
                    NULL); 


  _plotlab= XtVaCreateManagedWidget(
                    "plotlab", xmLabelWidgetClass, topWidget(),
                    XmNleftAttachment,   XmATTACH_FORM,
                    XmNleftOffset,       50,
                    XmNtopAttachment,    XmATTACH_WIDGET,
                    XmNtopWidget,        _color_control->W(),
                    XmNtopOffset,        10, NULL );


  XtVaSetValues( _params->W(),
                    XmNleftAttachment,   XmATTACH_FORM,
                    XmNleftOffset,       5,
                    XmNtopAttachment,    XmATTACH_WIDGET, 
                    XmNtopWidget,        _plotlab,NULL);


  Widget ptypelab= XtVaCreateManagedWidget(
                    "ptypelab", xmLabelWidgetClass, topWidget(),
                    XmNleftAttachment,   XmATTACH_OPPOSITE_WIDGET,
                    XmNleftWidget,       _ptypebox->W(),
                    XmNleftOffset,       45,
                    XmNbottomAttachment, XmATTACH_WIDGET,
                    XmNbottomWidget,     _ptypebox->W(), NULL);

  Widget timelab=  XtVaCreateManagedWidget(
                    "timelab", xmLabelWidgetClass, topWidget(),
                    XmNrightAttachment,   XmATTACH_WIDGET,
                    XmNrightWidget,       _ptypebox->W(),
                    XmNtopAttachment,    XmATTACH_OPPOSITE_WIDGET, 
                    XmNtopWidget,        _ptypebox->W(),
                    XmNtopOffset,        20, NULL);

  Widget depthlab= XtVaCreateManagedWidget(
                    "depthlab", xmLabelWidgetClass, topWidget(),
                    XmNrightAttachment,   XmATTACH_WIDGET,
                    XmNrightWidget,       _ptypebox->W(),
                    XmNbottomAttachment, XmATTACH_OPPOSITE_WIDGET,
                    XmNbottomWidget,     _ptypebox->W(),
                    XmNbottomOffset,     5, NULL);

  
  XtVaSetValues( _ptypebox->W(),
                    XmNleftAttachment,   XmATTACH_WIDGET,
                    XmNleftWidget,       _params->W(),
                    XmNleftOffset,       120,
                    XmNtopAttachment,    XmATTACH_WIDGET, 
                    XmNtopWidget,        _color_control->W(),
                    XmNtopOffset,        20, NULL);
  _ptypebox->SetRadio( VTNM );


  XtVaSetValues( _grade->W(), 
                    XmNtopAttachment,    XmATTACH_WIDGET,
                    XmNtopWidget,        _ptypebox->W(),
                    XmNleftAttachment,   XmATTACH_OPPOSITE_WIDGET,
                    XmNleftWidget,       _ptypebox->W(), NULL );
  _grade->SetTog(GRADEH,  True);
  XtSetSensitive( _grade->TogW(GRADEV),  False); 

  _linelab= XtVaCreateManagedWidget(
                    "linelab", xmLabelWidgetClass, topWidget(),
                    XmNbottomAttachment, XmATTACH_WIDGET,
                    XmNbottomWidget,     _linebox->W(),
                    XmNtopAttachment,    XmATTACH_WIDGET,
                    XmNtopWidget,        _grade->W(),
                    XmNleftAttachment,   XmATTACH_WIDGET,
                    XmNleftWidget,       _params->W(),
                    XmNleftOffset,       100,  NULL );

    
  XtVaSetValues( _linebox->W(), 
                    XmNbottomAttachment, XmATTACH_OPPOSITE_WIDGET,
                    XmNbottomWidget,     _params->W(),
                    XmNleftAttachment,   XmATTACH_WIDGET,
                    XmNleftWidget,       _params->W(),
                    XmNleftOffset,       15,
                    NULL); 
  XtVaSetValues(_linebox->GetRadioWidget(VaIsoPlot::CROSSLINE),
                    XmNmarginHeight,     3,NULL);
  

  XtVaSetValues( _binbox->W(), 
                    XmNtopAttachment,    XmATTACH_WIDGET,
                    XmNtopWidget,        _linelab,
                    XmNleftAttachment,   XmATTACH_WIDGET,
                    XmNleftWidget,       _linebox->W(),
                    XmNbottomAttachment, XmATTACH_OPPOSITE_WIDGET,
                    XmNbottomWidget,     _linebox->W(), NULL); 
  
  
   XtVaSetValues( _contour_gui->W(), 
                    XmNtopAttachment,    XmATTACH_WIDGET,
                    XmNtopWidget,        _linebox->W(),
                    XmNleftAttachment,   XmATTACH_OPPOSITE_WIDGET,
                    XmNleftWidget,       _linebox->W(), 
                    XmNrightAttachment,  XmATTACH_OPPOSITE_WIDGET,
                    XmNrightWidget,      _binbox->W(),NULL);


   XtVaSetValues( _movie_tog->W(),
                    XmNleftAttachment,   XmATTACH_FORM,
                    XmNleftOffset,       5,
                    XmNtopAttachment,    XmATTACH_WIDGET,
                    XmNtopWidget,        _params->W(),
                    NULL ); 
   _movie_tog->SetTog( DO_MOVIE, False );
   

   XtVaSetValues( _movie_box->W(),
                    XmNtopAttachment,    XmATTACH_NONE,
                    XmNleftAttachment,   XmATTACH_FORM,                
                    XmNleftOffset,       5,
                    XmNtopAttachment,    XmATTACH_WIDGET, 
                    XmNtopWidget,        _movie_tog->W(),
                    NULL);

   _rangelab= XtVaCreateManagedWidget(
                    "rangelab", xmLabelWidgetClass, topWidget(),
                    XmNtopAttachment,    XmATTACH_WIDGET,
                    XmNtopWidget,        _movie_box->W(),
                    XmNleftAttachment,   XmATTACH_FORM,
                    XmNleftOffset,       20, NULL);

   _vellab  = XtVaCreateManagedWidget(
                   "vellab",   xmLabelWidgetClass, topWidget(),
                    XmNtopAttachment,    XmATTACH_WIDGET,
                    XmNtopWidget,        _rangelab,
                    XmNleftAttachment,   XmATTACH_FORM,
                    XmNleftOffset,       20,  
                    NULL);

   Widget tmp=  XtVaCreateManagedWidget( "", 
                                   xmLabelWidgetClass, topWidget(),
                                   XmNtopAttachment,   XmATTACH_WIDGET,
                                   XmNtopWidget,       _vellab,
                                   XmNleftAttachment,  XmATTACH_FORM,
                                   XmNbottomAttachment,XmATTACH_WIDGET,
                                   XmNbottomWidget,    bottomSeparator(),
                                   XmNtopOffset,      5,
                                   XmNleftOffset,     5,
                                   XmNbottomOffset,   25,
                                   NULL);


   wprocShowMsg(_rangelab, "Time Range:");
   wprocShowMsg(_vellab,   "Velocity Range:");

  //following lines removed...will over-ride any save defaults the user uses
  //_color_control->setColorType(SeisCtype::USEDEF);
  //_color_control->setPredef(PlotImage::STANDARD);


  return topWidget();
}



Boolean VaIsoGui::notifyComplex(SLDelay *obj, int ident)
{
  if(obj == _movie_tog)//movie tog action
    {
    if(_movie_tog->IsSelected(DO_MOVIE))
      {
      XtSetSensitive(_movie_box->W(), True);
      XtSetSensitive(_grade->TogW(UNDERLAY), False);
      _grade->SetTog(UNDERLAY,  False);
      }
    else
      {
      XtSetSensitive(_movie_box->W(), False);
      XtSetSensitive(_grade->TogW(UNDERLAY), True);
      }
    }

   if(obj == _movie_box)
    {
    if(_movie_tog->IsSelected(DO_MOVIE)) setMovieParameters(ident);
    }

  if(obj == _grade)
    {
    if(_grade->IsSelected(UNDERLAY))
      _iso_plotter->setColorOptions(VaPlot::GVS,False);
    else
      _iso_plotter->setColorOptions(VaPlot::GVS,True);    
    }

   return True;
}


//=========================================================================
//==================== Validate parameters                =================
//=========================================================================
Boolean VaIsoGui::ValidInput()
{
Boolean stat= True;

 if (made()) 
   {
   stat= SLFPopSep::ValidInput();
   if (stat) stat= _params->validate();
   }

 return (stat); 
}


//=========================================================================
//===================== Set color options based on underlay usage  ========
//=========================================================================
void VaIsoGui::setColorOptions(Boolean turn_on)
{
  if(!made()) return;
  if(turn_on)
    {
    XtSetSensitive(_grade->TogW(UNDERLAY), True);
    }
  else
    {
    XtSetSensitive(_grade->TogW(UNDERLAY), False);
    _grade->SetTog(UNDERLAY,False);
    }
}

//=========================================================================
//==================== Handle line value changes          =================
//=========================================================================
void VaIsoGui::lineAction( void *data, long which )
{
VaIsoGui *obj = (VaIsoGui *)data;

 
 switch(which)
   {
   case VaIsoPlot::INLINE:
               XtSetSensitive( obj->_params->LabW(X1), True);
               XtSetSensitive( obj->_params->LabW(X2), True);
               XtSetSensitive( obj->_params->LabW(Y1), False);
               XtSetSensitive( obj->_params->LabW(Y2), False);
               XtSetSensitive( obj->_grade->TogW(UNDERLAY), True);
               obj->_line_type = VaIsoPlot::INLINE;
               if(!obj->_first_time)
                 {
                 obj->_movie_box->SetValue(FIRST_PANEL, 1.0F);
                 obj->_movie_box->SetValue(SKIP_PANELS, 0.0F);
                 wprocShowMsg(obj->_movie_box->LabW("first_panel"),
                                                    "First Panel:");
                 wprocShowMsg(obj->_movie_box->LabW("skip_panels"),
                                                    "Panels To Skip:");
                 }
               obj->setMovieParameters(FIRST_PANEL);
               XtSetSensitive(obj->_grade->TogW(UNDERLAY), True);
               break;
   case VaIsoPlot::CROSSLINE:
               XtSetSensitive( obj->_params->LabW(X1), False);
               XtSetSensitive( obj->_params->LabW(X2), False);
               XtSetSensitive( obj->_params->LabW(Y1), True);
               XtSetSensitive( obj->_params->LabW(Y2), True);
               XtSetSensitive( obj->_grade->TogW(UNDERLAY), True);
               obj->_line_type = VaIsoPlot::CROSSLINE;
               if(!obj->_first_time)
                 {
                 obj->_movie_box->SetValue(FIRST_PANEL, 1.0F);
                 obj->_movie_box->SetValue(SKIP_PANELS, 0.0F);
                 wprocShowMsg(obj->_movie_box->LabW("first_panel"),
                                                    "First Panel:");
                 wprocShowMsg(obj->_movie_box->LabW("skip_panels"),
                                                    "Panels To Skip:");
                 }
               obj->setMovieParameters(FIRST_PANEL);
               XtSetSensitive(obj->_grade->TogW(UNDERLAY), True);
               break;
   case VaIsoPlot::TIMESLICE:
               XtSetSensitive( obj->_params->LabW(X1),  True);
               XtSetSensitive( obj->_params->LabW(X2), True);
               XtSetSensitive( obj->_params->LabW(Y1),   True);
               XtSetSensitive( obj->_params->LabW(Y2),True);
               XtSetSensitive( obj->_grade->TogW(UNDERLAY), False);
               obj->_line_type = VaIsoPlot::TIMESLICE;
               if(!obj->_first_time)
                 {
                 obj->_movie_box->SetValue(FIRST_PANEL, 0.0F);
                 obj->_movie_box->SetValue(SKIP_PANELS, 0.5F); 
                 wprocShowMsg(obj->_movie_box->LabW("first_panel"),
                                                    "First Time:");
                 wprocShowMsg(obj->_movie_box->LabW("skip_panels"),
                                                    "Time Increment:");
                 }
               obj->setMovieParameters(FIRST_PANEL);
               XtSetSensitive(obj->_grade->TogW(UNDERLAY), False);
               obj->_grade->SetTog(UNDERLAY,False);
               break;
  }

}




//=========================================================================
//==================== Store previous parameter variables =================
//=========================================================================
void VaIsoGui::plotTypeAction(void *data, long which )
{
XmString xmstr1, xmstr2;
VaIsoGui *obj = (VaIsoGui *)data;
Widget text1 = obj->_linebox->GetRadioWidget(VaIsoPlot::TIMESLICE);
Widget text2 = obj->_binbox->LabW(TIME);
 
  obj->_vfd = obj->_plot->vfManager()->activeDataset(); 
  obj->_plot_type = (int)which;

  //note when these change need to get new tmin,tmax,vmin,vmax based on
  //the type selected from Tom
 switch(which)
   {
   case VTNM: 
   case VTAV:
   case VTIN:
   case VTRM:
            xmstr1 = XmStringCreateLtoR("Time Slice->",
                                         XmSTRING_DEFAULT_CHARSET);
            xmstr2 = XmStringCreateLtoR("Time:",
                                         XmSTRING_DEFAULT_CHARSET);
            if(obj->_was_depth)
              {
               obj->_olddval = obj->_time;
               obj->_binbox->SetValue(TIME,float(obj->_oldtval));
	      }
            obj->_was_time = True;
            obj->_was_depth= False;              
            break;
   default:   //depth images  
            xmstr1 = XmStringCreateLtoR("Dpth Slice->",
                                        XmSTRING_DEFAULT_CHARSET);
            xmstr2 = XmStringCreateLtoR("Dpth:",
                                        XmSTRING_DEFAULT_CHARSET);
            if(obj->_was_time)
	      {
               obj->_oldtval = obj->_time;
               obj->_binbox->SetValue(TIME,float(obj->_olddval));
              } 
            obj->_was_time = False;
            obj->_was_depth= True;
  }

  if(which != VTIN && which != VZIN)
    {
    XtSetSensitive( obj->_grade->TogW(GRADEV), False);
    obj->_grade->SetTog(GRADEV,  False);
    }
  else
    {
    XtSetSensitive( obj->_grade->TogW(GRADEV), True);
    }

  XtVaSetValues( text1, XmNlabelString, xmstr1, NULL);
  XtVaSetValues( text2, XmNlabelString, xmstr2, NULL);
  XmStringFree(xmstr1);
  XmStringFree(xmstr2);


  obj->_params->SetValue( VMIN, obj->_vfd->minimumOrdinate(obj->getPlotType()));
  obj->_params->SetValue( VMAX, obj->_vfd->maximumOrdinate(obj->getPlotType()));
  obj->_params->SetValue( TMIN, obj->_vfd->minimumAbscissa(obj->getPlotType()));
  obj->_params->SetValue( TMAX, obj->_vfd->maximumAbscissa(obj->getPlotType()));
  obj->_binbox->SetValue( TIME, obj->_vfd->minimumAbscissa(obj->getPlotType()));
 
  obj->_contour_gui->setContourMinValue(
                                obj->_vfd->minimumOrdinate(obj->getPlotType()));
  obj->_contour_gui->setContourMaxValue(
                                obj->_vfd->maximumOrdinate(obj->getPlotType()));
}


//=========================================================================
//==================== Store previous parameter variables =================
//=========================================================================
void VaIsoGui::paramFocusAction( void *data, long which )
{
VaIsoGui *obj = (VaIsoGui *)data;

  if(which != VMIN && which != VMAX)return;
  obj->_orig_vmin = obj->_vmin;
  obj->_orig_vmax = obj->_vmax;
}

//=========================================================================
//==================== Handle parameter box changes =======================
//=========================================================================
void VaIsoGui::paramLosingFocusAction( void *data, long which )
{
VaIsoGui *obj = (VaIsoGui *)data;

  if(which != VMIN && which != VMAX)return;

  if(obj->_vmin != obj->_orig_vmin || obj->_vmax != obj->_orig_vmax)
    { 
    obj->_sp->setMinColorAmp(obj->_vmin);
    obj->_sp->setMaxColorAmp(obj->_vmax);
    }

}


//=========================================================================
//==================== Store currrent bin values ==========================
//=========================================================================
void VaIsoGui::binFocusAction( void *data, long /*which*/ )
{
VaIsoGui *obj = (VaIsoGui *)data;

  obj->_oldt = obj->_time;
  obj->_oldx = obj->_xbin;
  obj->_oldy = obj->_ybin;
}


//=========================================================================
//==================== Handle Bin changes       ===========================
//=========================================================================
void VaIsoGui::binLosingFocusAction( void *data, long which )
{
VaIsoGui *obj = (VaIsoGui *)data;
float tmin, tmax;


 switch(which)
   {
   case YBIN:
     if(obj->_oldy != obj->_ybin)
       {
       obj->_oldy = obj->_ybin;
       obj->_linebox->SetRadio(VaIsoPlot::INLINE);
       obj->findFirstMoviePanel();
       obj->_movie_box->SetValue(FIRST_PANEL, obj->_first_panel);
       }
     break;

   case XBIN:
     if(obj->_oldx != obj->_xbin)
       {
       obj->_oldx =  obj->_xbin;
       obj->_linebox->SetRadio(VaIsoPlot::CROSSLINE);
       obj->findFirstMoviePanel();
       obj->_movie_box->SetValue(FIRST_PANEL, obj->_first_panel);
       }
     break;
  
   case TIME:
     tmin = obj->_vfd->minimumAbscissa(obj->getPlotType());
     tmax = obj->_vfd->maximumAbscissa(obj->getPlotType());
     if(obj->_time < tmin) obj->_time = tmin;
     if(obj->_time > tmax) obj->_time = tmax;
     if(obj->_oldt !=  obj->_time)
       {
       obj->_oldt =  obj->_time;
       obj->_linebox->SetRadio(VaIsoPlot::TIMESLICE);
       }
     obj->_movie_box->SetValue(FIRST_PANEL, obj->_time);
     break;
   }


   obj->setMovieParameters(FIRST_PANEL); 

}

//=========================================================================
//==================== Take action on main push buttons ===================
//=========================================================================
void VaIsoGui::DoAction()
{
int stat;

  _vfd = _plot->vfManager()->activeDataset(); 
  if(!reconcileParameters()) return;
  SLFPopSep::DoAction();
  setParameters();
  
  if(_dont_plot_yet == True || _vfd->numVelocityFunctions() == 0) return;
  stat = _plot->plot();
  if(stat == PlotImage::PlotSuccess)
    {
    _plotted_vel_type = _plot_type;
    _plot->showIsoWindow(True);
    }
} 

//=========================================================================
//===================== Set parameters for plotting =======================
//=========================================================================
void VaIsoGui::setParameters()
{

  
  _contour_gui->setParameters(_plot->SP());

  //If ranges are equal, pad them slightly so there will be width and height
  //to the plot
  if(_vmin == _vmax) _vmax += .5;
  if(_tmin == _tmax) _tmax += .5;
  if(_x1 == _x2) _x2 += .5;
  if(_y1 == _y2) _y2 += .5; 

  if(_user_visited)_colorbar->loadToSeisPlot();
  _sp->setPlotType(PlotImage::PlotARRAY);  
  _sp->setNorm(PlotImage::EXTERNALNORM);
  _sp->setExternalAmp(max(_vmin,_vmax));
  _sp->setTI(_pwidth); 
  _sp->setIS(_pheight);
  //following 2 lines necessary since SeisPlot::updateUser has changed
  //from the way we used to do it (setTI and setIS)
  _sp->setGridWidth(_pwidth);
  _sp->setGridHeight(_pheight);

  _sp->setMinColorAmp(_vmin);
  _sp->setMaxColorAmp(_vmax); 
  _sp->setGradeHorz((char)_grad_hor);
  _sp->setGradeVert((char)_grad_ver);
  _sp->setLabeling(1, max(1,_x_annotation_increment));
  
  if(_x_annotation_increment && _line_type != VaIsoPlot::TIMESLICE)
    _sp->setDrawXlines(True);
  else
    _sp->setDrawXlines(False);     

  
  if(_user_visited)
    {   
    if( _color_control->whichSelected() == SeisCtype::USEDEF)
      _sp->setDoAmplitude(True);//use velocity ranges for coloring
    else
      _sp->setDoAmplitude(False);//use external color bar file for coloring

    }
  else
    {
    _sp->setDoAmplitude(True);
    }


  if( _line_type == VaIsoPlot::TIMESLICE)
    {
    _sp->setGridXYS(_x1, _x2, _y1, _y2);
    _sp->setTminTmax(_y1, _y2);
    _sp->setDrawYlines(False);
    }
  else if(_line_type == VaIsoPlot::INLINE)
    {
    _sp->setGridXYS(_x1, _x2, _tmin, _tmax);
    _sp->setTminTmax(_tmin, _tmax);
    _sp->setDrawYlines(True);
    }
  else//CROSSLINE
    {
    _sp->setGridXYS(_y1, _y2, _tmin, _tmax);
    _sp->setTminTmax(_tmin, _tmax);
    _sp->setDrawYlines(True);
    }

  if(_movie_tog->IsSelected(DO_MOVIE))
    {
    _sp->setMovie(True);
    if(_line_type != VaIsoPlot::TIMESLICE)
      _sp->setSkipFrames((int)_skip_panels);
    else
      _sp->setSkipFrames(0);
    _sp->setFrames(_total_panels);
    }  
  else
    {
    _sp->setMovie(False);
    _sp->setSkipFrames(0);
    _sp->setFrames(1);
    }

  switch(_plot_type)
   {
   case VTNM://time image
   case VTAV:
   case VTIN:
   case VTRM:
     _sp->setTimingLines(_primary_timing, _secondary_timing); 
     break;

   default:  //depth image... 20000 is arbituary break over point
     if( ( max(_tmin, _tmax) - min(_tmax, _tmin) ) > 20000.0)
       _sp->setTimingLines(_primary_timing * 10000.0, 
                           _secondary_timing * 10000.0); 
     else
       _sp->setTimingLines(_primary_timing * 1000.0, 
                           _secondary_timing * 1000.0); 
     break;
   }


}


//=========================================================================
//==================== Reset when new file is read ========================
//=========================================================================
void VaIsoGui::setToFileDefaults()
{

}


//=========================================================================
//==================== Manage                   ===========================
//=========================================================================
void VaIsoGui::manage()
{
char label_string[100];
VfFileBase *fb;

  _vfd = _plot->vfManager()->activeDataset(); 

  //If the user has not visited this popup and the filechoice has not
  //initialized this menu, query the VfFileBase object and get plotting
  //parameters from it.
  if(_data_initialized == False && _user_visited == False)
    {
    fb = _plot->plotControl()->velFile();
    _params->SetValue( X1,   fb->minimumXloc());
    _params->SetValue( X2,   fb->maximumXloc());
    _params->SetValue( Y1,   fb->minimumYloc());
    _params->SetValue( Y2,   fb->maximumYloc() );
    _params->SetValue( VMIN, fb->minimumOrdinate(getPlotType()));
    _params->SetValue( VMAX, fb->maximumOrdinate(getPlotType()));
    _params->SetValue( TMIN, fb->minimumAbscissa(getPlotType()));
    _params->SetValue( TMAX, fb->maximumAbscissa(getPlotType()));
    _binbox->SetValue( TIME, fb->minimumAbscissa(getPlotType()));
    _contour_gui->setContourMinValue(fb->minimumOrdinate(getPlotType()));
    _contour_gui->setContourMaxValue(fb->maximumOrdinate(getPlotType()));
    }

  SLBase::manage();

  if(_first_time)
    {
    XtSetSensitive(_movie_box->W(), False); 
    switch(_line_type)
    {
    case VaIsoPlot::INLINE:
      wprocShowMsg(_movie_box->LabW("first_panel"), "First Panel:");
      wprocShowMsg(_movie_box->LabW("skip_panels"), "Panels To Skip:");
      break;

    case VaIsoPlot::CROSSLINE:
      wprocShowMsg(_movie_box->LabW("first_panel"), "First Panel:");
      wprocShowMsg(_movie_box->LabW("skip_panels"), "Panels To Skip:");
      break;

    case VaIsoPlot::TIMESLICE:
      wprocShowMsg(_movie_box->LabW("first_panel"), "First Time:");
      wprocShowMsg(_movie_box->LabW("skip_panels"), "Time Increment:");
      break;
    }
  _first_panel = 1.0;
  setMovieParameters(FIRST_PANEL);
  _first_time = False;

  if(_data_initialized == False && _user_visited == False)
    {
    sprintf(label_string,"Time Range: %8.3f to %8.3f",
            fb->minimumAbscissa(getPlotType()),
            fb->maximumAbscissa(getPlotType()));
    wprocShowMsg(_rangelab, label_string);

    sprintf(label_string,"Velocity Range: %8.3f to %8.3f",
            fb->minimumOrdinate(getPlotType()),
            fb->maximumOrdinate(getPlotType()));
    wprocShowMsg(_vellab,   label_string);
    }
  else
    {
    sprintf(label_string,"Time Range: %8.3f to %8.3f",_plot->minTime(),
                                                    _plot->maxTime());
    wprocShowMsg(_rangelab, label_string);
    sprintf(label_string,"Velocity Range: %8.3f to %8.3f",_plot->minVelocity(),
                                                     _plot->maxVelocity());
    wprocShowMsg(_vellab,   label_string);
    }

  }
 

  
    


  _user_visited = True;
  XtManageChild(topWidget()); 
}

//=========================================================================
//==================== Reset to system defaults ===========================
//=========================================================================
void VaIsoGui::reloadSystemDefaults(Boolean /*do_method*/)
{

 _params->SetValue(PWIDTH, 4.0F);
 _params->SetValue(PHEIGHT,4.0F);
 _params->SetValue(TMIN,   0.0F);
 _params->SetValue(TMAX,   1.5F);
 _params->SetValue(VMIN,   1000.0F);
 _params->SetValue(VMAX,   15000.0F);
 _params->SetValue(X1,     0.0F);
 _params->SetValue(X2,     1.0F);
 _params->SetValue(Y1,     0.0F);
 _params->SetValue(Y2,     1.0F);

 _contour_gui->setContourMinValue(1000.0F);
 _contour_gui->setContourMaxValue(15000.0F);
 

 _grade->SetTog(GRADEH,True);

 _ptypebox->SetRadio(VTNM);

 _movie_tog->SetTog(DO_MOVIE,   False);

 _movie_box->SetValue(FIRST_PANEL,  1.0F);
 _movie_box->SetValue(SKIP_PANELS,  0.0F);
 _movie_box->SetValue(TOTAL_PANELS, 1L);


}


//=========================================================================
//====================  Update public method    ===========================
//=========================================================================
void VaIsoGui::updateParams(Boolean /*update_file_limits*/)
{
char label_string[100];

  

  if(made())
    {
    sprintf(label_string,"Time Range: %8.3f to %8.3f",_plot->minTime(),
                                                      _plot->maxTime());
    wprocShowMsg(_rangelab, label_string);
    sprintf(label_string,"Velocity Range: %8.3f to %8.3f",_plot->minVelocity(),
                                                      _plot->maxVelocity());
    wprocShowMsg(_vellab,   label_string);
    }



  if(_user_visited == False)
    {
    _params->SetValue( VMIN, _plot->minVelocity() );
    _params->SetValue( VMAX, _plot->maxVelocity() );
    _params->SetValue( TMIN, _plot->minTime() );
    _params->SetValue( TMAX, _plot->maxTime() );
    _params->SetValue( X1,   _plot->minXbin() );
    _params->SetValue( X2,   _plot->maxXbin() );
    _params->SetValue( Y1,   _plot->minYbin() );
    _params->SetValue( Y2,   _plot->maxYbin() );
    _binbox->SetValue( XBIN, _plot->minXbin() );
    _binbox->SetValue( YBIN, _plot->minYbin() );
    _binbox->SetValue( TIME, _plot->minTime() );
    _contour_gui->setContourMinValue(_plot->minVelocity());
    _contour_gui->setContourMaxValue(_plot->maxVelocity());
    if(!reconcileParameters()) return;
    _data_initialized = True;
    setMovieParameters(TOTAL_PANELS);
    _sp->setNPlt(_functions_in_frame[0]);
    _sp->setTdec(1);
    _sp->setMinMaxP(_plot->minVelocity(),_plot->maxVelocity());
    }
  else//only update those items that are not within the user's parameters
    {
    if(_params->GetFloat(VMIN) >=  _plot->minVelocity() &&
       _params->GetFloat(VMIN) <=  _plot->maxVelocity()   )
      {
      }
    else
      {
      _params->SetValue(VMIN,    _plot->minVelocity() );
      _contour_gui->setContourMinValue(_plot->minVelocity());
      }
    if(_params->GetFloat(VMAX) >=  _plot->minVelocity() &&
       _params->GetFloat(VMAX) <=  _plot->maxVelocity()   )
      {
      }
    else
      {
      _params->SetValue(VMAX,    _plot->maxVelocity() );   
      _contour_gui->setContourMaxValue(_plot->maxVelocity());
      }
    if(_params->GetFloat(TMIN) >=  _plot->minTime() &&
       _params->GetFloat(TMIN) <=  _plot->maxTime()   )
      {
      }
    else
      {
      _params->SetValue(TMIN,    _plot->minTime() );   
      }
    if(_params->GetFloat(TMAX) >=  _plot->minTime() &&
       _params->GetFloat(TMAX) <=  _plot->maxTime()   )
      {
      }
    else
      {
      _params->SetValue(TMAX,    _plot->maxTime() );   
      }
    if(_params->GetFloat(X1)   >=  _plot->minXbin() &&
       _params->GetFloat(X1)   <=  _plot->maxXbin()   )
      {
      }
    else
      {
      _params->SetValue(X1,    _plot->minXbin() );   
      }
    if(_params->GetFloat(X2)   >=  _plot->minXbin() &&
       _params->GetFloat(X2)   <=  _plot->maxXbin()   )
      {
      }
    else
      {
      _params->SetValue(X2,    _plot->maxXbin() );   
      }
    if(_params->GetFloat(Y1)   >=  _plot->minYbin() &&
       _params->GetFloat(Y1)   <=  _plot->maxYbin()   )
      {
      }
    else
      {
      _params->SetValue(Y1,    _plot->minYbin() );   
      }
    if(_params->GetFloat(Y2)   >=  _plot->minYbin() &&
       _params->GetFloat(Y2)   <=  _plot->maxYbin()   )
      {
      }
    else
      {
      _params->SetValue(Y2,    _plot->maxYbin() );   
      }
    if(_binbox->GetFloat(XBIN)  >=  _plot->minXbin() &&
       _binbox->GetFloat(XBIN)  <=  _plot->maxXbin()   )
      {
      }
    else
      {
      _binbox->SetValue(XBIN,    _plot->minXbin() );   
      } 
    if(_binbox->GetFloat(YBIN)  >=  _plot->minYbin() &&
       _binbox->GetFloat(YBIN)  <=  _plot->maxYbin()   )
      {
      }
    else
      {
      _binbox->SetValue(YBIN,    _plot->minYbin() );   
      }
    if(_binbox->GetFloat(TIME) >=  _plot->minTime() &&
       _binbox->GetFloat(TIME) <=  _plot->maxTime()   )
      {
      }
    else
      {
      _binbox->SetValue(TIME,    _plot->minTime() );   
      }
    if(!reconcileParameters()) return;
    _data_initialized = True;
    setMovieParameters(TOTAL_PANELS);
    _sp->setNPlt(_functions_in_frame[0]);
    _sp->setTdec(1);
    }

  
}

//=========================================================================
//==================== Set movie parameters     ===========================
//=========================================================================
void VaIsoGui::setMovieParameters(int ident)
{
float tmax;
float temp,tslice;
long *temp_list;
long max_possible;
int current_maxpnl;
int new_frames;
int i;
long num_locations;


  _vfd = _plot->vfManager()->activeDataset(); 
  num_locations = 
       max(_vfd->numVelocityFunctions(),_plot->numberLocationsInFile());

  tmax = _vfd->maximumAbscissa(getPlotType());

  temp_list =
       (long *)calloc(1,((int)(num_locations*sizeof(long))));
  if(temp_list == NULL) return;
 
  
  if(_line_type == VaIsoPlot::TIMESLICE && _skip_panels == 0.0)
     _skip_panels = 0.5;//default

  if(_line_type == VaIsoPlot::INLINE)
     _vfd->getVelfunCrossline(_vfd->minimumYloc(),_vfd->maximumYloc(),
                              FNIL,temp_list,&max_possible);
  else if(_line_type == VaIsoPlot::CROSSLINE)
    _vfd->getVelfunInline(_vfd->minimumXloc(),_vfd->maximumXloc(),
                          FNIL,temp_list,&max_possible); 
  else
    max_possible = (long)((max(_tmax,_tmin) - min(tmax,_tmin)) /
                           _skip_panels + 1.0);


  if(_line_type == VaIsoPlot::TIMESLICE)
    temp = (float)(max_possible - _first_panel );
  else
    temp = ( (float)max_possible - _first_panel )
            / (_skip_panels + 1.0) + 1.0;
  current_maxpnl = int(temp);


  switch(ident)
    {
    case FIRST_PANEL:
      if(_line_type == VaIsoPlot::TIMESLICE)
        {
        new_frames= (int)(( tmax - _first_panel) / _skip_panels + 1.0);
        if(_time_of_frame == NULL)
          _time_of_frame =
            (float *)calloc(1,((int)( new_frames * sizeof(float))));
        else
          _time_of_frame =
            (float *)realloc(_time_of_frame,(int)
                        ( new_frames * sizeof(float)));
        if(_time_of_frame == NULL) return;
        tslice = _first_panel;
        for(i=0; i<new_frames; i++)
          {
          _time_of_frame[i] = tslice;
          tslice += _skip_panels;
          }
        _total_panels = new_frames;
        _movie_box->SetValue(TOTAL_PANELS, _total_panels);
        _binbox->SetValue(TIME, _first_panel); 
        }
      else // Inlines or Crosslines selected
        {
        if(_first_panel >= 1)
          {
          _total_panels = current_maxpnl;
          _movie_box->SetValue(TOTAL_PANELS, _total_panels);
	  }
        if(_first_panel >= max_possible)
          {
          _first_panel  = (float)max_possible;
          _skip_panels  = 0.0;
          _total_panels = 1;
          _movie_box->SetValue(FIRST_PANEL,  _first_panel);
          _movie_box->SetValue(SKIP_PANELS,  _skip_panels);
          _movie_box->SetValue(TOTAL_PANELS, _total_panels);
	  }
        }
      break;


    case SKIP_PANELS:
      if( _line_type == VaIsoPlot::TIMESLICE)
        {
        new_frames=(int)(( tmax -  _first_panel) / _skip_panels + 1.0);
        if(_time_of_frame == NULL)
          _time_of_frame =
            (float *)calloc(1,((int)( new_frames * sizeof(float))));
        else
          _time_of_frame =
            (float *)realloc(_time_of_frame,(int)
                        ( new_frames * sizeof(float)));
        if(_time_of_frame == NULL) return;
        tslice =  _first_panel;
        for(i=0; i<new_frames; i++)
          {
          _time_of_frame[i] = tslice;
          tslice +=  _skip_panels;
          }
        _total_panels = new_frames;
        _movie_box->SetValue(TOTAL_PANELS, _total_panels);
        }
      else // not a slice
        {
        if(_skip_panels)
	  {
          temp = ( (float)max_possible - _first_panel )
                  / (_skip_panels + 1.0) + 1.0;
          current_maxpnl = int(temp);
          _total_panels = current_maxpnl;
          _movie_box->SetValue(TOTAL_PANELS, _total_panels);
          if( _skip_panels >= (max_possible - _first_panel) )
	    {
            _skip_panels  = 0;
            temp = ( (float)max_possible - _first_panel )
                       / (_skip_panels + 1.0) + 1.0;
            current_maxpnl = int(temp);
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
        }
      break;


    case TOTAL_PANELS:
      if( _line_type == VaIsoPlot::TIMESLICE)
        {
        new_frames=(int)((tmax - _first_panel) / _skip_panels + 1.0);
        if(_total_panels > new_frames)
          _total_panels = new_frames;
        else
          new_frames = (int)_total_panels;
        tslice =  _time;
        if(_time_of_frame == NULL)
          _time_of_frame =
            (float *)calloc(1,((int)( new_frames * sizeof(float))));
        else
          _time_of_frame =
            (float *)realloc(_time_of_frame,(int)
                        ( new_frames * sizeof(float)));
        if(_time_of_frame == NULL) return;
        for(i=0; i<new_frames; i++)
          {
          _functions_in_frame[i] = _functions_in_frame[0];
          _time_of_frame[i] = tslice;
          tslice +=  _skip_panels;
          }
        _movie_box->SetValue(TOTAL_PANELS, _total_panels);
	}
      else // not a slice
	{
        if(_total_panels > current_maxpnl)
           _total_panels = current_maxpnl;
        _movie_box->SetValue(TOTAL_PANELS, _total_panels);
	}
      break;
    }//end switch



  free(temp_list);

}


//=========================================================================
//=========== Find first sequential movie panel based on user line  =======
//=========== id request                                            =======
//=========================================================================
Boolean VaIsoGui::findFirstMoviePanel()
{
long count;
long i;
float xloc, yloc;
long num_locations;

  _vfd = _plot->vfManager()->activeDataset(); 
  num_locations =
            max(_vfd->numVelocityFunctions(),_plot->numberLocationsInFile());

  if(_vel_request_list == NULL)
    _vel_request_list =
     (long *)calloc(1,((int)(num_locations * _total_panels
                             *sizeof(long))));
  else
    _vel_request_list=
     (long *)realloc(_vel_request_list,
                       (int)(num_locations * _total_panels
                             *sizeof(long)));
  if(_vel_request_list == NULL) return False;   

  switch(_line_type)
    {
    case VaIsoPlot::INLINE:
      _vfd->getVelfunInline(_x1,_x2,_ybin,&_vel_request_list[0],&count);
      if(!count) return False;
      yloc = _vfd->getYloc(_vel_request_list[0]);
      _vfd->getVelfunCrossline(_vfd->minimumYloc(),_vfd->maximumYloc(),
                               FNIL,&_vel_request_list[0],&count);
      for(i=0;i<count;i++)
        {
        if(yloc == _vfd->getYloc(_vel_request_list[i]))
          {
          _first_panel = i + 1;
          break;
          }
        }
      break;

    case VaIsoPlot::CROSSLINE:
      _vfd->getVelfunCrossline(_y1,_y2,_xbin,&_vel_request_list[0],&count);
      if(!count) return False;
      xloc = _vfd->getXloc(_vel_request_list[0]);
      _vfd->getVelfunInline(_vfd->minimumXloc(),_vfd->maximumXloc(),
                            FNIL,&_vel_request_list[0],&count);
      for(i=0;i<count;i++)
        {
        if(xloc == _vfd->getXloc(_vel_request_list[i]))
          {
          _first_panel = i + 1;
          break;
          }
        }
      break;

    }

  return True;

}


//=========================================================================
//=========== Make sure user requests can be honored ======================
//=========================================================================
Boolean VaIsoGui::reconcileParameters()
{
long *temp_list = NULL;
long count, index = 0;
long i;
long num_locations_in_file;
  

  _vfd = _plot->vfManager()->activeDataset(); 

  num_locations_in_file = max(_vfd->numVelocityFunctions(),
                              _plot->numberLocationsInFile());

  //allocate arrays
  if(_vel_request_list == NULL)
    _vel_request_list =
     (long *)calloc(1,((int)(num_locations_in_file * _total_panels
                             *sizeof(long))));
  else
    _vel_request_list=
     (long *)realloc(_vel_request_list,
                       (int)(num_locations_in_file * _total_panels
                             *sizeof(long)));
  if(_vel_request_list == NULL) return False;   


  if(_vel_ylines_list == NULL)
    _vel_ylines_list =
     (long *)calloc(1,((int)(num_locations_in_file * _total_panels
                             *sizeof(long))));
  else
    _vel_ylines_list=
     (long *)realloc(_vel_ylines_list,
                      (int)(num_locations_in_file  * _total_panels
                             *sizeof(long)));
  if(_vel_ylines_list == NULL) return False;


  if(_functions_in_frame == NULL)
    _functions_in_frame =
     (long *)calloc(1,((int)(num_locations_in_file * _total_panels
                             *sizeof(long))));
  else
    _functions_in_frame=
     (long *)realloc(_functions_in_frame,
                      (int)(num_locations_in_file  * _total_panels
                             *sizeof(long)));

  if(_functions_in_frame == NULL) return False;
  

  if(_movie_tog->IsSelected(DO_MOVIE))
    {
     temp_list =
       (long *)calloc(1,((int)(num_locations_in_file*sizeof(long))));
     if(temp_list == NULL) return False;

     if(_time_of_frame == NULL)
       _time_of_frame =
            (float *)calloc(1,((int)( _total_panels * sizeof(float))));
     else
       _time_of_frame =
        (float *)realloc(_time_of_frame,(int)( _total_panels * sizeof(float)));
     if(_time_of_frame == NULL) return False;            
    }

  _functions_in_frame[0] = 0;






  //request data from dataset
  switch(_line_type)
    {
    case VaIsoPlot::INLINE:
      if(_movie_tog->IsSelected(DO_MOVIE))
        {
        _vfd->getVelfunCrossline(_vfd->minimumYloc(),_vfd->maximumYloc(),
                                 FNIL,temp_list,&count);
        for(i=0;i<count;i++)
          {
          _vfd->getVelfunInline(_x1,_x2,_vfd->getYloc(temp_list[i]),
                                &_vel_request_list[index],
                                &_functions_in_frame[i]);
          index += _functions_in_frame[i];
          }
        if(!index){if(temp_list) free(temp_list); return False;}
        }
      else
        {
        _vfd->getVelfunInline(_x1,_x2,_ybin,_vel_request_list,
                              _functions_in_frame);
        if(_functions_in_frame[0] == 0)
           {if(temp_list) free(temp_list); return False;}
        }
      break;

    case VaIsoPlot::CROSSLINE:
      if(_movie_tog->IsSelected(DO_MOVIE))
        {
        _vfd->getVelfunInline(_vfd->minimumXloc(),_vfd->maximumXloc(),
                              FNIL,temp_list,&count);
        for(i=0;i<count;i++)
          {
          _vfd->getVelfunCrossline(_y1,_y2,_vfd->getXloc(temp_list[i]),
                                &_vel_request_list[index],
                                &_functions_in_frame[i]);
          index += _functions_in_frame[i];
          }
        if(!index){if(temp_list) free(temp_list); return False;}
        }
      else
        {
        _vfd->getVelfunCrossline(_y1,_y2,_xbin,_vel_request_list,
                              _functions_in_frame);
        if(_functions_in_frame[0] == 0)
           {if(temp_list) free(temp_list); return False;}
        }
      break;

    case VaIsoPlot::TIMESLICE:
      //get number of y lines in request area
      _vfd->getVelfunCrossline(_vfd->minimumYloc(),_vfd->maximumYloc(),
                               FNIL,_vel_ylines_list,
                               _functions_in_frame);
      _number_y_lines = _functions_in_frame[0];
      //get number of x lines
      _vfd->getVelfunInline(_vfd->minimumXloc(),_vfd->maximumXloc(),
                            FNIL,_vel_request_list, _functions_in_frame);
      if(_functions_in_frame[0] < 2)
           {if(temp_list) free(temp_list); return False;} 
      _time_of_frame[0] = _time;
      if(_movie_tog->IsSelected(DO_MOVIE)) setMovieParameters(TOTAL_PANELS);
      break;
    }




 if(temp_list) free(temp_list); 

 return True;

}


//===========================================================================
//============= External method to change the data plotted     ==============
//===========================================================================
Boolean VaIsoGui::changePlot(float x1, float x2, float y1, float y2,
                             Boolean refresh_only)
{
float oldx1, oldx2, oldy1, oldy2;
int oldlinetype;
float oldxbin, oldybin;
int stat;
long  oldtotalpanels;
float oldfirstpanel;
float oldskippanels;
float oldtmin, oldtmax, oldvmin, oldvmax;
Boolean use_vel_data = False;

  _vfd = _plot->vfManager()->activeDataset(); 

  //if 0's are passed in get the coordinates from the velocity data set
  if(x1 == 0.0 && x2 == 0.0 && y1 == 0.0 && y2 == 0.0)
    {
    use_vel_data = True;
    x1 = _vfd->minimumXloc();
    x2 = _vfd->maximumXloc();
    y1 = _vfd->minimumYloc();
    y2 = _vfd->maximumYloc();
    oldtmin = _tmin;
    oldtmax = _tmax;
    oldvmin = _vmin;
    oldvmax = _vmax;
    //Following commented because the vmin and vmax are used as the color
    //bar amplitudes and the user may have set them. The control panel's
    //refresh iso velocity option calls this block of code. I dont think
    //we want to have the user's distribution change. MLS 11/01
    //_vmin   = _vfd->minimumOrdinate(getPlotType());
    //_vmax   = _vfd->maximumOrdinate(getPlotType());
    _tmin   = _vfd->minimumAbscissa(getPlotType());
    _tmax   = _vfd->maximumAbscissa(getPlotType()); 
    }

  oldx1          = _x1;
  oldx2          = _x2;
  oldy1          = _y1;
  oldy2          = _y2;
  oldlinetype    = _line_type;
  oldxbin        = _xbin;
  oldybin        = _ybin;
  oldfirstpanel  = _first_panel;
  oldtotalpanels = _total_panels;
  oldskippanels  = _skip_panels;

  if(refresh_only == False)//not being called by the control panel popup
    {
    if(x1 == x2)
      {
      _line_type = VaIsoPlot::CROSSLINE;
      _xbin = x1;
      }
    else if(y1 == y2)
      {
      _line_type = VaIsoPlot::INLINE;
      _ybin = y1;
      }
    else if(x1 != x2 && y1 != y2)
      {
      _line_type = VaIsoPlot::TIMESLICE;
      }
    }


  _x1 = x1;
  _x2 = x2;
  _y1 = y1;
  _y2 = y2;

  if(_movie_tog->IsSelected(DO_MOVIE)) 
    {
    findFirstMoviePanel();
    setMovieParameters(TOTAL_PANELS);
    }

  if(!reconcileParameters())
    {
    _x1           = oldx1;
    _x2           = oldx2;
    _y1           = oldy1;
    _y2           = oldy2;
    _line_type    = oldlinetype;
    _xbin         = oldxbin;
    _ybin         = oldybin; 
    _first_panel  = oldfirstpanel;
    _skip_panels  = oldskippanels;
    _total_panels = oldtotalpanels;
    if(use_vel_data)
      {
      _tmin = oldtmin;
      _tmax = oldtmax;
      _vmin = oldvmin;
      _vmax = oldvmax;
      }
    return False;
    }

  setParameters();
  stat = _plot->plot();
  if(stat == PlotImage::PlotSuccess) _plotted_vel_type = _plot_type;

  _x1           = oldx1;
  _x2           = oldx2;
  _y1           = oldy1;
  _y2           = oldy2;
  _line_type    = oldlinetype;
  _xbin         = oldxbin;
  _ybin         = oldybin;
  _first_panel  = oldfirstpanel;
  _skip_panels  = oldskippanels;
  _total_panels = oldtotalpanels;
  if(use_vel_data)
    {
    _tmin = oldtmin;
    _tmax = oldtmax;
    _vmin = oldvmin;
    _vmax = oldvmax;
    }


  if(stat == PlotImage::PlotSuccess)
    return True;
  else
    return False;
}


//===========================================================================
//============= External method to set whether to plot on DoAction ==========
//===========================================================================
void VaIsoGui::setDontPlotYet(Boolean dont_plot_yet)
{
  _dont_plot_yet = dont_plot_yet;
}
