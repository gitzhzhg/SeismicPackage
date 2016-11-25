//********************************************
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
//Author Michael L. Sherrill 06/95
//Creates menu to control geometry qc images
//********************************************


#include <Xm/Xm.h>
#include <Xm/Label.h>
#include <Xm/Frame.h>
#include <Xm/Separator.h>
#include <stdio.h>
#include <X11/cursorfont.h>
#include <math.h>
#include <Xm/Form.h>
#include "fgqc/fgqc_pop.hh"
#include "fgqc/fgqc_plot.hh"
#include "fgqc/fgqc_statics_type.hh"
#include "fgqc/fgqc_pop_sub_menu.hh"
#include "sp/seis_plot.hh"
#include "sl/sl_push_box.hh"
#include "sl/shell_stat_msg.hh"
#include "geom/field_geometry.hh"
#include "fgmap/fg_seis_plot_list.hh"
#include "fgqc/statics_read_pop.hh"
#include "fgqc/statics_file.hh"
#include "fgqc/header_word_menu.hh"
#include "fgqc/fgqc_trace_counter.hh"
#include "fgqc/fgqc_plot_constants.hh"
#include "sl/sl_quest_pop.hh"
#include "sl/sl_prim.hh"

#define plotfail "Unable to create plot\nYou may have entered incorrect xy\nranges or have run out of colors\nand need to use the extended colors option."
#define nobins "Midpoints may not be calculated\nPlease visit the Data Control Menu\nand create the gathers."
#define notransform "Grid transformation does not seem\nto have been calculated"
#define nogathers "Necessary gathers have not been created yet."
#define sizefail "Plot height and width sizes must be greater than 0"
#define coord_error "Currently fold of stack plots must have the left\ncoordinate less than the right and the top coordinate\ngreater than the bottom."
#define    PWIDTH_VAL           7.0F
#define    PHEIGHT_VAL          7.0F
#define    XMIN_VAL             0.0F
#define    YMIN_VAL             0.0F
#define    XMAX_VAL             1.0F
#define    YMAX_VAL             1.0F
#define    NUM_ATTRIBUTES       15
#define    PRIVATE_COLOR_MAP    220
#define    NORMAL_COLOR_MAP     66
#define    PRIVATE_GRAPHICS     20
#define    NORMAL_GRAPHICS       0

static String  defres[]= {
    "*popup.title:                   AERIAL QC PLOTS",
    "*SLLP_title.labelString:        ATTRIBUTE",
    ".height:                        525",
    ".width:                         640",
    "*gridsystem.labelString:        Grid Coordinates",
    "*surveysystem.labelString:      Survey Coordinates",
    "*coordradiobox_Frame.topPosition:  5",
    "*coordradiobox_Frame.leftPosition: 5",
    "*coord_box*columns:             12",
    "*pwidthL.labelString:           Width/Inches:",
    "*pheightL.labelString:          Height/Inches:",
    "*xminL.labelString:             Left:",
    "*xmaxL.labelString:             Right:",
    "*yminL.labelString:             Top:",
    "*ymaxL.labelString:             Bottom:",
    "*pwidth.value:                  7.0",
    "*pheight.value:                 7.0",
    "*xmin.value:                    0.0",
    "*xmax.value:                    1.0",
    "*ymin.value:                    0.0",
    "*ymax.value:                    1.0",
    "*coordop.labelString:           <-- Update Coordinates",
    "*mapop.labelString:             Select From Map",
    "*colorop.labelString:           Color Options...",
    "*cbarop.labelString:            Color Bar",
    "*newinstance.labelString:       Make New Plot",
    "*oldinstance.labelString:       Replace Plot",
    "*shader.labelString:            Extended Colors",
    "*enforce.labelString:           Enforce Symetrical Size",
    NULL};


// Note: Keep these in the same order as the enums in fgqc_plot.hh so that
// a for loop can be used to find the menu selected plot type. See 
// notifyComplex function in this file.

static char *atrname[NUM_ATTRIBUTES] = { 
                            "Azimuthal Distribution",
                            "Differences","Flag Elevations","Fold of Stack",
                            "Source Hole Depths","Normalized Binning",
                            "Cmp Distribution",
                            "Receiver Datum Shifts", "Source Datum Shifts",
                            "Source Progression", "Statics",
                            "Source Upholes", "Header Word", 
                            "Bin Centers Distribution", "Source Elevations"};



#define ParentClass SLFPopSep


FgQcPop::FgQcPop( Widget              p,
                  char                *name,
                  HelpCtx             hctx, 
                  class FieldGeometry *fg,
                  FgSeisPlotList      *fgsp_list,
                  SLApp               *app)
                  : SLFPopSep(p,name,FP_DOALL,hctx,True,False),
                  _plot_on_doaction(True), _use_file_defaults(False), 
                  _new_appdefaults(True),  _first_time(True), 
                  _hctx(hctx), _fg(fg), _fgsp_list(fgsp_list), _app(app)

{

  

  static SLText texts[]  = {
    {"pwidth",  NULL, NULL, SLType_float, PWIDTH},
    {"pheight", NULL, NULL, SLType_float, PHEIGHT},
    {"xmin",    NULL, NULL, SLType_float, XMIN},
    {"xmax",    NULL, NULL, SLType_float, XMAX},
    {"ymin",    NULL, NULL, SLType_float, YMIN},
    {"ymax",    NULL, NULL, SLType_float, YMAX},
  };
  texts[0].target= &_pwidth;
  texts[1].target= &_pheight;
  texts[2].target= &_left;
  texts[3].target= &_right;
  texts[4].target= &_top;
  texts[5].target= &_bottom;
     

  static SLRadio coordtypes[]  = 
     {
       { "gridsystem",   GRIDSYSTEM },
       { "surveysystem", SURVEYSYSTEM },
     };

  static SLPush coordop[] =
     {
       { "coordop", COORDOP },
     };

  static SLPush buttons[]  = 
     {
       { "mapop",    MAPOP  },
       { "colorop",  COLOROP},
       { "cbarop",   CBAROP },
     };

  static SLRadio instances[]  = 
     {
       { "newinstance", NEWINSTANCE },
       { "oldinstance", OLDINSTANCE },
     };


  static SLTog shader[]  = 
     {
      { "shader", NULL, EXTENDED_COLORS },
     };

  static SLTog enforce[]  = 
     {
      { "enforce", NULL, ENFORCE },
     };

  setDefaultResources( p, name, defres);

  _numcolors = NORMAL_COLOR_MAP + NORMAL_GRAPHICS;
  _numgraphics = NORMAL_GRAPHICS;
   
  _coordradiobox= new SLRadioBox(this,"coordradiobox",getHelpCtx(),coordtypes,
                               XtNumber(coordtypes), NULL, True, False );
  _coordradiobox->setAltChoiceAction((SLRadioButtonfunc)GridSurveyAction,this);

  _coord_box= new SLTextBox( this, "coord_box", getHelpCtx(),
                             texts, XtNumber(texts), True, 1, True, False );
  _coord_box->setAltLosingAction( (SLTextfunc)CoordLosingFocusAction, this );
  _coord_box->setAltFocusAction ( (SLTextfunc)CoordFocusAction, this );

  _coordop = new SLPushBox(this,"coord_button",getHelpCtx(),
                           coordop, XtNumber(coordop));
  _coordop->setAltPushAction( (SLPushButtonfunc)buttonCoordinates, this);

  _buttons = new SLPushBox(this,"push_buttons",getHelpCtx(),
                           buttons,XtNumber(buttons));
  _buttons->setAltPushAction( (SLPushButtonfunc)button_control, this);

  _attribute = new SLListPicker(this, "list_pick", getHelpCtx());
  _attribute->setExtra (&SLPrim::updateEverything);

  setComplexNotify(this);

  _newbox = new SLRadioBox(this, "new_box",getHelpCtx(),instances,
                             XtNumber(instances), NULL, True, False );
  _newbox->setAltChoiceAction((SLRadioButtonfunc)InstanceAction,this);


  _extendedcolorsbox = new SLTogBox(this, "shade_box",getHelpCtx(),shader,
                            XtNumber(shader), True, False, False );
  _extendedcolorsbox->setAltChoiceAction( 
                            (SLToggleButtonfunc)ExtendedColorsAction, this);

  _enforcebox = new SLTogBox(this, "enforce_box",getHelpCtx(),enforce,
                            XtNumber(enforce), True, False, False );
  _enforcebox->setAltChoiceAction( 
                            (SLToggleButtonfunc)EnforceAction, this);
  _plot_type = ELEVATIONS; //for a default type
  _use_centers = False;

  if(_app == NULL)
    {
    _active_plot = new FgQcPlot(p, "fgqc_plot", _hctx, _fg, this, _fgsp_list,
                                 _numcolors);
    }
  else
    {
    _active_plot = new FgQcPlot(p, "fgqc_plot", _hctx, _fg, this, _fgsp_list,
                                _numcolors, _app);

    _active_plot->reduceColors (_numgraphics);
    _active_plot->make(p);
    _active_plot->unmanage();
    }

  
  addPlot(_active_plot);

  _first_plot_made = False;
  _previous_active_plot = NULL;
  _distribution_menu = NULL;
  _normalized_bin_menu = NULL;
  _srp = NULL;
  _header_word_menu = NULL;
  if (!_fg->midpointGathersOutOfDate()) _data_loc = PLOT_AT_CMP;
  else if (!_fg->receiverGathersOutOfDate()) _data_loc = PLOT_AT_RECEIVER;
  else /*if (!_fg->sourceGathersOutOfDate())*/ _data_loc = PLOT_AT_SOURCE;
  _header_word     = 42;
  _question_pop = NULL;
  _questions_answered = True;  
}




FgQcPop::~FgQcPop()
{
FgQcPlot *lfp;

 for(lfp = _plist.top(); lfp; lfp = _plist.next() ) delete lfp;
 if(_srp) delete _srp;
 if (_header_word_menu) delete _header_word_menu;
}



Widget FgQcPop::make(Widget p)
{

   if ( made() ) return topWidget();

   _active_plot->reduceColors (_numgraphics);
   _active_plot->make(p); 

   Widget parent = p ? p : wParent();
   ShellStatMsg bld_info(parent, "Building Aerial Plot Menu...");

   SLFPopSep::make(p);

   _srp = new StaticsReadPop(topWidget(), "Statics Read Pop",
                             _hctx, _fg, False);

   _coordradiobox->SetRadio(SURVEYSYSTEM);

   _enforcebox->SetTog(ENFORCE,True);

   XtVaSetValues( _coordradiobox->W(), XmNleftAttachment, XmATTACH_POSITION,
                                   XmNtopAttachment,  XmATTACH_POSITION, NULL);

    
   XtVaSetValues( _coord_box->W(), XmNtopAttachment,  XmATTACH_WIDGET,
                                   XmNtopWidget,      _coordradiobox->W(),
                                   XmNleftAttachment, XmATTACH_OPPOSITE_WIDGET, 
                                   XmNleftWidget,     _coordradiobox->W(), 
                                   XmNtopOffset,      25, NULL);

   XtVaSetValues( _coordop->W(),   XmNtopAttachment,  XmATTACH_OPPOSITE_WIDGET,
                                   XmNtopWidget,      _coordradiobox->W(),
                                   XmNtopOffset,      10,
                                   XmNleftAttachment, XmATTACH_OPPOSITE_WIDGET, 
                                   XmNleftWidget,     _coordradiobox->W(),
                                   XmNleftOffset,     200, 
                                   XmNnumColumns,     1, NULL);


   XtVaSetValues( _buttons->W(),   XmNtopAttachment,  XmATTACH_WIDGET,
                                   XmNtopWidget,      _coord_box->W(),
                                   XmNleftAttachment, XmATTACH_OPPOSITE_WIDGET, 
                                   XmNleftWidget,     _coord_box->W(), 
                                   XmNtopOffset,      40,
                                   XmNnumColumns,     2, NULL);

   XtVaSetValues( _attribute->W(), XmNtopAttachment, XmATTACH_OPPOSITE_WIDGET,
                                  XmNtopWidget,       _coordradiobox->W(),
                                  XmNtopOffset,       63,
                                  XmNleftAttachment,  XmATTACH_WIDGET, 
                                  XmNleftWidget,      _coord_box->W(), 
                                  XmNleftOffset,      20, 
                                  XmNbottomAttachment,XmATTACH_OPPOSITE_WIDGET,
                                  XmNbottomWidget,   _enforcebox->W(), NULL); 

   XtVaSetValues( _newbox->W(),   XmNtopAttachment,  XmATTACH_WIDGET,
                                  XmNtopWidget,      _attribute->W(),
                                  XmNtopOffset,      10,
                                  XmNleftAttachment, XmATTACH_WIDGET, 
                                  XmNleftWidget,     _coord_box->W(), 
                                  XmNleftOffset,     25, NULL); 

   XtVaSetValues( _extendedcolorsbox->W(),  XmNtopAttachment,  XmATTACH_WIDGET,
                                  XmNtopWidget,      _newbox->W(),
                                  XmNtopOffset,      5,
                                  XmNleftAttachment, XmATTACH_WIDGET, 
                                  XmNleftWidget,     _coord_box->W(), 
                                  XmNleftOffset,     25, NULL);

   XtVaSetValues( _enforcebox->W(),XmNtopAttachment, XmATTACH_WIDGET,
                                  XmNtopWidget,     _coord_box->W(),
                                  XmNtopOffset,     0,
                                  XmNleftAttachment,XmATTACH_OPPOSITE_WIDGET, 
                                  XmNleftWidget,    _coord_box->W(), 
                                  XmNleftOffset,    0, 
                                  XmNrightAttachment,XmATTACH_OPPOSITE_WIDGET, 
                                  XmNrightWidget,    _coord_box->W(), 
                                  XmNrightOffset,    0, NULL);


   _selected = "Flag Elevations";

   _attribute->setList(atrname,NUM_ATTRIBUTES,_selected);

   _left    = XMIN_VAL;
   _right    = XMAX_VAL;
   _top    = YMIN_VAL;
   _bottom    = YMAX_VAL;
   _pwidth  = PWIDTH_VAL;
   _pheight = PHEIGHT_VAL;
   
   _coord_system = SURVEYSYSTEM;


   if(_app != NULL)
     {
     _newbox->unmanage();
     _extendedcolorsbox->unmanage();
     XtSetSensitive(_buttons->pushW(MAPOP),False);
     }

//Until implemented
  XtSetSensitive(_buttons->pushW(MAPOP),False);

   return topWidget();

}

SLPullPop * FgQcPop::pulldown()
{
  return _active_plot->_pulldown;
}

Widget FgQcPop::PW()
{
  return _active_plot->W();
}


//============================================================================
//====================== Create a new or use old plot=========================
//============================================================================
void FgQcPop::InstanceAction( void *data, long which )
{
FgQcPop *obj = (FgQcPop *)data;
FgQcPlot *lfp;

  obj->setPlotTypeParameters();

  //If the active plot has not yet been displayed just return,
  //otherwise make a new plot and keep track of the previous active plot
  if(which == NEWINSTANCE && obj->_first_time == False)
    {
    if(!obj->_active_plot->sp()->isPlotDisplayed()) return;
    obj->_previous_active_plot = obj->_active_plot;
    for(lfp=obj->_plist.top(); lfp; lfp=obj->_plist.next() )lfp->setInActive();
    if(obj->_app == NULL)
      obj->_active_plot = new FgQcPlot(obj->wParent(), "fgqc_plot", obj->_hctx,
                                       obj->_fg, obj, obj->_fgsp_list,
                                       obj->_numcolors);
    else
      obj->_active_plot = new FgQcPlot(obj->wParent(), "fgqc_plot", obj->_hctx,
                                       obj->_fg, obj, obj->_fgsp_list,
                                       obj->_numcolors, obj->_app);

    obj->_active_plot->reduceColors (obj->_numgraphics);
    obj->_active_plot->make(obj->wParent());
    if(obj->_app != NULL)obj->_active_plot->unmanage();
    obj->addPlot(obj->_active_plot);
    }


  //If the active plot has never been displayed check to see if we have
  //a previous one. If we have a previous and the active has not been
  //displayed then destroy the active and set the previous to be the new active.
  if(which == OLDINSTANCE)
    {
    if(obj->_previous_active_plot)
      {
       if(!obj->_active_plot->sp()->isPlotDisplayed() )
         obj->setActivePlot(obj->_active_plot, True, True);
      }
    }
    

}


//============================================================================
//====================== Allows private color map    =========================
//============================================================================
void FgQcPop::ExtendedColorsAction( void *data, long /*which*/ )
{
FgQcPop *obj = (FgQcPop *)data;
Boolean make_new = False;

  if(obj->_active_plot->sp()->isPlotDisplayed()) return; //should not happen

  if(obj->shaderRequested()) {
    obj->_numgraphics = PRIVATE_GRAPHICS;
    obj->_numcolors = PRIVATE_COLOR_MAP + obj->_numgraphics;
  }
  else {
    obj->_numgraphics = NORMAL_GRAPHICS;
    obj->_numcolors = NORMAL_COLOR_MAP + obj->_numgraphics;
  }

  //Delete plot class and create a new one with appropriate number of colors
  //If there is only one plot class setActivePlot will create a new one so
  //we do not want to make another here.
  if(obj->_plist.count() > 1) make_new = True;
  obj->setActivePlot(obj->_active_plot, True, True);    

  if(make_new)
    {
    if(obj->_app == NULL)
      obj->_active_plot = new FgQcPlot(obj->wParent(), "fgqc_plot", obj->_hctx,
                                       obj->_fg, obj, obj->_fgsp_list,
                                       obj->_numcolors);
    else
      obj->_active_plot = new FgQcPlot(obj->wParent(), "fgqc_plot", obj->_hctx,
                                       obj->_fg, obj, obj->_fgsp_list,
                                       obj->_numcolors, obj->_app);
    obj->addPlot(obj->_active_plot);
    
    }

  obj->_active_plot->reduceColors (obj->_numgraphics);
  obj->_active_plot->make(obj->wParent());

  if(make_new)obj->setActivePlot(obj->_active_plot);
  if(obj->_app != NULL)obj->_active_plot->unmanage();

}




//============================================================================
//====================== Enforce symetrical size     =========================
//============================================================================
void FgQcPop::EnforceAction( void *data, long /*which*/ )
{
FgQcPop *obj = (FgQcPop *)data;

  if(obj->_enforcebox->IsSelected(ENFORCE))
    {
    if(obj->_plot_type != NORMALIZED_BINNING)
       obj->_coord_box->SetValue(PHEIGHT,
                          obj->_active_plot->sp()->getSymetricalSize(False, 
                                                  obj->_left, obj->_right,
                                                  obj->_top, obj->_bottom,
                                                  &obj->_pwidth));
    else
      obj->_coord_box->SetValue(PHEIGHT,obj->_pwidth);
    }
}


//============================================================================
//====================== Is hill shading possible    =========================
//============================================================================
Boolean FgQcPop::shaderRequested()
{
  return (_extendedcolorsbox->IsSelected(EXTENDED_COLORS));
}



//============================================================================
//====================== Set grid or survey coordinates    ===================
//============================================================================
void FgQcPop::GridSurveyAction( void *data, long which )
{
FgQcPop *obj = (FgQcPop *)data;

 if(which == GRIDSYSTEM)
  {
  obj->_coord_system = GRIDSYSTEM;

  if(obj->_use_centers)
   {
   obj->_coord_box->SetValue(XMIN, (float)obj->_fg->getMinimumXgridBinCenter());
   obj->_coord_box->SetValue(XMAX, (float)obj->_fg->getMaximumXgridBinCenter());
   obj->_coord_box->SetValue(YMIN, (float)obj->_fg->getMaximumYgridBinCenter());
   obj->_coord_box->SetValue(YMAX, (float)obj->_fg->getMinimumYgridBinCenter());
   }
  else
   { 
   obj->_coord_box->SetValue(XMIN, (float)obj->_fg->minimumXgridInSurvey());
   obj->_coord_box->SetValue(XMAX, (float)obj->_fg->maximumXgridInSurvey());
   obj->_coord_box->SetValue(YMIN, (float)obj->_fg->maximumYgridInSurvey());
   obj->_coord_box->SetValue(YMAX, (float)obj->_fg->minimumYgridInSurvey());
   }

  //Make sure grids have been computed
  if(obj->_left == obj->_fg->minimumXlocInSurvey() &&
     obj->_right == obj->_fg->maximumXlocInSurvey() &&
     obj->_top == obj->_fg->maximumYlocInSurvey() &&
     obj->_bottom == obj->_fg->minimumYlocInSurvey())
           obj->_errpop = new SLErrorPop(obj->topWidget(), "Warning", nobins);
   }
 else //survey system
   {
   obj->_coord_system = SURVEYSYSTEM;
   obj->_coord_box->SetValue(XMIN, (float)obj->_fg->minimumXlocInSurvey());
   obj->_coord_box->SetValue(XMAX, (float)obj->_fg->maximumXlocInSurvey());
   obj->_coord_box->SetValue(YMIN, (float)obj->_fg->maximumYlocInSurvey());
   obj->_coord_box->SetValue(YMAX, (float)obj->_fg->minimumYlocInSurvey());
   }


  if(obj->_enforcebox->IsSelected(ENFORCE))
    {
    if(obj->_plot_type != NORMALIZED_BINNING)
       obj->_coord_box->SetValue(PHEIGHT,
                          obj->_active_plot->sp()->getSymetricalSize(False, 
                                                  obj->_left, obj->_right,
                                                  obj->_top, obj->_bottom,
                                                  &obj->_pwidth));
    else
      obj->_coord_box->SetValue(PHEIGHT,obj->_pwidth);
    }
}



//============================================================================
//====================== Internal method to update coordinates =================
//============================================================================
void FgQcPop::buttonCoordinates(void *data, long /*button*/ )
{
FgQcPop *obj = (FgQcPop *)data;

  obj->updateCoordinates();

}



//============================================================================
//====================== Public method to update coordinates =================
//============================================================================
void FgQcPop::updateCoordinates( )
{

  if(!made()) return;

  //When statics popup is called it causes an informer finishedChanges to be
  //called which then calls this function, but we do not want to update the
  //the coordinates at that time.
  if(_plot_type == STATICS) return;

  if(_coord_system == GRIDSYSTEM)
    {
    if(_use_centers)
      {
      _coord_box->SetValue(XMIN, (float)_fg->getMinimumXgridBinCenter());
      _coord_box->SetValue(XMAX, (float)_fg->getMaximumXgridBinCenter());
      _coord_box->SetValue(YMIN, (float)_fg->getMaximumYgridBinCenter());
      _coord_box->SetValue(YMAX, (float)_fg->getMinimumYgridBinCenter());
      }
    else
      { 
      _coord_box->SetValue(XMIN, (float)_fg->minimumXgridInSurvey());
      _coord_box->SetValue(XMAX, (float)_fg->maximumXgridInSurvey());
      _coord_box->SetValue(YMIN, (float)_fg->maximumYgridInSurvey());
      _coord_box->SetValue(YMAX, (float)_fg->minimumYgridInSurvey());
      }
    }
  else //survey system
    {
    _coord_system = SURVEYSYSTEM;
    _coord_box->SetValue(XMIN, (float)_fg->minimumXlocInSurvey());
    _coord_box->SetValue(XMAX, (float)_fg->maximumXlocInSurvey());
    _coord_box->SetValue(YMIN, (float)_fg->maximumYlocInSurvey());
    _coord_box->SetValue(YMAX, (float)_fg->minimumYlocInSurvey());
    }



  if(_enforcebox->IsSelected(ENFORCE))
    {
    if(_plot_type != NORMALIZED_BINNING)
      {
      if(_active_plot->sp() != NULL)
       _coord_box->SetValue(PHEIGHT, 
                            _active_plot->sp()->getSymetricalSize(False, 
                            _left, _right, _top, _bottom, &_pwidth));
      }
    else
      {
      _coord_box->SetValue(PHEIGHT,_pwidth);//insure a square
      }
    }

}



//============================================================================
//====================== Coordinate box actions      =========================
//============================================================================
void FgQcPop::CoordFocusAction( void *data, long which )
{
FgQcPop *obj = (FgQcPop *)data;

  if(which == PHEIGHT)return;
  obj->_focus_id = which;
  obj->_focus_value = obj->_coord_box->GetFloat((int)which);
}


void FgQcPop::CoordLosingFocusAction( void *data, long which )
{
FgQcPop *obj = (FgQcPop *)data;

  if(which == PHEIGHT)
    {
    if(obj->_plot_type == NORMALIZED_BINNING)
      obj->_coord_box->SetValue(PWIDTH,obj->_pheight);
    return;
    }


  if(which != obj->_focus_id)return;
  if(obj->_focus_value == obj->_coord_box->GetFloat((int)which)) return;

  if(obj->_enforcebox->IsSelected(ENFORCE))
    {
    if(obj->_plot_type != NORMALIZED_BINNING)
      obj->_coord_box->SetValue(PHEIGHT,  
                            obj->_active_plot->sp()->getSymetricalSize(False, 
                                                      obj->_left, obj->_right,
                                                      obj->_top, obj->_bottom,
                                                      &obj->_pwidth));
    else
      obj->_coord_box->SetValue(PHEIGHT,obj->_pwidth);//must be square
    }
}


Boolean FgQcPop::ValidInput()
{
 Boolean stat;

  if (made()) 
    stat= _questions_answered;
  else 
    stat= True;

  return (stat); 
}


void FgQcPop::UndoInput()
{
  SLFormPop::UndoInput();
}


void FgQcPop::okButton()
{

  if(_pwidth <= 0.0 || _pheight <= 0.0)
    {
     _errpop = new SLErrorPop(topWidget(), "Error", sizefail);
     return;
    }

  if(_plot_type == FOLD_OF_STACK)//Doesnt support reversing coordinates yet
    {
    if( _left > _right || _top < _bottom)
      {
      _errpop = new SLErrorPop(topWidget(), "Error", coord_error);
      return;
      }
    }

  if(_top == _bottom && _top == 0.0) //autogridder has problem with this
    {
    _top = -1.0;
    _bottom = 1.0;
    }


  if(!_questions_answered)
    {
    checkTimes();
    _ok_button = True;
    if (!_questions_answered) return;
    }

  if( ValidInput() ) 
    {
    unmanage();
    XmUpdateDisplay(W());
    DoAction();
    if (_completionFunc) _completionFunc( _completionData, FP_OK);
   }

  setQuestionsAnswered ();

}

void FgQcPop::applyButton()
{


  if(_top == _bottom && _top == 0.0) //autogridder has problem with this
    {
    _top = -1.0;
    _bottom = 1.0;
    }

  if(_pwidth <= 0.0 || _pheight <= 0.0)
    {
     _errpop = new SLErrorPop(topWidget(), "Error", sizefail);
     return;
    }

  if(_plot_type == FOLD_OF_STACK)//Doesnt support reversing coordinates yet
    {
    if( _left > _right || _top < _bottom)
      {
      _errpop = new SLErrorPop(topWidget(), "Error", coord_error);
      return;
      }
    }

  if(!_questions_answered)
    {
    checkTimes();
    _ok_button = False;
    if (!_questions_answered) return;
    }

  if( ValidInput() ) 
    {
    DoAction();
    if (_completionFunc) _completionFunc( _completionData, FP_APPLY);
    }

  setQuestionsAnswered ();
 
}

void FgQcPop::DoAction()
{

int stat;
  

  if(_top == _bottom && _top == 0.0) //autogridder has problem with this
    {
    _top = -1.0;
    _bottom = 1.0;
    }

  if(_pwidth <= 0.0 || _pheight <= 0.0)
    {
     _errpop = new SLErrorPop(topWidget(), "Error", sizefail);
     return;
    }

  ParentClass::DoAction();

  // If doing a statics type plot we need to copy the statics file
  // to the FgQcStaticsType plot which will make it's own copy. This
  // is necessary because we can have several statics file plots showing
  // and if one of those plots is replotted by it's color menu it would
  // get the last file read into the statics file pop up instead of
  // the one it was originally made from. MLS 07-30-2002
  if(_plot_type == STATICS)
    {
      _active_plot->setStaticsFilename(
                  (char *)getStaticsFile()->getCurrentFilename());
    }
    

  stat = _active_plot->plot(_plot_type);
  if(stat == False || _active_plot->sp()->isPlotDisplayed() == False)
    {
    if(!_active_plot->_grid_error)
      _errpop = new SLErrorPop(topWidget(), "Error", plotfail); 
    if(_app == NULL)  setActivePlot(_active_plot, True, True);
    if(_active_plot->sp()->isPlotDisplayed())_newbox->SetRadio(OLDINSTANCE);
    }
  else
    {
    _newbox->SetRadio(OLDINSTANCE);
    _first_plot_made = True;
    _active_plot->resetColors(False);
    }

  _active_plot->_grid_error = False;
   
}

//===========================================================================
//======== This method makes it possible to create a plot from   ============
//======== outside of this menu. Could be used to select an      ============
//======== area from an existing plot and displaying another     ============
//======== attribute from it.                                     ============
//===========================================================================
int FgQcPop::makeIndirectPlot(int plot_type, float width, float height,
                              float xmin, float ymin, float xmax, float ymax,
                              int coord_system)
{
int stat;

  _pwidth       = width;
  _pheight      = height;
  _left         = xmin;
  _right         = xmax;
  _top         = ymin;
  _bottom         = ymax;
  _coord_system = coord_system;
  _previous_active_plot = _active_plot;

  _active_plot = new FgQcPlot(wParent(),"fgqc_plot",_hctx,_fg,this,_fgsp_list,
                              PRIVATE_COLOR_MAP);
  addPlot(_active_plot);

  _active_plot->reduceColors (PRIVATE_GRAPHICS);
  _active_plot->make(wParent());

  stat = _active_plot->plot(plot_type);
  if(stat == False || _active_plot->sp()->isPlotDisplayed() == False)
    {
    if(!_active_plot->_grid_error)
      _errpop = new SLErrorPop(topWidget(), "Error", plotfail); 
    setActivePlot(_active_plot,True,True);
    if(_active_plot->sp()->isPlotDisplayed())_newbox->SetRadio(OLDINSTANCE);
    }
  else
    {
    setActivePlot(_active_plot);
    _active_plot->resetColors(False);
    }

  _active_plot->_grid_error = False;
  
  return(stat);
}


void FgQcPop::manage()
{
char *select = NULL;
  if(!_fg->numLines())
    {
    _coord_box->SetValue(PWIDTH,  PWIDTH_VAL);
    _coord_box->SetValue(PHEIGHT, PHEIGHT_VAL);
    _coord_box->SetValue(XMIN,    XMIN_VAL);
    _coord_box->SetValue(XMAX,    XMAX_VAL);
    _coord_box->SetValue(YMIN,    YMIN_VAL);
    _coord_box->SetValue(YMAX,    YMAX_VAL);
    return;
    }
  else if(_left == XMIN_VAL && _right == XMAX_VAL) // user has not set yet
    {
    if(_coord_system == SURVEYSYSTEM)
      {
      _coord_box->SetValue(XMIN, (float)_fg->minimumXlocInSurvey());
      _coord_box->SetValue(XMAX, (float)_fg->maximumXlocInSurvey());
      _coord_box->SetValue(YMIN, (float)_fg->maximumYlocInSurvey());
      _coord_box->SetValue(YMAX, (float)_fg->minimumYlocInSurvey());
      }
    else
      {
      _coord_box->SetValue(XMIN, (float)_fg->minimumXgridInSurvey());
      _coord_box->SetValue(XMAX, (float)_fg->maximumXgridInSurvey());
      _coord_box->SetValue(YMIN, (float)_fg->maximumYgridInSurvey());
      _coord_box->SetValue(YMAX, (float)_fg->minimumYgridInSurvey());
      }
    }

  if(_first_time)
    {
    if(_enforcebox->IsSelected(ENFORCE))
      {
      if(_fg->numLines())
        {
         _coord_box->SetValue(PHEIGHT,  
               _active_plot->sp()->getSymetricalSize(False, _left, _right,
                                                _top, _bottom,&_pwidth,True));
         _coord_box->SetValue(PWIDTH,_pwidth);
        }
      }
    _newbox->SetRadio(NEWINSTANCE);
    _first_time = False;
    _extendedcolorsbox->SetTog(EXTENDED_COLORS,False);
    }
  else
    {
    select = _attribute->getSelection();
    _attribute->setSelection(select);
    free(select);
    }


  XtManageChild(topWidget()); 

}


 
void FgQcPop::reloadDefaults(Boolean)
{
  SLFPopSep::reloadDefaults();
  _coord_box->reloadDefaults();
  _coordradiobox->reloadDefaults();
  DoAction();
}



void FgQcPop::reloadSystemDefaults(Boolean do_method)
{
  SLFPopSep::reloadSystemDefaults(do_method);
  _coord_box->SetValue(PWIDTH,  PWIDTH_VAL);
  _coord_box->SetValue(PHEIGHT, PHEIGHT_VAL);
  _coord_box->SetValue(XMIN,    XMIN_VAL);
  _coord_box->SetValue(XMAX,    XMAX_VAL);
  _coord_box->SetValue(YMIN,    YMIN_VAL);
  _coord_box->SetValue(YMAX,    YMAX_VAL);
  DoAction();
}




//============================================================================
//====================== Misc options buttons        =========================
//============================================================================
void FgQcPop::button_control( void *data, long button)
{
  FgQcPop *obj = (FgQcPop *)data;

  switch(button) 
    {
    case MAPOP:
      //Put Trey's overlays here
      break;

    case COLOROP:
      if(obj->_plot_type != NORMALIZED_BINNING &&
         obj->_plot_type != BIN_CENTERS)
        {
        obj->_active_plot->manageColorPop();
        obj->_active_plot->enableColorOptions();
        obj->_active_plot->resetColors(False);
        }
      break;

    case CBAROP:
      obj->_active_plot->manageColorBar();
      break;
    }
        
}



//============================================================================
//====================== List Picker plot type notify=========================
//============================================================================
Boolean FgQcPop::notifyComplex(SLDelay *obj, int ident)
{
Boolean stat = True;
char *select = NULL;
      
//Only used on the list picker widget 
  if(obj != _attribute && obj != _question_pop) return(stat);

  _previous_plot_type = _plot_type;
  _previous_selected = _selected;
 



  if(obj == _attribute)
    {
    switch(ident)
      {
      case SLListPicker::SINGLE_CLICK:
      case SLListPicker::TEXT_UNIQUE:
        {
        select = _attribute->getSelection();
        for(int i = 0; i < NUM_ATTRIBUTES; i++)
          if(strcmp(_attribute->getSelection(), atrname[i]) == 0)
            _plot_type = i;
        if(setPlotTypeParameters()) setSubMenu();
        if(_active_plot->getColorPop()->beenManaged())
           _active_plot->resetColors(True);
        free(select);
        }
      break;

      default:    
      break;
      }
    }
  else // question pop
    {
     if(ident)
       {
       _questions_answered = True;
       if(_ok_button)
         okButton();
       else
         applyButton();
       }
     else
       {
       _questions_answered = False;
       }
    } 



  return(True);
}


//============================================================================
//============= Create and/or manage any sub menus here ======================
//============================================================================
void FgQcPop::setSubMenu()
{
  switch(_plot_type)
    {
    case FOLD_OF_STACK:
    case AZIMUTHAL_DISTRIBUTION:
      if(_distribution_menu == NULL)
        {
         if(_plot_type == FOLD_OF_STACK)
            _distribution_menu = 
              new OffsetDistributionMenu(topWidget(),"offset_menu",
                                         _hctx,this);
         else
            _distribution_menu = 
              new OffsetDistributionMenu(topWidget(),"offset_menu",
                                         _hctx,this,AZIMUTHALD);
        }
      _distribution_menu->setActivePlot(_active_plot);
      _distribution_menu->makeAndManage();
    break;

    case NORMALIZED_BINNING:
      if(_normalized_bin_menu == NULL)
         _normalized_bin_menu = 
           new NormalizedBinMenu(topWidget(),"normalized_bin",_hctx,this);
      _normalized_bin_menu->setActivePlot(_active_plot);
      _normalized_bin_menu->makeAndManage();
    break;

    case STATICS:
      _srp->makeAndManage();
    break;

    case HEADER_WORD:
// check validity of making a header word plot
      if (_fg->sourceGathersOutOfDate ()   &&
          _fg->receiverGathersOutOfDate () &&
          _fg->midpointGathersOutOfDate ()   ) {
        _selected = _previous_selected;
        _plot_type = _previous_plot_type;
        _errpop = new SLErrorPop (topWidget(), "Error", nogathers);
        _attribute->setList (atrname, NUM_ATTRIBUTES, _selected);
        return;
      }
      else {
        if (!_header_word_menu)
          _header_word_menu =
            new HeaderWordMenu (topWidget(), "header_menu", _hctx, this);
        _header_word_menu->setActivePlot (_active_plot);
        _header_word_menu->makeAndManage ();
      }
    }
}

//============================================================================
//============= General routine to set plot type parameters ==================
//============================================================================
Boolean FgQcPop::setPlotTypeParameters()
{
Boolean stat = True;

  if(_newbox->WhichSelected() == OLDINSTANCE)
     XtSetSensitive( _extendedcolorsbox->TogW(EXTENDED_COLORS), False);
  else
     XtSetSensitive( _extendedcolorsbox->TogW(EXTENDED_COLORS), True);


  //Disable color options if the following plot types are selected
  if(_plot_type == NORMALIZED_BINNING || _plot_type == BIN_CENTERS ||
     _plot_type == SOURCE_PROGRESSION)
    {
    XtSetSensitive(_buttons->pushW(COLOROP),False);
    XtSetSensitive(_buttons->pushW(CBAROP),False);
    }
  else
    {
    XtSetSensitive(_buttons->pushW(COLOROP),True);
    XtSetSensitive(_buttons->pushW(CBAROP),True);
    }
  //Unmanage the color popup if it is up and the plot type has been changed
  _active_plot->unmanageColorPop();



  switch(_plot_type)
    {
    case ELEVATIONS:
      _use_centers = False;
      _coordradiobox->SetRadio(SURVEYSYSTEM);
      break;

    case BIN_CENTERS:
    case FOLD_OF_STACK:
    case CMP_DISTRIBUTION:
    case NORMALIZED_BINNING:
    case AZIMUTHAL_DISTRIBUTION:
    case SOURCE_PROGRESSION: 
      if(_fg->midpointGathersOutOfDate() && _plot_type != SOURCE_PROGRESSION)
        {
        _errpop = new SLErrorPop(topWidget(), "Error", nobins);
        _plot_type = _previous_plot_type;
        _selected =  _previous_selected;
        _attribute->setSelection(_selected);
        stat = False;
        return(stat);
        }
      else if(_plot_type == SOURCE_PROGRESSION && 
              _fg->sourceGathersOutOfDate())
        {
        _errpop = new SLErrorPop(topWidget(), "Error", nobins);
        _plot_type = _previous_plot_type;
        _selected =  _previous_selected;
        _attribute->setSelection(_selected);
        stat = False;
        return(stat);
        }
      if(_fg->getXgridWidth() < 2.0)
        {
        _errpop = new SLErrorPop(topWidget(), "Error", notransform);
        _plot_type = _previous_plot_type;
        _selected =  _previous_selected;
        _attribute->setSelection(_selected);
        stat = False;
        return(stat);
        }
      if(_plot_type == FOLD_OF_STACK)//Dont support reversing coordinates yet
        {
        if( _left > _right || _top < _bottom)
          {
          _errpop = new SLErrorPop(topWidget(), "Error", coord_error);
          _plot_type = _previous_plot_type;
          _selected =  _previous_selected;
          _attribute->setSelection(_selected);
          stat = False;
          return(stat);
          }
        }
      _use_centers = True;
      if(_plot_type != SOURCE_PROGRESSION) 
         _coordradiobox->SetRadio(GRIDSYSTEM);
      else
        _coordradiobox->SetRadio(SURVEYSYSTEM);
      break;


    case DIFFERENCES:
    case STATICS:
       _use_centers = False;
       break;

 
    case HEADER_WORD:
    case HOLE_DEPTHS:
    case RECEIVER_SHIFTS:
    case SOURCE_SHIFTS:
    case UPHOLES:
    case SOURCE_ELEVATIONS:
      _use_centers = False;
      _coordradiobox->SetRadio(SURVEYSYSTEM);
      break;

    }

  setQuestionsAnswered ();

  return(stat);
}

void FgQcPop::setQuestionsAnswered ()
{
  switch(_plot_type)
    {
    case ELEVATIONS:
    case FOLD_OF_STACK:
    case BIN_CENTERS:
    case NORMALIZED_BINNING:
    case AZIMUTHAL_DISTRIBUTION:
    case DIFFERENCES:
    case HOLE_DEPTHS:
    case RECEIVER_SHIFTS:
    case SOURCE_SHIFTS:
    case SOURCE_PROGRESSION:
    case STATICS:
    case UPHOLES:
    case SOURCE_ELEVATIONS:
      _questions_answered = True;
      break;

    case HEADER_WORD:
    case CMP_DISTRIBUTION:
      _questions_answered = False;
      break;  

    }
}

//============================================================================
//====================== Add a QC plot to list       =========================
//============================================================================

void FgQcPop::addPlot(FgQcPlot *newfp)
{
  _plist.add(newfp);
}


//============================================================================
//============= Set a QC plot active, also used for removes  =================
//============================================================================
void FgQcPop::setActivePlot(FgQcPlot *fp, Boolean remove, Boolean do_destroy)
{
FgQcPlot *lfp = NULL;

  if(remove)
    {
    _plist.remove(fp);
    if(do_destroy) delete fp;
    }
  else
    {
    _active_plot = fp;
    }

  //First set all plots inactive
  for(lfp = _plist.top(); lfp; lfp = _plist.next() ) lfp->setInActive();

  lfp = _plist.top();

  //If we are removing a plot and still have one left, make it the new active
  if(remove && lfp)
    {  
    lfp->setActive();
    _active_plot = lfp;
    }
  else if(!lfp) //Make sure we have a plot class around at all times
    {
    if(_app == NULL)
      _active_plot = new FgQcPlot(wParent(), "fgqc_plot", _hctx, _fg, this,
                                  _fgsp_list, _numcolors);
    else
      _active_plot = new FgQcPlot(wParent(), "fgqc_plot", _hctx, _fg, this,
                                  _fgsp_list, _numcolors, _app);

    _active_plot->reduceColors (_numgraphics);
    _active_plot->make(wParent());
    if(_app != NULL) _active_plot->unmanage();
    addPlot(_active_plot);
    _active_plot->setActive();
    return;
    }

  if(!remove) _active_plot->setActive();

  if(_active_plot->sp()->isPlotDisplayed())
    _coordradiobox->SetRadio(_active_plot->getPlottedCoordinateSystem());
  else
    _coordradiobox->SetRadio(_active_plot->getCoordinateSystem());

  if(_active_plot->sp()->isPlotDisplayed())
    {
    _coord_box->SetValue(PWIDTH,  _active_plot->getPlotWidth());
    _coord_box->SetValue(PHEIGHT, _active_plot->getPlotHeight());
    _coord_box->SetValue(XMIN,    _active_plot->getUserLeft());
    _coord_box->SetValue(XMAX,    _active_plot->getUserRight());
    _coord_box->SetValue(YMIN,    _active_plot->getUserTop());
    _coord_box->SetValue(YMAX,    _active_plot->getUserBottom());
    }

}


StaticsFile *FgQcPop::getStaticsFile ()
{
  return _srp->getFile ();
}


void FgQcPop::checkTimes()
{

   _questions_answered = False;
   char *string = 0;
   FgQcTraceCounter *ftc;
  
  switch(_plot_type)
    {
    case HEADER_WORD:
      ftc = new FgQcTraceCounter (_data_loc, this);
      string = ftc->questionText ();
      delete ftc;
    break;

    case AZIMUTHAL_DISTRIBUTION:
      ftc = new FgQcTraceCounter (PLOT_AT_CMP, this);
      string = ftc->questionText ();
      delete ftc;
    break;

    case CMP_DISTRIBUTION:
      ftc = new FgQcTraceCounter (CMP_DISTRIBUTION, this);
      string = ftc->questionText ();
      delete ftc;
    break;
    }



    if (strcmp(string,"") != 0) {
      _question_pop = new SLQuestPop (topWidget(), "Time Notice",string);
      _question_pop->setComplexNotify(this);
    }
    else
      _questions_answered = True;

}

long FgQcPop::getOffsetLimitType ()
{
  return _distribution_menu->getLimitType ();
}

long FgQcPop::getAzimuthLimitType ()
{
  return _distribution_menu->getLimitType ();
}

float FgQcPop::getOffsetMinimum ()
{
  return _distribution_menu->getOffsetMinimum ();
}

float FgQcPop::getOffsetMaximum ()
{
  return _distribution_menu->getOffsetMaximum ();
}

float FgQcPop::getAzimuthMinimum ()
{
  return _distribution_menu->getAzimuthMinimum ();
}

float FgQcPop::getAzimuthMaximum ()
{
  return _distribution_menu->getAzimuthMaximum ();
}

float FgQcPop::getNormalizedBinPercent()
{
  return _normalized_bin_menu->getNormalizedBinPercent();
}

char *FgQcPop::getAttributeName (int plot_type)
{
  if (plot_type < 0 || plot_type >= NUM_ATTRIBUTES) return 0;
  return atrname[plot_type];
}

int FgQcPop::getVersionNumber (FgQcPlot *plot)
{
  FgQcPlot *lfp = 0;
  char title[40];
  int match = 1, version = 0;
  while (match) {
    match = 0;
    for (lfp = _plist.top(); lfp && !match; lfp = _plist.next()) {
      if (lfp != plot)
        match = !strcmp (lfp->getTitle(), plot->titleVersion(title,version));
    }
    if (match) version++;
  }
  return version;
}

int FgQcPop::numberInPrivateColorMap ()
{
  return PRIVATE_COLOR_MAP;
}
