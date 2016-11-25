#include "fgmap/fg_map_anno.hh"
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
#include "fgmap/fg_map.hh"
#include "fgmap/fg_map_pick.hh"
#include "sp/seis_plot.hh"
#include "fgmap/fg_seis_plot.hh"
#include "fgmap/fg_loc_out.hh"
#include "fgxp/ll_fgxp_plot.hh"
#include "fgxp/ll_fgxp_vect.hh"
#include "fgxp/fgxp_2d_pick.hh"
#include "fgxp/fgxp_2d_plot.hh"
#include "fgmap/fgmap_control_pop.hh"
#include "fgmap/fg_map_to_flag.hh"
#include "fgmap/fg_seis_plot_list.hh"
#include "sl/sl_app.hh"
#include "sl/sl_pull_pop.hh"
#include "sl/shell_watch.hh"
#include "vect/vector.hh"
#include <assert.h>


static const String  defres[]= {
    ".resizePolicy:   RESIZE_NONE",
    "_popup.title:    Map View",
    ".height:         400",
    ".width:          550",
    NULL };

enum _Actions { RESCALE, NOACTION };

enum _PULL_BUTTONS { SCALE, MAPCP= 99 };


// Make map as form
FgMap::FgMap(Widget p, FgSeisPlotList *fgsp_list, SLApp *app)  
             : SLFormHelpPop(p, "map", FP_DOREMOVE|FP_DOHELP, 
                             fgsp_list->helpCtx(), False, 2, False, False), 
               FgInform(fgsp_list->fieldGeometry()), 
               _sp(NULL),
               _fieldg(fgsp_list->fieldGeometry()),
               _plot_list(NULL),
               _picker(NULL),
               _first_time(True),
               _action(NOACTION),  
               _curr_plot(NULL),
               _fgsp_list(fgsp_list),
               _pulldown(NULL),
               _pick_cas(NULL),
               _type_cas(NULL),
               _mode_cas(NULL), 
               _changing(False), _first_plot(True), 
               _selected_lines_only(False)
{
   setDefaultResources( p, "map", defres);

   _pulldown= new SLPullPop("Map", NULL, app);
   _type_cas= new SLPullPop("Map Type", _pulldown);
   _pick_cas= new SLPullPop("Picking Mode", _pulldown);
   _pulldown->addSep();
   _mode_cas= new SLPullPop("Display Mode", _pulldown);
   _pulldown->addPush("Rescale", SCALE);
   _pulldown->addPush("Control Panel...", MAPCP);

   _type_cas->addRadio("Plot by Location", MAP_LOCATION);
   _type_cas->addRadio("Plot by Location - Selected Lines", MAP_LOC_SELECTED);
   _type_cas->addRadio("Plot by Grid",     MAP_GRID);

   _pick_cas->addRadio("Edit Flags",       EditFlag);
   _pick_cas->addRadio("Flag Info",        FlagInfo);
   _pick_cas->addRadio("Select Area",      SelectFlags);
   _pick_cas->addRadio("Compute Distance", Distance);
   _pick_cas->addRadio("Show Rec/Shots",   ShowRec);
   _pick_cas->addRadio("Interpolate",      Interpolate);
   _pick_cas->setRadioValue(FlagInfo);

   _mode_cas->addRadio("Show Lines Only", Lines);
   _mode_cas->addRadio("Show Flags Only", Flags);
   _mode_cas->addRadio("Lines & Flags"  , LinesAndFlags);
   _mode_cas->addRadio("Lines & Auto"   , LinesAndAutoFlags);

   _pulldown->setComplexNotify(this);
   _type_cas->setComplexNotify(this);
   _pick_cas->setComplexNotify(this);
   _mode_cas->setComplexNotify(this);
   make(p);
}

// Make map as dialog
FgMap::FgMap(Widget p, FgSeisPlotList *fgsp_list)  
             : SLFormHelpPop(p, "map", FP_DOREMOVE|FP_DOHELP, 
                             fgsp_list->helpCtx(), False, 2, False, 
                             True, 0), 
               FgInform(fgsp_list->fieldGeometry()), 
               _fieldg(fgsp_list->fieldGeometry()), 
               _action(NOACTION), _picker(NULL), _first_time(True),
               _fgsp_list(fgsp_list), _curr_plot(NULL),
               _changing(False), _type_cas(NULL), _pick_cas(NULL), 
               _mode_cas(NULL), _pulldown(NULL), 
               _first_plot(True), _sp(NULL)
{
   _control_pop= new FgMapControlPop(this, "Map_control", this);   
   setDefaultResources( p, "map", defres);
   addExtraButton("Control...", MAPCP);
}

FgMap::~FgMap()
{
  delete _plot_list;
  if (_picker) delete _picker;
  delete _sp;
  delete _fg_loc_out;
  delete _control_pop;
  delete _map_anno;
  if (_pulldown) delete _pulldown;
  if (_type_cas) delete _type_cas;
  if (_pick_cas) delete _pick_cas;
  if (_mode_cas) delete _mode_cas;
}


Widget FgMap::make( Widget p)
{
  if (made()) return topWidget();

  SLFormHelpPop::make(p);
  XtVaSetValues( topWidget(), XmNresizePolicy, XmRESIZE_NONE, NULL);

  _plot_list= new FgXpPlotLinkedList(_fgsp_list->defaultLineColor (),
                                     _fgsp_list->sourceLineColor  (),
                                     _fgsp_list->receiverLineColor(),
                                     _fgsp_list->bothLineColor    (),
                                     _fgsp_list->selectedLineColor(),
                                     _fgsp_list->activeLineColor  (),
                                     _fgsp_list->defaultFlagColor (),
                                     _fgsp_list->computedFlagColor(),
                                     _fgsp_list->selectedFlagColor(),
                                     _fgsp_list->activeFlagColor  (),
                                     _fgsp_list->useActiveLine    (),
                                     _fgsp_list->useSelectedLine  (),
                                     _fgsp_list->useBothLine      (),
                                     _fgsp_list->useReceiverLine  (),
                                     _fgsp_list->useSourceLine    (),
                                     _fgsp_list->useActiveFlag    (),
                                     _fgsp_list->useSelectedFlag  (),
                                     _fgsp_list->useComputedFlag  (),
                                     _fgsp_list->flagMode         () );

  _sp= new FgSeisPlot(topWidget(), "plot", _fgsp_list, True, True);
  _sp->setXpList(_plot_list);
  _fg_loc_out = new FgLocOut(topWidget(), "out", getHelpCtx(), _fg, _sp);
  _fg_loc_out->setTranslator(_fgsp_list->translator());
  _fg_loc_out->setAttr2(FG_SKID_TYPE);
  _control_pop= new FgMapControlPop(this, "Map_control", this);   

  XtVaSetValues(_sp->W(), XmNleftAttachment,   XmATTACH_FORM,
                          XmNrightAttachment,  XmATTACH_FORM,
                          XmNtopAttachment,    XmATTACH_FORM,
                          XmNbottomAttachment, XmATTACH_WIDGET,
                          XmNbottomWidget,     _fg_loc_out->W(),
                          NULL);

  if (SLFormPop::isDialog()) {

        XtVaSetValues(_fg_loc_out->W(), XmNrightAttachment,  XmATTACH_FORM,
                                        XmNbottomAttachment, XmATTACH_WIDGET,
                                        XmNbottomWidget,     bottomSeparator(),
                                        NULL);
  }
  else {
        XtVaSetValues(_fg_loc_out->W(), XmNrightAttachment,  XmATTACH_FORM,
                                        XmNbottomAttachment, XmATTACH_FORM,
                                        NULL);

  }
  _sp->setPlotType(PlotImage::PlotGRID);
  _sp->setPlotSize(6.0,6.0);
  _sp->setGridXYS(0.0, 100.0, 100.0, 0.0);

  _map_anno=  new FgMapAnno(_fieldg, _sp);

  updateFromControlPanel();
  setMapType((MapType)_control_pop->currentSelectedPlot()); 
  changeSelectedPlot((MapType)_control_pop->currentSelectedPlot());

  _map_anno->setPlotIsGrid(_curr_map_type==MAP_GRID);

  return topWidget();
}


void FgMap::updateFromControlPanel()
{
  enablePicking(_control_pop->currentPicking()); 
  changeSelectedPicking(_control_pop->currentPicking());

  setDisplayMode((DisplayMode)_control_pop->currentDisplay()); 
  changeSelectedDisplay((DisplayMode)_control_pop->currentDisplay()); 
}




void FgMap::extraButton(int ident)
{
  if (ident==MAPCP) {
        _control_pop->makeAndManage();
  }
  else assert(0);
}




void FgMap::enablePicking(int mode)
{
  FgMapPick *map_pick= NULL;
  Boolean is_grid= False;
  if (_picker) {
       delete _picker;
       _picker= NULL;
  } // end if
  if (_sp) {
     if (_sp->isPlotDisplayed()) {
        switch (mode) {
          case FlagInfo:
                     map_pick= new FgMapPick(_sp, _fieldg, _plot_list, 
                                            FgMapPick::FlagInfo);
                     map_pick->setPlotIsSelected(_selected_lines_only);
                     break;
          case ShowRec:
                     map_pick= new FgMapPick(_sp, _fieldg, _plot_list, 
                                            FgMapPick::ShowRec);
                     map_pick->setPlotIsSelected(_selected_lines_only);
                     break;
          case Distance:
                     map_pick= new FgMapPick(_sp, _fieldg, _plot_list, 
                                            FgMapPick::Distance, _fg_loc_out);
                     map_pick->setPlotIsSelected(_selected_lines_only);
                     break;
          case EditFlag:
                     _picker= new FgXp2DPick( _sp, _plot_list, 
                                              noConstraint,
                                              False, selectFlags,
                                              _fg_loc_out);
                     break;
          case MapNone:
                      break;
          case SelectFlags:
                     map_pick= new FgMapPick(_sp, _fieldg, _plot_list, 
                                            FgMapPick::SelectFlags);
                     map_pick->setPlotIsSelected(_selected_lines_only);
                     break;
          case Interpolate:
                     map_pick= new FgMapPick(_sp, _fieldg, _plot_list, 
                                            FgMapPick::Interpolate);
                     map_pick->setPlotIsSelected(_selected_lines_only);
                     break;
          case ClosestLine:
                     if (_curr_map_type == MAP_GRID) is_grid= True;
                     map_pick= new FgMapPick(_sp, _fieldg, 
                                            _fgsp_list->translator(),
                                            is_grid);
                     map_pick->setPlotIsSelected(_selected_lines_only);
                     break;
          default: assert(0); break;
        } // end switch mode
     } // end if isPlotDisplayed
  } // end if _sp
  if (mode != MapNone) _control_pop->changeSelectedPicking(mode);
  if (map_pick) _picker= map_pick;
}


void FgMap::anyPlot(Boolean    doit, 
                    int        xtype, 
                    int        ytype,
                    SelectMode smode)
{
     if (doit) {
          if (_curr_plot) delete _curr_plot;
          _curr_plot= new FgXp2DPlot(_fieldg, _plot_list, smode, 
                               (DisplayMode)_control_pop->currentDisplay(),
                                    1, LdCardType, xtype, -1, LdCardType,
                                    ytype, -1, 3);

         if (XtWindow(_sp->W())) {
               scalePlot();
         } // end if XtWindow
         _curr_plot->addPlot(_sp, (Bool)_first_time);
         _first_time= False;
         if (_map_anno) _map_anno->setPlotIsGrid(_curr_map_type==MAP_GRID);
     }
     else if (_curr_plot) {
          delete _curr_plot;
          _curr_plot= NULL;
     }
}

void FgMap::scalePlot()
{
   float xMin, xMax, yMin, yMax, grid_height, grid_width;
   if ( (made()) && (_curr_plot) ) {
      if (_plot_list->scale(&xMin, &xMax, &yMin, &yMax, NULL, NULL, 0.02)) { 
          ShellWatch sw;
          _sp->setGridXYS(xMin, xMax, yMax, yMin);
          grid_width= _sp->gridWidth();
          grid_height= _sp->getSymetricalSize(False,xMin,xMax,
                                         yMin,yMax,&grid_width,True);
          _sp->setGridWidth(grid_width);
          _sp->setGridHeight(grid_height);
          _sp->setSymetricalAnnotation(xMin, xMax, yMin, yMax);
          _sp->setDrawXlines(True);
          _sp->plot();

          if (_first_plot) {
                updateFromControlPanel();
                // _fgsp_list->translator()->drawGrid(_sp,False);
          } /// end if
         _first_plot=False;
         _map_anno->show();
      } // end if
   } // end if
}

void FgMap::showFlagsByGrid(Boolean doit)
{
    anyPlot(doit, FG_XGRID, FG_YGRID, AllLines);
    _fg_loc_out->setSelectedLinesOnly(False);
    _selected_lines_only= False;
}

void FgMap::showFlagsByLoc(Boolean doit)
{
    anyPlot(doit, FG_XLOC, FG_YLOC, AllLines);
    _fg_loc_out->setSelectedLinesOnly(False);
    _selected_lines_only= False;
}

void FgMap::showFlagsByLocSelected(Boolean doit)
{
    anyPlot(doit, FG_XLOC, FG_YLOC, SelectedLines);
    _fg_loc_out->setSelectedLinesOnly(True);
    _selected_lines_only= True;
}


void FgMap::setMapType(MapType map_type)
{
   if ((!_curr_plot) || (_curr_map_type!= map_type)) {
      anyPlot(False);
      _curr_map_type= map_type;
      switch (map_type) {
         case MAP_LOCATION:  
                        anyPlot(True, FG_XLOC, FG_YLOC, AllLines);
                        _fg_loc_out->translateByGrid(False);
                        _fg_loc_out->setSelectedLinesOnly(False);
                        _selected_lines_only= False;
                        break;
         case MAP_GRID:  
                        anyPlot(True, FG_XGRID, FG_YGRID, AllLines);
                        _fg_loc_out->translateByGrid(True);
                        _fg_loc_out->setSelectedLinesOnly(False);
                        _selected_lines_only= False;
                        break;
         case MAP_LOC_SELECTED:  
                        anyPlot(True, FG_XLOC, FG_YLOC, SelectedLines);
                        _fg_loc_out->translateByGrid(False);
                        _fg_loc_out->setSelectedLinesOnly(True);
                        _selected_lines_only= True;
                        break;
         default:       assert(0); break;

      } // end switch
   } // end if
   _control_pop->changeSelectedPlot((int)map_type);
   updateFromControlPanel();
}


void FgMap::managing()
{
   if (_sp) {
      if ((XtWindow(_sp->W()))&&(_first_plot)) {
          scalePlot();
      } // end if
   } // end if _sp
  SLFormPop::managing();
}


void FgMap::showShots(Boolean ) { }
void FgMap::showReceivers(Boolean ) { }
void FgMap::showMidpoints(Boolean ) { }

void FgMap::freezingDependentUpdates(FieldGeometry *)
{
}

void FgMap::dependentValuesOutOfDate(FieldGeometry *)
{
  _need_to_rescale= True;
}

void FgMap::postResumeDependentUpdates(FieldGeometry *)
{
  if (_need_to_rescale) {
      scalePlot();
      _need_to_rescale= False;
  } // end if
}

void FgMap::postNewGridTransform(FieldGeometry *)
{
  if (made()) {
       if (_curr_map_type != MAP_LOCATION) {
              _action= RESCALE;
              scalePlot();
       } // end if
  } // end if _made
}


void FgMap::finishedChanges(FieldGeometry *)
{
   if (made()) {
      if ((XtWindow(_sp->W()))&&(_first_plot)) {
          scalePlot();
      } // end if
   } // end if _sp
   switch (_action) {
     case  RESCALE:  
                 break;
     case  NOACTION:  
                 break;
     default:
                 assert(0);
                 break;
   };

   _action= NOACTION;
}



FgSeisPlot *FgMap::sp()
{
  return _sp;
}

FgXpPlotLinkedList *FgMap::xList()
{
  return _plot_list;
}

void FgMap::setDisplayMode( DisplayMode mode)
{
  if ((_curr_plot) &&(_sp->isPlotDisplayed())) {
        _curr_plot->setDisplayMode(mode);
  }
}

void FgMap::changeSelectedPicking(int ident)
{
  if (_pick_cas) {
     _changing= True;
     _pick_cas->setRadioValue(ident);
     _changing= False;
  }
}
void FgMap::changeSelectedPlot(int ident)
{
  if (_type_cas) {
     _changing= True;
     _type_cas->setRadioValue(ident);
     _changing= False;
  }
}
void FgMap::changeSelectedDisplay(int ident)
{
  if (_mode_cas) {
     _changing= True;
     _mode_cas->setRadioValue(ident);
     _changing= False;
  }
}


Boolean FgMap::notifyComplex(SLDelay *obj, int ident)
{
  if (!_changing) {
      if (obj==_type_cas) {
             setMapType((MapType)ident);
      } // end if
      else if (obj==_pick_cas) {
             enablePicking(ident);
      } // end if
      else if (obj==_mode_cas) {
                 setDisplayMode((DisplayMode)ident);
                 _control_pop->changeSelectedDisplay(ident);
      }
      else if (obj==_pulldown) {
         switch (ident) {
             case SCALE:  scalePlot();             break;
             case MAPCP:  _control_pop->makeAndManage();  break;
         } // end switch
      } // end if
  } // end if !_changing
  _changing= False;
  return True;
}
