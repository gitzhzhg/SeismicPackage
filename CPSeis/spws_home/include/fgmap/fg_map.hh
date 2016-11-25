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
#ifndef FG_MAP_HH
#define FG_MAP_HH

#include <Xm/Xm.h>
#include "sl/sl_form.hh"
#include "sl/sl_form_help_pop.hh"
#include "geom/fg_inform.hh"
#include "fgxp/fgxp_constants.hh"
#include "geom/fg_constants.hh"

class SeisPlot;
class SeisGridPop;
class FieldGeometry;
class FgXpPlotLinkedList;
class FgXp2DPlot;
class FgMapPick;
class FgMapAnno;
class PickBase;
class SeisZoomOpPop;
class FgSeisPlotList;
class FgSeisPlot;
class FgLocOut;
class FgMapControlPop;

class SLApp;
class SLPullPop;


class FgMap : public SLFormHelpPop, public FgInform {

  public:
      typedef enum _MapType { MAP_LOCATION, MAP_GRID, 
                              MAP_LOC_SELECTED, MAP_MIDPOINT } MapType;
  private:
  protected:
      FgSeisPlot           *_sp;
      SLForm               *_main_form;
      FieldGeometry        *_fieldg;
      FgXpPlotLinkedList   *_plot_list;
      PickBase             *_picker;
      FgMapAnno            *_map_anno;
      Boolean              _first_time;
      int                  _action;
      FgXp2DPlot           *_curr_plot;
      FgSeisPlotList       *_fgsp_list;
      SLPullPop            *_pulldown;
      SLPullPop            *_pick_cas;
      SLPullPop            *_type_cas;
      SLPullPop            *_mode_cas;
      FgLocOut             *_fg_loc_out;
      FgMapControlPop      *_control_pop;
      Boolean              _changing;
      Boolean              _first_plot;
      Boolean              _need_to_rescale;
      MapType              _curr_map_type;
      Boolean              _selected_lines_only;

      void anyPlot(Boolean    doit, 
                   int        xtype =FG_XGRID, 
                   int        ytype =FG_YGRID,
                   SelectMode smode =SelectedLines);
      Boolean notifyComplex(SLDelay *obj, int ident);
      virtual void    extraButton(int ident);
   
  public:
      FgMap(Widget p, FgSeisPlotList *fgsp_list, SLApp *app);
      FgMap(Widget p, FgSeisPlotList *fgsp_list);
      virtual ~FgMap();

      void showFlagsByLoc(Boolean doit);
      void showFlagsByLocSelected(Boolean doit);
      void showFlagsByGrid(Boolean doit);

      void setMapType(MapType map_type);

      void showShots(Boolean doit);
      void showReceivers(Boolean doit);
      void showMidpoints(Boolean doit);

      void updateFromControlPanel();

      void changeSelectedPicking(int ident);
      void changeSelectedPlot(int ident);
      void changeSelectedDisplay(int ident);

      Widget make( Widget p =NULL);
      virtual void managing();
      enum { FlagInfo, EditFlag, ShowRec, ShowShot, SelectFlags, 
             Distance, ClosestLine, Interpolate, MapNone };
      void enablePicking(int mode= FlagInfo);
      virtual void dependentValuesOutOfDate   (FieldGeometry *fg);
      virtual void freezingDependentUpdates   (FieldGeometry *fg);
      virtual void postResumeDependentUpdates (FieldGeometry *fg);  
      virtual void postNewGridTransform (FieldGeometry *fg);  
      virtual void finishedChanges    (FieldGeometry *fg);  
      void setDisplayMode( DisplayMode mode);
      void scalePlot();
      FgSeisPlot *sp();
      FgXpPlotLinkedList *xList();
      SLPullPop *pulldown() { return _pulldown;}
};

#endif
