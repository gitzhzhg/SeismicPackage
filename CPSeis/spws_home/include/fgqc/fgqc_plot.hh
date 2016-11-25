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
//********************************************************
//Author Michael L. Sherrill
//Class that creates window and plot for geometry aerial qc plots
//********************************************************

#ifndef FG_QC_PLOT_H
#define FG_QC_PLOT_H

#include "sl/sl_form_help_pop.hh"
#include "sl/sl_form_pop.hh"
#include "sl/sl_push_box.hh"
#include "fgseis_color_pop.hh"
#include "sp/seis_cbar_pop.hh"
#include "geom/fg_inform.hh"
#include "fgxp/ll_fgxp_plot.hh"
#include "vect/ll_tag.hh"
#include "vect/tag.hh"
#include "fgxp/fgxp_2d_plot.hh"
#include "sl/sl_push_box.hh"
#include "fgqc/fgqc_pop.hh"
#include "dp/hill_shader_pop.hh"
#include "fgmap/fg_loc_out.hh"
#include "fgmap/fg_seis_plot.hh"

#define MAX_NUM  99999999.0
#define MIN_NUM -99999999.0

enum     
  {
  AZIMUTHAL_DISTRIBUTION, DIFFERENCES,        ELEVATIONS,       
  FOLD_OF_STACK,          HOLE_DEPTHS,        NORMALIZED_BINNING, 
  CMP_DISTRIBUTION,       RECEIVER_SHIFTS,    SOURCE_SHIFTS,
  SOURCE_PROGRESSION,     STATICS,            UPHOLES,
  HEADER_WORD,            BIN_CENTERS,        SOURCE_ELEVATIONS
  };

enum 
  {
   MENU_OP, COLOR_OP, CBAR_OP, ZOOMUP, ZOOMDOWN, ZOOMORIGINAL, 
   ZOOMUPSEPARATE, HILL_SHADER, MAP_OVERLAY, MENU_POP, PICKING_MENU
  };

enum
  {
   NO_ACTION, MAKE_NEW_PLOT, EDIT_PLOT, NEW_ACTIVE_LINE
  };

enum {GRIDSYSTEM, SURVEYSYSTEM};

class FgQcPlot :  public SLFormHelpPop, public FgInform {

 friend class FgQcComputeGridPop;
 friend class FgQcComputeGrid;
 friend class FgQcOptionPop;

 private:
       class FgQcOptionPop    *_options_menu;
       SLPushBox              *_buttons;       
       SLPushBox              *_app_buttons;
       SLPushBox              *_popup;
       int                    _plot_type;
       int                    _inform_action;
       int                    _coordinate_system;
       float                  _pwidth;
       float                  _pheight;
       float                  _user_left;
       float                  _user_right;
       float                  _user_top;
       float                  _user_bottom;
       Boolean                _changing;
       Boolean                _first_time;
       Boolean                _reset_colors;
       Boolean                _been_managed;
       long                   _line_index;
       long                   _flag_index;
       long                   _numx;
       long                   _numy;
       long                   _num_points;
       Pixel                  _foreground_no_hilite_color;
       Pixel                  _background_no_hilite_color;
       Pixel                  _topshadow_no_hilite_color;
       Pixel                  _bottomshadow_no_hilite_color;
       Pixel                  _foreground_hilite_color;
       Pixel                  _background_hilite_color;
       FgSeisColorPop         *_color_pop;
       SeisCbarPop            *_cbar_pop;
       HillShaderPop          *_hill_shader_pop; 
       class FgQcPlotType     *_plot_class;
       class FgMapPick        *_fgmap_picker;
       FgXpPlotLinkedList     *_overlay_list;
       FgXp2DPlot             *_overlay;
       TagLinkedList          *_tagll;
       Tag                    *_tag;
       char                   _title[40];
       char                   _statics_filename[512];
       virtual void computeArraySize();
       virtual void getPreRegion (long ixl, long ident, long index, long nins);
       virtual void getPostRegion(long ixl, long ident, long index, long nins);
       virtual void makeOverlay();
       virtual void makeTag();

 protected:
       HelpCtx              _hctx;
       class FgSeisPlot     *_sp;
       class FgSeisPlotList *_fgsp_list;
       class FgMap          *_fgmap;
       class FgQcPickerMenu *_picking_menu;
       FgQcPop              *_pop;
       FgLocOut             *_loc;
       class FieldGeometry  *_fg;
       int                  _numcolors;
       void manage();
       static void postActiveLineClientMessageFunc(void *obj);
       static void postDestructorClientMessageFunc(void *obj);
       virtual void removeButton();  
       static void button_control( void *data, long which );
       Boolean notifyComplex(SLDelay *obj, int ident);
       //inform methods
       virtual void startingChanges(FieldGeometry *fg);
       virtual void finishedChanges(FieldGeometry *fg);
       virtual void preFlagValuesChanged(FieldGeometry *fg, long ixl,
	                int ident, long index, long nrem, long nins);
       virtual void postFlagValuesChanged(FieldGeometry *fg, long ixl,
			int ident, long index, long nrem, long nins);
       virtual void preRemoveInsertLines(FieldGeometry *fg,
			long index, long nrem, long nins);
       virtual void postRemoveInsertLines(FieldGeometry *fg,
			long index, long nrem, long nins);
       virtual void preNewActiveLine (FieldGeometry *fg);
       virtual void postNewActiveLine(FieldGeometry *fg);
       virtual void preNewActiveFlag (FieldGeometry *fg, long ixl);
       virtual void postNewActiveFlag(FieldGeometry *fg, long ixl);
       virtual void postNewGridTransform(FieldGeometry *fg);
       virtual void postNewTestingGridTransform(FieldGeometry *fg);
       virtual void sourceGathersOutOfDate(FieldGeometry *fg);
       virtual void receiverGathersOutOfDate(FieldGeometry *fg);
       virtual void midpointGathersOutOfDate(FieldGeometry *fg);
       virtual void liveFoldOutOfDate(FieldGeometry *fg);
       virtual void postUpdateSourceGathers(FieldGeometry *fg);
       virtual void postUpdateReceiverGathers(FieldGeometry *fg);
       virtual void postUpdateMidpointGathers(FieldGeometry *fg);

 public:
       FgQcPlot( Widget               p,
                char                 *name,
                HelpCtx              hctx,
                class FieldGeometry  *fg,
                class FgQcPop        *pop,
                class FgSeisPlotList *fgsp_list,
                int                  _numcolors);
       FgQcPlot( Widget               p,
                char                 *name,
                HelpCtx              hctx,
                class FieldGeometry  *fg,
                class FgQcPop        *pop,
                class FgSeisPlotList *fgsp_list,
                int                  _numcolors,
                class SLApp          *app);
       virtual ~FgQcPlot();
       class SLPullPop *_pulldown;
       virtual Widget make(Widget p);
       virtual void extraButton(int ident);
       virtual int plot(int plot_type);
       virtual int editPlot();
       virtual void setInActive();
       virtual void setActive();
       virtual void manageColorPop();
       virtual void unmanageColorPop();
       virtual void enableColorOptions();
       virtual void manageColorBar();
       virtual void redrawExtras();
       Boolean _grid_error;
       Widget gridError(){ _grid_error = True; return _pop->topWidget();}
       Widget plotWidget(){ return topWidget(); }
       HelpCtx hctx(){ return _hctx; }
       float _x_reach;
       float _y_reach;
       float *_image_data;
       float _minx;
       float _maxx;
       float _miny;
       float _maxy;
       float _minz;
       float _maxz;
       float _pre_minx;
       float _pre_maxx;
       float _pre_miny;
       float _pre_maxy;
       float _post_minx;
       float _post_maxx;
       float _post_miny;
       float _post_maxy;
       int   _num_attributes;
       float _offset_min;
       float _offset_max;
       float _azimuth_min;
       float _azimuth_max;
       long _limit_offsets_azimuths;
       long _limit_type;
       float  _percent;
       FgSeisPlot *sp(){return _sp;}        
       class FieldGeometry *fg(){return _fg;}  
       void  setMinx(float v){ _minx = v;}
       void  setMaxx(float v){ _maxx = v;}
       void  setMiny(float v){ _miny = v;}
       void  setMaxy(float v){ _maxy = v;}
       void  setMinz(float v){ _minz = v;}
       void  setMaxz(float v){ _maxz = v;}    
       void  setReach(float v1, float v2){_x_reach = v1; _y_reach = v2;}
       float getPlotWidth(){return _pwidth;}
       float getPlotHeight(){return _pheight;}
       float getUserLeft(){return _user_left;} 
       float getUserRight(){return _user_right;}
       float getUserTop(){return _user_top;}
       float getUserBottom(){return _user_bottom;}
       float getPostMinx(){return _post_minx;} 
       float getPostMaxx(){return _post_maxx;}
       float getPostMiny(){return _post_miny;}
       float getPostMaxy(){return _post_maxy;}
       long  getNumAttributes(){return _num_attributes;}
       long  getNumx(){return _numx;}
       long  getNumy(){return _numy;}
       int   getCoordinateSystem(){ return _pop->getCoordinateSystem();} 
       int   getPlottedCoordinateSystem(){ return _coordinate_system; }
       int   getNumColors(){ return _numcolors;}
       SeisColorPop *getColorPop() { return _color_pop;}
       FgSeisColorPop *getFgColorPop() { return _color_pop;}
       Boolean inApplicationWindow();
       class StaticsFile *getStaticsFile () { return _pop->getStaticsFile (); }
       void setDataLocation (long data_loc) {_pop->setDataLocation (data_loc);}
       long getDataLocation () {return _pop->getDataLocation ();}
       void setHeaderWord (int header_word)
         {_pop->setHeaderWord (header_word);}
       long getHeaderWord () {return _pop->getHeaderWord ();}
       void setLimitType (long limit_type) {_limit_type = limit_type;}
       virtual void setOffsets(float min, float max)
                              {_offset_min = min; _offset_max = max;}
       virtual void setAzimuths(float min, float max)
                              {_azimuth_min = min; _azimuth_max = max;}
       virtual void setPercent(float percent){_percent = percent;}
       char *getTitle() { return _title; }
       void setTitles();
       char *titleVersion (char *title, int version_num);
       virtual int makeIndirectPlot(int plot_type, float width, float height,
                                    float xmin, float ymin, float xmax,
                                    float ymax, int coord_system);
       void resetColors(Boolean r){_reset_colors = r;}
       Boolean getResetColors(){return _reset_colors;}
       virtual void setStaticsFilename(char *filename);

       void reduceColors (int reduction);
};

#endif
