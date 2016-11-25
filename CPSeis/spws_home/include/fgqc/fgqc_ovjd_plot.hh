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
//***************************************************************
//Author Michael L. Sherrill
//Class that creates window and plot for geometry ovjd qc plots
//***************************************************************

#ifndef FG_QC_OVJD_PLOT_H
#define FG_QC_OVJD_PLOT_H

#include "sl/sl_form_help_pop.hh"
#include "sl/sl_form_pop.hh"
#include "sl/sl_push_box.hh"
#include "sp/seis_cbar_pop.hh"
#include "geom/fg_inform.hh"
#include "sl/sl_push_box.hh"
#include "sl/sl_pull_pop.hh"
#include "sp/seis_loc_out.hh"
#include "fgmap/fg_seis_plot.hh"
#include "fgseis_color_pop.hh"
#include "sp/seis_select_pop.hh"
#include "sp/seis_control.hh"

enum
  {
   NO_ACTION, MAKE_NEW_PLOT, EDIT_PLOT, NEW_ACTIVE_LINE
  };


class FgQcOvjdPlot :  public SLFormHelpPop, public FgInform {

 private:
       SLFPopSep              *_options_menu;
       SLPushBox              *_buttons;       
       SLPushBox              *_popup;
       class SLApp            *_app;
       class HeaderDumpPop    *_header_dump_pop;
       class FgSeisShift      *_seis_shift;
       class SeisShiftPop     *_shift_pop;
       int                    _inform_action;
       Boolean                _changing;
       Boolean                _first_time;
       long                   _line_index;
       long                   _flag_index;
       FgSeisColorPop         *_color_pop;
       SeisCbarPop            *_cbar_pop;
       class SeisOvjd         *_ovjd;
       class FgSeisOvjdPop    *_ovjd_pop;
       
 protected:
       HelpCtx              _hctx;
       class FgSeisPlot     *_sp;
       class FgSeisPlotList *_fgsp_list;
       SeisLocOut           *_loc;
       class FieldGeometry  *_fg;
       SeisSelectPop        *_pop;
       SeisControl          *_control_area;
       int                  _numcolors;
       SLPullPop            *_pulldown;
       virtual void removeButton();  
       Boolean notifyComplex(SLDelay *obj, int ident);
       static void button_control( void *data, long which );
       
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
       virtual void prePpValuesChanged(FieldGeometry *fg, int ident, 
                                       long index, long nrem,long nins);
       virtual void postPpValuesChanged(FieldGeometry *fg, int ident, 
                                       long index, long nrem,long nins);
       virtual void preRemoveInsertPpCards(FieldGeometry *fg, long index, 
                                           long nrem,long nins);
       virtual void postRemoveInsertPpCards(FieldGeometry *fg, long index, 
                                           long nrem,long nins);
       virtual void preRpValuesChanged(FieldGeometry *fg, int ident, 
                                       long index, long nrem,long nins);
       virtual void postRpValuesChanged(FieldGeometry *fg, int ident, 
                                       long index, long nrem,long nins);
       virtual void preRemoveInsertRpCards(FieldGeometry *fg, long index, 
                                           long nrem,long nins);
       virtual void postRemoveInsertRpCards(FieldGeometry *fg, long index, 
                                           long nrem,long nins);
       friend class FgQcOvjdSeisSelectPop;  

 public:
       FgQcOvjdPlot( Widget               p,
                     char                 *name,
                     HelpCtx              hctx,
                     class FieldGeometry  *fg,
                     class FgSeisPlotList *fgsp_list,
                     int                  _numcolors);
       FgQcOvjdPlot( Widget               p,
                     char                 *name,
                     HelpCtx              hctx,
                     class SLApp          *app,
                     class FieldGeometry  *fg,
                     class FgSeisPlotList *fgsp_list,
                     int                  _numcolors);
       virtual ~FgQcOvjdPlot();
       virtual Widget make(Widget p);
       virtual void extraButton(int ident);
       virtual int preparePlot();
       virtual int editPlot();
       virtual void manageColorPop();
       virtual void manageColorBar();
       SeisSelectPop *getOvjdPop(){return _pop;}
       FgSeisPlot *sp(){return _sp;}        
       class FieldGeometry *fg(){return _fg;}  
       int   getNumColors(){ return _numcolors;}
       SeisColorPop *getColorPop() { return _color_pop;}
       SLPullPop *pulldown(){return _pulldown;}
       Boolean inApplicationWindow();
};




class FgQcOvjdSeisSelectPop :  public SeisSelectPop {

 protected:
        virtual void    DoAction();
        FgQcOvjdPlot    *_fop;

 public:
        FgQcOvjdSeisSelectPop(Widget    p,
                              char      *name,
                              HelpCtx   hctx,
                              FgQcOvjdPlot *fop,
                              SeisPlot  *sp,
                              SeisPlot  *info_sp =NULL) :
                              SeisSelectPop(p,name,hctx,sp,info_sp),_fop(fop){};
        virtual ~FgQcOvjdSeisSelectPop(){delete _fop;}        
        

};
#endif


