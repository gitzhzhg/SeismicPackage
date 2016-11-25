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
#ifndef VA_PLOT_CONTROL_HH
#define VA_PLOT_CONTROL_HH

#include <Xm/Xm.h>
#include "sp/seis_inform.hh"
#include "vf/vf_inform.hh"

// $Id: va_plot_control.hh,v 1.2 2004/06/07 14:17:42 wjdone Exp $
// $Name:  $

class VaPlot;
class VaIsoPlot;
class VaCmpPlot;
class VaSemblancePlot;
class VaGvsPlot;
class VaGridPlot;
class VaCrossplotPlot;
class VaEtaPlot;
class VfManager;
class VaTraceFile;
class VfFileBase;
class FileBase;
class ViewWin;
class WinCtl;
class SLPullPop;
class VaVectColors;

class VaPlotControl : public SeisInform,  public VfInform {
  public:
     enum PlotType { GVS, CROSSPLOT, SEM, CMP, VEL, VGRID, ETA, ALLPLOTS};
     enum          { BOTTOMPLOTS = 4, NARROW_BORDER = 10, WIDE_BORDER = 91};
     enum PlotStat { VaSuccess, VaFail, VaPartialSuccess };
     ViewWin         *_view_win_array[ALLPLOTS];    
     SeisPlot        *_seisplot_array[ALLPLOTS];
     char            _need_to_replot_array[ALLPLOTS];
     
  private:

  protected:
     VfManager        *_manager;
     VaIsoPlot        *_iso_plot;
     VaGridPlot       *_grid_plot;
     VaCrossplotPlot  *_crossplot_plot;
     VaSemblancePlot  *_semblance_plot;
     VaGvsPlot        *_gvs_plot;
     VaCmpPlot        *_cmp_plot;
     VaEtaPlot        *_eta_plot;
     Boolean           _i_am_plotting;
     Boolean           _show_the_iso;

     WinCtl     *_winctl;
     SLPullPop  *_window_pull;
     ViewWin    *_iso_win;
     ViewWin    *_grid_win;
     ViewWin    *_crossplot_win;
     ViewWin    *_sem_win;
     ViewWin    *_gvs_win;
     ViewWin    *_cmp_win;

     VaTraceFile  *_sem_file; 
     VaTraceFile  *_cmp_file; 
     VaTraceFile  *_gvs_file; 
     VfFileBase *_vel_file;
     char       *_fail_string;
     Boolean     _replot_all;

     VaVectColors *_vect_colors;

     PlotStat plotIfNecessary(VaPlot   *plot, 
                              PlotType  which, 
                              FileBase *file_base);
 
     void setMasterScrollBar();
     void updateOptions();
     Boolean is3d();
     Boolean nothingShowing();

  public:

     VaPlotControl( VfManager       *manager, 
                    WinCtl          *winctl,
                    VaIsoPlot       *va_iso_plot,
                    VaGridPlot      *va_grid_plot,
                    VaSemblancePlot *va_semblance_plot,
                    VaGvsPlot       *va_gvs_plot,
                    VaCmpPlot       *va_cmp_plot,
                    VaCrossplotPlot *va_crossplot_plot,
                    VaEtaPlot       *va_eta_plot,
                    VaVectColors    *vect_colors );
     ~VaPlotControl();

     PlotStat plot(PlotType);
     PlotStat plotEverything();
     void ddOptions(SLPullPop *window_pull);
 
     VaIsoPlot        *iso();
     VaGridPlot       *grid();
     VaCrossplotPlot  *crossplot();
     VaSemblancePlot  *semblance();
     VaGvsPlot        *gvs();
     VaCmpPlot        *cmp();
     VaEtaPlot        *eta();

     VaTraceFile  *semFile();
     VaTraceFile  *cmpFile();
     VaTraceFile  *gvsFile();
     VfFileBase *velFile();
  
     VfManager *manager();
     void updateOthers(FileBase *set_from);
     void setIfEmpty(FileBase *set_from, 
                     char     *file_root);

     char *getFailString();
     void  setReplotAll(Boolean s =True);

     void addOptions(SLPullPop *window_pull);
     void calledFromWindow(int ident);
     Boolean isShowing(int ident);
     VaVectColors *vectorColors();
 
     virtual void newPlot(SeisPlot *sp);
     virtual void postTotalChanges(VfDataset *dataset);

     void showIsoWindow(Boolean show);

};

#endif
