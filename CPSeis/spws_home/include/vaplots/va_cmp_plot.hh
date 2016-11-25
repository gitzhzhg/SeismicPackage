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


//===========================================================================
//========== Cmp plot class derived from VaPlot object             ==========
//========== Michael L. Sherrill 09/97                             ==========
//===========================================================================

#ifndef VA_CMP_PLOT_H
#define VA_CMP_PLOT_H

#include "vaplots/va_plot.hh"

class VaCmpGui;
class VaVectColors;
class VaEtaPlot;


class VaCmpPlot :  public VaPlot
{



public:
  VaCmpPlot(VfManager          *vf_manager, 
            class VfHorizons   *horizons,
            Widget             w, 
            char               *name,
            HelpCtx            hctx,
            VaPlotCommonParams *common_params,
            VaVectColors       *vect_colors,
            long               numcolors);
  ~VaCmpPlot();
  void notCurrentInWindow(SeisPlot *); 
  void setPlotParameters();
  int  plot();
  int  getFileParameters(char *filename);
  void updateGui(Boolean update_file_limits = True); 
  void manageGui();
  void applyForwardMoveout();
  void applyReverseMoveout();
  void externalModifyForwardMoveout();
  void externalModifyReverseMoveout();

  virtual SLShellContainer *getDialog();
  void changePanel(float x,float y, float time=0.0, float velocity=0.0);
  Boolean nmcApplied();
  long getNumberPanelsInFile();
  const float getPanelXlocation(long i);
  const float getPanelYlocation(long i);
  long getMovieTotalPanels();
  long getMovieFirstPanel();
  long getMovieSkipPanels();
  void setFirstPanelToPlot(long first_panel);
  long    _numcolors;
  void setEtaManager(VfManager *eta_manager, VaEtaPlot *eta_plot){
                          _eta_manager = eta_manager; _eta_plot = eta_plot;}
  enum{FORWARD_NMC = 1,REVERSE_NMC = -1, REAPPLY_NMC = 0, MAX_PIXMAP = 1000};

protected:
  Boolean _nmc_applied[MAX_PIXMAP];
  float   _doppler;
  int     _nmc_mode;
  long    _modify_nmc_frame;
  class VfMoveout *_vf_moveout;
  void applyMoveout();
    
private:
  VaCmpGui        *_gui;
  void postNewActiveVelocityFunction (VfDataset *dataset);
  void preModifyPicks(VfDataset *vfd, long ifun,int type, long ipick, 
                      long nrem);
  void postModifyPicks(VfDataset *vfd,long ifun,int type, long ipick,
                       long nrem, long nins); 
  void  preChangeCoords(VfDataset *vfd, long ifun, long nchng);
  void postChangeCoords(VfDataset *vfd, long ifun, long nchng);
  void preRemoveInsertVelocityFunctions(VfDataset *vfd,long ifun,
                                        long nrem, long nins);
  void preChangeMoveoutOrder(VfDataset *vfd); 
  void preTotalChanges(VfDataset *vfd);
  void preNewActiveDataset();
  long modifyMoveout(int direction, long func_index, 
                     long frame, Boolean redraw);
  void postChangeBinTolerances();
  VfManager *_eta_manager;
  VaEtaPlot *_eta_plot;
};

#endif
