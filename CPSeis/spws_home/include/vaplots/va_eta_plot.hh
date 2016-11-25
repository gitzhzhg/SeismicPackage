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
//========== Eta plot class derived from VaPlot object     ============
//========== Michael L. Sherrill 09/97                           ============
//===========================================================================

#ifndef VA_ETA_PLOT_H
#define VA_ETA_PLOT_H

#include "vaplots/va_plot.hh"
#include "vaplots/va_bottom_control.hh"
#include "sl/sl_form_help_pop.hh"
#include "oprim/base_data.hh"

class VaVectColors;
class VaEtaGui;

class VaEtaPlot :  public VaPlot, SLFormHelpPop
{



public:
  VaEtaPlot(      VfManager          *vf_manager, 
                  class VfHorizons   *horizons,
                  Widget             w, 
                  char               *name,
                  HelpCtx            hctx,
                  VaPlotCommonParams *common_params,
                  VaVectColors       *vect_colors,
                  int                numcolors,
                  VaPlot             *semblance_plot,
                  VaPlot             *cmp_plot);
  ~VaEtaPlot();
  void notCurrentInWindow(SeisPlot *); 
  void newPlot(SeisPlot * );
  void setPlotParameters();
  int  plot();
  int  getFileParameters(char *filename);
  void updateGui(Boolean update_file_limits = True); 
  Widget make(Widget w);
  void manage();
  void manageGui();
  virtual SLShellContainer *getDialog();
  void changePanel(float x,float y, float time=0.0,float velocity=0.0);
  const float getPanelXlocation(long i);
  const float getPanelYlocation(long i);
  long _numcolors;
  long _numcontourcolors;

  //temporary manager until va's main manager supports eta picks
  VfManager *getTempVfManager(){return _temp_manager;}
  VfManager *getVfManager(){return _vf_manager;}


  Boolean managed() {return _gui_has_been_managed;}
  void setActive(Boolean b);
  void receivePick(int action, float x, float y, float func_x, float func_y,
                   long index);
  enum{INSERT_PICK, DELETE_PICK};
  float getMinAmp(){return _amp_min;}
  float getMaxAmp(){return _amp_max;}
  SeisPlot *SP()   {return _esp;}
  float getVelocityPick() {return _velocity_pick;}
  float getPickXval() {return _pick_xval;}
  float getPickYval() {return _pick_yval;}
  float getDeleteXval() {return _delete_xval;}
  float getDeleteYval() {return _delete_yval;}
  float getLargestVnmo() {return _largest_vnmo;}
  float getLargestHnmo() {return _largest_hnmo;}
  Boolean isActivated() {return _activated;}
  void  acceptAutoPick();
  void registerBottomControl(VaBottomControl *bc){_bottom_control = bc;}

  //Temporary kludge
  void pickingCompleted(float *new_velocity = NULL);
  void clearNmo();
  void removeNmo();
  void applyNmo();

  enum{BOTH_X_AND_Y, X_ONLY, Y_ONLY};
  enum{CHECK_CONTROL, CHECK_AUTO, CHECK_GRID, CHECK_SHADER, CHECK_FGA};



protected:

  void                    extraButton(int ident);
  float                   *_eta_data;    
  float                   _velocity_pick;
  float                   _pick_xval;
  float                   _pick_yval;
  float                   _delete_xval;
  float                   _delete_yval;
  float                   _largest_vnmo;
  float                   _largest_hnmo;
  float                   _yWC;
  long                    _index;

private:
  //temporary manager until va's main manager supports eta picks
  VfManager               *_temp_manager;

  class VfHorizons         *_horizons;
  VaVectColors             *_vect_colors;
  void                     cleanUp();
  VaEtaGui                 *_gui;
  Boolean                  _gui_has_been_managed;
  Boolean                  _activated;
  Boolean                  _been_managed;
  VaPlot                   *_semblance_plot;
  VaPlot                   *_cmp_plot; 
  Boolean                  processEta(float x, float y);
  class SeisLocOut         *_loc;
  float                    _amp_min;
  float                    _amp_max;
  float                    *_amps_out;
  float                    _not_defined;
  class FloatGrid          *_float_grid;
  class FloatGridAccessor  *_float_grid_accessor;
  class ControlPoints      *_control_points;
  class AutoGridderFloat   *_auto_gridder_float;
  SeisPlot                 *_esp;
  VaBottomControl          *_bottom_control;

  void markNmo();
};

#endif
