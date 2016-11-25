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
//========== Gvs plot class derived from VaPlot object           ============
//========== Michael L. Sherrill 09/97                           ============
//===========================================================================

#ifndef VA_GVS_PLOT_H
#define VA_GVS_PLOT_H

#include "vaplots/va_plot.hh"

class VaGvsGui;
class VaVectColors;
class History;
class SeisPlotUnder;
class VaIsoPlotter;
class VaGvsGui;
class VfDataset;

class VaGvsPlot :  public VaPlot
{

  friend class VaIsoPlotter;

  public:
    VaGvsPlot(VfManager          *vf_manager, 
              class VfHorizons   *horizons,
              Widget             w, 
              char               *name,
              HelpCtx            hctx,
              VaPlotCommonParams *common_params,
              VaVectColors       *vect_colors,
              long               numcolors);
    ~VaGvsPlot();
    void notCurrentInWindow(SeisPlot *);  
    void setPlotParameters();
    int  plot();
    int  getFileParameters(char *filename);
    void updateGui(Boolean update_file_limits = True); 
    void manageGui();
    virtual SLShellContainer *getDialog();
    enum {ADDITIVE, MULTIPLICATIVE, VELHEADER = 62};
    void centerPanel(float x, float y);
    void changePanel(float x, float y, float time, float velocity);
    SeisPlotUnder *underSP(){return _sp_under;}
    float getVelocityFromPanel(long panel_index, float time, long trace_num);
    int   populateReferenceFileVelocity(float x, float y);
    int   populateGvsFileVelocity(float x, float y);
    void  setGvsReferenceFileOption(int set);
    long  _numcolors;

  protected:
    float    *_ref_times;
    float    *_ref_velocities;
    float    *_trace_velocities;
    float    _trace_velocities_xloc;
    float    _trace_velocities_yloc;
    int      _num_ref_picks;
    Boolean  _is_gvs;
    int      _reference_file_option;
    int      _vmod_type;
    History  *_history;
    void initializeGvsVelocities();   

  private:
    VaIsoPlotter  *_plotter;
    VaGvsGui      *_gui;
    SeisPlotUnder *_sp_under;
    VfDataset     *_gvs_dataset;
    void determineDataType();
    void postModifyPicks(VfDataset *vfd, long ifun, 
                         int type,       long ipick, 
                         long nrem,      long nins);
    void postNewActiveVelocityFunction(VfDataset *vfd);
};

#endif
