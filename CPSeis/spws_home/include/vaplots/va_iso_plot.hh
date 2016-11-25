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
//========== Iso Velocity plot class derived from VaPlot object  ============
//========== Michael L. Sherrill 09/97                           ============
//===========================================================================

#ifndef VA_ISO_PLOT_H
#define VA_ISO_PLOT_H

#include "vaplots/va_plot.hh"

class VaIsoGui;
class VaVectColors;
class VaIsoPlotter;

class VaIsoPlot :  public VaPlot
{

  friend class VaIsoPlotter;

  public:
    VaIsoPlot(VfManager        *vf_manager, 
              class VfHorizons *horizons,
              Widget           w, 
              char             *name,
              HelpCtx          hctx,
              VaVectColors     *vect_colors,
              long             numcolors);
    ~VaIsoPlot();
    class VaPlotControl *_plot_control;
    void showIsoWindow(Boolean show);
    void notCurrentInWindow(SeisPlot *); 
    void setPlotParameters();
    void setDontPlotYet(Boolean dont_plot_yet);
    int  plot();
    Boolean changePlot(float x1, float x2, float y1, float y2, 
                       Boolean refresh_only = False);
    int  getFileParameters(char *filename);
    void updateGui(Boolean update_file_limits = True); 
    void manageGui();
    void registerPlotControl(class VaPlotControl *pc) {_plot_control = pc;}
    class VaPlotControl *plotControl(){return _plot_control;}
    virtual SLShellContainer *getDialog();
    VfManager *vfManager(){return _vf_manager;}
    int getLineType();
    int getPrePlottedLineType();
    int getPlottedLineType();
    int getPlottedVelocityType();
    enum {INLINE, CROSSLINE, TIMESLICE};
    char _filename[200];
    char *getFilename(){return _filename;}
    float getTimeOfFrame(long frame_index);

  protected:
    long           _numcolors;
    void initialize();    

  private:
    VaIsoPlotter    *_plotter;
    VaIsoGui        *_gui;
    int             _plotted_line_type;
    void            postNewActiveDataset();

};

#endif
