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
//========== Grid plot class derived from VaPlot object     ============
//========== Michael L. Sherrill 09/97                           ============
//===========================================================================

#ifndef VA_GRID_PLOT_H
#define VA_GRID_PLOT_H

#include "vaplots/va_plot.hh"

class VaGridGui;
class VaVectColors;

class VaGridPlot :  public VaPlot
{



  public:
    VaGridPlot(VfManager          *vf_manager, 
               class VfHorizons   *horizons,
               Widget             w, 
               char               *name,
               HelpCtx            hctx,
               VaVectColors       *vect_colors);
    ~VaGridPlot();
    void notCurrentInWindow(SeisPlot *); 
    void setPlotParameters();
    int  plot();
    int  getFileParameters(char *filename);
    void updateGui(Boolean update_file_limits = True); 
    void manageGui();
    virtual SLShellContainer *getDialog();

  protected:


  private:
    VaGridGui   *_gui;
    void        postNewActiveDataset();
    void        postChangeCoords(VfDataset *dataset, long ifun, long nchng);
    void        postTotalChanges(VfDataset *dataset);
    void        postRemoveInsertVelocityFunctions(VfDataset *dataset,
                                                  long ifun, long nrem,
                                                  long nins);
};

#endif
