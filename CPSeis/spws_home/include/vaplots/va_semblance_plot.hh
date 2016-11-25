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
//========== Semblance plot class derived from VaPlot object     ============
//========== Michael L. Sherrill 09/97                           ============
//===========================================================================

#ifndef VA_SEMBLANCE_PLOT_H
#define VA_SEMBLANCE_PLOT_H

// $Id: va_semblance_plot.hh,v 1.5 2004/11/03 19:17:58 wjdone Exp $
// $Name:  $

#include "vaplots/va_plot.hh"

class VaVectColors;
class VaSemblanceGui;

class VaSemblancePlot :  public VaPlot
{



  public:
    VaSemblancePlot(VfManager          *vf_manager, 
                    class VfHorizons   *horizons,
                    Widget             w, 
                    char               *name,
                    HelpCtx            hctx,
                    VaPlotCommonParams *common_params,
                    VaVectColors       *vect_colors,
                    long               numcolors,
                    long               numcontourcolors);
    ~VaSemblancePlot();
    void notCurrentInWindow(SeisPlot *); 
    void setPlotParameters();
    int  plot();
    int  getFileParameters(char *filename);
    void updateGui(Boolean update_file_limits = True); 
    void manageGui();
    virtual SLShellContainer *getDialog();
    void changePanel(float x,float y, float time=0.0,float velocity=0.0);
    long getNumberPanelsInFile();
    const float getPanelXlocation(long i);
    const float getPanelYlocation(long i);
    long getMovieTotalPanels();
    long getMovieFirstPanel();
    long getMovieSkipPanels();
    void setFirstPanelToPlot(long first_panel);
    long _numcolors;
    long _numcontourcolors;
    VfManager *getVfManager(){return _vf_manager;}
    void registerEtaPlot(class VaEtaPlot *eta);
    void showEtaOverlays();
    void hideEtaOverlays();
    
    /*! \name Semblance Pick Copying
        Interface for copying semblance picks from adjacent velocity
        function locations.
     */
    //@{
    //! Copy pick array data from previous panel function.
    void copyPrevPanelVelocityFunc(long prev_func, long new_func);
    //! Copy pick array data from source function.
    void copySourceVelocityFunc(long option);
    //! Get index to source velocity function.
    long getSourceVelocityFuncIndex(long option);
    //! Get index to comparison velocity function and its dataset.
    long getComparisonVelocityFuncIndex(long option, VfDataset* &compDset);
    //@}
    
    /*! \name Snap Mode for Semblance Picking
        Interface for using the semblance picking snap mode.
     */
    //@{
    //! Activate the snap mode for semblance picking.
    void activateSnapMode(float vel_halfwin, float time_halfwin,
                          float semb_t_min, float semb_t_max,
                          float semb_v_min, float semb_v_max);
    //! Deactivate the snap mode for semblance picking.
    void deactivateSnapMode();
    //@}

    enum {
        SEMB_COPY_PREV_INLINE, SEMB_COPY_NEXT_INLINE,
        SEMB_COPY_PREV_XLINE,  SEMB_COPY_NEXT_XLINE,
        SEMB_COPY_REFERENCE,   SEMB_COPY_PREV_ACTIVE,
        SEMB_COPY_COMPARISON
    };

  protected:
    

  private:
    VaSemblanceGui        *_gui;
    void postNewActiveVelocityFunction (VfDataset *dataset);
    void postChangeCoords(VfDataset *vfd, long ifun, long nchng);
    void postChangeBinTolerances();
    
    /*! \name Semblance Pick Copying
        Interface for copying semblance picks from adjacent velocity
        function locations.
     */
    //@{
    //! Copy picks from function associated with previous panel.
    void copyPicksFromPrevPanelFunc(VfDataset &vfds, long prev_func,
                                    long new_func);
    //! Copy picks from function determined as source.
    void copyPicksFromSourceFunc(VfDataset &vfds, long nearfun, long actfun);
    //! Copy picks from comparison function determined as source.
    void copyPicksFromComparisonFunc(VfDataset &compvfds, long nearfun,
                                     VfDataset &vfds, long actfun);
    //@}
};

#endif
