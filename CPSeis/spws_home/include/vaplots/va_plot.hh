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
//========== Base VA plot class that all images are derived from  ===========
//========== VaInform inherited from so that each plot can update ===========
//========== itself as needed.                                    ===========
//========== Michael L. Sherrill 09/97                            ===========
//===========================================================================

#ifndef VA_PLOT_H
#define VA_PLOT_H

#include <Xm/Xm.h>
#include "vf/vf_inform.hh"
#include "sp/seis_inform.hh"
#include "wproc.h"

// $Id: va_plot.hh,v 1.3 2004/07/16 12:59:57 wjdone Exp $
// $Name:  $



class VfManager;
class VfDataSet;
class SeisPlot;
class VaPlotCommonParams;
class VaPicks;
class SLShellContainer;
class SLErrorPop;


class VaPlot :  public VfInform, public SeisInform
{

public:
  VaPlot(VfManager     *vf_manager, 
         Widget        w, 
         char          *name);
  ~VaPlot();
  enum{ MAX_LOCATIONS = 100000};//need to work on this
  enum{ SEMBLANCE, GVS, CMP, ISO, VGRID, UNDERLAY, CROSSPLOT, ETA};
  enum { NUM_CONTOUR_COLORS = 20};
  enum { NUM_MISC_COLORS = 20};

  //--- Pure functions common to all derived VaPlot types
  virtual int  plot()                        = 0;
  virtual void setPlotParameters()           = 0;     
  virtual int  getFileParameters(char *file) = 0;
  virtual void updateGui(Boolean update_file_limits = True) = 0;     
  virtual SLShellContainer *getDialog()      = 0;

  virtual void errorPopup(char *error_msg);
  virtual void changePanel(float x = 0.0, float y = 0.0, float time=0.0,
                           float velocity=0.0);
  virtual Boolean ableToPlot(){return _able_to_plot;}
  virtual void setAbleToPlot(Boolean able_to_plot)
  {_able_to_plot = able_to_plot;}
  VfDataset *vfDataset();
  const float getXlocation(long i)     {return _xlocations[i];} 
  const float getYlocation(long i)     {return _ylocations[i];}
  long numberLocationsInFile()         {return _number_locations_in_file;}
  long tracesPerGroup()                {return _traces_per_group;}
  long firstTrace()                    {return _first_trace;}
  float minTime()                      {return _tmin;}
  float maxTime()                      {return _tmax;}
  float minVelocity()                  {return _velocity_min;}
  float maxVelocity()                  {return _velocity_max;}
  float firstXbin()                    {return _first_xbin;}
  float lastXbin()                     {return _last_xbin;}
  float firstYbin()                    {return _first_ybin;}
  float lastYbin()                     {return _first_ybin;}
  float minXbin()                      {return _min_xbin;}
  float maxXbin()                      {return _max_xbin;}
  float minYbin()                      {return _min_ybin;}
  float maxYbin()                      {return _max_ybin;}
  Boolean needsReplotting()            {return _needs_replotting;}
  void setNeedsReplotting(Boolean n)   {_needs_replotting = n;}
  virtual SeisPlot *SP()               {return _sp;}
  int getPlotType()                    {return _plot_type;}
  Boolean blankImage()                 {return _blank_image;}
  VaPlotCommonParams *getCommonParams(){return _common_params;}
  void setCommonParams(VaPlotCommonParams *p){_common_params = p;}
  void dataReadByXYheaders(float xhdr, float yhdr)
  {_data_xhdr = xhdr; _data_yhdr = yhdr;}
  void getDataReadByXYheaders(float *xhdr, float *yhdr)
  {*xhdr= _data_xhdr; *yhdr= _data_yhdr;}
  int getActiveXheader();
  int getActiveYheader();
  int getDisplayedPanelIndex();
  int getFilePanelIndexFromXY(float x, float y);
  int getDisplayedPanelIndexFromXY(float x, float y);
  float getDisplayedXbin();
  float getDisplayedYbin();
  int findArrayMatch(double xloc,       double xwidth,
                     double yloc,       double ywidth,
                     float *xarray,     float *yarray,
                     long num_elements, int array_stride,
                     Boolean            match_within_width);
  char *getFileLabel();
      
    /*! \name Snap Mode for Picking
        Interface for using the picking snap mode.
     */
    //@{
    //! Snap a pick to max data value within search window, changing
    //! the values of xWC and yWC. Currently applies only to semblance
    //! data.
    void snapPickToLocalMax(float x_snap_min, float x_snap_max,
                            float y_snap_min, float y_snap_max,
                            float &xWC, float &yWC);
    //@}

protected:
  //--- Data objects common to the derived VaPlot types
  VaPlotCommonParams        *_common_params;
  VfManager                 *_vf_manager;
  SeisPlot                  *_sp;
  VaPicks                   *_va_picks;
  Widget                    _top_widget;
  SLErrorPop                *_errorpop;
  Boolean                   _able_to_plot;
  int                       _plot_type;
  float                     *_xlocations;
  float                     *_ylocations;
  int                       _active_xheader;
  int                       _active_yheader;
  long                      _number_locations_in_file;
  long                      _traces_per_group;
  long                      _first_trace;
  float                     _tmin;
  float                     _tmax;
  float                     _velocity_min;
  float                     _velocity_max;
  float                     _first_xbin;
  float                     _first_ybin;
  float                     _last_xbin;
  float                     _last_ybin;
  float                     _min_xbin;
  float                     _max_xbin;
  float                     _min_ybin;
  float                     _max_ybin;
  Boolean                   _needs_replotting;
  float                     _data_xhdr;
  float                     _data_yhdr;
  Boolean                   _blank_image;
  char                      _file_label[256];
  // methods
  long                      numberOfFunctions();
  Boolean                   allocateLocationArrays(int asize);
    

private:
  // VfInform methods

};
#endif
