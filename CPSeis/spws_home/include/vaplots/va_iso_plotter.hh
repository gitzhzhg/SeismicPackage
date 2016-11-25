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
//========== Iso Velocity plotter class. This class handles the  ============
//========== plotting control of the iso and gvs underlay        ============
//========== Michael L. Sherrill 09/97                           ============
//===========================================================================

#ifndef VA_ISO_PLOTTER_H
#define VA_ISO_PLOTTER_H

#include "vf/vf_inform.hh"
#include "vaplots/va_iso_plot.hh"
#include "vaplots/va_gvs_plot.hh"

class VfManager;
class VaIsoPlot;
class VaIsoGui;
class VaGvsPlot;
class VaGvsGui;
class SeisPlot;
class SeisPlotUnder;

class VaIsoPlotter :  public VfInform
{


  public:
    VaIsoPlotter(VfManager *vf_manager,
                 VaIsoPlot *iso_plot,
                 VaGvsPlot *gvs_plot);
    ~VaIsoPlotter();
    int   plot(SeisPlot *sp);
    int   plotUnderlay(SeisPlotUnder *sp_under, Boolean prepare_data = True);
    Boolean changePlot(float x1, float x2, float y1, float y2,
                       Boolean refresh_only = False);
    int   getFileParameters(char *filename);
    void  changePanel(float x, float y, float time, float velocity, 
                      SeisPlot *sp);
    long  modifyPanel(float x, float y, float time, float velocity,
                      long ifun, SeisPlot *sp);
    int   prepareData(SeisPlot *sp);
    void  setColorOptions(int set_which_plot, Boolean turn_on);
    float getTmin()     {return _tmin;}
    float getTmax()     {return _tmax;}
    float getVelMin()   {return _velocity_min;}
    float getVelMax()   {return _velocity_max;}
    float getMinXbin()  {return _min_xbin;}
    float getMaxXbin()  {return _max_xbin;}
    float getMinYbin()  {return _min_ybin;}
    float getMaxYbin()  {return _max_ybin;} 
    float getFirstXbin(){return _first_xbin;}
    float getLastXbin() {return  _last_xbin;}
    float getFirstYbin(){return _first_ybin;}
    float getLastYbin() {return _last_ybin;}
    long  getNumberLocationsInFile(){return _number_locations_in_file;}


  protected:
    float           *_xlocations;
    float           *_ylocations;
    long            _number_locations_in_file;
    float           _tmin;
    float           _tmax;
    float           _velocity_min;
    float           _velocity_max;
    float           _min_xbin;
    float           _max_xbin;
    float           _min_ybin;
    float           _max_ybin; 
    float           _first_xbin;
    float           _last_xbin;
    float           _first_ybin;
    float           _last_ybin; 
    void            drawLabel(SeisPlot *sp);

  private:
    VfManager       *_vf_manager;
    VaIsoPlot       *_iso_plot;
    VaIsoGui        *_iso_gui;
    VaGvsPlot       *_gvs_plot;
    VaGvsGui        *_gvs_gui;
    float           *_headers;
    float           *_image_array;
    void postNewActiveVelocityFunction(VfDataset *vfd);
    void postTotalChanges(VfDataset *dataset);
    void postModifyPicks(VfDataset *vfd, long ifun, 
                         int type,       long ipick, 
                         long nrem,      long nins);
    void postRemoveInsertVelocityFunctions
                        (VfDataset *dataset, long ifun, long nrem, long nins);
};

#endif
