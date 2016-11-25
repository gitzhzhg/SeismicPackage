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


//=========================================================================
//========== Shared parameters used by the gvs, cmp              ==========
//========== and semblance plots                                 ==========
//========== Author Michael L. Sherrill 09/97                    ==========
//=========================================================================

// $Id: va_common_params.cc,v 1.2 2004/04/05 18:12:11 wjdone Exp $
// $Name:  $

#include "vaplots/va_common_params.hh"
#include "vaplots/va_common_params_gui.hh"
#include "vaplots/va_common_movie_gui.hh"
#include "vaplots/va_gvs_gui.hh"
#include "vaplots/va_cmp_gui.hh"
#include "vaplots/va_semblance_gui.hh"
#include "vaplots/va_crossplot_gui.hh"
#include "plot_image.hh"

//=========================================================================
//====================== Constructor ======================================
//=========================================================================
VaPlotCommonParams::VaPlotCommonParams()
{
  _have_gvs_limits       = False;
  _have_sem_limits       = False;
  _have_cmp_limits       = False;
  _is                    = 2.0;
  _tmin                  = -999.0;
  _tmax                  = 999.0;
  _sem_vmin              = 5000.0;
  _sem_vmax              = 20000.0;
  _sem_total_file_movies = 0;
  _cmp_total_file_movies = 0;
  _gvs_needs_replot      = False;
  _cmp_needs_replot      = False;
  _sem_needs_replot      = False;
  _do_movie              = False;
  _first_movie           = 1;
  _skip_movies           = 0;
  _total_movies          = 1;
}


//=========================================================================
//===================== Destructor ========================================
//=========================================================================
VaPlotCommonParams::~VaPlotCommonParams()
{
}


//=========================================================================
//====================  Register tmin, tmax etc gui  ======================
//=========================================================================
void VaPlotCommonParams::registerCommonGui(int which_plot, 
                                           VaCommonParamsGui *gui)
{
  switch(which_plot)
    {
      case VaPlot::GVS:
        _gvs_common_gui = gui;
      break;

      case VaPlot::SEMBLANCE:
        _sem_common_gui = gui;
      break;

      case VaPlot::CMP:
        _cmp_common_gui = gui;
      break;

      case VaPlot::CROSSPLOT:
        _crossplot_common_gui = gui;
      break;

    }
}

//=========================================================================
//====================  Register Gvs Plot Gui        ======================
//=========================================================================
void VaPlotCommonParams::registerGvsPlotGui(VaGvsGui *gvs_plot_gui)
{
  _gvs_plot_gui = gvs_plot_gui;
}

//=========================================================================
//====================  Register Cmp Plot Gui        ======================
//=========================================================================
void VaPlotCommonParams::registerCmpPlotGui(VaCmpGui *cmp_plot_gui)
{
  _cmp_plot_gui = cmp_plot_gui;
}

//=========================================================================
//====================  Register Semblance Plot Gui  ======================
//=========================================================================
void VaPlotCommonParams::registerSemblancePlotGui(VaSemblanceGui *sem_plot_gui)
{
  _sem_plot_gui = sem_plot_gui;
}


//=========================================================================
//====================  Register Semblance Plot Gui  ======================
//=========================================================================
void VaPlotCommonParams::registerCrossPlotGui(VaCrossplotGui *crossplot_gui)
{
  _crossplot_gui = crossplot_gui;
}

//=========================================================================
//====================  Register plot           ===========================
//=========================================================================
void VaPlotCommonParams::registerCommonMovieGui(int which_plot, 
                                                VaCommonMovieGui *gui)
{
  switch(which_plot)
    {
      case VaPlot::SEMBLANCE:
        _sem_movie_gui = gui;
      break;

      case VaPlot::CMP:
        _cmp_movie_gui = gui;
      break;

    }
}


//=========================================================================
//===================== Set Tmin   ========================================
//=========================================================================
void VaPlotCommonParams::setFileTmin(int which_plot, float tmin)
{

  switch(which_plot)
    {
      case VaPlot::GVS:
        _gvs_file_tmin = tmin;
      break;

      case VaPlot::SEMBLANCE:
        _sem_file_tmin = tmin;
      break;

      case VaPlot::CMP:
        _cmp_file_tmin = tmin;
      break;
 
    }

}


//=========================================================================
//===================== Set Tmax   ========================================
//=========================================================================
void VaPlotCommonParams::setFileTmax(int which_plot, float tmax)
{

  switch(which_plot)
    {
      case VaPlot::GVS:
        _gvs_file_tmax = tmax;
      break;

      case VaPlot::SEMBLANCE:
        _sem_file_tmax = tmax;
      break;

      case VaPlot::CMP:
        _cmp_file_tmax = tmax;
      break;

    }

}



//=========================================================================
//===================== Set Max file movies ===============================
//=========================================================================
void VaPlotCommonParams::setFileMaxMovies(int which_plot, long max_movies)
{

  switch(which_plot)
    {

      case VaPlot::SEMBLANCE:
        _sem_total_file_movies = max_movies;
      break;

      case VaPlot::CMP:
        _cmp_total_file_movies = max_movies;
      break;

    }

}


//=========================================================================
//===================== Get Max file movies ===============================
//=========================================================================
long VaPlotCommonParams::getFileMaxMovies()
{

  if(_have_sem_limits && _have_cmp_limits)
    return min(_sem_total_file_movies,_cmp_total_file_movies);
  else if(_have_sem_limits)
    return _sem_total_file_movies;
  else if(_have_cmp_limits)
    return _cmp_total_file_movies;
  else
    return 0;  

}

//=========================================================================
//===================== Set having file limits   ==========================
//=========================================================================
void VaPlotCommonParams::haveFileLimits(int which_plot, Boolean have)
{

  switch(which_plot)
    {
      case VaPlot::GVS:
        _have_gvs_limits = have;
      break;

      case VaPlot::SEMBLANCE:
        _have_sem_limits = have;
      break;

      case VaPlot::CMP:
        _have_cmp_limits = have;
      break;

    }

}

//=========================================================================
//===================== Get tmin limit           ==========================
//=========================================================================
float VaPlotCommonParams::getFileTminLimit()
{
float limit;

  if(!_have_gvs_limits) _gvs_file_tmin = -99999.0;
  if(!_have_sem_limits) _sem_file_tmin = -99999.0;
  if(!_have_cmp_limits) _cmp_file_tmin = -99999.0;

  limit = max(_gvs_file_tmin, _sem_file_tmin);
  limit = max(limit,          _cmp_file_tmin);

  _file_tmin_limit = limit;

  return _file_tmin_limit;
}

//=========================================================================
//===================== Get tmin limit           ==========================
//=========================================================================
float VaPlotCommonParams::getFileTmaxLimit()
{
float limit;

  if(!_have_gvs_limits) _gvs_file_tmax = 99999.0;
  if(!_have_sem_limits) _sem_file_tmax = 99999.0;
  if(!_have_cmp_limits) _cmp_file_tmax = 99999.0;

  limit = min(_gvs_file_tmax, _sem_file_tmax);
  limit = min(limit,          _cmp_file_tmax);

  _file_tmax_limit = limit;

  return _file_tmax_limit;
}


//=========================================================================
//===================== Update all guis with new common params  ===========
//=========================================================================
void VaPlotCommonParams::synchGuis()
{
  _sem_common_gui->synch();
  _gvs_common_gui->synch();
  _cmp_common_gui->synch();
  _crossplot_common_gui->synch();
}

//=========================================================================
//===================== Update movie guis with new movie params   =========
//=========================================================================
void VaPlotCommonParams::synchMovieGuis()
{
  _sem_movie_gui->synch();
  _cmp_movie_gui->synch();
}

//=========================================================================
//===================== Plot all plots due to new common params  ==========
//=========================================================================
void VaPlotCommonParams::replotAllPlots(int which_plot)
{
  switch(which_plot)
    {
      case VaPlot::GVS:
        if(_sem_needs_replot)
          {
          _sem_plot_gui->commonReplot();
          _sem_needs_replot = False;
          }
        if(_cmp_needs_replot)
          {
          _cmp_plot_gui->commonReplot();
          _cmp_needs_replot = False;
          }
        if(_crossplot_needs_replot)
          {
          _crossplot_gui->commonReplot();
          _crossplot_needs_replot = False;
          }
      break;

      case VaPlot::SEMBLANCE:
        if(_gvs_needs_replot)
          {
          _gvs_plot_gui->commonReplot();
          _gvs_needs_replot = False;
          }
        if(_cmp_needs_replot)
          {
          _cmp_plot_gui->commonReplot();
          _cmp_needs_replot = False;
          }
        if(_crossplot_needs_replot)
          {
          _crossplot_gui->commonReplot();
          _crossplot_needs_replot = False;
          }
      break;

      case VaPlot::CMP:
        if(_gvs_needs_replot)
          {
          _gvs_plot_gui->commonReplot();
          _gvs_needs_replot = False;
          }
        if(_sem_needs_replot)
          {
          _sem_plot_gui->commonReplot();
          _sem_needs_replot = False;
          }
        if(_crossplot_needs_replot)
          {
          _crossplot_gui->commonReplot();
          _crossplot_needs_replot = False;
          }
      break;

      case VaPlot::CROSSPLOT:
        if(_gvs_needs_replot)
          {
          _gvs_plot_gui->commonReplot();
          _gvs_needs_replot = False;
          }
        if(_sem_needs_replot)
          {
          _sem_plot_gui->commonReplot();
          _sem_needs_replot = False;
          }
        if(_cmp_needs_replot)
          {
          _cmp_plot_gui->commonReplot();
          _cmp_needs_replot = False;
          }
      break;

    }




}


void VaPlotCommonParams::annotationChanged(int which_plot, float primary, 
                                           float secondary, Boolean depth)
{
  switch(which_plot)
    {
      case VaPlot::GVS :
        _sem_plot_gui->annotationChanged(primary, secondary, depth);
        _crossplot_gui->annotationChanged(primary, secondary, depth);
        _cmp_plot_gui->annotationChanged(primary, secondary, depth);
        break;

      case VaPlot::CMP :
        _sem_plot_gui->annotationChanged(primary, secondary, depth);
        _crossplot_gui->annotationChanged(primary, secondary, depth);
        _gvs_plot_gui->annotationChanged(primary, secondary, depth);
        break;

      case VaPlot::CROSSPLOT :
        _sem_plot_gui->annotationChanged(primary, secondary, depth);
        _gvs_plot_gui->annotationChanged(primary, secondary, depth);
        _cmp_plot_gui->annotationChanged(primary, secondary, depth);
        break;

      case VaPlot::SEMBLANCE :
        _gvs_plot_gui->annotationChanged(primary, secondary, depth);
        _crossplot_gui->annotationChanged(primary, secondary, depth);
        _cmp_plot_gui->annotationChanged(primary, secondary, depth);
        break;
 
      default :
        assert(0);
        break;
    }
}
