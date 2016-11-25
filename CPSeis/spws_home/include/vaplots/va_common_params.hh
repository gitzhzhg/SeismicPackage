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


//==========================================================================
//==================== Shared parameters used           ====================
//==================== between gvs, cmp and semblance   ====================
//==================== The VaplotcommonparamsGui interacts  ================
//==================== with this class to insure the    ====================
//==================== same is, tmin and tmax           ====================
//==================== and cmp/semblance movies         ====================
//==================== Michael L. Sherrill 09/97        ====================
//==========================================================================


#ifndef VA_COMMON_PARAMS_H
#define VA_COMMON_PARAMS_H

#include "vaplots/va_plot.hh"

class VaPlot;
class VaCommonParamsGui;
class VaCommonMovieGui;
class VaGvsGui;
class VaCmpGui;
class VaSemblanceGui;
class VaCrossplotGui;

   
class VaPlotCommonParams
{

  public:
       VaPlotCommonParams();
       ~VaPlotCommonParams();
       void    setFileTmin(int which_plot, float tmin);
       void    setFileTmax(int which_plot, float tmax);
       void    setTmin(float tmin){_tmin = tmin;}
       void    setTmax(float tmax){_tmax = tmax;}
       void    setIS  (float is){ _is = is;}
       float   getFileTminLimit();
       float   getFileTmaxLimit();
       float   getTmin(){return _tmin;}
       float   getTmax(){return _tmax;}
       float   getIS(){ return _is;}
       void    setSemblanceMinMaxVelocity(float vmin, float vmax)
                                          {_sem_vmin = vmin; _sem_vmax = vmax;}
       float   getSemblanceMinVelocity(){return _sem_vmin;}
       float   getSemblanceMaxVelocity(){return _sem_vmax;}
       void    setMovie(Boolean movie){_do_movie = movie;}
       void    setFileMaxMovies(int which_plot, long max_movies);
       void    setTotalMovies(long total_movies){_total_movies = total_movies;}
       void    setFirstMovie(long first_movie){_first_movie = first_movie;}
       void    setSkipMovies(long skip_movies){_skip_movies = skip_movies;}
       Boolean getMovie(){return _do_movie;}
       long    getFileMaxMovies();
       long    getFirstMovie(){return _first_movie;}
       long    getSkipMovies(){return _skip_movies;}
       long    getTotalMovies(){return _total_movies;}
       void    haveFileLimits(int which_plot, Boolean have);
       void    registerCommonGui(int which_plot, VaCommonParamsGui *gui);
       void    registerCommonMovieGui(int which_plot, VaCommonMovieGui *gui);
       void    registerGvsPlotGui(VaGvsGui *gvs_gui);
       void    registerCmpPlotGui(VaCmpGui *cmp_gui);
       void    registerSemblancePlotGui(VaSemblanceGui *sem_gui);
       void    registerCrossPlotGui(VaCrossplotGui *cross_plot_gui);
       void    synchGuis();
       void    synchMovieGuis();
       void    replotAllPlots(int which_plot);
       Boolean haveSemblanceFileLimits(){return _have_sem_limits;}
       Boolean haveCmpFileLimits()      {return _have_cmp_limits;}
       Boolean haveGvsFileLimits()      {return _have_gvs_limits;}
       Boolean gvsNeedsReplot()         {return _gvs_needs_replot;}
       Boolean cmpNeedsReplot()         {return _gvs_needs_replot;}
       Boolean semNeedsReplot()         {return _gvs_needs_replot;}
       Boolean crossplotNeedReplot()    {return _crossplot_needs_replot;}
       void    setGvsReplot(Boolean replot){_gvs_needs_replot = replot;}
       void    setCmpReplot(Boolean replot){_cmp_needs_replot = replot;}
       void    setSemReplot(Boolean replot){_sem_needs_replot = replot;}
       void    setCrossplotReplot(Boolean replot)
                                           {_crossplot_needs_replot = replot;}
       void    annotationChanged(int which_plot, float primary, 
                                 float secondary, Boolean depth);

   protected:
       VaCommonParamsGui *_sem_common_gui;
       VaCommonParamsGui *_gvs_common_gui;
       VaCommonParamsGui *_cmp_common_gui;
       VaCommonParamsGui *_crossplot_common_gui;
       VaCommonMovieGui  *_sem_movie_gui;
       VaCommonMovieGui  *_cmp_movie_gui;
       VaGvsGui          *_gvs_plot_gui;
       VaSemblanceGui    *_sem_plot_gui;
       VaCmpGui          *_cmp_plot_gui;
       VaCrossplotGui    *_crossplot_gui;
       Boolean           _have_gvs_limits;
       Boolean           _have_sem_limits;
       Boolean           _have_cmp_limits;
       float             _is;
       float             _tmin;
       float             _tmax;
       float             _file_tmin_limit;
       float             _file_tmax_limit;
       float             _gvs_file_tmin;
       float             _sem_file_tmin;
       float             _cmp_file_tmin;
       float             _gvs_file_tmax;
       float             _sem_file_tmax;
       float             _cmp_file_tmax;
       float             _sem_vmin;
       float             _sem_vmax;
       long              _cmp_total_file_movies;
       long              _sem_total_file_movies;
       Boolean           _do_movie;
       long              _first_movie;
       long              _skip_movies;
       long              _total_movies;
       Boolean           _gvs_needs_replot;
       Boolean           _cmp_needs_replot;
       Boolean           _sem_needs_replot;
       Boolean           _crossplot_needs_replot;
  private:

};

#endif
