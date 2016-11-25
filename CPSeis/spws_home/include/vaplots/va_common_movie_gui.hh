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
//==================== Shared movie parameters used     ====================
//==================== between cmp and semblance        ====================
//==================== Michael L. Sherrill 09/97        ====================
//==========================================================================


#ifndef VA_COMMON_MOVIE_GUI_H
#define VA_COMMON_MOVIE_GUI_H

#include "sl/sl_form.hh"
#include "sl/sl_delay.hh"


class SLTextBox;
class SLTogBox;
class VaPlotCommonParams;
   
class VaCommonMovieGui :  public SLForm
{

  public:
       VaCommonMovieGui(  SLDelay             *p,
                          char                *name,
                          HelpCtx             hctx,
                          VaPlotCommonParams  *common_params,
                          int                 which_plot);
       ~VaCommonMovieGui();
       enum {DO_MOVIE};
       enum {FIRST_PANEL, SKIP_PANELS, TOTAL_PANELS};
       virtual void manage();
       virtual Widget make(Widget p = NULL);
       virtual Boolean notifyComplex(SLDelay*, int);
       void    setSensitive(Boolean sensitive);
       void    updateParams();
       void    setMovieParameters(long ident);
       void    setMovieOption(Boolean set);
       Boolean getMovieOption();
       void    setFileMaxMovies(int which_plot, long max_movies);
       void    setFirstMovie(long first_movie);
       void    setSkipMovies(long skip_movies);
       void    setTotalMovies(long total_movies);
       long    getMaxMovies();
       long    getFirstMovie();
       long    getSkipMovies();
       long    getTotalMovies();
       void    registerPlot(int which_plot, VaPlot *plot);
       void    notifyCommon();
       void    synch();

   protected:
       SLTogBox            *_movie_tog;
       SLTextBox           *_movie_box;
       virtual Boolean ValidInput();
       void setToFileDefaults();
       virtual void reloadSystemDefaults(Boolean do_method =True);
       long        _do_movie;
       long        _first_panel;
       long        _skip_panels;
       long        _total_panels;

  private:
       Boolean            _first_time;
       SLTextBox          *_movie_params;
       VaPlotCommonParams *_common_params;
       int                _which_plot;
};

#endif
