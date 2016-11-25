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
//***********************************************
//Author Michael L. Sherrill 11/96
//Creates menu to control cube movie preferences
//***********************************************

#ifndef CUBE_MOVIE_PREFER_H
#define CUBE_MOVIE_PREFER_H

#include "sl/sl_form_pop.hh"
#include "sl/sl_text_box.hh"
#include "sl/sl_tog_box.hh"

class CubeMoviePop;
class CubeMovieControl;

enum{INLINE_SCAN_INCREMENT,    INLINE_MOVIE_INCREMENT, INLINE_TOTAL, 
     CROSSLINE_SCAN_INCREMENT, CROSSLINE_MOVIE_INCREMENT, CROSSLINE_TOTAL,
     TIMESLICE_SCAN_INCREMENT, TIMESLICE_MOVIE_INCREMENT, TIMESLICE_TOTAL};

enum{INLINE_AUTOLOAD, CROSSLINE_AUTOLOAD, TIMESLICE_AUTOLOAD, CUBE_AUTOLOAD};


class CubeMoviePreference : public SLFPopSep {

  private:
    CubeMoviePop     *_cube_movie_pop;
    CubeMovieControl *_cube_movie_control;
    SLTextBox        *_line_select_text;
    SLTogBox         *_auto_load_inline;
    SLTogBox         *_auto_load_crossline;
    SLTogBox         *_auto_load_timeslice;
    SLTogBox         *_auto_load_cube;
    Widget           _cube_id_label;
    int              _previous_inline_total;
    int              _previous_crossline_total;
    int              _previous_timeslice_total;
    int              _previous_inline_increment;
    int              _previous_crossline_increment;
    int              _previous_timeslice_increment;

  protected:
    char             _cube_id[256];
  
  public:
    long             _inline_autoload;
    long             _crossline_autoload;
    long             _timeslice_autoload;
    long             _cube_autoload;
    int              _inline_scan_increment;
    int              _inline_movie_increment;
    int              _inline_total;
    int              _crossline_scan_increment;
    int              _crossline_movie_increment;
    int              _crossline_total;
    int              _timeslice_scan_increment;
    int              _timeslice_movie_increment;
    int              _timeslice_total;
    CubeMoviePreference( Widget           p,
                         char             *name,
                         HelpCtx          hctx,
                         CubeMoviePop     *cube_movie_pop,
                         CubeMovieControl *cube_movie_control);  
    ~CubeMoviePreference();
    virtual Widget make(Widget p);
    static void lineSelectFocusAction( void *data, long which );
    static void lineSelectLosingFocusAction( void *data, long which );
    static void autoLoadAction( void *data, long which );
    void updateCubeInfo();
 
};

#endif
