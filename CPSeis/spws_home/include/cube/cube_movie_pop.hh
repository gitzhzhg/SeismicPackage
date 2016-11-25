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
//********************************************
//Author Michael L. Sherrill 10/96
//Creates menu to control cube movie images
//********************************************

#ifndef CUBE_MOVIE_POP_H
#define CUBE_MOVIE_POP_H

#include "sl/sl_form_pop.hh"
#include "sl/sl_arrow_scale.hh"
#include "sl/slp_arrow.hh"
#include "sl/sl_text_box.hh"
#include "sl/sl_push_box.hh"


enum{ PREFERENCE = 11};


class CubeMoviePop :  public SLFPopSep
{

 friend class CubeMoviePreference;

 private:
     class CubeMoviePreference *_prefer_pop;
     class CubeMovieControl    *_cube_movie_control;
     HelpCtx         _hctx; 
     SLArrowScale    *_inline_scale;
     SLArrowScale    *_crossline_scale;
     SLArrowScale    *_timeslice_scale;
     SLArrowScale    *_cube_scale;
     SLpArrow        *_previous_inline_read;
     SLpArrow        *_next_inline_read;
     SLpArrow        *_previous_crossline_read;
     SLpArrow        *_next_crossline_read;
     SLpArrow        *_previous_timeslice_read;
     SLpArrow        *_next_timeslice_read;
     SLTextBox       *_read_box_inline;
     SLTextBox       *_read_box_crossline;
     SLTextBox       *_read_box_timeslice;
     SLPushBox       *_load_inline;
     SLPushBox       *_load_crossline;
     SLPushBox       *_load_timeslice;
     SLPushBox       *_load_all;
     Widget           _cube_id_label;
     int              _inline_frame;
     int              _crossline_frame;
     int              _timeslice_frame;
     int              _cube_number;
     float            _read_inline;
     float            _read_crossline;
     float            _read_timeslice;
     char             _cube_id[256];  
     int              _plot_type;
     Boolean          _derived_class_made;

 protected:
     void    extraButton(int ident);
     void    DoAction();
     Boolean ValidInput();
     void    UndoInput();
     static  void arrowTrap(void *data, long ident);
     void    setAttachments();       
     static void readFocusAction( void *data, long which );
     static void readLosingFocusAction( void *data, long which );
     static void loadButton( void *data, long which );
     virtual Boolean notifyComplex(SLDelay*, int ident);
     void    setValue (class SLArrowScale *scale, int value, int high);

 public:
     CubeMoviePop( Widget              p,
                   char                *name,
                   HelpCtx             hctx,
                   CubeMovieControl    *cube_movie_control); 
     ~CubeMoviePop();
     virtual Widget make(Widget p =NULL);
     virtual void manage();     
     virtual void makeAndManage(Widget p =NULL);
     void updateCubeInfo();
     void readPattern(long ident, int who_called);
     void movieFrame(long ident);
     void setLoadInline(Boolean sensitive);
     void setLoadCrossline(Boolean sensitive);
     void setLoadTimeslice(Boolean sensitive);
};







#endif
