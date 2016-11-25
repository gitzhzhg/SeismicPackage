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
// Author Michael L. Sherrill
// Class that creates a header graph above SeisPlot

#ifndef SEISHEADERPOP_H
#define SEISHEADERPOP_H


#include "sl/sl_text_box.hh"
#include "sl/sl_tog_box.hh"
#include "sl/sl_form_pop.hh"
#include "sp/seis_plot.hh"
#include "sp/seis_plot_tie.hh"
#include "vect/ll_seis_vect.hh"
#include "vect/vector.hh"
#include "vect/vect_data.hh"

class HeaderPopInform;

class SeisHeaderPop :  public SLFPopSep {
 
 friend class HeaderPopInform;

 public:
       SeisHeaderPop( Widget               p,
                      char                 *name,
                      SeisPlot             *sp,
                      SeisPlotTie          *parent_sp,
                      HelpCtx              hctx,
                      char                 *primary_color = "red",
                      char                 *secondary_color = "green",
                      char                 *tertiary_color = "blue");
       virtual ~SeisHeaderPop();
       virtual Widget make(Widget p);
       virtual void manage();
       virtual Boolean notifyComplex(SLDelay*, int ident);
       virtual void reloadDefaults(Boolean do_method= True);
       virtual void reloadSystemDefaults(Boolean do_method =True);
       void getHeaders();
       void drawHeaders(Boolean new_size);
       void headerColor(int which, char *color);
       void hide(Boolean hide);
       SeisVectLinkedList *_vect_primary_ll;
       SeisVectLinkedList *_vect_secondary_ll;
       SeisVectLinkedList *_vect_tertiary_ll;
       VectData           *_vect_primary_data;
       VectData           *_vect_secondary_data;
       VectData           *_vect_tertiary_data;
       Vector             *_V_primary;
       Vector             *_V_secondary;
       Vector             *_V_tertiary;
       float              *_xdata;
       float              *_ydata;
       Boolean            _visible;
       float              _user_y_top;
       float              _user_y_bottom;


 protected:
       SeisPlot        *_sp;
       SeisPlotTie     *_parent_sp;
       HeaderPopInform *_inform;
       Boolean         _first_primary_time;
       Boolean         _first_secondary_time;
       Boolean         _first_tertiary_time;
       Boolean         _plot_on_doaciton;
       Boolean         _use_file_defaults;
       Boolean         _new_appdefaults;
       Boolean         _first_time;
       Boolean         _draw_headers;
       Boolean         _hide;
       long            _primary_header;
       long            _secondary_header;
       long            _tertiary_header;
       float           _plot_height;
       float           _timing_lines;
       SLTextBox       *_header_box;
       SLTogBox        *_user_y_tog;
       SLTextBox       *_user_y_box;
       virtual void    UndoInput();
       virtual void    DoAction();
       virtual Boolean ValidInput();
       char            *_primary_color;
       char            *_secondary_color;
       char            *_tertiary_color;


  private:
     
};


#endif



