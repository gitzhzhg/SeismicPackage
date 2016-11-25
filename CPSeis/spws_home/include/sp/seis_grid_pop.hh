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
//author Michael L. Sherrill
//class that creates a grid type image
#ifndef SEISGRIDPOP_H
#define SEISGRIDPOP_H

#include "sl/sl_scale.hh"
#include "sl/sl_text_box.hh"
#include "sl/sl_form_pop.hh"
#include "sl/sl_tog_box.hh"
#include "sp/seis_plot.hh"

class SeisGridPop :  public SLFPopSep {

 private:
       long                _focus_id;
       float               _focus_value;

 protected:
       SeisPlot        *_sp;
       Boolean         _first_time;
       Boolean         _plot_on_doaciton;
       Boolean         _use_file_defaults;
       Boolean         _new_appdefaults;
       int             _plot_type;
       float           _pwidth;
       float           _pheight;
       float           _left;
       float           _right;
       float           _top;
       float           _bottom;
       SLTextBox       *_header_box;
       SLTogBox        *_enforcebox;
       virtual void    UndoInput();
       virtual void    DoAction();
       virtual Boolean ValidInput();
       static void CoordFocusAction( void *data, long which );
       static void CoordLosingFocusAction( void *data, long which );
       static void EnforceAction( void *data, long which );

     public:
       SeisGridPop( Widget               p,
                    char                 *name,
                    SeisPlot             *sp,
                    HelpCtx              hctx);
       virtual ~SeisGridPop();
       virtual Widget make(Widget p);
       virtual void manage();
       virtual void reloadDefaults(Boolean do_method= True);
       virtual void reloadSystemDefaults(Boolean do_method =True);
       virtual void setPlotType(int type);
       enum {PWIDTH, PHEIGHT, PLEFT, PRIGHT, PTOP, PBOTTOM, ENFORCE};
};


#endif



