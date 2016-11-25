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
#ifndef SEISSHIFTPOP_HH
#define SEISSHIFTPOP_HH

#include "sl/sl_prim.hh"
#include "sp/sp_list.hh"
#include "sl/sl_text_box.hh"
#include "sl/sl_radio_box.hh"
#include "sl/sl_tog_box.hh"
#include "sl/sl_push_box.hh"
#include "sl/sl_form_pop.hh"
#include "pick/seis_shift.hh"

#define OFFSETHDR 5

class SeisShift;

class SeisShiftPop :  public SLFPopSep {

 private:

 protected:
       SeisPlot        *_sp;
       Boolean         _first_time;
       SeisShift       *_ss;
       Boolean         _plot_on_doaciton;
       Boolean         _new_file;
       Boolean         _use_file_defaults;
       Boolean         _new_appdefaults;
       float           _new_velocity;
       float           _new_time;
       float           _old_time;
       int             _new_header;
       long            _shift_on_scan;
       SLTextBox       *_velocitybox;
       SLTextBox       *_timebox;
       SLRadioBox      *_stypebox;
       SLRadioBox      *_mtypebox;
       SLPushBox       *_original_data;
       SLTogBox        *_shift_scan_tog;
       virtual void    UndoInput();
       virtual void    DoAction();
       virtual Boolean ValidInput();
       static void     original(void *data, long which );
       static void     settype(void *data, long which );
       static void     timechange(void *data, long which );
       static void     velhdrchange(void *data, long which );
       static void     scanToggleAction(void *data, long which);
     public:
       SeisShiftPop( Widget               p,
                     char                 *name,
                     SeisPlot             *sp,
                     SeisShift            *ss,
                     HelpCtx              hctx);
       virtual ~SeisShiftPop();
       friend class SeisShift;
       virtual Widget make(Widget p);
       virtual void manage();
       virtual void reloadDefaults(Boolean do_method= True);
       virtual void reloadSystemDefaults(Boolean do_method =True);
       void newData();
       static void scanShift(void *obj);
};


#endif



