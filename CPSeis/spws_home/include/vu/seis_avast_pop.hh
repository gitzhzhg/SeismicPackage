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
#ifndef SEISAVASTPOP_HH
#define SEISAVASTPOP_HH

#include "sl/sl_prim.hh"
#include "sp/sp_list.hh"
/*
#include "sl/sl_text_box.hh"
*/
#include "sl/sl_push_box.hh"
#include "sl/sl_form_pop.hh"
#include "vu/seis_avast.hh"
#include "plot/pick_base.hh"
#include "wproc.h"
//#include "file_choice.h"


#define SXHDR 10
#define SYHDR 11
#define RXHDR 13
#define RYHDR 14
#define OFFSETHDR 5

class SeisAvast;
class PickAvast;
class SLpFile;

class SeisAvastPop :  public SLFPopSep {

 private:
       static void fileCallback (void *data, long ident, char *oldvar,
         char *newvar);

 protected:
       SeisPlot        *_sp;
       Boolean         _first_time;
       Boolean         _trace_selected;
       SeisAvast        *_so;
       PickAvast        *_pick_avast;
       SLpFile         *_infile;
       Boolean         _plot_on_doaciton;
       Boolean         _new_file;
       Boolean         _use_file_defaults;
       Boolean         _new_appdefaults;
       float           _new_angle;
       long            _tindex;
       char            _new_avastfile[300];
       char            *_infile_failstr;
       Boolean         _infile_valid;
/*
       SLTextBox       *_angle_box;
*/
       class TpboxAngleTable *_angle_box;
       virtual void    UndoInput();
       virtual void    DoAction();
       virtual Boolean ValidInput();
       static  void selected  ( void *data, long which );
       static  void original  ( void *data, long which );
       void DoSelect( Widget w, XEvent *event, char *mode[]);
       void filein (long ident, char *oldvar, char *newvar);

     public:
       SeisAvastPop( Widget               p,
                    char                 *name,
                    SeisPlot             *sp,
                    SeisAvast             *so,
                    HelpCtx              hctx);
       SeisAvastPop( Widget               p,
                    char                 *name,
                    SeisPlot             *sp,
                    SeisAvast             *so,
                    HelpCtx              hctx,
                    Boolean              custom_widgets);
       virtual ~SeisAvastPop();
       friend class SeisAvast;
       friend class PickAvast;
       virtual Widget make(Widget p);
       virtual void manage();
       virtual void reloadDefaults(Boolean do_method= True);
       virtual void reloadSystemDefaults(Boolean do_method =True);
       void changeHeaders();
       virtual void newData();
};


class PickAvast : public PickBase {

      private:
           SeisAvastPop *_sop;
      public:
           PickAvast(SeisPlot *sp, SeisAvastPop *sop);
      protected:
           virtual void buttonAny(int x1, int x2, int y1, int y2, int button,
                                  Action action, Modifier modifier);

};


#endif



