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
#ifndef FGSEISOVJDPOP_HH
#define FGSEISOVJDPOP_HH

#include "sl/sl_prim.hh"
#include "sp/sp_list.hh"
#include "sl/sl_text_box.hh"
#include "sl/sl_push_box.hh"
#include "sl/sl_form_pop.hh"
#include "sl/sl_tog_box.hh"
#include "vu/seis_ovjd_pop.hh"
#include "plot/pick_base.hh"
#include "sl/sl_radio_box.hh"

#define SXHDR 10
#define SYHDR 11
#define RXHDR 13
#define RYHDR 14
#define OFFSETHDR 5

class SeisOvjd;
class FgPickOvjd;
class FgSeisPlot;
class FgQcOvjdPlot;

class FgSeisOvjdPop :  public SeisOvjdPop, public SeisInform {


 protected:
       FgPickOvjd        *_pick_ovjd;
       virtual void      DoAction();
       FgQcOvjdPlot      *_plot;
       SLRadioBox         *_select_box;
       SLRadioBox        *_match_radiobox;
       SLTextBox         *_match_box;
       SLTextBox         *_skip_box;
       SLTogBox          *_scan_box;
       long              _table_select;
       long              _primary_header;
       long              _secondary_header;
       long              _skip_headers;
       long              _previous_file_skip;
       long              _original_skip;
       Boolean           _use_match_header;
       static void tableSelect( void *data, long which );
       static void MatchAction( void *data, long which );
       static void SkipAction ( void *data, long which );
       static void ScanTogAction( void *data, long which );

     public:
       FgSeisOvjdPop( Widget               p,
                     char                 *name,
                     SeisPlot             *sp,
                     SeisOvjd             *so,
                     HelpCtx              hctx,
                     FgQcOvjdPlot         *plot);
       virtual ~FgSeisOvjdPop();
       virtual void postScan(SeisPlot *, SeisPlot::ScanDir );
       virtual void preScan(SeisPlot *, SeisPlot::ScanDir );
       friend class SeisOvjd;
       friend class FgPickOvjd;
       virtual Widget make(Widget p);
       virtual void manage();
       virtual void newData();
       long primaryHeader(){return _primary_header;}
       long secondaryHeader(){return _secondary_header;}
       long skipHeaders(){if(!_use_match_header) return _skip_headers;
                          else return 0;}
       Boolean matchHeaderMode(){return _use_match_header;}
};


class FgPickOvjd : public PickBase {

      private:
           FgSeisOvjdPop *_sop;
      public:
           FgPickOvjd(FgSeisPlot *sp, FgSeisOvjdPop *sop);
      protected:
           virtual void buttonAny(int x1, int x2, int y1, int y2, int button,
                                  Action action, Modifier modifier);

};


#endif



