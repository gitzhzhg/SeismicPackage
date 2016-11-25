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
//********************************************************
//Author Michael L. Sherrill
//Class that creates a sub menu of the main qc pop up menu
//********************************************************

#ifndef FG_QC_SUB_POP_H
#define FG_QC_SUB_POP_H

#include "sl/sl_text_box.hh"
#include "sl/sl_radio_box.hh"
#include "sl/sl_form_pop.hh"
#include "sl/sl_push_box.hh"
#include "sl/sl_tog_box.hh"
#include "sl/sl_error_pop.hh"
#include "sl/sl_pull_pop.hh"


class FgQcPop;
class FgQcPlot;



class FgQcPopSubMenu :  public SLFPopSep {

 private:
       int                 _plot_type;
       long                _focus_id;
       float               _focus_value;
       char                *_selected;
       Boolean             _new_appdefaults;
       Boolean             _use_file_defaults;
       Boolean             _plot_on_doaction;
       

 protected:
       FgQcPop             *_fgqc_pop;
       FgQcPlot            *_active_plot;
       HelpCtx              _hctx;
       Boolean              _first_time;
       float                _xmin;
       float                _xmax;
       float                _ymin;
       float                _ymax;
       static void button_control( void * /*data*/, long /*which*/ ){};
       static void FocusAction( void * /*data*/, long /*which*/ ){};
       static void LosingFocusAction( void * /*data*/, long /*which*/ ){};

 public:
       FgQcPopSubMenu( Widget               p,
                       char                 *name,
                       HelpCtx              hctx,
                       FgQcPop              *fgqc_pop);
       virtual ~FgQcPopSubMenu();
       friend class FgQcPop;
       virtual void setActivePlot(FgQcPlot *fp){_active_plot = fp;}
       SLErrorPop *_errpop;
};



class OffsetDistributionMenu : public FgQcPopSubMenu {

  private:
    SLTextBox        *_offset_box;
    long             _menu_type;
    class SLRadioBox *_limittyperadiobox;

  protected:
    float      _offset_min;
    float      _offset_max;
    float      _azimuth_min;
    float      _azimuth_max;
    long       _limit_type;
    static void LosingFocusAction( void *data, long which );
    static void LimitTypeAction ( void *data, long which );

  public:
    OffsetDistributionMenu( Widget               p,
                            char                 *name,
                            HelpCtx              hctx,
                            FgQcPop              *fgqc_pop,
                            long                 menu_type=0); 
    virtual ~OffsetDistributionMenu();
    long getLimitType () {return _limit_type;}
    float getOffsetMinimum () {return _offset_min;}
    float getOffsetMaximum () {return _offset_max;}
    float getAzimuthMinimum () {return _azimuth_min;}
    float getAzimuthMaximum () {return _azimuth_max;}
    virtual void manage();
    virtual Widget make(Widget p);
};


class NormalizedBinMenu : public FgQcPopSubMenu {

  private:
    SLTextBox        *_percent_box;

  protected:
    float               _percent;
    static void LosingFocusAction( void *data, long which );

  public:
    NormalizedBinMenu( Widget               p,
                       char                 *name,
                       HelpCtx              hctx,
                       FgQcPop              *fgqc_pop);
    virtual ~NormalizedBinMenu();
    virtual Widget make(Widget p);
    float getNormalizedBinPercent() {return _percent;}
};
#endif
