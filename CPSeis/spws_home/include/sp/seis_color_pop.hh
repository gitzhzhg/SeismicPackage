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
//class that creates color processing menu
#ifndef SEISCOLORPOP_H
#define SEISCOLORPOP_H

#include "sl/sl_scale.hh"
#include "sp/sp_list.hh"
#include "sl/sl_tog_box.hh"
#include "sl/sl_text_box.hh"
#include "sl/sl_form_pop.hh"
#include "sl/sl_radio_box.hh"
#include "sp/seis_cbar.hh"
#include "sp/seis_color.hh"
#include "sp/seis_ctype.hh"
#include "sp/seis_inform.hh"




class SeisCtypePop;
class PercentScale;



class SeisColorPop :  public SLFPopSep, public SeisInform {

 protected:
       Boolean         _allow_data_processing;
       Boolean         _dont_plot_yet;
       Boolean         _first_time;
       Boolean         _been_managed;
       Boolean         _do_amps;
       Boolean         _grading;
       SeisPlot        *_sp;
       Boolean         _plot_on_doaction;
       Boolean         _new_file;
       Boolean         _use_file_defaults;
       Boolean         _new_appdefaults;
       Boolean         _change_button;
       int             _color_num;
       SeisCbar        *_cbar;
       SeisCtypePop    *_seiscolortype;
       SPList          _list;
       ColorInfo       _col;
       float           _minamp;
       float           _maxamp;
       float           _minstart;
       float           _maxstart;
       long            _grad_vert;
       long            _grad_horz;
       long            _hi_res;
       Widget          _selectlab;
       Widget          _processlab;
       SLTogBox        *_gradebox;
       SLTextBox       *_amp1box;
       SLTextBox       *_amp2box;
       SLRadioBox      *_ctypebox;
       SLRadioBox      *_amptypebox;
       PercentScale    *_pncscale;
       PercentScale    *_ppcscale;
       SLTogBox        *_centerbox;
       long            _center_percent;
       static void     ChoiceAction( void *data, long which );
       static void     barAmpAction( void *data, long which );
       virtual void    DoAction();
       virtual Boolean ValidInput();
       static void AmpFocusAction( void *data, long which );
       static void AmpLosingFocusAction  ( void *data, long which );


     public:
       SeisColorPop( Widget               p,
                 char                 *name,
                 SeisPlot             *sp,
                 HelpCtx              hctx,
                 Boolean              allow_data_processing = True,
                 Boolean              plot_on_doaction = True);
       virtual ~SeisColorPop();
       virtual Widget make(Widget p =NULL);
       virtual void manage();
       virtual void reloadDefaults(Boolean do_method= True);
       virtual void reloadSystemDefaults(Boolean do_method =True);
       virtual void addSP(SeisPlot *sp);
       virtual void removeSP(SeisPlot *sp);
       virtual void newColorBar();
       void presetAmplitudes(float min, float max){_minamp = min;_maxamp = max;}
       void presetAmpType(Boolean t){_do_amps = t;}
       void presetGrading(Boolean t){_grading = t;} 
       void dontPlotYet(Boolean n)  {_dont_plot_yet = n;}
       long gradingHorizontal()     {return _grad_horz;}
       long gradingVertical()       {return _grad_vert;}
       long centerPercents()        {return _center_percent;}
       long hiRes()                 {return _hi_res;}
       Boolean beenManaged(){return _been_managed;}
       virtual void notCurrentInWindow(SeisPlot *);
       virtual void destroyed(SeisPlot *);
       void setAmplitudes(float amp_min, float amp_max);
       float getMinAmp() {return _minamp;}
       float getMaxAmp() {return _maxamp;}
       friend  class SeisCtypePop;
       friend  class PercentScale;
       SLRadioBox   *getColorTypeBox() {return _ctypebox;}
       SLTogBox     *getGradeBox(){return _gradebox;}
       SeisCtypePop *getSeisColorTypePop(){return _seiscolortype;}
       SLRadioBox   *getAmpTypeBox(){return _amptypebox;}
       PercentScale *getPncScale(){return _pncscale;}
       PercentScale *getPpcScale(){return _ppcscale;}
       void updateSeisPlotParameters();
       void setColorOnSeisPlot(SeisPlot *sp);
       enum {MINAMP, MAXAMP};
       enum {GRAD_VERT, GRAD_HORZ, HI_RES};
       enum {GRAYTYPE, COLORTYPE, RAMTYPE};
       enum {BARVALS, AMPVALS, PCNTVALS, CENTER_PERCENT};  
};




class SeisCtypePop : public SeisCtype {

 protected:
       SeisColorPop  *_scp;
       static void doCtog(  SeisCtype*, long);

 public:
       SeisCtypePop( Widget   p,
                     char     *name,
                     HelpCtx  hctx,
                     SeisPlot *sp,
                     SeisCbar *cbar,
                     Boolean  make_now,
                     SeisColorPop *scp)
         : SeisCtype(p,name,hctx,sp,cbar,make_now),_scp(scp){};
       virtual void typeChange(int which);
       void newColorBar();
};



class PercentScale : public SLScale {

 protected:
       SeisColorPop  *_scp;
       virtual void ScaleAction(int val);
       
 public:
       PercentScale(  SLDelay       *contain,
                      char          *name,
                      HelpCtx       hctx,
                      int           *valptr,
                      SeisColorPop  *scp)
       : SLScale(contain,name,hctx,valptr),_scp(scp){};
};


#endif

