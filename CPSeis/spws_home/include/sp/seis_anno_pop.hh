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
#ifndef SEISANNOPOP_H
#define SEISANNOPOP_H

#include "sl/sl_form_pop.hh"
#include "sp/seis_inform.hh"

class SeisPlot;
class SLTextBox;
class SLRadioBox;


class SeisAnnoPop : public SLFPopSep, public SeisInform {

    private:
    protected:
       virtual void    DoAction();
       virtual void    UndoInput();
       virtual Boolean ValidInput();

       SeisPlot     *_sp, *_info_sp;
       SLTextBox    *_anno_text;
       SLRadioBox   *_anno_type;
       SLRadioBox   *_units_type;

       float _ptl, _stl;
       long  _primh, _sech;
       long  _start_trace, _trace_inc;
       char  *_plot_title;
       Boolean _will_plot;
       Boolean _first_time;

       Widget _titlew;

    public:
       SeisAnnoPop( SLDelay   *contain,
                    char      *name,
                    HelpCtx   hctx,
                    SeisPlot  *sp,
                    SeisPlot  *info_sp =NULL);
       SeisAnnoPop( Widget    p,
                    char      *name,
                    HelpCtx   hctx,
                    SeisPlot  *sp,
                    SeisPlot  *info_sp =NULL,
                    Boolean   make_now= False);
       virtual ~SeisAnnoPop();
       void init();
       virtual Widget make(Widget p);
       virtual void manage();
       void    plotOnOK(Boolean p) {_will_plot= p; }
       void    setFromOther(SeisPlot *othersp =NULL);
       virtual void  reloadDefaults(Boolean do_method= True);
       void    reloadSystemDefaults(Boolean);
       virtual void  notCurrentInWindow(SeisPlot *sp);
       virtual void  applyParams();
       virtual void  turnOffTimeDepthOption();
       virtual float getPrimaryTimingLine()  {return _ptl;}
       virtual float getSecondaryTimingLine(){return _stl;}
       virtual void  setPrimaryTimingLine(float ptl);
       virtual void  setSecondaryTimingLine(float stl);
       virtual Widget getTitleWidget(){return _titlew;}
       virtual long getStartTrace(){return _start_trace;}
       virtual void setStartTrace(long t);
       virtual long getTraceIncrement(){return _trace_inc;}
       virtual void setTraceIncrement(long i);
       virtual long getPrimaryHeader(){return _primh;}
       virtual void setPrimaryHeader(long h);
       virtual long getSecondaryHeader(){return _sech;}
       virtual void setSecondaryHeader(long h);
       virtual void setSeisPlot(SeisPlot *sp);
       virtual void setTextState(int which, Boolean state);
       virtual void setTimeDepthState(Boolean disable);
       virtual void setDepthOption(Boolean set);
       virtual Boolean getDepthOption();
       enum {PTL, STL, PRIMH, SECH, START_TRACE, TRACE_INC, TIME, DEPTH, 
             METRIC, ENGLISH};
};

#endif
