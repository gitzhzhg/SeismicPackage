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
#ifndef SEISXYOUTPOP_HH
#define SEISXYOUTPOP_HH

#include "sl/sl_form_pop.hh"
#include "sp/seis_inform.hh"

class SeisPlot;
class SLRadioBox;
class SLTextBox;



class SeisXYOutPop : public SLFPopSep, public SeisInform {

    private:
        int           _x_current_hw;
        int           _y_current_hw;
        SLRadioBox   *_x_which_hw;
        SLTextBox    *_x_hw_choice;
        SLRadioBox   *_y_which_hw;
        SLTextBox    *_y_hw_choice;

    protected:
       virtual void    DoAction();
       virtual Boolean ValidInput();
       void    applyParams(SeisPlot *sp);

    public:
       SeisXYOutPop( Widget    p,
                     char      *name,
                     HelpCtx   hctx,
                     SeisPlot  *sp);
       virtual ~SeisXYOutPop();
       virtual Widget make(Widget p);
       void addControl(SeisPlot *sp);
       void removeControl(SeisPlot *sp);
       virtual void notCurrentInWindow(SeisPlot *sp);
       virtual void newSeisPlotCreatedInWindow(SeisPlot *);
       virtual void    managing();
       virtual Boolean notifyComplex(SLDelay*, int);

};
#endif
