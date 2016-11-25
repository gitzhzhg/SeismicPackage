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
#ifndef VA_PANEL_GUI_HH
#define VA_PANEL_GUI_HH

#include "sl/sl_arrow_scale.hh"

class VaPlotControl;

class VaPanelGui : public SeisInform,
                   public VfInform, 
                   public SLArrowScale {

  private:
     VaPlotControl   *_plot_ctl;
     SeisPlot        *_blank_sp;
     SeisPlot        *_sem_sp;
     SeisPlot        *_cmp_sp;
     Boolean         _dragging_scale;
     Boolean         _doing_hardcopy;
     long            _total_panels;
     long            _first_panel;
     long            _skip;
     Widget          _push;
     Pixel           _green_pixel;
     Pixel           _normal_pixel;
     int             _current_displayed_panel;
 
     void setPush(Boolean);
     Boolean haveThisPanel(int panel);
     void setActiveFunctionTimer(int panel);
     void setActiveFunction();
     static void timerCallback(XtPointer, XtIntervalId *);
     void timer(int panel);
     void doBigChange(VfDataset *dataset);
     void showPanel(SeisPlot *sp);

  public:
     VaPanelGui(SLDelay *p, VaPlotControl *plot_ctl, Widget push);

     virtual Widget make(Widget p);
     virtual void ValueChangeAction( long value);
     virtual void newPlot(SeisPlot *sp);
     virtual void noPlotDisplayed(SeisPlot *sp);
     virtual void postMovie(SeisPlot *, SeisPlot::MovieDir );
     virtual void notCurrentInWindow(SeisPlot *);
     virtual void postTotalChanges(VfDataset *dataset);
     virtual void postRemoveInsertVelocityFunctions
                       (VfDataset *dataset, long ifun, long nrem, long nins);
     virtual void postChangeDataLock(VfDataset *dataset);
     virtual void postNewActiveDataset();
     virtual void preWriteHardCopy(SeisPlot *);
     virtual void postWriteHardCopy(SeisPlot *);

     void plotCurrentPanel();
};
#endif
