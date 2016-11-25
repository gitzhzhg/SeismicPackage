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
//Class that creates window and plot for random lines
//********************************************************

#ifndef RANDOM_PLOT_H
#define RANDOM_PLOT_H

#include "sl/sl_form_help_pop.hh"
#include "sl/sl_form_pop.hh"

class SeisPlot;
class CubeRandomLinePop;
class SeisLocOut;
class CubeDisplay;


class CubeRandomLinePlot :  public SLFormHelpPop {

 friend class CubeRandomLinePop;

 private:
       Boolean                _changing;
       Boolean                _first_time;
       Boolean                _good_seis_plot_object;
       CubeRandomLinePop      *_pop;
       SeisPlot               *_sp;
       SeisLocOut             *_loc;
       CubeDisplay            *_cube_display;

 protected:
       HelpCtx              _hctx;
       int                  _numcolors;
       void manage();
       virtual void removeButton();
       static void postDestructorClientMessageFunc(void *obj);

 public:
       CubeRandomLinePlot( Widget               p,
                           char                 *name,
                           HelpCtx              hctx,
                           CubeRandomLinePop    *pop,
                           int                  numcolors,
                           CubeDisplay          *cube_display);
       virtual ~CubeRandomLinePlot();
       virtual Widget make(Widget p);
       virtual int plot();
       virtual int replotIfNeeded();
       virtual void display(); 
       SeisPlot *SP ();
};

#endif
