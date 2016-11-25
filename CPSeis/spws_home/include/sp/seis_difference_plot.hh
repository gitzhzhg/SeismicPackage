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
//************************ COPYRIGHT NOTICE ****************************
//      CONFIDENTIAL AND PROPRIETARY INFORMATION OF CONOCO INC.        *
//       PROTECTED BY THE COPYRIGHT LAW AS AN UNPUBLISHED WORK         *
//************************ COPYRIGHT NOTICE ****************************


//********************************************************
//Author Michael L. Sherril 12-18-2001
//Class that creates window and plot for difference plots
//********************************************************

#ifndef DIFFERENCE_PLOT
#define DIFFERENCE_PLOT

#include "sl/sl_form_help_pop.hh"
#include "sp/seis_inform.hh"


class SeisDifferencePlot :  public SLFormHelpPop, public SeisInform {


 public:
       SeisDifferencePlot(Widget                 p,
                         char                    *name,
                         HelpCtx                 hctx,
                         class SeisPlot          *first_sp,
                         class SeisPlot          *second_sp,
                         class SeisDifferencePop *pop,
                         int                     numcolors);

       virtual ~SeisDifferencePlot();

       virtual void manage();
       virtual Widget make(Widget p);
       class   SeisPlot *getSP() {return _sp;}
       virtual void newPlot(SeisPlot *sp); //inform
       virtual void destroyed(SeisPlot *sp);
       virtual void removeButton();
       virtual void postScan(SeisPlot *sp, SeisPlot::ScanDir dir);
       virtual void extraButton(int ident);
       virtual class SeisColorPop *getColorPop(){return _color_pop;}
       virtual int plot(char *errormsg);
       enum{COLOR_POP = 77, COLOR_BAR};

  protected:

  private:
       class SeisPlot           *_first_sp;
       class SeisPlot           *_second_sp;
       class SeisPlot           *_sp;
       class SeisDifferencePop  *_pop;
       class SeisCbarPop        *_cbar_pop;
       class SeisColorPop       *_color_pop;
       class SLPushBox          *_color_opts;
       class SeisLocOut         *_loc;
       Boolean                  _been_managed;
       long                     _numcolors;
       char                     _title[512];
       float                    _lav;
       void getMinAndMaxOfData(long n, float *data, float *min, float *max);
       virtual int computeDifference();
};

#endif
