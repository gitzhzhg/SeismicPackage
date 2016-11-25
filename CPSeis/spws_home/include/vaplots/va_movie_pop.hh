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
#ifndef VAMOVIEPOP_HH
#define VAMOVIEPOP_HH

#include "sl/sl_form_pop.hh"
#include "sp/seis_inform.hh"


class SeisMovie;
class VaPlotControl;
class SLClientMessage;


class VaMoviePop : public SLFPopSep, public SeisInform {

  private:
        VaPlotControl     *_plot_ctl;
        SeisMovie         *_sem_ctl;
        SeisMovie         *_gvs_ctl;
        SeisMovie         *_iso_ctl;
        SLClientMessage   *_cmessage;

        void showMovieControl();

  public:
        VaMoviePop(Widget         p,
                   char          *name,
                   HelpCtx        hctx,
                   VaPlotControl *plot_ctl);
        ~VaMoviePop();
        Widget make(Widget p);
        virtual void newPlot(SeisPlot *sp);
        virtual void noPlotDisplayed(SeisPlot *sp);
        virtual void postMovie(SeisPlot *, SeisPlot::MovieDir );
        virtual void notCurrentInWindow(SeisPlot *);
        virtual Boolean notifyComplex(SLDelay*, int );

};


#endif
