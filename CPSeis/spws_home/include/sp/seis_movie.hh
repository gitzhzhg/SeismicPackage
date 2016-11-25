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
#ifndef SEISMOVIE_H
#define SEISMOVIE_H

#include "sl/sl_arrow_scale.hh"
#include "sp/seis_inform.hh"

#define MAXSPLIST 10
class SeisPlot;
class MovieInform;

class SeisMovie : public SeisInform, public SLArrowScale {

   private:
   protected:
       int         _prev_value;

   public:
       SeisMovie( Widget   p,
                  char     *name    ="movie",
                  SeisPlot *sp      =NULL,
                  HelpCtx  hctx     =NULL);
       virtual ~SeisMovie();
       virtual void ValueChangeAction( long value);
       void addControl(SeisPlot *sp);
       void removeControl(SeisPlot *sp);
       virtual void newPlot(SeisPlot *sp );
       virtual void noPlotDisplayed(SeisPlot *sp );
       virtual void newSeisPlotCreatedInWindow(SeisPlot *);
       virtual void notCurrentInWindow(SeisPlot *);
       virtual void postMovie(SeisPlot *sp, SeisPlot::MovieDir );
       long getNumFrames();
};

#endif
