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
//class that creates and controls the panning of an image in a separate window
#ifndef SEISPAN_H
#define SEISPAN_H

#include "wproc.h"
#include "sl/sl_form_pop.hh"
#include "sp/seis_movie.hh"

class SeisPlotZoom;
class SeisPlotZoomUnder;

class SeisPanner : public SLFPopSep {

   private:
       static void doPanCallback(Widget, struct CB*, XmAnyCallbackStruct*);
       SeisMovie  *_movie;

   protected:
       enum {NUM_PUSH = 8};
       void doPan(Widget, struct CB*, XmAnyCallbackStruct*);
       Widget            _pan_ctl;           
       Widget            _sep;
       struct CB         _cb[NUM_PUSH];
       Widget            _pan_w[NUM_PUSH];
       struct PlotImage  *_image;
       virtual void      DoAction();
       SeisPlotZoom      *_spz;
       SeisPlotZoomUnder *_spzu;

   public:
       SeisPanner(Widget p,char *name,SeisPlotZoom *spz,Boolean movie = True,
                  HelpCtx = NULL);
       virtual ~SeisPanner();
       Widget panW() { return _pan_ctl; };
       Widget sepW() { return _sep; };
       void setPanImage (SeisPlotZoom *spz, SeisPlotZoomUnder *spzu = NULL);
       void addMovie(SeisMovie *movie);
};

#endif
