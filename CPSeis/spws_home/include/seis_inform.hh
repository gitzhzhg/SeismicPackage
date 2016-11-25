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
#ifndef SEISPICK_H
#define SEISPICK_H

#include "sp/seis_plot.hh"
#include "sp/sp_list.hh"

class SeisZoomer;
class SeisPlotTie;
class HardCopyPlot;


class SeisInform  : public SPList {

  protected:
  public:
     SeisInform() {};
     SeisInform(SeisPlot *sp);
     virtual ~SeisInform(); 

     void addSeisPlot(SeisPlot *sp, void *user_data =NULL); 
     void delSeisPlot(SeisPlot *sp);

     virtual void preZoom(SeisPlot *, SeisZoomer *, SeisPlot::ZoomDir ) {}
     virtual void zoomInProgress(SeisPlot *, SeisPlot::ZoomDir )        {}
     virtual void postZoom(SeisPlot *, SeisPlot::ZoomDir )              {}
     virtual void postZoomSeparateWindow(SeisPlot *, SeisPlot *)        {}

     virtual void preScan(SeisPlot  *, SeisPlot::ScanDir )              {}
     virtual void postScan(SeisPlot *, SeisPlot::ScanDir )              {}

     virtual void preMovie(SeisPlot *,  SeisPlot::MovieDir )            {}
     virtual void postMovie(SeisPlot *, SeisPlot::MovieDir )            {}

     virtual void plotTypeChange(SeisPlot *, 
                                 int /*new_plot_mode*/,
                                 int /*old_plot_mode*/ )                {}
     virtual void preDataChange(SeisPlot * )                            {}
     virtual void prePlot(SeisPlot * )                                  {}
     virtual void newPlot(SeisPlot * )                                  {}
     virtual void noPlotDisplayed(SeisPlot *)                           {}

     virtual void addingTie(SeisPlotTie *, SeisPlot * )                 {}
     virtual void removingTie(SeisPlotTie *, SeisPlot *)                {}

     //virtual void addingUnderlay(SeisPlot *, SeisPlotUnder *)         {}
     //virtual void removingUnderlay(SeisPlot *, SeisPlotUnder *)       {}
     virtual void dragImage(SeisPlot *)                                 {}

     virtual void unitChange(SeisPlot *, int )                          {}

     virtual void expose(SeisPlot *, int, int, int, int)                {}
     virtual void destroyed(SeisPlot *)                                 {}

     virtual void colorChange(long, long )                              {}
     virtual void postDataPreImage(SeisPlot *)                          {}
     virtual void visableAreaChange(SeisPlot *, int, int, int, int)     {}

     virtual void startingDragScroll(SeisPlot *)                        {}
     virtual void endingDragScroll(SeisPlot *)                          {}

     virtual void mouseOutputUpdate(SeisPlot*, float /*x*/, float /*y*/){}

     virtual void preWriteHardCopy(SeisPlot *) {}
     virtual void writeToHardCopy( SeisPlot     *, 
                                   HardCopyPlot *, 
                                   float        /*marker_scale */ )     {}
     virtual void postWriteHardCopy(SeisPlot *) {}

     virtual void notCurrentInWindow(SeisPlot *)                        {}
     virtual void newSeisPlotCreatedInWindow(SeisPlot *)                {}

     virtual void backingStoreChange(SeisPlot*, Boolean /*backing_on*/)  {}
};

#endif
