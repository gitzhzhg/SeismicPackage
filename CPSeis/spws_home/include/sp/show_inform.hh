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
#ifndef SHOW_INFORM_HH
#define SHOW_INFORM_HH

#include <stdio.h>
#include "sp/seis_inform.hh"

class ShowInform : public SeisInform {

  public:
    ShowInform(SeisPlot *sp) : SeisInform(sp) {}

     virtual void preZoom(SeisPlot *sp, SeisZoomer *zoomer,
                          SeisPlot::ZoomDir dir)
          {printf("preZoom: sp= %x, zoomer= %x, dir= %d\n", sp, zoomer, dir);}
     virtual void postZoom(SeisPlot *sp, SeisPlot::ZoomDir dir)
          {printf("postZoom: sp= %x, dir= %d\n", sp, dir);}
     virtual void postZoomSeparateWindow(SeisPlot *sp, SeisPlot *zoomsp)
          {printf("postZoomSeparateWindow: sp= %x, zoomsp= %x\n", sp, zoomsp);}

     virtual void preScan(SeisPlot  *sp, SeisPlot::ScanDir dir  )
          {printf("preScan: sp= %x, dir= %d\n", sp, dir);}
     virtual void postScan(SeisPlot *sp, SeisPlot::ScanDir dir )
          {printf("postScan: sp= %x, dir= %d\n", sp, dir);}

     virtual void preMovie(SeisPlot *sp,  SeisPlot::MovieDir dir)
          {printf("preMovie: sp= %x, dir= %d\n", sp, dir);}
     virtual void postMovie(SeisPlot *sp, SeisPlot::MovieDir dir)
          {printf("postMovie: sp= %x, dir= %d\n", sp, dir);}

     virtual void preDataChange(SeisPlot *sp )
          {printf("preDataChange: sp= %x\n", sp);}
     virtual void prePlot(SeisPlot *sp )
          {printf("prePlot: sp= %x\n", sp);}
     virtual void newPlot(SeisPlot *sp )
          {printf("newPlot: sp= %x\n", sp);}
     virtual void noPlotDisplayed(SeisPlot *sp )
          {printf("noPlotDisplayed: sp= %x\n", sp);}

     virtual void addingTie(SeisPlotTie *sp, SeisPlot *tieplot )
          {printf("addingTie: sp= %x, tieplot= %x\n", sp, tieplot);}
     virtual void removingTie(SeisPlotTie *sp, SeisPlot *tieplot)
          {printf("removingTie: sp= %x, tieplot= %x\n", sp, tieplot);}

     virtual void dragImage(SeisPlot *sp)
          {printf("dragImage: sp= %x\n", sp);}

     virtual void unitChange(SeisPlot *sp, int units)
          {printf("unitChange: sp= %x, units= %d\n", sp, units);}

     virtual void expose(SeisPlot *sp, int x, int y, int width, int height)
          {printf("expose: sp= %x, x=%d, y=%d, width=%d, height=%d\n",
                                              sp, x,y,width,height); }
     virtual void destroyed(SeisPlot *sp )
          {printf("destroyed: sp= %x\n", sp); SeisInform::destroyed(sp);}

     virtual void colorChange(long num_allocated, long which_set)
          {printf("colorChange: num_allocated= %d, which_set= %d\n", 
                                num_allocated, which_set);}
};


#endif
