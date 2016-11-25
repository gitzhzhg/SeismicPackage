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
//class that creates an underlay image
#ifndef SEISUNDER_H
#define SEISUNDER_H

#include "sp/seis_plot.hh"
#include "sp/seis_inform.hh"

class SeisDrag;
class SeisPlotUnderInform;

class SeisPlotUnder : public SeisPlot {

   private:

   protected:
       SeisPlot            *_sp;
       SeisDrag            *_sdi;
       int                 _dest_x;
       int                 _dest_y;
       int                 _do_drag;
       SeisPlotUnderInform *_under_inform;

       virtual void displayPixmap();

   public:
       SeisPlotUnder( SeisPlot          *sp,
                      int               ptype = PlotImage::PlotCOLOR,
                      int               do_multi_plane = True);
       virtual ~SeisPlotUnder();
       virtual SeisPlot& operator=(SeisPlot& sp);
       long load_under();
       virtual int plot();
       virtual int scan(ScanDir dir);
       virtual void expose( Widget, XtPointer, XmDrawingAreaCallbackStruct*);
       virtual void getVisibleArea(int *x, int *y, int *width, int *height);
       virtual Boolean isCurrentInWindow();
       int  plotArrayTypeToSeismic(int header_number);
       void plotGridToSeismic(int header_number);
       void plotSeismicToGrid(int header_number);
       void setReadOut(Boolean underlay_only);
       void drag();
       void hide();
       void show();
       SeisPlot *overlaySP();
       Boolean plotExist(){return _image.getCurrentPixmapIndex()
                           ? True : False;}
};


class SeisPlotUnderInform : public SeisInform {

 private:
      SeisPlot      *_sp;
      SeisPlotUnder *_undersp;
  
 public:
      SeisPlotUnderInform(SeisPlot *sp, SeisPlotUnder *undersp) 
                          : SeisInform(sp),_sp(sp),_undersp(undersp){};
      virtual ~SeisPlotUnderInform();
      virtual void newPlot(SeisPlot *sp);
};

#endif


