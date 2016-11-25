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
//class that controls the zooming of an image
#ifndef SEISZOOMER_H
#define SEISZOOMER_H

#define SpArea 1
#define SpPointer 2
#define SpPointerTie 3
#define ZoomerAbort 1
#define ZoomerFail  2
#define ZoomerOk    3

#include "sp/seis_plot.hh"
#include "sp/seis_panner.hh"
#include "sp/seis_plot_zoom.hh"
#include "plot/pick_base.hh"

class SeisPlot;
class SeisPlotZoom;

class SeisZoomer : public PickBase {

  private:
                                  
  protected:     
        float                  _zoom_factor;
        int                    _zoom_stat;
        int                    _zoom_type;
        long                   _zoom_mode;
        Boolean                _doing_zoom;
        Boolean                _separate_window;
        int                    _box_size;
        GC                     _zoom_gc;
        char                   _errstr[200];
        struct PlotImage       *_ip;
        SeisPlot               *_sp;
        SeisPlot               *_tiesp;
        SeisPlot               *_zsp; 
        SeisPlotZoom           *_zoomsp;
        void DoZoom( Widget w, XEvent *event, char *mode[]);
        int processZoomAll(SeisPlot *sp, 
                           int       mode, 
                           int       x1, 
                           int       y1, 
                           int       x2, 
                           int       y2);
        
  public:
        SeisZoomer( SeisPlot *sp, 
                    SeisPlotZoom *zoomsp,
                    float _zoom_factor = 2.0, 
                    int _zoom_type     = SpPointer);
        virtual ~SeisZoomer();
        void setTiePlot(SeisPlot *sp= NULL);
        void setPixelSize(int box_size);       //sets size of rectangle to zoom
        void setZoomFactor(float zoom_factor); //sets zoom factor to use
        void setZoomWindow(SeisPlot *zsp);     //sets alternate drawable
        void startZoom();                      //installs zoom
        void zoomDown();                       //zooms down
        void zoomOriginal();                   //goes back to original size
        void zoomAbort();                      //abort the zoom
        virtual void buttonAny(int x1, int x2, int y1, int y2, int button,
                               Action action, Modifier modifier);
        virtual void manageZoomWindow();
        virtual void zoomComplete(SeisPlot *, 
                                  SeisPlot *, 
                                  SeisPlot::ZoomDir,
                                  int){};
};

#endif




