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
#ifndef OVJD_HH
#define OVJD_HH


#include "sp/seis_plot.hh"
#include "sp/seis_inform.hh"
#include "seis_ovjd_pop.hh"
#include "vect/ll_seis_vect.hh"
#include "vect/vector.hh"
#include "vect/vect_data.hh"

class SeisOvjdPop;


class SeisOvjd : public SeisInform {

   private:
     float              _velocity;
     int                _header;
     char               *_named_color;
     Boolean            _first_time;
     Boolean            _visible;
     Boolean            _use_external_headers;
     SeisPlot           *_current_sp;

   public:
     SeisOvjd(SeisPlot *sp, float velocity=5000.0,
                int header=5, char *color = "red",
                Boolean use_external_headers = False);
     virtual ~SeisOvjd();
     SeisVectLinkedList *_vect_ll;
     VectData           *_vect_data;
     Vector             *_V;
     void post(SeisPlot *sp = NULL);

     virtual void newPlot(SeisPlot *sp);
     virtual void dragImage(SeisPlot *sp);
     virtual void postMovie(SeisPlot *sp, SeisPlot::MovieDir dir);
     virtual void postZoom (SeisPlot *sp, SeisPlot::ZoomDir  dir);
     virtual void postScan (SeisPlot *sp, SeisPlot::ScanDir  dir);
     virtual void postZoomSeparateWindow(SeisPlot *sp, SeisPlot *zoomsp);
     virtual void notCurrentInWindow(SeisPlot *sp);

     void useExternalHeaders(Boolean use) {_use_external_headers = use;}
     void setExternalHeaders(long trace_index, long header_index,
                             float external_header);
     void ovjdColor(char *color);
     void changeVelocity(float v) {_velocity = v;}
     float getVelocity() {return _velocity;}
     void loadHeaders(SeisPlot *sp = NULL);
     void makeVisible();
     void makeInvisible();
     SeisPlot *getCurrentSP(){ return _current_sp;}
     SeisOvjdPop *_sop;
     long       _numpoints;
     float      *_xdata;
     float      *_ydata;
     float      *_hd;
     Boolean    _headers_modified;
};



#endif










