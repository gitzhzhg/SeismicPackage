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
#ifndef VELOCITY_HH
#define VELOCITY_HH


#include "sp/seis_plot.hh"
#include "sp/seis_inform.hh"
#include "vect/ll_seis_vect.hh"
#include "vect/vector.hh"
#include "vect/vect_data.hh"
#include "plot/pick_base.hh"

class SeisVelocity : public SeisInform {

   private:
     float              _velocity;
     float              _start_time;
     float              _end_time;
     int                _header;
     char               *_named_color;
     Boolean            _first_time;
     Boolean            _visible;
     Boolean            _use_external_offsets;
     Boolean            _do_hyper;
     short              _marker_length;
     SeisPlot           *_sp;
     class PickVelocity *_pick_velocity;
     
   public:
     SeisVelocity(SeisPlot *sp,
                  int header=5, char *color = "red",
                  Boolean use_external_offsets = False,
                  Boolean do_hyper = False, Boolean transient = False);
     virtual ~SeisVelocity();
     friend class PickVelocity;
     SeisVectLinkedList *_vect_ll;
     VectData           *_vect_data;
     Vector             *_V;
     void post(SeisPlot *sp);
     virtual void newPlot(SeisPlot *sp);
     virtual void dragImage(SeisPlot *sp);
     virtual void postMovie(SeisPlot *sp, SeisPlot::MovieDir dir);
     virtual void postZoom (SeisPlot *sp, SeisPlot::ZoomDir  dir);
     virtual void postScan (SeisPlot *sp, SeisPlot::ScanDir  dir);
     virtual void postZoomSeparateWindow(SeisPlot *sp, SeisPlot *zoomsp);
     virtual void removeVectors();
     void useExternalOffset(Boolean use) {_use_external_offsets = use;}
     void setExternalOffset(long trace_index, float external_offset);
     void velocityColor(char *color);
     float getVelocity() {return _velocity;}
     void loadHeaders(SeisPlot *sp);
     void makeVisible();
     void makeInvisible();
     void setHyper( Boolean do_hyper);
     long       _numpoints;
     Boolean    _can_post;
     long       _start_trace;
     long       _end_trace;
     float      *_xdata;
     float      *_ydata;
     float      *_hd;
     Boolean    _headers_modified;
     Boolean    _transient;
};

class PickVelocity : public PickBase {

      private:
           SeisVelocity *_sv;
           Boolean       _first_time;
           int           _mouse_type;
      public:
           PickVelocity(SeisPlot *sp, SeisVelocity *sv);
           virtual ~PickVelocity();           
           int  hyper(float start_time, float end_time, 
                      long  start,      long end,
                      float *xarray,    float *yarray);
           int linear(float start_time, float end_time, 
                      long  start,      long end,
                      float *xarray,    float *yarray, Boolean draw_now);
           SeisVectLinkedList *_pick_vect_ll;
           VectData           *_pick_vect_data;
           Vector             *_pick_V;
           float              *_pick_xdata;
           float              *_pick_ydata;
      protected:
           virtual void buttonAny(int x1, int x2, int y1, int y2, int button,
                                  Action action, Modifier modifier);
};

#endif










