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
#ifndef AVAST_HH
#define AVAST_HH


#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "sp/seis_plot.hh"
#include "sp/seis_inform.hh"
#include "vu/seis_avast_pop.hh"
#include "vect/ll_seis_vect.hh"
#include "vect/vector.hh"
#include "vu/seis_avast_data.hh"


typedef struct AvastPoint {
               float offset;
               float time;
               } AvastPointStruct;

typedef struct AvastGrid {
               float angle;
               float xgrid;
               float ygrid;
               AvastPointStruct *start;
               } AvastGridStruct;

typedef struct _FloatFloatFloat
  {
  float *x;
  float *y;
  float  *r;
  } FloatFloatFloat;


class SeisAvastPop;


class SeisAvast : public SeisInform {

   private:
     float              _angle;
     long               _ndpt;
     long               _nx;
     long               _ny;
     long               _nang;
     float              _ang[20];
     long               _nang_toplot;
     long               _nang_lastplot;
     float              _ang_toplot[20];
     Boolean            _select_flags[20];
     long               _nhx;
     long               _nhy;
     long               _nhz;
     float              *_xgrid;
     float              *_ygrid;
     AvastPointStruct   *_points;
     AvastGridStruct    *_vgrid;
     long               _vcount;
     float              _a_last;
     float              _x_last;
     float              _y_last;
     long               _nlast;
     long               _ifirst;
     float              *_offset;
     float              *_time;
     char               _avastfile[300];
     FILE               *_fileptr;
     int                _header;
     char               *_named_color;
     Boolean            _button;
     Boolean            _first_time;
     Boolean            _visible;
     Boolean            _use_external_headers;
     SeisPlot           *_sp;
     Boolean            _new_file;
     Boolean open_file(char *avastfile);
     Boolean read_file();
     static int avast_fun(void *data, long lo, long up);
     void avast_sort(long nsort, float *x, float *y, float *r);
     long setupInterp(float x, float y, float *xs, float *ys, float *facs);

   public:
     SeisAvast(SeisPlot *sp, float angle=30.0,
                int header=5, char *color = "red",
                Boolean use_external_headers = False);
     virtual ~SeisAvast();
     SeisVectLinkedList *_vect_ll;
     AvastData          *_vect_data;
     Vector             *_V;
     void post(SeisPlot *sp);
     virtual void newPlot(SeisPlot *sp);
     virtual void dragImage(SeisPlot *sp);
     virtual void postMovie(SeisPlot *sp, SeisPlot::MovieDir dir);
     virtual void postZoom (SeisPlot *sp, SeisPlot::ZoomDir  dir);
     virtual void postScan (SeisPlot *sp, SeisPlot::ScanDir  dir);
     virtual void postZoomSeparateWindow(SeisPlot *sp, SeisPlot *zoomsp);

     void useExternalHeaders(Boolean use) {_use_external_headers = use;}
     void setExternalHeaders(long trace_index, long header_index,
                             float external_header);
     void avastColor(char *color);
     void changeAngle(float v) {_angle = v;}
     float getAngle() {return _angle;}
     void getAngles(float *v);
     long getAngleCount() {return _nang;}
     Boolean setFilename(char *infile);
     void changeAvastFile(char *v) {strcpy(_avastfile,v);}
     char *getAvastFile() {return _avastfile;}
     Boolean checkForNewFile();
     void get_init();
     float get(float o,float x, float y, float angle);
     void get_headers(long *hx, long *hy) { *hx = _nhx; *hy = _nhy;}
     void supplyInfo(int nangles, float *angles, int *select);
     void loadHeaders(SeisPlot *sp);
     void makeVisible();
     void makeInvisible();
     void avastEnd(SeisPlot *sp);
     void setAvastButtonStatus(Boolean button);
     Boolean getAvastButtonStatus();
     SeisAvastPop *_sop;
     long       _numpoints;
     float      *_xdata;
     float      *_ydata;
     float      *_zdata;
     float      *_hd;
     Boolean    _headers_modified;
};



#endif

