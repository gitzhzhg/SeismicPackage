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
#include <stdlib.h>
#include <string.h>
#include <X11/Xlib.h>
#include <X11/Xutil.h>
#include <X11/Intrinsic.h>
#include <Xm/Xm.h>
#include "vu/seis_avast.hh"
#include "vu/seis_avast_data.hh"
#include "vect/vector.hh"
#include "cprim.h"
#include "exptilde_crou.h"

#define MIN(a,b)   (  (a) < (b) ? (a) :  (b)  )
#define MAX(a,b)   (  (a) > (b) ? (a) :  (b)  )



SeisAvast::SeisAvast( SeisPlot *sp, float angle, int header, char *color,
                    Boolean use_external_headers)
                  : SeisInform(sp), _sp(sp), _angle(angle), 
                    _header(header), _first_time(True), _vect_data(NULL), 
                    _use_external_headers(use_external_headers)
{
 Screen *scr;
 Display *dpy;

  dpy   = XtDisplay(sp->imageGraphic());
  scr   = DefaultScreenOfDisplay(dpy);

  if(color == NULL) 
    _named_color = "black";
  else
    _named_color = color;
  loadHeaders(sp);
  _headers_modified = False;
  _vect_ll = new SeisVectLinkedList();
  _vect_ll->addPlot(sp);
  _visible = False;
  _button = False;
  _avastfile[0] = '\0';
  _xdata     = NULL;
  _ydata     = NULL;
  _zdata     = NULL;
  _hd        = NULL;
  _xgrid     = NULL;
  _ygrid     = NULL;
  _vgrid     = NULL;
  _points    = NULL;
  _offset    = NULL;
  _time      = NULL;
  _vect_data = NULL;
}


SeisAvast::~SeisAvast()
{
  if (_xdata)     delete _xdata;     _xdata     = NULL;
  if (_ydata)     delete _ydata;     _ydata     = NULL;
  if (_zdata)     delete _zdata;     _zdata     = NULL;
  if (_hd)        delete _hd;        _hd        = NULL;
  if (_xgrid)     delete _xgrid;     _xgrid     = NULL;
  if (_ygrid)     delete _ygrid;     _ygrid     = NULL;
  if (_vgrid)     delete _vgrid;     _vgrid     = NULL;
  if (_points)    delete _points;    _points    = NULL;
  if (_offset)    delete _offset;    _offset    = NULL;
  if (_time)      delete _time;      _time      = NULL;
  if (_vect_data) delete _vect_data; _vect_data = NULL;
  delete _vect_ll;
}


//public method to set the marker color
void SeisAvast::avastColor(char *color) 
{
  if(_V == NULL) return;
  _named_color = color;
  _V->setColor(_named_color);
}


void SeisAvast::newPlot(SeisPlot *sp)
{
  if(!_visible) return;
  loadHeaders(sp);
  post(sp);
  if(_sop != NULL) _sop->newData();
}

void SeisAvast::postScan(SeisPlot *sp, SeisPlot::ScanDir)
{
  if(!_visible) return;
  loadHeaders(sp);
  post(sp);
  if(_sop != NULL) _sop->newData();
}

void SeisAvast::dragImage(SeisPlot *sp)
{
  if(!_visible) return;
  post(sp);
}

 
void SeisAvast::postMovie(SeisPlot *sp, SeisPlot::MovieDir)
{
  if(!_visible) return;
  post(sp);
}


void SeisAvast::postZoom(SeisPlot *sp, SeisPlot::ZoomDir)
{
  if(!_visible || _V == NULL) return;
  _V->setMarker(Vector::NoMarker, 
                ((unsigned int)((float)sp->traceWidth() * .80)), 2);
}


void SeisAvast::postZoomSeparateWindow(SeisPlot * /*sp*/, SeisPlot * /*zoomsp*/)
{

}


Boolean SeisAvast::setFilename(char *infile)
{
   Boolean succ =False;
   char    fname[300];

   _new_file = False;
   if (infile) {
      exptilde_crou2(fname, infile);

      if (strcmp(fname,_avastfile) != 0)  {
         strcpy(_avastfile,fname);
         succ=open_file(_avastfile);
         if (succ) _new_file = True;
         return(succ);
      }
      else
         succ=True;
   }
   return(succ);
}


Boolean SeisAvast::open_file(char *file_in)
{
  long i,j;
  char *ctmp, line[150];
  Boolean status = True;

  if( (_fileptr=fopen(file_in,"r")) == NULL ) {
     printf("File Open Failed\n");
     status = False;
     return(status);
  }

  i=fscanf(_fileptr,"### AVAST ANGLE TABLE %d %d %d %d\n",&_ndpt,&_nx,&_ny,&_nang);
  if (i != 4) {
     printf("This file is not a avast file\n");
     status = False;
     return(status);
  }

  i=fscanf(_fileptr, "### angles: %120[^\n]\n",line);
  if (i != 1) {
     printf("This file is not a avast file\n");
     status = False;
     return(status);
  }
  j=0;
  ctmp=strtok(line," ");
  while (ctmp != NULL) {
      sscanf(ctmp,"%f",&_ang[j]);
      j++;
      ctmp=strtok(NULL," ");
  }
  if (j != _nang) _nang = j;

  i=fscanf(_fileptr,"### offset, x, y headers:  %d  %d %d\n",&_nhz,&_nhx,&_nhy);
  if (i != 3) {
     printf("This file is not a avast file\n");
     status = False;
     return(status);
  }

  i=fscanf(_fileptr, "%120[^\n]\n",line);
  return(status);
}


Boolean SeisAvast::read_file()
{
  float in_line_tmp, in_line_last;
  float crossline_tmp, crossline_last;
  float time_tmp, offset_tmp;;
  float angle_tmp, angle_last;
  long ix,iy,ifirst,flag;
  int i;
  long ptotal, pcount = 0;
  Boolean status = True;

  ptotal = _nang * _nx * _ny * _ndpt;
  if (_xgrid  != NULL) delete _xgrid;
  if (_ygrid  != NULL) delete _ygrid;
  if (_points != NULL) delete _points;
  if (_vgrid  != NULL) delete _vgrid;
  _xgrid  = new float [_nx];
  _ygrid  = new float [_ny];
  _points = new AvastPointStruct [ptotal];
  _vgrid  = new AvastGridStruct [_nang * _nx *_ny];
  ix = 0;
  iy = 0;
  _vcount = 0;
  ifirst = True;
  i = 0;
  while (i != EOF && ix < _nx && iy < _ny && pcount < ptotal) {
    i=fscanf(_fileptr,"%f %f %f %f %f\n",&offset_tmp,&in_line_tmp,&crossline_tmp,&time_tmp,&angle_tmp);
    if (i == 5) {
       if (ifirst == True) {
          angle_last     = angle_tmp;
          in_line_last   = in_line_tmp;
          crossline_last = crossline_tmp;
          _vgrid[_vcount].angle = angle_tmp;
          _vgrid[_vcount].xgrid = in_line_tmp;
          _vgrid[_vcount].ygrid = crossline_tmp;
          _vgrid[_vcount].start = &(_points[pcount]);
          _xgrid[ix] = in_line_tmp;
          _ygrid[iy] = crossline_tmp;
          ifirst = False;
       }
       else if (angle_last != angle_tmp || in_line_last != in_line_tmp || crossline_last != crossline_tmp) {
          _vcount++;
          angle_last     = angle_tmp;
          in_line_last   = in_line_tmp;
          crossline_last = crossline_tmp;
          _vgrid[_vcount].angle = angle_tmp;
          _vgrid[_vcount].xgrid = in_line_tmp;
          _vgrid[_vcount].ygrid = crossline_tmp;
          _vgrid[_vcount].start = &(_points[pcount]);
          flag = False;
          for (i=0; i<ix+1; i++) {
              if (_xgrid[i] == in_line_tmp) flag = True;
          }
          if (flag == False) {
             ix++;
              _xgrid[ix] = in_line_tmp;
          }
          flag = False;
          for (i=0; i<iy+1; i++) {
              if (_ygrid[i] == crossline_tmp) flag = True;
          }
          if (flag == False) {
             iy++;
             _ygrid[iy] = crossline_tmp;
          }
       }
       _points[pcount].offset = offset_tmp;
       _points[pcount].time = time_tmp;
       pcount++;
    }
  }
  _vcount++;
  fclose(_fileptr);
  _new_file = False;
  return(status);
}

Boolean SeisAvast::checkForNewFile()
{
  Boolean status = False;

  if (_new_file) status = read_file();
  return(status);
}


long SeisAvast::setupInterp(float x, float y, float *xs, float *ys, float *facs)
{
  long i, i1, i2, j1, j2, nf;
  float fac1, fac2, fx1, fx2, fx3, fx4;
  float xfl, yfl;

  yfl = y;
  if (yfl < _ygrid[0])     yfl = _ygrid[0];
  if (yfl > _ygrid[_ny-1]) yfl = _ygrid[_ny-1];

  i1  = _ny-1;
  i2  = _ny-1;
  fac1 = 0.0;
  for (i=1; i<_ny; i++) {
      if (yfl < _ygrid[i]) {
         i1 = i - 1;
         i2 = i;
         break;
      }
  }
  if (i1 != i2) fac1 = (yfl - _ygrid[i1]) / (_ygrid[i2] - _ygrid[i1]);

  xfl = x;
  if (xfl < _xgrid[0])     xfl = _xgrid[0];
  if (xfl > _xgrid[_nx-1]) yfl = _xgrid[_nx-1];

  j1  = _nx-1;
  j2  = _nx-1;
  fac2 = 0.0;
  for (i=1; i<_nx; i++) {
      if (xfl < _xgrid[i]) {
         j1 = i - 1;
         j2 = i;
         break;
      }
  }
  if (j1 != j2) fac2 = (xfl - _xgrid[j1]) / (_xgrid[j2] - _xgrid[j1]);

  fx4 = fac1 * fac2;
  fx3 = fac1 - fx4;
  fx2 = fac2 - fx4;
  fx1 = 1.0  - fac1 - fx2;
  
  nf = 0;
  if (fx1 != 0.0) {
     facs[nf] = fx1;
     xs[nf] = _xgrid[j1];
     ys[nf] = _ygrid[i1];
     nf++;
  }
  if (fx2 != 0.0) {
     facs[nf] = fx2;
     xs[nf] = _xgrid[j2];
     ys[nf] = _ygrid[i1];
     nf++;
  }
  if (fx3 != 0.0) {
     facs[nf] = fx3;
     xs[nf] = _xgrid[j1];
     ys[nf] = _ygrid[i2];
     nf++;
  }
  if (fx4 != 0.0) {
     facs[nf] = fx4;
     xs[nf] = _xgrid[j2];
     ys[nf] = _ygrid[i2];
     nf++;
  }
  return(nf);
}


void SeisAvast::get_init()
{
  _ifirst = True;
  if (_offset != NULL) delete _offset;
  if (_time   != NULL) delete _time;
  _offset = new float [_ndpt];
  _time   = new float [_ndpt];
}


float SeisAvast::get(float o,float x, float y, float angle)
{
  long i, j, n, nf, flag;
  float a_tmp, x_tmp, y_tmp;
  float x_near, y_near;
  float xs[4], ys[4], facs[4];
  float *offset_tmp;
  float t;
  AvastPointStruct *point;
 
  if (_ifirst == True) {
     _ifirst = False;
     _a_last = angle;
     _x_last = x;
     _y_last = y;
     _nlast = 0;
     flag = True;
  }

  if (_a_last != angle || _x_last != x || _y_last != y || flag == True) {
     flag = False;
     nf = setupInterp(x, y, xs, ys, facs);
     for (i=0; i< _ndpt; i++) {
         _offset[i] = 0.0;
     }
     for (j=0; j<nf; j++) {
         offset_tmp = new float [_ndpt];
         i = 0;
         n = 0;
         while (i < _vcount) {
            a_tmp = _vgrid[i].angle;
            x_tmp = _vgrid[i].xgrid;
            y_tmp = _vgrid[i].ygrid;
            if (a_tmp == angle) {
               if (y_tmp == ys[j]) {
                  if (x_tmp == xs[j]) {
                     point = _vgrid[i].start;
                     offset_tmp[n] = point->offset;
                     _time[n] = point->time;
                     n = n + 1;
                     point++;
                     while (n < _ndpt) {
                        offset_tmp[n] = point->offset;
                        _time[n] = point->time;
                        n = n + 1;
                        point++;
                     }
                  }
               }
            }
            i++;
         }
         for (i=0; i< _ndpt; i++) {
             _offset[i] = _offset[i] + facs[j]*offset_tmp[i];
         }
     }
     delete offset_tmp;
     _x_last = x;
     _y_last = y;
     _nlast = 0;
  }
 
  t = -999.0;
  if (_nlast == 0) {
     while (o < _offset[_nlast]) {
       _nlast++;
       if (_nlast == _ndpt) {
          t = _time[_nlast - 1];
          return(t);
       }
     }
  }
  while (_nlast < _ndpt) {
    if (o == _offset[_nlast]) {
       t = _time[_nlast];
       _nlast++;
       return(t);
    }
    else if (_nlast == _ndpt-1) {
       if (o >= _offset[_nlast]) {
          t = _time[_nlast];
          _nlast++;
       }
       else {
          _nlast = 0;
       }
       return(t);
    }
    else if (o > _offset[_nlast] && o <= _offset[_nlast+1]) {
       t = (_time[_nlast+1]-_time[_nlast])/(_offset[_nlast+1]-_offset[_nlast])*(o-_offset[_nlast]) + _time[_nlast];
       _nlast++;
       return(t);
    }
    else {
       _nlast++;
    }
  }
  _nlast = 0;
  return(t);
 
}


void SeisAvast::supplyInfo(int nangles, float *angles, int *select)
{
  long i;

  _nang_toplot = 0;
  for (i=0; i<nangles; i++) {
      _select_flags[i] = False;
      if (select[i] == TRUE) {
         _select_flags[i] = True;
         _ang_toplot[_nang_toplot] = angles[i];
         _nang_toplot++;
      }
  }
}


void SeisAvast::getAngles(float *angles)
{
  long i;

  for (i=0; i<_nang; i++) {
      angles[i] = _ang[i];
  }
}


void SeisAvast::post(SeisPlot *sp)
{
  long i,j,k;
  float offset,xloc,yloc,ang;
  float time;
  long num_points = {0};
  long count;
  long start, end;
  static long old_numsegs = 0;
  static long num_segments;
  short marker_length;
  long location;
  long cmp_count;
  float xsav, ysav,osav;
  long xheader,yheader;
  float otmp;

//  if( !sp->imageIsDisplayed() || !_visible ) return;
  if( !sp->imageIsDisplayed() ) return;
  
  if(_nang_toplot == 0) return; 

  if(_hd == NULL) loadHeaders(sp);

  if(_hd == NULL) return;  

  if (_vect_data) {
     delete _vect_data;
     _vect_data = NULL;
  }
  if (_vect_ll) delete _vect_ll;
  _vect_ll = new SeisVectLinkedList();
  _vect_ll->addPlot(_sp);
  _visible = False;
  if (_vect_data == NULL)  _vect_data = new AvastData(_nang_toplot);

  if (_xdata) delete _xdata;
  if (_ydata) delete _ydata;
  if (_zdata) delete _zdata;

  _xdata = new float [(unsigned int)(sp->displayedTraces()) *25];
  _ydata = new float [(unsigned int)(sp->displayedTraces()) *25];
  _zdata = new float [(unsigned int)(sp->displayedTraces()) *25];

  if(_xdata == NULL || _ydata == NULL || _zdata == NULL) {
        printf("couldnt allocate x, y, or z arrays in SeisAvast::post\n");
        return;
  }

  for (k=0; k<_nang_toplot; k++) {

     num_points = (long)(sp->originalTraces()*25);

     start = (sp->firstTrace() - 1) + (sp->currentFrame() * sp->originalTraces());
     end   = (sp->firstTrace() - 1 + sp->displayedTraces())
           + (sp->currentFrame() * sp->originalTraces() );

     num_segments = 1;
     location = sp->firstTrace();
     count = sp->firstTrace();
     ang = _ang_toplot[k];
     get_init();
     get_headers(&xheader,&yheader);
     xsav = (float) _hd[start * sp->numHeaders() + (xheader-1)] ;
     ysav = (float) _hd[start * sp->numHeaders() + (yheader-1)] ;
     osav = -999.0;
     cmp_count = 1;
     for( i=start; i<end; i++) {
        xloc = (float) _hd[i * sp->numHeaders() + (xheader-1)] ;
        yloc = (float) _hd[i * sp->numHeaders() + (yheader-1)] ;
        if (xloc != xsav || yloc != ysav) {
           osav = -999.0;
           cmp_count++;
           xsav = xloc;
           ysav = yloc;
        }
        offset = (float) _hd[i * sp->numHeaders() + _header] ;
        if (osav != -999.0) {
           otmp = (offset - osav) / 10.0;  
           for (j=1; j<9; j++) {
               osav = osav + otmp;
               time = 1.0;
               while (time != -999.0) {
                 time = get(osav,xloc,yloc,ang);
                 if (time != -999.0) {
                    _xdata[count - 1] = location - 1.0 + (j * .1);
                    _ydata[count - 1] = time;
                    _zdata[count - 1] = (float)cmp_count;
                    count++;
                    if (count > num_points) break;
                    num_segments++;
                 }
                 if (count > num_points) break;
               }
           }
        }
        time = 1.0;
        while (time != -999.0) {
          time = get(offset,xloc,yloc,ang);
          if (time != -999.0) {
             _xdata[count - 1] = (float)location;
             _ydata[count - 1] = time;
             _zdata[count - 1] = (float)cmp_count;
             count++;
             if (count > num_points) break;
             num_segments++;
          }
          if (count > num_points) break;
        }
        osav = offset;
        location++;
        if (count > num_points) break;
     }
     num_segments--;
     avast_sort(num_segments,_zdata,_ydata,_xdata);

     _first_time = False;
     marker_length = (int)((float)sp->traceWidth() * .80);
     _vect_data->insert((int)(num_segments),_xdata,_ydata,k);
     _V = _vect_ll->add(_vect_data,k,_named_color,2);

  }
  makeVisible();
  old_numsegs = num_segments;
}


void SeisAvast::loadHeaders(SeisPlot *sp)
{
 long headersize;
 long i;
 long images; 


  if(sp == NULL) sp = _sp;
  const float *sphd = sp->headers();
  if(!sp->imageIsDisplayed() || !_visible )return;


  images = sp->movie() ? sp->frames() : 1;

  headersize =  sp->numHeaders() * (2*sizeof(double));
  headersize += (sp->plottedNplt() *images * headersize);
  
  if (_hd) delete _hd;
  _hd = new float [(unsigned int)headersize];

  if(_hd == NULL) {printf("error in loadheaders allocation\n");return;}

  if(!_use_external_headers)
    for(i=0; i < (sp->numHeaders()*sp->plottedNplt()*images); i++)
        _hd[i] = sphd[i];
}


void SeisAvast::makeVisible()
{
  if (_vect_ll != NULL) {
    _vect_ll->makeVisible();
    _visible = True;
  }
}


void SeisAvast::makeInvisible()
{
  if (_vect_ll != NULL) {
     _vect_ll->makeInvisible();
     _visible = False;
  }
}


void SeisAvast::setAvastButtonStatus(Boolean button)
{
   _button = button;
}


Boolean SeisAvast::getAvastButtonStatus()
{
   return(_button);
}


void SeisAvast::avastEnd(SeisPlot *sp)
{
  if (_vect_data) {
     delete _vect_data;
     _vect_data = NULL;
  }
  if (_vect_ll) delete _vect_ll;
  _vect_ll = new SeisVectLinkedList();
  _vect_ll->addPlot(sp);
  _visible = False;
  _button = False;
}


void SeisAvast::setExternalHeaders(long trace_index, long header_index,
                                  float external_header)
{
static long old_num_traces = 0;
long images = _sp->movie() ? _sp->frames() : 1;


  _use_external_headers = True; 

  if(!_sp->imageIsDisplayed() || _sp->plottedNplt() * images < trace_index + 1) 
     return;  

  //allocate array if needed
  if(_hd == NULL || old_num_traces != _sp->plottedNplt() * images) {
    _visible = True;
    loadHeaders( (SeisPlot *)NULL );
    old_num_traces = _sp->plottedNplt() * images;
  }


  _hd[trace_index * _sp->numHeaders() + header_index] = external_header;
}

int SeisAvast::avast_fun(void *data, long lo, long up)
{
  FloatFloatFloat *st = (FloatFloatFloat*)data;
  float ytemp, xtemp;
  float rtemp;
  long lo2 = lo - 1;
  long up2 = up - 1;
 
  if(st->x[lo2] > st->x[up2] ||
     (st->x[lo2] == st->x[up2] && st->y && st->y[lo2] > st->y[up2]))
     {
     xtemp = st->x[lo2];  st->x[lo2] = st->x[up2];  st->x[up2] = xtemp;
    if(st->y)
   { ytemp = st->y[lo2];  st->y[lo2] = st->y[up2];  st->y[up2] = ytemp;
}
     if(st->r)
   { rtemp = st->r[lo2];  st->r[lo2] = st->r[up2];  st->r[up2] = rtemp;
}
     return TRUE;
     }
  return FALSE;
}
 
 
void SeisAvast::avast_sort(long nsort, float *x, float *y, float *r)
{
  FloatFloatFloat data;
 
  data.x = x;
  data.y = y;
  data.r = r;
  generic_sort(nsort, avast_fun, &data);
}

