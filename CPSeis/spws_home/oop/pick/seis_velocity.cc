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
// author Michael L. Sherrill 05/95
// creates velocity picking class to be used with SeisPlot

#include <stdlib.h>
#include <X11/Xlib.h>
#include <X11/Xutil.h>
#include <X11/Intrinsic.h>
#include <Xm/Xm.h>
#include "pick/seis_velocity.hh"
#include "vect/vector.hh"
#include "vect/vect_data.hh"
#include "minimax.h"

static char *picking_mode = "Mode: Determine Velocity";
static const char * const help_token = "VELOCITY";
static char *velocityhelp= "mouse*VELOCITY:  BTN#1: Select trace range, \
BTN#2: Delete velocity vector, BTN#3: None";

SeisVelocity::SeisVelocity(SeisPlot *sp,int header,char *color,
                           Boolean use_external_offsets, 
                           Boolean do_hyper,
                           Boolean transient)
                  : SeisInform(sp), _sp(sp),
                    _header(header), _first_time(True), _vect_data(NULL), 
                    _use_external_offsets(use_external_offsets),
                    _do_hyper(do_hyper),_transient(transient)
{
 Screen *scr;
 Display *dpy;

  dpy   = XtDisplay(sp->imageGraphic());
  scr   = DefaultScreenOfDisplay(dpy);

  if(color == NULL) 
    _named_color = "black";
  else
    _named_color = color;
  _hd = _xdata = _ydata = NULL;
  loadHeaders(sp);
  _headers_modified = False;
  _vect_ll = new SeisVectLinkedList();
  _vect_ll->addPlot(sp);
  _visible = False;
  _pick_velocity = new PickVelocity(_sp,this);
  _start_trace = _end_trace = 0;

}


SeisVelocity::~SeisVelocity()
{
  if (_xdata) free(_xdata); _xdata = NULL;
  if (_ydata) free(_ydata); _ydata = NULL;
  if (_hd)    free(_hd);    _hd    = NULL;
  Vector *ptr;
  void *p;
  for (ptr = _vect_ll->top(&p); ptr; ptr = _vect_ll->next(&p))
    delete ptr->getData();
  delete _vect_ll;
  //if (_vect_data) delete _vect_data;
  if (_pick_velocity) delete(_pick_velocity);
}


//public method to set the marker color
void SeisVelocity::velocityColor(char *color) 
{
  if(_V == NULL) return;
  _named_color = color;
  _V->setColor(_named_color);
}

void SeisVelocity::removeVectors()
{
  if(_vect_ll == NULL) return;
  Vector *ptr;
  void *p;
  for(ptr = _vect_ll->top(&p); ptr; ptr = _vect_ll->next(&p))
    {
    delete ptr->getData();
    _vect_ll->remove(ptr);
    }
}

void SeisVelocity::newPlot(SeisPlot *sp)
{
  removeVectors();
  loadHeaders(sp);
}

void SeisVelocity::postScan(SeisPlot *sp, SeisPlot::ScanDir)
{
  removeVectors();
  loadHeaders(sp);
}

void SeisVelocity::dragImage(SeisPlot *sp)
{
  if(!_visible) return;
  post(sp);
}

 
void SeisVelocity::postMovie(SeisPlot *  /*sp*/, SeisPlot::MovieDir)
{
  removeVectors();
}


void SeisVelocity::postZoom(SeisPlot * /*sp*/, SeisPlot::ZoomDir)
{
  //if(!_visible || _V == NULL) return;
  //_V->setMarker(Vector::HorizontalLineMarker, 
  //              ((unsigned int)((float)sp->traceWidth() * .80)), 2);
}


void SeisVelocity::postZoomSeparateWindow(SeisPlot * /*sp*/,
                                          SeisPlot * /*zoomsp*/)
{
  // Currently not using this since it will cause the unzoomed image
  // markers to be large also. Would have to maintain 2 sets of vectors.
  //
  //if(!_visible || _V == NULL) return;
  //_V->setMarker(Vector::HorizontalLineMarker,
  //              ((unsigned int)((float)zoomsp->traceWidth() * .80)), 2);
}


void SeisVelocity::post(SeisPlot *sp)
{
  long start, end;
  static long old_numsegs = 0;
  static long num_segments;

  if( !sp->imageIsDisplayed() || !_visible || !_can_post) return;
  
  if(_hd == NULL) loadHeaders(sp);

  if(_hd == NULL) return;  

  if(_xdata == NULL)
     {
     _xdata = (float *)calloc(1,(unsigned int)(sp->displayedTraces()
                                 *sizeof(float)));
     _ydata = (float *)calloc(1,(unsigned int)(sp->displayedTraces()
                                 *sizeof(float)));
     }
  else if(old_numsegs != sp->displayedTraces()){
     _xdata  =(float *)realloc(_xdata,(unsigned int)(sp->displayedTraces()
                               *sizeof(float)));
     _ydata  =(float *)realloc(_ydata,(unsigned int)(sp->displayedTraces()
                               *sizeof(float)));
     }

  if(_xdata == NULL || _ydata == NULL)
     {
     printf("couldnt allocate x or y arrays in SeisVelocity::post\n");
     return;
     }

  start = (_start_trace - 1) + (sp->currentFrame() * sp->originalTraces());
  end   = (_end_trace - 1  ) + (sp->currentFrame() * sp->originalTraces() );

  if(_do_hyper)
    num_segments = _pick_velocity->hyper(_start_time,_end_time,start,end,
                                         &_xdata[0],&_ydata[0]);
  else
    num_segments = _pick_velocity->linear(_start_time,_end_time,start,end,
                                          &_xdata[0],&_ydata[0],True);
  if(_first_time)
     {
     _first_time = False;
     _marker_length = (int)(_sp->traceWidth());
     }


  _vect_data = new VectData((int)(num_segments),_xdata,_ydata);
  _V = _vect_ll->add(_vect_data,_named_color,2,False,Vector::SolidLine,
                        Vector::NoMarker,_marker_length,2);

  makeVisible();
  old_numsegs = num_segments;
}


void SeisVelocity::loadHeaders(SeisPlot *sp)
{
 long headersize;
 long i;
 const float *sphd = sp->headers(); 
 long images; 


  if(!sp->imageIsDisplayed())return;

  images = sp->movie() ? sp->frames() : 1;

  headersize =
          max( sp->numHeaders(),(PlotImage::MAXHDRS *(2*sizeof(double))) );
  headersize += (sp->plottedNplt() * images * headersize);
  
  if(_hd == NULL)
    _hd = (float *) calloc( 1, (unsigned int)headersize);
  else
    _hd  = (float *) realloc( _hd,  (unsigned int)headersize );

  if(_hd == NULL){printf("error in loadheaders allocation\n");return;}

  for(i=0; i < (sp->numHeaders()*sp->plottedNplt()*images); i++)
    {
    _hd[i] = sphd[i];
    if(_hd[i] < 0.0) _hd[i] = (-_hd[i]);//Change negative offsets to positive
    }


}

void SeisVelocity::makeVisible()
{
 _vect_ll->makeVisible();
 _visible = True;
}


void SeisVelocity::makeInvisible()
{
 _vect_ll->makeInvisible();
 _visible = False;
}


void SeisVelocity::setExternalOffset(long trace_index, float external_offset)
{

  if(!_sp->imageIsDisplayed() || _sp->plottedNplt() < trace_index + 1 ) 
     return;  
  _use_external_offsets = True; 
  _hd[trace_index * _sp->numHeaders() + _header] = external_offset;
  
}


void SeisVelocity::setHyper( Boolean do_hyper)
{
//may need to do other things here
  _do_hyper = do_hyper;
}






//******************** SeisVelocity's PickBase ***************************

PickVelocity::PickVelocity (SeisPlot    *sp, SeisVelocity *sv)
                    : PickBase(sp, picking_mode, help_token, velocityhelp,
                               XC_tcross, allow, True),_sv(sv)
{
  _sv->_start_trace = _sv->_end_trace = 0;
  _pick_vect_data = NULL; 
  _pick_V = NULL;
  _pick_xdata = _pick_ydata = NULL;
  _first_time = True;
  _pick_vect_ll = new SeisVectLinkedList();
  _pick_vect_ll->addPlot(sp);
  _mouse_type = sp->getLocationOutputType();
  sp->setLocationOutputType(PlotImage::MOUSE_AUX,"Velocity:",0.0);
}


PickVelocity::~PickVelocity()
{
  if(_pick_xdata)     free(_pick_xdata); _pick_xdata = NULL;
  if(_pick_ydata)     free(_pick_ydata); _pick_ydata = NULL;
  if(_pick_V)         _pick_vect_ll->remove(_pick_V);
  if(_pick_vect_ll)   delete _pick_vect_ll;
  if(_pick_vect_data) delete _pick_vect_data;
  _sv->_sp->setLocationOutputType(_mouse_type,"",0.0);
}


int PickVelocity::hyper(float start_time, float end_time, 
                         long  start,      long end,
                         float *xarray,    float *yarray)
{
 double t1sq, t2sq, o1sq, o2sq;
 double o1,o2;
 double vsq, osq, tp;
 double temp_val;
 long i,j,k,l;
 Boolean incrementing = True;
 int num_segments;
 const float *sphd = _sv->_sp->headers();
 float offset_diff;


    if(start_time == end_time)return(0);
    t1sq = start_time * start_time;
    t2sq = end_time * end_time;
    if(t1sq > t2sq)
      {
      temp_val = t1sq;
      t1sq = t2sq;
      t2sq = temp_val;
      }

    o1 = _sv->_hd[start * _sv->_sp->numHeaders() + _sv->_header];
    o2 = _sv->_hd[end   * _sv->_sp->numHeaders() + _sv->_header];
    if( max(o1,o2) - min(o1,o2) <= 1.0 ) return(0);
    o1sq = o1 * o1;
    o2sq = o2 * o2;
    if(o1sq > o2sq)
      {
      temp_val = o1sq;
      o1sq = o2sq;
      o2sq = temp_val;
      }
    temp_val = sqrt((o2sq - o1sq) / (t2sq - t1sq));
    vsq = temp_val * temp_val;
    if(vsq <= 0)return(0);
    _sv->_velocity = temp_val;
    _sv->_sp->setLocationOutputType(PlotImage::MOUSE_AUX,"Velocity:",temp_val);
    tp = max( t1sq - o1sq / vsq, 0.0);

    k = _sv->_sp->numHeaders();
    l = _sv->_header;

    offset_diff = sphd[end*k+l] - sphd[start*k+l];
    if(offset_diff < 0.0)incrementing = False;

    num_segments = 0;
    for( i=start, j=0; i<=end; i++,j++) 
      {
      xarray[j] = (float)i + 1.0;
      if(j == 0)
	{
        if(incrementing)
          yarray[j] = min(start_time,end_time);
        else
          yarray[j] = max(start_time,end_time);
	}
      else
	{
        osq = sphd[i*_sv->_sp->numHeaders()+_sv->_header] 
            * sphd[i*_sv->_sp->numHeaders()+_sv->_header];
        yarray[j] = sqrt(tp + osq / vsq);
	}
      num_segments++;
      }

    return(num_segments);
}


int PickVelocity::linear(float start_time, float end_time, 
                         long  start,      long end,
                         float *xarray,    float *yarray,
                         Boolean draw_now)
{
 float o1,o2,t1,t2;
 float temp_val;
 float offset_diff;
 float time_diff;
 long i,j,k,l;
 Boolean incrementing = True;
 int num_segments;
 const float *sphd = _sv->_sp->headers();
 
    t1 = start_time;
    t2 = end_time;
    if(t1 == t2)return(0);
    if(t1 > t2)
      {
      temp_val = t1;
      t1 = t2;
      t2 = temp_val;
      }

    o1 = _sv->_hd[start * _sv->_sp->numHeaders() + _sv->_header]; 
    o2 = _sv->_hd[end * _sv->_sp->numHeaders() + _sv->_header];
    if( max(o1,o2) - min(o1,o2) <= 1.0 ) return(0);
    if(o1 > o2)
      {
      temp_val = o1;
      o1 = o2;
      o2 = temp_val;
      }

    temp_val = (o2 - o1) / (t2 - t1);
    if(temp_val <= 0.0)return(0);
    _sv->_velocity = temp_val;
    _sv->_sp->setLocationOutputType(PlotImage::MOUSE_AUX,"Velocity:",temp_val);
    if(!draw_now) //we only wanted to compute velocity 
      {
      num_segments = 2;
      return(num_segments);
      }

    num_segments = 0;
    k = _sv->_sp->numHeaders();
    l = _sv->_header;

    offset_diff = sphd[(start+1)*k+l] - sphd[start*k+l];
    if(offset_diff < 0.0)incrementing = False;
    for( i=start, j=0; i<=end; i++,j++) 
      {
      xarray[j] = (float)i + 1.0;
      if(j == 0)
	{
        if(incrementing)
          yarray[j] = start_time;
        else
          yarray[j] = end_time;
	}
      else
	{
        offset_diff = sphd[i*k+l] - sphd[start*k+l];
        time_diff = offset_diff / _sv->_velocity;
        if(time_diff > 0.0)
          yarray[j] = start_time + time_diff;
        else
          yarray[j] = end_time + time_diff;
	}
      num_segments++;
      }

    return(num_segments);
}


void PickVelocity::buttonAny(int x1, int x2, int y1, 
                         int y2, int button, Action action, 
                         Modifier /*modifier*/)
{
 long current_trace;
 int marker_length;
 double temp_val;
 long temp_long;
 static int old_numsegs;
 int num_segments;
 static long start, end;

  if(!_sv->_sp->imageIsDisplayed())return;


  if(button == 1 && action == press)
    {
    current_trace = (long)(_sv->_sp->xWC(x1) + .5);
    if(current_trace < 1) current_trace = 1;
    if(current_trace > _sv->_sp->plottedNplt()) 
       current_trace = _sv->_sp->plottedNplt(); 
    _sv->_start_trace = current_trace;
    _sv->_start_time = _sv->_sp->yWC(y1);
    _sv->_can_post = True;
    }


  if(button == 1 && action == motion)
    {
    if(_pick_xdata == NULL)
     {
     _pick_xdata = (float *)calloc(1,(unsigned int)(_sv->_sp->displayedTraces()
                                 *sizeof(float)));
     _pick_ydata = (float *)calloc(1,(unsigned int)(_sv->_sp->displayedTraces()
                                 *sizeof(float)));
     }
   else if(old_numsegs != _sv->_sp->displayedTraces()){
     _pick_xdata =(float *)realloc(_pick_xdata,
                                   (unsigned int)(_sv->_sp->displayedTraces()
                                    *sizeof(float)));
     _pick_ydata =(float *)realloc(_pick_ydata,
                                   (unsigned int)(_sv->_sp->displayedTraces()
                                    *sizeof(float)));
     }
   if(_pick_xdata == NULL || _pick_ydata == NULL)
     {
     printf("couldnt allocate x or y arrays in PickVelocity::buttonAny\n");
     return;
     }

    start = ( ((int)(_sv->_sp->xWC(x1)+.5)) - 1) + (_sv->_sp->currentFrame() 
             * _sv->_sp->originalTraces());
    end =   ( ((int)(_sv->_sp->xWC(x2)+.5)) - 1) + (_sv->_sp->currentFrame() 
             * _sv->_sp->originalTraces());
    if(start == end)return;
    if(start < 0)
       start = 0;
    if(start > (_sv->_sp->currentFrame()+1)*_sv->_sp->originalTraces() - 1)
       start = (_sv->_sp->currentFrame()+1)*_sv->_sp->originalTraces() - 1;
    if(end   < 0)
       end   = 0;
    if(end   > (_sv->_sp->currentFrame()+1)*_sv->_sp->originalTraces() - 1)
       end   = (_sv->_sp->currentFrame()+1)*_sv->_sp->originalTraces() - 1;
    if(start > end)
      {
      temp_long = start;
      start = end;
      end = temp_long;
      } 

    _sv->_end_time = _sv->_sp->yWC(y2);
    if(_sv->_do_hyper)
      {
      num_segments = hyper(_sv->_start_time,_sv->_end_time,start,end,
                           &_pick_xdata[0],&_pick_ydata[0]);
      if(!num_segments)return;
      }
    else
      {
      num_segments = linear(_sv->_start_time,_sv->_end_time,start,end,
                           &_pick_xdata[0],&_pick_ydata[0],False);
      if(!num_segments)return;
      _pick_xdata[0] = _sv->_sp->xWC(x1) + .5;
      _pick_xdata[1] = _sv->_sp->xWC(x2) + .5;
      _pick_ydata[0] = _sv->_sp->yWC(y1);
      _pick_ydata[1] = _sv->_sp->yWC(y2);
      }

    if(_first_time || _pick_vect_ll == NULL)
      {
      //if(!_first_time)_sv->makeInvisible();
      _first_time = False;
      marker_length = (int)(_sv->_sp->traceWidth());
      _pick_vect_data=new VectData((int)(num_segments),_pick_xdata,_pick_ydata);
      if(_pick_vect_ll == NULL)
	{
        _pick_vect_ll = new SeisVectLinkedList();
        _pick_vect_ll->addPlot(_sv->_sp);
	}
      _pick_V = _pick_vect_ll->add(_pick_vect_data,_sv->_named_color,2,
                                   True,Vector::SolidLine,
                                   Vector::NoMarker,marker_length,2);
      }   
    else
      {
      _pick_vect_data->replace(0, _pick_vect_data->getNumPts(), 
                           (int)num_segments,&_pick_xdata[0],
                           &_pick_ydata[0]);
      }
    old_numsegs = num_segments;
    }




  if(button == 1 && action == release)
     {
     _sv->_start_trace = start + 1;
     _sv->_end_trace   = end + 1;
     if(_sv->_start_trace == _sv->_end_trace)
       {
       _sv->_can_post = False;
       if(_sv->_transient)
	 {
         _sv->_pick_velocity = NULL;
         delete this;
	 }
       return;
       }

     if(_sv->_start_time == _sv->_end_time) _sv->_end_time += .0001;
     if(_sv->_start_time > _sv->_end_time)
       {
       temp_val = _sv->_start_time;
       _sv->_start_time = _sv->_end_time;
       _sv->_end_time = temp_val;
       }

     if(!_sv->_transient)
       {
       _sv->_can_post = True;
       _sv->makeVisible();
       if(_pick_V) _pick_vect_ll->remove(_pick_V);
       if(_pick_vect_ll) delete _pick_vect_ll;
       if(_pick_vect_data)delete _pick_vect_data;
       _pick_vect_ll = NULL;
       _pick_vect_data = NULL;
       _pick_V = NULL;
       _sv->post(_sv->_sp);
       }
     else
       {
       _sv->_pick_velocity = NULL;  
       delete this;
       }
     }



  if(button == 2 && action == press )//delete closest vector
    {
    if(_sv->_vect_ll == NULL)return;
    Vector *vptr = _sv->_vect_ll->closest(x2,y2,_sv->_sp);
    if(vptr == NULL)return;
    delete vptr->getData();
    _sv->_vect_ll->remove(vptr);
    //_sv->_can_post = False;
    //if(_sv->_transient)
    //  {
    //  _sv->_pick_velocity = NULL;  
    //  delete this;
    //  }
    }

}
