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
// author Michael L. Sherrill 10/93
// creates offset velocity picking class to be used with SeisPlot

#include <stdlib.h>
#include <X11/Xlib.h>
#include <X11/Xutil.h>
#include <X11/Intrinsic.h>
#include <Xm/Xm.h>
#include "vu/seis_ovjd.hh"
#include "vect/vector.hh"
#include "vect/vect_data.hh"


SeisOvjd::SeisOvjd( SeisPlot *sp, float velocity, int header, char *color,
                    Boolean use_external_headers)
                  : SeisInform(sp), _velocity(velocity), 
                    _header(header), _first_time(True), _vect_data(NULL), 
                    _use_external_headers(use_external_headers),
                    _current_sp(sp)
        
{
 Screen *scr;
 Display *dpy;

  dpy   = XtDisplay(_current_sp->imageGraphic());
  scr   = DefaultScreenOfDisplay(dpy);

  if(color == NULL) 
    _named_color = "black";
  else
    _named_color = color;
  _hd = _xdata = _ydata = NULL;
  loadHeaders(_current_sp);
  _headers_modified = False;
  _vect_ll = new SeisVectLinkedList();
  _vect_ll->addPlot(_current_sp);
  _visible = False;
}


SeisOvjd::~SeisOvjd()
{
  if (_xdata) free(_xdata); _xdata = NULL;
  if (_ydata) free(_ydata); _ydata = NULL;
  if (_hd)    free(_hd);    _hd    = NULL;
  delete _vect_ll;
  if (_vect_data) delete _vect_data;
}


//public method to set the marker color
void SeisOvjd::ovjdColor(char *color) 
{
  if(_V == NULL) return;
  _named_color = color;
  _V->setColor(_named_color);
}




void SeisOvjd::post(SeisPlot *sp)
{
  long i;
  double time;
  long start, end;
  static long old_numsegs = 0;
  static long num_segments;
  short marker_length;
  long location, array_start;
  long num_replace;
  int cf;

  if(sp == NULL) sp = _current_sp;   

  cf = sp->currentFrame();

  if( !sp->imageIsDisplayed() || !_visible ) return;
  
  if(_velocity == 0.0) return; 

  if(_hd == NULL) loadHeaders(sp);

  if(_hd == NULL) return;  

  if(_xdata == NULL)
     {
     _xdata = (float *)calloc(1,(unsigned int)(sp->displayedTraces(cf)
                                 *sizeof(float)));
     _ydata = (float *)calloc(1,(unsigned int)(sp->displayedTraces(cf)
                                 *sizeof(float)));
     }
  else if(old_numsegs != sp->displayedTraces(cf)){
     _xdata  =(float *)realloc(_xdata,(unsigned int)(sp->originalTraces()
                               *sizeof(float)));
     _ydata  =(float *)realloc(_ydata,(unsigned int)(sp->originalTraces()
                               *sizeof(float)));
     }

  if(_xdata == NULL || _ydata == NULL)
     {
     printf("couldnt allocate x or y arrays in SeisOvjd::post\n");
     return;
     }

  start = (sp->firstTrace() - 1) + (sp->currentFrame() * sp->originalTraces());
  end   = (sp->firstTrace() - 1 + sp->displayedTraces(cf))
        + (sp->currentFrame() * sp->originalTraces() );

  num_segments = 1;
  location = sp->firstTrace();
  for( i=start; i<end; i++) 
     {
     time = _hd[i * sp->numHeaders() + _header] / _velocity;
     if(time < 0) time = (-time);  /*for negative offset data*/
     _xdata[location - 1] = (float)location;
     _ydata[location - 1] = time;
     num_segments++;
     location++;
     }
  num_segments--;

  if(_first_time)
     {
     _first_time = False;
     marker_length = (int)((float)sp->traceWidth() * .80);
     _vect_data = new VectData((int)(num_segments),_xdata,_ydata);
     _V = _vect_ll->add(_vect_data,_named_color,2,False,Vector::NoLine,
                        Vector::HorizontalLineMarker,marker_length,2);
     }   
  else
    //Need to check into a better way of replacing these instead of
    //deleting and starting over. Maybe the vector class can do this?
    //I need to remove all points and start over because we may have
    //a varying number of traces from one movie pixmap to the next.
     {
     delete _vect_ll;
     if (_vect_data) delete _vect_data;
     _vect_ll = new SeisVectLinkedList();
     _vect_ll->addPlot(sp);
     marker_length = (int)((float)sp->traceWidth() * .80);
     _vect_data = new VectData((int)(num_segments),_xdata,_ydata);
     _V = _vect_ll->add(_vect_data,_named_color,2,False,Vector::NoLine,
                        Vector::HorizontalLineMarker,marker_length,2);

     /*
     _vect_data->remove(0, old_numsegs);
     array_start = sp->firstTrace() - 1;
     num_replace = ( num_segments > _vect_data->getNumPts() ) 
                 ? _vect_data->getNumPts() : num_segments;
     marker_length = (int)((float)sp->traceWidth() * .80);
     _V->setMarker(Vector::HorizontalLineMarker, marker_length,2);
     
     _vect_data->replace((int)array_start,(int)num_replace,(int)num_segments, 
                         &_xdata[array_start],&_ydata[array_start]);
     */
     }

  makeVisible();
  old_numsegs = num_segments;
}


void SeisOvjd::loadHeaders(SeisPlot *sp)
{
 long headersize;
 long i;
 long images; 

  if(sp == NULL) sp = _current_sp;

  const float *sphd = sp->headers();
  if(!sp->imageIsDisplayed() || !_visible )return;


  images = sp->movie() ? sp->frames() : 1;

  headersize = sp->numHeaders()  * (2*sizeof(double));
  headersize += (sp->plottedNplt() *images * headersize);
  
  if(_hd == NULL)
    _hd = (float *) calloc( 1, (unsigned int)headersize);
  else
    _hd  = (float *) realloc( _hd,  (unsigned int)headersize );

  if(_hd == NULL){printf("error in loadheaders allocation\n");return;}

  if(!_use_external_headers)
    for(i=0; i < (sp->numHeaders()*sp->plottedNplt()*images); i++)
        _hd[i] = sphd[i];


}

void SeisOvjd::makeVisible()
{
 _vect_ll->makeVisible();
 _visible = True;
}


void SeisOvjd::makeInvisible()
{
 _vect_ll->makeInvisible();
 _visible = False;
}




void SeisOvjd::setExternalHeaders(long trace_index, long header_index,
                                  float external_header)
{
static long old_num_traces = 0;
long images = _current_sp->movie() ? _current_sp->frames() : 1;


  _use_external_headers = True; 

  if(!_current_sp->imageIsDisplayed() ||
      _current_sp->plottedNplt() * images < trace_index + 1) 
     return;  

  //allocate array if needed
  if(_hd == NULL || old_num_traces != _current_sp->plottedNplt() * images)
    {
    _visible = True;
    loadHeaders(_current_sp);
    old_num_traces = _current_sp->plottedNplt() * images;
    }


  _hd[trace_index * _current_sp->numHeaders() + header_index] = external_header;
  
}


//=========================================================================
//=== Please keep all informs in this area                              ===
//=========================================================================

void SeisOvjd::newPlot(SeisPlot *sp)
{
  if(!_visible) return;
  if(sp == getCurrentSP())
    {
    loadHeaders(sp);
    post(sp);
    if(_sop != NULL) _sop->newData();
    }
}

void SeisOvjd::postScan(SeisPlot *sp, SeisPlot::ScanDir)
{
  if(!_visible) return;
  if(sp == getCurrentSP())
    {
    loadHeaders(sp);
    post(sp);
    if(_sop != NULL) _sop->newData();
    }
}

void SeisOvjd::dragImage(SeisPlot *sp)
{
  if(!_visible) return;
  if(sp == getCurrentSP())
    post(sp);
}

 
void SeisOvjd::postMovie(SeisPlot *sp, SeisPlot::MovieDir)
{
  if(!_visible) return;
  if(sp == getCurrentSP())
    post(sp);
}


void SeisOvjd::postZoom(SeisPlot *sp, SeisPlot::ZoomDir)
{
  if(!_visible || _V == NULL) return;
  if(sp == getCurrentSP())
    _V->setMarker(Vector::HorizontalLineMarker, 
                  ((unsigned int)((float)sp->traceWidth() * .80)), 2);
}


void SeisOvjd::postZoomSeparateWindow(SeisPlot * /*sp*/, SeisPlot * /*zoomsp*/)
{
  // Currently not using this since it will cause the unzoomed image
  // markers to be large also. Would have to maintain 2 sets of vectors.
  //
  //if(!_visible || _V == NULL) return;
  //_V->setMarker(Vector::HorizontalLineMarker,
  //              ((unsigned int)((float)zoomsp->traceWidth() * .80)), 2);
}


void SeisOvjd::notCurrentInWindow(SeisPlot *sp)
{
  //addSeisPlot(sp->currentSPInWindow());
  if(_current_sp != sp->currentSPInWindow())
    {
    _current_sp = sp->currentSPInWindow();
    loadHeaders(_current_sp);
    post(_current_sp);
    }
}
