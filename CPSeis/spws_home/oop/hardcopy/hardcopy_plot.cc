#include <string.h>
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
#include <assert.h>
#include <math.h>
#include <stdlib.h>
#include "hardcopy/hardcopy_plot.hh"
#include "hardcopy/paper_plot.hh"
#include "sp/seis_plot.hh"
#include "sp/seis_plot.hh"
#include "sp/sp_transform.hh"
#include "cgm.h"

#define MAIN_PLOT 1 
#define WHOLE_PLOT_INCHES 2
#define numCoodSystems 4
#define DEFAULT_LEFT_ANNO_WIDTH   1.5F
#define DEFAULT_RIGHT_ANNO_WIDTH  1.5F
#define DEFAULT_TOP_ANNO_HEIGHT   2.0F
#define DEFAULT_BOTTOM_ANNO_HEIGHT 1.0F

#define True  1
#define False 0
#define BLACK 1

HardCopyColor  *HardCopyPlot::_color;

HardCopyPlot::HardCopyPlot(PaperPlot *pp, SeisPlot *sp) :
                      _pp(pp),
                      _dim_set(False), _range_set(False),
                      _left_anno_width(DEFAULT_LEFT_ANNO_WIDTH), 
                      _right_anno_width(DEFAULT_RIGHT_ANNO_WIDTH), 
                      _top_anno_height(DEFAULT_TOP_ANNO_HEIGHT),
                      _bottom_anno_height(DEFAULT_BOTTOM_ANNO_HEIGHT),
                      _xmap(0), _sp(sp), _transform(NULL), _x_delta(0.0),
                      _trace_x0(0.0), _trace_x1(0.0), _headers_decrement(0)
{
  /*
   * color initializations
   */
 assert(_color);
 PaperPlot::addPlot();
 _plot_number= PaperPlot::plotCount();
 if (_sp) _transform= _sp->transform();
}

HardCopyPlot::~HardCopyPlot()
{
}

unsigned char HardCopyPlot::goodFile()
{
  return True;
}


void HardCopyPlot::setWidthHeight(float xinch,
                                  float yinch,
                                  float width, 
                                  float height)
{
  float total_width= _pp->totalWidth();
  float total_height=_pp->totalHeight();


  assert(!_dim_set);
  _dim_set= True;
  _width= width;
  _height= height;
  _xinch= xinch;
  _yinch= yinch;

  _minx = xinch/total_width;
  _maxx = (xinch+width) / total_width;
  _miny = yinch/total_height;
  _maxy = (yinch+height) / total_height;

  cgmGswn(  getTransformation(INCHES), 0, _width, 0, _height);
  cgmGsvp(  getTransformation(INCHES), _minx,  _maxx, _maxy,  _miny);
}

void HardCopyPlot::setRange( float x0, float y0, float x1, float y1)
{
  if (x0 == x1) {
     x0 -= .001;
     x1 += .001;
  }
  if (y0 == y1) {
     y0 -= .001;
     y1 += .001;
  }
  // 
  // when a metric mode is added then we need to map cm into the inches
  //
  float total_width=  _pp->totalWidth();
  float total_height= _pp->totalHeight();
  float x_inch_to_percent_start;
  float x_inch_to_percent_end;
  float y_inch_to_percent_start;
  float y_inch_to_percent_end;
  float half_x_delta=0;
  //float xmap_shift;
  //if (_xmap > 0) xmap_shift= _xmap - _left_anno_width;

  assert(!_range_set);
  assert(_dim_set);
  _range_set= True;

  _x0= x0;
  _x1= x1;
  _y0= y0;
  _y1= y1;

  int ptype=  (int)_sp->plotType();
  if ( ptype == PlotImage::PlotWONLY    ||
       ptype == PlotImage::PlotWFILL    ||
       ptype == PlotImage::PlotCOLOR    ||
       ptype == PlotImage::PlotHEADER ) {
         float screen_plot_width= (1/_sp->plottedTI()) * (fabs(x0-x1) +1);
         _x_delta= ( drawingAreaWidth() * (1/_sp->plottedTI()) ) /
                                                        screen_plot_width;
         half_x_delta= _x_delta * 0.5;

         _trace_x0= _sp->firstTraceIndex() + 1;
         if(_sp->rToL())
           _trace_x1= (_trace_x0 - _sp->plottedNplt()) + 1;
         else
           _trace_x1= (_trace_x0 + _sp->plottedNplt()) - 1;

         if( ( _sp->rToL() && _x0 < _x1) ||
             (!_sp->rToL() && _x0 > _x1) )
           _headers_decrement = 1;

  }

  x_inch_to_percent_start = (_xinch + _left_anno_width + half_x_delta) 
                                                            / total_width;
  x_inch_to_percent_end   = (_xinch + _width  - _right_anno_width - 
                                                          half_x_delta)  
                                                            / total_width;
  y_inch_to_percent_start = (_yinch + _bottom_anno_height) / total_height;
  y_inch_to_percent_end   = (_yinch + _height - _top_anno_height) 
                                                / total_height;

  cgmGswn(   getTransformation(PLOT), x0, x1, y0, y1); 
  cgmGsvp(   getTransformation(PLOT), x_inch_to_percent_start,  
                                         x_inch_to_percent_end, 
                                         y_inch_to_percent_end,
                                         y_inch_to_percent_start);


  cgmGswn(   getTransformation(YplotXinches), 0, _width, y0, y1);
  cgmGsvp(   getTransformation(YplotXinches), _minx,  _maxx, 
                          y_inch_to_percent_end, y_inch_to_percent_start);

  cgmGswn(   getTransformation(XplotYinches), x0, x1, 0, _height);
  cgmGsvp(   getTransformation(XplotYinches) , 
                          x_inch_to_percent_start, y_inch_to_percent_end, 
                          _maxy,  _miny);

  setTransformation( INCHES);
  setTextHeight(.25);
}

float HardCopyPlot::getX0()   { return _x0;}
float HardCopyPlot::getX1()   { return _x1;}
float HardCopyPlot::getY0()   { return _y0;}
float HardCopyPlot::getY1()   { return _y1;}
float HardCopyPlot::getTraceX0()        { return _trace_x0;}
float HardCopyPlot::getTraceX1()        { return _trace_x1;}
float HardCopyPlot::plotWidth()         { return _width;}
float HardCopyPlot::plotHeight()        { return _height;}
float HardCopyPlot::drawingAreaWidth()  
{ 
  return (_width - _left_anno_width - _right_anno_width);
}
float HardCopyPlot::drawingAreaHeight() 
{ 
  return _height - _top_anno_height - _bottom_anno_height;
}

void HardCopyPlot::setLeftBorderWidth(const    float w){ _left_anno_width= w;}
void HardCopyPlot::setRightBorderWidth(const   float w){ _right_anno_width= w;}
void HardCopyPlot::setTopBorderHeight(const    float h){ _top_anno_height= h;}
void HardCopyPlot::setBottomBorderHeight(const float h)
                                                     { _bottom_anno_height= h;}

float HardCopyPlot::leftBorderWidth()        { return _left_anno_width;}
float HardCopyPlot::rightBorderWidth()       { return _right_anno_width;}
float HardCopyPlot::topBorderHeight()        { return _top_anno_height;}
float HardCopyPlot::bottomBorderHeight()     { return _bottom_anno_height;}

float HardCopyPlot::defaultRightBorderWidth() {
  return DEFAULT_RIGHT_ANNO_WIDTH;
}

float HardCopyPlot::defaultLeftBorderWidth() {
  return DEFAULT_LEFT_ANNO_WIDTH;
}

float HardCopyPlot::defaultTopBorderHeight() {return DEFAULT_TOP_ANNO_HEIGHT;}
float HardCopyPlot::defaultBottomBorderHeight() 
                                           {return DEFAULT_BOTTOM_ANNO_HEIGHT;}

/*
 * ==================== COLOR ===================
 */
int HardCopyPlot::defineColorIndex( float percent_red,
                                    float percent_green, 
                                    float percent_blue)
{
  assert(_color);
  return ( _color->defineColorIndex(percent_red,percent_green,percent_blue) );
}

int HardCopyPlot::blackColor() 
{ 
  assert(_color);
  return BLACK;
}

void HardCopyPlot::initColor()
{
  assert(!_color);
  _color= new HardCopyColor();
}

void HardCopyPlot::resetColor()
{
  assert(_color);
  delete _color;
  _color= NULL;
}

/*
 * ==================== LINES ===================
 */
void HardCopyPlot::setLineWidth(float line_width)
{
  cgmGslwsc(line_width);
}

void HardCopyPlot::setLineType(LineType line_type)
{
  cgmGsln(line_type);                /* Set line type*/
}

void HardCopyPlot::setLineColor(int color_pix)
{
  cgmGsplci(color_pix);
}


float HardCopyPlot::computePlotOffset() 
{
 float adder= 0;

 if (_x_delta > 0.0) {
      adder= (_xmap - _left_anno_width) / _x_delta;

      if(adder < 0.0) adder = 0.0;

      float map_range       = max(_x0, _x1) - min(_x0, _x1);
      float trace_range     = max(_trace_x0, _trace_x1) - 
                              min(_trace_x0, _trace_x1);
      float trace_increment = map_range / trace_range;

      if(!_headers_decrement)
        adder = min(_x0, _x1) + adder - trace_increment;
      else
        adder = max(_x0, _x1) - adder + trace_increment;
 }

 return adder;
}

//#define MAX_PTS 4094
#define MAX_PTS 25

void HardCopyPlot::drawLines( HCpoint     *pts, 
                              int          numpts,
                              CoordSystem  coord_sys)
{
  float adder= 0;
  int next_chunk=0;
  int start_pt=0;
  int end_pt;
  int left_to_plot= numpts;
  float *x, *y;
  int i, j;
  if (numpts > 1) {
     assert(_dim_set&&_range_set);
     setTransformation(coord_sys); 
     if (coord_sys == PLOT) {
           if (_transform) {
                for(i=0;(i<numpts); i++) transformXY(&pts[i].x, &pts[i].y); 
           }
           cgmGsclip(True);
           adder= computePlotOffset();
     } // end if
   
     if(_sp->useLogarithmicX())
        convertToLogarithmicX(pts, numpts);

     for( ; (left_to_plot > 0); ) {
         if (left_to_plot > MAX_PTS) {
               next_chunk= MAX_PTS;
         }
         else 
               next_chunk= left_to_plot;
         left_to_plot-= next_chunk;
         end_pt= next_chunk + start_pt -1;
         
   
   
         x= (float*)malloc( sizeof(float) * next_chunk);
         y= (float*)malloc( sizeof(float) * next_chunk);
         for (j=0,i=start_pt; (i<=end_pt); i++,j++) {
              if(!_headers_decrement)
                {
                x[j]= pts[i].x + adder;
                y[j]= pts[i].y;
                }
              else
                {
                x[j]= adder - pts[i].x;
                y[j]= pts[i].y;
                }
         } // end loop
         cgmGpl(next_chunk,x,y); 
         free(x);
         free(y);
   
         start_pt= end_pt;
         if (left_to_plot>0) left_to_plot++;;    
     } // end loop
     cgmGsclip(False);
  } // end if numpts > 1
  else {
     // Currently if I have a line with 1 point I don't do anything.
     // This might need to be reconsidered.  X puts a point on the 
     // screen in this case.
     // We could draw a short line or draw a marker.
  }
}

void HardCopyPlot::drawLine( float        x1, 
                             float        y1, 
                             float        x2, 
                             float        y2,
                             CoordSystem  coord_sys)
{
  float x[2];
  float y[2];
  float adder= 0;
  assert(_dim_set&&_range_set);
  setTransformation(coord_sys); 
  if (coord_sys == PLOT) 
    {
      transformXY(&x1, &y1); 
      transformXY(&x2, &y2); 
      cgmGsclip(True);

      if(_sp->useLogarithmicX())
        {
          HCpoint *points;
          points = new HCpoint[2];
          points[0].x = x1;
          points[1].x = x2;
          convertToLogarithmicX(points, 2);
          x1 = points[0].x;
          x2 = points[1].x;
        }

      adder= computePlotOffset();
      if(!_headers_decrement)
        {
          x[0]= x1 + adder;
          x[1]= x2 + adder;
        }
      else
        {
          x[0]= adder - x1;
          x[1]= adder - x2;
        }
    }
  else {
    x[0]= x1;
    x[1]= x2;
  }


  y[0]= y1;
  y[1]= y2;
  cgmGpl(2,x,y); 
  cgmGsclip(False);
}



/*
 * ==================== Polygons ===================
 */
void HardCopyPlot::setFillType(FillType fill_type)
{
 cgmGsfais(fill_type);          // Set fill type

}

void HardCopyPlot::setFillColor(int color_pix)
{
  cgmGsfaci(color_pix);
}


void HardCopyPlot::drawFilledPolygon( HCpoint     *pts, 
                                      int          numpts,
                                      CoordSystem  coord_sys)
{
  float adder= 0;
  int i;
  assert(_dim_set&&_range_set);
  setTransformation(coord_sys); 
  if (coord_sys == PLOT) {
        if (_transform) {
             for(i=0;(i<numpts); i++) transformXY(&pts[i].x, &pts[i].y); 
        }
        cgmGsclip(True);
        adder= computePlotOffset();
        if(_sp->useLogarithmicX())
          convertToLogarithmicX(pts, numpts);
  } // end if

  

  float *x= (float*)malloc( sizeof(float) * numpts);
  float *y= (float*)malloc( sizeof(float) * numpts);
  for (i=0; (i<numpts); i++) {
      if(!_headers_decrement)
        {
        x[i]= pts[i].x + adder;
        y[i]= pts[i].y;
        }
      else
        {
        x[i]= adder - pts[i].x;
        y[i]= pts[i].y;
        }
  }
  cgmGfa(numpts,x,y); 
  free(x);
  free(y);
  cgmGsclip(False);
}



/*
 * ==================== Text ===================
 */
void HardCopyPlot::setTextColor(int color_pix)
{
  cgmGstxci(color_pix);   /* Set text colors */
}

void HardCopyPlot::setTextHeight(float height, CoordSystem coord_sys)
{
  assert(_dim_set&&_range_set);
  setTransformation(coord_sys); 
  cgmGschh(height);      /* Set character heights */
}


void HardCopyPlot::writeText(float        x, 
                             float        y, 
                             char        *text,
                             CoordSystem  coord_sys)
{
  float adder= 0;
  assert(_dim_set&&_range_set);
  setTransformation(coord_sys); 
  if (coord_sys == PLOT) {
        transformXY(&x, &y); 
        adder= computePlotOffset();
        if(!_headers_decrement)
          cgmGtx(x + adder, y, text);
        else
          cgmGtx(adder - x, y, text);
  }
  else {
        cgmGtx(x, y, text);
  }
}

void HardCopyPlot::setFont(enum FontType font)
{
  cgmGstxfp(font,2);
}

void HardCopyPlot::writeConstrainedText(float        x, 
                                        float        y, 
                                        float        width,
                                        float        height, 
                                        char        *text,
                                        CoordSystem  coord_sys)
{
  float adder= 0;
  assert(_dim_set&&_range_set);
  setTransformation(coord_sys); 
  if (coord_sys == PLOT) {
        transformXY(&x, &y); 
        adder= computePlotOffset();
        if(!_headers_decrement)
          cgmGtxr(width,height,x+adder,y,text);
        else
          cgmGtxr(width,height,adder - x,y,text);
  }
  else {
        cgmGtxr(width,height,x,y,text);
  }
}


/*
 * ==================== Markers ===================
 */
void HardCopyPlot::setMarkerColor(int color_pix)
{
  cgmGspmci(color_pix);   /* Set text colors */
}

void HardCopyPlot::setMarkerSize(float size, CoordSystem  coord_sys)
{
  assert(_dim_set&&_range_set);
  setTransformation(coord_sys); 
  cgmSetMarkerSize(size);
}

void HardCopyPlot::drawMarker(float        x, 
                              float        y, 
                              int          type,
                              CoordSystem  coord_sys)
{
  float adder= 0;
  assert(_dim_set&&_range_set);
  setTransformation(coord_sys); 

  if (coord_sys == PLOT) 
    {
      transformXY(&x, &y); 
      cgmGsclip(True);
      if(_sp->useLogarithmicX())
        {
          HCpoint *points;
          points = new HCpoint[1];
          points[0].x = x;
          convertToLogarithmicX(points, 1);
          x = points[0].x;
        }
      adder= computePlotOffset();
      if(!_headers_decrement)
        x = adder + x;
      else
        x = adder - x;
    }
 

  cgmGsmk(type);   /* Set marker type */
  cgmGpm(1,&x,&y);
}



void HardCopyPlot::setMarkerAngle(float angle)
{
 cgmSetMarkerAngle(angle);
}


void HardCopyPlot::setLabelPlacement( HardCopyLabelPlacement place)
{

     switch (place) {
       case Normal: 
                      cgmGstxal(0,0);
                      break;
       case LowerLeft: 
                      cgmGstxal(1,5);
                      break;
       case LowerCenter:
                      cgmGstxal(2,5);
                      break;
       case LowerRight:
                      cgmGstxal(3,5);
                      break;
       case CenterLeft:
                      cgmGstxal(1,3);
                      break;
       case DeadCenter:
                      cgmGstxal(2,3);
                      break;
       case CenterRight:
                      cgmGstxal(3,3);
                      break;
       case TopLeft:    
                      cgmGstxal(1,1);
                      break;
       case TopCenter: 
                      cgmGstxal(2,1);
                      break;
       case TopRight:
                      cgmGstxal(3,1);
                      break;

     } // end switch
}


void HardCopyPlot::setTransformation(CoordSystem  coord_sys)
{
  cgmGselnt( (numCoodSystems*_plot_number)+coord_sys );
}


int HardCopyPlot::getTransformation(CoordSystem  coord_sys)
{
  return( (numCoodSystems*_plot_number)+coord_sys );
}


void HardCopyPlot::drawRasterImage(float        x1,
                                   float        y1,
                                   float        x2,
                                   float        y2,
                                   int          x_array_length,
                                   int          y_array_length,
                                   char        *array,
                                   CoordSystem  coord_sys)
{
  float adder= 0;
  assert(_dim_set&&_range_set);
  setTransformation(coord_sys); 
  if (coord_sys == PLOT) {
          transformXY(&x1, &y1); 
          transformXY(&x2, &y2); 
          adder= computePlotOffset();
          if(!_headers_decrement)
            {
            x1+= adder;
            x2+= adder;
            }
          else
            {
            x1 = adder - x1;
            x2 = adder - x2;
            }
  } // end if
  cgmGscell(x1,y1,x2,y2,x_array_length,y_array_length,array,1);
}

void HardCopyPlot::transformXY(float  *x,
                               float  *y)
{
  if (_transform) {
       *x= _transform->convDataToXOffset(_sp,*x);
       *y= _transform->convDataToYOffset(_sp,*y);
  } 
}


//===========================================================================
//=========== Convert xys to logarithmic display points        ==============
//===========================================================================
void HardCopyPlot::convertToLogarithmicX( HCpoint     *pts, 
                                          int          numpts)
{
double val_per_point;
double log_start;
double log_value;
double width = _pp->totalWidth() - (_left_anno_width + _right_anno_width);
double point;
double scaler = (_x1 - _x0) / width;


  assert(_x0 > 0.0 && _x1 > 0.0);

  val_per_point = ( log10(_x1) - log10(_x0) ) / width;   
  log_start = log10(_x0);

  for(long i = 0; i < numpts; i++)
    {
    log_value = log10(pts[i].x);
    point     = pts[i].x  = (log_value - log_start) / val_per_point;
    pts[i].x  = point * scaler + _x0;
    }

}
