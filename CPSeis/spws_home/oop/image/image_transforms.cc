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
/*------------------------------------------------------------------
 *USER DOC
 *Name   : imageTransforms
 *Purpose: Supply transformations from and to the image
 *
 *Author : Michael L. Sherrill
 *Date   : 03/31/94 (C++ version 4/97)
 *
 *NOTES:
 * 1.
 *
 *
 *Revisions:
 * DATE      WHO         DESCRIPTION
 * --------  --------    --------------------------------------------
 *
 *END DOC
 *------------------------------------------------------------------*/

#include "plot_image.hh"
#include <math.h>

//===========================================================================
//================ Get a screen x pixel from a x coord       ================
//===========================================================================
int PlotImage::getXpixelFromX(float x)
{

  if(!useLogarithmicX())
    {  
    return (int)( (float)getLeftImageX() + 0.5 +
                  (float)getFirstTraceLocation()  +
                  (x - getX1()) / getXvaluePerPixel());
    }
  else
    {
    return(int)( (float)getLeftImageX() + 0.5 +
                 (float)getFirstTraceLocation()  +   
                 getLogarithmicPixelFromValue(x, getX1(), getX2(), 
                                              _ximage.width));
    }
}

//===========================================================================
//================ Get a screen x pixel from a user coord    ================
//================ in a manually defined coordinate system   ================
//===========================================================================
int PlotImage::getManualXpixelFromX(float x)
{

  if(!useLogarithmicX())
    { 
    return (int)( (float)getLeftImageX() + 0.5 + 
                  (float)getFirstTraceLocation() +
                  ((x) - manualX1()) / manualXvaluePerPixel());
    }
  else
    {
    return (int)( (float)getLeftImageX() + 0.5 + 
                  (float)getFirstTraceLocation() +
                  getLogarithmicPixelFromValue(x, manualX1(), manualX2(),
                                               _ximage.width));
    }

}

//===========================================================================
//================ Get a x value from a pixel x              ================
//===========================================================================
float PlotImage::getXfromXpixel(int x)
{
float image_x;
long half_a_trace_width;

  if(_user->getMode() <= PlotCOLOR ||
     _user->getMode() == PlotHEADER)//trace type display spacing
    {
    half_a_trace_width = (long)( (float)getTraceWidth() / 2.0 );
    image_x = (float)( getX1() + 
                 (float)(x - getLeftImageX() - half_a_trace_width) *
                 getXvaluePerPixel());
    image_x = max(image_x, min(getX1(),getX2()));
    image_x = min(image_x, max(getX1(),getX2()));
    }
  else
    {
    if(!useLogarithmicX())
      { 
      image_x = (float)( getX1() + (float)(x - getLeftImageX()) *
                 getXvaluePerPixel());
      }
    else
      {
      image_x = getLogarithmicValueFromPixel(x - getLeftImageX(), 
                                             getX1(), getX2(), _ximage.width);
      }
    }

  //image_x = max(image_x, min(getX1(),getX2()));
  //image_x = min(image_x, max(getX1(),getX2()));

  return image_x;
}

//===========================================================================
//================ Get user x from a pixel x in a manually   ================
//================ defined coordinate system                 ================
//===========================================================================
float PlotImage::getManualXfromXpixel(int x)
{
  if(!useLogarithmicX())
    return (float)( manualX1() + (float)((x) - getLeftImageX())
                    * manualXvaluePerPixel());
  else
    return getLogarithmicValueFromPixel(x - getLeftImageX(), manualX1(),
                                        manualX2(), _ximage.width);
}

//===========================================================================
//================ Get a screen y pixel from a coord    =====================
//===========================================================================
int PlotImage::getYpixelFromY(float y)
{
  if(!useLogarithmicY())
    return (int)( (float)getTopImageY() + 0.5 + 
                  ((y) - getY1()) / getYvaluePerPixel() );
  else
    return (int)( (float)getTopImageY() + 0.5 +
           getLogarithmicPixelFromValue(y, getY1(), getY2(), _ximage.height));
}

//===========================================================================
//================ Get y pixel from a y in a manually        ================
//================ defined coordinate system                 ================
//===========================================================================
int PlotImage::getManualYpixelFromY(float y)
{
  if(!useLogarithmicY())
    return (int)( getTopImageY() + 0.5 +
                 ((y) - manualY1()) / manualYvaluePerPixel() );
  else
    {
    return (int)( getTopImageY() + 0.5 +
           getLogarithmicPixelFromValue(y, manualY1(), manualY2(),
                                        _ximage.height) );
    }
}

//===========================================================================
//================ Get y coordinate from a pixel y           ================
//===========================================================================
float PlotImage::getYfromYpixel(int y)
{
  if(!useLogarithmicY())
    return (float)( getY1() + (float)((y) - getTopImageY())
                    * getYvaluePerPixel()); 
  else
    {
    return getLogarithmicValueFromPixel(y - getTopImageY(), getY1(),
                                        getY2(), _ximage.height);
    }
}

//===========================================================================
//================ Get user y from a pixel y in a manually   ================
//================ defined coordinate system                 ================
//===========================================================================
float PlotImage::getManualYfromYpixel(int y)
{
  if(!useLogarithmicY())
     return (float)( manualY1() + (float)((y) - getTopImageY())
                    * manualYvaluePerPixel());
  else
     return getLogarithmicValueFromPixel(y - getTopImageY(), manualY1(),
                                         manualY2(), _ximage.height);
}

//===========================================================================
//================ Get y pixel from a time                   ================
//===========================================================================
int PlotImage::getYpixelFromTime(float t)
{
  return (int)( getTopImageY() + 0.5 +
               ((t) - getTmin()) / getYvaluePerPixel() );
}

//===========================================================================
//================ Get time from a y pixel                   ================
//===========================================================================
float PlotImage::getTimeFromYpixel(int y)
{
  return (float)( getTmin() + (float)((y) - getTopImageY())
                  * getYvaluePerPixel());
}

//===========================================================================
//================ Get x pixel from a trace number           ================
//===========================================================================
int PlotImage::getXpixelFromTraceNumber(int t)
{
  return (int)( getLeftImageX() + 0.5 +
                 ((float)(t) - 0.5) * getTraceWidth() );
}


//===========================================================================
//===========Get a pixel from a value in a logarithmic display ==============
//===========================================================================
int PlotImage::getLogarithmicPixelFromValue(float value, float start_coord,
                                            float end_coord, long num_pixels)
{
double val_per_pix;
double log_start;
double log_value;

  assert(value > 0.0 && start_coord > 0.0 && end_coord > 0.0);

  val_per_pix = ( log10(end_coord) - log10(start_coord) ) / (num_pixels-1);   
  log_start = log10(start_coord);
  log_value = log10(value);
  return  (int)( (log_value - log_start) / val_per_pix );

}


//===========================================================================
//===========Get a value from a pixel in a logarithmic display ==============
//===========================================================================
float PlotImage::getLogarithmicValueFromPixel(long pixel, float start_coord,
                                              float end_coord, long num_pixels)
{
double val_per_pix;
double log_start;
double log_power;
double the_value;

  assert(start_coord > 0.0 && end_coord > 0.0);

  val_per_pix = ( log10(end_coord) - log10(start_coord) ) / (num_pixels-1);   
  log_start = log10(start_coord);
  log_power = val_per_pix * pixel + log_start;
  the_value = pow(10.0, log_power);

  return (float)the_value;

}

//===========================================================================
//================ Get trace number from a x pixel           ================
//===========================================================================
long PlotImage::getTraceNumberFromXpixel(int x)
{
  return (long)( getXfromXpixel(x) + .5);
}

//===========================================================================
//================ Get a sample index from a time            ================
//===========================================================================
long PlotImage::getSampleIndexFromTime(float t)
{
  return (long)( ((t) - getMemoryTmin()) / getImageSampleRate() + 0.5 );
}

//===========================================================================
//================ Get a time from a sample index            ================
//===========================================================================
float PlotImage::getTimeFromSampleIndex(long i)
{
  return (float)( getMemoryTmin() + getImageSampleRate() * (i) );
}

//===========================================================================
//================ Get a time from a sample index            ================
//===========================================================================
float PlotImage::getHeaderFromTrace(long t, long h)
{
  return (float) (_hd[(t - 1) * _nhdrs + h - 1] );
}


//===========================================================================
//================ Get a pixel from header info              ================
//===========================================================================
long PlotImage::getPixelFromHeader(float            header_val, 
                                   int              header_number)

{
int imin = -1;
long i;
long pixel;
float xdist, dist, distmin = 9999999.9;

  for(i = getFirstTraceInImageIndex(); i < getFirstTraceInImageIndex() + 
                                           getNumberDisplayedTraces(); i++)
     {
     xdist = fabs(_hd[(i*getNumberOfHeaders()+header_number-1)] 
                   - header_val);
     dist = xdist;
     if(i == getFirstTraceInImageIndex() || dist < distmin)
        { 
        distmin = dist; 
        imin = (int)i; 
        }
     }

  //The above for loop started uses getFirstTraceInImageIndex which takes
  //into account any movies. So to equate this back to a pixel on the 
  //displayed frame we need to subtract any preceeding frames.
  imin -= (int)getFirstTraceInImageIndex();

  //Must match exactly
  if(imin == getFirstTraceInImageIndex() && distmin) return -1;
  if(imin + 1 == getNumberDisplayedTraces() + getFirstTraceInImageIndex()
     && distmin) return -1;

  if(_zoomed) 
      imin = imin - (int)getFirstTraceInImage() + 1;
  

  if(!_user->getRightToLeft())
    pixel = imin * _trace_delta + getLeftImageX();
  else
    pixel = (getLeftImageX() + getNumberDisplayedTraces() * _trace_delta)
          - (imin + 1) * _trace_delta;

  return (pixel);
}

//===========================================================================
//================ Get horizontal pixels per inch of the display ============
//===========================================================================
int PlotImage::horizontalPixelsPerInch(Display *dsp, int scrn)
{
  return (int)(((float) DisplayWidth(dsp,scrn)) / 
                 (0.0394 * DisplayWidthMM(dsp, scrn)));
}

//===========================================================================
//================ Get vertical   pixels per inch of the display ============
//===========================================================================
int PlotImage::verticalPixelsPerInch(Display *dsp, int scrn)
{
  return (int)(((float) DisplayHeight(dsp,scrn)) / 
                (0.0394 * DisplayHeightMM(dsp, scrn)));
}

