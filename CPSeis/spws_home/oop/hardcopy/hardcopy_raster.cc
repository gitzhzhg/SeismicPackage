//===========================================================================
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
//===================== Hardcopy rasterization    ===========================
//===================== M.L. Sherrill 05/96       ===========================
//===========================================================================
 
//Note it seems that cgm expects data arrays with unsigned char precision
//yet allows color indicies up to 4096, will have to keep an eye on this


#include <math.h>
#include <stdlib.h>
#include "cprim.h"
#include "named_constants.h"
#include "hardcopy/hardcopy_raster.hh"
#include "hardcopy/hardcopy_plot.hh"
#include "hardcopy/hardcopy_color_bar.hh"
#include "sp/seis_plot.hh"
#include "image_amplitude_recovery.hh"

#define SEMBLANCE_HEADER 5
#define MAX_LABEL_LENGTH 11.0
//===========================================================================
//========================== Constructor                          ===========
//========================== If frame_number is set               ===========
//========================== rasterization occurs on construction ===========
//===========================================================================
HardCopyRaster::HardCopyRaster( HardCopyPlot  *hc,
                                SeisPlot      *sp,
                                long          num_colors,
                                float         *rgbs,
                                unsigned char *map_array,
                                float         x1,
                                float         y1,
                                float         x2,
                                float         y2,
                                float         dots_per_inch,
                                long          frame_number,
                                Boolean       use_pip_extensions)
{
int cgm_index = 0;
long i; 
float xp1, xp2, yp1, yp2;

  _hc         = hc;
  _sp         = sp;
  _num_colors = num_colors;
  _map_array  = map_array;
  _rgbs       = rgbs;
  _status     = False;
  _contiguous = 1;
 
  //load in cgm assigned colors
  for(i = 0; i < 4 * _num_colors; i+=4)
    {
    _cgm_colors[cgm_index] = 
                   _hc->defineColorIndex(_rgbs[i],_rgbs[i+1],_rgbs[i+2]);
    ++cgm_index;
    }

  //see if cgm colors are contiguous
  if(_num_colors > 1)
    {
    long increment = _cgm_colors[1] - _cgm_colors[0];
    for(i = 0; i < _num_colors -1 ; i++)
      {
      if(_cgm_colors[i+1] - _cgm_colors[i] != increment)
        {
        _contiguous = 0;
        i = _num_colors;
        }
      }
    }


  _dots_per_inch = dots_per_inch;
  _raster_array = NULL;

  if(x1 == 0.0 && y1 == 0.0 && x2 == 0.0 && y2 == 0.0)
    {
    xp1 = _hc->leftBorderWidth();
    yp1 = _hc->topBorderHeight();
    xp2 = _hc->leftBorderWidth() + _hc->drawingAreaWidth();     
    yp2 = _hc->topBorderHeight() + _hc->drawingAreaHeight();
    }
  else
    {
    xp1 = x1;
    yp1 = y1;
    xp2 = x2;
    yp2 = y2;
    }


  //CGM does not handle metric so we will set all metric parameters to
  //the equivalent english system.
  //if(_sp->units() != PlotEnglish) _dots_per_inch /= .39;

  _coordinate_system  = HardCopyPlot::INCHES;

  _raster_width = (int)((xp2 - xp1) * _dots_per_inch);
  _raster_height= (int)((yp2 - yp1) * _dots_per_inch);
                        
  if(frame_number)
    {
    if(use_pip_extensions)
      {
      if(_sp->plotType() == PlotImage::PlotARRAY) 
        _status = rasterizeFloats(frame_number);
      else
        _status = rasterizeTraces(frame_number);
      }
    else
      {
      _status = rasterizeNonPip(frame_number);
      }
    }

  if(_status)
    {
    //If we are plotting color trace data we need to shift the data left by
    //half a trace width so that the raster will line up with overlay traces
    //if there happen to be any.
      // if(_sp->plotType() == PlotImage::PlotCOLOR)
      //{
      //float half_trace_delta = ( _hc->drawingAreaWidth() / 
      //                           (float)_sp->plottedNplt() )  / 2.0;
      //xp1 -= half_trace_delta;
      //xp2 -= half_trace_delta;
      //}
     _hc->drawRasterImage( xp1,  yp1, xp2, yp2, 
                           _raster_width, _raster_height, 
                           (char *)_raster_array,
                           _coordinate_system);
    }


  if(_raster_array != NULL)free(_raster_array);
  _raster_array = NULL;
}


//===========================================================================
//===================== Destructor  =========================================
//===========================================================================
HardCopyRaster::~HardCopyRaster()
{

  if(_raster_array != NULL) free(_raster_array);

}


//===========================================================================
//===================== Main method for byte rasterization ==================
//===================== Frame number is movie number  =======================
//===========================================================================
int HardCopyRaster::rasterizeTraces(long frame_number)
{
 long StartTraceIndex;
 float *float_array = _sp->floatTraceDataForUpdate();
 long index, header_offset;
 int error;

  _raster_array = ( unsigned char *) malloc(_raster_height * _raster_width * 
                   sizeof( unsigned char));
  if(_raster_array == NULL) return(False);


  StartTraceIndex = (frame_number - 1) * _sp->memoryTraces();
  if(_sp->rToL()) StartTraceIndex += _sp->displayedTraces() - 1;

  header_offset = (frame_number - 1) * 
                  (_sp->displayedTraces() * _sp->numHeaders());

  for(index = 0; index < _sp->displayedTraces(); index++)
    {
      if(_sp->plotType() == PlotImage::PlotSEMB)
        {
          _sp->getAmplitudeRecovery()->scaleDataForDisplay(0,StartTraceIndex,
                                               0,_sp->displayedSamples()-1); 
          error = _sp->rasterizeSemblance(
                   _sp->getAmplitudeRecovery()->getScaledByteDataArray(0),
                   index, StartTraceIndex, header_offset,
                   _raster_array, _raster_width, _raster_height,
                   (unsigned char *)_cgm_colors, _map_array, True);
        }
      else
        {
          if(_sp->useHiResolution())
            error = _sp->rasterizeByFloats(
                     &float_array[StartTraceIndex * _sp->displayedSamples()],
                     index, StartTraceIndex,
                     _raster_array, _raster_width, _raster_height,
                     (unsigned char *)_cgm_colors, _map_array, True);        
          else
            error = _sp->rasterizeByBytes(
                     &float_array[StartTraceIndex * _sp->displayedSamples()],
                     index, StartTraceIndex,
                     _raster_array, _raster_width, _raster_height,
                     (unsigned char *)_cgm_colors, _map_array, True);
        }

      if(error) 
        return 0;

      if(_sp->rToL())
        StartTraceIndex--;
      else
        StartTraceIndex++;
    }

  return 1;

}



//=========================================================================
//==========  This method only used for iso type images    ================
//==========  It calls the image library for rasterization ================
//==========  so it can take advantage of the interpolator ================
//=========================================================================
int HardCopyRaster::rasterizeFloats(long frame_number)
{
long error;
long frame_index = frame_number - 1;

  _raster_array = ( unsigned char *) malloc(_raster_height * _raster_width * 
                   sizeof( unsigned char));
  if(_raster_array == NULL) return(False);

  error = _sp->variableArray(frame_index,
                             _raster_array,
                             _raster_width,
                             _raster_height,
                             (unsigned char *)_cgm_colors,
                             _map_array,
                             True);
                              
  if(error)
    return (False);
  else
    return(True);

}



//===========================================================================
//===================== Method to rasterize data when pip  ==================
//===================== extensions are not supported       ==================
//===========================================================================
int HardCopyRaster::rasterizeNonPip(long frame_number)
{
float factor, r, fraction, temp, temp2;
long i, j;
long *integer_trace;
long tindex;
long xstart, ystart;
long xmin, xmax;
long type = 1;
long start, end, length;
long byte_index, line_index;
long shift;
static long *ext=NULL;
static long length_of_ext=0;  
long  totalpixels, k, x;
long rowswap;
unsigned char *junk;
long StartTraceIndex;
long index;
unsigned char *trace_in;
float trace_delta;
double lval, display_scale;

 

  /*allocate size of array*/
  integer_trace = NULL;
  integer_trace = (long *) malloc(_raster_height * sizeof(long));
  if(integer_trace == NULL) return False;
  _raster_array = (unsigned char *)calloc(1,_raster_height * _raster_width * 
                   sizeof( unsigned char));
  if(_raster_array == NULL)
    {
    free (integer_trace); 
    return False;
    }


  trace_delta =  _raster_width /  ((float)_sp->plottedNplt()); 
  if(_sp->norm() == PlotImage::FILENORM)//use 255 for computing display scale
    lval = 255.0;
  else
    lval = max( fabs(_sp->minDisplayedAmplitude() - 128.0),
                     fabs(_sp->maxDisplayedAmplitude() - 128.0) ) + 128.0;
  display_scale = (_sp->ct() * trace_delta) / lval;




  StartTraceIndex = (frame_number - 1) * _sp->memoryTraces();
  if(_sp->rToL())  StartTraceIndex += _sp->displayedTraces() - 1;

  for(index = 0; index < _sp->displayedTraces(); index++)
    {
    _sp->getAmplitudeRecovery()->scaleDataForDisplay(0,StartTraceIndex,
                                                 0,_sp->displayedSamples()-1); 
    trace_in = _sp->getAmplitudeRecovery()->getScaledByteDataArray(0);
    factor = (float) (_sp->displayedSamples() - 1)/(float)_raster_height; 
    r = 0.0;
    if(_sp->rToL())
      StartTraceIndex--;
    else
      StartTraceIndex++;
    for(i=0; i<_raster_height-1; i++)
      {
      tindex = (long)r;
      if (tindex >= _sp->displayedSamples()-1) break;
      fraction = r - tindex;
      temp = display_scale*( ((float)trace_in[tindex]-128.0) + fraction 
           *  ( ((float)trace_in[tindex+1]-128.0)
           - ((float)trace_in[tindex]-128.0) ));
      if (temp >= 0)
         integer_trace[i] = (long)(temp + 0.5);
      else
         integer_trace[i] = (long)(temp - 0.5);
      if(_sp->rp()) integer_trace[i] = -integer_trace[i];
      r += factor;
      }
    for(i=i; i<_raster_height; i++) /*last value in trace*/ 
      {
      integer_trace[i] = (long)(display_scale
                         * ((float)trace_in[_sp->displayedSamples()-1]-128.0));
      if(_sp->rp()) integer_trace[i] = -integer_trace[i];
      }

    xstart = (long)((index * trace_delta) + (trace_delta / 2));
    ystart = 0;
    xmin = (long)(xstart - _sp->ct() * trace_delta);
    if (xmin < 0) xmin = 0;
    xmax = (long)(xstart + _sp->ct() * trace_delta);
    if (xmax > _raster_width-1) xmax = _raster_width - 1;


    /******************* digitize wiggle type traces ********************/
    if(_sp->plotType() == PlotImage::PlotWONLY)
      type = 0;
    if(_sp->plotType() == PlotImage::PlotWFILL && _sp->negativeFill() == 0)
      type=1;
    if(_sp->plotType() == PlotImage::PlotWFILL && _sp->negativeFill() == 1)
      type=2; 

    line_index = ystart * _raster_width;
    /* allocate memory for ext array if necessary */

    if(length_of_ext < _raster_height) 
      {
      if (ext != NULL) {free(ext); ext = NULL;}
      ext = (long *) malloc( _raster_height*sizeof(long));
      if(ext == NULL) return False;
      length_of_ext = _raster_height;
      }


    /* find the extension for trace */
    for(i=0; i<_raster_height-1; i++) 
      {
      if(integer_trace[i] > integer_trace[i+1])
        ext[i] = integer_trace[i+1] - integer_trace[i] + 1;
      else if (integer_trace[i] < integer_trace[i+1])
        ext[i] = integer_trace[i+1] - integer_trace[i] - 1;
      else
        ext[i] = 0;
      }
    ext[_raster_height-1] = 0;
    

    /* now fill in the trace */
    for(i=0; i<_raster_height; i++) 
      {
      if(type == 0) /* wiggle only */ 
        {
        start = xstart + integer_trace[i];
        end = start + ext[i];
        } 
      else if (type == 1) /* va + wiggle, positive fill */ 
        {
        if(integer_trace[i] > 0) 
          {
          start = xstart;
          if (ext[i] > 0) 
             end = xstart + integer_trace[i] + ext[i];
          else 
             end = xstart + integer_trace[i];
          if (ext[i] + integer_trace[i] < 0) 
             start += (ext[i]+integer_trace[i]);
          } 
        else 
          {
          start = xstart + integer_trace[i];
          end = start + ext[i];
          }
        } 
      else /*type 2 va + wiggle, negative fill */ 
        {
        if(integer_trace[i] < 0) 
          {
          start = xstart;
          if (ext[i] < 0) 
             end = xstart + integer_trace[i] + ext[i];
          else 
             end = xstart + integer_trace[i];
          if (ext[i] + integer_trace[i] > 0)start += (ext[i]+integer_trace[i]);
          } 
        else 
          {
          start = xstart + integer_trace[i];
          end = start + ext[i];
          }
        }

      if(start < end) 
        {
        length = end - start + 1;
        } 
      else 
        {
        length = start - end + 1;
        temp2 = end;
        end = start;
        start = (long)temp2;
        }

      /* take care of clipping */
      if(end < xmin) 
        {
        start = xmin;
        length = 1;
        } 
      else if (start < xmin) 
        {
        length -= (xmin - start);
        start = xmin;
        }
  
      if(start > xmax) 
        {
        start = xmax;
        length = 1;
        } 
      else if (start + length - 1 > xmax) 
        {
        length = xmax - start + 1;
        }

      /* translate start into a byte address and shift */
      byte_index = start >> 3;
      shift = start - (byte_index << 3);

      /* fill the raster */
      if(length > 0) 
        {
        for(j=0; j<length; j++) 
           {
           _raster_array[line_index + start] = _hc->blackColor();
           start++;
           //      (char)(_raster_array[line_index + start] | (128 >> shift));
           //shift++;
           // if(shift == 8) 
           //   {
           //   shift = 0;
           //   start++;
           //   }
           }
        }
  

      line_index += _raster_width;
      }/*end for i to height*/  

    /*invert y axis of image if requested*/
    if( (index == _sp->displayedTraces() - 1) && (_sp->invert() == True) )
      {
      totalpixels = _raster_height * _raster_width;
      junk = NULL;
      junk = (unsigned char *) malloc(_raster_width
                                    * sizeof(unsigned char));
      if(junk == NULL) return False;
      rowswap = (int)((float)_raster_height / 2.0 + 0.9);
      for(i=0;i<rowswap;i++)
         {
         x = i * _raster_width;
         k = totalpixels - _raster_width - x;
         for(j=0;j<_raster_width;j++)
           {
           junk[j] = _raster_array[j+x];
           _raster_array[j+x] = _raster_array[k];
           _raster_array[k]   = junk[j];
           k++;
           }
         }
      free(junk);
      }

    }

 
  free (integer_trace); 
  return True;

}




//===========================================================================
//=====================         Draw color bar       ========================
//===========================================================================
void HardCopyRaster::drawColorBar(float x,     float y, 
                                  float width, float height,
                                  int   /*deci_places*/)
{
  HardCopyColorBar cb(_hc, _num_colors, _rgbs);
  cb.drawCbarAt(x, y, width, height); 
}

//===========================================================================
//=====================         Draw Square bar       ========================
//===========================================================================
void HardCopyRaster::drawSquare9(float x,     float y, 
                                 float width, float height)
{
  HardCopyColorBar cb(_hc, _num_colors, _rgbs);
  cb.drawSquare9At(x, y, width, height); 
}
