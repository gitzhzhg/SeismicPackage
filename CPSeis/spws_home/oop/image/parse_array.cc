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
 *Name   : parseArrayValue
 *Purpose: Convert an x y pixel location on an array image to the proper
 *         float values. This is a temporary routine that
 *         will be replaced by Tom in his vel library. 
 *
 *Author : Michael L. Sherrill
 *Date   : 10/92 (C++ version 4/97)
 *
 * Function Definition:
 * void parseArrayValue(int     yin,
 *                  double  xval,
 *                  double  *aval,
 *                  long    nhdrs,
 *                  long    samples,
 *                  float   *hd,
 *                  float   *float_array)
 *
 * yin       in         Y pixel location.
 * xval      in         X pixel location.
 * aval      out        The output value
 * nhdrs     in         Number of headers in header array.
 * samples   in         Number of samples in the array.
 * hd        in         Header array.
 * float_array  in      float array.
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
#include "xy_display.hh"

void PlotImage::parseArrayValue( int     yin,  
                                 long    top_border,
                                 double  xval,
                                 double  *aval,
                                 long    nhdrs,    
                                 long    samples,
                                 float   *hd,
                                 float   *float_array)
{
long max_num;
long i;
int xhdr;
float temp1, temp2, x1, x2;
float diff = 0.0, x_diff, ratio;
long ysample, addsamples;
float minx, maxx;
Boolean ascending;
long skip_traces = 0;
long toffset, hoffset;
float ytime;

  xhdr = (int)_coordinate_header - 1;
 
  for(i=1;i<=_cpixm;i++) skip_traces += _tpnl[i-1];
  skip_traces += _first_trace_in_image - 1;
  toffset = getSamplesInMemory() * skip_traces;
  hoffset = _nhdrs * skip_traces + xhdr;

  if(_zindex == NO_ZOOM)
    {
    max_num = _tpnl[_cpixm];
    ytime = (yin - top_border - 1) * _y_value_per_pixel + _tmin;
    ysample = (long)((ytime - _tmin) / _user->getGlobals().srval);
    if(ysample < 0) ysample = (-ysample);
    }
  else
    {
    max_num = _ntot + _first_trace_in_image - 1;
    ytime = (yin - top_border - 1) * _y_value_per_pixel + _tmin;
    ysample = (long)((ytime - _tmin) / _user->getGlobals().srval);
    if(ysample < 0) ysample = (-ysample);
    addsamples = (long)((_tmin - _zoomyary[NO_ZOOM][0]) 
               / _user->getGlobals().srval);
    if(addsamples < 0) addsamples = (-addsamples);
    ysample += addsamples;
    }


  if(hd[hoffset]<hd[(max_num-1)*nhdrs+hoffset])
      {
      minx = hd[hoffset];
      maxx = hd[(max_num-1)*nhdrs+hoffset];
      ascending = True;
      }
   else
      {
      minx = hd[(max_num-1)*nhdrs+hoffset];
      maxx = hd[hoffset];
      ascending = False;
      }

/**** x location is outside of image *****/
  if(xval <= minx)
     {
     if(ascending)
        *aval = float_array[ysample+toffset];
     else
        *aval = float_array[(max_num-1) * samples + ysample + toffset];
     return;
     }
  if(xval >= maxx)
     {
     if(ascending)
        *aval = float_array[(max_num-1) * samples + ysample + toffset];
     else
        *aval = float_array[ysample+toffset];
     return;
     }

/****** x location is somewhere in the interior of plot*******/
  i = 1;
  if(ascending)
     while( hd[i*(nhdrs)+hoffset] < xval && i < max_num) i++;
  else
     while( hd[i*(nhdrs)+hoffset] > xval && i < max_num) i++;
  temp1 = float_array[(i-1)*(samples) + ysample + toffset];
  temp2 = float_array[i*(samples) + ysample + toffset];
  x1 = hd[(i-1)*(nhdrs)+hoffset];
  x2 = hd[i*(nhdrs)+hoffset];
  if(temp1 != _undefined_value && temp2 != _undefined_value)
     diff = temp2 - temp1;

  if(diff && _xydisp->interpolate_readout == True)
     {
     x_diff = x2 - x1;
     ratio= 1.0 / (x_diff/(xval-x1));
     *aval = diff * ratio + temp1;
     }
  else
     {
     *aval = temp1;
     }

  if(_xydisp->interpolate_readout == False && 
     _xydisp->not_defined_value == *aval )
     *aval = 0.0;
}
