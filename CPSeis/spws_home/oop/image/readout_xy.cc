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
 *Name   : readoutXY  
 *Purpose: Convert an x y pixel location on an image to data values.         
 *                              
 *Author : Michael L. Sherrill
 *Date   : 11/91 (C++ version 4/97)
 *
 * Function Definition:
 * long readoutXY  (long xin , long yin, long *trace_index, double *yval)
 *
 * xin       in         X pixel location.    
 * yin       in         Y pixel location.                           
 * xindex    out        X converted index.                       
 * xval      out        X converted data value.
 * yval      out        Y converted data value.
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
#include "tfdefs.h"
#include "cprim.h"
#include "image_amplitude_recovery.hh"


long PlotImage::readoutXY(long             xin, 
                          long             yin, 
                          long             *trace_index, 
                          double           *xval,
                          double           *yval,
                          double           *aval)
{
 long x_offset;
 long x_width;
 long in_bounds = False;
 long trace_sample;
 long velhdr = 62;
 long integer_samples;
 int   xheader= _xydisp->getXReadoutHeader();
 int   alty_header= _xydisp->getAltYReadoutHeader();
 float t0;

 *trace_index = 0;
 *xval = 0.0;
 *yval = 0.0;
 *aval = 0.0;

 if(!_hd) return (False);

 if(xheader == -1) xheader= (int)_user->getPrimaryAnnotationHeader();

 if(xheader > _nhdrs || xheader < -1) return False;

 if(!_filedata) 
   {
   velhdr = _coordinate_header;
   if(velhdr > _nhdrs || velhdr < 1) return False;
   }

 if(alty_header > _nhdrs) return False;

/*image may be offset in the drawable*/
 xin -= _dest_x;
 yin -= _dest_y;


 x_offset = _left_border;

/*******determine borders and width****************/
 if (_user->getMode() >= PlotCOLOR)
   {
   x_width = _ximage.width;
   }
 else
   {
   x_width  =  _displayed_traces * _trace_delta;
   }


/*******if in image area convert coordinates************/
 if(xin >= x_offset && xin < x_width + x_offset &&
    yin >  _top_border && 
    yin <= _ximage.height + _top_border)
    {
    in_bounds = True;
    }
 else
    {
    return(in_bounds);
    }



/*********************CONVERT PIXELS TO X COORDINATES *********************/
  switch(_user->getMode())
  {
   case PlotWONLY:
   case PlotWFILL:
   case PlotCOLOR:
   case PlotHEADER:
      //add dest_x back since following methods take it into account
      xin += _dest_x;
      yin += _dest_y;
      *trace_index= (int)getTraceNumberFromXpixel((int)xin) - 1
                          + (_cpixm * getTracesInMemory());
      *xval= getHeaderFromTrace( *trace_index + 1, xheader );
      if(_user->getMode() == PlotHEADER)
	{
        *yval = (yin-_top_border) * _y_value_per_pixel + _tmin;
        *aval = 0.0;
	return(True);
	}
      else if (alty_header != -1)
        {
        *yval= getHeaderFromTrace( *trace_index + 1, alty_header);
        }
      break;

   case PlotCONTOUR:
   case PlotSEMB:
       *xval = (xin - x_offset) / _velfactor + _vel_min; 
       *trace_index = 0; /*not used*/
        break;

   case PlotISO:
       *xval = (xin - x_offset) * _x_value_per_pixel
             + _grid_x1;
        break;
   case PlotGRID:
       *aval = 0.0;
       if(!useLogarithmicX())
         {
         *xval = (xin - x_offset) * _x_value_per_pixel + _grid_x1;
         }
       else
         {
         if(!_manual_annotate)
           *xval = getXfromXpixel((int)xin);
         else
           *xval = getManualXfromXpixel((int)xin);
         }
       if(!useLogarithmicY())
         {
         *yval = (yin-_top_border) * _y_value_per_pixel + _grid_y1;
         /* Do not do the following on grid types, it will
            give too coarse of a sampling on the readout
         integer_samples = NearestInteger(*yval / 
                       (_user->getGlobals().srval*_user->getTdec()));
         if(integer_samples < 0) integer_samples += (-1);
         *yval = integer_samples * (_user->getGlobals().srval *
                       _user->getTdec());
         */

         }
       else
         {
         if(!_manual_annotate)
           *yval = getYfromYpixel((int)yin);
         else
           *yval = getManualYfromYpixel((int)yin);
         }
       return(True);
  }
/******************END OF CONVERTING X COORDINATES******************/ 




/******************CONVERT PIXELS TO Y COORDINATES******************/
  if(alty_header == -1)  // if -1 then we have already used yval for a HW
    {
    if(_manual_annotate)
      {
      *yval = (yin-_top_border) * _manual_y_value_per_pixel;
      t0    = _manual_grid_y1;
      }
    else
      {
      *yval = (yin-_top_border) * _y_value_per_pixel;
      t0    = _tmin;
      }
    //set to correct sample based on sample rateun
    if(_user->getMode() != PlotSEMB)
      {
      integer_samples = (int)(*yval / 
                        (_user->getGlobals().srval * _user->getTdec()));
      if(integer_samples < 0) integer_samples += (-1);
      *yval = integer_samples * (_user->getGlobals().srval * _user->getTdec())+t0;
      }
    if(_user->getDepthMode() && _manual_annotate == False) *yval *= 1000.0;
   }
/********************DONE WITH PIXEL TO Y COORDINATES********************/


  /*********convert y value to amplitude or velocity value***********/
  if(_xydisp->mouse_readout_type == MOUSE_VEL) /*velocity type*/
     {
     /*if(_image->getMode() == PlotISO && image->filedata)*/
     /*if(_image->getMode() == PlotISO && image->point_to_data == False)*/
     if(_user->getMode() == PlotISO && _float_array != NULL)
        parseArrayValue((int)yin, _top_border, *xval, aval, _nhdrs, 
                       getSamplesInMemory(), _hd, _float_array);
     else
        *aval = _hd[*trace_index * _nhdrs + velhdr]; // velocity header
     }    
  if(_xydisp->mouse_readout_type == MOUSE_AMP) // amplitude readout
     {
     *aval=((yin-_top_border)*_y_value_per_pixel + _tmin) - getMemoryTmin();
     trace_sample = (long)( *aval / 
                      (_user->getGlobals().srval * _user->getTdec()) );
     *aval = _amp_recovery->getTrueAmplitude(*trace_index, trace_sample);
     *aval *= _median_scale;//in case median ram color has been used
     }



  if(_user->getMode() == PlotSEMB) *aval = 0.0;

  return(in_bounds);

}


