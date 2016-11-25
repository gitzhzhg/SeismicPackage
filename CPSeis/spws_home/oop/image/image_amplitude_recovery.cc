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
//========================= COPYRIGHT NOTICE ================================
//====  CONFIDENTIAL AND PROPRIETARY INFORMATION OF CONOCO INC.      ========
//====   PROTECTED BY THE COPYRIGHT LAW AS AN UNPUBLISHED WORK       ========
//======================== COPYRIGHT NOTICE =================================


//===========================================================================
//========== Utility class for image amplitude recovery           ===========
//========== Michael L. Sherrill 11/97                            ===========
//===========================================================================
#include <stdlib.h>
#include <stdio.h>
#include <math.h>
#include "image_amplitude_recovery.hh"
#include "plot_image.hh"
#include "image_input.hh"


AmplitudeRecovery::AmplitudeRecovery(PlotImage *image)
{
  _image = image;
  _scale_factor_array = NULL;
  _scaled_byte_data = NULL;
  _scale_factor_for_read = NULL;
  _min_displayed_amp = 0.0;
  _max_displayed_amp = 0.0;
  _scale_amp = (float *)calloc(1, (int)(MAX_PIXMAP * sizeof(float)));
}


AmplitudeRecovery::~AmplitudeRecovery()
{
  if(_scale_factor_array)    free (_scale_factor_array);
  if(_scaled_byte_data)      free (_scaled_byte_data);
  if(_scale_factor_for_read) free (_scale_factor_for_read);
  free(_scale_amp);
}


//========================================================================
//============ Allocate the array for holding individual trace scale amps=
//========================================================================
int AmplitudeRecovery::allocateScaleFactorArray(long size)
{
long for_read;

  if(_scale_factor_array == NULL)
    _scale_factor_array = (float *) malloc((int)((size+1)*sizeof(float)));
  else
    _scale_factor_array = (float *) realloc(_scale_factor_array,
                                           (int)((size+1)*sizeof(float)));
  if(_scale_factor_array == NULL) 
    return False;

  for_read = size / _image->getTracesInMemory() + 1;
  if(_scale_factor_for_read == NULL)
    _scale_factor_for_read = (float *) malloc((int)((for_read)*sizeof(float)));
  else
    _scale_factor_for_read = (float *) realloc(_scale_factor_for_read,
                                           (int)((for_read)*sizeof(float)));
  if(_scale_factor_for_read == NULL) 
    return False;

  return True;  

}


//========================================================================
//============ Get the array of trace scale amplitudes ===================
//========================================================================
float *AmplitudeRecovery::getScaleFactorArray(long index)
{
  return &_scale_factor_array[index];
}



//========================================================================
//============ Get a scaled byte data value  =============================
//========================================================================
unsigned char AmplitudeRecovery::getScaledByteDataValue(long index)
{
  return _scaled_byte_data[index];
}


//========================================================================
//============ Get scaled data arra for displaying =======================
//========================================================================
unsigned char *AmplitudeRecovery::getScaledByteDataArray(long index)
{
  return &_scaled_byte_data[index];
}


//========================================================================
//============ Compute the largest display scaler amplitude  =============
//============ This array is populated by tfio.              =============
//========================================================================
void AmplitudeRecovery::setScaleFactorForReadArray(long panel_index,
                                                   long num_traces)
{
float largest_scale_factor = 0.0;
long i,j;
long aryoffset, sampleoffset;
float largest_value = 0.0;
long num_to_do;
Boolean cube_slice;


  cube_slice = (_image->_Cl.axis == 1);

  //note tfio stores last scaler in num traces + 1 element



  //If we have made more than one read request the last _scale_factor_array
  //array member has not been populated by tfio with the largest abs value
  //----- Byte Data 
  if(_image->isFloatData() == False)
    {
    if(_image->_user->getNumberOfReadRequests() > 1 || cube_slice)
      {
      if(!cube_slice)
        {
        aryoffset = panel_index * _image->getTracesInMemory();
        num_to_do = num_traces;
        }
      else
        {
        aryoffset = panel_index * _image->getTracesInMemory() *
                    _image->getSamplesInMemory();
        num_to_do = num_traces * _image->getSamplesInMemory();
        }
      for(i=0;i<num_to_do;i++) 
        {
        if(largest_scale_factor < fabs(_scale_factor_array[i+aryoffset]))
           largest_scale_factor = fabs(_scale_factor_array[i+aryoffset]);
        }
      _scale_factor_for_read[panel_index] = largest_scale_factor;
      }
    else//one read request and not a cube slice
      {
      aryoffset = (panel_index + 1) * _image->getTracesInMemory();
      _scale_factor_for_read[panel_index] = _scale_factor_array[aryoffset];
      }
    }
  else//-----float data ---- _scale_amp is used unless NORM requested
    {
    if(_image->_user->getNorm() == PlotImage::NORM)
      {
      if(!cube_slice)
        {
        aryoffset = panel_index * _image->getTracesInMemory();
        sampleoffset = aryoffset * _image->getSamplesInMemory();
        }
      else
        {
        aryoffset = panel_index * _image->getTracesInMemory() * 
                    _image->getSamplesInMemory();
        sampleoffset = aryoffset;
        }
      for(i=0;i<_image->getTracesInMemory();i++)
        {
        for(j=0;j<_image->getSamplesInMemory();j++)
          {
          if(largest_value < 
             fabs(_image->_float_array[i*_image->getSamplesInMemory() +
                                       j + sampleoffset]                  ))
            {
            largest_value = 
                 fabs(_image->_float_array[i*_image->getSamplesInMemory() +
                                     j + sampleoffset] );
            }
          }
        _scale_factor_array[i+aryoffset] = largest_value;
        largest_value = 0.0;
        }
      }
    }

}


//========================================================================
//============ Compute the min and max display scaled amps ===============
//========================================================================
void AmplitudeRecovery::computeDisplayedAmps(long AryOffset)
{
long i,j;
int min_byte = 255;
int max_byte = 1;


  

  if(_image->_user->_mode != PlotImage::PlotGRID   &&
     _image->_user->_mode != PlotImage::PlotISO    &&
     _image->_user->_mode != PlotImage::PlotHEADER    )
    {
    _max_displayed_amp = 1.0;
    _min_displayed_amp = 255;
    for( i = 0; i < _image->_ntot; i++)
      {
      scaleDataForDisplay(AryOffset, i, 0, _image->_nsamp - 1);
      for ( j = 0; j < _image->_nsamp; j++ )
        {
        if ((int)getScaledByteDataValue(j) < min_byte)
            min_byte = (int)getScaledByteDataValue(j);
        if ((int)getScaledByteDataValue(j) > max_byte)
            max_byte = (int)getScaledByteDataValue(j);
        if(min_byte == 1 && max_byte == 255.0)
          { 
          _min_displayed_amp = (float)min_byte;
          _max_displayed_amp = (float)max_byte;
          return;
          }
        }
      }
    _min_displayed_amp = (float)min_byte;
    _max_displayed_amp = (float)max_byte;
    }
  else//get float ranges
    {
    if(_image->_user->_mode == PlotImage::PlotARRAY)
      {
      _max_displayed_amp = 0.0;
      for ( i = 0; i < _image->_nsamp * _image->_ntot; i++ )
        {
        if (_image->_float_array[i+AryOffset] > _max_displayed_amp)
               _max_displayed_amp = _image->_float_array[i+AryOffset];
        } 
      if(_max_displayed_amp == 0.0)/*May have all negatives*/
        {
        for ( i = 0; i < _image->_nsamp * _image->_ntot; i++ )
          {
          if (_image->_float_array[i+AryOffset] < _max_displayed_amp)
                   _max_displayed_amp = _image->_float_array[i+AryOffset];
          }
        }
      }
    else
     {
     _min_displayed_amp = 1.0;
     _max_displayed_amp = 255.0;
     }
   }


}

//========================================================================
//============ Get minimum displayed amp. This value is already scaled ===
//========================================================================
float AmplitudeRecovery::getMinDisplayedAmplitude()
{
  return _min_displayed_amp;
}


//========================================================================
//============ Get maximum displayed amp. This value is already scaled ===
//========================================================================
float AmplitudeRecovery::getMaxDisplayedAmplitude()
{
  return _max_displayed_amp;
}


//========================================================================
//=========== Set scale amp used to scale the display with ===============
//========================================================================
void AmplitudeRecovery::setScaleAmp(long panel_index)
{
long i, j, aryoffset;
long trace_offset;

  trace_offset = panel_index * _image->getTracesInMemory();



  //----- Byte Data 
  if(_image->isFloatData() == False)
    {
    if(_image->_user->getNorm() == PlotImage::EXTERNALNORM) /*get external amp*/
      {
      _scale_amp[panel_index] = _image->_user->getExternalAmplitude();
      } 
    else if(_image->_user->getNorm() == PlotImage::FILENORM)/*use file max*/
      {
      _scale_amp[panel_index] = _image->_user->_G.trmaxg;
      }
    else if(_image->_user->getNorm() == PlotImage::PANELNORM)//get amp for panel 
      {
      _scale_amp[panel_index] = 0.0;
      for(i=0;i<_image->getTracesInMemory();i++)
          {
          for(j=0;j<_image->getSamplesInMemory();j++)
            {
            if(_scale_amp[panel_index] <
               fabs(getTrueAmplitude(i+trace_offset, j)))
                 _scale_amp[panel_index] = getTrueAmplitude(i+trace_offset, j);
            }
          }
      }
    else if(_image->_user->getNorm() == PlotImage::NORM)//scale to each trace
      {
        //the scale_factor_array will have the largest of each trace however
        //the user may have erroneously normalized a color plot and the
        //color routines will need something in the ballpark to get a reasonable
        //display, so set it to the lav of the panel
      _scale_amp[panel_index] = 0.0;
      for(i=0;i<_image->getTracesInMemory();i++)
        {
        for(j=0;j<_image->getSamplesInMemory();j++)
          {
          if(_scale_amp[panel_index] <
             fabs(getTrueAmplitude(i+trace_offset, j)))
               _scale_amp[panel_index] = getTrueAmplitude(i+trace_offset, j);
          }
        }
      }
    }
  else//----float data
    {
    if(_image->_user->getNorm() == PlotImage::EXTERNALNORM) /*get external amp*/
      {
      _scale_amp[panel_index] = _image->_user->getExternalAmplitude();
      } 
    else if(_image->_user->getNorm() == PlotImage::FILENORM)/*use file max*/
      {
      _scale_amp[panel_index] = _image->_user->_G.trmaxg;
      }
    else if(_image->_user->getNorm() == PlotImage::PANELNORM)//get amp for panl 
      {
      aryoffset = panel_index * _image->getTracesInMemory() *
                  _image->getSamplesInMemory();
      _scale_amp[panel_index] = fabs(_image->_float_array[aryoffset]);
      for(i=0;i<_image->getSamplesInMemory() * _image->getTracesInMemory();i++) 
        { 
        _scale_amp[panel_index]=
         (_scale_amp[panel_index] < fabs(_image->_float_array[aryoffset+i])) ?
                                  fabs(_image->_float_array[aryoffset+i])  :
                                  _scale_amp[panel_index];
        }
      }
    else if(_image->_user->getNorm() == PlotImage::NORM)//scale to each trace
      {
      //the scale_factor_array will have the largest of each trace however
      //the user may have erroneously normalized a color plot and the
      //color routines will need something in the ballpark to get a reasonable
      //display, so set it to the lav of the panel
      _scale_amp[panel_index] = 0.0;
      for(i=0;i<_image->getTracesInMemory();i++)
        {
        for(j=0;j<_image->getSamplesInMemory();j++)
          {
          if(_scale_amp[panel_index] <
             fabs(getTrueAmplitude(i+trace_offset, j)))
               _scale_amp[panel_index] = getTrueAmplitude(i+trace_offset, j);
          }
        }
      }
    }
}


//========================================================================
//=======================scale data for display ==========================
//========================================================================
int AmplitudeRecovery::scaleDataForDisplay(long data_array_offset,
                                           long trace_index, 
                                           long first_sample,
                                           long last_sample)
{
long num_elements;
long total_data_offset;
long trace_skip;

  if(_image->_user->getMode() == PlotImage::PlotARRAY)
    assert(_image->_user->getNorm() == PlotImage::EXTERNALNORM);

  trace_skip = data_array_offset / _image->getSamplesInMemory();

  total_data_offset = trace_index * _image->getSamplesInMemory() +
                      data_array_offset + first_sample;

  num_elements = last_sample - first_sample + 1;
  if(_scaled_byte_data == NULL)
    { 
    _scaled_byte_data = 
          (unsigned char *)malloc((int)num_elements * sizeof(unsigned char));
    }
  else
    {
     _scaled_byte_data = 
          (unsigned char *)realloc(_scaled_byte_data,
                                  (int)num_elements * sizeof(unsigned char));
    }
  if(_scaled_byte_data == NULL)
    {
     printf("alloc failed in AmplitudeRecovery::scaleDataForDisplay\n");
     return False;
    }


  //----- Byte Data 
  if(_image->isFloatData() == False)
    scaleBytes(&_image->_byte_array[total_data_offset], 
               trace_index + trace_skip, num_elements, total_data_offset);
  else//float data
    scaleFloats(&_image->_float_array[total_data_offset], 
                trace_index + trace_skip, num_elements, total_data_offset);


  return True;
}


//========================================================================
//=================== Scale bytes for display ============================
//========================================================================
void AmplitudeRecovery::scaleBytes(unsigned char *byte_array_in,
                                   long          trace_index,
                                   long          num_elements,
                                   long          offset_for_slice)
{
int j,ic;
int temp;
float reference_scaler;
long i, panel_index;
float scfac;
unsigned char max_byte;
Boolean cube_slice;

  cube_slice = (_image->_Cl.axis == 1);

  panel_index = trace_index / _image->getTracesInMemory();

  if(_image->_user->getNorm() != PlotImage::NORM) 
    {
    if(_image->_user->getNorm() == PlotImage::FILENORM ||
       _image->_user->getNorm() == PlotImage::EXTERNALNORM ||
       _image->_user->getNorm() == PlotImage::PANELNORM)
      reference_scaler = _scale_amp[0] / 127.0;
    else// scale to panel
      reference_scaler = _scale_factor_for_read[panel_index];
    scfac=1.0;
    if(!cube_slice)
      {
      if(reference_scaler != 0.)
        scfac = _scale_factor_array[trace_index] / reference_scaler;
      if(scfac != 1.0)
        {
        for(j=0;j<num_elements;j++) 
          { 
          ic = byte_array_in[j] - 128;
          temp = (int)(128.0 + (scfac * (float)ic + 0.5));
          if(temp < 1) temp = 1;
          if(temp > 255) temp = 255;
          _scaled_byte_data[j]= (unsigned char)temp;
          }
        }
      else//no scaling needed
        {
        for(j=0;j<num_elements;j++)_scaled_byte_data[j] = byte_array_in[j]; 
        }
      }
    else//cube slice
      {
      for(j=0;j<num_elements;j++) 
        {
        if(reference_scaler != 0.)
          scfac = _scale_factor_array[offset_for_slice+j] / reference_scaler;
        if(scfac != 1.0)
          {
          ic = byte_array_in[j] - 128;
          temp = (int)(128.0 + (scfac * (float)ic + 0.5));
          if(temp < 1) temp = 1;
          if(temp > 255) temp = 255;
          _scaled_byte_data[j]= (unsigned char)temp;
          }
        else//no scaling needed
          {
          for(j=0;j<num_elements;j++)_scaled_byte_data[j] = byte_array_in[j];
          }
        }
      }
    }
  else //Normalize the bytes
    {
    max_byte = 1;
    for(i=0;i<num_elements;i++)//find larges byte in trace
      {
      if(max_byte < byte_array_in[i]) max_byte = byte_array_in[i];
      }
    if(max_byte == 128) max_byte = 255;//so no scaling will happen
    scfac = 127.0 / ((float)max_byte - 128.0);
    for(i=0;i<num_elements;i++)//scale up so that largest byte == 255
      {
      ic = byte_array_in[i] - 128;
      temp = (int)(128.0 + (scfac * (float)ic + 0.5));
      if(temp < 1) temp = 1;
      if(temp > 255) temp = 255;
      _scaled_byte_data[i] = (unsigned char)temp;
      }
    }


}

//========================================================================
//================== Scale floats for display ============================
//========================================================================
void AmplitudeRecovery::scaleFloats(float         *float_array_in, 
                                    long          trace_index,
                                    long          num_elements,
                                    long          offset_for_slice)
{
int   i, maxval, ival;
int   zeropt; 
int   Lowbyte, Hibyte;
float sc;
float reference_scaler;
Boolean cube_slice;
long panel_index;

  panel_index = trace_index / _image->getOriginalDisplayedTraces();

  cube_slice = (_image->_Cl.axis == 1);

  Lowbyte = 1;
  Hibyte  = 255;
  zeropt = Lowbyte + (Hibyte -1)/2;
  maxval = (Hibyte-1)/2;
  sc = 0.0;

  if(!cube_slice)
    {
    if(_image->_user->getNorm() != PlotImage::NORM) 
      {
      if(_image->_user->getNorm() == PlotImage::PANELNORM)
        reference_scaler = _scale_amp[panel_index];
      else
        reference_scaler = _scale_amp[0];
      }
    else // norm equals NORM so each trace is scaled by its LAV*/
      { 
      reference_scaler = _scale_factor_array[trace_index];
      }
 
    if(reference_scaler != 0.0) sc = maxval / reference_scaler;

    for(i=0;i<num_elements;i++) 
      {
      ival= (zeropt + (int) (float_array_in[i]*sc+0.5));
      if(ival < 1) ival=1;
      if(ival > 255) ival=255;
      _scaled_byte_data[i] = (unsigned char)ival;
      }
    }
  else//cube slice
    {
    for(i=0;i<num_elements;i++) 
      {
      if(_image->_user->getNorm() != PlotImage::NORM) 
        {
        if(_image->_user->getNorm() == PlotImage::PANELNORM)
          reference_scaler = _scale_amp[panel_index];
        else
          reference_scaler = _scale_amp[0];
        }
      else // norm equals NORM so each trace is scaled by its LAV*/
        {
        reference_scaler = _scale_factor_array[offset_for_slice + i];
        }
      if(reference_scaler != 0.0) sc = maxval / reference_scaler;
      ival= (zeropt + (int) (float_array_in[i]*sc+0.5));
      if(ival < 1) ival=1;
      if(ival > 255) ival=255;
      _scaled_byte_data[i] = (unsigned char)ival;
      }
    }

}

//========================================================================
//================== Scale a floats for display ==========================
//========================================================================
unsigned char AmplitudeRecovery::scaleAfloat(float  value, 
                                             long   trace_index,
                                             long   offset_for_slice)
{
int   i, maxval, ival;
int   zeropt; 
int   Lowbyte, Hibyte;
float sc;
float reference_scaler;
Boolean cube_slice;
long panel_index;
unsigned char byte_val;

  panel_index = trace_index / _image->getOriginalDisplayedTraces();

  cube_slice = (_image->_Cl.axis == 1);

  Lowbyte = 1;
  Hibyte  = 255;
  zeropt = Lowbyte + (Hibyte -1)/2;
  maxval = (Hibyte-1)/2;
  sc = 0.0;

  if(!cube_slice)
    {
    if(_image->_user->getNorm() != PlotImage::NORM) 
      {
      if(_image->_user->getNorm() == PlotImage::PANELNORM)
        reference_scaler = _scale_amp[panel_index];
      else
        reference_scaler = _scale_amp[0];
      }
    else // norm equals NORM so each trace is scaled by its LAV*/
      { 
      reference_scaler = _scale_factor_array[trace_index];
      }
 
    if(reference_scaler != 0.0) sc = maxval / reference_scaler;

    ival= (zeropt + (int) (value * sc + 0.5) );
    if(ival < 1) ival=1;
    if(ival > 255) ival=255;
    byte_val = (unsigned char)ival;
    }
  else//cube slice
    {
    if(_image->_user->getNorm() != PlotImage::NORM) 
      {
      if(_image->_user->getNorm() == PlotImage::PANELNORM)
        reference_scaler = _scale_amp[panel_index];
      else
        reference_scaler = _scale_amp[0];
      }
    else // norm equals NORM so each trace is scaled by its LAV*/
      {
      reference_scaler = _scale_factor_array[offset_for_slice + i];
      }
    if(reference_scaler != 0.0) sc = maxval / reference_scaler;
    ival= (zeropt + (int) (value * sc + 0.5) );
    if(ival < 1) ival=1;
    if(ival > 255) ival=255;
    byte_val = (unsigned char)ival;
    }

  return byte_val;

}


//========================================================================
//================ Get the true amplitude of a data point ================
//========================================================================
float AmplitudeRecovery::getTrueAmplitude(long trace_index, long trace_sample)
{

  _max_true_amplitude = _image->_user->_G.trmaxg;

  if(_image->_byte_array != NULL)
    return getTrueAmpFromByte(trace_index,trace_sample);
  else if(_image->_float_array != NULL)
    return getTrueAmpFromFloat(trace_index,trace_sample);
  else
    return 0.0;

}

float AmplitudeRecovery::convertScaledByteDataToFloat(long trace_index,
                                                      unsigned char byte_val)
{
int   MidPoint  = 128;
float ScaleFactor;
float float_val;
int data_byte_val;
int adjusted_byte_val;
long panel_index;

  panel_index = trace_index / _image->getTracesInMemory();

  if(_image->_user->getNorm() != PlotImage::NORM) 
    {
    if(_image->_user->getNorm() == PlotImage::FILENORM ||
       _image->_user->getNorm() == PlotImage::EXTERNALNORM)
      ScaleFactor = _scale_amp[0] / 127.0;
    else// scale to panel
      ScaleFactor = _scale_factor_for_read[panel_index];
    }
  else//Normalize
    {
    ScaleFactor = _scale_factor_array[trace_index];
    }

  data_byte_val = (int)byte_val;
  adjusted_byte_val = data_byte_val - MidPoint;
  float_val = (float)adjusted_byte_val * ScaleFactor; 

  return float_val;
}


float AmplitudeRecovery::getTrueAmpFromByte(long trace_index,long trace_sample) 
{
int   MidPoint  = 128;
float ScaleFactor;
float true_amp;
int data_byte_val;
int adjusted_byte_val;
Boolean cube_slice;
long data_index;
long scaler_index;

  data_index = trace_index * _image->getSamplesInMemory() + trace_sample;
  
  cube_slice = (_image->_Cl.axis == 1);

//Byte data is normalized trace by trace when created on cps
//so we have to use the scale factor for the trace to recover the amplitude
  if(!cube_slice)
    scaler_index = trace_index;
  else
    scaler_index = data_index;//cube slices have 1 scaler for each data value

  ScaleFactor = _scale_factor_array[scaler_index];
  data_byte_val = (int)_image->_byte_array[data_index];
  adjusted_byte_val = data_byte_val - MidPoint;
  true_amp = (float)adjusted_byte_val * ScaleFactor; 

  return true_amp;
}


float AmplitudeRecovery::getTrueAmpFromFloat(long trace_index,long trace_sample) 
{
float true_amp;
long data_index;

  data_index = trace_index * _image->getSamplesInMemory() + trace_sample;

  true_amp = _image->_float_array[data_index];
  return true_amp; 

}


float AmplitudeRecovery::getScaleAmp(long panel_index)
{
  return _scale_amp[panel_index];
}


//========================================================================
//================== Scale a float for display ==========================
//========================================================================
float AmplitudeRecovery::getAfloat(float  value, 
                                   long   trace_index,
                                   long   offset_for_slice)
{
float retval, lav, sc;
float reference_scaler;
Boolean cube_slice;
long panel_index;

  _max_true_amplitude = _image->_user->_G.trmaxg;

  panel_index = trace_index / _image->getOriginalDisplayedTraces();

  cube_slice = (_image->_Cl.axis == 1);

  sc = 0.0;

  if(!cube_slice)
    {
    if(_image->_user->getNorm() != PlotImage::NORM) 
      {
      if(_image->_user->getNorm() == PlotImage::PANELNORM)
        reference_scaler = _scale_amp[panel_index];
      else
        reference_scaler = _scale_amp[0];
      lav = _max_true_amplitude;
      }
    else // norm equals NORM so each trace is scaled by its LAV*/
      { 
      reference_scaler = _scale_factor_array[trace_index];
      lav = _max_true_amplitude;
      }
 
    if(reference_scaler != 0.0) sc = lav / reference_scaler;

    retval = value * sc;
    retval = retval > lav ? lav : (retval < -lav ? -lav : retval);
    }
  else//cube slice
    {
    if(_image->_user->getNorm() != PlotImage::NORM) 
      {
      if(_image->_user->getNorm() == PlotImage::PANELNORM)
        reference_scaler = _scale_amp[panel_index];
      else
        reference_scaler = _scale_amp[0];
      lav = _max_true_amplitude;
      }
    else // norm equals NORM so each trace is scaled by its LAV*/
      {
      reference_scaler = _scale_factor_array[offset_for_slice];
      lav = _max_true_amplitude;
      }
    if(reference_scaler != 0.0) sc = lav / reference_scaler;
    retval = value * sc;
    retval = retval > lav ? lav : (retval < -lav ? -lav : retval);
    }

  return retval;

}
