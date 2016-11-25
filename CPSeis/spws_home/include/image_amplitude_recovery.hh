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
//========== Main class for image generation                      ===========
//========== Michael L. Sherrill 11/91 (C++ version 4/97)         ===========
//===========================================================================

#ifndef _AMPLITUDE_RECOVERY_H
#define _AMPLITUDE_RECOVERY_H

class PlotImage;


//===========================================================================
//========== Utility class for handling amplitude recovery        ===========
//===========================================================================
class AmplitudeRecovery
{
  public:
    AmplitudeRecovery(PlotImage *image);
    ~AmplitudeRecovery();
    float         *_scale_factor_array;
    float         *_scale_factor_for_read;
    unsigned char *_scaled_byte_data;
    float         _scaler;
    float         *_scale_amp;
    float         _min_real_amp;
    float         _max_real_amp;
    float         _max_displayed_amp;
    float         _min_displayed_amp;
    float         _max_true_amplitude;
    unsigned char _min_byte_amp;
    unsigned char _max_byte_amp;
    void setScaleAmp(long panel_index);
    float getScaleAmp(long panel_index = 0);
    int allocateScaleFactorArray(long size);
    float *getScaleFactorArray(long index);
    int scaleDataForDisplay(long data_array_offset,
                            long trace_index, 
                            long first_sample,
                            long last_sample);
    void scaleBytes(unsigned char *byte_array_in,
                    long trace_index,
                    long num_elements,
                    long offset_for_slice);
    void scaleFloats(float *float_array_in, 
                    long trace_index,
                    long num_elements,
                    long offset_for_slice);
    unsigned char scaleAfloat(float value,
                    long trace_index,
                    long offset_for_slice);
    float getAfloat(float  value, 
                    long   trace_index,
                    long   offset_for_slice);
    unsigned char getScaledByteDataValue(long index);
    unsigned char *getScaledByteDataArray(long index);
    float convertScaledByteDataToFloat(long trace_index,unsigned char byte_val);
    float getTrueAmplitude(long trace_index, long sample);
    float getTrueAmpFromByte(long trace_index, long sample);
    float getTrueAmpFromFloat(long trace_index, long sample);
    void  setScaleFactorForReadArray(long panel_index, long num_traces);
    void  computeDisplayedAmps(long array_offset);
    float getMaxDisplayedAmplitude();
    float getMinDisplayedAmplitude();

  private:
    PlotImage     *_image;
    
};

#endif
