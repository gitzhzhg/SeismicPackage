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
 *Name   : imageColor 
 *Purpose: Determine request type of color display to create. Make  
 *         appropriate calls to read a Cray type color bar file
 *         and calculate percentages of amplitude values to color or
 *         rescale to a median value.
 *
 *Author : Michael L. Sherrill
 *Date   : 02/92 (C++ version 4/97) 
 *
 * Function Definition:
 * void color_image ( long BytOffset,  long num_images)
 *
 *
 * BytOffset  in         Number of byte samples to skip 
 *
 * num_images in         Number of images we will make
 *
 *
 *NOTES:
 *
 *
 *Revisions:
 * DATE      WHO         DESCRIPTION
 * --------  --------    --------------------------------------------
 *
 *END DOC
 *------------------------------------------------------------------*/
#include <Xm/Xm.h>
#include "tfdefs.h"
#include "plot_image.hh"
#include "image_amplitude_recovery.hh"

void PlotImage::imageColor(long BytOffset, long /*num_images*/) 

{

  int nin, nout;
  float LowVal, HiVal;
  float FullLowVal, FullHiVal;
  long calculate_colors;
  float not_used;
  float scale_amp;
  long trace_index;
  long panel_index;


  _median_scale = 1.0;

  nin = nout = (int)(getSamplesInMemory() * getTracesInMemory());

  if(_user->getNorm() == PANELNORM)
    panel_index = BytOffset / getSamplesInMemory();
  else
    panel_index = 0;
  
  scale_amp = _amp_recovery->getScaleAmp(panel_index);

  if(_user->getDoMedian())
    {
      trace_index = BytOffset / getSamplesInMemory();
      imageMedian(scale_amp, nin, trace_index,&_median_scale,&not_used);
      LowVal = 0.0; /*wont be used*/
      HiVal  = 0.0; /*wont be used*/
      calculate_colors = False;
    }
  else
    {
      if(_user->getDoPercent())  /* use amplitude percentages*/
        {
          trace_index = BytOffset / getSamplesInMemory();
          scale_amp = _amp_recovery->getScaleAmp(panel_index);
          if(scale_amp < 0.0) scale_amp = (-scale_amp);

          if(_user->getCenterPercent())//User wants zero amps centered
            //First get the 100% case
            imagePercent(scale_amp, 100.0F, 100.0F, 
                         nin, &FullLowVal, &FullHiVal,
                         trace_index, &_histogram[0]);

          //Get the user's requested %
          imagePercent(scale_amp, _user->getPnc(), _user->getPpc(), 
                       nin, &LowVal, &HiVal,
                       trace_index, &_histogram[0]);

          if(LowVal == HiVal) HiVal += .00001;
          _user->setColorAmpmin(LowVal);
          _user->setColorAmpmax(HiVal);

          if(_user->getCenterPercent())//User wants zero amps centered
            { 
            _percent_ampmin = LowVal;
            _percent_ampmax = HiVal;
            //Now assign the low and hi to the full range for the color bar
            LowVal = FullLowVal;
            HiVal  = FullHiVal;
            }

          calculate_colors = True;
        }
      else if(_user->getDoAmps()) /*do vmin vmax type*/
        {
          if(_user->getColorAmpmin() ==  _user->getColorAmpmax()) 
            _user->setColorAmpmax( _user->getColorAmpmax() + .00001);
          LowVal = _user->getColorAmpmin();
          HiVal  = _user->getColorAmpmax();
          calculate_colors = True;
        }
      else /*use color bar trace amplitudes*/
        {
          if(_user->getColorAmpmin() ==  _user->getColorAmpmax()) 
            _user->setColorAmpmax( _user->getColorAmpmax()+.00001);
          LowVal = _user->getColorAmpmin();
          HiVal  = _user->getColorAmpmax();
          calculate_colors = False;
        }

    }



  /*map colors to data*/
  mapColors( &LowVal, &HiVal, &calculate_colors);

} 
