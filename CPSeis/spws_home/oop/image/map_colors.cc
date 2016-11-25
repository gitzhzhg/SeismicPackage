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
 *Name   : mapColors     
 *Purpose: Map byte values into available colors via a remap array. 
 * 
 *
 *Author : Michael L. Sherrill
 *Date   : 02/92 (C++ version 4/97)
 *
 * Function Definition:
 *
 * void map_colors(float *LowAmp, float *HiAmp, long *calculate_colors)
 * 
 * LowAmp     in         Lowest data amplitude for remap.
 * HiAmp      in         Highest data amplitude for remap. 
 * calculate  in         Flag to use predefined amplitudes or calculate.
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



#include "plot_image.hh"
#include "image_amplitude_recovery.hh"
#include <X11/Xlib.h>
#include <X11/Xutil.h>
#include "cprim.h"
#include "sl/sl_error_pop.hh"

#define NORM_WARNING "The values annotated on the color bar pop up may not\n\
be accurate due to the normalization scaling option\n\
you have selected. This is a one time warning that will\n\
not appear on subsequent plots."





void PlotImage::mapColors(float *LowAmp, float *HiAmp, long *calculate_colors)

{
Display *dpy;
long j = 1;
unsigned char i;
float factor;
float LowVal, HiVal;
float ByteRange = 127.0;
float MidByte   = 128.0;
float round_amt = 0.0;
float newvalue;
long limit;
long LowIndex, HiIndex; 
float lowabs;
float MinusVel;
float amp_min, amp_max;
float scale_amp;    
long panel_index;
static Boolean show_warning = True;


  if(_user->getNorm() == PANELNORM)
    panel_index = _cpixm;
  else
    panel_index = 0;

  dpy = XtDisplay(_graphic);
  limit  = 3; /*subscript for float values in color bar array if any*/
  lowabs = *LowAmp * -1.0;

  //If doing percents try to keep the middle of the color bar at the
  //zero amplitude area.
  if(_user->getDoPercent() && *calculate_colors && _user->getCenterPercent()) 
    {
      factor = lowabs  / (((float)_user->_num_cbar_cols-1.0F) / 2.0F);
      newvalue = *LowAmp;
      for(i = 0; i < _user->_num_cbar_cols / 2; i++)
        { 
          _user->_cbar_rgb[limit] = newvalue;
          newvalue += factor;
          limit += 4;
        } 

      factor = *HiAmp  / (((float)_user->_num_cbar_cols-1.0F) / 2.0F);
      newvalue = 0.0F;
      for(i = _user->_num_cbar_cols / 2; i < _user->_num_cbar_cols; i++)
        { 
          _user->_cbar_rgb[limit] = newvalue;
          newvalue += factor;
          limit += 4;
        } 
    }
  else
    {
      factor = (lowabs + *HiAmp) / ((float)_user->_num_cbar_cols-2.0);   

      if(*calculate_colors)
        {
          newvalue = *LowAmp;
          for(i=0;i<_user->_num_cbar_cols;i++)
            { 
              _user->_cbar_rgb[limit] = newvalue;
              newvalue += factor;
              limit += 4;
            } 
        }
    }


  limit    = 3;  
  if(_user->_mode == PlotISO) /*velocity plotting*/
     {
       // if(*calculate_colors)
       //{
       //amp_min = _user->_cbar_rgb[3];
       //amp_max = _user->_cbar_rgb[_user->_num_cbar_cols * 4 - 1];
       // }
       //else
       //{
       amp_min = min(_user->_color_ampmin,_user->_color_ampmax);
       amp_max = max(_user->_color_ampmin,_user->_color_ampmax);
       // }
     factor = (amp_max - amp_min) / 254.0;
     MidByte= 0.0;
     LowVal = 0.0;
     MinusVel = amp_min;
     round_amt = 1.5;
     }
  else
     {
     //If norm there is no single scale amp since amps are scaled to
     //largest value in each trace so we use the color bar max amp instead
     //See notes about norm at bottom of this file.
     if(_user->getNorm() == NORM)// || _user->getNorm() == PANELNORM)
       scale_amp = _user->_cbar_rgb[_user->_num_cbar_cols * 4 - 1];
     else
       scale_amp = _amp_recovery->getScaleAmp(panel_index);
     if(scale_amp == 0.0) scale_amp = .00001; //prevent divide by zero
     factor   = scale_amp / ByteRange;
     LowVal   = -scale_amp / _median_scale;
     MinusVel = 0.0;
     round_amt = 0.5;
     }

  LowIndex = (long)( (LowVal / factor) + MidByte + round_amt);
  LowIndex = min(255,LowIndex);
  LowIndex = max(1,LowIndex);

  HiVal    = _user->_cbar_rgb[limit] / _median_scale;
  HiIndex  = (long)( ((HiVal - MinusVel) / factor) + MidByte + round_amt);
  HiIndex  = min(255,HiIndex);
  HiIndex  = max(1,HiIndex);



/*  Info about the rest of this source file....
  1. The code is stopped if we have mapped the highest byte.
  2. The MapColors array should have 1 thru 255 elements mapped to 
     0 thru num_cbar_colors - 1;
*/    

  if(LowIndex > 1) /*make sure all lower array values are set*/
    {
    for(j=1;j<LowIndex;j++)_MapColors[j] = 0;
    }

  for(i = 0; i < _user->_num_cbar_cols; i++)
     { 
     for(j=LowIndex;j<=HiIndex;j++) _MapColors[j] = i;
     if(HiIndex==255 && i>0)i = (unsigned char)_user->_num_cbar_cols-1;//stop
     limit += 4;
     HiVal    = _user->_cbar_rgb[limit] / _median_scale;
     LowIndex = HiIndex + 1;
     LowIndex = min(255,LowIndex);
     LowIndex = max(1,LowIndex);
     HiIndex  = (long)( ((HiVal - MinusVel) / factor) + MidByte + round_amt);
     HiIndex  = min(255,HiIndex);
     HiIndex  = max(1,HiIndex);
     }

  if(j - 1 < 255) /*make sure all upper array values are set*/
    {
    for(j = j-1;j<256;j++)_MapColors[j] = i - 1;
    }

  //If the user has requested normalization or external norm the color
  //bar no longer reflects a true mapping. For instance in the norm case
  //each trace will have a new value of 255 somewhere in it. That 255
  //will correspond to a different real value in the next trace, so if
  //red were 255, a red on one trace would be for a different value than
  //for a red in the next trace. This means we would need a separate
  //color bar popup for each trace to be accurate. If the user scales
  //data to an external amplitude that is lower than an acutal data
  //value those values will be affected the same way as the norm option.
  //Really these two options should not
  //be supported when doing color but users desire to have them available
  //when they need to scale up a weak display.
  //The plot array type displays are the exception since code scales 
  //that data with an external amplitude that is physically in the data.
  if(_user->getMode() != PlotImage::PlotARRAY && show_warning == True &&
     _user->getNorm() != FILENORM)
    {
    SLErrorPop *temp = new SLErrorPop(graphicWidget(),"warning",NORM_WARNING);
    show_warning = False;//Show it only one time
    }


  //Initialize the float color map array
  colorIndexOfFloat(1.0, True);

}


