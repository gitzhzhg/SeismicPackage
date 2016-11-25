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


/*------------------------------------------------------------------
 *USER DOC
 *Name   : colorIndexOfFloat
 *Purpose: Map a float value into the color bar
 *Author : Michael L. Sherrill
 *Date   : 04/2002
 *
 * Function Definition:
 *int colorIndexOfFloat(float value, Boolean initialize)
 *
 * value           in         Float data value to find the index of.
 * initialize      in         Flag to initialize the color bar values.
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
#include "binary_search.h"



int PlotImage::colorIndexOfFloat(float value, Boolean initialize)
{
  int i, j, index;
  int stride = 4;
  long numcolors = _user->_num_cbar_cols;
  static float colorvals[256];
  float tolerance = 0.0F;
  long index_a = 0, index_b = 0;
  static int mid_index;
  

  assert(numcolors < 255);

  if(initialize)
    {
      j = stride - 1;
      for(i = 0; i < numcolors; i++)
        {
          colorvals[i] = _user->_cbar_rgb[j];
          j += stride;
        }

      mid_index = (float)numcolors / 2.0F;

    }

  //If doing percents place any value that is less than the
  //percent min or greater than the percent max at the middle of
  //the color bar if requested by user
  if(_user->getDoPercent() && _user->getCenterPercent())
    {
      if(value < _percent_ampmin || value > _percent_ampmax ||
         value == 0.0F)
        return mid_index;
    }

  //See if value is less than color bar
  if(value <= colorvals[0])
    {
      index = 0;
      return index;
    }

  //See if value is more than color bar
  if(value >= colorvals[numcolors - 1]) 
    {
      index = numcolors - 1;
      return index;
    }

  //Somewhere in the middle
  binary_search_floats(colorvals, value, tolerance, numcolors, &index_a,
                       &index_b);
  
  return (int)index_b;

} 
