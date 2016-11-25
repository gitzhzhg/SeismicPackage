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
#include "plot_image.hh"
#include "image_data_analysis.hh"

#include <Xm/Xm.h>
#include <assert.h>
#include <stdlib.h>

void PlotImage::movePixel (int type, void *to, int to_index, void *from,
  int from_index)
{
  unsigned char *uc_to_array;
  unsigned char *uc_from_array;
  Pixel *p_to_array;
  // with IA64, this got more complicated because it appears
  // X pixmaps only use 32 bit entities but a Pixel is an unsigned long!!!!!
  Pixel *p_from_array;
  unsigned int *ui_to_array;
  unsigned int *ui_from_array;

  switch (type) {
  case BYTE :
    uc_to_array   = (unsigned char *)to;
    uc_from_array = (unsigned char *)from;
    uc_to_array[to_index] = uc_from_array[from_index];
    break;
  case PIXEL :
    p_to_array   = (Pixel *)to;
    p_from_array = (Pixel *)from;
    p_to_array[to_index] = p_from_array[from_index];
    break;
  case P2UI :
    ui_to_array  = (unsigned int *)to;
    p_from_array = (Pixel *)from;
    ui_to_array[to_index] = (unsigned int)p_from_array[from_index];
    break;
  case UINT :
    ui_to_array   = (unsigned int *)to;
    ui_from_array = (unsigned int *)from;
    ui_to_array[to_index] = ui_from_array[from_index];
    break;
  default:
    assert (0);
  }
}

void *PlotImage::allocatePixels (int type, int num_pixels)
{
  void *retval;

  switch (type) {
  case BYTE :
    retval = malloc (num_pixels*sizeof(unsigned char));
    break;
  case PIXEL :
    retval = malloc (num_pixels*sizeof(Pixel));
    break;
  case UINT :
    retval = malloc (num_pixels*sizeof(unsigned int));
    break;
  default:
    assert (0);
  }
  return retval;
}

unsigned long PlotImage::getPixel (int type, void *array, int array_index)
{
  unsigned long retval;

  unsigned char *uc_array;
  Pixel *p_array;
  unsigned int *ui_array;

  switch (type) {
  case BYTE :
    uc_array = (unsigned char *)array;
    retval = (unsigned long)(uc_array[array_index]);
    break;
  case PIXEL :
    p_array = (Pixel *)array;
    retval = (unsigned long)(p_array[array_index]);
    break;
  case UINT :
    ui_array = (unsigned int *)array;
    retval = (unsigned long)(ui_array[array_index]);
    break;
  default:
    assert (0);
  }
  return retval;
}
