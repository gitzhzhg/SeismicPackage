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
 *Name   : variableArray       
 *Purpose: Interpolate and convert  float array data to a color 
 *         image format.
 *                                               
 *Author : Michael L. Sherrill
 *Date   : 10/92 (C++ version 4/97)
 *
 *Function Definition:
 *void variableArray (long frame)
 *
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
  
/*#include <X11/Xlib.h>
#include <X11/Xutil.h>*/
#include <stdio.h>
#include <stdlib.h>
#include "plot_image.hh"
#include "cprim.h"
#include "named_constants.h"
#include "interp/interp_2d_by_locs.hh"
#include "image_amplitude_recovery.hh"

#include "sl/ximage.hh"
#include "sl/paintset.hh"
#include "sl/colorset.hh"


#define OK 0
#define NOT_OK 1



int  PlotImage::variableArray(long frame, 
                              unsigned char *raster_array,
                              long rwidth,
                              long rheight, 
                              unsigned char *color_array,
                              unsigned char *color_map_array,
                              Boolean hardcopy)


{
 float addamt, increment;
 long i, j, k, l, m, o;
 long xcol0, xcol1, xpos, xdelta;
 long index;
 float diff;
 float new_val;
 long temp, firstx = 0, lastx = 0;
 int byte_val, byte_val_a, byte_val_b;
 long xhdr = 0;
 long first_to_do, last_to_do;
 int found_one = 0;
 long tindex;
 float r, fraction, factor, tempv, new_val_a, new_val_b;
 long height, width;
 long data_mem_index;
 long skip_traces = 0;
 long first_sample;
 long count, ti, nti;
 long totalpixels,rowswap;
 unsigned char *junk;
 long x;
 long mapped;
 void *out_array;
 double xval;
 float xoff, extra_col_width;
 int type;


 //Test
 Boolean trace_data = False;

// trace data means that the columns of data are equal-sized as they span
//   from _grid_x1 to _grid_x2.  this means there is one less data location
//   than for non-trace (or velocity) data

// non-trace (or velocity) data means that the columns of data are half-sized
//   at _grid_x1 and _grid_x2 (the left and right ends) and equal elsewhere

// xval is the number of grid columns per pixel (typically less than 1.0)
// _trace_delta the number of pixels per grid column (typically greater
//   than 1.0)

// xoff is zero for non-trace (or velocity) and a half column spacing for
//   trace data
  if (trace_data) {
    extra_col_width = (float)_trace_delta;
  }
  else {
    extra_col_width = (float)0;
  }

  if(hardcopy)
    {
    out_array      = (void *)raster_array;
    _cfc_colors    = (unsigned long *)color_array;
    _cfc_mapcolors = color_map_array;
    width          = rwidth;
    height         = rheight;
    xval           = (_grid_x2 - _grid_x1)
                     / ((float)width - extra_col_width);
    _cfc_type      = BYTE;
    type           = BYTE;
    }
  else
    {
    out_array = (void *)_ximage.data;
    _cfc_colors    = _col->pix;
    _cfc_mapcolors = _MapColors;
    width          = _ximage.width;
    height         = _ximage.height;
    xval           = _x_value_per_pixel;
    _cfc_type      = P2UI;
    type           = UINT;
    }
  xoff             = extra_col_width / 2.0 + 0.5;


  /*see if color map has contiguous pixel locations*/
///////////// new /////////////////////
  _cfc_contiguous_colors = contiguousColors (_cfc_type, _col->cnum,
    _cfc_colors);
///////////// new /////////////////////
/*
///////////// old /////////////////////
  _cfc_contiguous_colors = True;
  for(i=0;i<_col->cnum-1;i++)
    {
    if( abs((long)_cfc_colors[i+1] - (long)_cfc_colors[i]) > 1)
      {
      _cfc_contiguous_colors = False;
      i = _col->cnum;
      } 
    }
///////////// old /////////////////////
*/

 //If we are only contouring this image and we have an overlay trace image
 //we need to fill the ximage data array with the plane masked color and return
 if(_user->plottingContoursOnly() && _over_image != NULL && hardcopy == False)
   {
   for(i=0; i < width * height; i++)
//////////// new /////////////////////
     movePixel (_cfc_type, out_array, i, &_white_pixel);
//////////// new /////////////////////
/*
//////////// old /////////////////////
      out_array[i] = (char)_white_pixel;
//////////// old /////////////////////
*/
   return OK;
   }
 else if(_user->plottingContoursOnly())
   {
   return OK;
   }


  if(!_filedata) xhdr = _coordinate_header - 1;



  for(i=1;i<=frame;i++) skip_traces += _tpnl[i-1];
  skip_traces += (_first_trace_in_image - 1);

  if(_zoomup)
     first_sample=(int)((_zoomyary[_zindex][0]-getMemoryTmin())
                 /(_user->_G.srval * _user->getTdec()) + 1.5);
  else
     first_sample = 1;


  data_mem_index = getSamplesInMemory() * skip_traces + (first_sample - 1);


/*loop thru function locations and find the first and last to plot*/
   if(_zindex == NO_ZOOM)
     {
     first_to_do = last_to_do = 0;
     for(i=skip_traces;i<skip_traces+_ntot;i++)
        {
        firstx = (long)((_hd[i*_nhdrs+xhdr]
               - _grid_x1)/xval+xoff);
        if(firstx >= 0)
           {
           first_to_do = i;
           i = skip_traces+_ntot;
           found_one = True;
           if(firstx >= width)firstx = width - 1;
           if(lastx  >= width)lastx  = width - 1;
           }
        }
     for(i=skip_traces+_ntot-1;i>=skip_traces;i--)
        {
        lastx  = (long)((_hd[i*_nhdrs+xhdr] 
               - _grid_x1) / xval + xoff);
        if(lastx <= width && lastx > 0)
           {
           last_to_do = i;
           i = -1;
           found_one = True;
           } 
        }
     if(!found_one)
       {
       printf("Error in image_iso requested range not in data\n");
       return(NOT_OK);
       }
     }
   else
     {
     first_to_do = (long)(_zoomxary[_zindex][0] - 1);
     last_to_do  = (long)(_zoomxary[_zindex][1] - 1);
     firstx = (long)((_hd[first_to_do*_nhdrs+xhdr] 
              - _grid_x1)/xval+xoff);
     lastx  = (long)((_hd[last_to_do*_nhdrs+xhdr] 
              - _grid_x1)/xval+xoff);
     if(firstx >= width)firstx = width - 1;
     if(lastx  >= width)lastx  = width - 1;
     }

   if(lastx < firstx)    
      {
      temp  = firstx; 
      firstx= lastx;
      lastx = temp;
      _user->setRightToLeft(True);
      }
   else if(_zindex != NO_ZOOM)
      {
      _user->setRightToLeft(False);
      }

   if(first_to_do > last_to_do)
      {
      temp = first_to_do;
      first_to_do = last_to_do;
      last_to_do = temp;
      }

/* determine the relationship between the indices in the float array and
   the indices in the output image */
   float first[2], last[2];
   int size[2], stype;

   first[0] = (int)((_hd[first_to_do*_nhdrs+xhdr] - _grid_x1) / xval
     + 0.5); // on _grid_x1
   first[1] = (int)((float)(1 - first_sample)
     * (float)(height - 1) / (float)(_nsamp - 1) + 0.5);
   last [0] = (int)((_hd[last_to_do*_nhdrs+xhdr] 
     - _grid_x1) / xval + extra_col_width + 0.5); // on _grid_x2
   last [1] = first[1] + (int)((float)(getSamplesInMemory() - 1)
     * (float)(height - 1) / (float)(_nsamp - 1) + 0.5);

   size [0] = last_to_do - first_to_do + 1;
   size [1] = getSamplesInMemory ();

/* determine the interpolation or sampling type */
   if (_user->getGradeVertical()) {
     if (_user->getGradeHorizontal()) {
       stype = Interpolator2D::LIN;
     }
     else {
       stype = Interpolator2D::NN_LIN;
     }
   }
   else {
     if (_user->getGradeHorizontal()) {
       stype = Interpolator2D::LIN_NN;
     }
     else {
       stype = Interpolator2D::NN;
     }
   }

/* construct the interpolator and assign the float array to it */
   Interp2DByLocs *interpolator = new Interp2DByLocs (first, last, size,
     FNIL, stype, 0, _interp_lt_2D);

   interpolator->pointToBlock (
     &(_float_array[first_to_do*getSamplesInMemory()]));

/*scale factor to fit vels into byte values*/
   if (!trace_data) {
     _cfc_amp_min = min(_user->getColorAmpmin(),_user->getColorAmpmax());
     _cfc_amp_max = max(_user->getColorAmpmin(),_user->getColorAmpmax()); 

     if (_cfc_amp_min == _cfc_amp_max)  {
       printf("Warning color min and max are equal in function image_iso\n");
       _cfc_scf = 1.0;
     }
     else {
       _cfc_scf = 254.0 / (_cfc_amp_max - _cfc_amp_min);
     }
   }
   else {
     interpolator->setCorrectionFunction (amplitudeCorrection, this);
     _cfc_amp_min = -1;
     _cfc_amp_max = +1;
     _cfc_scf = 127.0;
   }
//////////// new /////////////////////
   assert (_ximage_ptr->colorset());
   _ximage_ptr->colorset()->setFirstValue (_cfc_amp_min);
   _ximage_ptr->colorset()->setLastValue  (_cfc_amp_max);
//////////// new /////////////////////

   first[1] = 0;
   last [1] = height - 1;
   size[0] = 1;
   size[1] = height;

/* mark locations of each column */
  float *xcols = (float *)malloc (sizeof(float)*(last_to_do-first_to_do+1));
//////////// new /////////////////////
  void *byte_array = allocatePixels (type, height);
//////////// new /////////////////////
/*
//////////// old /////////////////////
  unsigned char *byte_array = (unsigned char *)malloc (
    sizeof(unsigned char)*height);
//////////// old /////////////////////
*/
  if (!xcols || !byte_array) {
    printf ("Error in image_varray failure to allocate memory\n");
    return (NOT_OK);
  }

  count = 0;
  for (i = first_to_do; i <= last_to_do; i++) {
    if (!_user->getRightToLeft()) {
      ti = i;
    }
    else {
      ti = last_to_do - count;
    }
    xcols[count] = (int)((_hd[ti *_nhdrs+xhdr] - _grid_x1)
      / xval + xoff);
    count++;
  } /*end for of i to last_to_do*/
  int isize[2];
  isize[0] = count;  // length of x-column array
  isize[1] = height; // length of y-row    array
  interpolator->setLocs (0, xcols, isize);
  free (xcols);

/* fill image array */
  int pix_size;
  switch (_cfc_type) {
  case BYTE :
    pix_size = sizeof (unsigned char);
    break;
  case P2UI :
    pix_size = sizeof (unsigned int);
    break;
  default:
    assert (0);
  }
  interpolator->setConversionFunction (convertFloatToPix, this, pix_size);
  count = 0;
  for (i = first_to_do; i <= last_to_do; i++) {
    if (i == first_to_do) {
      if (i == last_to_do) {
// obscure case where the data is really 1-D NOT 2-D
	xcol0 = 0;
	xcol1 = width;
      }
      else {
        if (!_user->getRightToLeft()) {
	  _ac_nti = first_to_do + 1;
        }
        else {
          _ac_nti  = last_to_do - 1;
        }
        xcol0 = 0;
        xcol1 = (int)((_hd[_ac_nti*_nhdrs+xhdr] - _grid_x1)
          / xval + xoff);
      }
    }
    else if (i == last_to_do) {
      if (!_user->getRightToLeft()) {
	_ac_nti = last_to_do;
      }
      else {
	_ac_nti = first_to_do;
      }
      xcol0 = (int)((_hd[_ac_nti*_nhdrs+xhdr] - _grid_x1)
        / xval + xoff);
      xcol1 = width;
    }
    else {
      if (!_user->getRightToLeft()) {
        _ac_ti  = i;
        _ac_nti = _ac_ti + 1;
      }
      else {
        _ac_ti = last_to_do - count;
        _ac_nti= _ac_ti - 1;
      }
      xcol0 = (int)((_hd[_ac_ti *_nhdrs+xhdr] - _grid_x1)
        / xval + xoff);
      xcol1 = (int)((_hd[_ac_nti*_nhdrs+xhdr] - _grid_x1)
        / xval + xoff);
    }
    xdelta = xcol1 - xcol0;

    for (k = 0; k < xdelta; k++) {
      first[0] = (float)xcol0;
      last [0] = (float)xcol0;
/*
//////////// old /////////////////////
      _cfc_default = out_array[xcol0];
//////////// old /////////////////////
*/
//////////// new /////////////////////
      movePixel (type, &_cfc_default, 0, out_array, xcol0);
//////////// new /////////////////////

      interpolator->getValues (byte_array, first, last, size);

      xpos = xcol0;
      for (j = 0; j < height; j++) {
//////////// new /////////////////////
	movePixel (type, out_array, xpos, byte_array, j);
//////////// new /////////////////////
/*
//////////// old /////////////////////
	out_array[xpos] = byte_array[j];
//////////// old /////////////////////
*/
	xpos += width;
      }
      xcol0 += 1;
    } /*end for of k to xdelta */
    count++;
  }/*end for of i to last_to_do*/
  free (byte_array);
  delete interpolator;

/*****flip image y axis if requested*****/
  if(_user->getInvertedYaxis())
   {
   totalpixels = height * width;
   junk = NULL;
   junk = (unsigned char *)malloc(width * sizeof(unsigned char));
   if(junk == NULL)
      {
      printf("could not allocate memory for invert y in image_iso\n");
      return(NOT_OK);
      }
   rowswap = (int)((float)height / 2.0 + 0.9);
   for(i=0;i<rowswap;i++)
      {
      x = i * width;
      k = totalpixels - width - x;
      for(j=0;j<width;j++)
         {
//////////// new /////////////////////
	 movePixel (type, junk,      j,   out_array, j+x);
	 movePixel (type, out_array, j+x, out_array, k  );
	 movePixel (type, out_array, k,   junk,      j  );
//////////// new /////////////////////
/*
//////////// old /////////////////////
         junk[j] = out_array[j+x];
         out_array[j+x] = out_array[k];     
         out_array[k] = junk[j];
//////////// old /////////////////////
*/
         k++;
         }
      }
    free(junk);
   }/*end flip*/
  return(OK);
}

// checks to see if Pixel values are consecutive - only important for
//   8-bit Displays
Boolean PlotImage::contiguousColors (int type, int num_colors, Pixel *colors)
{
  Boolean retval;

  int k2;

  switch (type) {
  case BYTE :
    for (k2 = 0, retval = True; k2 < num_colors-1; k2++) {
      if (abs((long)colors[k2+1]-(long)colors[k2]) > 1) {
	retval = False;
	k2 = num_colors;
      }
    }
    break;
  case PIXEL :
  case P2UI :
  case UINT :
    // not important for 24-bit displays
    retval = True;
    break;
  default:
    assert (0);
  }
  return retval;
}

void PlotImage::convertFloatToPix (void *pi_obj, void *void_out,
  float value_in)
{
  PlotImage *obj = (PlotImage *)pi_obj;

  switch (obj->_cfc_type) {
  case BYTE :
    convertFloatToChar (pi_obj, void_out, value_in);
    break;
  case PIXEL :
    convertFloatToPixel (pi_obj, void_out, value_in);
    break;
  case P2UI :
  case UINT :
    convertFloatToInt (pi_obj, void_out, value_in);
    break;
  default:
    assert (0);
  }
}

void PlotImage::convertFloatToChar (void *pi_obj, void *void_out,
  float value_in)
{
  PlotImage *obj = (PlotImage *)pi_obj;
  if (value_in < obj->_cfc_amp_min) value_in = obj->_cfc_amp_min;
  if (value_in > obj->_cfc_amp_max) value_in = obj->_cfc_amp_max;
  int byte_val = (int)(obj->_cfc_scf * (value_in - obj->_cfc_amp_min) + 1.5);
  int result;

  unsigned char *value_out = (unsigned char *)void_out;

  if (obj->_cfc_contiguous_colors) {
    *value_out
      = (unsigned char)(obj->_cfc_colors[obj->_cfc_mapcolors[byte_val]]);
  }
  else { /*allocated pixels are not contiguous find closest*/
    int o;
    int mapped = 0;
    result = (int)(obj->_cfc_colors[obj->_cfc_mapcolors[byte_val]]);
                       
    for (o = 0; o < obj->_col->cnum-1; o++) {
      if (result >= (int)obj->_cfc_colors[o  ]&&
	  result <= (int)obj->_cfc_colors[o+1]  ) {
	mapped = o + 1;
	o = obj->_col->cnum;
      }
    }
    if (mapped) {
      if (abs(result-(int)obj->_cfc_colors[mapped-1]) <
	  abs(result-(int)obj->_cfc_colors[mapped  ])   ) {
	*value_out
	  = (unsigned char)(obj->_cfc_colors[mapped-1]);
      }
      else {
	*value_out
	  = (unsigned char)(obj->_cfc_colors[mapped  ]);
      }
    }
    else {
      *value_out = obj->_cfc_default;
    }
  }
}

void PlotImage::convertFloatToPixel (void *pi_obj, void *void_out,
  float value_in)
{
  PlotImage *obj = (PlotImage *)pi_obj;
  if (value_in < obj->_cfc_amp_min) value_in = obj->_cfc_amp_min;
  if (value_in > obj->_cfc_amp_max) value_in = obj->_cfc_amp_max;
  int byte_val = (int)(obj->_cfc_scf * (value_in - obj->_cfc_amp_min) + 1.5);

  Pixel *value_out = (Pixel *)void_out;
  Colorset *colorset = obj->_ximage_ptr->colorset ();

  int icolor = obj->_cfc_mapcolors[byte_val];
  *value_out = colorset->getPixel (icolor);
}

void PlotImage::convertFloatToInt (void *pi_obj, void *void_out,
  float value_in)
{
  PlotImage *obj = (PlotImage *)pi_obj;
  if (value_in < obj->_cfc_amp_min) value_in = obj->_cfc_amp_min;
  if (value_in > obj->_cfc_amp_max) value_in = obj->_cfc_amp_max;
  int byte_val = (int)(obj->_cfc_scf * (value_in - obj->_cfc_amp_min) + 1.5);

  unsigned int *value_out = (unsigned int *)void_out;
  Colorset *colorset = obj->_ximage_ptr->colorset ();

  int icolor = obj->_cfc_mapcolors[byte_val];
  *value_out = (unsigned int)colorset->getPixel (icolor);
}

void PlotImage::amplitudeCorrection (void *pi_obj, float *value_out,
  float value_in, int which_index)
{
  PlotImage *obj = (PlotImage *)pi_obj;
  long ti;

  switch (which_index) {
  case Interp2DByLocs::LOC0:
    ti = obj->_ac_ti;
    break;
  case Interp2DByLocs::LOC1:
    ti = obj->_ac_nti;
    break;
  default:
    assert (0);
  }
  *value_out = (float)obj->_amp_recovery->getAfloat (value_in, ti,
    obj->_nsamp);
}
