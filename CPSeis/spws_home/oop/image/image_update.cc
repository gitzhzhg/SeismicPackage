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
 *Name   : updateImage
 *Purpose: Interpolate and convert velocity float data to a color
 *         image format. This function creates an image which is
 *         a sub image of an existing color array plot, and then replaces
 *         that portion of the plot.
 *         NOTE: input parameters are 1 to n, not 0 to n-1
 *
 *Author : Michael L. Sherrill
 *Date   : 10/92 (C++ version 4/97)
 *
 *Function Definition:
 *void updateImage     (long              start_column,
 *                      long              end_column,
 *                      long              starting_sample,
 *                      long              ending_sample)
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

#include <stdio.h>
#include <stdlib.h>
#include "plot_image.hh"
#include "named_constants.h"
#include "interp/interp_2d_by_locs.hh"
///////////////// new /////////////////////////
#include "pixmap_set.hh"
#include "sl/colorset.hh"
#include "sl/ximage.hh"
///////////////// new /////////////////////////

#define OK 1
#define NOT_OK 0


long PlotImage::updateImage(long              start_column,
                            long              end_column,
                            long              starting_sample,
                            long              ending_sample,
                            long              pixmap_index,
                            long              *expose_x,
                            long              *expose_y,
                            long              *expose_width,
                            long              *expose_height)
{
float addamt, increment;
long i, j, k, l, m, n, o;
long start, end;
long xcol0, xcol1, xpos, xdelta;
long index;
float diff, xbin;
float new_val;
long firstx, lastx;
long backx = 0;
int byte_val, byte_val_a, byte_val_b;
long xhdr = _user->_hdrOne - 1;
long firstindex, lastindex;
Display *dpy;
/*
///////////////// old /////////////////////////
int screen;
///////////////// old /////////////////////////
*/
XImage newximage;
///////////////// new /////////////////////////
Ximage *newximage_ptr;
Screen *screen;
///////////////// new /////////////////////////
long  y1;
long first_column_in_image, last_column_in_image;
long tindex, ntindex, hindex, height;
float r, fraction, factor, tempv, new_val_a, new_val_b;
float gmin, gmax;
long which, frameskip;
long skip_traces = 0, hoffset;
int right_to_left = 0;
long count = 0;
long ti, nti;
long mapped;
unsigned int *out_array;
double xval;
float xoff, extra_col_width;

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

  _cfc_colors    = _col->pix;
  _cfc_mapcolors = _MapColors;
  _cfc_type      = UINT;
  xval           = _x_value_per_pixel;
  xoff           = extra_col_width / 2.0 + 0.5;

/*see if color map has contiguous colors*/
///////////////// new /////////////////////////
  _cfc_contiguous_colors = contiguousColors (_cfc_type, _col->cnum, _col->pix);
///////////////// new /////////////////////////
/*
///////////////// old /////////////////////////
  _cfc_contiguous_colors = True;
  for (i = 0; i < _col->cnum-1; i++) {
    if (abs((long)_cfc_colors[i+1]-(long)_cfc_colors[i]) > 1) {
      _cfc_contiguous_colors = False;
      i = _col->cnum;
    }
  }
///////////////// old /////////////////////////
*/

  if ( !_displayed ) return(NOT_OK);

  if(_user->_movie)
     which = pixmap_index + 1;
  else
     which = 0;

  if(which)
     {
     for(i=1;i<which;i++) skip_traces += _tpnl[i-1];
     frameskip = _nsamp * skip_traces;
     hoffset   = _nhdrs * skip_traces;
     }
  else
     {
     frameskip = 0;
     hoffset = 0;
     }

  start = end = OK;


/*see if y area is in image*/
  if(starting_sample < _Cl.samp1 || starting_sample > _nsamp)
     {
     start = NOT_OK;
     starting_sample =  _Cl.samp1;
     }
  if(ending_sample   < _Cl.samp1 || ending_sample   > _nsamp)
     {
     end   = NOT_OK;
     ending_sample = _nsamp;
     }
  if(start == NOT_OK && end == NOT_OK) return(NOT_OK);


/*Find out what columns are in the image, currently I update one column
  less and one column more than I really need to in case of bin center
  roundoffs.*/
  gmin = min(_grid_x1, _grid_x2);
  gmax = max(_grid_x1, _grid_x2);

  first_column_in_image = last_column_in_image = 0;
  for(i=0;i<_tpnl[pixmap_index];i++)
     {
     xbin = _hd[i*_nhdrs+xhdr+hoffset];
     if(xbin >=  gmin && xbin <= gmax)
        {
        first_column_in_image = max(i,1);
        i = _tpnl[pixmap_index]; /*stop loop*/
        }
     }

  for(i=_tpnl[pixmap_index]-1;i>=0;i--)
     {
     xbin = _hd[i*_nhdrs+xhdr+hoffset];
     if(xbin >=  gmin && xbin <= gmax)
        {
        last_column_in_image = min(i + 2,_tpnl[pixmap_index]);
        i = -1; /*stop loop*/
        }
     }

  // start_column -= skip_traces;
  //end_column   -= skip_traces;

  if(start_column < first_column_in_image && end_column < first_column_in_image)
     return(NOT_OK);
  if(start_column > last_column_in_image && end_column > last_column_in_image)
     return(NOT_OK);

  if(start_column - first_column_in_image > 0)
     firstindex = start_column - 2; //  firstindex is zero rel. & one less
  else
     firstindex = first_column_in_image - 1;

  if(last_column_in_image - end_column > 0)
     lastindex = end_column; // lastindex is zero rel. & one more
  else
     lastindex = last_column_in_image - 1;

  if(firstindex > lastindex)
    {
    long junk  = lastindex;
    lastindex  = firstindex;
    firstindex = junk;
    right_to_left = 1;
    }

  firstx = (long)((_hd[firstindex*_nhdrs+xhdr+hoffset]
           -  _grid_x1) / xval + xoff);

  lastx  = (long)((_hd[lastindex*_nhdrs+xhdr+hoffset]
         - _grid_x1) / xval + xoff);

  if(firstx > lastx)
    {
    long junk = lastx;
    lastx      = firstx;
    firstx     = junk;
    right_to_left = 1;
    }


  if(firstx > 0 && firstindex+1 == first_column_in_image)
     {
     backx = firstx;
     firstx = 0;
     }

  if(lastx+1 < _ximage.width && lastindex+1 == last_column_in_image)
     lastx = _ximage.width - 1;

  height = _ximage.height;
  factor = (float)(_nsamp - 1) / (float)(height - 1);
  int firsty, lasty;
  firsty = (int)((starting_sample - 1) / factor + 0.5);
   lasty = (int)((  ending_sample - 1) / factor + 0.5);

  if (firsty > lasty) {
    int temp = lasty;
    lasty = firsty;
    firsty = temp;
  }

  if (firsty < 0       ) firsty = 0         ;
  if ( lasty > height-1)  lasty = height - 1;

  dpy = XtDisplay(_graphic);
/*
//////////////////// old ////////////////////
  screen = DefaultScreen(XtDisplay(_graphic));
//////////////////// old ////////////////////
*/
//////////////////// new ////////////////////
  screen = XtScreen (_graphic);
  assert (_ximage_ptr->colorInfo());
  newximage_ptr = new Ximage (_ximage_ptr->colorInfo());
  newximage_ptr->assignTo (&newximage);
//////////////////// new ////////////////////
/*
  if(_can_overlay)
    height = _ximage.height - _height_diff;
  else
*/

  newximage.width  = (int)(lastx - firstx + 1);
  newximage.height = (int)(lasty - firsty + 1);
  newximage.format = ZPixmap;
  int depth        = DefaultDepth (dpy, DefaultScreen(dpy));
  newximage.depth  = depth;

/*//////////////// old ////////////////////////////////
  newximage.bytes_per_line = newximage.width;
  newximage.bits_per_pixel = _ximage.depth;
  newximage.xoffset = 0;
  newximage.bitmap_unit = 8;
  newximage.bitmap_pad = 0;
  newximage.byte_order = MSBFirst;
  newximage.bitmap_bit_order = MSBFirst;
  newximage.data = NULL;
  newximage.data = (char *) malloc( newximage.height *
                     newximage.bytes_per_line * sizeof(char));
*//////////////// old ////////////////////////////////

///////////////// new ////////////////////////////////
    newximage_ptr->allocate (&newximage);
///////////////// new ////////////////////////////////

  if(newximage.data == NULL)
     {
     /*need to handle error better later*/
     printf("allocation error in image_update\n");
     delete newximage_ptr;
     return(NOT_OK);
     }

  out_array = (unsigned int *)newximage.data;

/* determine the relationship between the indices in the float array and
   the indices in the Ximage */
  float first[2], last[2], x0, x1;
  int size[2], stype;

  x0 = (_hd[xhdr+hoffset] - _grid_x1) / xval; // on _grid_x1
  x1 = (_hd[(_tpnl[pixmap_index]-1)*_nhdrs+xhdr+hoffset] - _grid_x1)
    / xval + extra_col_width; // on _grid_x2
  first[0] = (int)((x1 - x0) / (float)(_tpnl[pixmap_index]-1)
    * (0.0 - (float)frameskip) + x0 + 0.5);
  last [0] = (int)((x1 - x0) / (float)(_tpnl[pixmap_index]-1)
    * (float)(getTracesInMemory() - 1 - frameskip) + x0 + 0.5);
  first[1] = 0;
  last [1] = (int)((float)(getSamplesInMemory() - 1)
    * (height - 1) / (float)(_nsamp - 1) + 0.5);

  size [0] = getTracesInMemory  ();
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

   interpolator->pointToBlock (_float_array);

/*scale factor to fit vels into byte values*/
   if (!trace_data) {
     _cfc_amp_min = min(_user->getColorAmpmin(),_user->getColorAmpmax());
     _cfc_amp_max = max(_user->getColorAmpmin(),_user->getColorAmpmax());

     if (_cfc_amp_min == _cfc_amp_max)  {
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
     _cfc_scf = 127;
   }

///////////////// new /////////////////////////
   _ximage_ptr->colorset()->setFirstValue (_cfc_amp_min);
   _ximage_ptr->colorset()->setLastValue  (_cfc_amp_max);
///////////////// new /////////////////////////

   first[1] = firsty;
   last [1] =  lasty;
   size[0] = 1;
   size[1] = newximage.height;

/* mark locations of each column */
   float *xcols = (float *)malloc (sizeof(float)*(lastindex-firstindex+1));
   unsigned int *pixel_array = (unsigned int *)malloc (sizeof(unsigned int)
     *height);
   if (!xcols || !pixel_array) {
     printf ("Error in image_update failure to allocate memory\n");
     delete newximage_ptr;
     return (NOT_OK);
   }
  count = 0;
  for (index = firstindex; index <= lastindex; index++) {
    if (!right_to_left) {
      tindex  = index;
    }
    else {
      tindex  = lastindex - count;
    }
    xcols[count] = (int)((_hd[tindex *_nhdrs+xhdr+hoffset]
      - _grid_x1) / xval + xoff);
    count++;
  } /*end for of index to lastindex */
  int isize[2];
  isize[0] = count;            // length of x-column array
  isize[1] = newximage.height; // length of y-row    array
  interpolator->setLocs (0, xcols, isize);
  free (xcols);

/*fill image array */
  interpolator->setConversionFunction (convertFloatToPixel, this,
    sizeof(Pixel));
  count = 0;
  for (index = firstindex; index <= lastindex; index++) {
    if (index == firstindex) {
      if (index == lastindex) {
	xcol0 = firstx;
        xcol1 = firstx + newximage.width;
      }
      else {
	if (!right_to_left) {
	  tindex  = firstindex + 1;
	}
	else {
	  tindex  = lastindex - 1;
	}
	xcol0 = firstx;
	xcol1 = (int)((_hd[tindex *_nhdrs+xhdr+hoffset]
	  - _grid_x1) / xval + xoff);
      }
    }
    else if (index == lastindex) {
      if (!right_to_left) {
        tindex  = lastindex;
      }
      else {
        tindex  = firstindex;
      }
      xcol0 = (int)((_hd[tindex *_nhdrs+xhdr+hoffset]
        - _grid_x1) / xval + xoff);
      xcol1 = firstx + newximage.width;
    }
    else {
      if (!right_to_left) {
        tindex  = index;
	ntindex = tindex + 1;
      }
      else {
        tindex  = lastindex - count;
	ntindex = tindex - 1;
      }
      xcol0 = (int)((_hd[tindex *_nhdrs+xhdr+hoffset]
        - _grid_x1) / xval + xoff);
      xcol1 = (int)((_hd[ntindex*_nhdrs+xhdr+hoffset]
	- _grid_x1) / xval + xoff);
    }
    xdelta = xcol1 - xcol0;

    for (k = 0; k < xdelta; k++) {
      first[0] = (float)xcol0;
      last [0] = (float)xcol0;
      _cfc_default = (unsigned int)out_array[xcol0-firstx];
      interpolator->getValues ((void *)pixel_array, first, last, size);

      xpos = xcol0 - firstx;
      for (j = 0; j < newximage.height; j++) {
	out_array[xpos] = pixel_array[j]; // may need Ximage to do put
	xpos += newximage.width;
      }
      xcol0 += 1;
    } /*end for of k to xdelta */
    count++;
  } /*end for of index to lastindex */
  free (pixel_array);
  delete interpolator;

  y1 = (long)(firsty + _top_border);
/*
////////////////// old /////////////////////////
  wpPutImage( dpy, _pixmary[pixmap_index], _gc1,
             &newximage, 0, 0, (int)(firstx+_left_border),
             (int)(y1), newximage.width,
	     newximage.height );
////////////////// old /////////////////////////
*/
////////////////// new /////////////////////////
  int old_cpixm = pixmap_index;
  _cpixm = pixmap_index;
  _ximage_ptr->colorset()->paintDrawable (_pixmap_set,
    (const unsigned int *)newximage.data, newximage.width, newximage.height,
    0, 0,
    newximage.width, newximage.height, (int)(firstx+_left_border), (int)(y1));
  _cpixm = old_cpixm;
//_pixmap_set->setRange (newximage.width, newximage.height,
//  (int)(firstx+_left_border), (int)(y1));
////////////////// new /////////////////////////
  free(newximage.data);
  newximage.data = NULL;


  if(_over_image == NULL)//no seismic overlay image
    {
    annotatePlot(hoffset);
    refresh(firstx+_left_border,y1,
              newximage.width, newximage.height);
    *expose_x     = firstx+_left_border;
    *expose_y     = y1;
    *expose_width = newximage.width;
    *expose_height= newximage.height;
    }
  else//have seismic over this image
    {
    _over_image->refreshMain(firstx+_left_border+_dest_x,y1+_dest_y,
                             newximage.width, newximage.height,
                             _dest_x+firstx+_left_border,
                             _dest_y+y1+_dest_y, False,
                             _over_image->graphicWidget());
    *expose_x     = firstx+_left_border+_dest_x;
    *expose_y     = y1+_dest_y;
    *expose_width = newximage.width;
    *expose_height= newximage.height;
    }

    delete newximage_ptr;
    return ( OK );
}
