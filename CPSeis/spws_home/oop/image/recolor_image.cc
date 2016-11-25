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
#include <stdio.h>
#include <stdlib.h>
#include <X11/Xlib.h>
#include <X11/Xutil.h>
#include <X11/Intrinsic.h>
#include <Xm/Xm.h>
#include <math.h>
#include <limits.h>
#include "plot_image.hh"
#include "image_amplitude_recovery.hh"
#include "sl/ximage.hh"
#include "sl/colorset.hh"
#include "pixmap_set.hh"

#define pix_estr \
"You did not have enough memory on your server for the new image."


long PlotImage::recolorImage (long frame_index, int do_refresh)
{
  long i;
  long StartTraceIndex;
  long stat = 0;
  static Display *dpy;
  long HdrOffset;
  char *tempstr;
  GC temp_gc;
  Boolean aborted;
  long AryOffset, d_offset;
  float xscaler;
  float remap_ratio;

  //make sure image is not zoomed
  if (_zindex != NO_ZOOM) processZoom (Z_ORIGINAL,0,0,0,0,0);

  //movie to appropriate frame so annotation will draw on the correct image
  if (_user->getMovieOption() && _cpixm != frame_index) {
    imageMovie(frame_index,0,0,ImageAll,ImageAll);
  }

  tempstr = "";
  if (_statusw) tempstr= wprocPushMsg(_statusw, "");

  /*determine scale factor and image params based on pixels per inch*/
  dpy = XtDisplay(_graphic);
  
  //if grid or header type, replot with imageGrid and return
  if (_user->_mode == PlotGRID || _user->_mode == PlotHEADER) {
    stat = imageGrid();
    if (stat == ResourceFail || stat == ReadFail || stat == UserAbort)
      strcpy(_errstr,pix_estr);
    return stat;
  }

  AryOffset = frame_index * getTracesInMemory() * getSamplesInMemory(); 
  HdrOffset = frame_index * getTracesInMemory() * _nhdrs;
  if (_user->_RtoL) {
    StartTraceIndex = (long)((frame_index * _ntot) + _ntot - 1); 
  }
  else {
    StartTraceIndex = (long)((frame_index * _ntot));
  }

  /*free and allocate image array*/
  _ximage_ptr->deallocate (&_ximage);
  _ximage_ptr->allocate (&_ximage);
   
  if (_ximage.data == NULL) {
    strcpy (_errstr,pix_estr);
    if (_statusw) wprocPopMsg (_statusw, tempstr);
    return (ResourceFail);
  }

  if (_user->getDoColor() && _user->_mode != PlotSEMB) {
    // don't color semblance 
    // If doing color movies and the first movie has already been thru
    // the color processing, do not color process again. To do so
    // would cause every image to have its own color distribution
    // calculated and would not be representative on the single
    // color bar. Possibly we can use multiple color bars (one for
    // each movie panel) in the future.
    imageColor (AryOffset, 0);
  }
  else {
    if (!_user->getNumberCbarColors()) {
      _user->setNumberCbarColors (_col->cnum);
      if (_col->numplanes) {
	_user->setNumberCbarColors (_user->getNumberCbarColors() - 2);
      }
    }
    remap_ratio = ((float)(_user->getNumberCbarColors() - 1)) / 255.0;
    for (i = 1; i < 256; i++) {
      _MapColors[i] = (int)((float)i * remap_ratio + 0.5);
    }
  }

  /*loop each trace thru linear interpolation, and digitize*/
  if (_user->_mode != PlotCONTOUR && _user->_mode != PlotISO) {
    for (i = 0; i < _ntot; i++) {
      if (_user->_mode == PlotSEMB) {
	_amp_recovery->scaleDataForDisplay (0, StartTraceIndex, 0, _nsamp-1); 
	rasterizeSemblance (_amp_recovery->getScaledByteDataArray(0), i,
          StartTraceIndex, HdrOffset);
      }
      else if (_user->_mode == PlotCOLOR) {
	d_offset = StartTraceIndex * _nsamp;
	if (_user->useHiResolution()) {
	  rasterizeByFloats (&_float_array[d_offset], i, StartTraceIndex);
	}
	else {
	  rasterizeByBytes (&_float_array[d_offset], i, StartTraceIndex);
	}
      }
      else if (_user->_mode == PlotWONLY || _user->_mode == PlotWFILL) {
	_amp_recovery->scaleDataForDisplay (0, StartTraceIndex, 0,
          _nsamp - 1); 
	variableArea (_amp_recovery->getScaledByteDataArray(0), i,
          StartTraceIndex);
      }
      if (_user->_RtoL) {
	StartTraceIndex--;
      }
      else {
	StartTraceIndex++;
      }
    } 
  }
  else if (_user->_mode == PlotISO) {
    variableArray (frame_index);
  }

  if ((_abort_data) && (_abort_function)) {
    aborted = _abort_function (_abort_data);
    if (aborted) {
      if (_statusw) wprocPopMsg (_statusw, tempstr);
      return (UserAbort);
    }
  }

  /*set gc for fills and put image*/
  if (_ximage.depth == 1) {
    temp_gc = _bitmap_gc1;
  }
  else {
    temp_gc = _gc1;
  }

  if (_ximage.depth == 1) {
    XSetForeground(dpy, temp_gc, 0 );
    XSetBackground(dpy, temp_gc, 1 );
  }
  else {
    XSetForeground(dpy, temp_gc, _white_pixel);
    XSetBackground(dpy, temp_gc, _black_pixel);
  }
  _ximage_ptr->colorset()->paintRectangle (_pixmary[frame_index], temp_gc,
    0,0,
    (_ximage.width+(int)_left_border+(int)_right_border),
    (_ximage.height+(int)_top_border+(int)_bottom_border));

  if (_ximage.depth == 1) {
    XSetForeground( dpy, temp_gc, 1 );
    XSetBackground( dpy, temp_gc, 0 );
    // The following changed 9/99 because tags were getting a
    // black background in Geopress
    // _foreground_pixel = 1;
    // _background_pixel = 0;
    _foreground_pixel = _black_pixel;
    _background_pixel = _white_pixel;
  }
  else {
    XSetForeground(dpy, temp_gc, _black_pixel);
    XSetBackground(dpy, temp_gc, _white_pixel);
    _foreground_pixel = _black_pixel;
    _background_pixel = _white_pixel;
  }

  /*if plot contours only*/
  if (_user->_mode == PlotCONTOUR) {
    _ximage_ptr->colorset()->paintRectangle (_pixmary[frame_index], temp_gc,
      (int)_left_border,(int)_top_border, _ximage.width, _ximage.height);
    contour (StartTraceIndex*getSamplesInMemory(), HdrOffset);
  }

  /*if doing an iso array plot that requires contours, or contours only on it*/
  if (_user->_mode == PlotISO && _user->plotWithContours()) {
    contourGridder (AryOffset, HdrOffset);
  }

  /*trace type or semblance data display*/
  if (_user->_mode != PlotCONTOUR) {
    if (_ximage.depth == 1) {
      _ximage_ptr->colorset()->paintBits (_pixmap_set,
	(const char *)_ximage.data, _ximage.width, _ximage.height,
	0, 0, _ximage.width, _ximage.height, (int)_left_border,
        (int)_top_border);
    }
    else {
      _ximage_ptr->colorset()->paintDrawable (_pixmap_set,
	(const unsigned int *)_ximage.data, _ximage.width, _ximage.height,
        0, 0, _ximage.width, _ximage.height, (int)_left_border,
        (int)_top_border);
    }
  }

  /*if semblance and contours requested overlay image with contours*/
  if (_user->_mode == PlotSEMB && _user->_contours) {
    contour (AryOffset, HdrOffset);
  }

  /*done with this frame's ximage data*/
  
  /*annotate the plot*/
  if (_manual_annotate) {
    xscaler = (_manual_grid_x2 - _manual_grid_x1) / (_grid_x2 - _grid_x1);
    _manual_grid_x1 = _grid_x1 * xscaler;
    _manual_grid_x2 = _grid_x2 * xscaler;
    _manual_x_value_per_pixel = (_manual_grid_x2-_manual_grid_x1) /
      _ximage.width;

    _manual_grid_y1 = _grid_y1 * _manual_scaler;
    _manual_grid_y2 = _grid_y2 * _manual_scaler;
    _manual_y_value_per_pixel = (_manual_grid_y2-_manual_grid_y1) /
      _ximage.height;
  }

  /* if we are zooming an underlay image make sure images line up*/
  if (_can_overlay && _over_image != NULL) {
    setDestinations (_over_image,this);
  }

  if (_user->_annotate) annotatePlot(HdrOffset);

  if (do_refresh) refresh (0, 0, ImageAll, ImageAll);

  if (_statusw) wprocPopMsg (_statusw, tempstr);

  return (PlotSuccess);

}
