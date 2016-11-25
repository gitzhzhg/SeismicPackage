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
 *Name   : redraw_image             
 *Purpose: Redraws an entire pixmap with a revised image.             
 *                                 
 *Author : Michael L. Sherrill
 *Date   : 01/98
 *
 * Function Definition:
 * long redrawImage (long frame_index)
 *
 * frame_index   in   index of pixmap to redraw   
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
#include <X11/Xlib.h>
#include <X11/Xutil.h>
#include <X11/Intrinsic.h>
#include <Xm/Xm.h>
#include <math.h>
#include <limits.h>
#include "plot_image.hh"
#include "image_amplitude_recovery.hh"
#include "pixmap_set.hh"
#include "sl/ximage.hh"
#include "sl/colorset.hh"

#define pix_estr \
"You did not have enough memory on your server for the new image."


long PlotImage::redrawImage (long frame_index, int do_refresh)
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

  //make sure image is not zoomed
  if(_zindex != NO_ZOOM) processZoom(Z_ORIGINAL,0,0,0,0,0);

  //movie to appropriate frame so annotation will draw on the correct image
  if(_user->getMovieOption() && _cpixm != frame_index)
    imageMovie(frame_index,0,0,ImageAll,ImageAll);

  tempstr = "";
  if (_statusw) tempstr= wprocPushMsg(_statusw, "");


  /*determine scale factor and image params based on pixels per inch*/
  dpy = XtDisplay(_graphic);
  
  //if grid or header type, replot with imageGrid and return
  if(_user->_mode == PlotGRID || _user->_mode == PlotHEADER)
    {
      stat = imageGrid();
      if(stat == ResourceFail || stat == ReadFail || stat == UserAbort)
        strcpy(_errstr,pix_estr);
      return stat;
    }


  AryOffset = frame_index * getTracesInMemory() * getSamplesInMemory(); 
  HdrOffset = frame_index * getTracesInMemory() * _nhdrs;
  if(_user->_RtoL)
    StartTraceIndex = (long)( (frame_index  * _ntot) + _ntot - 1 ); 
  else
    StartTraceIndex = (long)((frame_index * _ntot));

  /*free and allocate image array*/
/*//////////////// old //////////////////////////
  if(_ximage.data != NULL) 
    {
      free(_ximage.data);
      _ximage.data = NULL;
    }

  if(_user->_mode == PlotWONLY || _user->_mode == PlotWFILL)
    _ximage.data = (char *) calloc(1, _ximage.height*
                                   _ximage.bytes_per_line
                                   * sizeof(char));
  else
    _ximage.data = (char *) malloc(   _ximage.height*
                                      _ximage.bytes_per_line
                                      * sizeof(char));
*///////////////// old //////////////////////////

////////////////// new //////////////////////////
  _ximage_ptr->deallocate (&_ximage);
  _ximage_ptr->allocate (&_ximage);
////////////////// new //////////////////////////
   
  if(_ximage.data == NULL)
    {
      strcpy(_errstr,pix_estr);
      if (_statusw) wprocPopMsg(  _statusw, tempstr);
      return ( ResourceFail );
    }


  /*loop each trace thru linear interpolation, and digitize*/
  if(_user->_mode != PlotCONTOUR && _user->_mode != PlotISO)
    {
      for (i = 0; i < _ntot; i++)
        {
          if(_user->_mode == PlotSEMB)
            {
              _amp_recovery->scaleDataForDisplay(0,StartTraceIndex, 
                                                 0, _nsamp - 1); 
              rasterizeSemblance(_amp_recovery->getScaledByteDataArray(0), i,
                                   StartTraceIndex, HdrOffset);
            }
          else if(_user->_mode == PlotCOLOR)
            {
              d_offset = StartTraceIndex * _nsamp;
              if(_user->useHiResolution())
                rasterizeByFloats(&_float_array[d_offset],
                                  i, StartTraceIndex);
              else
                rasterizeByBytes (&_float_array[d_offset],
                                  i, StartTraceIndex);
            }
          else if(_user->_mode == PlotWONLY || _user->_mode == PlotWFILL)
            {
              _amp_recovery->scaleDataForDisplay(0,StartTraceIndex,
                                                 0, _nsamp - 1); 
              variableArea(_amp_recovery->getScaledByteDataArray(0), i,
                           StartTraceIndex);
            }
          if(_user->_RtoL) 
            StartTraceIndex--;
          else
            StartTraceIndex++;
        } 
    }
  else if(_user->_mode == PlotISO)
    {
      variableArray(frame_index);
    }

  if((_abort_data) && (_abort_function) ) 
    {
      aborted = _abort_function( _abort_data );
      if( aborted ) 
        {
          if (_statusw) wprocPopMsg(  _statusw, tempstr);
          return(UserAbort);
        }
    }


  /*set gc for fills and put image*/
  if(_ximage.depth == 1)
    {
      temp_gc = _bitmap_gc1;
    }
  else
    {
      temp_gc = _gc1;
    }


  if(_ximage.depth == 1)
    {
      XSetForeground(dpy, temp_gc, 0 );
      XSetBackground(dpy, temp_gc, 1 );
    }
  else
    {
      XSetForeground(dpy, temp_gc, _white_pixel);
      XSetBackground(dpy, temp_gc, _black_pixel);
    }
  _ximage_ptr->colorset()->paintRectangle (_pixmary[frame_index], temp_gc,
                 0,0,
                 (_ximage.width+(int)_left_border
                  +(int)_right_border),
                 (_ximage.height+(int)_top_border
                  +(int)_bottom_border) );
/////////////////// new ////////////////////
/*
  _pixmap_set->setRange (
    (_ximage.width +(int)_left_border+(int) _right_border),
    (_ximage.height+(int) _top_border+(int)_bottom_border) );
*/
/////////////////// new ////////////////////
  if(_ximage.depth == 1)
    {
      XSetForeground( dpy, temp_gc, 1 );
      XSetBackground( dpy, temp_gc, 0 );
      //The following changed 9/99 because tags were getting a
      //black background in Geopress
      //_foreground_pixel = 1;
      //_background_pixel = 0;
      _foreground_pixel = _black_pixel;
      _background_pixel = _white_pixel;
    }
  else
    {
      XSetForeground(dpy, temp_gc, _black_pixel);
      XSetBackground(dpy, temp_gc, _white_pixel);
      _foreground_pixel = _black_pixel;
      _background_pixel = _white_pixel;
    }


  /*if plot contours only*/
  if(_user->_mode == PlotCONTOUR)
    {
      _ximage_ptr->colorset()->paintRectangle (_pixmary[frame_index], temp_gc,
                     (int)_left_border,(int)_top_border, 
                     _ximage.width, _ximage.height);
/////////////////// new ////////////////////
/*
      _pixmap_set->setRange (_ximage.width, _ximage.height,
        (int)_left_border, (int)_top_border);
*/
/////////////////// new ////////////////////
      contour(StartTraceIndex*getSamplesInMemory(),HdrOffset);
    }


  /*if doing an iso array plot that requires contours, or contours only on it*/
  if(_user->_mode == PlotISO && _user->plotWithContours())
    {
      contourGridder(AryOffset, HdrOffset);
    }

  /*trace type or semblance data display*/
  if (_user->_mode != PlotCONTOUR) {
/*////////////////// old ////////////////////
    wpPutImage( dpy, _pixmary[frame_index], temp_gc,
                &_ximage, 0,0, 
                (int)_left_border,(int)_top_border, 
                _ximage.width, _ximage.height );
*/////////////////// old ////////////////////

/////////////////// new ////////////////////
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
//  _pixmap_set->setRange (_ximage.width, _ximage.height,
//    (int)_left_border, (int)_top_border);
/////////////////// new ////////////////////
  }
  /*if semblance and contours requested overlay image with contours*/
  if(_user->_mode == PlotSEMB && _user->_contours)
/*
    contour(StartTraceIndex*getSamplesInMemory(),HdrOffset);
*/
    contour (AryOffset, HdrOffset);
////////////// appears to be a long standing bug! 01/21/05 ///////////



  /*done with this frame's ximage data*/
/*
///////////////////////// old //////////////////////
  free(_ximage.data);
  _ximage.data = NULL;
///////////////////////// old //////////////////////
*/

  /*annotate the plot*/
  if(_manual_annotate)
    {
      xscaler = (_manual_grid_x2 - _manual_grid_x1)
        / (_grid_x2 - _grid_x1);
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
  if(_can_overlay && _over_image != NULL)
    setDestinations(_over_image,this);


  if(_user->_annotate) annotatePlot(HdrOffset);



  if (do_refresh) refresh(0,0,ImageAll,ImageAll);


  if (_statusw) wprocPopMsg(  _statusw, tempstr);

  return ( PlotSuccess );

}
