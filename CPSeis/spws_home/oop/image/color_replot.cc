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
 *Name   : colorReplot
 *Purpose: Re-plot color data with out going to disk
 *        
 *Author : Michael L. Sherrill
 *Date   : 07/91 (C++ version 4/97)
 *
 * Function Definition:
 * long color_replot ()
 *
 *
 *NOTES:
 * 1.
 * 2.
 *
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

#include "plot_image.hh"
#include "image_amplitude_recovery.hh"
#include "pixmap_set.hh"

#include "sl/ximage.hh"
#include "sl/colorset.hh"
#include "sl/paintset.hh"

long PlotImage::colorReplot()


{
  float temp;
  long screen;
  Screen *scr;
  Display *dpy;
  Arg arglist[22];
  int i, j, n;
  long skip_samples;
  long AryOffset, HdrOffset, d_offset;
  long displayed_pixmap;
  long images;
  long StartTraceIndex, StartOverlay;
  float remap_ratio;
  GC temp_gc;
  char   msg[200], *tempstr;
  tempstr = "";

  scr = XtScreen(_graphic);
  dpy = XtDisplay(_graphic);
  screen = DefaultScreen(XtDisplay(_graphic));

 

  displayed_pixmap = _cpixm;

  if(_zoomed)
    skip_samples = (long)((_zoomyary[_zindex][0]-_user->getTmin())
                          / (_user->getGlobals().srval * _user->getTdec()));
  else
    skip_samples = 0;


  if(_user->getRightToLeft())
    StartOverlay = StartTraceIndex = (long)(_zoomxary[_zindex][1] - 1);
  else
    StartOverlay = StartTraceIndex = (long)(_zoomxary[_zindex][0] - 1);



  if(!_user->getMovieOption())
    images = 1;
  else
    images = _user->getFrames();


  AryOffset =  HdrOffset = 0;
  _cpixm = 0;

  if((_user->getInvertedYaxis() == True) && (_user->getMode() != PlotHEADER)
     && (_user->getMode() != PlotGRID))
    {
      temp = _tmin;
      _tmin = _tmax;
      _tmax = temp;
    } 
  _grid_y1 = _tmin;
  _grid_y2 = _tmax;
  _y_value_per_pixel = (_grid_y2 - _grid_y1) 
    / (_ximage.height - 1);


  /***********************START FRAMES GENERATION**********************/
  if (_statusw) tempstr= wprocPushMsg(  _statusw, "");

  for(j=0;j<images;j++)
    {
      if(_user->_mode == PlotCOLOR)
        {  
          if(!_user->_RtoL)
            {
              _grid_x1 = _first_trace_in_image;
              _grid_x2 = _ntot;
            }
          else
            {
              _grid_x1 = _ntot;
              _grid_x2 = _first_trace_in_image;
            }
          _x_value_per_pixel = 1.0 / (double) _trace_delta;
          if(_user->_RtoL) 
            _x_value_per_pixel = (-_x_value_per_pixel);
        }
      if (_statusw) 
        {
          sprintf( msg, "Creating Frame: %d", j+1 );
          wprocShowMsg(_statusw, msg);
        }
/*
////////////// old //////////////////////
      if(_ximage.data != NULL)
        {
          free(_ximage.data);
          _ximage.data = NULL;
        }
      _ximage.data = (char *) malloc( _ximage.height*
                                      _ximage.bytes_per_line);
////////////// old //////////////////////
*/
////////////// new //////////////////////
      _ximage_ptr->deallocate (&_ximage);
      _ximage_ptr->allocate (&_ximage);
////////////// new //////////////////////
      if(_ximage.data == NULL) return(ResourceFail);
      if(_user->getDoColor() && _user->_mode != PlotSEMB)//dont color semblance 
        {
          //If doing color movies and the first movie has already been thru
          //the color processing, do not color process again. To do so
          //would cause every image to have its own color distribution
          //calculated and would not be representative on the single
          //color bar. Possibly we can use multiple color bars (one for
          //each movie panel) in the future.
          if(!j) imageColor(AryOffset, images);
        }
      else
        {
          if(!_user->getNumberCbarColors())
            {
              _user->setNumberCbarColors(_col->cnum);
              if(_col->numplanes)
                _user->setNumberCbarColors(_user->getNumberCbarColors() - 2);
            }
          remap_ratio = ((float)(_user->getNumberCbarColors() - 1)) / 255.0;
          for(i=1;i<256;i++)_MapColors[i]= (int)((float)i * remap_ratio+ .5);
        }
      if(_user->getMode() != PlotCONTOUR && _user->getMode() != PlotGRID &&
         _user->getMode() != PlotHEADER  && _user->getMode() != PlotISO)
        { 
          //recompute scale amps for image because movie will have  
          //set these to the last image created
          if(_user->_mode != PlotGRID &&
             _user->_mode != PlotHEADER && _frames > 1)
            {           
              _amp_recovery->setScaleAmp(_cpixm);
              _amp_recovery->computeDisplayedAmps(AryOffset);    
              _max_amplitude = _amp_recovery->getMaxDisplayedAmplitude();
              _min_amplitude = _amp_recovery->getMinDisplayedAmplitude();
            } 
          for (i = 0; i < _ntot; i++)
            {
              if(_user->_mode == PlotSEMB)
                {
                  _amp_recovery->scaleDataForDisplay(0, StartTraceIndex, 
                                                 skip_samples,
                                                 _nsamp - 1 + skip_samples);
                  rasterizeSemblance(
                                _amp_recovery->getScaledByteDataArray(0), i, 
                                StartTraceIndex, HdrOffset);
                }
              else
                {
                  d_offset= StartTraceIndex * _nsamp + skip_samples;
                  if(_user->useHiResolution())
                    rasterizeByFloats(&_float_array[d_offset],
                                      i, StartTraceIndex);
                  else
                    rasterizeByBytes (&_float_array[d_offset],
                                      i, StartTraceIndex);
                }
              if(_user->getRightToLeft())
                StartTraceIndex--;
              else
                StartTraceIndex++;
            }
        }



      /*if doing an iso plot call appropriate image routine*/
      if(_user->getMode() == PlotISO)
        variableArray(j);


      temp_gc = _gc1;
      XSetForeground(dpy, temp_gc, _white_pixel);
      XSetBackground(dpy, temp_gc, _black_pixel);
      _ximage_ptr->colorset()->paintRectangle (_pixmary[_cpixm], temp_gc, 0,0,
                     (int)(_ximage.width+_left_border+
                           _right_border),
                     (int)(_ximage.height+_top_border+
                           _bottom_border) );
/////////////////// new ////////////////////
//      _pixmap_set->setRange (
//        (int)(_ximage.width +_left_border+ _right_border),
//        (int)(_ximage.height+ _top_border+_bottom_border) );
/////////////////// new ////////////////////
      XSetForeground(dpy, temp_gc, _black_pixel);
      XSetBackground(dpy, temp_gc, _white_pixel);
      XSetForeground(dpy, _gc2, _black_pixel);
      XSetBackground(dpy, _gc2, _white_pixel);

      /*if plot contours only*/
      if(_user->getMode() == PlotCONTOUR)
        {
          _ximage_ptr->colorset()->paintRectangle (_pixmary[_cpixm], temp_gc, 
                         (int)_left_border,(int)_top_border,
                         _ximage.width, _ximage.height);
/////////////////// new ////////////////////
//	  _pixmap_set->setRange (_ximage.width, _ximage.height,
//	    _left_border, _top_border);
/////////////////// new ////////////////////
          contour(AryOffset, HdrOffset );
        }

      /*if doing an iso array plot that requires contours, or contours*/
      if(_user->_mode == PlotISO && _user->plotWithContours())
        {
          contourGridder(AryOffset, HdrOffset);
        } 

      /*if semblance and contours*/
      if(_user->getMode() == PlotSEMB)
        {
/*
////////////////// old /////////////////////////
          wpPutImage(dpy, _pixmary[_cpixm], temp_gc, 
                     &_ximage, 0, 0, (int)_left_border, 
                     (int) _top_border, _ximage.width, 
                     _ximage.height);
////////////////// old /////////////////////////
*/
////////////////// new /////////////////////////
	  _ximage_ptr->colorset()->paintDrawable (_pixmap_set,
            (const unsigned int*)_ximage.data, _ximage.width, _ximage.height,
            0, 0,
	    _ximage.width, _ximage.height, (int)_left_border,
	    (int)_top_border);
//        _pixmap_set->setRange (_ximage.width, _ximage.height,
//	    _left_border, _top_border);
////////////////// new /////////////////////////
          if(_user->getNumberOfContours()) contour(AryOffset, HdrOffset);
        }

      /*normal color trace type display*/
      if(_user->getMode() != PlotSEMB && _user->getMode() != PlotCONTOUR)
        {
/*
////////////////// old /////////////////////////
          wpPutImage(dpy, _pixmary[_cpixm], temp_gc,
                     &_ximage, 0, 0, (int)_left_border,
                     (int)_top_border, _ximage.width,
                     _ximage.height);
////////////////// old /////////////////////////
*/
////////////////// new /////////////////////////
	  _ximage_ptr->colorset()->paintDrawable (_pixmap_set,
            (const unsigned int *)_ximage.data, _ximage.width, _ximage.height,
            0, 0,
	    _ximage.width, _ximage.height, (int)_left_border,
	    (int)_top_border);
//	  _pixmap_set->setRange (_ximage.width, _ximage.height,
//	    _left_border, _top_border);
////////////////// new /////////////////////////
        }

/*
////////////////// old /////////////////////////
      free(_ximage.data);
      _ximage.data = NULL;
////////////////// old /////////////////////////
*/
////////////////// new /////////////////////////
      _ximage_ptr->deallocate (&_ximage);
////////////////// new /////////////////////////

      if(_user->getAnnotateImage())
        {
          annotatePlot(HdrOffset);
        }

      if(_user->getRightToLeft())
        StartTraceIndex = (long)(((j + 1) * getTracesInMemory()) +
                                 _zoomxary[_zindex][1] - 1);
      else
        StartTraceIndex = StartTraceIndex - _ntot + getTracesInMemory();

      StartOverlay = StartTraceIndex;
      _cpixm++;
      AryOffset = _cpixm * getTracesInMemory() * getSamplesInMemory(); 
      HdrOffset      = _cpixm * getTracesInMemory() * _nhdrs;

    }

  /***********************END FRAMES GENERATION*********************/


  _cpixm = (int)displayed_pixmap;

  n=0;
  _graph_height= _ximage.height+_top_border+
    _bottom_border + _dest_y;
  _graph_width=  _ximage.width+_left_border+
    _right_border + _dest_x;
  XtSetArg (arglist[n], XmNheight, _graph_height); n++;
  XtSetArg (arglist[n], XmNwidth, _graph_width); n++;
  XtSetValues(_graphic, arglist, n );

  // if following is true, a refresh at this point seems redundant but
  //   implementing it didn't seem to help.
  //  if (!(_can_overlay && _over_image != NULL)) {
    refresh (0, 0, ImageAll, ImageAll);
  //  }

  if (_statusw) wprocPopMsg(  _statusw, tempstr);

  return ( PlotSuccess );


}




