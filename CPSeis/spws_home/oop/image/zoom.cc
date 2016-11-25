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
 *Name   : zoom             
 *Purpose: Zoom up or down an image by the zoom factor requested.      
 *                                 
 *Author : Michael L. Sherrill
 *Date   : 07/91 (C++ version 4/97)
 *
 * Function Definition:
 * long zoom (double *zoom_factor_ptr)
 *
 * factor    in         Zoom factor to apply to an image that has previously
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
"You did not have enough memory on your server for the zoomed image.\n\
You might try again with a smaller area or factor."

#define zoomerr \
"If you attempted to zoom up -> your zoomed image was too large.\n\
If you attempted to zoom down-> your trace width was smaller than one pixel\n\n\
The number of traces and/or time has been changed to fit your screen." 


#define ZOOMUPSTR    "Creating Display..."
#define ZOOMDOWNSTR  "Zooming Down..."
#define ZOOMORGINSTR "Re-creating Display...."

long PlotImage::zoom( double  *zoom_factor_ptr)
{
  long i, j;
  int n;
  long FrameStartTraceIndex;
  long StartTraceIndex, StartOverlay;
  long screen, skip_samples = 0;
  long skip_traces = 0;
  float  zfact;
  Arg arglist[22];
  long stat = 0;
  Screen *scr; 
  static Display *dpy;
  long warning = False;
  long old_height, old_width;
  float old_tmin, old_tmax, old_ti, old_is, old_user_tmax;
  long old_delta, old_nsamp, old_ntot; 
  long old_first_trace, old_display_traces;
  static float old_velfactor;
  long displayed_pixmap;
  double old_yval, old_xval;
  long old_orig_samples; 
  long old_orig_traces;
  long HdrOffset;
  long images;
  char *zoomdirstr;
  char msg[200], *tempstr;
  GC temp_gc;
  long firsthdr, lasthdr;
  double temp;
  long zi, first_sample, last_sample;
  long first_trace, last_trace;
  float tempv;
  long maxsize;
  Boolean aborted;
  long AryOffset, d_offset;
  float lval, xscaler;


  zoomdirstr = ZOOMORGINSTR;
  if (_zoomup)          zoomdirstr= ZOOMUPSTR;
  if (_zoomdown)        zoomdirstr= ZOOMDOWNSTR;
  if (_zindex==NO_ZOOM) zoomdirstr= ZOOMORGINSTR;

  tempstr = "";
  zfact = *zoom_factor_ptr;
  _zoomed = True;
  displayed_pixmap = _cpixm;
   
  if (_statusw) tempstr= wprocPushMsg(  _statusw, "");



  /*keep old variables in case we get a resource fail in this function*/
  old_height = _ximage.height;
  old_width  = _ximage.width;
  old_is     = _is;
  old_ti     = _ti;
  old_ntot   = _ntot;
  old_nsamp  = _nsamp;
  old_tmax   = _tmax;
  old_tmin   = _tmin;
  old_delta  = _trace_delta;
  old_user_tmax = _user->_tmax;
  old_yval   = _y_value_per_pixel;
  old_xval   = _x_value_per_pixel;
  old_orig_samples = _original_samples;
  old_orig_traces  = _original_traces;
  old_first_trace = _first_trace_in_image;
  old_display_traces = _displayed_traces;
  old_velfactor = _velfactor;
  zi = _zindex;  
   



  /*determine scale factor and image params based on pixels per inch*/
  scr = XtScreen(_graphic); 
  dpy = XtDisplay(_graphic);
  screen = DefaultScreen(XtDisplay(_graphic));
  
  _first_trace_in_image = (long)_zoomxary[zi][0];
  _displayed_traces = (long)(_zoomxary[zi][1]
                             - _zoomxary[zi][0] + 1);

  if(_user->_mode == PlotISO && zi <= NO_ZOOM)/*iso original size or less*/
    {
      _first_trace_in_image = 1;
      _displayed_traces = _original_traces;
    }


  if(_zoomup)
    {
      if(_user->_mode == PlotGRID || _user->_mode == PlotHEADER)
        _ti *= zfact;
      else
        _ti /= zfact;
      _is *= zfact;
      _velfactor *= zfact;
    }
  else  /*zoom down*/  
    {
      if(_user->_mode == PlotISO || _user->_mode == PlotGRID ||
         _user->_mode == PlotHEADER)
        {
          _ti /= zfact;
          _is /= zfact;
          _velfactor /= zfact;
        }
      else
        {
          _ti *= zfact;
          _is /= zfact;
          _velfactor /= zfact;
        }
    }


  if(_user->_mode == PlotGRID || _user->_mode == PlotHEADER) 
    {
      if(_zoomup) /*zoom up*/
        {
          _user->_grid_width = 
            (long)(((_zoomxary[zi][1] - _zoomxary[zi][0])
                    / _x_value_per_pixel) * zfact);
          _user->_grid_height =
            (long)(((_zoomyary[zi][1] - _zoomyary[zi][0])
                    / _y_value_per_pixel) * zfact);
        }
      else if(_zoomdown) /*zoom down*/
        {
          _user->_grid_width = 
            (long)(((_zoomxary[zi][1] - _zoomxary[zi][0])
                    / _x_value_per_pixel) / zfact);
          _user->_grid_height =
            (long)(((_zoomyary[zi][1] - _zoomyary[zi][0])
                    / _y_value_per_pixel) / zfact);
        }
      else /*original size*/
        { 
          setGridSize();    
        }
    }


  first_trace = min( ((int)(_zoomxary[zi][0] + 0.5)), 
                     ((int)(_zoomxary[zi][1] + 0.5)));
  last_trace  = max( ((int)(_zoomxary[zi][0] + 0.5)),
                     ((int)(_zoomxary[zi][1] + 0.5)));
  _ntot = last_trace - first_trace + 1;


  first_sample = (int)(_zoomyary[zi][0] 
                       / (_user->_G.srval * _user->_tdec) + 1.5);
  last_sample = (int)(_zoomyary[zi][1]
                      / (_user->_G.srval * _user->_tdec) + 1.5);
  _nsamp = abs((int)last_sample - (int)first_sample) + 1;
  _tmax = _zoomyary[zi][1];
  _tmin = _zoomyary[zi][0];

  /*flip image coordinates if user has request inverted y image. PlotHEADER
    and PlotGrid are typically set for an inverted plot so we dont want to 
    reset coordinates*/
  if((_user->_invert_yaxis == True) && (_user->_mode != PlotHEADER)
     && (_user->_mode != PlotGRID))
    {
      temp = _tmin;
      _tmin = _tmax;
      _tmax = temp;
    }     

  _displayed_traces = _ntot;

  if(_user->_mode == PlotSEMB || _user->_mode == PlotCONTOUR
     || _user->_mode == PlotISO)
    {
      firsthdr = (long)((_zoomxary[zi][0] - 1)*_nhdrs 
                        + _coordinate_header - 1);
      lasthdr  = (long)((_zoomxary[zi][1] - 1)*_nhdrs 
                        + _coordinate_header - 1);
      _vel_min = _hd[firsthdr];
      _vel_max = _hd[lasthdr];
      _grid_x1 = _vel_min;
      _grid_x2 = _vel_max;
      if(zi == NO_ZOOM)_velfactor = _original_velfactor;
    }

  

  /*check size of zoomed image and re-size if too large or small*/ 
  if(_user->_mode == PlotGRID || _user->_mode == PlotHEADER)
    stat = imageGrid();
  else
    stat= checkSize();

  if(stat == ResourceFail || stat == ReadFail || stat == UserAbort)
    {
      warning = True;
      _ximage.height = (int)old_height;
      _ximage.width = (int)old_width;
//////////////// new //////////////////////////
      _ximage_ptr->update (&_ximage);
//////////////// new //////////////////////////
      _is = old_is;
      _ti = old_ti;
      _ntot = old_ntot;
      _nsamp = old_nsamp;
      _tmax = old_tmax;
      _tmin = old_tmin;
      _trace_delta = old_delta;
      _user->_tmax = old_tmax;
      _y_value_per_pixel = old_yval;
      _x_value_per_pixel = old_xval;
      _original_samples = old_orig_samples;
      _original_traces  = old_orig_traces;
      _first_trace_in_image = old_first_trace;
      _displayed_traces = old_display_traces;
      if(_zindex > NO_ZOOM)--_zindex;
      if(_zindex < NO_ZOOM)++_zindex;
      if(_zindex <= NO_ZOOM)_zoomup = False;
      if(_zindex >= NO_ZOOM)_zoomdown = False;
      if(_zindex == NO_ZOOM)_zoomed = False;
      zi = _zindex;
      strcpy(_errstr,pix_estr);
      if(_pixmary[_cpixm] == 0) return (stat);
    }




  /*we are done if this is a grid image*/
  if(_user->_mode == PlotGRID || _user->_mode == PlotHEADER) 
    {
      if(_user->_mode == PlotHEADER)
        {
          _x_value_per_pixel =  1.0 / (double) _trace_delta;
          if(_user->_RtoL) 
            _x_value_per_pixel= (-_x_value_per_pixel);
        }
      else
        {
          _x_value_per_pixel = (_grid_x2 - _grid_x1)
            / (_ximage.width -1);
        }
      _y_value_per_pixel = (_grid_y2 - _grid_y1)
        / (_ximage.height - 1);
      return(stat);
    }



  /*determine byte data offset to make image*/
  skip_samples = (int)((_zoomyary[zi][0]-getMemoryTmin())
                       / (_user->_G.srval * _user->_tdec) + 0.5);


  if(stat == PlotWarning && _frame_buffer)
    {
      strcpy(_errstr,zoomerr);
      warning = True;
    } 

  if(stat == PlotWarning && !_frame_buffer && _zoomdown)
    {
      strcpy(_errstr,zoomerr);
      warning = True;
    }

  if(_user->_mode >= PlotCOLOR)
    {

/*//////////////// old //////////////////////////
      _ximage.bytes_per_line = _ximage.width;
*///////////////// old //////////////////////////
    }
  else
    {
/*//////////////// old //////////////////////////
      _ximage.bytes_per_line = (_ximage.width + 7) / 8;
      _ximage.width = _ximage.bytes_per_line * 8;
/*//////////////// old //////////////////////////
////////////////// new //////////////////////////
      _ximage.width = (int)((_ximage.width + 7) / 8) * 8;
      _ximage_ptr->update (&_ximage);
////////////////// new //////////////////////////
    }  

  if(_user->_RtoL)
    StartOverlay = StartTraceIndex = (long)(_zoomxary[zi][1] - 1);
  else
    StartOverlay = StartTraceIndex = (long)(_zoomxary[zi][0] - 1);



  /************ start generation of number of images requested ************/
  _cpixm = 0;
  if(!_user->_movie)
    images = 1;
  else
    images = _user->_frames;

  for(j=0;j<images;j++)
    {
      HdrOffset = j * getTracesInMemory() * _nhdrs;
      if (_statusw) 
        {
          if (images == 1)
            sprintf( msg, "%s", zoomdirstr );
          else
            sprintf( msg, "Frame: %d of %d - %s", j+1, images, zoomdirstr );
          wprocShowMsg( _statusw, msg);
          XSync(dpy,False);
        }

      //Wiggles
      if (_user->_mode < PlotCOLOR ) 
        {//We need to use a byte value of 255 when computing the maximum
          //trace excursion if the data is normalized to the file or an
          //external user supplied amplitude. The member variable _display_scale
          //is used to compute the excursion of the trace when it is digitized.
          //If for instance the lav of a file or an external amplitude is 10.0 
          //then real amplitudes of a trace that are 10.0 should reach the
          //maximum excursion of the alloted trace width display area.
          if(_user->getNorm() == FILENORM || 
             _user->getNorm() == EXTERNALNORM )
     
            lval = 255.0;
          else
            lval = max( fabs(_min_amplitude - 128.0),
                        fabs(_max_amplitude - 128.0) ) + 128.0;
          _display_scale = (_user->_ct * _trace_delta) / lval;
        }

      if (_user->_mode <  PlotCOLOR )/*wiggles*/
        {
          _ximage.format = XYBitmap;
          _ximage.depth = 1;
        }
      else /*gray scale*/
        {
          _ximage.depth = DefaultDepth(dpy,screen);   
          _ximage.format = ZPixmap;
/*//////////////// old //////////////////////////
          _ximage.bits_per_pixel = _ximage.depth;
*///////////////// old //////////////////////////
        }
////////////////// new //////////////////////////
      _ximage_ptr->update (&_ximage);
////////////////// new //////////////////////////


      if(_user->_mode <= PlotCOLOR)
        {
          if(!_user->_RtoL)
            {
              _grid_x1 = _first_trace_in_image;
              _grid_x2 = _ntot + _grid_x1 - 1;
            }
          else
            {
              _grid_x2 = _first_trace_in_image;
              _grid_x1 = _ntot +_grid_x2 - 1;
            }
        }

      if(_user->_mode == PlotARRAY || (_can_overlay && _over_image != NULL) )
        {
          for(n=0;n<_cpixm;n++) skip_traces += _tpnl[n];
          skip_traces += (_first_trace_in_image - 1);
          _grid_x1 = _hd[skip_traces*_nhdrs
                        +_coordinate_header-1];
          _grid_x2 = _hd[(_ntot-1+skip_traces)
                        *_nhdrs+_coordinate_header-1];
          if(_user->_RtoL)
            {
              tempv = _grid_x1;
              _grid_x1 = _grid_x2;
              _grid_x2 = tempv;
            }
        }
     

      _grid_y1 = _tmin;
      _grid_y2 = _tmax;

      if(_user->_mode == PlotWONLY  || _user->_mode == PlotWFILL ||
         _user->_mode == PlotCOLOR)
        {
          _x_value_per_pixel = 1.0 / (double) _trace_delta;
          if(_user->_RtoL) 
            _x_value_per_pixel= (-_x_value_per_pixel);
        }
      else
        {
          _x_value_per_pixel = (_grid_x2 - _grid_x1)
            / (_ximage.width - 1);
        }

      _y_value_per_pixel = (_grid_y2 - _grid_y1) 
        / (_ximage.height - 1);




      /*allocate image array*/
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
          _displayed= False;
          if (_statusw) wprocPopMsg(  _statusw, tempstr);
          return ( ResourceFail );
        }

     

      /*loop each trace thru linear interpolation, and digitize*/
      FrameStartTraceIndex = StartTraceIndex;
      AryOffset = j * getTracesInMemory() * getSamplesInMemory(); 
      //recompute scale amps for image because movie will have  
      //set these to the last image created
      if(_user->_mode != PlotGRID && _user->_mode != PlotHEADER && _frames > 1)
        {           
          _amp_recovery->setScaleAmp(j);
          _amp_recovery->computeDisplayedAmps(AryOffset);    
          _max_amplitude = _amp_recovery->getMaxDisplayedAmplitude();
          _min_amplitude = _amp_recovery->getMinDisplayedAmplitude();
        }

      if(_user->_mode != PlotCONTOUR && _user->_mode != PlotISO)
        {
          for (i = 0; i < _ntot; i++)
            {
              if(_user->_mode == PlotSEMB)
                {
                  _amp_recovery->scaleDataForDisplay(0,StartTraceIndex, 
                                                   skip_samples,
                                                   _nsamp - 1 + skip_samples); 
                  rasterizeSemblance(
                                    _amp_recovery->getScaledByteDataArray(0), i,
                                    StartTraceIndex, HdrOffset);
                }
              else if (_user->_mode == PlotCOLOR)
                {
                  d_offset =  StartTraceIndex * getSamplesInMemory() +
                               skip_samples;
                  if(_user->useHiResolution())
                    rasterizeByFloats( &_float_array[d_offset], 
                                       i, StartTraceIndex); 
                  else
                    rasterizeByBytes ( &_float_array[d_offset], 
                                       i, StartTraceIndex);
                }
              else if(_user->_mode == PlotWONLY || _user->_mode == PlotWFILL)
                {
                  _amp_recovery->scaleDataForDisplay(0,StartTraceIndex,
                                                   skip_samples, 
                                                   _nsamp - 1 + skip_samples); 
                  variableArea(_amp_recovery->getScaledByteDataArray(0), i,
                               StartTraceIndex);
                }
              if(_user->_RtoL) 
                StartTraceIndex--;
              else
                StartTraceIndex++;
            } 
        }

      if((_abort_data) && (_abort_function) ) 
        {
          aborted = _abort_function( _abort_data );
          if( aborted ) 
            {
              for(i=0; i<MAX_PIXMAP; i++) 
                {
                  if(_pixmary[i] != 0) 
                    XFreePixmap(XtDisplay(_graphic),_pixmary[i]);
                  _pixmary[i] = 0;
/////////////////// new ////////////////////
//		  _pixmap_set->clearRange (i);
/////////////////// new ////////////////////
                }
              _displayed= False;
              if (_statusw) wprocPopMsg(  _statusw, tempstr);
              return(UserAbort);
            }
        }


      if(_user->_mode == PlotISO)
        variableArray(j);


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
          /*this does not work on HP, get reverse video on the borders******
            if(_can_overlay)
            {
            XSetForeground(dpy, temp_gc, 0);
            XSetBackground(dpy, temp_gc, 1);
            }
            else
            {
            XSetForeground(dpy, temp_gc, _white_pixel);
            XSetBackground(dpy, temp_gc, _black_pixel);
            }
            ******  use the next two lines instead to solve HP problem ****/
          XSetForeground(dpy, temp_gc, _white_pixel);
          XSetBackground(dpy, temp_gc, _black_pixel);
        }
      _ximage_ptr->colorset()->paintRectangle (_pixmary[_cpixm], temp_gc, 0,0,
                     (_ximage.width+(int)_left_border
                      +(int)_right_border),
                     (_ximage.height+(int)_top_border
                      +(int)_bottom_border) );
/////////////////// new ////////////////////
//      _pixmap_set->setRange (
//	(_ximage.width +(int)_left_border+(int)_right_border ),
//	(_ximage.height+(int)_top_border +(int)_bottom_border) );
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
          _ximage_ptr->colorset()->paintRectangle (_pixmary[_cpixm], temp_gc, 
                         (int)_left_border,(int)_top_border, 
                         _ximage.width, _ximage.height);
/////////////////// new ////////////////////
//	  _pixmap_set->setRange (_ximage.width, _ximage.height,
//	    (int)_left_border, (int)_top_border);
/////////////////// new ////////////////////
          contour(FrameStartTraceIndex * getSamplesInMemory() + skip_samples,
                  HdrOffset);
        }


      /*trace type or semblance data display*/
      if(_user->_mode != PlotCONTOUR)
/*
/////////////////// old ////////////////////
        wpPutImage( dpy, _pixmary[_cpixm], temp_gc,
                    &_ximage, 0,0, 
                    (int)_left_border,(int)_top_border, 
                    _ximage.width, _ximage.height );
/////////////////// old ////////////////////
*/
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
//      _pixmap_set->setRange (_ximage.width, _ximage.height,
//	  (int)_left_border, (int)_top_border);
/////////////////// new ////////////////////


      /*if semblance and contours requested overlay image with contours*/
      if(_user->_mode == PlotSEMB && _user->_contours)
        contour(FrameStartTraceIndex * getSamplesInMemory() + skip_samples,
                HdrOffset);


      /*if doing an iso array plot that requires contours, or contours only*/
      if(_user->_mode == PlotISO && _user->plotWithContours())
        {
          contourGridder(AryOffset, HdrOffset);
        }


      /*done with this frame's ximage data*/
/*/////////////////// old ////////////////////
      free(_ximage.data);
      _ximage.data = NULL;
*//////////////////// old ////////////////////
/////////////////// new ////////////////////
      _ximage_ptr->deallocate (&_ximage);
/////////////////// new ////////////////////



      /*annotate the plot*/
      if(_manual_annotate)
        {
          xscaler = (_manual_grid_x2 - _manual_grid_x1) / (_grid_x2 - _grid_x1);
          _manual_grid_x1 = _grid_x1 * xscaler;
          _manual_grid_x2 = _grid_x2 * xscaler;
          _manual_x_value_per_pixel = 
            (_manual_grid_x2-_manual_grid_x1)
            / (_ximage.width - 1);

          _manual_grid_y1 = _grid_y1 * _manual_scaler;
          _manual_grid_y2 = _grid_y2 * _manual_scaler;
          _manual_y_value_per_pixel = 
            (_manual_grid_y2-_manual_grid_y1)
            / (_ximage.height - 1);
        }



      /* if we are zooming an underlay image make sure images line up*/
      if(_can_overlay && _over_image != NULL)
        setDestinations(_over_image,this);


      if(_user->_annotate) annotatePlot(HdrOffset);


      if(_user->_RtoL)
        StartTraceIndex=(long)(((j + 1)*old_orig_traces)+_zoomxary[zi][1]-1); 
      else
        StartTraceIndex = StartTraceIndex - _ntot + old_orig_traces;

      StartOverlay = StartTraceIndex;
      _cpixm++;
    }/********************* end frames generation ****************/




  /*set active image frame to the frame that was active before zooming*/
  _cpixm = (int)displayed_pixmap;

  /*set window size to image size*/
  _graph_height = _ximage.height + _dest_y
    + (int)_top_border+(int)_bottom_border;
  _graph_width  = _ximage.width + _dest_x
    + (int)_left_border+(int)_right_border;

  if(!_can_overlay)
    {
      n=0;
      XtSetArg (arglist[n], XmNheight, _graph_height); n++;
      XtSetArg (arglist[n], XmNwidth, _graph_width); n++;
      XtSetValues(_graphic, arglist, n );
    }
  else if(_over_image != NULL)
    {
      n=0;
      maxsize = max( _left_border + _ximage.width + 
                     _right_border + _dest_x,
                     _over_image->_left_border + 
                     _over_image->_ximage.width + 
                     _over_image->_right_border + 
                     _over_image->_dest_x );
      _graph_width = _over_image->_graph_width = maxsize;
      XtSetArg (arglist[n], XmNwidth, maxsize); n++;
      maxsize = max( _top_border + _ximage.height + 
                     _bottom_border + _dest_y,
                     _over_image->_top_border + 
                     _over_image->_ximage.height + 
                     _over_image->_bottom_border + 
                     _over_image->_dest_y );
      _graph_height = _over_image->_graph_height = maxsize;
      XtSetArg (arglist[n], XmNheight, maxsize); n++;
      XtSetValues(_graphic, arglist, n );
    }

  if(_can_overlay && _over_image != NULL)//refresh using the overlay image
    _over_image->refresh(0,0,ImageAll,ImageAll);
  else
    refresh(0,0,ImageAll,ImageAll);

  if (_statusw) wprocPopMsg(  _statusw, tempstr);

  if(!warning)
    return ( PlotSuccess );
  else
    return ( PlotWarning );
}
