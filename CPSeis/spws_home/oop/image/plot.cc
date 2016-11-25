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
 *Name   : plot 
 *Purpose: Main function for the control of the construction 
 *         of the graphics image.
 *Author : Michael L. Sherrill
 *Date   : 07/91 (C++ version 4/97)
 *
 * Function Definition:
 * long plot ()
 * 
 *                     
 *
 *NOTES:
 * 1. 
 * 2.  
 *
 *Revisions:
 * DATE      WHO         DESCRIPTION
 * --------  --------    --------------------------------------------
 *
 *END DOC
 *------------------------------------------------------------------*/

#include <stdio.h>
#include <math.h>
#include <X11/Xlib.h>
#include <X11/Xutil.h>
#include <X11/Intrinsic.h>
#include <Xm/Xm.h>
#include "sl/sl_error_pop.hh"
#include "plot_image.hh"
#include "tfdefs.h"
#include "image_amplitude_recovery.hh"
#include "sl/ximage.hh"
#include "sl/colorset.hh"
#include "pixmap_set.hh"



#define XIMAGE_WARN \
 "You do not have enough memory on your server for this image.\n\
You might try again with a smaller plot."

#define HEADER_ERR "You have requested a header to label your traces by that \n\
is not in your file.. I have reset your trace annotation to header word one."

#define TLINEWARN "Your requested timing lines would have\n\
obscured your data and have been reset."

#define LABELWARN "Your trace label increment has been increased to\n\
prevent overlap."

#define CT_WARN "Warning you have requested a ct less than 1.0\n\
A new ct has been recalculated based on the\n\
median data value similar to splt cctf processing.\n\
This is a one time warning that will\n\
not appear on subsequent plots."




long PlotImage::plot( )

{
  long i, k;
  int n;
  long istat;
  long screen;
  long StartTraceIndex;
  char msg[80], *tempstr;
  Screen *scr; 
  Display *dpy;
  Arg arglist[22];
  int nin, nout;
  long warning = False;
  long HdrOffset, AryOffset, d_offset;
  long displayed_pixmap;
  long images;
  GC temp_gc;
  float remap_ratio;
  long ndo_nskp, ndo_sets;
  long PixelsPerTline;
  long ten_pixels = 10;
  long TraceLabelIncr;
  float median_value;
  float temp;
  double lval;
  float scale_amp;
  Boolean aborted = False;
  long trace_index;
  long index;
  float aval, factor;
  static Boolean show_warning = True;
  SLErrorPop *slpop;


  scr = XtScreen(_graphic);
  dpy = XtDisplay(_graphic);
  screen = DefaultScreen(XtDisplay(_graphic));
  HdrOffset = AryOffset = 0;
  _max_amplitude = 0.0;
  _min_amplitude = 0.0;
  _median_scale = 1.0;


  tempstr = "";
  if (_statusw) tempstr= wprocPushMsg(  _statusw, "");

  if(_user->_grid_y1 == _user->_grid_y2) _user->_grid_y2 += .001;
  if(_user->_grid_x1 == _user->_grid_x2) _user->_grid_x2 += .001;
  if(_grid_x1 == _grid_x2) _grid_x2 += .001;
  if(_grid_y1 == _grid_y2) _grid_y2 += .001;

  if(!_user->_movie)
    {
      images = 1;
      displayed_pixmap = 0;
    } 
  else
    {
      images = _user->_frames;
      displayed_pixmap = _cpixm;
    }
 
  _cpixm = 0;
  if(_user->_RtoL)
    StartTraceIndex = _ntot - 1;
  else
    StartTraceIndex = 0;


  /**********************make number of images requested***************/
  for(k=0; k<images; k++)
    {
      if(_statusw) 
        {
          if (images == 1)
            sprintf( msg, "Reading Data..." );
          else
            sprintf( msg, "Frame: %d of %d - Reading Data...", k+1, images );
          wprocShowMsg( _statusw, msg);
          XSync(dpy,False);
        }

      if(_user->_mode != PlotGRID && _user->_mode != PlotHEADER)
        {
/*
////////////////////// old ///////////////////////////
          if(_ximage.data != NULL) 
            {
              free(_ximage.data);
              _ximage.data = NULL;
            }
////////////////////// old ///////////////////////////
*/

////////////////////// new ///////////////////////////
	  _ximage_ptr->deallocate (&_ximage);
	  _ximage_ptr->allocate (&_ximage);
////////////////////// new ///////////////////////////

/*
////////////////////// old ///////////////////////////
          _ximage.data = (char *) calloc(1, _ximage.height *
					 _ximage.bytes_per_line * sizeof(char));
////////////////////// old ///////////////////////////
*/
          if(_ximage.data == NULL)
            {
              strcpy(_errstr,XIMAGE_WARN);
              _displayed= False;
              if (_statusw) wprocPopMsg(  _statusw, tempstr);
              return ( ResourceFail );
            }
        }


      /*assign trace structure values and read in the data*/
      if(!_zoomed)
        {
          _zindex = NO_ZOOM;
          if(_user->_mode != PlotGRID && _user->_mode != PlotISO &&
             _user->_mode != PlotHEADER)
            {
              _zoomyary[_zindex][0]= min(_user->_tmin,_tmax);
              _zoomyary[_zindex][1]= max(_user->_tmin,_tmax);
              _zoomxary[_zindex][0]= 1;
              _zoomxary[_zindex][1]= _ntot;
            }
          else if(_user->_mode == PlotISO)
            {
              _zoomyary[_zindex][0] = _user->_grid_y1;
              _zoomyary[_zindex][1] = _user->_grid_y2;
              _zoomxary[_zindex][0] = 1;
              if(_user->_movie)
                _ntot = _tpnl[k];
              _zoomxary[_zindex][1] = _ntot;
            }
          else
            {
              _zoomyary[_zindex][0] = _user->_grid_y1;
              _zoomyary[_zindex][1] = _user->_grid_y2;
              _zoomxary[_zindex][0] = _user->_grid_x1;
              _zoomxary[_zindex][1] = _user->_grid_x2;
            }
          _Cl.samp1=(int)((_user->_tmin - _user->_G.tstrt )
                          /_user->_G.srval+1.5);
          if(!_Cl.samp1)_Cl.samp1 = 1;
          _tmin = _user->_tmin;
          _tmax = _user->_tmax;
          if(_user->_mode <= PlotCOLOR)
            {
              _user->_grid_x1 = 1;
              _user->_grid_x2 = _ntot;
              _user->_grid_y1 = _tmin;
              _user->_grid_y2 = _tmax;
            }
        }
      else /*zoomed*/
        {
          _Cl.samp1=(int)((_zoomyary[_zindex][0]-
                           (_user->_tmin*_user->_G.srval/_user->_tdec))
                          / _user->_G.srval / _user->_tdec+1.5);
        }
 

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


      _Cl.ntot = (int)_ntot;
      _Cl.nskp= (int)_user->_nskp;
      if(!_Cl.nskp) 
        _Cl.ndo = (int)_ntot;  /*faster io*/
      else
        _Cl.ndo = (int)_user->_ndo;
      _Cl.sdec= (int)_user->_tdec; 
      _Cl.nsamp = (int)_nsamp;
      _displayed_traces = _ntot;

      if(_user->_mode == PlotWFILL || _user->_mode == PlotWONLY)
        _user->_do_color = False;



      if(_user->_mode == PlotHEADER)
        _first_trace_in_image = (long)(min(_grid_x1,_grid_x2));
      else
        _first_trace_in_image = 1;

      if(_user->_mode <= PlotCOLOR)
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
          _grid_y1 = _tmin;
          _grid_y2 = _tmax;
        }

      if(_user->_mode == PlotSEMB || _user->_mode == PlotCONTOUR)
        {
          _grid_x1 = _user->_grid_x1;
          _grid_x2 = _user->_grid_x2;
          _grid_y1 = _tmin;
          _grid_y2 = _tmax;
        }
      if(_user->_mode == PlotISO)
        {
          _grid_y1 = _tmin;
          _grid_y2 = _tmax;
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
//////////////////// new /////////////////
//		  _pixmap_set->clearRange (i);
//////////////////// new /////////////////
                }
              _displayed= False;
              if (_statusw) wprocPopMsg(  _statusw, tempstr);
              return(UserAbort);
            }
        }



      /*Read data from a file if necessary */
      if(_user->_mode != PlotGRID && _user->_mode != PlotISO &&
         _user->_mode != PlotHEADER && _point_to_data == False)
        {
          istat = imageIO( AryOffset, HdrOffset, k);
          if(istat != PlotSuccess)
            {
              _displayed= False;
              if (_statusw) wprocPopMsg(  _statusw, tempstr);
              return ( istat );
            }
          _original_traces = _ntot;
        }         




      if( (_abort_data) && (_abort_function) ) 
        {
          aborted = _abort_function( _abort_data );
          if( aborted ) 
            {
              for(i=0; i<MAX_PIXMAP; i++) 
                {
                  if(_pixmary[i] != 0) 
                    XFreePixmap(XtDisplay(_graphic),_pixmary[i]);
                  _pixmary[i] = 0;
//////////////////// new /////////////////
//		  _pixmap_set->clearRange (i);
//////////////////// new /////////////////
                }
              _displayed= False;
              if (_statusw) wprocPopMsg(  _statusw, tempstr);
              return(UserAbort);
            }
        }

      /* call any external function that has been registered with image*/
      if( _external_function != NULL )
        {
          _external_function(_external_function_data);
        }


      /*find max amplitude to be used in scaling data to image size requested*/
      if(_user->_mode != PlotGRID && _user->_mode != PlotHEADER)
        {
          _amp_recovery->setScaleAmp(k);
          _amp_recovery->computeDisplayedAmps(AryOffset);    
          _max_amplitude = _amp_recovery->getMaxDisplayedAmplitude();
          _min_amplitude = _amp_recovery->getMinDisplayedAmplitude();
        }

      /*process color if requested, if not map byte vals to all colors we have*/
      if(_user->_do_color && _user->_mode == PlotCOLOR 
         || _user->_mode == PlotISO)
        {
          //If doing color movies and the first movie has already been thru
          //the color processing, do not color process again. To do so
          //would cause every image to have its own color distribution
          //calculated and would not be representative on the single
          //color bar. Possibly we can use multiple color bars (one for
          //each movie panel) in the future.
          if(!k) imageColor(AryOffset, images);
          _user->_color_data = True;
        }
      else
        {
          _user->_color_data = False;
          if(_user->_mode >= PlotCOLOR && _col != NULL)
            {
              if(!_user->_num_cbar_cols)
                {
                  _user->_num_cbar_cols = _col->cnum;
                  if(_col->numplanes) /*dont count black and white overlay*/
                    _user->_num_cbar_cols -= 2;
                }
              remap_ratio = ((float)(_user->_num_cbar_cols - 1)) / 255.0;
              for(i=1;i<256;i++)_MapColors[i]=(int)((float)i*remap_ratio+.5);
              if(_user->_mode == PlotSEMB)//fill amps with 1-255 semblance vals
                {
                  factor = 254.0 / ((float)_user->_num_cbar_cols-2.0);
                  index = 3;
                  aval = 1.0;
                  for(i=0;i<_user->_num_cbar_cols;i++)
                    { 
                      _user->_cbar_rgb[index] = (float)((int)(aval + .5));
                      aval += factor;
                      index += 4;
                    } 
                }
            }
        }
   

      if(_user->_mode == PlotWONLY  || _user->_mode == PlotWFILL ||
         _user->_mode == PlotHEADER || _user->_mode == PlotCOLOR)
        {
          _x_value_per_pixel = 1.0 / (double) _trace_delta;
          if(_user->_RtoL) 
            _x_value_per_pixel = (-_x_value_per_pixel);
        }
      else
        {
          if(_underlay_only)
            _x_value_per_pixel = (_grid_x2 - _grid_x1)
              / (_ximage.width - 1);
        }

      _y_value_per_pixel = (_grid_y2 - _grid_y1) 
        / (_ximage.height - 1);

      if(_user->_mode == PlotISO)
        {
          _user->_vel_max = _vel_max = _grid_x2;
          _user->_vel_min = _vel_min = _grid_x1;
          _original_velfactor = _velfactor = 
            (_ximage.width - 1) / (_vel_max - _vel_min);
        }

      /*begin annotation checking */
      if(_user->_annotate)
        {
          if(_manual_annotate)
            {
              _manual_x_value_per_pixel = 
                (_manual_grid_x2-_manual_grid_x1)
                / (_ximage.width - 1);
              _manual_y_value_per_pixel = 
                (_manual_grid_y2-_manual_grid_y1)
                / (_ximage.height - 1);
            }
          /*display error if header to annotate by is not in the file*/
          if(_user->_hdrOne > _nhdrs) 
            {
              _user->_hdrOne = 1;
              strcpy(_errstr,HEADER_ERR);
              warning = True; 
            } 
          if(_user->_hdrTwo > _nhdrs)
            {
              _user->_hdrTwo = 1;
              strcpy(_errstr,HEADER_ERR);
              warning = True;
            }
          /*if timing lines obscure data reset*/
          if(_user->_ptl && !_zoomed)
            {
              PixelsPerTline = (long)(_user->_ptl / _y_value_per_pixel);
              if(PixelsPerTline < 0)PixelsPerTline = -PixelsPerTline;
              if(PixelsPerTline < ten_pixels)
                {
                  int enough = 0;
                  while(PixelsPerTline < ten_pixels)
                    {
                      _user->_ptl *= 2;
                      PixelsPerTline = (long)(_user->_ptl / _y_value_per_pixel);
                      if(PixelsPerTline < 0)PixelsPerTline = -PixelsPerTline;
                      ++enough;
                      if(enough > 10000) PixelsPerTline = ten_pixels;
                    }
                }
            }
          if(_user->_stl && !_zoomed)
            {
              PixelsPerTline = (long)(_user->_stl / _y_value_per_pixel);
              if(PixelsPerTline < 0)PixelsPerTline = -PixelsPerTline;
              if(PixelsPerTline < ten_pixels)
                {
                  int enough = 0;
                  while(PixelsPerTline < ten_pixels)
                    {
                      _user->_stl *= 2;
                      PixelsPerTline = (long)(_user->_stl / _y_value_per_pixel);
                      if(PixelsPerTline < 0)PixelsPerTline = -PixelsPerTline;
                      ++enough;
                      if(enough > 10000) PixelsPerTline = ten_pixels;
                    }
                }
            }
          /*display error if trace annotation is going to overlap*/
          if(_user->_LblInc > 0 && _user->_mode <= PlotCOLOR)
            {
              TraceLabelIncr = _user->_LblInc * _trace_delta;
              if(TraceLabelIncr < 8*max(_boldcharwidth,_fixedcharwidth))
                {
                  /*warning = True;
                    strcpy(_errstr,LABELWARN);*/
                }
            }
        }/*end of annotation checking*/

       
      /*if ct is less than 1 then calculate ct for user like cps*/
      if(_user->_mode != PlotGRID) 
        {
          if(_user->getNorm() == PANELNORM)
            scale_amp = _amp_recovery->getScaleAmp(k);
          else
            scale_amp = _amp_recovery->getScaleAmp(0);
        }
      if(_user->_mode != PlotGRID && _user->_mode != PlotISO &&
         _user->_mode != PlotHEADER && _user->_mode != PlotCOLOR)
        {
          if(_user->_ct < 1.0 && _user->_ct > 0.0)
            {
              nin = nout = (int)(getSamplesInMemory() * getTracesInMemory());
              trace_index = AryOffset / getSamplesInMemory();
              imageMedian( scale_amp, nin, 
                           trace_index,
                           &_median_scale,&median_value);
              _user->_ct = (_user->_ct * scale_amp) 
                / median_value;
              if(show_warning)
                slpop = new SLErrorPop(graphicWidget(),"warning",CT_WARN);
              show_warning = False;
            }
          if(_user->_ct < 0.0)
            _user->_ct = (-_user->_ct) * scale_amp;
        }


      /*populate ximage structure*/

      /*wiggles*/
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
/*
          // this was an interesting idea but won't work
	  if (DefaultDepth(dpy,screen) > 8) {
	    _ximage.format = ZPixmap;
            _ximage.depth = DefaultDepth (dpy,screen);
	  }
	  else {
*/
            _ximage.format = XYBitmap;
            _ximage.depth = 1;
//        }
        }
      else /*color*/
        {
          _ximage.depth = DefaultDepth(dpy,screen);   
          /*color display_scale not being used now*/
          if(_max_amplitude != 0)
            _display_scale = (_trace_delta) / _max_amplitude;
          _ximage.format = ZPixmap;
/*
//////////////////// old /////////////////////////////////
          _ximage.bits_per_pixel = _ximage.depth;
//////////////////// old /////////////////////////////////
*/
         if(!_user->_do_color && _user->_mode == PlotCOLOR) /*vert grade*/
            {
              _user->_gradev = 1;
              _user->_gradeh = 0;
            }
        }

      /*all types*/
/*
//////////////////// old /////////////////////////////////
      _ximage.xoffset = 0;
      _ximage.bitmap_unit = 8;
      _ximage.bitmap_pad = 0;
      _ximage.byte_order = MSBFirst;
      _ximage.bitmap_bit_order = MSBFirst;
/////////////////// old /////////////////////////////////
*/
      /*_ximage.byte_order = ImageByteOrder(dpy); implement later*/
      /*_ximage.bitmap_bit_order = BitmapBitOrder(dpy); implement later*/

//////////////////// new /////////////////////////////////
      _ximage_ptr->update (&_ximage);
//////////////////// new /////////////////////////////////


      /*loop each trace thru linear interpolation, and digitize*/
      if(_user->_mode != PlotCONTOUR && _user->_mode != PlotGRID && 
         _user->_mode != PlotISO     && _user->_mode != PlotHEADER)
        {
          for (i = 0; i < _ntot; i++)
            {
              if(_user->_mode == PlotSEMB)
                {
                  _amp_recovery->scaleDataForDisplay(0,StartTraceIndex,
                                                     0,_nsamp-1); 
                  rasterizeSemblance(
                                  _amp_recovery->getScaledByteDataArray(0), i,
                                  StartTraceIndex, HdrOffset);
                }
              else if(_user->_mode == PlotCOLOR)
                {
                  d_offset = StartTraceIndex * _nsamp;
                  if(_user->useHiResolution())
                    rasterizeByFloats(&_float_array[d_offset],
                                      i,
                                      StartTraceIndex);
                  else
                    rasterizeByBytes (&_float_array[d_offset],
                                      i,
                                      StartTraceIndex);
                }
              else
                { 
                  _amp_recovery->scaleDataForDisplay(0,StartTraceIndex,
                                                     0,_nsamp-1); 
                  variableArea(_amp_recovery->getScaledByteDataArray(0), i,
                               StartTraceIndex);
                }
              if(_user->_RtoL) 
                StartTraceIndex--;
              else
                StartTraceIndex++;
            } 
        }

      if( (_abort_data) && (_abort_function) ) 
        {
          aborted = _abort_function( _abort_data );
          if( aborted ) 
            {
              for(i=0; i<MAX_PIXMAP; i++) 
                {
                  if(_pixmary[i] != 0) 
                    XFreePixmap(XtDisplay(_graphic),_pixmary[i]);
                  _pixmary[i] = 0;
//////////////////// new /////////////////
//		  _pixmap_set->clearRange (i);
//////////////////// new /////////////////
                }
              _displayed= False;
              if (_statusw) wprocPopMsg(  _statusw, tempstr);
              return(UserAbort);
            }
        }


      if(_statusw)
        {
          if (images == 1)
            sprintf( msg, "Loading Display..." );
          else
            sprintf( msg, "Frame: %d of %d - Loading Display...", k+1, images );
          wprocShowMsg( _statusw, msg);
          XSync(dpy,False);
        }


      /*if doing an iso plot call appropriate image routine*/
      if(_user->_mode == PlotISO)
        {
          istat = variableArray(k);
          if(istat)
            {
             strcpy(_errstr,"Error in creating image, color amps may be equal\n\
              or the coordinates requested are not in your data.");
              _displayed= False;
              if (_statusw) wprocPopMsg(  _statusw, tempstr);
              return ( ResourceFail );
            }
        }

      /*set gc for fills and put image*/
      if(_ximage.depth == 1)
        temp_gc = _bitmap_gc1;
      else
        temp_gc = _gc1;


      /*fill background of image in white*/
      if(_ximage.depth == 1)
        {
          XSetForeground(dpy, temp_gc, 0);
          XSetBackground(dpy, temp_gc, 1);
        }
      else
        {
          XSetForeground(dpy, temp_gc, _white_pixel);
          XSetBackground(dpy, temp_gc, _black_pixel);
        }
      _ximage_ptr->colorset()->paintRectangle (_pixmary[_cpixm], temp_gc, 0,0,
                     (_ximage.width
                      +(int)_left_border+(int)_right_border),
                     (_ximage.height
                      +(int)_top_border+(int)_bottom_border) );
//////////////////// new /////////////////
/*
      _pixmap_set->setRange (
	 (_ximage.width +(int)_left_border+(int)_right_border),
         (_ximage.height+(int)_top_border +(int)_bottom_border));
*/
//////////////////// new /////////////////



      /*fill a grid type image with the color specified*/
      if(_user->_mode == PlotGRID || _user->_mode == PlotHEADER) 
        {
          XSetForeground(dpy, temp_gc, _grid_pixel);
          XSetBackground(dpy, temp_gc, _black_pixel);
          _ximage_ptr->colorset()->paintRectangle (_pixmary[_cpixm], temp_gc,
                         (int)_left_border,(int)_top_border,
                         _ximage.width, _ximage.height);
//////////////////// new /////////////////
/*
      _pixmap_set->setRange (_ximage.width, _ximage.height,
        (int)_left_border, (int)_top_border);
*/
//////////////////// new /////////////////
        }



      /*need foreground 1 since I set bits in the bitmap case instead of a 
      color*/
      if(_ximage.depth == 1)
        {
          XSetForeground(dpy, temp_gc, 1);
          XSetBackground(dpy, temp_gc, 0);
          //The following changed 9/99 because tags were getting a
          //black background in Geopress
          //_foreground_pixel = 1;
          //_background_pixel = 0;
          _foreground_pixel = _black_pixel;
          _background_pixel = _white_pixel;
          XSetForeground(dpy, _bitmap_gc2, 1);
          XSetBackground(dpy, _bitmap_gc2, 0); 
        }
      else
        {
          XSetForeground(dpy, temp_gc, _black_pixel);
          XSetBackground(dpy, temp_gc, _white_pixel);
          _foreground_pixel = _black_pixel;
          _background_pixel = _white_pixel;
          XSetForeground(dpy, _gc2, _black_pixel);
          XSetBackground(dpy, _gc2, _white_pixel);
        }




      /*if plot contours only*/
      if(_user->_mode == PlotCONTOUR)
        {
          _ximage_ptr->colorset()->paintRectangle (_pixmary[_cpixm], temp_gc, 
                         (int)_left_border,(int)_top_border,
                         _ximage.width, _ximage.height);
//////////////////// new /////////////////
/*
	  _pixmap_set->setRange (_ximage.width, _ximage.height,
	    (int)_left_border, (int)_top_border);
*/
//////////////////// new /////////////////
          contour(AryOffset, HdrOffset);
        }



      /*semblance type display*/
      if(_user->_mode == PlotSEMB)
        {
/*	  
/////////////////// old ////////////////////
          wpPutImage( dpy, _pixmary[_cpixm], temp_gc,
                      &_ximage, 0,0, 
                      (int)_left_border,(int)_top_border,
                      _ximage.width, _ximage.height );
/////////////////// old ////////////////////
*/
/////////////////// new ////////////////////
	  _ximage_ptr->colorset()->paintDrawable (_pixmap_set,
            (const unsigned int *)_ximage.data, _ximage.width, _ximage.height,
            0, 0,
            _ximage.width, _ximage.height, (int)_left_border,
            (int)_top_border);
//	  _pixmap_set->setRange (_ximage.width, _ximage.height,
//            (int)_left_border, (int)_top_border);
/////////////////// new ////////////////////
          if(_user->_contours) contour(AryOffset, HdrOffset);
        }


      /*trace type display*/
      if(_user->_mode != PlotSEMB && _user->_mode != PlotCONTOUR &&
         _user->_mode != PlotGRID && _user->_mode != PlotHEADER)
        {
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
	      (const unsigned int *)_ximage.data, _ximage.width,
              _ximage.height,
              0, 0, _ximage.width, _ximage.height, (int)_left_border,
              (int)_top_border);
	  }
//	  _pixmap_set->setRange (_ximage.width, _ximage.height,
//            (int)_left_border, (int)_top_border);
/////////////////// new ////////////////////
        }
  

      /*if doing an iso array plot that requires contours or contours only 
       on it*/
      if(_user->_mode == PlotISO && _user->plotWithContours())
        {
          contourGridder(AryOffset, HdrOffset);
        }


      /*done with image data array*/
/*
///////////////////////// old //////////////////////
      if(_ximage.data != NULL) free(_ximage.data);
      _ximage.data = NULL;
///////////////////////// old //////////////////////
*/

      /*annotate the plot if requested*/
      if(_user->_annotate)
        annotatePlot(HdrOffset);


      /*Increment trace skip for movies. If traces_per_group is 0 a normal
        cbyt type movie is set up, if traces_per_group is 1 a vertical
        crossline pattern is assumed, and if traces_per_group is > 1 a inline
        pattern of traces is set up like a cvst file.
      */
      if(_user->_movie)
        {
          _cpixm++;
          if(!_traces_per_group)
            {
              ndo_nskp = _Cl.ndo + _Cl.nskp;
              ndo_sets = _ntot / _Cl.ndo;
              _Cl.iskp += (int)(ndo_nskp * ndo_sets);
              if(_Cl.axis)_Cl.index += (int)(_user->_skip_frames + 1);
            }
          else /*pulling off a cube crossline or gvs panel*/
            {
              _Cl.iskp += (int)_traces_per_group;
              if(_user->_skip_frames)
                _Cl.iskp += (int)(_traces_per_group*_user->_skip_frames);
              if(_Cl.axis)_Cl.index += (int)(_user->_skip_frames + 1);
            }
          HdrOffset += (_ntot * _nhdrs);
          AryOffset += (_ntot * _nsamp);
          if(_user->_RtoL)
            StartTraceIndex=(long)(((k + 1) * _ntot) + _ntot - 1); 
          else
            StartTraceIndex = (long)(((k + 1) * _ntot));
        }


    }/************************end of frame generation**************/

  if(_user->_movie)/*reset to prior value before the loop above*/
    _Cl.iskp = (int)_user->_iskp;

  /*set frame number to the frame that was displayed before this function */
  //  _cpixm = (int)displayed_pixmap; disabled this 10/97 MLS
  _cpixm = 0;


  /*set graphic window to size of image unless it can be overlayed by wiggles*/
  _graph_height = _ximage.height + _dest_y
    + (int)_top_border+(int)_bottom_border;
  _graph_width  = _ximage.width + _dest_x
    + (int)_left_border+(int)_right_border;


  

  
  

  if(!_can_overlay)
    {
      _displayed = False;//Prevent the following resize from causing a redraw
      _dest_x = 0;
      _dest_y = 0;
      n=0;
      XtSetArg (arglist[n], XmNheight, _graph_height); n++;
      XtSetArg (arglist[n], XmNwidth, _graph_width); n++;
      XtSetValues(_graphic, arglist, n );
    }


  /*remove possible connection with a underlay image unless scanning*/
  if(_chain_image != NULL && _scanleft == False 
     && _scanright == False)
    _chain_image = NULL;


  _displayed = True;

  if(_user->_movie)
    _frames = _user->_frames;
  else
    _frames = 1;
  _skip_frames = _user->_skip_frames;


  /*copy the image to screen*/
  refresh(0,0,ImageAll,ImageAll);
  
  if (_statusw) wprocPopMsg(  _statusw, tempstr);

  if(!warning)
    return ( PlotSuccess );
  else
    return ( PlotWarning );
}

void PlotImage::testPlot ()
{
  if (!this || !_ximage.data) return;

  /*trace type display*/
  if (_user->_mode != PlotSEMB && _user->_mode != PlotCONTOUR &&
    _user->_mode != PlotGRID && _user->_mode != PlotHEADER) {

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
/////////////////// new ////////////////////
//  _pixmap_set->setRange (_ximage.width, _ximage.height,
//    (int)_left_border, (int)_top_border);
/////////////////// new ////////////////////
  }
}
