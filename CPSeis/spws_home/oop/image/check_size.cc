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
 *Name   : checkSize 
 *Purpose: Determine if image requested will fit on the screen and if
 *         not reset some of the display parameters and notify the
 *         user. Also abort image if server or client cannot allocate
 *         enough memory.
 *Author : Michael L. Sherrill
 *Date   : 07/91 (C++ version 4/97)
 *
 * Function Definition:
 * long  checkSize () 
 *
 *
 *NOTES:
 * 1. Some original user parameters may be redefined if the requested display
 *    will not fit on the visible screen area.
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
#include <X11/cursorfont.h>
#include <Xm/Xm.h>
#include <math.h>

#include "plot_image.hh"
#include "pixmap_set.hh"
#include "sl/ximage.hh"

#include "wproc.h"

static long pixmap_error= False;

#define pix_estr \
"You do not have enough memory on your server for this plot.\n\
It will be necessary to reduce the amount of data or size of the plot."

#define mem_estr \
"You do not have enough client memory on your computer\n\
for this plot. It will be necessary to reduce the amount\n\
of data or size of the plot."

#define movie_read "Your movie parameters will read past the end of file."

#define eofread "You have attempted to read traces \n\
before or after your file \n\
or the file cannot be found or opened."

#define too_big "Your requested image is too large.\n\
Would you like to display a trimmed down version ?"

/*Xlib's xCreatePixmapReq struct in Xproto.h will only allow an integer
  16 so the maximum pixel width and height currently cannot exceed
  32768 for any pixmap operation. Normaly the server will detect the
  error but the Tektronix and DEC PC433 are not*/
#define MAXPIXEL 32768


long PlotImage::checkSize() 

{
long i, j, k, l, n;
long max_width, max_height;
int screen, depth;
double traces_per_inch, inches_per_second;
float vpixels_per_inch, hpixels_per_inch;
double time_reduction;
Screen *scr; 
Display *dpy;
long images;
long warning = False;
long skipdo  = False;
long numtraces;
float total_traces;
float ndosets, nskpsets;
long addsample, integer_samples;
char msg[80], *tempstr;
long old_iskp, old_nplt;
long headersize;
XErrorHandler prev_handler;  
long MAX_V;
float x1, x2;
long skip_traces = 0;
long index;
float temp, temp_delta;
Boolean aborted;
int pattern_len, left_over;
long ndo, nskp, iskp, traces_in_file;

  tempstr = "";
  
    
/*free up any chained image unless zooming or scanning*/
 if(_chain_image != NULL && _zoomup == False && 
    _zoomdown == False && _scanright == False  && 
    _scanleft == False) 
    {
    _chain_image->imageFree();
    _chain_image = NULL;
    _dest_x = _dest_y = 0;
    }

 if (_statusw) tempstr= wprocPushMsg(  _statusw, "");

 MAX_V = _user->getVelocityArraySize();

 if(_ntot == 0 && _filedata == False) _ntot = _user->getNumberToPlot(); 

 if(_user->getMode() == PlotHEADER) 
    {
    _point_to_data = True;
    _ntot = _tpnl[_cpixm];
    }

 if(_user->getY1() == _user->getY2()) _user->setY2( (_user->getY2() + .10));
 if(_user->getX1() == _user->getX2()) _user->setX2( (_user->getX2() + .10));
 if(_grid_x1 == _grid_x2) _grid_x2 += .10;
 if(_grid_y1 == _grid_y2) _grid_y2 += .10;
 


/*if doing velocity type plots reset traces to user velocity min and max*/
 if( (_user->getMode() == PlotSEMB || _user->getMode() == PlotCONTOUR)
      && _zoomed == False )
    {
       i = j = k = l = 0;
       old_iskp = _user->getInitialSkip();
       old_nplt = _user->getNumberToPlot();
       while(_user->getVelocityMinimum() > _vary[i] && i < MAX_V) i++; 
       while(_user->getVelocityMaximum() > _vary[j] && j < MAX_V) j++;
       if(i < MAX_V && j < MAX_V)
         {
         _user->setInitialSkip(_user->getInitialSkip() + i);
         _user->setNumberToPlot( (j+1) - i );
         }
       else //could not find range so leave paramaters as they were
         {
         _user->setInitialSkip(old_iskp);
         _user->setNumberToPlot(old_nplt);
         _user->setVelocityMinimum(_vel_min);
         _user->setVelocityMaximum(_vel_max);
         return(PlotSuccess);
         }
       //reset user input to match what was found in the file
       _user->setVelocityMinimum(_vary[i]);
       _user->setVelocityMaximum(_vary[j]);
       _user->setX1(_vary[i]);
       _user->setX2(_vary[j]);
    } /****end velocity type plot re-selecting*****/
  
  

/*Attempt to prevent user from reading data that is outside of his file*/
  if(_user->getInitialSkip() < 0)
     {
     _ntot = _user->getNumberToPlot() + _user->getInitialSkip();
     if(_ntot < 1)
        {
        _ntot = _user->getNumberToPlot();
        strcpy(_errstr,eofread);
        if (_statusw) wprocPopMsg(  _statusw, tempstr);
        return(ReadFail);
        }
     _user->setInitialSkip(0);
     warning = True;
     strcpy(_errstr,eofread);
     }

  if(_user->getInitialSkip() >= _user->getGlobals().ntrfil)
     {
     strcpy(_errstr,eofread);
     if (_statusw) wprocPopMsg(  _statusw, tempstr);
     return(ReadFail);
     }

  if(!_user->getMovieOption())
     images = 1;
  else
     images = _user->getFrames();


  if(images > MAX_PIXMAP)
    {
    if(_statusw) wprocPopMsg(  _statusw, "Number of movies greater than 5000");
    return (ResourceFail);
    }


  if(!_zoomed)
     numtraces = _user->getNumberToPlot(); 
  else
     numtraces = _ntot;

  if(!_user->getNumberToDo()) _user->setNumberToDo(1);


  ndo  = _user->getNumberToDo();
  nskp = _user->getNumberToSkip();
  iskp = _user->getInitialSkip(); 
  pattern_len = ndo + nskp;

  if(ndo > 1 || iskp > 0) skipdo = True;

  //Check total traces to read if doing a do skip pattern
  if(skipdo)
    {
    if(!_user->getMovieOption())
      total_traces = (_user->getGlobals().ntrfil - iskp) /  pattern_len * ndo;
    else
      total_traces = pattern_len * (numtraces / ndo);
    left_over = (_user->getGlobals().ntrfil - iskp) % pattern_len;
    if(left_over > ndo)
      total_traces += ndo;
    else
      total_traces += left_over;
    if(_user->getMode() != PlotGRID && !usingSelector())
      {
      total_traces *= (images - 1);
      total_traces += iskp;
      }
    else
      {
      total_traces = 1;
      }
    }
  else//Check total traces to read if no do skip pattern
    {
    if(_user->getMode() != PlotGRID && !usingSelector())
      total_traces = numtraces * images + iskp;
    else
      total_traces = 1;
    }

  /* one more chance to make good if movie */
  if (total_traces > _user->getGlobals().ntrfil && nskp > 0 &&
    _user->getMovieOption()) {
    total_traces -= nskp;
  }

  /*if skip do pattern too large try to reset (if movie just bail)*/
  if(total_traces > _user->getGlobals().ntrfil && skipdo)
     {
     if(_user->getMovieOption())
        {
        strcpy(_errstr,movie_read);
        if (_statusw) wprocPopMsg(  _statusw, tempstr);
        return(ReadFail);
        }
     warning = True;
     strcpy(_errstr,eofread);
     while(total_traces > _user->getGlobals().ntrfil)
       {
       numtraces--;
       total_traces = (numtraces * pattern_len) * images + iskp;
       if(left_over > ndo)
         total_traces += ndo;
       else
         total_traces += left_over;
       }
     if(numtraces < 1) /*couldnt reset number to plot so abort*/
       {
       strcpy(_errstr,eofread);
       if (_statusw) wprocPopMsg(  _statusw, tempstr);
       return(ReadFail);
       }
     if(!_zoomed)
         _user->setNumberToPlot(numtraces);
     else
         _ntot    = numtraces;
     if(_ntot > numtraces || _ntot == 0) _ntot = numtraces;
     if(_ntot + _user->getInitialSkip() > _user->getGlobals().ntrfil)
        _ntot = max(1,_user->getGlobals().ntrfil - _user->getInitialSkip());
     }

  /*If not doing a skip do pattern and we will hit the eof try to reset
    the number of traces to plot (unless it is movie in which case we bail).*/ 
  if(total_traces > _user->getGlobals().ntrfil)
     {
     if(_user->getMovieOption())
        {
        strcpy(_errstr,movie_read);
        if (_statusw) wprocPopMsg(  _statusw, tempstr);
        return(ReadFail);
        }
     if(!_zoomed && !_scanleft && !_scanright) 
       {
       _ntot = _user->getGlobals().ntrfil - _user->getInitialSkip(); 
       _user->setNumberToPlot(_ntot);
       }
     else
       {
       _ntot = _user->getGlobals().ntrfil - _user->getInitialSkip();
       }
     warning = True;
     strcpy(_errstr,eofread);
     }


  

/*ok to continue and try to read data*/
  _Cl.iskp = (int)_user->getInitialSkip();


/*determine scale factor and image parameters based on pixels per inch*/
  scr = XtScreen(_graphic); 
  dpy = XtDisplay(_graphic);
  screen = DefaultScreen(XtDisplay(_graphic));
  hpixels_per_inch =  horizontalPixelsPerInch(dpy, screen);
  vpixels_per_inch =  verticalPixelsPerInch(dpy, screen);

/*This line put in 4/96 to scale the is parameter from inches to a factor
  for iso/array type images */
  if(_user->getMode() == PlotISO && _zoomed == False)
    {
    _user->setInchesPerSecond(_user->getInchesPerSecond() / 
                              (max(_user->getTmin(),_user->getTmax()) -
                               min(_user->getTmin(),_user->getTmax())));
    _is = _user->getInchesPerSecond();
    }

  if(_user->getMetricMode()) 
     {
     hpixels_per_inch /=  2.54;
     vpixels_per_inch /=  2.54;
     }
  if(!_zoomed)
     {
     traces_per_inch = _ti = _user->getTracesPerInch();
     inches_per_second = _is = _user->getInchesPerSecond();
     }  
  else
     {
     traces_per_inch = _ti;
     inches_per_second = _is;
     }


  
  if(_user->getMode() == PlotSEMB || _user->getMode() == PlotCONTOUR ||
     _user->getMode() == PlotISO)
     _trace_delta = (long)((_user->getTracesPerInch() * hpixels_per_inch)
                         / _ntot);
  else if(_user->getMode() != PlotHEADER)
     _trace_delta = (long)(hpixels_per_inch / traces_per_inch + .5);




/*The next if caused plot iso and grid types to set the ti which was a
  plot width in inches to be a trace increment. (e.g. 7 inch plot became 94).
  I dont think we really need to worry about a trace delta of less than 1 
  since it should just drop out some of the vertical data 4/96*/
  if(_user->getMode() >= PlotCOLOR && _trace_delta <= 1) 
     {
     /*traces_per_inch = _ti = hpixels_per_inch;*/
     _trace_delta = 1;
     }

  //make sure a wiggle trace is at least 2 pixels wide
  if(_user->getMode() <  PlotCOLOR && _trace_delta < 2)
     {
     traces_per_inch = _ti = hpixels_per_inch / 2.0;
     _trace_delta = 2;
     }


  if(_user->getMode() == PlotWONLY  || _user->getMode() == PlotWFILL ||
                                       _user->getMode() == PlotHEADER)
     { 
     if(_user->getNumberToPlot() > 1)
        _first_trace_location = (int)((float)_trace_delta / 2.0);
     else /*if only one trace make enough room for all of its excursion*/
        _first_trace_location = _trace_delta;
     }
  else
     {
     _first_trace_location = 0;
     }


  if(!_zoomed && _filedata)
     {
     if(!warning) _ntot = _user->getNumberToPlot() ;
     integer_samples = (long)((_user->getTmin() - _user->getGlobals().tstrt)
                     / (_user->getGlobals().srval * _user->getTdec()) + .5);
     _user->setTmin(   integer_samples 
                       * (_user->getGlobals().srval * _user->getTdec())
                       + _user->getGlobals().tstrt);
     integer_samples = (long)((_user->getTmax() - _user->getGlobals().tstrt)
                     / (_user->getGlobals().srval * _user->getTdec()) + .5);
     _user->setTmax(   integer_samples *
                       (_user->getGlobals().srval * _user->getTdec())
                       + _user->getGlobals().tstrt);
     _nsamp = (long)((_user->getTmax()- _user->getTmin())
                       / _user->getGlobals().srval / _user->getTdec() +1.5);
     _tmax = _user->getTmax();
     }



  switch(_user->getMode())
    {
    case PlotGRID:
    case PlotHEADER:
         _vel_min = _user->getVelocityMinimum();
         _vel_max = _user->getVelocityMaximum();
         if(_zindex == NO_ZOOM && _zoomed == False)
	    {
            _ximage.width = (int)(_user->getTracesPerInch() * hpixels_per_inch);
	    }
         else if(_zoomup)
	    {
            _ximage.width= (int)(( max(_grid_x1,_grid_x2) 
                                - min(_grid_x1,_grid_x2) ) /
                                (_x_value_per_pixel / _zoom_factor));
	    }
         else
            {
            _ximage.width = (int)_user->getGridWidth();
	    }

         if(_ximage.width >= 0 && _ximage.width <= 1) _ximage.width = 2;
         if(_ximage.width < 0)    _ximage.width = (-_ximage.width);
         break;

    case PlotISO:
         if(_zindex == NO_ZOOM)/*not zoomed*/
	    {
            _vel_min = _user->getVelocityMinimum();
            _vel_max = _user->getVelocityMaximum();
            _ximage.width = (int)(_user->getTracesPerInch() * hpixels_per_inch);
	    }
         else if(_zoomdown)
	    {
            _ximage.width = (int)(_ti * hpixels_per_inch);
            }
         else
	    {
            if(_over_image != NULL)
	      {
              index = _over_image->getFirstTraceInImageIndex() *
                      _over_image->getNumberOfHeaders() +
                      _coordinate_header - 1;
              x1 = _over_image->_hd[index];
              index= ( _over_image->getFirstTraceInImageIndex()
                     + _over_image->getNumberDisplayedTraces()- 1)
                     * _over_image->getNumberOfHeaders() + 
                      _coordinate_header-1;
              x2 = _over_image->_hd[index];
              if(_over_image->_user->getRightToLeft())
		{
                temp = x1;
                x1   = x2;
                x2   = temp;
	        }
              _x_value_per_pixel = (x2 - x1)/((_over_image->_ntot-1)
                                       * _over_image->getTraceWidth());
	      }
            for(n=0;n<_cpixm;n++) skip_traces += _tpnl[n];
            skip_traces += (_first_trace_in_image - 1);
            x1 = _hd[skip_traces*_nhdrs
                                    +_coordinate_header-1];
            x2 = _hd[(_ntot-1+skip_traces)
                                    *_nhdrs+_coordinate_header-1];
            if(_over_image != NULL)
               _ximage.width = (int)(( max(x1,x2) - min(x1,x2) ) 
                                   /  fabs(_x_value_per_pixel) 
                                   + _over_image->getTraceWidth());
            else
               _ximage.width = (int)(( max(x1,x2) - min(x1,x2) ) /
                               ( fabs(_x_value_per_pixel) / _zoom_factor));
	    }
         break;

    case PlotWONLY:
    case PlotWFILL:
         _ximage.width  = (int)(_ntot * _trace_delta + _first_trace_location);
         break;

    case PlotCOLOR:
         //next insures that trace type underlays have the same delta as overlay
         if(_can_overlay && _over_image != NULL)
           {
           _trace_delta = _over_image->_trace_delta;
           }
         _ximage.width  = (int)(_ntot * _trace_delta + _first_trace_location);
         break;

    case PlotSEMB:
    case PlotCONTOUR:
         if(!_zoomed)
            {
            _vel_min = _user->getVelocityMinimum();
            _vel_max = _user->getVelocityMaximum(); 
            _ximage.width = (int)(_user->getTracesPerInch() * hpixels_per_inch);
            }
         else
            {
            _ximage.width = (int)((_vel_max - _vel_min)
                                * _velfactor) + 1;  
            }
         break;
    }   

  if(_ximage.width >= 0 && _ximage.width <= 1) _ximage.width = 2;
  if(_ximage.width < 0)    _ximage.width = (-_ximage.width);

  if(_user->getMode() == PlotISO || _user->getMode() == PlotGRID
                                  || _user->getMode() == PlotHEADER)
     {
     if(!_zoomed)
       _ximage.height = (int)(((max(_user->getTmin(),_user->getTmax()))
                        - (min(_user->getTmin(),_user->getTmax())))
                        * _user->getInchesPerSecond() * vpixels_per_inch + 0.5);
     else
        _ximage.height = (int)(((max(_tmin,_tmax))
                            - (min(_tmin,_tmax)))
                             * inches_per_second * vpixels_per_inch + 0.5);
     }
  else
     {
     _ximage.height = (int)((float)(inches_per_second * vpixels_per_inch
                          * (_user->getTdec() * _user->getGlobals().srval)
                          * (_nsamp-1))); 
     }


  if(_ximage.height >= 0 &&  _ximage.height <= 1) _ximage.height = 2;
  if(_ximage.height < 0) _ximage.height = (-_ximage.height);

/*put restraints on number of traces at scale requested if frame buffered*/
  if(_frame_buffer)
     {
     max_width = DisplayWidth(dpy,screen) 
                 - (_left_border + _right_border 
                    + _trace_delta + 8) - 1;
     if(max_width < _ximage.width) 
        {
        warning= True;
        strcpy(_errstr,too_big);
        _ntot = (long) max_width / _trace_delta;
        _ximage.width = (int)(_ntot * _trace_delta + _first_trace_location);
        if(_ximage.width >= 0 && _ximage.width <= 1) _ximage.width = 2;
        if(_ximage.width < 0)    _ximage.width = (-_ximage.width);
        }

     max_height = DisplayHeight(dpy,screen)  
                - (_top_border + _bottom_border) - 1;
     if(max_height < _ximage.height) 
        {
        warning = True;
        strcpy(_errstr,too_big);
        time_reduction = ((float)max_height) / ((float)_ximage.height);
        _nsamp = (long) ((float)_nsamp * time_reduction + .5);
        if(!_zoomed)
           {
           _tmax = _nsamp*(_user->getTdec()*_user->getGlobals().srval)
                       + _user->getTmin();
           integer_samples = (long)(_tmax /
                            (_user->getGlobals().srval * _user->getTdec()) +.5);
           _user->setTmax(   integer_samples *
                             (_user->getGlobals().srval * _user->getTdec()));
           _nsamp = (long)((max(_user->getTmax(),_user->getTmin()) 
                           - min(_user->getTmax(),_user->getTmin()))
                           / _user->getGlobals().srval/_user->getTdec()+1);
           if(_nsamp < 0) _nsamp = -_nsamp;
           _tmax = _user->getTmax();
           _zoomyary[_zindex][1] = _tmax;
           }
        else
           {
           _tmax = _nsamp*(_user->getTdec()*_user->getGlobals().srval)
                       + _zoomyary[_zindex][0];
           addsample = (_user->getTmin() > 0.0) ? 1 : 0;
           integer_samples = (long)(_tmax /
                            (_user->getGlobals().srval * _user->getTdec()) +.5);
           _user->setTmax(  integer_samples *
                             (_user->getGlobals().srval * _user->getTdec()));
           _nsamp = (long)((max(_user->getTmax(),_user->getTmin()) 
                           - max(_user->getTmax(),_user->getTmin()))
                           / _user->getGlobals().srval/_user->getTdec()+ 1);
           if(_nsamp < 0) _nsamp = -_nsamp;
           _tmax = _user->getTmax();
           _zoomyary[_zindex][1] = _tmax;
           }
       _ximage.height = (int)((float)(inches_per_second * vpixels_per_inch
              * (_user->getTdec()*_user->getGlobals().srval)*(_nsamp-1)));
       if(_ximage.height >= 0 &&  _ximage.height <= 1) _ximage.height = 2;
       if( _ximage.height < 0)    _ximage.height = (-_ximage.height);
       }
     }/*end frame buffer*/





/*transfer some needed parameters into the image sturcture*/
  if(!_zoomed && !_scanright && !_scanleft)
     {
     _original_samples = _nsamp;
     _original_traces = _ntot;
     _displayed_traces = _ntot;
     _original_ti = _ti;
     _original_is = _is;
     }


/*set some parameters needed to create pixmaps*/
  if(_user->_mode == PlotWONLY  || _user->_mode == PlotWFILL ||
     _user->_mode == PlotHEADER)
    {//width needs to be extended to be divisible by 8 bytes
/*
/////////////// old ///////////////////////////////
    _ximage.bytes_per_line = (_ximage.width + 7) / 8;
    _ximage.width = _ximage.bytes_per_line * 8;
/////////////// old ///////////////////////////////
*/
/////////////// new ///////////////////////////////
    _ximage.width = (int)((_ximage.width + 7) / 8) * 8;
/////////////// new ///////////////////////////////
    if(_ximage.width >= 0 && _ximage.width <= 1) _ximage.width = 2;
    if(_ximage.width < 0)    _ximage.width = (-_ximage.width);
    temp_delta = (float)_ximage.width / (float)_ntot;
    _trace_delta = (long) temp_delta;
    _first_trace_location = (long)((float)temp_delta / 2.0 + .5);
    }
  else
    {
//  _ximage.bytes_per_line = _ximage.width;
    }

/*abort if the number of pixels is too large for xlib to handle*/
  if(_ximage.width + _left_border + _right_border >= MAXPIXEL || 
     _ximage.height + _top_border + _bottom_border>= MAXPIXEL)
     {
     strcpy(_errstr,pix_estr);
     _displayed= False;
     if (_statusw) wprocPopMsg( _statusw, tempstr);
     return ( ResourceFail );
     }





/********************* START OF NEW ARRAY ALLOCATIONS**************************/
/*free and create pixmaps only if old are not being reused*/
 if(!_use_old_arrays)
    {
    /*see if we have enough memory for image. Have to free previous first*/
    for(i=0; i<MAX_PIXMAP; i++)
       {
       if(_pixmary[i] != 0) 
          {
          XFreePixmap(XtDisplay(_graphic),_pixmary[i]);
          _pixmary[i] = 0;
//////////////////// new /////////////////
//	  _pixmap_set->clearRange (i);
//////////////////// new /////////////////
          }
       }

    if (_statusw)
       {
       sprintf( msg, "Allocating Memory..." );
       wprocShowMsg( _statusw, msg);
       }

    /*now do dummy pixmap creates to see if we have enough server memory*/
    for(i=0;i<images;i++)
       {
       prev_handler= XSetErrorHandler(checkPixmap);
       XSync( dpy, False);
       pixmap_error= False;
       if(_user->getMode() >= PlotCOLOR)
         depth =  DefaultDepthOfScreen(scr);
       else
         depth = 1;
       _pixmary[i]= XCreatePixmap( dpy, XtWindow(_graphic),
                           (int)(_ximage.width+_left_border+_right_border),
                           (int)(_ximage.height+_top_border+_bottom_border),
                           depth );
//////////////////// new /////////////////
/*
       _pixmap_set->setRange (
                           (int)(_ximage.width+_left_border+_right_border),
                           (int)(_ximage.height+_top_border+_bottom_border),
			   0, 0, i);
*/
//////////////////// new /////////////////
       XSync( dpy, False);
       XSetErrorHandler( prev_handler);
       if (pixmap_error)  /*couldnt make this request*/
          {
          _pixmary[i] = 0;
          strcpy(_errstr,pix_estr);
          _displayed= False;
          for(i=0; i<MAX_PIXMAP; i++)
             {
             if(_pixmary[i] != 0)
                { 
                XFreePixmap(XtDisplay(_graphic),_pixmary[i]);
                _pixmary[i] = 0;
//////////////////// new /////////////////
//		_pixmap_set->clearRange (i);
//////////////////// new /////////////////
	        }
             }
         if (_statusw) wprocPopMsg( _statusw, tempstr);
         return ( ResourceFail );
	 }

       if( (_abort_data) && (_abort_function) )
          {
          aborted = _abort_function( _abort_data );
          if( aborted )
             {
             for(i=0; i<MAX_PIXMAP; i++)
               {
               if(_pixmary[i] != 0) 
		  {
                  XFreePixmap(XtDisplay(_graphic),_pixmary[i]);
                  _pixmary[i] = 0;
//////////////////// new /////////////////
//		  _pixmap_set->clearRange (i);
//////////////////// new /////////////////
		  }
               }
             _displayed= False;
             if (_statusw) wprocPopMsg(  _statusw, tempstr);
             return(UserAbort);
             }
          }
       }/*end pixmap creates*/


    /*if we currently have an unzoomed image, free the arrays that created it*/
    if( (_displayed == True) && (_zoomed == False) 
         && (_filedata == True) && (_point_to_data == False))
       {
       if(_hd != NULL){free(_hd); _hd=NULL;}
       if(_byte_array != NULL){free(_byte_array);
          _byte_array=NULL;}
       if(_float_array != NULL && isFloatData())
          {
          free(_float_array);
          _float_array = NULL;
          }
       }

    /*allocate arrays for data that we have to keep around*/
    if(!_zoomed)
       {
       headersize = _user->getGlobals().nhdwd * (2*sizeof(double));
       if( (_filedata == True) && (_point_to_data == False))
          {
          if(_hd != NULL){free(_hd); _hd = NULL;}
          _hd  = (float *) calloc( 1, (int)((_ntot*headersize) * images *
                                             (2*sizeof(double))));
	  }
       if(_hd == NULL)
          {
          strcpy(_errstr,mem_estr);
          _displayed= False;
          for(i=0; i<MAX_PIXMAP; i++)
             {
             if(_pixmary[i] != 0) 
	        {
                XFreePixmap(XtDisplay(_graphic),_pixmary[i]);
                _pixmary[i] = 0;
//////////////////// new /////////////////
//		_pixmap_set->clearRange (i);
//////////////////// new /////////////////
	        }
             }
          if (_statusw) wprocPopMsg( _statusw, tempstr);
          return ( ResourceFail );
          }
       if( (_abort_data) && (_abort_function) )
          {
          aborted =  _abort_function( _abort_data );
          if (aborted)
             {
             for(i=0; i<MAX_PIXMAP; i++)
               {
               if(_pixmary[i] != 0) 
		  {
                  XFreePixmap(XtDisplay(_graphic),_pixmary[i]);
                  _pixmary[i] = 0;
//////////////////// new /////////////////
//		  _pixmap_set->clearRange (i);
//////////////////// new /////////////////
	          }
               }
             if((_filedata == True) && (_point_to_data == False)) 
                {free(_hd); _hd = NULL;}
             _displayed= False;
             if (_statusw) wprocPopMsg(  _statusw, tempstr);
             return(UserAbort);
             }
          }
       if(_user->getMode()==PlotGRID)/*fill header array in user coordinates*/
          { 
          _hd[0] = _grid_x1;
          for(i=1;i<_ntot;i++)
              _hd[i] = _hd[i-1] + _user->getXgridIncrement();
          }

       if(_user->getMode() != PlotGRID   && _user->getMode() != PlotISO &&
          _user->getMode() != PlotHEADER && isFloatData() == False)//byte data
          {
          if(_byte_array != NULL)
              {
              free(_byte_array);
              _byte_array = NULL;
	      }
          _byte_array = ( unsigned char *) malloc( 
                              (int)((_nsamp+(2*sizeof(float)))
                              * _ntot * sizeof(unsigned char)
                              * images + (sizeof(float)*images) ));

          if(_byte_array == NULL)
             {
             strcpy(_errstr,mem_estr);
             _displayed= False;
             if((_filedata == True) && (_point_to_data == False)) 
                {free(_hd); _hd = NULL;}
             for(i=0; i<MAX_PIXMAP; i++)
               {
               if(_pixmary[i] != 0) 
		  {
                  XFreePixmap(XtDisplay(_graphic),_pixmary[i]);
                  _pixmary[i] = 0;
//////////////////// new /////////////////
//		  _pixmap_set->clearRange (i);
//////////////////// new /////////////////
		  }
               }
             if (_statusw) wprocPopMsg(  _statusw, tempstr);
             return ( ResourceFail );
             }
          }
     /*float data allocation*/
    if( (isFloatData()) ||  
        (_user->_mode == PlotSEMB || _user->_mode == PlotCONTOUR 
         && _user->getNumberOfContours() ))
       {
       if( (_float_array != NULL) && (_point_to_data == False))
	 {
         free(_float_array);
         _float_array = NULL;
         }
       if(_point_to_data == False)//If true it should have been alloc'd in Seisplot
         {
         _float_array = (float *) calloc(1, 
                          (int)((_nsamp+(2*sizeof(float)))
                          * _ntot*sizeof(float)
                          * images));
         }
       if(_float_array == NULL)
          {
          strcpy(_errstr,mem_estr);
          if((_filedata == True) && (_point_to_data == False)) 
              {free(_hd); _hd = NULL;}
          free(_byte_array);
          _byte_array = NULL;
          _displayed= False;
          for(i=0; i<MAX_PIXMAP; i++)
             {
             if(_pixmary[i] != 0)
	        {
                XFreePixmap(XtDisplay(_graphic),_pixmary[i]);
                _pixmary[i] = 0;
//////////////////// new /////////////////
//		_pixmap_set->clearRange (i);
//////////////////// new /////////////////
	        }
             }
          if (_statusw) wprocPopMsg(  _statusw, tempstr);
          return ( ResourceFail );
          }
       } 
     }/*end of if not zoomed*/

   /*now do a dummy image data allocation to see if we have enough memory*/
/*
/////////////// old ///////////////////////////////
    if(_ximage.data != NULL)
       {
       free(_ximage.data);
       _ximage.data = NULL;
       }
    _ximage.data = (char *) malloc( (_ximage.height *
                        _ximage.bytes_per_line) * sizeof(char));
/////////////// old ///////////////////////////////
*/

/////////////// new ///////////////////////////////
    _ximage_ptr->deallocate (&_ximage);
    _ximage_ptr->allocate (&_ximage);
/////////////// new ///////////////////////////////
    if(_ximage.data == NULL)
       {
       strcpy(_errstr,pix_estr);
       _displayed= False;
       free(_byte_array);
       _byte_array = NULL;
       if((_filedata== True) && (_point_to_data == False))
           {free(_hd); _hd = NULL;}
       if(isFloatData() || _user->getNumberOfContours())
         {
         if(_point_to_data == False)
           {
           free(_float_array);
           _float_array = NULL;
	   }
         }
       for(i=0; i<MAX_PIXMAP; i++)
          {
          if(_pixmary[i] != 0) 
	     {
             XFreePixmap(XtDisplay(_graphic),_pixmary[i]);
             _pixmary[i] = 0;
//////////////////// new /////////////////
//	     _pixmap_set->clearRange (i);
//////////////////// new /////////////////
	     }
          }
       if (_statusw) wprocPopMsg(  _statusw, tempstr);
       return ( ResourceFail );
       }

    }
/************************* END NEW ARRAY ALLOCATIONS ***********************/




  _displayed = True;

  if (_statusw) wprocPopMsg(  _statusw, tempstr);

  if(!warning)
     return(PlotSuccess);
  else
     return(PlotWarning);
  
}

int PlotImage::checkPixmap(Display * /*dpy*/, XErrorEvent *err )
{
  if (err->error_code == BadAlloc    ||
      err->error_code == BadDrawable ||
      err->error_code == BadValue      )
        pixmap_error= True;
  
  return(pixmap_error);
}
