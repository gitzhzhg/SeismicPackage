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
 *Name   : process_zoom
 *Purpose: Zoom up or down an image by the zoom factor requested.
 *
 *Author : Michael L. Sherrill
 *Date   : 12/92 (C++ version 4/97)
 *
 * Function Definition:
 * int  process_zoom( long   zoom_type,
 *                    long   x1,
 *                    long   y1,
 *                    long   x2,
 *                    long   y2,
 *                    long   pan)
 *
 * zoom_type in       Flag for zoom up, down, or original size action.
 * x1        in       Left pixel coordinate.
 * y1        in       Upper pixel coordinate.
 * x2        in       Right pixel coordinate.
 * y2        in       Lower pixel coordinate.
 * pan       in       Flag to pan the zoomed image.
 *
 *NOTES:
 * 1. The structure member zindex is used to keep track of what level of
 *    zoom the image is currently in. When it is less than 100 it is zoomed
 *    down (i.e. zindex of 8 means it has been zoomed down twice), when
 *    it is greater than 100 it indicates it is zoomed up, when equal to 100
 *    it is original size.
 *
 *
 *Revisions:
 *DATE      WHO         DESCRIPTION
 * --------  --------    --------------------------------------------
 *
 *END DOC
 *------------------------------------------------------------------*/





#include <stdio.h>
#include "plot_image.hh"
#include "trace_selector.hh"
#include <X11/Xlib.h>
#include <math.h>

#define zoomerr "Zoom was outside of data area..."

/*++++++++++++++++++++++++++CALL PROPER ZOOM ROUTINE+++++++++++++++++*/
int PlotImage::processZoom( long   zoom_type,
                            long   x1,
                            long   y1,
                            long   x2,
                            long   y2,
                            long   pan)

{
 int stat = PlotSuccess;
 long sx, sy, ex, ey;
  
  sx = x1 - _dest_x;
  sy = y1 - _dest_y;
  ex = x2 - _dest_x;
  ey = y2 - _dest_y;

  switch(zoom_type)
  {
    case Z_ORIGINAL:
         stat = zoomOriginal();
    break;

    case Z_UP:
         stat = zoomUp(sx,sy,ex,ey,pan);
    break;

    case Z_DOWN:
         if(_zindex != 11)
            stat = zoomDown();
         else
            stat = zoomOriginal();
    break;
  }


  /*recursive call to zoom an underlay image*/
  if(stat == PlotSuccess && _chain_image != NULL)
     {
     _chain_image->_zoom_factor = _zoom_factor;
     _chain_image->processZoom(zoom_type,x1,y1,x2,y2,pan);
     }

  return(stat);

}



/*+++++++++++++++++++++++RETURN IMAGE TO ORIGINAL SIZE+++++++++++++++++*/

int PlotImage::zoomOriginal() 
{
 long stat;
 Boolean reread = False;

   if(_zindex == NO_ZOOM) return(PlotSuccess);
   _zindex = NO_ZOOM;

   if(_user->_mode == PlotGRID || _user->_mode == PlotHEADER)
     {
     _zoomup = False;
     _zoomdown = False;
     _zoom_scan = False;
     stat = zoom(&_zoom_factor);
     _zoomup = False;
     _zoomdown = False;
     _zoom_scan = False;
     _zoomed = False;
     return(stat);     
     }

   _tmin = _user->_tmin;
   _tmax = _user->_tmax;
   _ti = _original_ti;
   _is = _original_is;
   _user->_nplt = getTracesInMemory();
   _zoom_factor = 1.0;

   if(_user->_mode == PlotISO) _zoomup = False;

   if(_zoom_scan) reread = True;
   if(_Cl.nsamp < _original_samples) reread = True;

   if (reread)
      {
      _zoomed = False;
      stat = checkSize();
      stat = plot();
      }
   else
      {
      stat = zoom(&_zoom_factor);
      _zoomed = False;
      }

   _zoomup = False;
   _zoomdown = False;
   _zoom_scan = False;
   return(stat);
}


/*+++++++++++++++++++++++++++++ZOOM UP IMAGE +++++++++++++++++++++++++++++*/
int PlotImage::zoomUp( long   x1,
                       long   y1,
                       long   x2,
                       long   y2,
                       long   pan)

{
 long tempx;
 long stat = PlotSuccess;
 long ty;
 double tempy;
 long integer_samples;
 double min_x, max_x;
 double min_y, max_y;
 int one_trace_zoom = False;
 int cant_zoom = False;
 long zi;
 long hdr_id = 5;
 float start_val,end_val;
 long start_trace = 0;
 long end_trace = 0; 
 long i,pan_width;
 float diff;
 long zoomed_width;
 float tval,smallheader,bigheader; 
 int incrementing = 1;
 long max_trace = 0;

  
  if(_user->_mode == PlotISO) hdr_id = _coordinate_header - 1;

  if(y1 > y2)
     {
     ty = y1;
     y1 = y2;
     y2 = ty;
     }


  if(!pan) 
     {
     ++_zindex;
     if(_zindex == NO_ZOOM) ++_zindex;/*make sure > original size*/
     }
  zi = _zindex;


/***********find x zoom range by velocity if semblance or contours***********/
  if(_user->_mode == PlotSEMB || _user->_mode == PlotCONTOUR)
    {
    if(!usingSelector())
       max_trace = getTracesInMemory();
    else
       max_trace = _trace_selector->getNumTraces(getCurrentPanel());
    start_val= (x1 - _left_border)/_velfactor + _vel_min;
    end_val  = (x2 - _left_border)/_velfactor + _vel_min;
    start_val = max(start_val,_hd[hdr_id]);
    end_val= min(end_val,_hd[(max_trace-1)*_nhdrs+hdr_id]);
    for(i=0;i<max_trace-1;i++)
        { 
        if(start_val >= _hd[i*_nhdrs+hdr_id] &&
           start_val <= _hd[(i+1)*_nhdrs+hdr_id]) start_trace= i+1;
        if(end_val   >= _hd[i*_nhdrs+hdr_id] &&
           end_val   <= _hd[(i+1)*_nhdrs+hdr_id]) end_trace  = i+1;
        if(end_trace == max_trace - 1) end_trace++;
        }
     if(pan) /*spread image out to same size as previous image*/
        {
        zoomed_width = x2 - x1 + 1;
        pan_width = 0;
        i = end_trace - 1;
        start_val = _hd[(start_trace-1)*_nhdrs+hdr_id];
        while(pan_width < zoomed_width && i < max_trace)
           {   
           end_val =   _hd[i*_nhdrs+hdr_id];
           pan_width = (long)((end_val - start_val) * _velfactor);
           end_trace = i+1;
           i++;
           }
        i = start_trace - 1;
        end_val =   _hd[(end_trace-1)*_nhdrs+hdr_id];
        while(pan_width < zoomed_width && i > 1)
           {
           start_val = _hd[i*_nhdrs+hdr_id];
           pan_width = (long)((end_val - start_val) * _velfactor);
           start_trace = i+1;
           i--;
           }
        }
    _zoomxary[zi][0] = start_trace;
    _zoomxary[zi][1] = end_trace;
    }


/***********find x zoom range by x header id if iso type display*************/
  if(_user->_mode == PlotISO)
    {
    if(_can_overlay && _over_image != NULL)/*match to overlay*/
      {
      start_val = 
             _over_image->_hd[((int)_over_image->_zoomxary[zi][0]-1)
                                        * _nhdrs + hdr_id];
      end_val   = 
             _over_image->_hd[((int)_over_image->_zoomxary[zi][1]-1)
                                        * _nhdrs + hdr_id];
      }
    else
      {
      start_val = (x1 - _left_border)*_x_value_per_pixel 
                + _grid_x1;
      end_val   = (x2 - _left_border)*_x_value_per_pixel
                + _grid_x1;
      }
    if(start_val > end_val)
      {
      tval = start_val;
      start_val = end_val;
      end_val = tval;
      }
    smallheader = min(_hd[hdr_id],
                      _hd[(getTracesInMemory()-1)*_nhdrs+hdr_id]);
    bigheader =   max(_hd[hdr_id],
                      _hd[(getTracesInMemory()-1)*_nhdrs+hdr_id]);
    if(start_val < smallheader)start_val = smallheader;
    if(start_val > bigheader)start_val = bigheader;
    if(end_val > bigheader)end_val = bigheader;
    if(end_val < smallheader)end_val = smallheader;

    if(_ntot > 1)
        incrementing = _hd[hdr_id + _nhdrs] > _hd[hdr_id] ? 1 : 0;

    for(i=0;i<getTracesInMemory()-1;i++)
      {
      if(incrementing)
        {
        if(start_val >= _hd[i*_nhdrs+hdr_id] &&
           start_val <= _hd[(i+1)*_nhdrs+hdr_id])
          {
          if( fabs(start_val - _hd[i*_nhdrs+hdr_id]) <
              fabs(start_val - _hd[(i+1)*_nhdrs+hdr_id]) )
             start_trace = i + 1;
          else
             start_trace = i + 2;
          }
        if(end_val   >= _hd[i*_nhdrs+hdr_id] &&
           end_val   <= _hd[(i+1)*_nhdrs+hdr_id]) 
          {
          if( fabs(end_val - _hd[i*_nhdrs+hdr_id]) <
              fabs(end_val - _hd[(i+1)*_nhdrs+hdr_id]) )
             end_trace = i + 1;
          else
             end_trace = i + 2;
          }
        }
      else//decrementing iso headers
        {
        if(start_val <= _hd[i*_nhdrs+hdr_id] &&
           start_val >= _hd[(i+1)*_nhdrs+hdr_id])
          {
          if( fabs(start_val - _hd[i*_nhdrs+hdr_id]) <
              fabs(start_val - _hd[(i+1)*_nhdrs+hdr_id]) )
             start_trace = i + 1;
          else
             start_trace = i + 2;
          }
        if(end_val   <= _hd[i*_nhdrs+hdr_id] &&
           end_val   >= _hd[(i+1)*_nhdrs+hdr_id]) 
          {
          if( fabs(end_val - _hd[i*_nhdrs+hdr_id]) <
              fabs(end_val - _hd[(i+1)*_nhdrs+hdr_id]) )
             end_trace = i + 1;
          else
             end_trace = i + 2;
          }
        }
    }

    if(start_trace > end_trace)
      {
      tempx = start_trace;
      start_trace = end_trace;
      end_trace = tempx;
      }
    if(start_trace == 0 && end_trace == 0)/*no match*/
      {
      stat = ReadFail;
      strcpy(_errstr,"Could not match underlay data in zoom or pan\n");
      return(stat);
      }
    if(start_trace <= 0)start_trace = 1;
    if(start_trace > getTracesInMemory())start_trace=getTracesInMemory();
    if(end_trace > getTracesInMemory())  end_trace = getTracesInMemory();
    if(end_trace <= 0)end_trace = 1;
    _zoomxary[zi][0] = start_trace;
    _zoomxary[zi][1] = end_trace;
  }//end iso type


/******************* find x zoom area if grid type display ******************/
 if(_user->_mode == PlotGRID || _user->_mode == PlotHEADER)
    {
    _zoomxary[zi][0]= getXfromXpixel(x1);
    _zoomxary[zi][1]= getXfromXpixel(x2);
    }




/*******find traces to zoom if not a velocity or grid type display***********/
 if(_user->_mode != PlotSEMB && _user->_mode != PlotCONTOUR &&
    _user->_mode != PlotISO  && _user->_mode != PlotGRID    &&
    _user->_mode != PlotHEADER)
    {
    /*find first trace to zoom*/
    _zoomxary[zi][0] = (x1-_left_border +_trace_delta)
                                       / _trace_delta;
    if(_user->_RtoL && pan == False)
      _zoomxary[zi][0] = _displayed_traces
                             + 1 - _zoomxary[zi][0];
    if(_zoomed)
      _zoomxary[zi][0]= _zoomxary[zi][0]
                           + _zoomxary[zi-1][0] - 1;

    /*find trace to end zoom*/                              
    _zoomxary[zi][1] = (x2-_left_border+_trace_delta )
                          / _trace_delta;
    if(_user->_RtoL && pan == False)
       _zoomxary[zi][1] = _displayed_traces + 1 
                            - _zoomxary[zi][1];

    if(_zoomed)
       _zoomxary[zi][1]= _zoomxary[zi][1]
                           + _zoomxary[zi-1][0] - 1;
    }


/*********************** all types **********************************/

/*find starting time of zoom*/
   _zoomyary[zi][0] = getYfromYpixel(y1);

/*find ending time of zoom*/
   _zoomyary[zi][1] = getYfromYpixel(y2);

/*round the y times to nearest samples and flip coordinates if reversed*/
   if(_user->_mode != PlotGRID && _user->_mode != PlotHEADER &&
      _user->_mode != PlotISO)
     {
     integer_samples = (long)(_zoomyary[zi][0]  
                     / (_user->_G.srval * _user->_tdec) + .5);
     _zoomyary[zi][0] = integer_samples 
                            * (_user->_G.srval * _user->_tdec);
     integer_samples = (long)(_zoomyary[zi][1] 
                     / (_user->_G.srval * _user->_tdec) + .5);
     _zoomyary[zi][1] = integer_samples 
                            *  (_user->_G.srval * _user->_tdec);
     if(_zoomxary[zi][0] > _zoomxary[zi][1])
         {
         tempx = (long)_zoomxary[zi][0];
         _zoomxary[zi][0] = _zoomxary[zi][1];
         _zoomxary[zi][1] = tempx;
         }
     if(_zoomyary[zi][0] > _zoomyary[zi][1])
         {
         tempy = _zoomyary[zi][0];
         _zoomyary[zi][0] = _zoomyary[zi][1];
         _zoomyary[zi][1] = tempy;
         }
     }

/*keep x and y zoom points within data area*/
   if(_user->_mode == PlotGRID || _user->_mode == PlotHEADER ||
      _user->_mode == PlotISO) 
      {
      if(_user->_mode == PlotISO)
        {
        min_x = 1;
        max_x = getTracesInMemory();
        }
      else
        {
        min_x = min(_user->_grid_x1,_user->_grid_x2);
        max_x = max(_user->_grid_x1,_user->_grid_x2);
        }
      min_y = min(_user->_grid_y1,_user->_grid_y2);
      max_y = max(_user->_grid_y1,_user->_grid_y2);
      }
   else 
      {
      min_x = 1;
      if(!usingSelector())
        max_x = getTracesInMemory();
      else
        max_x = _trace_selector->getNumTraces(getCurrentPanel());
      min_y = _user->_tmin;
      max_y = _user->_tmax;
      }

   if(pan)  /*pan mode restraints*/
      {
      if(_zoomxary[zi][0] < min_x) 
         {
         diff = min_x - _zoomxary[zi][0];
         _zoomxary[zi][1] += diff;
         _zoomxary[zi][0] = min_x;
         }
      if(_zoomxary[zi][0] > max_x)
         {
         diff = _zoomxary[zi][0] - max_x;
         _zoomxary[zi][1] -= diff;
         _zoomxary[zi][0] = max_x;
         }
      if(_zoomxary[zi][1] < min_x)
         {
         diff = min_x - _zoomxary[zi][1];
         _zoomxary[zi][0] += diff;
         _zoomxary[zi][1] = min_x;
         }
      if(_zoomxary[zi][1] > max_x) 
         {
         diff = _zoomxary[zi][1] - max_x;
         _zoomxary[zi][0] -= diff;
         _zoomxary[zi][1] = max_x;
         }
      if(_zoomyary[zi][0] < min_y) 
         {
         diff = min_y - _zoomyary[zi][0];
         _zoomyary[zi][1] += diff;  
         _zoomyary[zi][0] = min_y;
         }
      if(_zoomyary[zi][0] > max_y)
         {
         diff = _zoomyary[zi][0] - max_y;
         _zoomyary[zi][1] -= diff;
         _zoomyary[zi][0] = max_y;
         }
      if(_zoomyary[zi][1] > max_y) 
         {
         diff = _zoomyary[zi][1] - max_y;
         _zoomyary[zi][0] -= diff; 
         _zoomyary[zi][1] = max_y;
         }
      if(_zoomyary[zi][1] < min_y)
         {
         diff = min_y - _zoomyary[zi][1];
         _zoomyary[zi][0] += diff;
         _zoomyary[zi][1] = min_y;
         }
      }/*end pan mode restraints*/


   if(_zoomxary[zi][0] < min_x) _zoomxary[zi][0] = min_x;
   if(_zoomxary[zi][0] > max_x) _zoomxary[zi][0] = max_x;
   if(_zoomxary[zi][1] > max_x) _zoomxary[zi][1] = max_x;
   if(_zoomxary[zi][1] < min_x) _zoomxary[zi][1] = min_x;
   if(_zoomyary[zi][0] < min_y) _zoomyary[zi][0] = min_y;
   if(_zoomyary[zi][0] > max_y) _zoomyary[zi][0] = max_y;
   if(_zoomyary[zi][1] > max_y) _zoomyary[zi][1] = max_y;
   if(_zoomyary[zi][1] < min_y) _zoomyary[zi][1] = min_y;


    
   if(_zoomxary[zi][0] == _zoomxary[zi][1]) one_trace_zoom = True;

   if(_user->_mode != PlotGRID && _user->_mode != PlotHEADER &&
      _user->_mode != PlotISO)
      {
      if(!one_trace_zoom)
         {
         if(_zoomxary[zi][1]-_zoomxary[zi][0] < 1)
            cant_zoom = True;
         }
      if(_zoomyary[zi][1]-_zoomyary[zi][0] < _user->_G.srval)
          cant_zoom = True;
      }

/*abort zoom if still outside of data area*/
   if( cant_zoom )
      {
      --_zindex;
      if(_zindex == NO_ZOOM)
         {
         _zoomed = _zoomup = _zoomdown = False;
         stat = PlotSuccess;
         }
      refresh(0,0, _graph_width, _graph_height );
      strcpy(_errstr,zoomerr);
      }
   else
      {
      _zoomup = True;
      _zoomdown = False;
      stat = zoom(&_zoom_factor);
      }



  return (stat);

}



/*+++++++++++++++++++++++++++ZOOM DOWN IMAGE++++++++++++++++++++++++++*/

int PlotImage::zoomDown( )


{
long stat;

   --_zindex;
   if(_zindex >= 0)
      {
      _zoomdown = True;
      _zoomup   = False;
      if(_zindex <= NO_ZOOM)  /*use all of original data*/
         {
         if(_user->_mode == PlotGRID || _user->_mode == PlotHEADER)
            {
            _zoomxary[_zindex][0] = _user->_grid_x1;
            _zoomxary[_zindex][1] = _user->_grid_x2;
            _zoomyary[_zindex][0] = _user->_grid_y1;
            _zoomyary[_zindex][1] = _user->_grid_y2;
            }
         else
            {
            _zoomxary[_zindex][0] = 1;
            _zoomxary[_zindex][1] = getTracesInMemory();
            _zoomyary[_zindex][0] = _user->_tmin;
            _zoomyary[_zindex][1] = _user->_tmax;
            }
         }
      stat = zoom(&_zoom_factor);
      }
   else 
      {
      stat = PlotSuccess;
      }

 
  return (stat);
}
