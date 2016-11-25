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
 *Name   : rasterizeByBytes
 *Purpose: Interpolate and convert float traces to a color image format.
 *         2D interpolation supported. This is faster but less resolution
 *         than rasterizeByFloats. The float value of the traces are used
 *         converted to bytes then mapped into the color map.
 *
 *Author : Michael L. Sherrill
 *Date   : 07/91 (C++ version 4/97)
 *
 * Function Definition:
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
#include "trace_selector.hh"
#include "cprim.h"
#include "named_constants.h"
#include "image_amplitude_recovery.hh"




int  PlotImage::rasterizeByBytes(float              *trace_in, 
                                 long               index,
                                 long               trace_num_index,
                                 unsigned char      *raster_array,
                                 long               rwidth,
                                 long               rheight, 
                                 unsigned char      *color_array,
                                 unsigned char      *color_map_array,
                                 Boolean            hardcopy)
{
  float factor, r, fraction, temp, temp2;
  float addamt, increment;
  long i, j, k, l, m, o, x;
  static float *float_trace;
  long tindex;
  long xstart, nextxstart;
  float xdelta;
  long xpos = 0;
  float diff;
  static long firsthdr,lasthdr;
  long totalpixels,rowswap;
  void *junk;
  unsigned char new_val;
  long mapped;
  unsigned char byte_val;
  long last_trace = _ntot;
  long start_index, ti, nti;
  void *out_array;
  unsigned long *colors;
  unsigned char *mapcolors;
  long width, height;
  int error = 1;
  int type, type2;
  void *pixel;

  //When using the selector we may have fewer traces than the pixmap has
  //room for. For now we will not draw a zeroed trace, instead we will 
  //leave it blank.
  if(usingSelector())
    {
      i = _nhdrs * trace_num_index;
      if(_hd[i] < 1.0F)//Sequential trace number header is zero
        {
        if(usingSelector() && !_zoomed && !_user->getRightToLeft())
          last_trace = min(_trace_selector->getNumTraces(getCurrentPanel()),
                           _zoomxary[_zindex][1]);
        if(index == last_trace - 1)
          free(float_trace);
        return (error = 0);
        }
    }

  if(hardcopy)
    {
      out_array      = (void *)raster_array;
      colors         = (unsigned long *)color_array;
      mapcolors      = color_map_array;
      width          = rwidth;
      height         = rheight;
      type           = BYTE;
      type2          = BYTE;
    }
  else
    {
      out_array      = (void *)_ximage.data;
      colors         = _col->pix;
      mapcolors      = _MapColors;
      width          = _ximage.width;
      height         = _ximage.height;
      type           = P2UI;
      type2          = UINT;
    }



  /*allocate height array*/
  if(index == 0)
    {
      float_trace = (float *) malloc(height * _ntot * sizeof(float));
      if(float_trace == NULL)
        {
          printf("allocation error in image_variable_density_floats\n");
          return error;
        }
    }



  xdelta = (float)width / (float)_ntot;



  /* interpolate the trace y */
  ti = index * height;
  if(_nsamp == height)
    {
      for(i=0; i<height; i++)
        {
          float_trace[ti + i] = trace_in[i];
          if(_user->_rp)float_trace[ti + i] = -float_trace[ti + i];
        }
    }
  else
    {
      factor = (float) (_nsamp - 1)/(float)height;
      r = 0.0;
      for(i=0; i<height-1; i++)
        {
          tindex = (long)r;
          if (tindex >= _nsamp-1) break;
          fraction = r - tindex;
          if(_user->_gradev) /*vertical interpolation requested*/
            { 
              temp = trace_in[tindex];
              temp2= trace_in[tindex+1];
              float_trace[ti + i] = temp + fraction * (temp2 - temp);
            }
          else /*vertical replication*/    
            {
              float_trace[ti + i] = trace_in[tindex];
            }
          if(_user->_rp)float_trace[ti + i] = -float_trace[ti + i];
          r += factor;
        }
      for(i=i; i<height; i++) /*last value in trace*/
        {
          float_trace[ti + i] = trace_in[_nsamp-1];
          if(_user->_rp)float_trace[ti + i] = -float_trace[ti + i];
        }
    }


  /*store y interpolated data*/
  xstart = (long)( (float)index * xdelta + _first_trace_location);

  for(i=0; i<height; i++) /*transfer y data to image array*/
    {
      byte_val = _amp_recovery->scaleAfloat(float_trace[ti + i], 
                                            trace_num_index,_nsamp);

      movePixel (type, out_array, xstart, &(colors[mapcolors[byte_val]]));
      xstart += width;
    }

  /***********if we are done with y, map x into xlib color image format*******/
  if(usingSelector() && !_zoomed && !_user->getRightToLeft())
    last_trace = min(_trace_selector->getNumTraces(getCurrentPanel()),
                     _zoomxary[_zindex][1]);

  

  if(index == last_trace - 1) 
    {  /*****x interpolate data on all but last trace*****/
      if(_user->_gradeh)
        {
          //If we are using the selector and doing a right to left plot and the
          //number of traces in this panel are less than the image area can 
          //contain there should be a blank area at the left of the image.
          //(e.g. image could hold 100 traces but only 90 traces are being
          // displayed). We will therefore skip to the first trace on
          // the left and start our coloring there.
          if(usingSelector() && _user->getRightToLeft() && !_zoomed)
            start_index= _ntot-_trace_selector->getNumTraces(getCurrentPanel());
          else
            start_index = 0;    
          for(i=start_index;i<last_trace - 1;i++)  /*do all but last trace*/
            {
              ti = i * height;
              nti = (i + 1) * height;
              xstart = xpos = (long)((float)i * xdelta + _first_trace_location);
              if(xdelta > 1) /*smooth values to next trace values*/
                {
                  for(j=0;j<height;j++)
                    {
                      xpos = j * width + xstart;
                      diff  = float_trace[nti + j] - float_trace[ti + j];
                      if(diff)
                        {
                          addamt = increment = (float)diff / xdelta;
                          for(k=1; k<xdelta; k++)
                            {
                              byte_val = _amp_recovery->scaleAfloat(
                                                   float_trace[ti+j]+addamt,
                                                   trace_num_index, _nsamp);
			      movePixel (type, out_array, k+xpos,
                                &(colors[mapcolors[byte_val]]));
                              addamt += increment;
                            }/*end for k*/
                        }
                      else /*adjacent trace values are the same just replicate*/
                        {
                          for(k=1; k<xdelta; k++)
                            {
			    movePixel (type2, out_array, k+xpos, out_array,
                              xpos);
                            }
                        }/*end diff*/
                    }/*end for j*/
                }/*end delta > 1*/  
            }/*end for i*/  
        }  
      else /*no horizontal grading so replicate for width of trace*/
        {
          for(i=0;i<last_trace;i++)
            {
              ti = i * height;
              xstart= (long)((float)i * xdelta + _first_trace_location);
              for(j=0;j<height;j++)
                {
                  xpos = j * width + xstart;
                  for(k=1;k<xdelta;k++)
                    {
		    movePixel (type2, out_array, k+xpos, out_array, xpos);
                    }
                } 
            }
        }

      /*****end x interpolate, now replicate last trace*****/
      for(l=0; l<height; l++)
        { 
          xpos = (long)(l * width + (index * xdelta));
          for(m=1; m<xdelta;m++)
	    movePixel (type2, out_array, m+xpos, out_array, xpos);
        } 



      /*****flip image y axis if requested*****/
      if(_user->_invert_yaxis)
        {
          totalpixels = height * width;
          junk = NULL;
	  junk = allocatePixels (type2, width);
          if(junk == NULL)
            {
              printf("could not allocate memory for invert y\n");
              return error;
            }
          rowswap = (int)((float)height / 2.0 + 0.9);
          for(i=0;i<rowswap;i++)
            {
              x = i * width;
              k = totalpixels - width - x;
              for(j=0;j<width;j++)
                {
		  movePixel (type2, junk,      j,   out_array, j+x);
		  movePixel (type2, out_array, j+x, out_array, k  );
		  movePixel (type2, out_array, k,   junk,      j  );
                  k++;
                }
            }
          free(junk);
        }/*end flip*/
    free(float_trace);
    }/********end final loop of x mapping into xlib color image format********/


  return (error = 0);

}



