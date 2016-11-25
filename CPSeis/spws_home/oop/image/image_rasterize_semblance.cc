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
 *Name   : rasterizeSemblance
 *Purpose: Interpolate and convert semblance traces to a color image format.
 *         2D interpolation may be requested.
 *Author : Michael L. Sherrill
 *Date   : 07/91 (C++ version 4/97)
 *
 * Function Definition:
 *void rasterizeSemblance(unsigned char      trace[],
 *                        long               index)
 *
 * trace     in         Byte data array.
 * index     in         Flag for the trace number we are digitizing.
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




int  PlotImage::rasterizeSemblance(    unsigned char      trace_in[], 
                                     long               index,
                                     long               trace_num_index, 
                                     long               HdrOffset,
                                     unsigned char      *raster_array,
                                     long               rwidth,
                                     long               rheight, 
                                     unsigned char      *color_array,
                                     unsigned char      *color_map_array,
                                     Boolean            hardcopy)
{
  float factor, r, fraction, temp;
  float addamt, increment;
  long i, j, k, l, m, o, x;
  long *integer_trace;
  long *trace;
  long tindex;
  long xstart, nextxstart, xdelta;
  long xpos = 0;
  long diff;
  static long firsthdr,lasthdr;
  long totalpixels,rowswap;
  void *junk;
  unsigned char new_val;
  long mapped;
  unsigned char contiguous_colors = True;
  long last_trace = _ntot;
  long start_index;
  void *out_array;
  unsigned long *colors;
  unsigned char *mapcolors;
  long width, height;
  int error = 1;
  int type, type2;

  //When using the selector we may have fewer traces than the pixmap has
  //room for. For now we will not draw a zeroed trace, instead we will 
  //leave it blank.
  if(usingSelector())
    {
      i = _nhdrs * trace_num_index;
      if(_hd[i] < 1.0F)//Sequential trace number header is zero
        return error;
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


  /*see if color map has contiguous colors*/
  for(i=0;i<_col->cnum-1;i++)
    {
      if( abs((long)colors[i+1] - (long)colors[i]) > 1)
        {
          contiguous_colors = False;
          i = _col->cnum;
        } 
    }


  /*allocate height array*/
  integer_trace = NULL;
  trace         = NULL;
  integer_trace = (long *) malloc(height * sizeof(long));
  trace         = (long *) malloc((int)(_nsamp * sizeof(long)));
  if(integer_trace == NULL || trace == NULL)
    {
      printf("allocation error in image_vd\n");
      return error;
    }
  xdelta = _trace_delta;


  


  /*semblance velocity data convert from range of 128-255 to 1-255*/         
  for(i=0;i<_nsamp;i++)trace[i]=(int)(((float)trace_in[i]-127.5)*2.0);


  /* interpolate the trace y */
  if(_nsamp == height)
    {
      for(i=0; i<height; i++)
        {
          integer_trace[i] = trace[i];
          if(_user->_rp)integer_trace[i] = 256 - integer_trace[i];
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
              temp =  ((float)trace[tindex]) + fraction 
                * (((float)trace[tindex+1]) - ((float)trace[tindex]) );
              if (temp >= 0)
                integer_trace[i] = (long)(temp + 0.5);
              else
                integer_trace[i] = (long)(temp - 0.5);
            }
          else /*vertical replication*/    
            {
              integer_trace[i] = trace[tindex];
            }
          if(_user->_rp)integer_trace[i] = 256 - integer_trace[i];
          r += factor;
        }
      for(i=i; i<height; i++) /*last value in trace*/
        {
          integer_trace[i] = trace[_nsamp-1];
          if(_user->_rp)integer_trace[i] = 256 - integer_trace[i];
        }
    }


  /*store y interpolated data*/
  if(!index) /*first time in */
    {
      firsthdr = (long)((_zoomxary[_zindex][0] - 1)* _nhdrs + 5);
      if(!usingSelector())
        {
          lasthdr  = (long)((_zoomxary[_zindex][1] - 1)* _nhdrs + 5);
        }
      else
        {
          long h1 =(long)((_trace_selector->getNumTraces(getCurrentPanel()) - 1)
                           * _nhdrs + 5);
          long h2 = (long)((_zoomxary[_zindex][1] - 1)* _nhdrs + 5);
          lasthdr  = min(h1, h2);
        }
      _vel_min = _hd[firsthdr + HdrOffset];
      _vel_max = _hd[lasthdr + HdrOffset];
      if(_ntot == 1)_vel_max = _vel_min + 1.0;
      _velfactor = (width - 1) 
        / (_vel_max - _vel_min);
      if(!_zoomed)_original_velfactor = _velfactor;
    }

  if(_ntot > 1)
    {
      xstart =(long)((_hd[index*_nhdrs+firsthdr+HdrOffset]-_vel_min) 
                     * _velfactor);
      nextxstart =(long)((_hd[(index+1)*_nhdrs+firsthdr+HdrOffset]-_vel_min) 
                         * _velfactor);
      xdelta = nextxstart - xstart;
    }
  else /*trace data starting position*/
    {
      xstart = index * xdelta + _first_trace_location;
    }

  for(i=0; i<height; i++) /*transfer y data to image array*/
    {
      movePixel (type, out_array, xstart,
        &(colors[mapcolors[integer_trace[i]]]));
      xstart += width;
    }

  /*********if we are done with y, map x into xlib color image format*********/
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
            start_index = 
              _ntot - _trace_selector->getNumTraces(getCurrentPanel());
          else
            start_index = 0;    
          for(i=start_index;i<last_trace - 1;i++)  /*do all but last trace*/
            {
              
              if(last_trace > 1)
                {
                  xstart=(long)((_hd[i*_nhdrs+firsthdr+HdrOffset]-_vel_min) 
                                * _velfactor);
                  nextxstart = (long)((_hd[(i+1)*_nhdrs+firsthdr+HdrOffset]
                                       - _vel_min) *_velfactor);
                  xdelta = nextxstart - xstart;
                }
              else
                {
                  xstart =  xpos = i * xdelta + _first_trace_location;
                }

              if(xdelta > 1) /*smooth values to next trace values*/
                {
		  int indx1, indx2;
                  for(j=0;j<height;j++)
                    {
                      xpos = j * width + xstart;
		      indx1 = getColorIndex (getPixel(type2,out_array,xpos),
			_col->cnum, colors);
		      indx2 = getColorIndex (getPixel(type2,out_array,
			xpos+xdelta), _col->cnum, colors);
		      mapped = indx1 != -1 && indx2 != -1;
		      if (!mapped) {
			int i = 0;
		      }
		      diff = indx2 - indx1;
                      if(diff) /*not much precision here*/
                        {
                          addamt = increment = (float)diff / (float)xdelta;
                          for(k=1; k<xdelta; k++)
                            {
			      if (mapped) {
				int indy;
				indy = NearestInteger ((float)indx1+addamt);
				if (indy < 0 || indy > _col->cnum) {
				  int i = 0;
				}
			        movePixel (type, out_array, k+xpos,
                                  &(colors[indy]));
			      }
			      else {
				movePixel (type2, out_array, k+xpos,
				  out_array, xpos);
			      }
			      if (getColorIndex(getPixel(type2,out_array,
                                k+xpos),_col->cnum,colors) == -1) {
				int i = 0;
			      }
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
              xstart = i * xdelta + _first_trace_location;
              for(j=0;j<height;j++)
                {
                  xpos = j * width + xstart;
                  for(k=1;k<xdelta;k++) 
		    movePixel (type2, out_array, k+xpos, out_array, xpos);
                }
            }
        }
      /*****end x interpolate, now replicate last trace*****/

      if(last_trace > 1)
        {
          xstart=(long)((_hd[index*_nhdrs+firsthdr+HdrOffset]-_vel_min) 
                        * _velfactor);
          nextxstart = width;
          xdelta =     nextxstart - xstart;
          xpos = xstart;
        }
      else
        {
          xstart = xpos = 0;
          xdelta = width;
        } 


      for(l=0; l<height; l++)
        { 
          for(m=1; m<xdelta;m++)
	    movePixel (type2, out_array, m+xpos, out_array, xpos);
          xpos += width; 
        } 



      /*****flip image y axis if requested*****/
      if(_user->_invert_yaxis)
        {
          totalpixels = height * _ximage.width;
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

    }/********end final loop of x mapping into xlib color image format********/



  free(integer_trace);
  free(trace);

  return (error = 0);
}

int PlotImage::getColorIndex (unsigned long color, int num_colors,
  unsigned long *colors)
{
  int retval;

  int k2;
  for (k2 = 0, retval = -1; k2 < num_colors; k2++) {
    if (color == colors[k2]) {
      retval = k2;
      k2 = num_colors;
    }
  }
  return retval;
}
