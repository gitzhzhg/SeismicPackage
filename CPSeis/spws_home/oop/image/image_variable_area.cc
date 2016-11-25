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
 *Name   : variableArea
 *Purpose: Interpolate and convert traces to an integer format,
 *         then digitize integer format into bitmap. 
 *Author : Michael L. Sherrill
 *Date   : 07/91 (C++ version 4/97)
 *
 * Function Definition:
 *void variable_area(unsigned char      trace[],
 *                   long               index)
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
  
#include <stdio.h>
#include <stdlib.h>
#include "plot_image.hh"
#include "trace_selector.hh"





void PlotImage::variableArea(unsigned char     trace[], 
                             long              index,
                             long              trace_num_index )
{
  float factor, r, fraction, temp, temp2;
  long i, j;
  long *integer_trace;
  long tindex;
  long xstart, ystart;
  long xmin, xmax;
  long type = 1;
  long start, end, length;
  long byte_index, line_index;
  long shift;
  static long *ext=NULL;
  static long length_of_ext=0;  
  long  totalpixels, k, x;
  long rowswap;
  unsigned char *junk;
 
  //When using the selector we may have fewer traces than the pixmap has
  //room for. For now we will not draw a zeroed trace, instead we will 
  //leave it blank.
  if(_use_selector )
    {
    i = _nhdrs * trace_num_index;
    if(_hd[i] < 1.0F)//Sequential trace number header is zero
      return;
    }

  /*allocate size of array*/
  integer_trace = NULL;
  integer_trace = (long *) malloc(_ximage.height * sizeof(long));
  if(integer_trace == NULL)
     {
     printf("allocation error in image_va\n");
     return;
     }


/*interpolate the trace */
  if(_nsamp == _ximage.height) 
     {
     for(i=0; i<_ximage.height; i++) 
        {
        integer_trace[i] = (long)(_display_scale * ((float)trace[i] - 128.0));
        if(_user->_rp)integer_trace[i] = -integer_trace[i];
        }
     } 
  else
     {
     factor = (float) (_nsamp - 1)/(float)_ximage.height;
     r = 0.0;
     for(i=0; i<_ximage.height-1; i++)
        {
        tindex = (long)r;
        if (tindex >= _nsamp-1) break;
        fraction = r - tindex;
        temp = _display_scale*( ((float)trace[tindex]-128.0) + fraction 
             *  ( ((float)trace[tindex+1]-128.0)
             - ((float)trace[tindex]-128.0) ));
        if (temp >= 0)
           integer_trace[i] = (long)(temp + 0.5);
        else
           integer_trace[i] = (long)(temp - 0.5);
        if(_user->_rp) integer_trace[i] = -integer_trace[i];
        r += factor;
        }
     for(i=i; i<_ximage.height; i++) /*last value in trace*/ 
        {
        integer_trace[i] = (long)(_display_scale 
                         * ((float)trace[_nsamp-1]-128.0));
        if(_user->_rp) integer_trace[i] = -integer_trace[i];
        }
     }

  xstart = index * _trace_delta + _first_trace_location;
  ystart = 0;
  xmin = (long)(xstart - _user->_ct * _trace_delta);
  if (xmin < 0) xmin = 0;
  xmax = (long)(xstart + _user->_ct * _trace_delta);
  if (xmax > _ximage.width-1) xmax = _ximage.width-1;


/******************* digitize wiggle type traces ********************/
  if (_user->_mode == PlotWONLY) type = 0;
  if (_user->_mode == PlotWFILL && _neg_fill == 0)type=1;
  if (_user->_mode == PlotWFILL && _neg_fill == 1)type=2; 

  line_index = ystart * _ximage.bytes_per_line;
  /* allocate memory for ext array if necessary */

  if(length_of_ext < _ximage.height) 
     {
     if (ext != NULL) {free(ext); ext = NULL;}
     ext = (long *) malloc( _ximage.height*sizeof(long));
     if(ext == NULL)
        {
        printf("allocation error in image_va\n");
        return;
        }
     length_of_ext = _ximage.height;
     }


  /* find the extension for trace */
  for(i=0; i<_ximage.height-1; i++) 
     {
     if(integer_trace[i] > integer_trace[i+1])
        ext[i] = integer_trace[i+1] - integer_trace[i] + 1;
     else if (integer_trace[i] < integer_trace[i+1])
        ext[i] = integer_trace[i+1] - integer_trace[i] - 1;
     else
        ext[i] = 0;
     }
  ext[_ximage.height-1] = 0;
    

  /* now fill in the trace */
  for(i=0; i<_ximage.height; i++) 
     {
     if(type == 0) /* wiggle only */ 
        {
        start = xstart + integer_trace[i];
        end = start + ext[i];
        } 
     else if (type == 1) /* va + wiggle, positive fill */ 
        {
        if(integer_trace[i] > 0) 
           {
           start = xstart;
           if (ext[i] > 0) 
              end = xstart + integer_trace[i] + ext[i];
           else 
              end = xstart + integer_trace[i];
           if (ext[i] + integer_trace[i] < 0) 
              start += (ext[i]+integer_trace[i]);
           } 
        else 
           {
           start = xstart + integer_trace[i];
           end = start + ext[i];
           }
        } 
     else /*type 2 va + wiggle, negative fill */ 
        {
        if(integer_trace[i] < 0) 
           {
           start = xstart;
           if (ext[i] < 0) 
              end = xstart + integer_trace[i] + ext[i];
           else 
              end = xstart + integer_trace[i];
           if (ext[i] + integer_trace[i] > 0)start += (ext[i]+integer_trace[i]);
           } 
        else 
           {
           start = xstart + integer_trace[i];
           end = start + ext[i];
           }
        }

     if(start < end) 
        {
        length = end - start + 1;
        } 
     else 
        {
        length = start - end + 1;
        temp2 = end;
        end = start;
        start = (long)temp2;
        }

     /* take care of clipping */
     if(end < xmin) 
        {
        start = xmin;
        length = 1;
        } 
     else if (start < xmin) 
        {
        length -= (xmin - start);
        start = xmin;
        }
  
     if(start > xmax) 
        {
        start = xmax;
        length = 1;
        } 
     else if (start + length - 1 > xmax) 
        {
        length = xmax - start + 1;
        }

     /* translate start into a byte address and shift */
/*
     // this was an interesting idea but won't work
     if (_ximage.depth > 1) {
       index = line_index + start;

       if (length > 0)  {
	 for (j = 0; j < length; j++) {
	   _ximage.data[index] = TRUE;
	   index++;
	 }
       }
     }
     else {
*/
       byte_index = start >> 3;
       shift = start - (byte_index << 3);

       /* fill the bitmap */
       if (length > 0)  {
	 for (j = 0; j < length; j++) {
           _ximage.data[line_index + byte_index] = 
             (char)(_ximage.data[line_index + byte_index] | (128 >> shift));
           shift++;
           if (shift == 8) {
	     shift = 0;
	     byte_index++;
	   }
	 }
       }
//   }
  

     line_index += _ximage.bytes_per_line;
     }/*end for i to height*/  



/*invert y axis of image if requested*/
 if( (index == _ntot - 1) && (_user->_invert_yaxis == True) )
    {
    totalpixels = _ximage.height * _ximage.bytes_per_line;
    junk = NULL;
    junk = (unsigned char *) malloc(_ximage.bytes_per_line
                                    * sizeof(unsigned char));
    if(junk == NULL)
       {
       printf("could not allocate memory for invert y in variable area\n");
       return;
       }
    rowswap = (int)((float)_ximage.height / 2.0 + 0.9);
    for(i=0;i<rowswap;i++)
       {
       x = i * _ximage.bytes_per_line;
       k = totalpixels - _ximage.bytes_per_line - x;
       for(j=0;j<_ximage.bytes_per_line;j++)
          {
          junk[j] = _ximage.data[j+x];
          _ximage.data[j+x] = _ximage.data[k];
          _ximage.data[k] = junk[j];
          k++;
          }
       }
    free(junk);
    }



 
 free(integer_trace);
}
