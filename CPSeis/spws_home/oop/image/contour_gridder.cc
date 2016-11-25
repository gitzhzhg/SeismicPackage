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
//******************************************************************************
//                 confidential and proprietary information                    *
//                              of conoco inc.                                 *
//                      protected by the copyright law                         *
//                          as an unpublished work                             *
//******************************************************************************


//=============================================================================
//========== This method draws contours over an iso array type plot        ====
//========== or contours alone based on the iso array type plot data       ====
//=============================================================================

#include <math.h>
#include "plot_image.hh"
#include "tfdefs.h"
#include "image_amplitude_recovery.hh"
#include "sl/paintset_collection.hh"


int PlotImage::contourGridder(long AryOffset, long HdrOffset)
{
 float xs[10], ys[10];
 float spcnvl[256];
 long  indivl;
 long  start_index,end_index;
 float gpiy, zminm, zmaxm;
 long i, j, k, m, n, s, t;
 long is;
 float smin, smax, conint, conval;
 long jpt, i1;
 float z1, z2, z3, z4, di, dj;
 float dx, dy;
 float timefactor; 
 int X1, X2, Y1, Y2;
 int max_x, max_y;
 long firsthdr,lasthdr;
 Display *dpy;
 Screen *screen;
 float *xarray, LabelValue;
 char label[8];
 long numt;
 long trace_index;
 float xmin, xmax, xfactor;
 long data_offset = AryOffset;
 float add_to_x = 0.5;


      //If the data is not graded horizontally the x locations
      //line up better if half a trace width is added to them.
      //This contouring tries to find semblance with adjacent trace 
      //values and centers at the trace location but the data
      //has been replicated from left to right (the trace is not
      //centered, its starts on the left)
      if(!_user->getGradeHorizontal())
         add_to_x += ( (float)getTraceWidth() / 2.0F );

      if(_col_two == NULL) return(ResourceFail);

      dpy = XtDisplay(_graphic);
      screen = XtScreen(_graphic);

      

      indivl           = _user->getNumberOfContours();      
      trace_index      = _cpixm * getTracesInMemory();
      start_index      = (long)(_zoomxary[_zindex][0] - 1);
      end_index        = (long)(_zoomxary[_zindex][1] - 1);
      numt             = abs((int)end_index - (int)start_index) + 1;
      firsthdr         = (long)(start_index*_nhdrs);
      lasthdr          = (long)(end_index  *_nhdrs);
      gpiy             = _user->getGlobals().srval;
      zminm            = 0.0;
      zmaxm            = _tmin;
      timefactor       = (_tmax - zmaxm) / (_ximage.height-1);
      if(_zoomed) 
        data_offset   += getSampleIndexFromTime(_tmin) +
                                          start_index * getSamplesInMemory();
        
      xarray = (float *) malloc((int)(numt * sizeof(float)));
      if(xarray == NULL) return(ResourceFail);
      j = _coordinate_header - 1;
      for(i = 0; i < numt; i++)
         {
         xarray[i] = _hd[j+HdrOffset+firsthdr];
         j += _nhdrs;
         }
      if(!_user->getRightToLeft())
        {
        xmin     = xarray[0];
        xmax     = xarray[numt - 1];
        }
      else
        { 
        xmin     = xarray[numt - 1];
        xmax     = xarray[0];
        }

      xfactor  = (xmax - xmin) / (_ximage.width - 1);

      max_x = _left_border + _ximage.width;
      max_y = _top_border  + _ximage.height;
      /***************************************************
      select contour values for display
      spcnvl(i) = value for contour i
      ****************************************************/
      conint = 0.0;
      smin = _user->_minp;
      smax = _user->_maxp;
      if(indivl > 1) conint =  _user->_contour_increment;
      for(j=0;j<indivl;j++) spcnvl[j] = smin + (conint*j);     
      dx = dy = 1.0;
      
      /************************************************
       select a contour value. scan from top to bottom left to right.
       determine the min and max values on the grid square
      z1 .. z4
      .      .
      .      . 
      z2 .. z3
      ************************************************/
      if(_over_image != NULL)//use a bolder line to show up better
        XSetLineAttributes( dpy, _gc2, 4, LineSolid, CapButt, JoinMiter);    
      else
        XSetLineAttributes( dpy, _gc2, 2, LineSolid, CapButt, JoinMiter);

      //Draw the contours
      for(k=0, t = 3; k<indivl;  k++, t+=4)
         {
         conval = spcnvl[k];
         LabelValue = conval;
         _user->setContourCbarRgb(t, conval);
         sprintf(label,"%10.3e",LabelValue);
         XSetForeground( dpy, _gc2, _col_two->pix[k] );
         //next lines would write the contour values in upper left plot corner
         /*
         Y1 = (int)((k + 1) * _fixedcharheight + (int)_top_border);
         X1 = (int)(_left_border + 5);
         if(!_zoomed)
           XDrawString(dpy,_pixmary[_cpixm],_gc2, X1,Y1,label,label_length);
         */
         for(i=0,j=0; i<numt-1; i++, j++)
            {
            jpt = j * getSamplesInMemory();
            for(s=0;s<_nsamp-1;s++)
               {
               i1 = jpt + s;
               z1 = _float_array[i1+  data_offset];
               z2 = _float_array[i1+1+data_offset];
               z3 = _float_array[i1+1+data_offset+getSamplesInMemory()];
               z4 = _float_array[i1+  data_offset+getSamplesInMemory()];
               di = i;
               dj = s;
               is = 0;
               contourLevel(&di,&dj,&conval,&z1,&z2,&z3,&z4,
                            &dx, &dy, xs, ys, xarray, &conint,
                            &zminm, &zmaxm, &gpiy, &is, &numt);
               if(is >= 2)      
                  {
                  for(m=1;m<is;m+=2) 
                     {
                     n  = m + 1;
                     X1 = (int) (((xs[m] - xmin) / xfactor
                        + _left_border + add_to_x)); 
                     X2 = (int) (((xs[n] - xmin) / xfactor
                        + _left_border + add_to_x));
                     Y1 = (int) (((ys[m] - _tmin) / timefactor
                        + _top_border + 0.5));
                     Y2 = (int) (((ys[n] - _tmin) / timefactor
                        + _top_border + 0.5));
                     if(X1 < max_x && 
                        X2 < max_x &&
                        Y1 < max_y && 
                        Y2 < max_y   )
                           XDrawLine(dpy,_pixmary[_cpixm],
                                         _gc2,X1,Y1,X2,Y2);
                     }
                  }        
               }
            }         
         is = 0;
         }

      XSetForeground( dpy, _gc2, PaintsetCollection::black(screen));
      XSetLineAttributes( dpy, _gc2, 2, LineSolid, CapButt, JoinMiter);
      if(xarray != NULL){free(xarray);xarray=NULL;}
      return(PlotSuccess);
}



