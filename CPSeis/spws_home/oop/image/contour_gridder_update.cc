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
//========== This method repairs contours over an iso array type plot      ====
//========== or contours alone based on the iso array type plot data       ====
//========== Note: It does not yet support a windowed time.                ====
//=============================================================================

//=============================================================================
//==========  Author Michael L. Sherrill                                   ====
//=============================================================================


#include <math.h>
#include "plot_image.hh"
#include "tfdefs.h"
#include "image_amplitude_recovery.hh"
#include "sl/paintset_collection.hh"

int PlotImage::contourGridderUpdate(long              first_column,
                                    long              last_column,
                                    long              starting_sample,
                                    long              ending_sample,
                                    long              pixmap_index,
                                    long              *expose_x,
                                    long              *expose_y,
                                    long              *expose_width,
                                    long              *expose_height)
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
 float scaler;
 int X1, X2, Y1, Y2;
 int max_x, max_y;
 long firsthdr,lasthdr;
 Display *dpy;
 Screen *screen;
 float *xarray, LabelValue;
 char label[8];
 long numt;
 float xmin, xmax, xfactor;
 long data_offset;
 float add_to_x = 0.0;
 long width;
 float firstx, lastx;

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
      scaler           = (float)_amp_recovery->getScaleAmp(_cpixm);
      start_index      = _cpixm * getTracesInMemory() + first_column - 1;
      end_index        = _cpixm * getTracesInMemory() + last_column  - 1;
      numt             = abs((int)end_index - (int)start_index) + 1;
      firsthdr         = (long)(start_index*_nhdrs);
      lasthdr          = (long)(end_index  *_nhdrs);
      gpiy             = _user->getGlobals().srval;
      zminm            = 0.0;
      zmaxm            = _tmin;
      timefactor       = (_tmax - zmaxm) / (_ximage.height-1);
      data_offset      = start_index * getSamplesInMemory();
      if(_zoomed) 
        data_offset   += getSampleIndexFromTime(_tmin) +
                                          start_index * getSamplesInMemory();
        
      xarray = (float *) malloc((int)(numt * sizeof(float)));
      if(xarray == NULL) return(ResourceFail);
      j = _user->getMatchXheader() - 1;
      for(i = 0; i < numt; i++)
         {
         xarray[i] = _hd[j+firsthdr];
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

      firstx = (long)((xmin - _grid_x1)/_x_value_per_pixel+.5);

      lastx  = (long)((xmax - _grid_x1)/_x_value_per_pixel+.5);

      width = (int)(lastx - firstx + 1);

      xfactor  = (xmax - xmin) / (width - 1);
     
      max_x = (int)(
              min(_left_border + width + firstx, _ximage.width + _left_border));
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
      
     

      //If we are only contouring this image, fill the background with
      //black to erase the old contours
      if(_user->plottingContoursOnly())
        {
        if(_over_image != NULL)
          XSetForeground( dpy, _gc2, _white_pixel);
        else
          //The following was changed to prevent va's contour only option
          //from doing the repair background in black (the original image
          //has a white background). Need to make sure this does not break
          //Geopress. MLS 06/2002
          //XSetForeground( dpy, _gc2, BlackPixelOfScreen(screen));
          XSetForeground( dpy, _gc2, getImageBackgroundPixel());

        XFillRectangle(dpy, _pixmary[pixmap_index], _gc2, 
                       (int)(firstx + _left_border),
                       _top_border, width, _ximage.height);

        }


      if(_over_image != NULL)//use a bolder line to show up better
        {
        XSetLineAttributes( dpy, _gc2, 4, LineSolid, CapButt, JoinMiter);    
        }
      else
        {
        XSetLineAttributes( dpy, _gc2, 2, LineSolid, CapButt, JoinMiter);
        }


      //Draw the contours
      //select a contour value. scan from top to bottom left to right.
      //determine the min and max values on the grid square
      // z1 .. z4
      // .      .
      // .      . 
      // z2 .. z3

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
                     X1 = (int) (((xs[m] - _grid_x1) / xfactor
                        + _left_border + add_to_x)); 
                     X2 = (int) (((xs[n] - _grid_x1) / xfactor
                        + _left_border + add_to_x));
                     Y1 = (int) (((ys[m] - _tmin) / timefactor
                        + _top_border + 0.5));
                     Y2 = (int) (((ys[n] - _tmin) / timefactor
                        + _top_border + 0.5));
                     if(X1 <= max_x && 
                        X2 <= max_x &&
                        Y1 <= max_y && 
                        Y2 <= max_y   )
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


  if(_over_image == NULL)//no seismic overlay image
    {
    annotatePlot(firsthdr);
    refresh(firstx+_left_border,_top_border,
            width, _ximage.height);
    *expose_x     = (long)(firstx+_left_border);
    *expose_y     = _top_border;
    *expose_width = width;
    *expose_height= _ximage.height;
    }
  else//have seismic over this image
    {
    _over_image->refreshMain(firstx+_left_border+_dest_x,_dest_y,
                             width, _ximage.height,
                             _dest_x+firstx+_left_border,
                             _dest_y+_dest_y, False, 
                             _over_image->graphicWidget());
    *expose_x     = (long)(firstx+_left_border+_dest_x);
    *expose_y     = _dest_y;
    *expose_width = width;
    *expose_height= _ximage.height;
    }


    return(PlotSuccess);


}



