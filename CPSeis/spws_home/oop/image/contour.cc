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
/*******************************************************************************
c*                 confidential and proprietary information                    *
c*                              of conoco inc.                                 *
c*                      protected by the copyright law                         *
c*                          as an unpublished work                             *
c*******************************************************************************
c-----------------------------------------------------------------------        
c                    interactive conoco processing system
c                 exploration research & services division                      
c                              conoco, inc.                                     
c                                                                               
c  process name: contour
c        author: c version of Richard Day's cntrc by Micheal L. Sherrill
c                (C++ version 4/97)
c  last revised:
c
c  purpose:      make a contour plot of data contained in a two dimensional
c                array. 
c-----------------------------------------------------------------------        
c
c                           input parameters
c                                                                               
c  name       type      inout    description                                    
c  ----       -------   -----    -----------
c-----------------------------------------------------------------------        
c                                 notes                                         
c
c
c-----------------------------------------------------------------------        
cend doc                                                                       
cprog doc                                                                      
c-----------------------------------------------------------------------        
c                           revision history                                    
c                                                                               
c date      author       description                                            
c ----      ------       -----------                                            
C
c-----------------------------------------------------------------------        
c                                 notes                                         
c                                                                               
c 1.                                                                            
c-----------------------------------------------------------------------        
c 
c+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
cend doc                                                                    
ccc*/


#include <math.h>
#include "plot_image.hh"
#include "trace_selector.hh"
#include "tfdefs.h"
#include "image_amplitude_recovery.hh"
#include "sl/paintset_collection.hh"




//=============================================================================
//========== This method draws contours over a semblance data type plot    ====
//========== or contours alone based on the semblance data type plot       ====
//=============================================================================



int PlotImage::contour(long AryOffset, long HdrOffset)


{
 static float scr[110];
 float xs[10], ys[10];
 float spcnvl[20];
 long  indivl;
 long  start_index,end_index;
 float gpiy, zminm, zmaxm;
 long i, j, k, l, m, n, s;
 float val;
 long nval, iflg, iflg1, is;
 float sum, smin, smax, conint, conval;
 long jpt, i1;
 float z1, z2, z3, z4, di, dj;
 float *floatvals;
 float dx, dy;
 float timefactor; 
 float scaler;
 int X1, X2, Y1, Y2;
 long firsthdr,lasthdr;
 Display *dpy;
 Screen *screen;
 float *velocity, LabelValue;
 char label[8];
 long intval, numt;
 long trace_index;
 long offset;
 unsigned char *scaled_bytes;
 long data_offset;

      dpy = XtDisplay(_graphic);
      screen = XtScreen(_graphic);
      floatvals = _float_array;

      
      if(_zoomed)
         data_offset = _cpixm * (getTracesInMemory() * getSamplesInMemory());
       else
         data_offset = AryOffset;

      
      scaler = (float)_amp_recovery->getScaleAmp(_cpixm);

      //Byte data so convert it to floats
      if(isFloatData() == False)
        {
        trace_index = _cpixm * getTracesInMemory();
        offset = data_offset;
        for(i=0;i<getTracesInMemory();i++)
          {
          _amp_recovery->scaleDataForDisplay( 0, trace_index + i,
                                              0,getSamplesInMemory()-1); 
          scaled_bytes = _amp_recovery->getScaledByteDataArray(0);
          for(j=0;j<getSamplesInMemory();j++)
            {
            floatvals[j+offset] =
               _amp_recovery->convertScaledByteDataToFloat(trace_index + i,
                                                           scaled_bytes[j]);
            }
          offset += getSamplesInMemory();
          }
        }


      start_index      = (long)(_zoomxary[_zindex][0] - 1);
      end_index        = (long)(_zoomxary[_zindex][1] - 1);
      firsthdr         = (long)((_zoomxary[_zindex][0] - 1)* _nhdrs + 5);
      if(!usingSelector())
       {
       lasthdr  = (long)((_zoomxary[_zindex][1] - 1)* _nhdrs + 5);
       }
     else
       {
       long h1 = (long)((_trace_selector->getNumTraces(getCurrentPanel()) - 1)
                 * _nhdrs + 5);
       long h2 = (long)((_zoomxary[_zindex][1] - 1)* _nhdrs + 5);
       lasthdr  = min(h1, h2);
       }

      _vel_min   = _hd[firsthdr+HdrOffset];
      _vel_max   = _hd[lasthdr+HdrOffset];
      gpiy             = _user->getGlobals().srval;
      zminm            = 0.0;
      zmaxm            = _tmin;
      timefactor       = (_ximage.height-1) / (_tmax - zmaxm);
      _velfactor = (_ximage.width - 1)
                       / (_vel_max - _vel_min);

      velocity = (float *) malloc((int)(getTracesInMemory() * sizeof(float)));
      if(velocity == NULL) return(ResourceFail);
      j = 5; /*velocity header index*/
      for(i=0;i<getTracesInMemory();i++)
         {
         velocity[i] = _hd[j+HdrOffset];
         j += _nhdrs;
         }
      numt = getTracesInMemory();

      _user->setNumberOfContours(min(_user->getNumberOfContours(),20));
      _user->setNumberOfContours(max(1,_user->getNumberOfContours()));
      indivl= _user->getNumberOfContours();
 
 
      for(m=0;m<indivl;m++) spcnvl[m]=0.0;
                 
 
/***************************************************
  find probability distribution function for samples   
  scr[i]=# of grid points with semblance value=i-1%
  of the maximum semblance value. semblance is divided 
  into 100 levels.
***************************************************/
      //if zoomed need to compute the next as if it were the
      //original image so contour levels will be the same
       if(_zoomed)
         data_offset = _cpixm * (getTracesInMemory() * getSamplesInMemory());
       else
         data_offset = AryOffset;
       for(m=0;m<102;m++) scr[m]=0.0;
       if(scaler <= 0.0) scaler = 1.0;
       l=0;
       for(i=0;i<getTracesInMemory();i++)
          {
          for(j=0;j<getSamplesInMemory();j++)
             {
             val  = floatvals[l+data_offset]/scaler*100.0;
             nval = (long)(val + 1.0);
             scr[nval] = scr[nval] + 1.0;
             l++;
             }
          }

 
/***************************************************
  find indices for percentile boundary
  iflg = contour level where cummulative population=pd1
  iflg1= contour level where cummulative population=pd2
***************************************************/
      sum   = 0.0;
      iflg  = 0;
      iflg1 = 0;
      if (_user->getContourMinimumVelocity() < 0.0) 
          _user->setContourMinimumVelocity(0.0);                
      for(j=0;j<101;j++)
         {
         sum = sum + scr[j]/(getSamplesInMemory()*getTracesInMemory());
         if(sum > _user->getContourMinimumVelocity() && iflg == 0)
           iflg = j + 1;        
         if(sum > _user->getContourMaximumVelocity() && iflg1== 0)
           iflg1= j + 1;           
         }
      if(iflg1 == 0) iflg1 = 100;    
      if(_user->getContourMaximumVelocity() >= 1.0)iflg1 = 100;   
 

/***************************************************
  select contour values for display
  smin =starting semblance value
  smax =ending semblance value, indivl= # of contour levels
  spcnvl(i)= semblance value for contour i
****************************************************/
      smin = scaler*iflg*0.01;
      smax = scaler*iflg1*0.01;
      conint=0.0;            
      if(indivl > 1) conint =  (smax-smin)/(indivl-1);
      for(j=0;j<indivl;j++) spcnvl[j] = smin + (conint*j);     
      dx = dy = 1.0;
  

/************************************************
   select a contour value. scan from top to bottom left to right.
   determine the min and max values on the grid square
************************************************/
      for(k=0;k<indivl;k++)
         {
         conval = spcnvl[k];
         intval = (long)((conval/scaler)*100.0);
         LabelValue = intval/100.0;
         sprintf(label,"%4f",LabelValue);
         Y1 = (int)((k + 1) * _fixedcharheight + (int)_top_border);
         X1 = (int)((_left_border + _ximage.width) - 5 * _fixedcharwidth);
         X1 = X1 < 0 ? 1 : X1;
         XSetForeground( dpy, _gc1, _col_two->pix[k] );
         XDrawString(dpy,_pixmary[_cpixm],_gc1, X1,Y1,label,4);
         for(i=start_index,j=0; i<end_index; i++, j++)
            {
            jpt = j * getSamplesInMemory();     
            for(s=0;s<_nsamp-1;s++)
               {
               i1 = jpt + s;
               z1 = floatvals[i1+AryOffset];
               z2 = floatvals[i1+1+AryOffset];
               z3 = floatvals[i1+1+getSamplesInMemory()+AryOffset];
               z4 = floatvals[i1+getSamplesInMemory()+AryOffset];
               di = i;
               dj = s;
               is = 0;
               contourLevel(&di,&dj,&conval,&z1,&z2,&z3,&z4,
                            &dx, &dy, xs, ys, velocity, &conint,
                            &zminm, &zmaxm, &gpiy, &is, &numt);
               if(is >= 2)      
                  {
                  for(m=1;m<is;m+=2) 
                     {
                     n  = m + 1;
                     X1 = (int)((xs[m] - _vel_min) * _velfactor
                        + _left_border); 
                     X2 = (int)((xs[n] - _vel_min) * _velfactor
                        + _left_border);
                     Y1 = (int)((ys[m] - _tmin) * timefactor + _top_border);
                     Y2 = (int)((ys[n] - _tmin) * timefactor + _top_border);
                     if(Y1<_ximage.height+(int)_top_border && 
                        Y2<_ximage.height+(int)_top_border)
                           XDrawLine(dpy,_pixmary[_cpixm],
                                         _gc1,X1,Y1,X2,Y2);
                     }
                  }        
               }
            }         
         is = 0;
         }

      XSetForeground( dpy, _gc1, PaintsetCollection::black(screen));
      if(velocity != NULL){free(velocity);velocity=NULL;}
      return(PlotSuccess);
}



