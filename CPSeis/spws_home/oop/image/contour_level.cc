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
c  process name: contourlevel
c        author: c version of Richard Day's program by Micheal L. Sherrill
c  last revised: (C++ version 4/97)
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
*/



#include "plot_image.hh"
#include <math.h>


/*++++++++++++++++++++++++++++function+++++++++++++++++++++++++*/

/*interpolator*/

float vint(float x, float v1, float v2)

{
 long i;
 float vout;

  i = (int)x;
  if (i < 0) i = -i;
  vout = v1+(v2-v1)*(x-i);
  return(vout);
}



/*+++++++++++++++++++++++++++++++++++++++++++++++++++++++*/

/*conval.......contour level
  di,dj........floating point value of jcol
  dx,dy.....grid incrementsto sample. normally (1.0,1.0)
  z1,z2,z3,z4..sample values at corners of cell jcol)
  is...........number of (x,y) points in contour level    output
  xs(i)........user grid coord. for contour points  output
  ys(i)........user grid coord. for contour points  output
*/
           
void PlotImage::contourLevel( float *di, float *dj, float *conval,
                              float *z1, float *z2, float *z3, float *z4,
                              float *dx, float *dy,
                              float *xs, float *ys,
                              float *xarray, float *conint, 
                              float *zminm, float *zmaxm, 
                              float *gpiy, long *is, long *numvels) 
{
 float i, j;
 float esp, zmn, zmx;
 long isd1, isd2, isd3, isd4, iretn;
 float zero;
 float xx1, yy1, xx2, yy2;
 float ax, ay, temp, vout;
 long ix, nilcnt;
  
 
/*first find the min and max value on the cell corners
     z1  4   *z4
       ......
     1 .    . 3
       ......
     *z2  2   *z3
*/
      zero = 0.0;
      esp = *conint * 0.0001;
      i = *di;
      j = *dj + 1.0;
      nilcnt=0;
      zmn=1000.0;
      zmx= (-1000.0);

      if(*z1 != zero) 
        {
        zmn=min(zmn,*z1);
        zmx=max(zmx,*z1);
        }
      else
        {
        nilcnt=nilcnt+1;
        }
             
      if(*z2 != zero)
        {
        zmn=min(zmn,*z2);
        zmx=max(zmx,*z2);
        }
      else
        {
        nilcnt=nilcnt+1;
        }
              
      if(*z3 != zero)
        {
        zmn=min(zmn,*z3);
        zmx=max(zmx,*z3);
        }
      else
        {
        nilcnt=nilcnt+1;
        if(nilcnt == 3) return;
        }
              
      if(*z4 != zero) 
        {
        zmn=min(zmn,*z4);
        zmx=max(zmx,*z4);
        }
      else
        {
        nilcnt=nilcnt+1;
        if(nilcnt >= 3) return;
        }
                  
/************************************************************
      does the selected contour pass through the grid square?
      determine which sides the contour passes through
      isd1=1,0--> does,does not pass thru side 1
      isd2=1,0--> does,does not pass thru side 2
      etc.
************************************************************/
      if ((zmn >= *conval) || (*conval >  zmx)) return;
      if ((*z1 == zero) || (*z2 == zero)) goto label41;
      if (*z1 <  *conval) goto label39;
      if (*z1 == *conval) *z1 = *z1 + esp;
      if (*z2 == *conval) goto label38;
      if (*z2 >  *conval) goto label41;

label37:
      isd1 = 1;
      goto label42;
 
label38: 
      *z2 = *z2 + esp;
      goto label41;
 
label39:
      if (*z2 <  *conval) goto label41;
      if (*z2 >  *conval) goto label37;
      *z2 = *z2 + esp;
      goto label37;
 
label41:
      isd1 = 0;
 
label42:
      if ((*z3 == zero) ||   (*z2 == zero)) goto label47;
      if (*z3 <  *conval) goto label46;
      if (*z3 >  *conval) goto label44;
      *z3 = *z3 + esp;

label44:
      if (*z2 >= *conval) goto label47;

label45:
      isd2 = 1;
      goto label48;
 
label46:
      if (*z2 >  *conval) goto label45;

label47:
      isd2 = 0;

label48:
      if ((*z4 == zero)  ||  (*z3 == zero)) goto label53;
      if (*z4 <  *conval) goto label52;
      if (*z4 == *conval) *z4 = *z4 + esp;
      if (*z3 >= *conval) goto label53;

label51:
      isd3 = 1;
      goto label54;
 
label52:
      if (*z3 >  *conval) goto label51;

label53:
      isd3 = 0;

label54:
      if ((*z1 == zero)  ||  (*z4 == zero)) goto label58;
      if (*z1 <  *conval) goto label57;
      if ((*z1 == *conval)  ||  (*z4 >= *conval)) goto label58;

label56:
      isd4 = 1;
      goto label59;
 
label57:
      if (*z4 >  *conval) goto label56;

label58:
      isd4 = 0;

/**********************************************************************
     calculate where the contour intersects the sides of the grid square
     units are in floating point grid values
     should be bounded by nx and ny
 *********************************************************************/
label59:
      if ((isd1+isd2+isd3+isd4) <= 1) return;
      if (isd1 == 0) goto label63;
      xx1 = i - 1.0;
      yy1 = j - 1.0;
      yy1 = yy1 + (*dy * (*z1- *conval)) / (*z1-*z2);
      if (isd2 == 0) goto label68;
      xx2 = i - 1.0;
      yy2 = j - 1.0 + *dy;
      xx2 = xx2 + (*dx * (*z2- *conval)) / (*z2-*z3);
      iretn = 1;
      goto label90;
 
label60:
      if (isd3 == 0) goto label67;

label62:
      xx1 = i + *dx - 1.0;
      yy1 = j - 1.0;
      yy1 = yy1 + (*dy * (*z4- *conval)) / (*z4-*z3);
      xx2 = i - 1.0;
      yy2 = j - 1.0;
      xx2 = xx2 + (*dx * (*z1- *conval)) / (*z1-*z4);
      iretn = 2;
      goto label90;
 
label63:
      if (isd2 == 0) goto label62;
      xx1 = i - 1.0;
      yy1 = j - 1.0 + *dy;
      xx1 = xx1 + (*dx * (*z2- *conval)) / (*z2-*z3);

label68:
      if (isd3 == 0) goto label66;
      xx2 = i - 1.0 + *dx;
      yy2 = j - 1.0;
      yy2 = yy2 + (*dy * (*z4- *conval)) / (*z4-*z3);
      iretn = 2;
      goto label90;
 
label66:
      xx2 = i - 1.0;
      yy2 = j - 1.0;
      xx2 = xx2 + (*dx * (*z1- *conval)) / (*z1-*z4); 
      iretn = 2;
      goto label90;
 
label67:       
      return;
  
/**************************************************************
  convert from grid to user coordinates and store in xs and ys
**************************************************************/
label90:
      *is = *is + 1;
      if (xx1 <= xx2)      
        {
        ax  = xx1+1.0;
        ay  = yy1+ 1.0;
        ix  = (int)ax;
        if(ix > *numvels - 1)ix = *numvels - 1;
        if(ix + 1 < *numvels)
          temp = xarray[ix+1]- *zminm;
        else
          temp = xarray[ix]- *zminm;
        vout = vint(ax, xarray[ix], temp);
        xs[*is]= vout;
        ys[*is]= yy1 * (*gpiy) + *zmaxm;
        *is = *is + 1;
        ax  = xx2+1.0;
        ay  = yy2+1.0;
        ix  = (int)ax;
        if(ix > *numvels - 1)ix = *numvels - 1;
        if(ix + 1 < *numvels)
          temp = xarray[ix+1]- *zminm;
        else
          temp = xarray[ix]- *zminm;
        vout = vint(ax, xarray[ix], temp);
        xs[*is]= vout;
        ys[*is]= yy2 * (*gpiy) + *zmaxm;
        }
      else
        {
        ax  = xx2+1.0;
        ay  = yy2+ 1.0;
        ix  = (int)ax;
        if(ix > *numvels - 1)ix = *numvels - 1;
        if(ix + 1 < *numvels)
          temp = xarray[ix+1]- *zminm;
        else
          temp = xarray[ix]- *zminm;
        vout = vint(ax, xarray[ix], temp);
        xs[*is]= vout;
        ys[*is]= yy2 * (*gpiy) + *zmaxm;
        *is = *is + 1;
        ax  = xx1+1.0;
        ay  = yy1+1.0;
        ix  = (int)ax;
        if(ix + 1 < *numvels)
          temp = xarray[ix+1]- *zminm;
        else
          temp = xarray[ix]- *zminm;
        vout = vint(ax, xarray[ix], temp);
        xs[*is]= vout;
        ys[*is]= yy1 * (*gpiy) + *zmaxm;
        }
  
      if (iretn == 1) goto label60;
      return;
}    
