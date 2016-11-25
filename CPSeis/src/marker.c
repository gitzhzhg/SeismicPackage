/****
!<CPS_v1 type=AUXILIARY_FILE"/>
!<license>
!-------------------------------------------------------------------------------
! Copyright (c) 2007 ConocoPhillips Company
!
! Permission is hereby granted, free of charge, to any person obtaining a copy
! of this software and associated documentation files (the "Software"), to deal
! in the Software without restriction, including without limitation the rights
! to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
! copies of the Software, and to permit persons to whom the Software is
! furnished to do so, subject to the following conditions:
!
! The above copyright notice and this permission notice shall be included in all
! copies or substantial portions of the Software.
!
! THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
! IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
! FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
! AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
! LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
! OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
! SOFTWARE.
!-------------------------------------------------------------------------------
!</license>


!<brief_doc>
!-------------------------------------------------------------------------------
!                        C P S  A U X I L I A R Y  F I L E
!
! Name       : marker
! Category   : stand-alone
! Written    : 2001-04-16   by: Donna K. Vunderink
! Revised    : 2002-04-01   by: Donna K. Vunderink
! Maturity   : production
! Purpose    : CGM library.
! Portability: No known limitations.
!
!-------------------------------------------------------------------------------
!</brief_doc>


!<history_doc>
!-------------------------------------------------------------------------------
!                     AUXILIARY FILE REVISION HISTORY
!
!     Date        Author       Description
!     ----        ------       -----------
!  3. 2002-04-01  Vunderink    Fix marker placement and sizing
!  2. 2001-09-24  Vunderink    Make sure circle or arc radius is positive
!  1. 2001-04-16  Vunderink    Initial version.
!
!
!
!-------------------------------------------------------------------------------
!</history_doc>
****/

char marker_ident[100] = 
"$Id: marker.c,v 1.1 2008/02/15 20:58:09 mengewm Exp $";


#include "cgm_common.h"

void cgmGsmk(int i)
{
  if (cgmdebug) fprintf(err_unit,"cgm_gsmk: %d \n",i);
  if (i >= 0 && i <= 135 && markerfont == 1)
      markertype = i;
  else if (i >= 0 && i <= 158 && markerfont == 2)
      markertype = i;
  else if (i >= 1 && i <= 5) {
      markertype = i;
      cgmPutIndex(markertype);
      cgmPutData(5,6);
  }
  else {
      markertype = 3;
      cgmPutIndex(markertype);
      cgmPutData(5,6);
      fprintf(err_unit,"Invalid cgm_gsmk value: %d \n",i);
  }
}

void cgmGsmksc(float sc)
{
  float temp, tempx, tempy;

  if (cgmdebug) fprintf(err_unit,"cgm_gsmksc: %f \n",sc);
  markerscale = sc;
  if (sc == 0.0) markerscale = 1.0;

  if (marker_scale_mode == 0) {
     temp = markerscale*scalefactor;
     if (vdc_type == 0 && temp < 1.0) temp = 1.0;
     cgmPutVdc(temp);
     cgmPutData(5,7);
  }
  else {
     cgmPutReal(markerscale);
     cgmPutData(5,7);
  }
  cgmGsdcwc(0.0,0.0,&tempx,&tempy);
  cgmGsdcwc(0.0,markerscale*0.125,&temp,&markersize);
  markersize = ABS(markersize - tempy);
}

void cgmSetMarkerSize(float h)
{
  float tmp1, tmp2, tmp, xrange, yrange, mdef, temp;

  if (cgmdebug) fprintf(err_unit,"cgm_set_congraf_marker_size: %f \n",h);
  if (marker_scale_mode == 0) {
     cgmGsdcwc(0.0,0.0,&tmp1,&tmp2);
     cgmGswcdc(tmp1+h,tmp2+h,&tmp,&markerscale);
     temp = ABS(markerscale*scalefactor);
     if (vdc_type == 0 && temp < 1.0) temp = 1.0;
     cgmPutVdc(temp);
     cgmPutData(5,7);
     markerscale = ABS(h);
  }
  else {
     xrange = ABS(xd_max_c - xd_min_c);
     yrange = ABS(yd_max_c - yd_min_c);
     if (xrange > yrange)
        mdef = xrange / 100.0;
     else
        mdef = yrange / 100.0;

     markerscale = ABS(h / mdef);
     temp = ABS(markerscale*scalefactor);
     if (vdc_type == 0 && temp < 1.0) temp = 1.0;
     cgmPutReal(temp);
     cgmPutData(5,7);
  }
  markersize = temp;

}

void cgmSetMarkerAngle(float angle)
{
  if (cgmdebug) fprintf(err_unit,"cgm_set_marker_angle: %f \n",angle);
  markerangle = angle;
}

void cgmSetMarkerFont(int mfont)
{
  if (cgmdebug) fprintf(err_unit,"cgm_set_congraf_marker_font: %d \n",mfont);
  markerfont = mfont;
}

void cgmGpm(int n, float x[], float y[])
{
  int i;
  float atemp,xtemp,ytemp,dx,dy;
  float markersize_tmp,tmpx1,tmpx2,tmpy1,tmpy2,xyasr;

  if (cgmdebug) {
     fprintf(err_unit,"cgm_gpm: %d ",n);
     for (i=0;i<n;i++) {
       fprintf(err_unit,"         %f %f \n",x[i],y[i]);
     }
  }
  if (markerfont != 0) cgmLoadMarkers(markerfont);
  if (markerfont != 0 || markertype < 1 || markertype > 5) {
     if (fillcolor != markercolor) cgmGsfaci(markercolor);
     if (linetype != 1) {
        cgmPutInt(1);
        cgmPutData(5,2);
    }
    if (linecolor != markercolor) cgmGsplci(markercolor);
    if (linewidth != 1) cgmGslwsc(1.0);
    cgmGsdcwc(0.0,0.0,&tmpx1,&tmpy1);
    cgmGsdcwc(markersize,markersize,&tmpx2,&tmpy2);
    markersize_tmp = ABS(tmpy2 - tmpy1);
    cgmGswcdc(0.0,0.0,&tmpx1,&tmpy1);
    cgmGswcdc(markersize_tmp,markersize_tmp,&tmpx2,&tmpy2);
    if (tmpy1 != tmpy2 && tmpx1 != tmpx2) {
      xyasr = ABS((tmpy2-tmpy1)/(tmpx2-tmpx1));
    }
    else {
      xyasr = 1.0;
    }
    for (i=0;i<n;i++) {
      xtemp = x[i];
      ytemp = y[i];
      atemp=(markerangle-360.*(markerangle/360.))*.017453294;
      dx = cos(atemp);
      dy = sin(atemp);
      cgmStrokeMarkers(xtemp,ytemp,dx,dy,markertype,xyasr,markersize_tmp);
    }
    if (fillcolor != markercolor) cgmGsfaci(fillcolor);
    if (linetype > 1) {
       cgmPutInt(linetype);
       cgmPutData(5,2);
    }
    if (linecolor != markercolor) cgmGsplci(linecolor);
    if (linewidth > 1) cgmGslwsc(linewidth);
  }
  else {
    for (i=0;i<n;i++) {
      cgmPutPoint(x[i],y[i]);
    }
    cgmPutData(4,3);
  }
}

static int iflag = {-1};
static short begs[257];
static char c[3000];
static float xs[3000],ys[3000];
static long npoints;

void cgmLoadMarkers(int mfont)
{
  int istat, nfile; 
  char file2[256], file[256];

  if( iflag != -1) return;

  if (mfont == 2) {
    nfile = 255;
    strcpy(file2,"cgm_geobase.fnt");
    istat = cgmFindFile(file2,file,&nfile);
    if (istat != 0) {
      markerfont = 0;
      iflag = 0;
      return;
    }
    printf("Loading marker file %s \n",file);
    npoints = 3000;
    istat = cgmFontRead(file,begs,c,xs,ys,&npoints);
    if (istat != 0) {
      markerfont = 0;
      iflag = 0;
      return;
    }
  }
  else {
    nfile = 255;
    strcpy(file2,"cgm_markers.fnt");
    istat = cgmFindFile(file2,file,&nfile);
    if (istat != 0) {
      markerfont = 0;
      iflag = 0;
      return;
    }
    printf("Loading marker file %s \n",file);
    npoints = 2000;
    istat = cgmFontRead(file,begs,c,xs,ys,&npoints);
    if (istat != 0) {
      markerfont = 0;
      iflag = 0;
      return;
    }
  }
  iflag = 0;
}

void cgmStrokeMarkers(float x,float y,float dx,float dy, int jchar,
                      float xyasr,float markersize_tmp)
{
  int ic,j,icold,np;
  float csize,x1,y1,x2,y2,dx1,dx2,dy1,dy2,r,xc,yc;
  float x3,y3;
  float siz[256],x1s[4096],y1s[4096];

  ic = jchar;
  j = begs[ic] - 1;
  if (j <= 0) return;
  csize = siz[ic];

  icold = 0;
  np = 0;
  ic = 1;
  while (ic != 0) {
    ic = c[j];
    x1 = xs[j] - 0.5;
    y1 = ys[j] - 0.5;
    x2 = x + (dx * x1 - dy * y1) * xyasr * markersize_tmp;
    y2 = y + (dy * x1 + dx * y1) * markersize_tmp;
    if (ic == 16) {
      j = j + 1;
      r = ABS(xs[j] * markersize_tmp * xyasr);
      if (filltype != 0) cgmGsfais(0);
      cgmGgdpCircle(x2,y2,r);
      if (filltype != 0) cgmGsfais(filltype);
    }
    else if (ic == 17) {
      j = j + 1;
      r = ABS(xs[j] * markersize_tmp * xyasr);
      if (filltype != 1) cgmGsfais(1);
      cgmGgdpCircle(x2,y2,r);
      if (filltype != 1) cgmGsfais(filltype);
    }
    else if(ic == 18) {
      xc = xs[j];
      yc = ys[j];
      j = j + 1;
      x3 = xs[j] - xc;
      y3 = ys[j] - yc;
      dx1 = dx * x3 - dy * y3;
      dy1 = dy * x3 + dx * y3;
      j = j + 1;
      x3 = xs[j] - xc;
      y3 = ys[j] - yc;
      dx2 = dx * x3 - dy * y3;
      dy2 = dy * x3 + dx * y3;
      j = j + 1;
      r = ABS(xs[j] * markersize_tmp * xyasr);
      if (filltype != 0) cgmGsfais(0);
      cgmGgdpArc(x2,y2,dx1,dy1,dx2,dy2,r,1);
      if (filltype != 0) cgmGsfais(filltype);
    }
    else if (ic == 19) {
      xc = xs[j];
      yc = ys[j];
      j = j + 1;
      x3 = xs[j] - xc;
      y3 = ys[j] - yc;
      dx1 = dx * x3 - dy * y3;
      dy1 = dy * x3 + dx * y3;
      j = j + 1;
      x3 = xs[j] - xc;
      y3 = ys[j] - yc;
      dx2 = dx * x3 - dy * y3;
      dy2 = dy * x3 + dx * y3;
      j = j + 1;
      r = ABS(xs[j] * markersize_tmp * xyasr);
      if (filltype != 1) cgmGsfais(1);
      cgmGgdpArc(x2,y2,dx1,dy1,dx2,dy2,r,1);
      if (filltype != 1) cgmGsfais(filltype);
    }
    else if(ic == 20) {
      xc = xs[j];
      yc = ys[j];
      j = j + 1;
      x3 = xs[j] - xc;
      y3 = ys[j] - yc;
      dx1 = dx * x3 - dy * y3;
      dy1 = dy * x3 + dx * y3;
      j = j + 1;
      x3 = xs[j] - xc;
      y3 = ys[j] - yc;
      dx2 = dx * x3 - dy * y3;
      dy2 = dy * x3 + dx * y3;
      j = j + 1;
      r = ABS(xs[j] * markersize_tmp * xyasr);
      if (filltype != 0) cgmGsfais(0);
      cgmGgdpArc(x2,y2,dx1,dy1,dx2,dy2,r,0);
      if (filltype != 0) cgmGsfais(filltype);
    }
    else if(ic == 21) {
      xc = xs[j];
      yc = ys[j];
      j = j + 1;
      x3 = xs[j] - xc;
      y3 = ys[j] - yc;
      dx1 = dx * x3 - dy * y3;
      dy1 = dy * x3 + dx * y3;
      j = j + 1;
      x3 = xs[j] - xc;
      y3 = ys[j] - yc;
      dx2 = dx * x3 - dy * y3;
      dy2 = dy * x3 + dx * y3;
      j = j + 1;
      r = ABS(xs[j] * markersize_tmp * xyasr);
      if (filltype != 1) cgmGsfais(1);
      cgmGgdpArc(x2,y2,dx1,dy1,dx2,dy2,r,0);
      if (filltype != 1) cgmGsfais(filltype);
    }
    else if (ic == 22) {
      xc = xs[j];
      yc = ys[j];
      j = j + 1;
      x3 = xs[j] - xc;
      y3 = ys[j] - yc;
      dx1 = dx * x3 - dy * y3;
      dy1 = dy * x3 + dx * y3;
      j = j + 1;
      x3 = xs[j] - xc;
      y3 = ys[j] - yc;
      dx2 = dx * x3 - dy * y3;
      dy2 = dy * x3 + dx * y3;
      j = j + 1;
      r = ABS(xs[j] * markersize_tmp * xyasr);
      cgmGgdpArc(x2,y2,dx1,dy1,dx2,dy2,r,2);
    }
    else {
      if (ic != 2) {
        if (np > 0) {
          if (icold == 1) {
            if (np == 1) np=0;
            cgmGpl(np,x1s,y1s);
          }
          else if (icold == 3)
            if (np > 0) cgmGfa(np,x1s,y1s);
        }
        icold = ic;
        np = 0;
      }
      np = np + 1;
      x1s[np-1] = x2;
      y1s[np-1] = y2;
    }
   j = j + 1;
  }

  if (np > 0) {
    if (icold == 1) {
      if (np == 1) np=0;
      cgmGpl(np,x1s,y1s);
    }
    else if (icold == 3)
      if(np>0) cgmGfa(np,x1s,y1s);
  }
}
