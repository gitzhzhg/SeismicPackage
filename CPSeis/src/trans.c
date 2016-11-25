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
! Name       : trans
! Category   : stand-alone
! Written    : 2001-04-16   by: Donna K. Vunderink
! Revised    : 2001-04-16   by: Donna K. Vunderink
! Maturity   : beta
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
!  1. 2001-04-16  Vunderink    Initial version.
!
!
!
!-------------------------------------------------------------------------------
!</history_doc>
****/

char trans_ident[100] = 
"$Id: trans.c,v 1.1 2008/02/15 20:58:09 mengewm Exp $";


#include "cgm_common.h"

static int extent_init;
static double ax2_cos,ay2_cos,ax2_sin,ay2_sin;
static double ax1,ay1,bx1,by1,ax2,ay2,bx2,by2;
static float x_org,y_org;

void cgmTransInit()
{
  int i;

  current_wk = 1;
  current_trans = 1;
  trans_init = 0;
  extent_init = 0;
  x_org = 0.0;
  y_org = 0.0;

/* Initialize device coordinates */
  for (i=0;i<MAX_WK;i++) {
      xd_max[i] = 8.5;
      xd_min[i] = 0.0;
      yd_max[i] = 11.0;
      yd_min[i] = 0.0;
  }

/* Initialize world coordinates */
  for (i=0;i<MAX_WINDOWS;i++) {
      xw_max[i] = 8.5;
      xw_min[i] = 0.0;
      yw_max[i] = 11.0;
      yw_min[i] = 0.0;
  }

/* Initialize normalized coordinates */
  for (i=0;i<MAX_VIEWS;i++) {
      xn_max[i] = 1.0;
      xn_min[i] = 0.0;
      yn_max[i] = 1.0;
      yn_min[i] = 0.0;
  }

/* Setup the current device, world, and normalized corrdinates */
  xd_min_c = xd_min[current_wk];
  xd_max_c = xd_max[current_wk];
  yd_min_c = yd_min[current_wk];
  yd_max_c = yd_max[current_wk];

  xw_min_c = xw_min[current_trans];
  xw_max_c = xw_max[current_trans];
  yw_min_c = yw_min[current_trans];
  yw_max_c = yw_max[current_trans];

  xn_min_c = xn_min[current_trans];
  xn_max_c = xn_max[current_trans];
  yn_min_c = yn_min[current_trans];
  yn_max_c = yn_max[current_trans];

}

void cgmUorg(float x_uorg,float y_uorg)
{

  x_org = x_org + x_uorg;
  y_org = y_org + y_uorg;
  bx2 = bx2 + ax2_cos*x_uorg - ay2_sin*y_uorg;
  by2 = by2 + ax2_sin*x_uorg + ay2_cos*y_uorg;
  if (clip_on == 1) cgmGsclip(clip_on);
}

void cgmGswkvpScale (float scale)
{
      scalefactor = ABS(scale);
      if (scale < 0.0) vdc_type = 1;
}

/* Routine to set the output cgm file size coordinates */
void cgmGswkvp(int iwk,float xmin,float xmax,float ymin,float ymax)
{
  if (cgmdebug) fprintf(err_unit,"cgm_gswkvp: %d %f %f %f %f \n",iwk,xmin,xmax,
                        ymin,ymax);
  if (trans_init != 0) {
     fprintf(err_unit,"GSWKVP - You must initialize the workstation device \
coordinates before activating the current transformation \n");
  }
  else {
    xd_min[current_wk] = xmin;
    xd_max[current_wk] = xmax;
    yd_min[current_wk] = ymin;
    yd_max[current_wk] = ymax;
    xd_min_c = xd_min[current_wk];
    xd_max_c = xd_max[current_wk];
    yd_min_c = yd_min[current_wk];
    yd_max_c = yd_max[current_wk];
    ax1 = (xd_max_c - xd_min_c) / (1.0 - 0.0);
    ay1 = (yd_max_c - yd_min_c) / (1.0 - 0.0);
    bx1 = xd_min_c - ax1*0.0;
    by1 = yd_min_c - ay1*0.0;

/* VDC Extents */
    cgmPutVdc(xmin*scalefactor);
    cgmPutVdc(ymin*scalefactor);
    cgmPutVdc(xmax*scalefactor);
    cgmPutVdc(ymax*scalefactor);
    cgmPutData(2,6);

/* Begin Picture Body - No Parameters */
    cgmPutData(0,4);
    cgmInitAttributes();
    extent_init = 1;
  }    

  if (xd_max_c == xd_min_c) fprintf(err_unit,
                             "Screen X-Coordinate dimension has zero width \n");
}

void cgmGsvp(int itn,float xmin,float xmax,float ymin,float ymax)
{
  if (cgmdebug) fprintf(err_unit,"cgm_gsvp: %d %f %f %f %f \n",itn,xmin,xmax,
                        ymin,ymax);

  xn_min[itn] = xmin;
  xn_max[itn] = xmax;
  yn_min[itn] = ymin;
  yn_max[itn] = ymax;
}

void cgmGswn(int itn,float xmin,float xmax,float ymin,float ymax)
{
  if (cgmdebug) fprintf(err_unit,"cgm_gswn: %d %f %f %f %f \n",itn,xmin,xmax,
                        ymin,ymax);

  xw_min[itn] = xmin;
  xw_max[itn] = xmax;
  yw_min[itn] = ymin;
  yw_max[itn] = ymax;
  w_ang[itn]  = 0.0;
}

void cgmGselnt(int itn)
{
  float xw_tmp, yw_tmp, xn_tmp, yn_tmp;

  if (cgmdebug) fprintf(err_unit,"cgm_gselnt: %d \n",itn);

  trans_init = 1;

  if (extent_init == 0) {               /* VDC Extents */
     cgmPutVdc(0.0);
     cgmPutVdc(0.0);
     cgmPutVdc(32767.0);
     cgmPutVdc(32767.0);
     cgmPutData(2,6);
     extent_init = 1;
  }

  current_trans = itn;
  if (xw_max[current_trans] == xw_min[current_trans])
        fprintf(err_unit,"Window X-Coordinate dimension has zero width \n");

/* Setup the current world and normalized limits */
  xw_min_c = xw_min[current_trans];
  xw_max_c = xw_max[current_trans];
  yw_min_c = yw_min[current_trans];
  yw_max_c = yw_max[current_trans];
  w_ang_c =  w_ang[current_trans];

  xn_min_c = xn_min[current_trans];
  xn_max_c = xn_max[current_trans];
  yn_min_c = yn_min[current_trans];
  yn_max_c = yn_max[current_trans];
  ax2 = (xn_max_c - xn_min_c) / (xw_max_c - xw_min_c);
  ay2 = (yn_max_c - yn_min_c) / (yw_max_c - yw_min_c);
  if (w_ang_c != 0) {
     xw_tmp = (xw_min_c + xw_max_c) * 0.5;
     yw_tmp = (yw_min_c + yw_max_c) * 0.5;
     xn_tmp = (xn_min_c + xn_max_c) * 0.5;
     yn_tmp = (yn_min_c + yn_max_c) * 0.5;
     ax2_cos = ax2*cos(w_ang_c);
     ax2_sin = ax2*sin(w_ang_c)*ABS(ax1/ay1);
     ay2_cos = ay2*cos(w_ang_c);
     ay2_sin = ay2*sin(w_ang_c)*ABS(ay1/ax1);
     bx2 = xn_tmp - (ax2_cos*xw_tmp - ay2_sin*yw_tmp);
     by2 = yn_tmp - (ax2_sin*xw_tmp + ay2_cos*yw_tmp);
  }
  else {
     ax2_cos = ax2;
     ax2_sin = 0.0;
     ay2_cos = ay2;
     ay2_sin = 0.0;
     bx2 = xn_min_c - ax2*xw_min_c;
     by2 = yn_min_c - ay2*yw_min_c;
  }

  if (clip_on == 1) cgmGsclip(clip_on);
}

void cgmGswcdc(float xw, float yw, float *xd, float *yd)
{
  float xw_tmp, yw_tmp, xn_tmp, yn_tmp, xd_tmp, yd_tmp;

  xw_tmp = xw;
  yw_tmp = yw;
  xn_tmp = ax2_cos*xw_tmp - ay2_sin*yw_tmp + bx2;
  yn_tmp = ax2_sin*xw_tmp + ay2_cos*yw_tmp + by2;
  xd_tmp = ax1*xn_tmp + bx1;
  yd_tmp = ay1*yn_tmp + by1;
  *xd    = xd_tmp;
  *yd    = yd_tmp;

  if (cgmdebug) fprintf(err_unit,"cgm_gswcdc: %f %f %f %f \n",xw,yw,*xd,*yd);
}

void cgmGsncdc(float xn, float yn, float *xd, float *yd)
{
  float xn_tmp, yn_tmp, xd_tmp, yd_tmp;

  xn_tmp = xn;
  yn_tmp = yn;
  xd_tmp = ax1*xn_tmp + bx1;
  yd_tmp = ay1*yn_tmp + by1;
  *xd    = xd_tmp;
  *yd    = yd_tmp;
  if (cgmdebug) fprintf(err_unit,"cgm_gsncdc: %f %f %f %f \n",xn,yn,*xd,*yd);
}

void cgmGsdcwc(float xd, float yd, float *xw, float *yw)
{
 float xw_tmp, yw_tmp, xn_tmp, yn_tmp, xd_tmp, yd_tmp;

  xd_tmp = xd;
  yd_tmp = yd;
  xn_tmp = (xd_tmp-bx1) / ax1;
  yn_tmp = (yd_tmp-by1) / ay1;
  if (ax2_sin == 0.0 && ay2_sin == 0.0) {
     xw_tmp = (xn_tmp-bx2) / ax2;
     yw_tmp = (yn_tmp-by2) / ay2;
  }
  else {
     xw_tmp = ay2_cos*(xn_tmp-bx2) + ax2_sin*(yn_tmp-by2);
     xw_tmp = xw_tmp / (ax2_cos*ay2_cos + ax2_sin*ay2_sin);
     yw_tmp = ax2_cos*(yn_tmp-by2) - ax2_sin*(xn_tmp-bx2);
     yw_tmp = yw_tmp / (ax2_cos*ay2_cos + ax2_sin*ay2_sin);
  }
  *xw     = xw_tmp;
  *yw     = yw_tmp;

  if (cgmdebug) fprintf(err_unit,"cgm_gsdcwc: %f %f %f %f \n",xd,yd,*xw,*yw);
}

void cgmGsncwc(float xn, float yn, float *xw, float *yw)
{
 float xw_tmp, yw_tmp, xn_tmp, yn_tmp;

  xn_tmp = xn;
  yn_tmp = yn;
  if (ax2_sin == 0.0 && ay2_sin == 0.0) {
     xw_tmp = (xn_tmp-bx2) / ax2;
     yw_tmp = (yn_tmp-by2) / ay2;
  }
  else {
     xw_tmp = ay2_cos*(xn_tmp-bx2) + ay2_sin*(yn_tmp-by2);
     xw_tmp = xw_tmp / (ax2_cos*ay2_cos + ax2_sin*ay2_sin);
     yw_tmp = ax2_cos*(yn_tmp-by2) - ay2_sin*(xn_tmp-bx2);
     yw_tmp = yw_tmp / (ax2_cos*ay2_cos + ax2_sin*ay2_sin);
  }
  *xw     = xw_tmp;
  *yw     = yw_tmp;

  if (cgmdebug) fprintf(err_unit,"cgm_gsncwc: %f %f %f %f \n",xn,yn,*xw,*yw);
}

void cgmGswcnc(float xw, float yw, float *xn, float *yn)
{
  float xw_tmp, yw_tmp, xn_tmp, yn_tmp;

  xw_tmp = xw;
  yw_tmp = yw;
  xn_tmp = ax2_cos*xw_tmp - ay2_sin*yw_tmp + bx2;
  yn_tmp = ax2_sin*xw_tmp + ay2_cos*yw_tmp + by2;
  *xn    = xn_tmp;
  *yn    = yn_tmp;
  if (cgmdebug) fprintf(err_unit,"cgm_gswcnc: %f %f %f %f \n",xw,yw,*xn,*yn);
}

void cgmGsdcnc(float xd, float yd, float *xn, float *yn)
{
  float xn_tmp, yn_tmp, xd_tmp, yd_tmp;

  xd_tmp = xd;
  yd_tmp = yd;
  xn_tmp = (xd_tmp - bx1) / ax1;
  yn_tmp = (yd_tmp - by1) / ay1;
  *xn    = xn_tmp;
  *yn    = yn_tmp;
  if (cgmdebug) fprintf(err_unit,"cgm_gsdcnc: %f %f %f %f \n",xd,yd,*xn,*yn);
}

void cgmGqxywc(float *x, float *y)
{
  *x = current_x;
  *y = current_y;
}

void cgmGqxync(float *x, float *y)
{
  cgmGswcnc (current_x,current_y,x,y);
}

void cgmGqxydc(float *x, float *y)
{
  cgmGswcdc (current_x,current_y,x,y);
}

void cgmGsclip(int clip)
{
  if (cgmdebug) fprintf(err_unit,"cgm_gsclip: %d \n",clip);
  if (clip == 1 && trans_init == 1) {
     cgmGswcdc(xw_min_c,yw_min_c,&clipx1,&clipy1);
     cgmGswcdc(xw_max_c,yw_max_c,&clipx2,&clipy2);
     cgmPutVdc(clipx1*scalefactor);
     cgmPutVdc(clipy1*scalefactor);
     cgmPutVdc(clipx2*scalefactor);
     cgmPutVdc(clipy2*scalefactor);
     cgmPutData(3,5);
     cgmPutEnum(clip);
     cgmPutData(3,6);
  }
  else if (clip == 0) {
     cgmPutEnum(clip);
     cgmPutData(3,6);
  }
   clip_on = clip;
}
