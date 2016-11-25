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
! Name       : pip_trace
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

char pip_trace_ident[100] = 
"$Id: pip_trace.c,v 1.1 2008/02/15 20:58:09 mengewm Exp $";


#include "cgm_common.h"

void cgmPipTrace(float x0, float y0, int grp, float s_ext, float tr[],
                   int n_tr, int va_cols[], int n_va_cols, int bg_cols[],
                   int n_bg_cols)
{
  int i,n;

  if (cgmdebug) fprintf(err_unit,"cgm_pip_trace: %f %f %d %f %d %d %d \n",
                        x0,y0,grp,s_ext,n_tr,n_va_cols,n_bg_cols);

  n = 14 + 4 + 4*n_tr + 4 + 2*n_va_cols + 4 + 2*n_bg_cols;
  if (n >= 32752) {
    fprintf(err_unit,"cgm_pip_trace: Error too many samples");
    return;
  }
  cgmPutInt(-3000);          /* trace */
  cgmPutInt(1);
  cgmPutPoint(x0,y0);
  if (n <= 254)
    cgmPutByte(n);
  else if (n < 32752) {       /* n + 15 < 32767 */
    cgmPutByte(255);
    cgmPutInt(n);
  }

  cgmPutInt(6);             /* trace group */
  cgmPutInt(1);
  cgmPutInt(grp);

  cgmPutInt(12);            /* sample normalization extent */
  cgmPutInt(1);
  cgmPutReal(s_ext);

  cgmPutInt(12);            /* sample values */
  cgmPutInt(n_tr);
  if (n_tr > 0) {
    for (i=0;i<n_tr;i++) {
      cgmPutReal(tr[i]);
    }
  }

  cgmPutInt(2);              /* VA fill colors */
  cgmPutInt(n_va_cols);
  if (n_va_cols > 0) {
    for (i=0;i<n_va_cols;i++) {
      cgmPutInt(va_cols[i]);
    }
  }

  cgmPutInt(2);              /* background fill colors */
  cgmPutInt(n_bg_cols);
  if (n_bg_cols > 0) {
    for (i=0;i<n_bg_cols;i++) {
      cgmPutInt(bg_cols[i]);
    }
  }

  cgmPutData(4,10);
}

void cgmPipTraceOrientation(float base_x,float base_y,float ampl_x,
                               float ampl_y)
{
  if (cgmdebug) fprintf(err_unit,"cgm_pip_trace_orientation: %f %f %f %f \n",
                        base_x,base_y,ampl_x,ampl_y);

  if ((base_x==0.0 && base_y==0.0) || (ampl_x==0.0 && ampl_y==0.0))
       fprintf(err_unit,
              "Bad cgmPipTraceOrientation Parameter -- %f %f %f %f \n",
               base_x,base_y,ampl_x,ampl_y);

  baseline_x = base_x;
  baseline_y = base_y;
  amplitude_x = ampl_x;
  amplitude_y = ampl_y;

  cgmPutInt(-3000);
  cgmPutByte(20);
  cgmPutInt(16);
  cgmPutInt(4);
  cgmPutReal(base_x);
  cgmPutReal(base_y);
  cgmPutReal(ampl_x);
  cgmPutReal(ampl_y);
  cgmPutData(6,1);
}

void cgmPipTraceScaleFactors(float base, float ampl)
{
  float tmp, tmp1, tmp2;

  if (cgmdebug) fprintf(err_unit,"cgm_pip_trace_scale_factors: %f %f \n",
                        base,ampl);

  if (baseline_x == 0.0) {
    cgmGsdcwc(0.0,0.0,&tmp1,&tmp2);
    cgmGswcdc(base+tmp1,base+tmp2,&tmp,&baseline_sf);
  }
  else {
    cgmGsdcwc(0.0,0.0,&tmp1,&tmp2);
    cgmGswcdc(base+tmp1,base+tmp2,&baseline_sf,&tmp);
  }

  if (amplitude_x == 0.0) {
    cgmGsdcwc(0.0,0.0,&tmp1,&tmp2);
    cgmGswcdc(ampl+tmp1,ampl+tmp2,&tmp,&amplitude_sf);
  }
  else {
    cgmGsdcwc(0.0,0.0,&tmp1,&tmp2);
    cgmGswcdc(ampl+tmp1,ampl+tmp2,&amplitude_sf,&tmp);
  }

  tmp1 = ABS(baseline_sf*scalefactor);
  tmp2 = ABS(amplitude_sf*scalefactor);

  if (tmp1==0.0 || tmp2==0.0) 
         fprintf(err_unit,"Bad cgmPipTraceScalefactors Parameter -- %f %f \n",
                 base,ampl);

  cgmPutInt(-3001);
  cgmPutByte(12);
  cgmPutInt(12);
  cgmPutInt(2);
  cgmPutReal(tmp1);           /* baseline scale factor */
  cgmPutReal(tmp2);           /* amplitude scale factor */
  cgmPutData(6,1);
}

void cgmPipTraceDisplayModes(int modes[], int n)
{
  int i,j;

  if (cgmdebug) {
     fprintf(err_unit,"cgm_pip_trace_display_modes: ");
     for (j=0;j<n;j++) {
        fprintf(err_unit,"%d ",modes[j]);
      }
     fprintf(err_unit,"\n");
  }

  for (i=0;i<n;i++) {
    if (modes[i]<1 || modes[i]>4) {
      fprintf(err_unit,"Bad cgmPipTraceDisplayModes Parameter -- ");
      for (j=0;j<n;j++) {
        fprintf(err_unit,"%d ",modes[j]);
      }
      fprintf(err_unit,"\n");
      return;
    }
  }

  cgmPutInt(-3002);
  cgmPutByte(4 + 2*n);
  cgmPutInt(11);
  cgmPutInt(n);
  for (i=0;i<n;i++) {
    cgmPutInt(modes[i]);
  }
  cgmPutData(6,1);
}

void cgmPipTraceMaxSamples(int n)
{
  if (cgmdebug) fprintf(err_unit,"cgm_pip_trace_max_samples: %d \n",n);
  cgmPutInt(-3003);
  cgmPutByte(4);
  cgmPutInt(11);
  cgmPutInt(n);
  cgmPutData(6,1);
}

void cgmPipTraceVaFill(float xpmin, float xpmax, float xmmin,float xmmax,
                       int istyle, int ipcol, int imcol, int ippat,
                       int impat, int vc_align)
{
  if (cgmdebug) fprintf(err_unit,
                      "cgm_pip_trace_va_fill: %f %f %f %f %d %d %d %d %d %d \n",
                       xpmin,xpmax,xmmin,xmmax,istyle,ipcol,imcol,ippat,impat,
                       vc_align);

  cgmPutInt(-3020);           /* minimum and maximum positive fill */
  cgmPutByte(12);
  cgmPutInt(12);
  cgmPutInt(2);
  cgmPutReal(xpmin);
  cgmPutReal(xpmax);
  cgmPutData(6,1);

  cgmPutInt(-3021);           /* minimum and maximum negative fill */
  cgmPutByte(12);
  cgmPutInt(12);
  cgmPutInt(2);
  cgmPutReal(xmmin);
  cgmPutReal(xmmax);
  cgmPutData(6,1);

  if (istyle<0 || istyle>2) {
    fprintf(err_unit,"Bad cgmPipTraceVaFill Parameter -- style = %d \n",istyle);
  }

  cgmPutInt(-3023);           /*VA fill style */
  cgmPutByte(6);
  cgmPutInt(11);
  cgmPutInt(1);
  cgmPutInt(istyle);
  cgmPutData(6,1);

  if (istyle == 0) {
    cgmPutInt(-3024);        /*VA constant color */
    cgmPutByte(8);
    cgmPutInt(2);
    cgmPutInt(2);
    cgmPutInt(ipcol);
    cgmPutInt(imcol);
    cgmPutData(6,1);
  }

  if (istyle == 1) {
    if (vc_align<0 || vc_align>1) {
      fprintf(err_unit,"Bad cgmPipTraceVaFill Parameter -- vc_align = %d \n",
              vc_align);
    }
    cgmPutInt(-3026);        /*VA variant color alignment */
    cgmPutByte(6);
    cgmPutInt(11);
    cgmPutInt(1);
    cgmPutInt(vc_align);
    cgmPutData(6,1);
  }

  if (istyle == 2) {
    cgmPutInt(-3025);        /*VA constant pattern */
    cgmPutByte(8);
    cgmPutInt(2);
    cgmPutInt(2);
    cgmPutInt(ippat);
    cgmPutInt(impat);
    cgmPutData(6,1);
  }
}

void cgmPipVaFillPosBound(float xpmin, float xpmax)
{
  if (cgmdebug) fprintf(err_unit,"cgm_pip_va_fill_pos_boundaries: %f %f \n",
                        xpmin,xpmax);

  cgmPutInt(-3020);           /* minimum and maximum positive fill */
  cgmPutByte(12);
  cgmPutInt(12);
  cgmPutInt(2);
  cgmPutReal(xpmin);
  cgmPutReal(xpmax);
  cgmPutData(6,1);
}

void cgmPipVaFillNegBound(float xmmin,float xmmax)
{
  if (cgmdebug) fprintf(err_unit,"cgm_pip_va_fill_neg_boundaries: %f %f \n",
                        xmmin,xmmax);

  cgmPutInt(-3021);           /* minimum and maximum negative fill */
  cgmPutByte(12);
  cgmPutInt(12);
  cgmPutInt(2);
  cgmPutReal(xmmin);
  cgmPutReal(xmmax);
  cgmPutData(6,1);
}

void cgmPipVaFillStyle(int istyle)
{
  if (cgmdebug) fprintf(err_unit,"cgm_pip_va_fill_style: %d \n",istyle);

  if (istyle<0 || istyle>2) {
    fprintf(err_unit,"Bad cgmPipTraceVaFill Parameter -- style = %d \n",istyle);
  }

  cgmPutInt(-3023);           /*VA fill style */
  cgmPutByte(6);
  cgmPutInt(11);
  cgmPutInt(1);
  cgmPutInt(istyle);
  cgmPutData(6,1);
}

void cgmPipVaFillConstColor(int ipcol, int imcol)
{
  if (cgmdebug) fprintf(err_unit,"cgm_pip_va_fill_constant_color: %d %d \n",
                        ipcol,imcol);

  cgmPutInt(-3024);        /*VA constant color */
  cgmPutByte(8);
  cgmPutInt(2);
  cgmPutInt(2);
  cgmPutInt(ipcol);
  cgmPutInt(imcol);
  cgmPutData(6,1);
}

void cgmPipVaFillConstPattern(int ippat,int impat)
{
  if (cgmdebug) fprintf(err_unit,"cgm_pip_va_fill_const_pattern: %d %d \n",
                        ippat,impat);

  cgmPutInt(-3025);        /*VA constant pattern  */
  cgmPutByte(8);
  cgmPutInt(2);
  cgmPutInt(2);
  cgmPutInt(ippat);
  cgmPutInt(impat);
  cgmPutData(6,1);
}

void cgmPipVaFillAlignment(int vc_align)
{
  if (cgmdebug) fprintf(err_unit,"cgm_pip_va_fill_alignment: %d \n",vc_align);

  if (vc_align<0 || vc_align>1) {
    fprintf(err_unit,"Bad cgmPipTraceVaFill Parameter -- vc_align = %d \n",
            vc_align);
  }
  cgmPutInt(-3026);        /*VA variant color alignment */
  cgmPutByte(6);
  cgmPutInt(11);
  cgmPutInt(1);
  cgmPutInt(vc_align);
  cgmPutData(6,1);
}

void cgmPipTraceBgFill(float pos_fill, float neg_fill, int imode,
                           int fill_style, int null_col, int const_col,
                           int bg_align)
{
  if (cgmdebug) fprintf(err_unit,
                       "cgm_pip_trace_bg_fill: %f %f %d %d %d %d %d \n",
                        pos_fill,neg_fill,imode,fill_style,null_col,const_col,
                        bg_align);

  cgmPutInt(-3030);           /*Background Fill Boundaries */
  cgmPutByte(12);
  cgmPutInt(12);
  cgmPutInt(2);
  cgmPutReal(pos_fill);
  cgmPutReal(neg_fill);
  cgmPutData(6,1);

  if (imode != 0 && imode != 1) {
    fprintf(err_unit,"Bad cgmPipTraceBgFill Parameter --imode = %d \n",imode);
  }
  cgmPutInt(-3031);           /*Background Fill Interpolation Mode */
  cgmPutByte(6);
  cgmPutInt(11);
  cgmPutInt(1);
  cgmPutInt(imode);
  cgmPutData(6,1);

  if (fill_style!=0 && fill_style!=1) {
    fprintf(err_unit,"Bad cgmPipTraceBgFill Parameter --fill_style = %d \n",
            fill_style);
  }
  cgmPutInt(-3032);           /* Background Fill Style */
  cgmPutByte(6);
  cgmPutInt(11);
  cgmPutInt(1);
  cgmPutInt(fill_style);
  cgmPutData(6,1);

  if (imode == 2) {
    cgmPutInt(-3033);        /* Background Fill Null Color */
    cgmPutByte(6);
    cgmPutInt(11);
    cgmPutInt(1);
    cgmPutInt(null_col);
    cgmPutData(6,1);
  }

  if (fill_style == 0) {
    cgmPutInt(-3034);        /* Background Fill Constant Color */
    cgmPutByte(6);
    cgmPutInt(11);
    cgmPutInt(1);
    cgmPutInt(const_col);
    cgmPutData(6,1);
  }

  if (bg_align != 0 && bg_align != 1) {
    fprintf(err_unit,"Bad cgmPipTraceBgFill Parameter --bg_align = %d \n",
            bg_align);
  }
  cgmPutInt(-3035);           /* Background Fill Alignment */
  cgmPutByte(6);
  cgmPutInt(11);
  cgmPutInt(1);
  cgmPutInt(bg_align);
  cgmPutData(6,1);
}

void cgmPipBgFillBound(float pos_fill, float neg_fill)
{
  if (cgmdebug) fprintf(err_unit,"cgm_pip_bg_fill_boundaries: %f %f \n",
                        pos_fill,neg_fill);

  cgmPutInt(-3030);           /* Background Fill Boundaries */
  cgmPutByte(12);
  cgmPutInt(12);
  cgmPutInt(2);
  cgmPutReal(pos_fill);
  cgmPutReal(neg_fill);
  cgmPutData(6,1);
}

void cgmPipBgFillInterpolation(int imode)
{
  if (cgmdebug) fprintf(err_unit,"cgm_pip_bg_fill_interpolation: %d \n",imode);

  if (imode!=0 && imode!=1) {
    fprintf(err_unit,
           "Bad cgmPipTraceBgFillInterpolation Parameter --imode = %d \n",
            imode);
  }
  cgmPutInt(-3031);           /* Background Fill Interpolation Mode */
  cgmPutByte(6);
  cgmPutInt(11);
  cgmPutInt(1);
  cgmPutInt(imode);
  cgmPutData(6,1);
}

void cgmPipBgFillStyle(int fill_style)
{
  if (cgmdebug) fprintf(err_unit,"cgm_pip_bg_fill_style: %d \n",fill_style);

  if (fill_style!=0 && fill_style!=1) {
    fprintf(err_unit,"Bad cgmPipTraceBgFillStyleParameter --fill_style = %d \n",
            fill_style);
  }
  cgmPutInt(-3032) ;          /* Background Fill Style */
  cgmPutByte(6);
  cgmPutInt(11);
  cgmPutInt(1);
  cgmPutInt(fill_style);
  cgmPutData(6,1);
}

void cgmPipBgFillNullColor(int null_col)
{
  if (cgmdebug) fprintf(err_unit,"cgm_pip_bg_fill_null_color: %d \n",null_col);

  cgmPutInt(-3033);        /* Background Fill Null Color */
  cgmPutByte(6);
  cgmPutInt(11);
  cgmPutInt(1);
  cgmPutInt(null_col);
  cgmPutData(6,1);
}

void cgmPipBgFillConstColor(int const_col)
{
  if (cgmdebug) fprintf(err_unit,"cgm_pip_bg_fill_constant_color: %d \n",
                        const_col);

  cgmPutInt(-3034);        /* Background Fill Constant Color */
  cgmPutByte(6);
  cgmPutInt(11);
  cgmPutInt(1);
  cgmPutInt(const_col);
  cgmPutData(6,1);
}

void cgmPipBgFillAlignment(int bg_align)
{
  if (cgmdebug) fprintf(err_unit,"cgm_pip_bg_fill_alignment: %d \n",bg_align);

  if (bg_align!=0 && bg_align!=1) {
    fprintf(err_unit,
           "Bad cgmPipTraceBgFillAlignment Parameter --bg_align = %d \n",
            bg_align);
  }
  cgmPutInt(-3035);          /* Background Fill Alignment */
  cgmPutByte(6);
  cgmPutInt(11);
  cgmPutInt(1);
  cgmPutInt(bg_align);
  cgmPutData(6,1);
}

void cgmPipBeginTraceGroup(int grp,int pos_pri,int neg_pri, int pos_mode,
                               int neg_mode)
{
  if (cgmdebug) fprintf(err_unit,"cgm_pip_begin_trace_group: %d %d %d %d %d \n",
                        grp,pos_pri,neg_pri,pos_mode,neg_mode);

  cgmPutInt(-3040);
  cgmPutByte(30);
  cgmPutInt(6);
  cgmPutInt(1);
  cgmPutInt(grp);
  cgmPutInt(5);
  cgmPutInt(1);
  cgmPutInt(pos_pri);
  cgmPutInt(5);
  cgmPutInt(1);
  cgmPutInt(neg_pri);
  cgmPutInt(11);
  cgmPutInt(1);
  cgmPutInt(pos_mode);
  cgmPutInt(11);
  cgmPutInt(1);
  cgmPutInt(neg_mode);
  cgmPutData(6,1);
}

void cgmPipEndTraceGroup()
{
  if (cgmdebug) fprintf(err_unit,"cgm_pip_end_trace_group:");
  cgmPutInt(-3041);
  cgmPutByte(0);
  cgmPutData(6,1);
}
