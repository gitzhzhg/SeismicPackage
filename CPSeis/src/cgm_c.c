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
! Name       : cgm_c
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

char cgm_c_ident[100] = 
"$Id: cgm_c.c,v 1.1 2008/02/15 20:58:09 mengewm Exp $";


/*------------------------- start of functions ----------------------------*/
/*------------------------- start of functions ----------------------------*/
/*------------------------- start of functions ----------------------------*/

#include "cgm.h"

void cgm_c_pip_trace(float *x0, float *y0, int *grp, float *fac, float tr[],
                     int *n_tr, int va_cols[], int *n_va_cols, int bg_cols[],
                     int *n_bg_cols)
{
  cgmPipTrace(*x0,*y0,*grp,*fac,tr,*n_tr,va_cols,*n_va_cols,bg_cols,*n_bg_cols);
}


void cgm_c_pip_trace_orientation(float *bx,float *by,float *ax,float *ay)
{
  cgmPipTraceOrientation(*bx,*by,*ax,*ay);
}


void cgm_c_pip_trace_scale_factors(float *base,float *ampl)
{
  cgmPipTraceScaleFactors(*base,*ampl);
}


void cgm_c_pip_trace_display_modes(int modes[],int *n)
{
  cgmPipTraceDisplayModes(modes,*n);
}


void cgm_c_pip_trace_va_fill(float *xpmin, float *xpmax, float *xmmin, 
                             float *xnmax, int *istyle, int *ipcol, int *imcol,
                             int *ippat, int *impat, int *vc_align)
{
  cgmPipTraceVaFill(*xpmin,*xpmax,*xmmin,*xnmax,*istyle,*ipcol,*imcol,
                    *ippat,*impat,*vc_align);
}


void cgm_c_pip_va_fill_pos_bound(float *xpmin, float *xpmax)
{
  cgmPipVaFillPosBound(*xpmin,*xpmax);
}


void cgm_c_pip_va_fill_neg_bound(float *xmmin, float *xnmax)
{
  cgmPipVaFillNegBound(*xmmin,*xnmax);
}


void cgm_c_pip_va_fill_style(int *istyle)
{
  cgmPipVaFillStyle(*istyle);
}


void cgm_c_pip_va_fill_const_color(int *ipcol, int *imcol)
{
  cgmPipVaFillConstColor(*ipcol,*imcol);
}


void cgm_c_pip_va_fill_const_pattern(int *ippat, int *impat)
{
  cgmPipVaFillConstPattern(*ippat,*impat);
}


void cgm_c_pip_va_fill_alignment(int *vc_align)
{
  cgmPipVaFillAlignment(*vc_align);
}


void cgm_c_pip_trace_bg_fill(float *pos_fill, float *neg_fill, int *imode,
                             int *fill_style, int *null_col, int *const_col,
                             int *bg_align)
{
  cgmPipTraceBgFill(*pos_fill,*neg_fill,*imode,*fill_style,*null_col,
                    *const_col,*bg_align);
}


void cgm_c_pip_bg_fill_bound(float *pos_fill, float *neg_fill)
{
  cgmPipBgFillBound(*pos_fill,*neg_fill);
}


void cgm_c_pip_bg_fill_interpolation(int *imode)
{
  cgmPipBgFillInterpolation(*imode);
}


void cgm_c_pip_bg_fill_style(int *fill_style)
{
  cgmPipBgFillStyle(*fill_style);
}


void cgm_c_pip_bg_fill_null_color(int *null_col)
{
  cgmPipBgFillNullColor(*null_col);
}


void cgm_c_pip_bg_fill_const_color(int *const_col)
{
  cgmPipBgFillConstColor(*const_col);
}


void cgm_c_pip_bg_fill_alignment(int *bg_align)
{
  cgmPipBgFillAlignment(*bg_align);
}


void cgm_c_pip_begin_trace_group(int *grp, int *pos_pri, int *neg_pri,
                                 int *pos_mode, int *neg_mode)
{
  cgmPipBeginTraceGroup(*grp,*pos_pri,*neg_pri,*pos_mode,*neg_mode);
}


void cgm_c_pip_end_trace_group()
{
  cgmPipEndTraceGroup();
}


void cgm_c_gopks(int *unit)
{
  cgmGopks(*unit);
}


void cgm_c_gopwk(int *wk, char *file, char *type)
{
  cgmGopwk(*wk,file,type);
}


void cgm_c_gacwk(int *wk)
{
  cgmGacwk(*wk);
}


void cgm_c_gswkvp(int *wk, float *xmin, float *xmax, float *ymin, float *ymax)
{
  cgmGswkvp(*wk,*xmin,*xmax,*ymin,*ymax);
}


void cgm_c_gswn(int *trans, float *xmin, float *xmax, float *ymin, float *ymax)
{
  cgmGswn(*trans,*xmin,*xmax,*ymin,*ymax);
}


void cgm_c_gsvp(int *trans, float *xmin, float *xmax, float *ymin, float *ymax)
{
  cgmGsvp(*trans,*xmin,*xmax,*ymin,*ymax);
}


void cgm_c_gselnt(int *trans)
{
  cgmGselnt(*trans);
}


void cgm_c_gsclip(int *clip)
{
  cgmGsclip(*clip);
}


void cgm_c_gscell(float *x1, float *y1, float *x2, float *y2, int *nx, int *ny,
                  char *array, int *dummy)
{
  cgmGscell(*x1,*y1,*x2,*y2,*nx,*ny,array,*dummy);
}


void cgm_c_gscr(int *wk, int *color, float *red, float *green, float *blue)
{
  cgmGscr(*wk,*color,*red,*green,*blue);
}


void cgm_c_gsln(int *type)
{
  cgmGsln(*type);
}


void cgm_c_gslwsc(float *width)
{
  cgmGslwsc(*width);
}


void cgm_c_gsplci(int *color)
{
  cgmGsplci(*color);
}


void cgm_c_gpl(int *n, float x[], float y[])
{
  cgmGpl(*n,x,y);
}


void cgm_c_gsfais(int *type)
{
  cgmGsfais(*type);
}


void cgm_c_gsfaci(int *color)
{
  cgmGsfaci(*color);
}


void cgm_c_gfa(int *n, float x[], float y[])
{
  cgmGfa(*n,x,y);
}


void cgm_c_set_marker_font(int *set)
{
  cgmSetMarkerFont(*set);
}


void cgm_c_gsmk(int *type)
{
  cgmGsmk(*type);
}


void cgm_c_set_marker_size(float *size)
{
  cgmSetMarkerSize(*size);
}


void cgm_c_set_marker_angle(float *angle)
{
  cgmSetMarkerAngle(*angle);
}


void cgm_c_gspmci(int *color)
{
  cgmGspmci(*color);
}


void cgm_c_gpm(int *n, float x[], float y[])
{
  cgmGpm(*n,x,y);
}


void cgm_c_gschh(float *height)
{
  cgmGschh(*height);
}


void cgm_c_gschxp(float *xp)
{
  cgmGschxp(*xp);
}


void cgm_c_gschsp(float *sp)
{
  cgmGschsp(*sp);
}


void cgm_c_gstxal(int *h, int *v)
{
  cgmGstxal(*h,*v);
}


void cgm_c_gstxp(int *path)
{
  cgmGstxp(*path);
}


void cgm_c_gstxfp(int *font, int *type)
{
  cgmGstxfp(*font,*type);
}


void cgm_c_gstxci(int *color)
{
  cgmGstxci(*color);
}


void cgm_c_gschup(float *x, float *y)
{
  cgmGschup(*x,*y);
}


void cgm_c_gtx(float *x, float *y, char *text)
{
  cgmGtx(*x,*y,text);
}


void cgm_c_gtxr(float *dx, float *dy, float *x, float *y, char *text)
{
  cgmGtxr(*dx,*dy,*x,*y,text);
}


void cgm_c_gdawk(int *wk)
{
  cgmGdawk(*wk);
}


void cgm_c_gclwk(int *wk)
{
  cgmGclwk(*wk);
}


void cgm_c_gclks()
{
  cgmGclks();
}


void cgm_c_insert_char (int *c, int *index, char *array)
{
  array[*index-1] = (char) (*c);
}
