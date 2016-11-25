/****
!<CPS_v1 type=HEADER_FILE"/>
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
!                           C P S  H E A D E R  F I L E
!
! Name       : cgm.h
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
!                       HEADER FILE REVISION HISTORY
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


#ifndef _CGM_H_
#define _CGM_H_


#ifdef __cplusplus
extern "C" {
#endif

#include "c2f_interface.h"

#ifdef NEED_UNDERSCORE
#define cgm_c_pip_trace                  cgm_c_pip_trace_
#define cgm_c_pip_trace_orientation      cgm_c_pip_trace_orientation_
#define cgm_c_pip_trace_scale_factors    cgm_c_pip_trace_scale_factors_
#define cgm_c_pip_trace_display_modes    cgm_c_pip_trace_display_modes_
#define cgm_c_pip_trace_va_fill          cgm_c_pip_trace_va_fill_
#define cgm_c_pip_va_fill_pos_bound      cgm_c_pip_va_fill_pos_bound_
#define cgm_c_pip_va_fill_neg_bound      cgm_c_pip_va_fill_neg_bound_
#define cgm_c_pip_va_fill_style          cgm_c_pip_va_fill_style_
#define cgm_c_pip_va_fill_const_color    cgm_c_pip_va_fill_const_color_
#define cgm_c_pip_va_fill_const_pattern  cgm_c_pip_va_fill_const_pattern_
#define cgm_c_pip_va_fill_alignment      cgm_c_pip_va_fill_alignment_
#define cgm_c_pip_trace_bg_fill          cgm_c_pip_trace_bg_fill_
#define cgm_c_pip_bg_fill_bound          cgm_c_pip_bg_fill_bound_
#define cgm_c_pip_bg_fill_interpolation  cgm_c_pip_bg_fill_interpolation_
#define cgm_c_pip_bg_fill_style          cgm_c_pip_bg_fill_style_
#define cgm_c_pip_bg_fill_null_color     cgm_c_pip_bg_fill_null_color_
#define cgm_c_pip_bg_fill_const_color    cgm_c_pip_bg_fill_const_color_
#define cgm_c_pip_bg_fill_alignment      cgm_c_pip_bg_fill_alignment_
#define cgm_c_pip_begin_trace_group      cgm_c_pip_begin_trace_group_
#define cgm_c_pip_end_trace_group        cgm_c_pip_end_trace_group_
#define cgm_c_gopks                      cgm_c_gopks_
#define cgm_c_gopwk                      cgm_c_gopwk_
#define cgm_c_gacwk                      cgm_c_gacwk_
#define cgm_c_gswkvp                     cgm_c_gswkvp_
#define cgm_c_gswn                       cgm_c_gswn_
#define cgm_c_gsvp                       cgm_c_gsvp_
#define cgm_c_gselnt                     cgm_c_gselnt_
#define cgm_c_gsclip                     cgm_c_gsclip_
#define cgm_c_gscell                     cgm_c_gscell_
#define cgm_c_gscr                       cgm_c_gscr_
#define cgm_c_gsln                       cgm_c_gsln_
#define cgm_c_gslwsc                     cgm_c_gslwsc_
#define cgm_c_gsplci                     cgm_c_gsplci_
#define cgm_c_gpl                        cgm_c_gpl_
#define cgm_c_gsfais                     cgm_c_gsfais_
#define cgm_c_gsfaci                     cgm_c_gsfaci_
#define cgm_c_gfa                        cgm_c_gfa_
#define cgm_c_set_marker_font            cgm_c_set_marker_font_
#define cgm_c_gsmk                       cgm_c_gsmk_
#define cgm_c_set_marker_size            cgm_c_set_marker_size_
#define cgm_c_set_marker_angle           cgm_c_set_marker_angle_
#define cgm_c_gspmci                     cgm_c_gspmci_
#define cgm_c_gpm                        cgm_c_gpm_
#define cgm_c_gschh                      cgm_c_gschh_
#define cgm_c_gschxp                     cgm_c_gschxp_
#define cgm_c_gschsp                     cgm_c_gschsp_
#define cgm_c_gstxal                     cgm_c_gstxal_
#define cgm_c_gstxp                      cgm_c_gstxp_
#define cgm_c_gstxfp                     cgm_c_gstxfp_
#define cgm_c_gstxci                     cgm_c_gstxci_
#define cgm_c_gschup                     cgm_c_gschup_
#define cgm_c_gtx                        cgm_c_gtx_
#define cgm_c_gtxr                       cgm_c_gtxr_
#define cgm_c_gdawk                      cgm_c_gdawk_
#define cgm_c_gclwk                      cgm_c_gclwk_
#define cgm_c_gclks                      cgm_c_gclks_
#define cgm_c_insert_char                cgm_c_insert_char_
#endif

#ifdef NEED_CAPITALS
#define cgm_c_pip_trace                  CGM_C_PIP_TRACE
#define cgm_c_pip_trace_orientation      CGM_C_PIP_TRACE_ORIENTATION
#define cgm_c_pip_trace_scale_factors    CGM_C_PIP_TRACE_SCALE_FACTORS
#define cgm_c_pip_trace_display_modes    CGM_C_PIP_TRACE_DISPLAY_MODES
#define cgm_c_pip_trace_va_fill          CGM_C_PIP_TRACE_VA_FILL
#define cgm_c_pip_va_fill_pos_bound      CGM_C_PIP_VA_FILL_POS_BOUND
#define cgm_c_pip_va_fill_neg_bound      CGM_C_PIP_VA_FILL_NEG_BOUND
#define cgm_c_pip_va_fill_style          CGM_C_PIP_VA_FILL_STYLE
#define cgm_c_pip_va_fill_const_color    CGM_C_PIP_VA_FILL_CONST_COLOR
#define cgm_c_pip_va_fill_const_pattern  CGM_C_PIP_VA_FILL_CONST_PATTERN
#define cgm_c_pip_va_fill_alignment      CGM_C_PIP_VA_FILL_ALIGNMENT
#define cgm_c_pip_trace_bg_fill          CGM_C_PIP_TRACE_BG_FILL
#define cgm_c_pip_bg_fill_bound          CGM_C_PIP_BG_FILL_BOUND
#define cgm_c_pip_bg_fill_interpolation  CGM_C_PIP_BG_FILL_INTERPOLATION
#define cgm_c_pip_bg_fill_style          CGM_C_PIP_BG_FILL_STYLE
#define cgm_c_pip_bg_fill_null_color     CGM_C_PIP_BG_FILL_NULL_COLOR
#define cgm_c_pip_bg_fill_const_color    CGM_C_PIP_BG_FILL_CONST_COLOR
#define cgm_c_pip_bg_fill_alignment      CGM_C_PIP_BG_FILL_ALIGNMENT
#define cgm_c_pip_begin_trace_group      CGM_C_PIP_BEGIN_TRACE_GROUP
#define cgm_c_pip_end_trace_group        CGM_C_PIP_END_TRACE_GROUP
#define cgm_c_gopks                      CGM_C_GOPKS
#define cgm_c_gopwk                      CGM_C_GOPWK
#define cgm_c_gacwk                      CGM_C_GACWK
#define cgm_c_gswkvp                     CGM_C_GSWKVP
#define cgm_c_gswn                       CGM_C_GSWN
#define cgm_c_gsvp                       CGM_C_GSVP
#define cgm_c_gselnt                     CGM_C_GSELNT
#define cgm_c_gsclip                     CGM_C_GSCLIP
#define cgm_c_gscell                     CGM_C_GSCELL
#define cgm_c_gscr                       CGM_C_GSCR
#define cgm_c_gsln                       CGM_C_GSLN
#define cgm_c_gslwsc                     CGM_C_GSLWSC
#define cgm_c_gsplci                     CGM_C_GSPLCI
#define cgm_c_gpl                        CGM_C_GPL
#define cgm_c_gsfais                     CGM_C_GSFAIS
#define cgm_c_gsfaci                     CGM_C_GSFACI
#define cgm_c_gfa                        CGM_C_GFA
#define cgm_c_set_marker_font            CGM_C_SET_MARKER_FONT
#define cgm_c_gsmk                       CGM_C_GSMK
#define cgm_c_set_marker_size            CGM_C_SET_MARKER_SIZE
#define cgm_c_set_marker_angle           CGM_C_SET_MARKER_ANGLE
#define cgm_c_gspmci                     CGM_C_GSPMCI
#define cgm_c_gpm                        CGM_C_GPM
#define cgm_c_gschh                      CGM_C_GSCHH
#define cgm_c_gschxp                     CGM_C_GSCHXP
#define cgm_c_gschsp                     CGM_C_GSCHSP
#define cgm_c_gstxal                     CGM_C_GSTXAL
#define cgm_c_gstxp                      CGM_C_GSTXP
#define cgm_c_gstxfp                     CGM_C_GSTXFP
#define cgm_c_gstxci                     CGM_C_GSTXCI
#define cgm_c_gschup                     CGM_C_GSCHUP
#define cgm_c_gtx                        CGM_C_GTX
#define cgm_c_gtxr                       CGM_C_GTXR
#define cgm_c_gdawk                      CGM_C_GDAWK
#define cgm_c_gclwk                      CGM_C_GCLWK
#define cgm_c_gclks                      CGM_C_GCLKS
#define cgm_c_insert_char                CGM_C_INSERT_CHAR
#endif


/*------------------------ start of prototypes ---------------------------*/
/*------------------------ start of prototypes ---------------------------*/
/*------------------------ start of prototypes ---------------------------*/


void cgm_c_pip_trace(float *x0, float *y0, int *grp, float *fac, float tr[], int *n_tr, int va_cols[], int *n_va_cols, int bg_cols[], int *n_bg_cols);
void cgm_c_pip_trace_orientation(float *bx,float *by,float *ax,float *ay);
void cgm_c_pip_trace_scale_factors(float *base,float *ampl);
void cgm_c_pip_trace_display_modes(int modes[],int *n);
void cgm_c_pip_trace_va_fill(float *xpmin, float *xpmax, float *xmmin, float *xnmax, int *istyle, int *ipcol, int *imcol, int *ippat, int *impat, int *vc_align);
void cgm_c_pip_va_fill_pos_bound(float *xpmin, float *xpmax);
void cgm_c_pip_va_fill_neg_bound(float *xmmin, float *xnmax);
void cgm_c_pip_va_fill_style(int *istyle);
void cgm_c_pip_va_fill_const_color(int *ipcol, int *imcol);
void cgm_c_pip_va_fill_const_pattern(int *ippat, int *impat);
void cgm_c_pip_va_fill_alignment(int *vc_align);
void cgm_c_pip_trace_bg_fill(float *pos_fill, float *neg_fill, int *imode, int *fill_style, int *null_col, int *const_col, int *bg_align);
void cgm_c_pip_bg_fill_bound(float *pos_fill, float *neg_fill);
void cgm_c_pip_bg_fill_interpolation(int *imode);
void cgm_c_pip_bg_fill_style(int *fill_style);
void cgm_c_pip_bg_fill_null_color(int *null_col);
void cgm_c_pip_bg_fill_const_color(int *const_col);
void cgm_c_pip_bg_fill_alignment(int *bg_align);
void cgm_c_pip_begin_trace_group(int *grp, int *pos_pri, int *neg_pri, int *pos_mode, int *neg_mode);
void cgm_c_pip_end_trace_group();
void cgm_c_gopks(int *unit);
void cgm_c_gopwk(int *wk, char *file, char *type);
void cgm_c_gacwk(int *wk);
void cgm_c_gswkvp(int *wk, float *xmin, float *xmax, float *ymin, float *ymax);
void cgm_c_gswn(int *trans, float *xmin, float *xmax, float *ymin, float *ymax);
void cgm_c_gsvp(int *trans, float *xmin, float *xmax, float *ymin, float *ymax);
void cgm_c_gselnt(int *trans);
void cgm_c_gsclip(int *clip);
void cgm_c_gscell(float *x1, float *y1, float *x2, float *y2, int *nx, int *ny, char *array, int *dummy);
void cgm_c_gscr(int *wk, int *color, float *red, float *green, float *blue);
void cgm_c_gsln(int *type);
void cgm_c_gslwsc(float *width);
void cgm_c_gsplci(int *color);
void cgm_c_gpl(int *n, float x[], float y[]);
void cgm_c_gsfais(int *type);
void cgm_c_gsfaci(int *color);
void cgm_c_gfa(int *n, float x[], float y[]);
void cgm_c_set_marker_font(int *set);
void cgm_c_gsmk(int *type);
void cgm_c_set_marker_size(float *size);
void cgm_c_set_marker_angle(float *angle);
void cgm_c_gspmci(int *color);
void cgm_c_gpm(int *n, float x[], float y[]);
void cgm_c_gschh(float *height);
void cgm_c_gschxp(float *xp);
void cgm_c_gschsp(float *sp);
void cgm_c_gstxal(int *h, int *v);
void cgm_c_gstxp(int *path);
void cgm_c_gstxfp(int *font, int *type);
void cgm_c_gstxci(int *color);
void cgm_c_gschup(float *x, float *y);
void cgm_c_gtx(float *x, float *y, char *text);
void cgm_c_gtxr(float *dx, float *dy, float *x, float *y, char *text);
void cgm_c_gdawk(int *wk);
void cgm_c_gclwk(int *wk);
void cgm_c_gclks();
void cgm_c_insert_char (int *c, int *index, char *array);



void cgmPipTrace(float x0, float y0, int grp, float fac, float tr[], int n_tr, int va_cols[], int n_va_cols, int bg_cols[], int n_bg_cols);
void cgmPipTraceOrientation(float bx,float by,float ax,float ay);
void cgmPipTraceScaleFactors(float base,float ampl);
void cgmPipTraceDisplayModes(int modes[],int n);
void cgmPipTraceVaFill(float xpmin, float xpmax, float xmmin, float xnmax, int istyle, int ipcol, int imcol, int ippat, int impat, int vc_align);
void cgmPipVaFillPosBound(float xpmin, float xpmax);
void cgmPipVaFillNegBound(float xmmin, float xnmax);
void cgmPipVaFillStyle(int istyle);
void cgmPipVaFillConstColor(int ipcol, int imcol);
void cgmPipVaFillConstPattern(int ippat, int impat);
void cgmPipVaFillAlignment(int vc_align);
void cgmPipTraceBgFill(float pos_fill, float neg_fill, int imode, int fill_style, int null_col, int const_col, int bg_align);
void cgmPipBgFillBound(float pos_fill, float neg_fill);
void cgmPipBgFillInterpolation(int imode);
void cgmPipBgFillStyle(int fill_style);
void cgmPipBgFillNullColor(int null_col);
void cgmPipBgFillConstColor(int const_col);
void cgmPipBgFillAlignment(int bg_align);
void cgmPipBeginTraceGroup(int grp, int pos_pri, int neg_pri, int pos_mode, int neg_mode);
void cgmPipEndTraceGroup();
void cgmGopks(int unit);
void cgmGopwk(int wk, char *file, char *type);
void cgmGacwk(int wk);
void cgmGswkvp(int wk, float xmin, float xmax, float ymin, float ymax);
void cgmGswn(int trans, float xmin, float xmax, float ymin, float ymax);
void cgmGsvp(int trans, float xmin, float xmax, float ymin, float ymax);
void cgmGselnt(int trans);
void cgmGsclip(int clip);
void cgmGscell(float x1, float y1, float x2, float y2, int nx, int ny, char *array, int dummy);
void cgmGscr(int wk, int color, float red, float green, float blue);
void cgmGsln(int type);
void cgmGslwsc(float width);
void cgmGsplci(int color);
void cgmGpl(int n, float x[], float y[]);
void cgmGsfais(int type);
void cgmGsfaci(int color);
void cgmGfa(int n, float x[], float y[]);
void cgmSetMarkerFont(int set);
void cgmGsmk(int type);
void cgmSetMarkerSize(float size);
void cgmSetMarkerAngle(float angle);
void cgmGspmci(int color);
void cgmGpm(int n, float x[], float y[]);
void cgmGschh(float height);
void cgmGschxp(float xp);
void cgmGschsp(float sp);
void cgmGstxal(int h, int v);
void cgmGstxp(int path);
void cgmGstxfp(int font, int type);
void cgmGstxci(int color);
void cgmGschup(float x, float y);
void cgmGtx(float x, float y, char *text);
void cgmGtxr(float dx, float dy, float x, float y, char *text);
void cgmGdawk(int wk);
void cgmGclwk(int wk);
void cgmGclks();


#ifdef __cplusplus
}
#endif


#endif

