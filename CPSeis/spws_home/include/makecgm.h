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
#ifndef _MAKECGM_H_

#define _MAKECGM_H_

#ifdef __cplusplus
extern "C" {                          /* for C++ */
#endif

void cgm_c_pip_init(void);
void cgm_c_pip_trace(float x0, float y0, int grp, float fac, float tr[], int n_tr, int va_cols[], int n_va_cols, int bg_cols[], int n_bg_cols);
void cgm_c_pip_trace_orientation(float bx, float by, float ax, float ay);
void cgm_c_pip_trace_scale_factors(float base, float amp);
void cgm_c_pip_trace_display_modes(int modes[], int n);
void cgm_c_pip_trace_va_fill(float xpmin, float xpmax, float xmmin, float xnmax, int istyle, int ipcol, int imcol, int ippat, int impat, int vc_align);
void cgm_c_pip_va_fill_pos_boundaries(float xpmin, float xpmax);
void cgm_c_pip_va_fill_neg_boundaries(float xmmin, float xmmax);
void cgm_c_pip_va_fill_style(int istyle);
void cgm_c_pip_va_fill_constant_color(int ipcol, int imcol);
void cgm_c_pip_va_fill_const_pattern(int ippat, int impat);
void cgm_c_pip_va_fill_alignment(int vc_align);
void cgm_c_pip_trace_bg_fill(float pos_fill, float neg_fill, int imode, int fill_style, int null_col, int const_col, int bg_align);
void cgm_c_pip_bg_fill_boundaries(float pos_fill, float neg_fill);
void cgm_c_pip_bg_fill_interpolation(int imode);
void cgm_c_pip_bg_fill_style(int fill_style);
void cgm_c_pip_bg_fill_null_color(int null_col);
void cgm_c_pip_bg_fill_constant_color(int const_col);
void cgm_c_pip_bg_fill_alignment(int bg_align);
void cgm_c_pis_begin_trace_group(int grp, int pos_pri, int neg_pri, int pos_mode, int neg_mode);
void cgm_pip_end_trace_group_(void);
void cgm_c_gopks(int unit);
void cgm_c_gopwk(int wk, char *file, char *type);
void cgm_c_gacwk(int wk);
void cgm_c_gswkvp(int wk, float xmin, float xmax, float ymin, float ymax);
void cgm_c_gswn(int trans, float xmin, float xmax, float ymin, float ymax);
void cgm_c_gsvp(int trans, float xmin, float xmax, float ymin, float ymax);
void cgm_c_gselnt(int trans);
void cgm_c_gsclip(int clip);
void cgm_c_gscell(float x1, float y1, float x2, float y2, int nr, int nc, char *array, int dummy);
void cgm_c_gscr(int wk, int color, float red, float green, float blue);
void cgm_c_gsln(int type);
void cgm_c_gslwsc(float width);
void cgm_c_gsplci(int color);
void cgm_c_gpl(int n, float x[], float y[]);
void cgm_c_gsfais(int type);
void cgm_c_gsfaci(int color);
void cgm_c_gfa(int n, float x[], float y[]);
void cgm_c_set_congraf_marker_font(int set);
void cgm_c_gsmk(int type);
void cgm_c_set_congraf_marker_size(float size);
void cgm_c_set_congraf_marker_angle(float angle);
void cgm_c_gspmci(int color);
void cgm_c_gpm(int n, float x[], float y[]);
void cgm_c_gschh(float height);
void cgm_c_gschxp(float xp);
void cgm_c_gschsp(float sp);
void cgm_c_gstxal(int h, int v);
void cgm_c_gstxp(int path);
void cgm_c_gstxfp(int font, int type);
void cgm_c_gstxci(int color);
void cgm_c_gschup(float x, float y);
void cgm_c_gtx(float x, float y, char *text);
void cgm_c_gtxr(float dx, float dy, float x, float y, char *text);
void cgm_c_gdawk(int wk);
void cgm_c_gclwk(int wk);
void cgm_c_gclks(void);

#ifdef __cplusplus
}
#endif

#endif           /* _MAKECGM_H_ */
