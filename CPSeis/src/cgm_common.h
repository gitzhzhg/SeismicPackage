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
! Name       : cgm_common.h
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


#ifdef MAIN_1
#define COMMON
#else
#define COMMON extern
#endif

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <math.h>

#define TRUE 1
#define FALSE 0

#define MIN(a,b) ( (a) < (b) ? (a) : (b) )
#define MAX(a,b) ( (a) > (b) ? (a) : (b) )
#define ABS(a)   ( (a) > 0 ? (a) : -(a) )

/* Set the maximum number of viewports */
#define MAX_VIEWS 20

/* Set the maximum number of windows */
#define MAX_WINDOWS 20

/* Set the maximum number of workstation sizes */
#define MAX_WK 20

/* Set the maximum number of colors definable */
#define MAX_COLORS 512

/* Set the maximum number of fonts */
#define MAX_FONTS 29

/* Diskio buffer */
COMMON char buffer[65535];
COMMON int pointer;
COMMON int max_pointer;
COMMON float scalefactor;
/* Current transformation and current workstation */
COMMON int current_wk;
COMMON int current_trans;
COMMON int vdc_type;

/* Maximum world coordinates values,view coordinate,    **
** and workstation (device) coordinate values           */
COMMON float xw_min[MAX_WINDOWS],yw_min[MAX_WINDOWS];
COMMON float xw_max[MAX_WINDOWS],yw_max[MAX_WINDOWS];
COMMON float w_ang[MAX_WINDOWS];
COMMON float xn_min[MAX_VIEWS],yn_min[MAX_VIEWS];
COMMON float xn_max[MAX_VIEWS],yn_max[MAX_VIEWS];
COMMON float xd_min[MAX_WK],yd_min[MAX_WK];
COMMON float xd_max[MAX_WK],yd_max[MAX_WK];
COMMON float xn_min_c,yn_min_c,xn_max_c,yn_max_c;
COMMON float xd_max_c,xd_min_c,yd_max_c,yd_min_c;
COMMON float xw_max_c,xw_min_c,yw_max_c,yw_min_c,w_ang_c;
COMMON float clipx1,clipy1,clipx2,clipy2;

/* Varibable for clipping */
COMMON int clip_on;
COMMON int trans_init;

/* Current x and y coordinate */
COMMON float current_y,current_x;

/* error file for the cgm error output file */
COMMON FILE *err_unit;
COMMON char cgmdebug;

/* Variables for color information */
COMMON float back_red;
COMMON float back_blue;
COMMON float back_green;

/* Variables for the line information */
COMMON int linetype, linecolor, linefilter;
COMMON float linewidth, linefilter_tolerence;
COMMON char dshflg;

/* Variables for marker information */
COMMON int markertype, marker_scale_mode;
COMMON int markercolor, markerfont;
COMMON float markerscale, markerangle, markersize;

/* Variables for fill information */
COMMON int fillcolor;
COMMON int filltype,fillhatch;

/* Variables for text information */
COMMON int textcolor,textpath;
COMMON float textheight,textxp,textsp;
COMMON int textih,textiv;
COMMON float textux,textuy;
COMMON int textfont,textprec;

/* Variables for pip trace */
COMMON float baseline_x,baseline_y,amplitude_x,amplitude_y;
COMMON float baseline_sf,amplitude_sf;

int cgmOrInt(int *ap, int *bp);

void cgmConLogo(float xc, float yc, float l,float ang, int icol);
void cgmDashLine(int n, float x[], float y[]);
void cgmFonts(void);
void cgmGacwk(int wk);
void cgmGAxis(float x1, float y1,float x2, float y2, float v1, float v2, float h, int ivcol, int itcol);
void cgmGclks(void);
void cgmGclwk(int wk);
void cgmGdawk(int wk);
void cgmGfa(int n, float *x, float *y);
void cgmGgdpArc(float xc, float yc, float dx1, float dy1, float dx2, float dy2, float r, int itype);
void cgmGgdpCircle(float xc, float yc, float r);
void cgmGopks(int unit);
void cgmGopwk(int wk, char *file, char *type);
void cgmGpfill(int icol,int ifasi,int ifais);
void cgmGpl(int n, float *x, float *y);
void cgmGpline(int ln, float sf, int icol);
void cgmGpm(int n, float *x, float *y);
void cgmGpras(void);
void cgmGptxt(int nfont,int nprec,float h,float xp,float sp,float xup, float yup,int ih,int iv,int icol,int ipath);
void cgmGqchh(int *ierr, float *h);
void cgmGqchhX(int *ierr, float *h);
void cgmGqchsp(int *ierr, float *sp);
void cgmGqchspX(int *ierr, float *sp);
void cgmGqchspY(int *ierr, float *sp);
void cgmGqchup(int *ierr, float *xup, float *yup);
void cgmGqchwX(int *ierr, float *h);
void cgmGqchwY(int *ierr, float *h);
void cgmGqchxp(int *ierr, float *xp);
void cgmGqfaci(int *ierr, int *icol);
void cgmGqfais(int *ierr, int *icol);
void cgmGqfill(int *icol,int *ifasi,int *ifais);
void cgmGqline(int *ln, float *sf, int *icol);
void cgmGqln(int *ierr, int *ln);
void cgmGqlwsc(int *ierr, float *sf);
void cgmGqmkci(int *ierr, int *icol);
void cgmGqplci(int *ierr, int *icol);
void cgmGqtxal(int *ierr, int *ih, int *iv);
void cgmGqtxci(int *ierr, int *icol);
void cgmGqtxfp(int *ierr, int *nfont, int *nprec);
void cgmGqtxp(int *ierr, int *ipath);
void cgmGqtxt(int *nfont,int *nprec,float *h,float *xp,float *sp,float *xup, float *yup,int *ih,int *iv,int *icol,int *ipath);
void cgmGqxydc(float *x, float *y);
void cgmGqxync(float *x, float *y);
void cgmGqxywc(float *x, float *y);
void cgmGscell(float x1, float y1, float x2, float y2, int nr, int nc, char *array, int dummy);
void cgmGschh(float height);
void cgmGschhX(float height);
void cgmGschsp(float sp);
void cgmGschup(float x, float y);
void cgmGschwX(float w);
void cgmGschwY(float w);
void cgmGschxp(float xp);
void cgmGsclip(int clip);
void cgmGscr(int wk, int color, float red, float green, float blue);
void cgmGsdcnc(float xd, float yd, float *xn, float *yn);
void cgmGsdcwc(float xd, float yd, float *xw, float *yw);
void cgmGselnt(int trans);
void cgmGsfaci(int color);
void cgmGsfais(int type);
void cgmGsfasi(int ihatch);
void cgmGsfbci(int color_index_in);
void cgmGsln(int type);
void cgmGslwsc(float width);
void cgmGsmk(int type);
void cgmGsmksc(float sc);
void cgmGsncdc(float xn, float yn, float *xd, float *yd);
void cgmGsncwc(float xn, float yn, float *xw, float *yw);
void cgmGspa(float dx, float dy);
void cgmGspar(int i_wk,int i_pat,int i_dx,int i_dy,int i_dim,int ipat_array[]);
void cgmGsparf(float x0, float y0);
void cgmGsplci(int color);
void cgmGspmci(int color);
void cgmGsras(float x, float y, char a[], int nr,int nc);
void cgmGstxal(int h, int v);
void cgmGstxci(int color);
void cgmGstxfp(int font, int type);
void cgmGstxp(int path);
void cgmGsvp(int trans, float xmin, float xmax, float ymin, float ymax);
void cgmGswcdc(float xw, float yw, float *xd, float *yd);
void cgmGswcnc(float xw, float yw, float *xn, float *yn);
void cgmGswkvp(int wk, float xmin, float xmax, float ymin, float ymax);
void cgmGswkvpScale (float scale);
void cgmGswn(int trans, float xmin, float xmax, float ymin, float ymax);
void cgmGtx(float x, float y, char *text);
void cgmGtxr(float dx, float dy, float x, float y, char *text);
void cgmHAxis(float x1, float y1, float al, float v1,float v2, float h, int ivcol, int itcol, int n_tics, float h_tic);
void cgmInitAttributes(void);
void cgmInitBackground(float r,float g,float b);
void cgmInitDesc(char *desc_tmp,int ndesc_tmp);
void cgmInitLinefilter(int linefilter_tmp,float linefilter_tolerence_tmp);
void cgmInitMarkerScalingMode(int marker_tmp);
void cgmLoadMarkers(int mfont);
void cgmPipBeginTraceGroup(int grp, int pos_pri, int neg_pri, int pos_mode, int neg_mode);
void cgmPipBgFillAlignment(int bg_align);
void cgmPipBgFillBound(float pos_fill, float neg_fill);
void cgmPipBgFillConstColor(int const_col);
void cgmPipBgFillInterpolation(int imode);
void cgmPipBgFillNullColor(int null_col);
void cgmPipBgFillStyle(int fill_style);
void cgmPipEndTrace_Group(void);
void cgmPipFonts(void);
void cgmPipInit(void);
void cgmPipTrace(float x0, float y0, int grp, float fac, float tr[], int n_tr, int va_cols[], int n_va_cols, int bg_cols[], int n_bg_cols);
void cgmPipTraceBgFill(float pos_fill, float neg_fill, int imode, int fill_style, int null_col, int const_col, int bg_align);
void cgmPipTraceDisplayModes(int modes[], int n);
void cgmPipTraceMaxSamples(int n);
void cgmPipTraceOrientation(float bx, float by, float ax, float ay);
void cgmPipTraceScaleFactors(float base, float amp);
void cgmPipTraceVaFill(float xpmin, float xpmax, float xmmin, float xnmax, int istyle, int ipcol, int imcol, int ippat, int impat, int vc_align);
void cgmPipVaFillAlignment(int vc_align);
void cgmPipVaFillConstPattern(int ippat, int impat);
void cgmPipVaFillConstColor(int ipcol, int imcol);
void cgmPipVaFillNegBound(float xmmin, float xnmax);
void cgmPipVaFillPosBound(float xpmin, float xpmax);
void cgmPipVaFillStyle(int istyle);
void cgmPixplt(float x_1,float y_1,float x_2,float y_2,int nx,int ny,char a[], int isw);

void cgmPutByte(int tmpint);
void cgmPutColor(float r, float g, float b);
void cgmPutData(int class, int id);
void cgmPutEnum(int in_int);
void cgmPutFloat(float inreal);
void cgmPutFloat_c(unsigned char inreal[]);
void cgmPutHeader(int class, int id,int length);
void cgmPutIndex(int in_int);
void cgmPutInt(int in_int);
void cgmPutPoint(float in_x, float in_y);
void cgmPutReal(float in_real);
void cgmPutString(char in_string[], int length);
void cgmPutVdc(float in_real);

void cgmSetMarkerAngle(float angle);
void cgmSetMarkerFont(int set);
void cgmSetMarkerSize(float size);
void cgmSetDashline(float dash_s[], int ndash_s);
void cgmSetDefaults(void);
void cgmSetTicLine(float tic1, float htic1);
void cgmStrokeMarkers(float x,float y,float dx,float dy, int jchar, float xyasr,float markersize_tmp);
void cgmTextInit(void);
void cgmTicLine(int n, float x[], float y[]);
void cgmTransInit(void);
void cgmUorg(float x_uorg,float y_uorg);
void cgmVAxis(float x1, float y1, float al, float v1,float v2, float h, int ivcol, int itcol, int n_tics, int h_tics);

void cgmPutByteNow(unsigned char byte);
void cgmPutIntNow(long int intin);
void cgmPutBlock(void);

int cgmFindFile(const char *file, char *fullFileName, int *fullFileNameLen);
int cgmFontRead(char *file_in, short *begs, char *c, float *xs, float *ys, long *npoints);
void cgmGetDate(int *imonth,int *iday,int *iyear);
long int cgmOpenFileWrite(char *file_name_in);
long int cgmCloseFileWrite(void);
