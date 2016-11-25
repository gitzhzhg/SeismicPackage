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
#ifndef _GPlot
#define _GPlot

#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include <X11/Intrinsic.h>
#include <X11/StringDefs.h>
#include <Xm/Xm.h>
#include "transform.h"
#include "image.h"

#define MAXTRAN 10
#define MAXGWIN 10
#define MAXWDTH 1200
#define MAXHGHT 900
#define MAXCOL  256

typedef struct window_stuff
 {
  GC     gc,gcr; /* normal & rubber band gcs */
  Pixmap pmap;
  int    width;
  int    height;
  Boolean actions_added;
  Widget shell;
  Widget main,graph,gmsg;
  Widget fgcoloredit,bgcoloredit,colormapedit,readcmap;
  Widget picking,pickend,modpop;
  Widget xloc,yloc;
  int    rtoggle;
  void   *sp;     /* pointer to SeisPlot object       */
  void   *veclist;/* pointer to SeisVectLinkedList    */
  void   *scale;  /* struct EZwin * for scale display */
  char   *fontname;
  int    fontw;
  int    fonth;
  long   clip;
  long   lcol,mcol,fcol,tcol;
  long   cflags;
  ColorInfoPtr colors;
 } WinStuff;

typedef struct _Gwindow
 { WinStuff win;
   ErsCoordInf wincoords[MAXTRAN];
   long transformation; /* currently active */
   long transcount;     /* total defined    */
   long wkstn;          /* zero if not open */
   Boolean picking_on;  /* picking active?  */
   void *model;         /* ErsModel data for picking */
   void (*pickend)();   /* Called when picking is stopped */
   void *pickend_data;  /* Pass this to pickend function  */
   struct PlotImage *image; /* see image.h */
   XrmDatabase help_data;
   HelpCtx Hctx;
 } Gwindow;

/* PROTOTYPES FOR PUBLIC PLOT METHODS */
#ifdef __cplusplus
extern "C" {                 // for C++
#endif

/* following methods are in scalex.c   */
void scale_update(Gwindow *gwin);

/* following methods are in msgbox.c   */
void   GPlotMsgBox(Widget , char *);

/* following methods are in GPlotGUI.c */
void GPlotGUI(Gwindow *gwin, long width, long height, Widget parent);

/* following are defined in ColorGUI.c */
Widget ColorEditGUI (Widget , WinStuff *, int);
Widget ColorMapGUI (Widget , WinStuff *);
Widget ColorReadGUI( Widget , Gwindow *gwin );

/* following methods are in ScaleGUI.c*/
Widget ScaleGUI(Gwindow *gwin, Widget parent,struct HELPCTX *Hctx);

/* following methods are in GPlot.c    */
int  xopws(void **sp, long *iwk, long width, long height,
           Widget parent, struct HELPCTX *);
void xclws(long *iwk);
void new_pix(long *iwk, long width, long height, int opt);
void destroy_pix(long *iwk);
long gqactive();
void gsactive(long *iwk);
void gsmodel(long *iwk, void *model, void (*pickend)(void), void *data);
void *gqmodel(long *iwk);
void gsmodpop(long *iwk, Widget pickpanel);

/* COORDINATE SYSTEM SUPPORT */
void gsvp(int *ntran, float *x1, float *x2, float *y1,float *y2);
void gswn(int *ntran, float *x1, float *x2, float *y1,float *y2);
void gselnt(long *transform);
void gqcntn(long *ierr,long *transform);
void gqnt(long *ntran, long *ierr,float *wndw, float *vprt);
void gsncwc(float *xi,float *yi,float *xo,float *yo);
void gswcnc(float *xi,float *yi,float *xo,float *yo);
void gsdcwc(float *xi,float *yi,float *xo,float *yo);
void gswcdc(float *xi,float *yi,float *xo,float *yo);
void gsncdc(float *xi,float *yi,float *xo,float *yo);
void gsdcnc(float *xi,float *yi,float *xo,float *yo);
void gsconsu(long *iwk,float *scx,float *owx,float *ovx,
                       float *scy,float *owy,float *ovy);
void gsclip(long *index);
void gclrwk(long *iwk, long *junk);

/* COLOR CONTROL     */
void gsplci(long *index);
void gspmci(long *index);
void gsfaci(long *index);
void gstxci(long *index);
void gscr_name(long *iwk,long *lut,  char *fg);
void gscr_gray(long *iwk, long *ngray);
long gsgray_colors(Display *dpy,ColorInfoPtr col,int num);
void gscr_Nrgb(long *iwk,long *i,long *N,float *r,float *g,float *b);

/* DRAWING SUPPORT  */
void Xgpm(long *N,float *x, float *y);
void Xgpl(long *N,float *x, float *y);
void Xvecgpl(long *N,float *x, float *y,
  char *vname, char *cname,unsigned int lwidth, int style);
void Xgplcw( long *N, float  *x, float  *y,
          long   *width, char   *colname);
void Xgfa(long *N, float *x, float *y);
void Xgfr(float *xl, float *xr, float *yt, float *yb);
void draw_str_(char *str, float *x, float *y, long *npix, long *dir);
void draw_axis_(long *itype,float *perp,float *v1,float *v2, long *iht);
void draw_frame_(float  *xl, float  *xr, float  *ytop, float  *ybot,
     long   *linewidth, char   *xlabel, char   *ylabel, char   *linecolor);
void refresh_all(long *iwk);
void refresh_box(long *iwk,float *x1, float *x2, float *y1, float *y2);

/* Manipulation of WinStuff structure */
WinStuff *get_winstuff(long *iwk);
void *win_getsp(WinStuff *win);
void win_setsp(WinStuff *win, void *dat);
void *win_getvl(WinStuff *win);
void win_setvl(WinStuff *win, void *dat);

#ifdef __cplusplus
}                   // for C++
#endif


/**************************************************************
C Support routines to carry out coordinate conversions.
C 1. World or User coordinates.       (WC)
C 2. Normalized device coordinates.   (NC)
C 3. Pixel or Device  coordinates.    (DC)
C Written By :  R.S. DAY    2/3/93
C void  gsncwc(xi,yi,xo,yo)
C void  gswcnc(xi,yi,xo,yo)
C void  gsdcnc(xi,yi,xo,yo)
C void  gsncdc(xi,yi,xo,yo)
C void  gsdcwc(xi,yi,xo,yo)
C void  gswcdc(xi,yi,xo,yo)
C++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
**************************************************************/

#endif /* dont add past this point */
