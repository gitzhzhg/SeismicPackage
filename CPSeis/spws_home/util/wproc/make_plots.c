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
/*
C      make_plots.c
C***************************** COPYRIGHT NOTICE ********************************
C*                                                                             *
C*                 CONFIDENTIAL AND PROPRIETARY INFORMATION                    *
C*                              OF CONOCO INC.                                 *
C*                      PROTECTED BY THE COPYRIGHT LAW                         *
C*                          AS AN UNPUBLISHED WORK                             *
C*                                                                             *
C***************************** COPYRIGHT NOTICE ********************************
c        1         2         3         4         5         6         7  |
c23456789012345678901234567890123456789012345678901234567890123456789012|
C\USER DOC
C-----------------------------------------------------------------------
C                     SEISMIC PROCESSING WORKSTATION
C                   EXPLORATION RESEARCH AND SERVICES
C                              CONOCO, INC.
C
C                             U T I L I T Y 
C              written in c -- designed to be called from c
C
C     Utility Name:  make_plots    (create plots)
C          Written:  93/04/06  by:  Tom Stoeckley
C     Last revised:  93/04/06  by:  Tom Stoeckley
C
C  Purpose:  Shorthand routines to create simple plots.
C            The plot expands to fill the available area.
C            The plot has labelled axes and a cursor readout.
C
C  Note:     These routines add a special structure to the widget's
C            userData resource; therefore this resource is not available
C            to the user of widgets created by these routines.
C
C  Related Documentation:
C-----------------------------------------------------------------------
C                          LOCATION OF CODE
C
C  machine:                ultrix  (pospsv)
C  library:                ~spws/lib/          trslib.a
C  source file:            ~spws/trslib/       make_plots.c
C  context help file:      ~spws/app-defaults/ Make_plots_help
C  include file:           ~spws/include/      make_plots.h
C----------------------------------------------------------------------
C             DIFFERENCES BETWEEN CODE ON DIFFERENT MACHINES
C                                n/a
C-----------------------------------------------------------------------
C                ROUTINES ON SOURCE FILE make_plots.c
C
C  Documented routines:
C
C     make_plot          register_xaxis          register_yaxis
C
C  Undocumented static routines:
C
C     display_readouts   event_handler           destroy_callback
C     startup_routine    display_blank_readouts  display_good_readouts
C     register_ndec      get_step                xactivate_callback
C     draw_xtic          draw_ytic               yactivate_callback
C     register_axis
C-----------------------------------------------------------------------
C          LIBRARIES AND HEADER FILES REQUIRED BY THIS UTILITY
C     (C, X, Xt, and Motif libraries and header files not included)
C
C  Libraries:     trslib.a   wproc.a
C  Header files:  wproc.h    widget_util.h
C-----------------------------------------------------------------------
C                 EXTERNALS REFERENCED BY THIS UTILITY
C          (C, X, Xt, and Motif library routines not included)
C
C  set_compound_resource      get_shell_widget    attach_widget
C  add_HELP                   
C-----------------------------------------------------------------------
C                           REVISION HISTORY
C
C     Date      Author     Description
C     ----      ------     -----------
C  2.
C  1. 93/04/06  Stoeckley  Initial version.
C-----------------------------------------------------------------------
C-----------------------------------------------------------------------
C-----------------------------------------------------------------------
C-----------------------------------------------------------------------
C  To create a managed plot widget:
C
C         w = make_plot(parent, name, hctx)
C
C  Widget             w = created form widget (returned).
C  Widget        parent = parent of form widget to create.
C  String          name = name of form widget to create.
C  struct HELPCTX *hctx = pointer to help context (can be NULL).
C
C  A private structure is saved in the XmNuserData resource of the
C     returned widget, so the programmer must not use this resource.
C  The following children of the returned form widget are also created:
C         a drawing area widget with name  "mp_draw"
C         two pushbuttons       with names "mp_xlabel" and "mp_ylabel"
C         two text widgets      with names "mp_xtext"  and "mp_ytext"
C-----------------------------------------------------------------------
C  To register information for the x or y axis:
C
C     void register_xaxis (w, label, array, n, ndec1, ndec2)
C     void register_yaxis (w, label, array, n, ndec1, ndec2)
C
C  Widget      w = form widget returned by make_plot.
C  String  label = label for x or y axis.
C  float array[] = array to be plotted on x or y axis.
C  long       *n = pointer to length of arrays (must be same in all calls).
C  long    ndec1 = max number of decimals to print on x or y axis.
C  long    ndec2 = max number of decimals to print in x or y cursor readout.
C
C  Pointers to the arrays are kept for use whenever the plotting is
C    done, so that the plot will always be up-to-date with the current
C    values in the arrays.
C  Each of these routines must be called at least once.  Additional
C    calls will register information allowing the user to change the 
C    array being plotted on the indicated axis whenever he wishes, by 
C    pressing pushbuttons.
C-----------------------------------------------------------------------
C  To draw or update the plot:
C
C                void do_plot(w)
C
C  Widget w = form widget returned by make_plot.
C
C  This routine is also called automatically upon expose events.
C-----------------------------------------------------------------------
C                                NOTES
C
C 1. The context help file Make_plots_help should be copied into the 
C    application's context help file.  This includes help on the drawing 
C    area part of the plot.  The programmer should provide the help on 
C    two pushbuttons which display the axis labels; see the file 
C    Make_plots_help for details.
C
C 2. Examples of application defaults are as follows:
C           *XXXX*background:  light blue
C           *XXXX*foreground:  brown
C    where XXXX is the name specified in the make_plot call.
C-----------------------------------------------------------------------
C\END DOC
*/



/*----------------------- header files ---------------------------------*/

#include <stdio.h>
#include <math.h>
#include <Xm/Text.h>
#include <Xm/Label.h>
#include <Xm/PushB.h>
#include <Xm/Form.h>
#include <Xm/DrawingA.h>
#include <X11/StringDefs.h>
#include <X11/cursorfont.h>
#include "wproc.h"



/*-------------------- defined constants -------------------------------*/

#define HEIGHT     200
#define TIC          6
#define NCHAR1       6
#define NCHAR2       9
#define NDEC1MAX    (NCHAR1 - 2)
#define NDEC2MAX    (NCHAR2 - 2)
#define TEXTLENGTH  80
#define TINY        1.0e-6
#define MAXNUMA     20


/*------------------------- transformations ----------------------------*/

#define get_pixel_from_x(x) (aa.left + 0.5 + ((x) - pp->xmin) * pp->xfactor)
#define get_pixel_from_y(y) (aa.top  + 0.5 + ((y) - pp->ymin) * pp->yfactor)

#define get_x_from_pixel(xp) (pp->xmin + (double)((xp) - aa.left) / pp->xfactor)
#define get_y_from_pixel(yp) (pp->ymin + (double)((yp) - aa.top ) / pp->yfactor)



/*--------------------- data declarations needed internally ------------*/

enum { XFLAG, YFLAG };

typedef struct _AllplotStruct           /* single global structure */
{
  Cursor     cross;
  GC         gc, gc2;
  Dimension  cellx, celly;
  int        left, right, top, bottom;
} AllplotStruct;

static AllplotStruct aa;
   

typedef struct _PlotStruct              /* allocated for each plot */
{
  Widget     shell, w, draw, xlabel, ylabel, xtext, ytext;
  double     xmin, ymin, xmax, ymax, xfactor, yfactor;
  long      *npoint;
  int        xpointer, ypointer;
  long       numa, ix, iy;
  Boolean    exact, xok, yok;
  int        xyflag[MAXNUMA];
  long       ndec1 [MAXNUMA];
  long       ndec2 [MAXNUMA];
  float     *apoint[MAXNUMA];
  char       label [MAXNUMA][TEXTLENGTH];
} PlotStruct;
   


/*------------------- startup routine ----------------------------------*/

static void startup_routine(Widget w)
{
  unsigned long valuemask;
  XFontStruct *font;
  XGCValues values;
  Display *disp;
  Window wind;
  static Boolean started = False;

  if(started) return;
  started = True;

  disp = XtDisplay(w);
  wind = XtWindow(w);

            font = XLoadQueryFont(disp, "8x13");
  if(!font) font = XLoadQueryFont(disp, "9x15");
  if(!font) font = XLoadQueryFont(disp, "fixed");
  valuemask = 0;
  valuemask = GCForeground | GCBackground | GCLineWidth | GCFont;
  valuemask =                               GCLineWidth | GCFont;
  values.font = font->fid;
  values.line_width = 2;
/*
  values.foreground = "black";        need pixel not string
  values.background = "light blue";        need pixel not string
*/
  aa.gc     = XCreateGC(disp, wind, valuemask, &values);
  values.line_width = 1;
  aa.gc2    = XCreateGC(disp, wind, valuemask, &values);
  aa.cross  = XCreateFontCursor(disp, XC_crosshair);
  aa.cellx  = font->max_bounds.width;
  aa.celly  = font->ascent + font->descent;
  aa.left   = (NCHAR1 + 1) * aa.cellx;
  aa.right  = aa.left / 2;
  aa.top    = aa.celly;
  aa.bottom = 2 * aa.celly;
}



/*------------------- display readouts ---------------------------------*/

static void display_good_readouts(PlotStruct *pp, double x, double y)
{
  char text[TEXTLENGTH];

  sprintf(text, "%*.*f", (int)NCHAR2, (int)pp->ndec2[pp->ix], x);
  XmTextSetString(pp->xtext, text);
  sprintf(text, "%*.*f", (int)NCHAR2, (int)pp->ndec2[pp->iy], y);
  XmTextSetString(pp->ytext, text);
  XDefineCursor(XtDisplay(pp->shell), XtWindow(pp->shell), aa.cross);
}



static void display_blank_readouts(PlotStruct *pp)
{
  XmTextSetString(pp->xtext, " ");
  XmTextSetString(pp->ytext, " ");
  XDefineCursor(XtDisplay(pp->shell), XtWindow(pp->shell), None);
}



static void display_readouts(PlotStruct *pp)
{
  double x, y;

  startup_routine(pp->w);
  if(pp->xfactor == 0.0 || pp->xpointer == 0 ||
     pp->yfactor == 0.0 || pp->ypointer == 0)
       {
       display_blank_readouts(pp);
       return;
       }
  x = get_x_from_pixel(pp->xpointer);
  y = get_y_from_pixel(pp->ypointer);
  if(x < pp->xmin || x > pp->xmax ||
     y < pp->ymin || y > pp->ymax)
       {
       display_blank_readouts(pp);
       }
  else
       {
       display_good_readouts(pp, x, y);
       }
}



/*---------- functions to draw tic marks and labels --------------------*/

static void draw_xtic(PlotStruct *pp, Display *disp, Window wind,
                                       Dimension height, double value)
{
  int x, xminus;
  char text[TEXTLENGTH];

  x = get_pixel_from_x(value);
  xminus = x - NCHAR1 * aa.cellx / 2;
  sprintf(text, "%*.*f", (int)NCHAR1, (int)pp->ndec1[pp->ix], value);
/*
  XDrawString(disp, wind, aa.gc, xminus,          aa.celly, text, NCHAR1);
*/
  XDrawString(disp, wind, aa.gc, xminus, height - aa.celly, text, NCHAR1);
  XDrawLine  (disp, wind, aa.gc2, x,        aa.top   , x,        aa.top   +TIC);
  XDrawLine  (disp, wind, aa.gc2, x, height-aa.bottom, x, height-aa.bottom-TIC);
}



static void draw_ytic(PlotStruct *pp, Display *disp, Window wind,
                                       Dimension width, double value)
{
  int y, yplus;
  char text[TEXTLENGTH];

  y = get_pixel_from_y(value);
  yplus = y + aa.celly / 4;
  sprintf(text, "%*.*f", (int)NCHAR1, (int)pp->ndec1[pp->iy], value);
  XDrawString(disp, wind, aa.gc, 3, yplus, text, NCHAR1);
  XDrawLine  (disp, wind, aa.gc2,       aa.left , y,       aa.left +TIC, y);
  XDrawLine  (disp, wind, aa.gc2, width-aa.right, y, width-aa.right-TIC, y);
}



/*----------------- get step size --------------------------------------*/

static void get_step(double xmin, double xmax, double stepmin,
                              double *step, int *istart, int *istop)
{
  *step = pow(10.0, ceil( log10(stepmin) ));
  while(*step > 2.00 * stepmin) *step *= 0.50;
  *istart = (int)ceil (xmin / *step - TINY);  /* now *istart * *step >= xmin */
  *istop  = (int)floor(xmax / *step + TINY);  /* now *istop  * *step <= xmax */
}



/*----------------- function to actually do the plot -------------------*/

void do_plot(Widget w)
{
  PlotStruct *pp;
  Dimension width, height, width2, height2;
  Display *disp;
  Window wind;
  double xdenom, ydenom, step, stepmin;
  int i, x1, y1, x2, y2, istart, istop;

  if(!w) return;
  startup_routine(w);
  XtVaGetValues(w, XmNuserData, &pp, NULL);
  if(!pp) return;
  disp = XtDisplay(pp->draw);
  wind = XtWindow(pp->draw);
  if(!disp || !wind) return;
  XClearWindow(disp, wind);
/*
  if(*pp->npoint <= 1) return;
*/
  if(*pp->npoint <= 0) 
       {
       XtVaGetValues(pp->draw, XmNwidth, &width, XmNheight, &height, NULL);
       width2  = width  - aa.left - aa.right;
       height2 = height - aa.top - aa.bottom;
       XDrawRectangle(disp, wind, aa.gc2, aa.left, aa.top, width2, height2);
       display_readouts(pp);
       pp->xfactor = pp->yfactor = 0; return;
       }
  if(!pp->xok || !pp->yok) return;

                /*-----get limits-----*/

  pp->xmin = pp->xmax = pp->apoint[pp->ix][0];
  pp->ymin = pp->ymax = pp->apoint[pp->iy][0];
  for(i = 0; i < *pp->npoint; i++)
     {
     if(pp->apoint[pp->ix][i] < pp->xmin) pp->xmin = pp->apoint[pp->ix][i];
     if(pp->apoint[pp->iy][i] < pp->ymin) pp->ymin = pp->apoint[pp->iy][i];
     if(pp->apoint[pp->ix][i] > pp->xmax) pp->xmax = pp->apoint[pp->ix][i];
     if(pp->apoint[pp->iy][i] > pp->ymax) pp->ymax = pp->apoint[pp->iy][i];
     }
  if(pp->xmax <= pp->xmin) { pp->xmax = pp->xmin + 1; pp->xmin--; }
  if(pp->ymax <= pp->ymin) { pp->ymax = pp->ymin + 1; pp->ymin--; }

             /*-----get transformations-----*/

  XtVaGetValues(pp->draw, XmNwidth, &width, XmNheight, &height, NULL);
  xdenom = pp->xmax - pp->xmin;
  ydenom = pp->ymax - pp->ymin;
  if(width  <= aa.left + aa.right || xdenom == 0 ||
     height <= aa.top + aa.bottom || ydenom == 0)
       {
       pp->xfactor = pp->yfactor = 0; return;
       }
  width2  = width  - aa.left - aa.right;
  height2 = height - aa.top - aa.bottom;
  pp->xfactor = width2  / xdenom;
  pp->yfactor = height2 / ydenom;

                 /*-----draw lines-----*/

  for(i = 1; i < *pp->npoint; i++)
       {
       x1 = get_pixel_from_x(pp->apoint[pp->ix][i - 1]);
       y1 = get_pixel_from_y(pp->apoint[pp->iy][i - 1]);
       x2 = get_pixel_from_x(pp->apoint[pp->ix][i    ]);
       y2 = get_pixel_from_y(pp->apoint[pp->iy][i    ]);
       XDrawLine     (disp, wind, aa.gc, x1  , y1  , x2, y2);
       }

                 /*-----draw points-----*/

  for(i = 0; i < *pp->npoint; i++)
       {
       x2 = get_pixel_from_x(pp->apoint[pp->ix][i    ]);
       y2 = get_pixel_from_y(pp->apoint[pp->iy][i    ]);
       XFillRectangle(disp, wind, aa.gc, x2-2, y2-2, 5 , 5 );
       }

                 /*-----draw axes-----*/

  if(pp->exact)
       {
       for(i = 0; i < *pp->npoint; i++)
            {
            draw_xtic(pp, disp, wind, height, pp->apoint[pp->ix][i]);
            draw_ytic(pp, disp, wind, width , pp->apoint[pp->iy][i]);
            }
       }
  else
       {
       stepmin = (NCHAR1 + 1) * aa.cellx * (pp->xmax - pp->xmin) / width2;
       get_step(pp->xmin, pp->xmax, stepmin, &step, &istart, &istop);
       for(i = istart; i <= istop; i++)
            {
            draw_xtic(pp, disp, wind, height, i * step);
            }
       stepmin = (1 + aa.celly) * (pp->ymax - pp->ymin) / height2;
       get_step(pp->ymin, pp->ymax, stepmin, &step, &istart, &istop);
       for(i = istart; i <= istop; i++)
            {
            draw_ytic(pp, disp, wind, width, i * step);
            }
       }
  XDrawRectangle(disp, wind, aa.gc2, aa.left, aa.top, width2, height2);
  display_readouts(pp);
}




/*-------------------- event handler -----------------------------------*/

static void event_handler(Widget draw, PlotStruct *pp, XEvent *event)
{
  switch(event->type)
       {
       case Expose:
           if(event->xexpose.count == 0) do_plot(pp->w);
           break;
       case ConfigureNotify:
           do_plot(pp->w);
           break;
       case ButtonPress:
           if(event->xbutton.button == 2)
                {
                pp->exact = !pp->exact;
                do_plot(pp->w);
                }
       case MotionNotify:
           pp->xpointer = event->xmotion.x;
           pp->ypointer = event->xmotion.y;
           display_readouts(pp);
           break;
       case LeaveNotify:
           pp->xpointer = 0;
           pp->ypointer = 0;
           display_readouts(pp);
           break;
       }
}




/*------------- callbacks ----------------------------------------------*/

static void destroy_callback (Widget w, PlotStruct *pp, void *call)
{
  free(pp);
}


static void xactivate_callback (Widget w, PlotStruct *pp, void *call)
{
  if(!pp->xok) return;
  do
       {
       pp->ix++;  if(pp->ix >= pp->numa) pp->ix = 0;
       }
       while(pp->xyflag[pp->ix] != XFLAG);
  set_compound_resource(pp->xlabel, XmNlabelString, pp->label[pp->ix]);
  do_plot(pp->w);
}


static void yactivate_callback (Widget w, PlotStruct *pp, void *call)
{
  if(!pp->yok) return;
  do
       {
       pp->iy++;  if(pp->iy >= pp->numa) pp->iy = 0;
       }
       while(pp->xyflag[pp->iy] != YFLAG);
  set_compound_resource(pp->ylabel, XmNlabelString, pp->label[pp->iy]);
  do_plot(pp->w);
}



/*------------------ create the plot widget ----------------------------*/

Widget make_plot(Widget parent, String name, struct HELPCTX *hctx)
{
  int i;
  Arg args[8];
  PlotStruct *pp;

  pp = (PlotStruct*)calloc(1,sizeof(PlotStruct));
  pp->shell = get_shell_widget(parent);

  i=0;
  XtSetArg(args[i], XmNborderWidth      , 5 ); i++;
  XtSetArg(args[i], XmNhorizontalSpacing, 4 ); i++;
  XtSetArg(args[i], XmNverticalSpacing  , 4 ); i++;
  XtSetArg(args[i], XmNuserData         , pp); i++;
  pp->w = XmCreateForm(parent, name, args, i);
 
  i=0;
  XtSetArg(args[i], XmNheight     , HEIGHT); i++;
  XtSetArg(args[i], XmNtraversalOn, False ); i++;
  pp->draw = XmCreateDrawingArea(pp->w, "mp_draw", args, i);

  i=0;
  XtSetArg(args[i], XmNmarginHeight  , 0     ); i++;
  XtSetArg(args[i], XmNresizable     , True  ); i++;
  XtSetArg(args[i], XmNrecomputeSize , True  ); i++;
  pp->xlabel = XmCreatePushButton(pp->w, "mp_xlabel", args, i);
  pp->ylabel = XmCreatePushButton(pp->w, "mp_ylabel", args, i);

  i = 0;
  XtSetArg(args[i], XmNcolumns  , (short)NCHAR2); i++;
  XtSetArg(args[i], XmNmaxLength,   (int)NCHAR2); i++;
  XtSetArg(args[i], XmNmarginHeight  , 0       ); i++;
  XtSetArg(args[i], XmNsensitive     , False   ); i++;
  pp->xtext = XmCreateText (pp->w, "mp_xtext" , args, i);
  pp->ytext = XmCreateText (pp->w, "mp_ytext" , args, i);

  attach_widget(pp->draw  , pp->w     , pp->w    , pp->w, pp->xtext, 0,0,0,0);
  attach_widget(pp->ylabel, pp->w     , NULL     , NULL , pp->w    , 0,0,0,0);
  attach_widget(pp->ytext , pp->ylabel, NULL     , NULL , pp->w    , 0,0,0,0);
  attach_widget(pp->xlabel, NULL      , pp->xtext, NULL , pp->w    , 0,0,0,0);
  attach_widget(pp->xtext , NULL      , pp->w    , NULL , pp->w    , 0,0,0,0);

  XtManageChild(pp->w     );
  XtManageChild(pp->draw  );
  XtManageChild(pp->xlabel);
  XtManageChild(pp->ylabel);
  XtManageChild(pp->xtext );
  XtManageChild(pp->ytext );

  XtAddEventHandler(pp->draw, ExposureMask | StructureNotifyMask |
              PointerMotionMask | ButtonPressMask | LeaveWindowMask,
              FALSE, (XtEventHandler)event_handler, (XtPointer)pp);
  XtAddCallback(pp->w     , XmNdestroyCallback , 
                        (XtCallbackProc)destroy_callback  , (XtPointer)pp);
  XtAddCallback(pp->xlabel, XmNactivateCallback, 
                        (XtCallbackProc)xactivate_callback, (XtPointer)pp);
  XtAddCallback(pp->ylabel, XmNactivateCallback, 
                        (XtCallbackProc)yactivate_callback, (XtPointer)pp);
  if(hctx)
       {
       add_HELP(pp->draw  , helper, hctx);
       add_HELP(pp->xlabel, helper, hctx);
       add_HELP(pp->ylabel, helper, hctx);
       }
  return pp->w;
}

 

/*----------------- register axes --------------------------------------*/

static void register_axis(Widget w, String label, float *array, long *n,
                                       long ndec1, long ndec2, int xyflag)
{
  PlotStruct *pp;

  if(!w) return;
  XtVaGetValues(w, XmNuserData, &pp, NULL);
  if(!pp) return;
  if(pp->numa >= MAXNUMA) return;
  pp->apoint[pp->numa] = array;
  pp->xyflag[pp->numa] = xyflag;
  strcpy(pp->label[pp->numa], label);
  pp->npoint = n;
  if(ndec1 <= ndec2)
       {
       ndec1 = ndec1 ^ ndec2;
       ndec2 = ndec1 ^ ndec2;
       ndec1 = ndec1 ^ ndec2;
       }
  if(ndec1 < 0) ndec1 = 0;  else if(ndec1 > NDEC1MAX) ndec1 = NDEC1MAX;
  if(ndec2 < 0) ndec2 = 0;  else if(ndec2 > NDEC2MAX) ndec2 = NDEC2MAX;
  pp->ndec1[pp->numa] = ndec1;
  pp->ndec2[pp->numa] = ndec2;
  if(xyflag == XFLAG && !pp->xok)
       {
       pp->ix = pp->numa;
       pp->xok = True;
       set_compound_resource(pp->xlabel, XmNlabelString, pp->label[pp->ix]);
       }
  if(xyflag == YFLAG && !pp->yok)
       {
       pp->iy = pp->numa;
       pp->yok = True;
       set_compound_resource(pp->ylabel, XmNlabelString, pp->label[pp->iy]);
       }
  pp->numa++;
}



void register_xaxis(Widget w, String label, float *array, long *n,
                                                long ndec1, long ndec2)
{ register_axis(w, label, array, n, ndec1, ndec2, XFLAG); }


void register_yaxis(Widget w, String label, float *array, long *n,
                                                long ndec1, long ndec2)
{ register_axis(w, label, array, n, ndec1, ndec2, YFLAG); }



/*--------------------------- end --------------------------------------*/

