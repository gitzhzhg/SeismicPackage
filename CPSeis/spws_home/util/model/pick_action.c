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
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <math.h>
#include <X11/Xos.h>
#include <Xm/XmP.h>

#include "transform.h"
#include "ers_seis.h"
#include "image.h"
#include "pick.h"

#define ErsMONOCHROME 0
#define ErsCOLOR      1
#define ErsGRAY       2

/* actions stuff */
static Bool    actions_added_flag = False;
static void PointInsert();
static void PointStartMove();
static void PointMove();
static void PointMoveConstraint();
static void PointEndMove();
static void PointDelete();
static void PointSelect();
static void AutoPick();
static void DeleteLastPoint();
static void HorizonCreate();
static void HorizonSelect();
static void HorizonDelete();
static void HorizonStartMove();
static void HorizonMove();
static void HorizonEndMove();
static void InitAreaSelection();
static void ExtendAreaSelection();
static void EndAreaSelection();
void PR_ToPostscript();

static XtActionsRec action_table[] =
 {
  {"PointInsert",       PointInsert},
  {"PointStartMove",    PointStartMove},
  {"PointEndMove",      PointEndMove},
  {"PointMove",         PointMove},
  {"PointMoveConstraint", PointMoveConstraint},
  {"PointDelete",       PointDelete},
  {"PointSelect",       PointSelect},
  {"AutoPick",          AutoPick},
  {"DeleteLastPoint",   DeleteLastPoint},
  {"HorizonCreate",     HorizonCreate},
  {"HorizonSelect",     HorizonSelect},
  {"HorizonDelete",     HorizonDelete},
  {"HorizonStartMove",  HorizonStartMove},
  {"HorizonMove",       HorizonMove},
  {"HorizonEndMove",    HorizonEndMove},
  {"InitAreaSelection", InitAreaSelection},
  {"ExtendAreaSelection", ExtendAreaSelection},
  {"EndAreaSelection",  EndAreaSelection},
 };

/* private functions declaration */
static void    RedrawHorizonBox();
static void    ErsDrawCallback();
static void    ErsDestroyCallback();
static void    ErsHorizonSetGC();
static void    ErsHorizonUpdateMinMax();
static int     CalculateMaxLineWidth();
static Bool    ErsPointInside();
static void    ErsPointTimeMinMax();

/* sample tracker and picker */
static void    PR_SnapCallback();
static void    PR_TrackerCallback();

extern float *ErsSeismicGetData();
extern float *ErsSeismicGetSamples();

void ErsPostscriptSetColor(FILE *fp, Widget *widget,Pixel pixel);

/***************************************************************************/
/*                                                                         */
/* void PR_Destroy()                                          */
/*   Function used to destroy and free the resources attached to a picking */
/*   record. Breaks connection with all attached widgets                   */
/*                                                                         */
/***************************************************************************/
void PR_Destroy(PR_ *picking_record)
{ ErsHorizon *slist[299];
  Widget widget;
  int count;
  register int i;

  if(picking_record == NULL) return;
  /* remove link with all connected widgets */
  while (picking_record->widget_count != 0)
  { widget = PR_GetWidget(picking_record, 1);
    PR_DisconnectWidget(picking_record, widget);
  }

  /* free GC */
  if(picking_record->gc != NULL)
   XFreeGC(picking_record->display, picking_record->gc);
  if(picking_record->gc_symbol != NULL)
   XFreeGC(picking_record->display, picking_record->gc_symbol);

  /* destroy all the horizons */
  count = picking_record->horizon_count;
  for (i=count-1; i>=0; i--) slist[i] =  picking_record->horizon_list[i];
  slist[count] = NULL;
  ErsHorizonDestroy(picking_record, slist);

  /* free structure */
  free(picking_record);
}

/*********************************************************/
/* void PR_ConnectWidget()                  */
/* Updates the widget_rec member of the pick record.     */
/* Stores the picking record in the widgets data field.  */
/* Adds Expose and Destroy CallBacks to the widget.      */
/*********************************************************/
void PR_ConnectWidget(picking_record, widget, P)
PR_ *picking_record;
Widget widget;
caddr_t P;
{ Display *display;
  register int i;
  extern void PR_SnapCallback(),PR_TrackerCallback();

  if(widget == NULL) return;

  if (!actions_added_flag)
   {/* add actions to list */
    XtAppAddActions(XtWidgetToApplicationContext(widget), action_table,
                 XtNumber(action_table));
    actions_added_flag = True;
   }

  display = XtDisplay(widget);
  picking_record->display = display;

  picking_record->colormap = XutilWidgetColormap(widget);

  if (picking_record->widget_count == 0)
   {/* create GC */
    picking_record->gc = XCreateGC(display,
                       DefaultRootWindow(display), 0, NULL);
    picking_record->gc_symbol = XCreateGC(display,
                       DefaultRootWindow(display), 0, NULL);

    picking_record->widget_count = 1;
    picking_record->widget_rec = (ErsWidgetRecord *)
                                 calloc(1,sizeof(ErsWidgetRecord));
   }
  else
   {picking_record->widget_count++;
    picking_record->widget_rec = (ErsWidgetRecord *)
                realloc(picking_record->widget_rec,
                sizeof(ErsWidgetRecord) * picking_record->widget_count);
   }

  picking_record->widget_rec[picking_record->widget_count-1].widget = widget;

  /* Save the Picking record in the widgets user_data field */
  ErsSetPickingRecord((Widget) widget,(caddr_t) picking_record);

  /* Save the PlotImage structure in the Picking Record */
  /* P must be a non-volatile pointer                   */
  ErsSetPlotInf(widget, P);

  /* save postscript proc in widget structure */
  /*seismic->seismic.picking_postscript_proc =
           (XtProc) PR_ToPostscript;
  */

  /* Install the current translations */
  ErsWidgetSetTranslations(widget, picking_record->translations,
                           picking_record->install_translations_mode);

  /* Add expose callback */
  XtAddCallback(widget, XmNexposeCallback, ErsDrawCallback,
picking_record);

  /* Add destroy callback, to disconnect widget when it is destroyed
*/
  XtAddCallback(widget, XmNdestroyCallback, ErsDestroyCallback, picking_record)
;

 /* Set the default coordinate map For User <-> Device mapping.  */
  ErsSeismicSetCoordInf(widget);
/*
  ErsSeismicSetCoordInf0(widget,T);
*/

  /* draw all the horizons */
  if(picking_record->horizon_count > 0)
   { PR_SetCurrentHorizon(picking_record,
      picking_record->horizon_list[0]);
   }
  for (i=0; i<picking_record->horizon_count; i++)
    ErsHorizonDraw(picking_record, picking_record->horizon_list[i],
                   widget, 0, 0, XutilWidgetWidth(widget),
                   XutilWidgetHeight(widget));

  picking_record->snap_callback    = PR_SnapCallback;
  picking_record->tracker_callback = PR_TrackerCallback;
}

/*********************************************************/
/* void PR_DisconnectWidget()               */
/* Updates the widget_rec member of the pick record.     */
/* Nulls the widgets data field.                         */
/* Removes CallBacks from the widget.                    */
/*********************************************************/
void PR_DisconnectWidget(picking_record, widget)
PR_ *picking_record;
Widget widget;
{
  register int i, k;

  for (i=0; i<picking_record->widget_count; i++)
    if (picking_record->widget_rec[i].widget == widget) break;

  if (i == picking_record->widget_count) {
    XtWarning("PR_DisconnectWidget: widget is not in list");
    return;
  }

  /* repaint the seismic data in the  widget */
  ErsSeismicDraw(widget, 0, 0, XutilWidgetWidth(widget),
                               XutilWidgetHeight(widget), False);

  /* remove widget destroy callback */
  XtRemoveCallback(widget, XmNdestroyCallback, ErsDestroyCallback,
                   picking_record);
  /* remove widget expose callback */
  XtRemoveCallback(widget, XmNexposeCallback, ErsDrawCallback,
picking_record);

  /* set picking record structure to NULL inside widget structure */
  ErsSetPickingRecord((Widget) widget, NULL);

  picking_record->widget_count--;
  for (k=i; k<picking_record->widget_count; k++) {
    picking_record->widget_rec[k].widget  =
                    picking_record->widget_rec[k+1].widget;
    picking_record->widget_rec[k].data =
picking_record->widget_rec[k+1].data;
    picking_record->widget_rec[k].proc =
picking_record->widget_rec[k+1].proc;
  }

}

/***********************************************************************
 * void ErsSetPickingRecord()
 *    public method to set the picking record.
 * PR_ *ErsGetPickingRecord()
 *    public method to get the picking record.

**********************************************************************/
void ErsSetPickingRecord(widget, picking_record)
Widget  widget;
caddr_t picking_record;
{
  Arg args[2];
  int n;

  if(widget == NULL) return;
  n = 0;
  XtSetArg (args[n], XmNuserData,picking_record); n++;
  XtSetValues(widget,args,n);
}
PR_ *ErsGetPickingRecord(Widget widget)
{ PR_ *picking_record;
  Arg args[2];
  int n;

  if(widget == NULL) return NULL;
  picking_record = NULL;
  n = 0;
  XtSetArg (args[n], XmNuserData,&picking_record); n++;
  XtGetValues(widget,args,n);
  return (PR_ *) picking_record;
}

Bool    PR_SetHkeys(Widget W, int Phdr, int Shdr, int Thdr)
{ PR_ *pikrec;
  pikrec = (PR_ *) ErsGetPickingRecord(W);
  if(pikrec == NULL) return False;

  pikrec->Phdr = Phdr;
  pikrec->Shdr = Shdr;
  pikrec->Thdr = Thdr;
 return True;
}
Bool    PR_GetHkeys(Widget W, int *Phdr, int *Shdr, int *Thdr)
{ PR_ *pikrec;
  *Phdr = UNDEFINED_KEY;
  *Shdr = UNDEFINED_KEY;
  *Thdr = UNDEFINED_KEY;
  pikrec = (PR_ *) ErsGetPickingRecord(W);
  if(pikrec == NULL) return False;

  *Phdr = pikrec->Phdr;
  *Shdr = pikrec->Shdr;
  *Thdr = pikrec->Thdr;
 return True;

}
/**********************************************************************
 *  ErsCoordInf *ErsGetCoordInf()
 *    Public method to get the coordinate transformation
 *  Bool    ErsSetCoordInf()
 * Save or redefine the coordinate transformation between
 * device and user coordinates for the widget.
 * One picking record may be tied to several widgets.
 * Each widget may have its own transformation so the
 * coordinate data is saved in the widget record.
 **********************************************************************/
ErsCoordInf *ErsGetCoordInf(Widget W)
{PR_ *pikrec;
 int n;

 pikrec = ErsGetPickingRecord(W);
 if(pikrec == NULL) return NULL;

 for(n=0;n<pikrec->widget_count;n++)
   { if(W == pikrec->widget_rec[n].widget) break; }
 if(n == pikrec->widget_count) return NULL;
 return (ErsCoordInf *) pikrec->widget_rec[n].data;
}

Bool   ErsSetCoordInf0(Widget W, ErsCoordInf *T)
{ PR_ *pikrec;
  int n;
  pikrec = (PR_ *) ErsGetPickingRecord(W);
  if(pikrec == NULL) return False;
  for(n=0;n<pikrec->widget_count;n++)
   { if(W == pikrec->widget_rec[n].widget)  break; }
  if(n == pikrec->widget_count) return False;
  if(T == NULL) return False;

  pikrec->widget_rec[n].data = (caddr_t) T;
 return True;
}

Bool    ErsSetCoordInf(Widget W,
               float *xd1, float *yd1, float *xw1, float *yw1,
               float *xd2, float *yd2, float *xw2, float *yw2)
{
  PR_ *pikrec;
  ErsCoordInf      *T;
  float            x[4];
  int n;

  pikrec = (PR_ *) ErsGetPickingRecord(W);
  if(pikrec == NULL) return False;
  for(n=0;n<pikrec->widget_count;n++)
   { if(W == pikrec->widget_rec[n].widget)  break; }
  if(n == pikrec->widget_count) return False;

  T  = (ErsCoordInf *) pikrec->widget_rec[n].data;
  if(T == NULL)
    {  T = (ErsCoordInf *) calloc(1,sizeof(struct _ErsCoordInf ));
       pikrec->widget_rec[n].data = (caddr_t) T; }
/*       left         right        top         bottom */
  x[0] = *xd1; x[1] = *xd2; x[2] = *yd1; x[3]= *yd2;
  Set_dc(T, x);
  x[0] = *xw1; x[1] = *xw2; x[2] = *yw1; x[3]= *yw2;
  Set_uc(T, x);

 return True;
}

void PR_SetTranslations(picking_record, translations, mode)
PR_ *picking_record;
XtTranslations translations;
int mode;
{ register int i;

  if (translations == NULL ) return;

  for (i=0; i<picking_record->widget_count; i++)
    ErsWidgetSetTranslations(picking_record->widget_rec[i].widget, translations,
                             mode);

  /* save the current translations */
  picking_record->translations = translations;
  picking_record->install_translations_mode = mode;
}

void PR_PointCallback(picking_record, point_select_callback,
             point_select_callback_data, point_add_callback,
             point_add_callback_data, point_delete_callback,
             point_delete_callback_data)
PR_ *picking_record;
XtCallbackProc point_select_callback;
caddr_t point_select_callback_data;
XtCallbackProc point_add_callback;
caddr_t point_add_callback_data;
PointFun point_delete_callback;
caddr_t point_delete_callback_data;
{
  picking_record->point_select_callback = point_select_callback;
  picking_record->point_select_callback_data =
point_select_callback_data;
  picking_record->point_add_callback = point_add_callback;
  picking_record->point_add_callback_data = point_add_callback_data;
  picking_record->point_delete_callback = point_delete_callback;
  picking_record->point_delete_callback_data =
point_delete_callback_data;
}

void PR_SetHorizonMoveCallback(picking_record,
     horizon_move_callback, horizon_move_callback_data)
PR_ *picking_record;
HorizFun horizon_move_callback;
caddr_t horizon_move_callback_data;
{
  picking_record->horizon_move_callback = horizon_move_callback;
  picking_record->horizon_move_callback_data =
horizon_move_callback_data;
}
void PR_HorizonCallback(picking_record, horizon_select_callback,
             horizon_select_callback_data, horizon_add_callback,
             horizon_add_callback_data, horizon_delete_callback,
             horizon_delete_callback_data)
PR_ *picking_record;
XtCallbackProc horizon_select_callback;
caddr_t horizon_select_callback_data;
XtCallbackProc horizon_add_callback;
caddr_t horizon_add_callback_data;
HorizDel horizon_delete_callback;
caddr_t horizon_delete_callback_data;
{
  picking_record->horizon_select_callback = horizon_select_callback;
  picking_record->horizon_select_callback_data =
horizon_select_callback_data;
  picking_record->horizon_add_callback = horizon_add_callback;
  picking_record->horizon_add_callback_data =
horizon_add_callback_data;
  picking_record->horizon_delete_callback = horizon_delete_callback;
  picking_record->horizon_delete_callback_data =
horizon_delete_callback_data;
}

void PR_SetSnapCallback(picking_record, snap_callback,
                               snap_callback_data)
PR_ *picking_record;
XtCallbackProc snap_callback;
caddr_t snap_callback_data;
{
  picking_record->snap_callback = snap_callback;
  picking_record->snap_callback_data = snap_callback_data;
}

void PR_SetTrackerCallback(picking_record, tracker_callback,
                                   tracker_callback_data)
PR_ *picking_record;
TrackFun tracker_callback;
caddr_t tracker_callback_data;
{
  picking_record->tracker_callback = tracker_callback;
  picking_record->tracker_callback_data = tracker_callback_data;
}




void ErsHorizonDestroy(PR_ *pikrec, ErsHorizon **hlist)
{ register int i;

  i = 0;
  while(hlist[i] != NULL)
   {ErsHorizonSegmentDestroy(pikrec,hlist[i]);
    i ++;
   }
}

void ErsHorizonSegmentDestroy(PR_ *picking_record,
                              ErsHorizon *horizon)
{
  ErsPoint *point, *npoint;
  ErsPoint min_point, max_point;
  int horizon_number;
  register int i;

  for (i=0; i<picking_record->horizon_count; i++)
    if (horizon == picking_record->horizon_list[i]) break;

  if (i == picking_record->horizon_count)
    { XtError("ErsHorizonSegmentDestroy: horizon is not in list");
      return;
    }

  horizon_number = i+1;

  for (i=horizon_number; i<picking_record->horizon_count; i++)
    picking_record->horizon_list[i-1] = picking_record->horizon_list[i];

  picking_record->horizon_count--;

  if (picking_record->horizon_count != 0 &&
      horizon == picking_record->current_horizon)
    {if (horizon_number <= picking_record->horizon_count)
        picking_record->current_horizon =
        picking_record->horizon_list[horizon_number-1];
     else
        picking_record->current_horizon =
        picking_record->horizon_list[horizon_number-2];
     /* paint new selected horizon */
     RedrawHorizonBox(picking_record, picking_record->current_horizon);
    }

  /* clear area where horizon was */
  RedrawHorizonBox(picking_record, horizon);

  if (picking_record->horizon_count == 0)
   {free(picking_record->horizon_list);
    picking_record->horizon_list = NULL;
   }

  /* free color cell allocated */
#ifdef NO_OPEN_WINDOWS
  if (horizon->color_name != NULL)
    XFreeColors(picking_record->display, picking_record->colormap,
                &horizon->pixel, 1, 0);
#endif

  ErsHorizonSegmentFree(horizon);
}

static void ErsHorizonSetGC(PR_ *picking_record,
       ErsHorizon *horizon)
{ XGCValues values;

  values.line_width = horizon->line_width;
  values.foreground = horizon->pixel;
  XChangeGC(picking_record->display, picking_record->gc,
            GCLineWidth | GCForeground, &values);

  if (horizon == picking_record->current_horizon &&
      picking_record->selected_pixel != ErsHORIZON_PIXEL)
  values.foreground = picking_record->selected_pixel;
  XChangeGC(picking_record->display, picking_record->gc_symbol,
            GCLineWidth | GCForeground, &values);

}

void ErsHorizonSetPixel(picking_record, horizon, pixel)
PR_ *picking_record;
ErsHorizon *horizon;
Pixel pixel;
{
  Widget widget;
  register int i;

  if (horizon->color_name != NULL)
   {free(horizon->color_name);
    horizon->color_name = NULL;
#ifdef NO_OPEN_WINDOWS
    /* free old pixel (it crashes under OpenWindows) */
    XFreeColors(picking_record->display, picking_record->colormap,
                &(horizon->pixel), 1, 0);
#endif
   }

  horizon->pixel = pixel;

  /* redraw horizon */
  for (i=0; i<picking_record->widget_count; i++)
   {widget = picking_record->widget_rec[i].widget;
    ErsHorizonDraw(picking_record, horizon, widget, 0, 0,
                  XutilWidgetWidth(widget),
XutilWidgetHeight(widget));
   }
}

void ErsHorizonChangeAttributes(PR_ *picking_record,
      ErsHorizon *horizon, char *horizon_name, char *color_name,
      int line_width)
 /* horizon_name=NULL means don't change name */
{ ErsHorizon  *slist[99];
  XColor cell, rgb_cell;
  int i,status, nseg;
  char old_name[32];


  if (horizon_name != NULL)
    {if (horizon->horizon_name != NULL)
       { strcpy(old_name,horizon->horizon_name);
          free(horizon->horizon_name); }
     horizon->horizon_name = (char *) malloc(strlen(horizon_name)+1);
     strcpy(horizon->horizon_name,horizon_name);
    }

  horizon->line_width = line_width;

  if (color_name != NULL && strcmp(color_name, horizon->color_name))
    { /* free old pixel & allocate new color */
      status = XAllocNamedColor(picking_record->display,
               picking_record->colormap, color_name, &cell,
&rgb_cell);
      if (status != 0)
        {
#ifdef NO_OPEN_WINDOWS
         /* free old pixel */
         if (horizon->color_name != NULL) /*otherwise pixel assigned
by user */
         XFreeColors(picking_record->display,
picking_record->colormap,
                     &(horizon->pixel), 1, 0);
#endif
         horizon->pixel = cell.pixel;

         /* save new colorname in horizon structure */
         free(horizon->color_name);
         horizon->color_name = (char *) malloc(strlen(color_name)+1);
         strcpy(horizon->color_name,color_name);
        }
    }

/*
 * Find all segments with name = old_name
 */
  ErsHorizonGetSegments(picking_record,old_name, &nseg, slist);
  for(i=0;i<nseg;i++)
    { slist[i]->line_width = horizon->line_width;
      slist[i]->pixel      = horizon->pixel;
      if(slist[i]->color_name != NULL)
        free(slist[i]->color_name);
      slist[i]->color_name = (char *)
malloc(strlen(horizon->color_name)+1);
      strcpy(slist[i]->color_name,horizon->color_name);
      if (slist[i]->horizon_name != NULL)
        free(slist[i]->horizon_name);
      slist[i]->horizon_name = (char *)
malloc(strlen(horizon_name)+1);
      strcpy(slist[i]->horizon_name,horizon_name);
    }
  /* redraw everything */
  PR_Draw(picking_record, NULL, NULL);
}

void ErsSegmentChangeAttributes(PR_ *picking_record,
      ErsHorizon *horizon, char *horizon_name, char *color_name,
      int line_width)
 /* horizon_name=NULL means don't change name */
{ XColor      cell, rgb_cell;
  int         i,status, nseg,old_width;
  char        old_name[32],old_cname[16];


  if (horizon_name != NULL)
    {if (horizon->horizon_name != NULL)
       { strcpy(old_name,horizon->horizon_name);
          free(horizon->horizon_name); }
     horizon->horizon_name = (char *) malloc(strlen(horizon_name)+1);
     strcpy(horizon->horizon_name,horizon_name);
    }

  old_width = horizon->line_width;
  horizon->line_width = line_width;

  if (color_name != NULL && strcmp(color_name, horizon->color_name))
    { /* free old pixel & allocate new color */
      status = XAllocNamedColor(picking_record->display,
               picking_record->colormap, color_name, &cell,
&rgb_cell);
      if (status != 0)
        {
#ifdef NO_OPEN_WINDOWS
         /* free old pixel */
         if (horizon->color_name != NULL) /*otherwise pixel assigned
by user */
         XFreeColors(picking_record->display,
picking_record->colormap,
                     &(horizon->pixel), 1, 0);
#endif
         horizon->pixel = cell.pixel;

         /* save new colorname in horizon structure */
         free(horizon->color_name);
         horizon->color_name = (char *) malloc(strlen(color_name)+1);
         strcpy(horizon->color_name,color_name);
        }
    }

  /* redraw everything */
  PR_Draw(picking_record, NULL, NULL);
}

void ErsHorizonSetSymbolSize(picking_record, horizon, symbol_size)
PR_ *picking_record;
ErsHorizon *horizon;
int symbol_size;
{
  picking_record->default_symbol_size = symbol_size;

  if (horizon == NULL || horizon->symbol_size == symbol_size) return;

  horizon->symbol_size = symbol_size;
  if (horizon->number_of_points == 0) return;

  /* redraw */
  PR_Draw(picking_record, NULL, NULL);
}

/***************************************************************************/
/*                                                                         */
/* - PR_SetCurrentHorizon -                                   */
/*  Function used to set which horizon is currently selected.              */
/*                                                                         */
/***************************************************************************/
void PR_SetCurrentHorizon(picking_record, horizon)
PR_ *picking_record;
ErsHorizon *horizon;
{ ErsHorizon *previous_current_horizon;
  Widget widget;
  Bool    redraw_all = True;
  Bool    redraw_previous = True;
  register int i;

  if (horizon == picking_record->current_horizon) return;
  if (picking_record->current_horizon == NULL) redraw_previous = False;

  previous_current_horizon = picking_record->current_horizon;
  picking_record->current_horizon = horizon;
  horizon->line_width = SELECTED_LINE_WIDTH;

  if (redraw_previous)
    RedrawHorizonBox(picking_record, previous_current_horizon);

  /* redraw newly selected horizon */
  if (horizon != NULL)
   {if (horizon->symbol_size <= picking_record->selected_symbol_size)
      { /* we can just redraw on top */
       for (i=0; i<picking_record->widget_count; i++)
          { widget = picking_record->widget_rec[i].widget;
            ErsHorizonDraw(picking_record, horizon, widget, 0, 0,
                     XutilWidgetWidth(widget), XutilWidgetHeight(widget));
          }
       }
    else
       { /* erase and redraw series */
         RedrawHorizonBox(picking_record, horizon);
       }

   }
}

void PR_SetCurrentHorizonSymbolSize(picking_record,
symbol_size)
PR_ *picking_record;
int symbol_size;
{
  if (picking_record->selected_symbol_size == symbol_size) return;

  /* update picking structure with new size */
  picking_record->selected_symbol_size = symbol_size;

  /* if no selected horizon or only one point in selected horizon
return
*/
  if (picking_record->current_horizon == NULL ||
      picking_record->current_horizon->number_of_points <= 1) return;

  /* redraw */
  PR_Draw(picking_record, NULL, NULL);
}

void ErsHorizonDraw(picking_record, horizon, widget,  ex, ey, width,
height)
PR_ *picking_record;
ErsHorizon *horizon;
Widget widget;
int ex, ey;
int width, height;
{
  Display *display = picking_record->display;
  ErsPoint *point, *ppoint;
  Bool    ppoint_inside , cpoint_inside;
  Bool    ppoint_visible, point_visible;
  Bool    in_between;
  Bool    invert_direction = False;
  int px, py, cx, cy;
  int min_x, min_y;
  int max_x, max_y;
  long tn, tn1, tn2;
  long io;
  float pkey,pkey1;
  float time, time1, tmin,tmax;
  float left_pkey,right_pkey,minkey,maxkey;
  int x, y;
  register int i, k;

  if (!XtIsRealized(widget)) return;
  if (horizon == NULL) return;
  if (horizon->number_of_points == 0 || !horizon->horizon_active)
return;

  ErsSeismicLandRPKey(widget, &left_pkey, &right_pkey,
     &tmin, &tmax,picking_record->Phdr, &invert_direction);
  minkey = left_pkey;
  if(right_pkey < left_pkey) minkey = right_pkey;
  maxkey = right_pkey;
  if(right_pkey < left_pkey) maxkey = left_pkey;

  /* find if this horizon needs to be repainted, if the min or max */
  /* key is outside widget, skip test.                             */

  if (ErsPKeyTimeToPoint(widget, horizon->max_pkey,
                         horizon->max_time, &max_x, &max_y))
   {if (ey > max_y) return;
    if (!invert_direction) { if (ex > max_x) return; }
    else /* max_x is min in this case */
      {  if (ex + width < max_x) return; }
   }

  if (ErsPKeyTimeToPoint(widget, horizon->min_pkey,
                         horizon->min_time, &min_x, &min_y))
   {if (ey + height < min_y) return;
    if (!invert_direction) { if (ex + width < min_x) return; }
    else { /* min_x is max in this case */ if (ex > min_x) return; }
   }

  /* horizon needs to be repainted, we set GC */
  ErsHorizonSetGC(picking_record, horizon);

  point = horizon->first_point;
  ppoint_visible = ErsPKeyTimeToPoint(widget, point->pkey,
point->time,
                                             &px, &py);
  if(ppoint_visible)
    ppoint_inside = ErsPointInside(horizon, px, py, ex, ey, width,
height);
  else
    ppoint_inside = False;

  /* Draw the first point in the horizon */
  if (ppoint_inside)
    ErsPointDraw(picking_record, horizon, widget, px, py);

  for (i=1; i<horizon->number_of_points; i++)
   {ppoint = point;
    point  = point->next;
    point_visible = ErsPKeyTimeToPoint(widget, point->pkey,
point->time,
                                              &cx, &cy);
    if (point_visible)
      cpoint_inside = ErsPointInside(horizon, cx, cy, ex, ey, width,
height);
    else
      cpoint_inside = False;

    /* check if repaint area is between the 2 points */
    in_between = True;
    if (ppoint_visible && point_visible)
     {/* remove obvious cases */
      if (ex+width < px && ex+width < cx)
        in_between = False;
      else if (ex > px && ex > cx)
        in_between = False;
      else
       {
        if (point->npicks == 0)
         {if (ey+height < py && ey+height < cy)
            in_between = False;
          else if (ey > py && ey > cy)
           in_between = False;
         }
        else
         {/* we have to look for the min and max over all the pick
points */
          float min_time, max_time;
          int min_y, max_y;
          int gx;

          ErsPointTimeMinMax(point, &min_time, &max_time);
          if (min_time < tmin) min_time = tmin;
          if (max_time > tmax) max_time = tmax;
          if (min_time > ppoint->time)      min_time = ppoint->time;
          else if (max_time < ppoint->time) max_time = ppoint->time;
          ErsPKeyTimeToPoint(widget, point->pkey, min_time, &gx,
&min_y);
          ErsPKeyTimeToPoint(widget, point->pkey, max_time, &gx,
&max_y);
          if (ey+height < min_y)
            in_between = False;
          else if (ey > max_y)
           in_between = False;
         }
       }
     }
    else if (!ppoint_visible && !point_visible)
     {/* if two points are outside, assume line crosses section and */
     /* remove obvious cases where it does not.                    */
      if (ppoint->pkey < minkey && point->pkey < minkey )
        in_between = False;
      else if (ppoint->pkey > maxkey && point->pkey > maxkey)
        in_between = False;
      else
       {if (point->npicks == 0)
         {if (ppoint->time > tmax && point->time > tmax)
            in_between = False;
          else if (ppoint->time < tmin && point->time < tmin)
            in_between = False;
         }
        else
         {float min_time, max_time;
          ErsPointTimeMinMax(point, &min_time, &max_time);
          if (ppoint->time > tmax && min_time > tmax)
            in_between = False;
          else if (ppoint->time < tmin && max_time < tmin)
            in_between = False;
         }
       }
     }

    /* Draw if 1 or more points in or line crosses the draw area */
    if (ppoint_inside || cpoint_inside || in_between)
     {if (!ppoint_visible && point->npicks == 0)
       {/* Find point where line crosses the boundarys.      */
        /* ppoint is outside and point is inside.            */
        pkey1 = ppoint->pkey;
        time1 = ppoint->time;
/*
        if ((pkey1-left_pkey)*(point->pkey-left_pkey) <=0 )
          pkey = left_pkey;
        else
          pkey = right_pkey;
*/
        if (fabs(pkey1-right_pkey) < fabs(pkey1-left_pkey) )
          pkey = right_pkey;
        else
          pkey = left_pkey;
        time = time1 + (point->time - time1) *
               ((pkey - pkey1) / (float) (point->pkey - pkey1));
        if (!ErsPKeyTimeToPoint(widget, pkey, time, &px, &py))
         {if (time1 > tmax) time = tmax;
          else time = tmin;
          pkey = pkey1 + ((time - time1)/(point->time - time1)) *
                 ( point->pkey - pkey1);
          if (!ErsPKeyTimeToPoint(widget, pkey, time, &px, &py))
           {/* segment doesn't cross section after all */
            goto next;
           }
         }
       }
      if (!point_visible && point->npicks == 0)
       {/* find intersecting point */
        pkey1 = point->pkey;
        time1 = point->time;
/*
        if ((pkey1-right_pkey)*(ppoint->pkey-right_pkey) <=0 )
          pkey = right_pkey;
        else
          pkey = left_pkey;
*/
        if (fabs(pkey1-right_pkey) < fabs(pkey1-left_pkey) )
          pkey = right_pkey;
        else
          pkey = left_pkey;
        time = time1 + (ppoint->time - time1) *
               ((pkey - pkey1) / (ppoint->pkey - pkey1));
        if (!ErsPKeyTimeToPoint(widget, pkey, time, &cx, &cy))
         {if (time1 > tmax) time = tmax;
          else time = tmin;
          pkey = pkey1 + ((time - time1)/(ppoint->time - time1)) *
                 (ppoint->pkey - pkey1);
          if (!ErsPKeyTimeToPoint(widget, pkey, time, &cx, &cy))
            XtError("error in logic to find intersection case2\n");
         }
       }

      if (point->npicks != 0)
       {for (k=0; k<point->npicks; k++)
         {if (ErsPKeyTimeToPoint(widget, point->pick_list[k].pkey,
                           point->pick_list[k].time, &x, &y))
            { if (px!=x || py!=y)
              XDrawLine(display, XtWindow(widget), picking_record->gc,
px, py,
                        x, y);
              px = x;
              py = y;
            }
         }
        if (point_visible)
          XDrawLine(display, XtWindow(widget), picking_record->gc,
                    px, py, cx, cy);
       }
      else
       {if (horizon->line_width != 0)
          XDrawLine(display, XtWindow(widget), picking_record->gc,
                    px, py, cx, cy);
       }
     }

    if (cpoint_inside) ErsPointDraw(picking_record, horizon, widget,
cx, cy);
next:
    px = cx;
    py = cy;
    ppoint_inside  = cpoint_inside;
    ppoint_visible = point_visible;
  }
}

/*******************************************************************/
/*                                                                 */
/* ErsRedrawBoundingBox                                            */
/*                                                                 */
/* horizon is only used when draw_seismic_flag is False so that we */
/* only repaint this horizon. We only redraw the seismic traces   */
/* under if draw_seismic_flag is True                              */
/*                                                                 */
/*******************************************************************/

void ErsRedrawBoundingBox(picking_record, horizon, point1, point2,
point3,
                     draw_horizon_flag, draw_seismic_flag)
PR_ *picking_record;
ErsHorizon *horizon;
ErsPoint *point1, *point2, *point3;
Bool    draw_horizon_flag;
Bool    draw_seismic_flag;
{
  Widget widget;
  Bool    p1_visible, p2_visible, p3_visible;
  int x1, y1;
  int x2, y2;
  int x3, y3;
  int min_x, min_y;
  int max_x, max_y;
  int width, height;
  int line_width;
  float min_time, max_time;
  register int i, k;

  if(picking_record->display == NULL) return;
  if (point1 == NULL) point1 = point2;
  if (point3 == NULL) point3 = point2;
  if (point2 == NULL) XtError("ErsRedrawBoundingBox: point2 is NULL");

  /* calculate maximum line width */
  line_width = CalculateMaxLineWidth(picking_record);

  /* check if points have picks attached and calculate a min_time and
max_time*/
  min_time = point1->time;
  if (point2->time < min_time) min_time = point2->time;
  max_time = point1->time;
  if (point2->time > max_time) max_time = point2->time;
  if (point1->npicks != 0)
   {for (i=0; i<point1->npicks; i++)
     {if (point1->pick_list[i].time < min_time)
        min_time = point1->pick_list[i].time;
      if (point1->pick_list[i].time > max_time)
        max_time = point1->pick_list[i].time;
     }
   }
  if (point2->npicks != 0)
   {for (i=0; i<point2->npicks; i++)
     {if (point2->pick_list[i].time < min_time)
        min_time = point2->pick_list[i].time;
      if (point2->pick_list[i].time > max_time)
        max_time = point2->pick_list[i].time;
     }
   }
  if (point3->npicks != 0)
   {for (i=0; i<point3->npicks; i++)
     {if (point3->pick_list[i].time < min_time)
        min_time = point3->pick_list[i].time;
      if (point3->pick_list[i].time > max_time)
        max_time = point3->pick_list[i].time;
     }
   }

  for (i=0; i<picking_record->widget_count; i++)
   {widget = picking_record->widget_rec[i].widget;
    p1_visible = ErsPKeyTimeToPoint(widget, point1->pkey, min_time,
                                           &x1, &y1);
    p2_visible = ErsPKeyTimeToPoint(widget, point2->pkey, max_time,
                                           &x2, &y2);
    p3_visible = ErsPKeyTimeToPoint(widget, point3->pkey, point3->time,
                                           &x3, &y3);

    if (p1_visible && p2_visible && p3_visible)
     {min_x = x1;
      if (x2 < min_x) min_x = x2;
      if (x3 < min_x) min_x = x3;

      max_x = x1;
      if (x2 > max_x) max_x = x2;
      if (x3 > max_x) max_x = x3;

      min_y = y1;
      if (y2 < min_y) min_y = y2;
      if (y3 < min_y) min_y = y3;

      max_y = y1;
      if (y2 > max_y) max_y = y2;
      if (y3 > max_y) max_y = y3;

      /* adjust for point size */
      min_x -= line_width;
      max_x += line_width;
      min_y -= line_width;
      max_y += line_width;
      if (min_x < 0) min_x = 0;
      if (min_y < 0) min_y = 0;

      width = max_x - min_x;
      height = max_y - min_y;
     }
    else if (p1_visible || p2_visible || p3_visible)
     {/* be safe and make sure we erase the connection with the point */
      /* that is not visible in the section.  */
      min_x = 0;
      min_y = 0;
      width = XutilWidgetWidth(widget);
      height = XutilWidgetHeight(widget);

     }
    else
     {/* all points are outside */
      min_x = 0;
      min_y = 0;
      width = 0;
      height = 0;
     }

    if (width !=0 && height != 0)
     {if (draw_seismic_flag)
        ErsSeismicDraw(picking_record->widget_rec[i].widget, min_x, min_y,
                       width, height, True);
      else
        ErsHorizonDraw(picking_record, horizon, widget,  min_x, min_y,
                      width, height);
     }
   }
}

static void RedrawHorizonBox(picking_record, horizon)
PR_ *picking_record;
ErsHorizon *horizon;
{
  Widget widget;
  ErsPoint min_point, max_point;
  int line_width;
  int x1, x2;
  int y1, y2;
  int min_x, max_x;
  int min_y, max_y;
  Bool    p1_visible, p2_visible, RtoL;
  register int i;

  if(picking_record->display == NULL) return;
  line_width = CalculateMaxLineWidth(picking_record);

  for (i=0; i<picking_record->widget_count; i++) {
    widget = (Widget) picking_record->widget_rec[i].widget;
    p1_visible = ErsPKeyTimeToPoint(widget, horizon->min_pkey,
                              horizon->min_time, &x1, &y1);
    p2_visible = ErsPKeyTimeToPoint(widget, horizon->max_pkey,
                              horizon->max_time, &x2, &y2);

    min_x = 0;
    min_y= 0;
    max_x = XtWidth(widget)-1;
    max_y = XtHeight(widget)-1;

    ErsSeismicRtoL(widget, &RtoL);
    if (!RtoL)
     {if (p1_visible) {
        min_x = x1-line_width;
        min_y = y1-line_width;
      }
      if (p2_visible) {
        max_x = x2+line_width;
        max_y = y2+line_width;
      }
     }
    else
     {if (p1_visible) {
        max_x = x1+line_width;
        min_y = y1-line_width;
      }
      if (p2_visible) {
        min_x = x2-line_width;
        max_y = y2+line_width;
      }
     }

    if (min_x < 0) min_x = 0;
    if (min_y < 0) min_y = 0;

    ErsSeismicDraw(picking_record->widget_rec[i].widget, min_x, min_y,
                 max_x-min_x+1, max_y-min_y+1, True);
  }
}

/**********************************************/
/*  Functions that operate on picking record  */
/**********************************************/
/********************************************************
 * void ErsDestroyCallback()                          ***
 * Was added as an destroy callback to the widget.    ***
 * Just calls PR_DisconnectWidget()      ***
 *******************************************************/
static void ErsDestroyCallback(widget, picking_record)
Widget widget;
PR_ *picking_record;
{ PR_DisconnectWidget(picking_record, widget); }


/********************************************************
 * void ErsDrawCallback()                             ***
 * Was added as an expose callback to the widget.     ***
 * Just calls PR_Draw()                  ***
 *******************************************************/
static void ErsDrawCallback(widget, picking_record, cb)
Widget widget;
PR_ *picking_record;
XmAnyCallbackStruct *cb;
{ PR_Draw(picking_record, widget, cb->event); }


/********************************************************
 * void PR_Draw()                        ***
 * Will redraw the seismic data and the horizons.     ***
 * May call ErsSeismicDraw() and/or ErsHorizonDraw(). ***
 *******************************************************/
void PR_Draw(picking_record, widget, event)
PR_ *picking_record;
Widget widget;
XEvent *event;
{ register int i, k;

  if (widget == NULL)
   { /* Redraw for all widgets tied to the picking record */
    for (k=0; k<picking_record->widget_count; k++)
      { widget = picking_record->widget_rec[k].widget;
        if (event == NULL) /* If event is NULL, we redraw the seismic*/
         { ErsSeismicDraw(widget, 0, 0, XutilWidgetWidth(widget),
                           XutilWidgetHeight(widget), True);
          }
        for (i=0; i<picking_record->horizon_count; i++)
          { if (event == NULL)
             { ErsHorizonDraw(picking_record, picking_record->horizon_list[i],
               widget, 0, 0, XutilWidgetWidth(widget),
               XutilWidgetHeight(widget));
             }
            else
             { ErsHorizonDraw(picking_record, picking_record->horizon_list[i],
                          widget, event->xexpose.x, event->xexpose.y,
                          event->xexpose.width, event->xexpose.height);
            }
          }
      }
   }
  else
   { /* Redraw for a particular widget */
     /* If event is NULL, we redraw and leave: callbacks will be activated */
     /* and we will come back here anyway because the expose flag is true  */
    if (event == NULL)
      { ErsSeismicDraw(widget, 0, 0, XutilWidgetWidth(widget),
                       XutilWidgetHeight(widget), True);
        return;
      }
    for (i=0; i<picking_record->horizon_count; i++)
      { if (event == NULL)
          { ErsHorizonDraw(picking_record, picking_record->horizon_list[i],
            widget, 0, 0, XutilWidgetWidth(widget),
            XutilWidgetHeight(widget)) ; }
        else
          { ErsHorizonDraw(picking_record, picking_record->horizon_list[i],
                       widget, event->xexpose.x, event->xexpose.y,
                       event->xexpose.width, event->xexpose.height);
          }
      }
   }
}

void ErsWidgetSetTranslations(widget, translations, mode)
Widget widget;
XtTranslations translations;
int mode;
{
  if (translations == NULL || widget == NULL) return;

  if (mode == ErsOVERRIDE_TRANSLATIONS)
    XtOverrideTranslations(widget, translations);
  else if (mode == ErsAUGMENT_TRANSLATIONS)
    XtAugmentTranslations(widget, translations);
  else if (mode == ErsREPLACE_TRANSLATIONS) {
    XtUninstallTranslations(widget);
    XtOverrideTranslations(widget, translations);
  }
}

static void ErsHorizonUpdateMinMax(ErsHorizon *horizon)
{ ErsPoint *point;

  if ((point = horizon->first_point) == NULL) return;

  if(point->tn != UNDEFINED_TN)
   {horizon->max_tn = point->tn;
    horizon->min_tn = point->tn;}
  horizon->max_time = point->time;
  horizon->min_time = point->time;
  horizon->max_pkey = point->pkey;
  horizon->min_pkey = point->pkey;

  point = point->next;

  while (point != NULL) {
    if(point->tn != UNDEFINED_TN)
     {if (point->tn > horizon->max_tn) horizon->max_tn = point->tn;
      if (point->tn < horizon->min_tn) horizon->min_tn = point->tn;}
    if (point->time > horizon->max_time) horizon->max_time =
point->time;
    if (point->time < horizon->min_time) horizon->min_time =
point->time;
    if (point->pkey > horizon->max_pkey) horizon->max_pkey =
point->pkey;
    if (point->pkey < horizon->min_pkey) horizon->min_pkey =
point->pkey;
    point = point->next;
  }
}

static int CalculateMaxLineWidth(picking_record)
PR_ *picking_record;
{
  int line_width = 0;
  register int i;

  /* calculate maximum of line width and symbol size */
  for (i=0; i<picking_record->horizon_count; i++) {
    if (picking_record->horizon_list[i]->line_width > line_width)
      line_width = picking_record->horizon_list[i]->line_width;
    if (picking_record->horizon_list[i]->symbol_size > line_width)
      line_width = picking_record->horizon_list[i]->symbol_size;
  }

  if (picking_record->selected_symbol_size > line_width)
    line_width = picking_record->selected_symbol_size;
  if (line_width < 3) line_width = 3;

  return(line_width);
}

static Bool    ErsPointInside(horizon, px, py, ex, ey, width, height)
ErsHorizon *horizon;
int px, py;
int ex, ey;
int width, height;
{
  if (px >= ex && px <= ex+width &&
      py >= ey && py <= ey+height) return(True);

  return(False);
}

static void ErsPointTimeMinMax(point, min_time, max_time)
ErsPoint *point;
float *min_time, *max_time;
{
  register float Min_t, Max_t;
  register int i;

  *min_time = point->time;
  *max_time = point->time;
  if (point->npicks == 0) return;

  Min_t = *min_time;
  Max_t = *max_time;
  for (i=0; i<point->npicks; i++)
    { if (point->pick_list[i].time < Min_t)
         Min_t = point->pick_list[i].time;
      else if (point->pick_list[i].time > Max_t)
         Max_t = point->pick_list[i].time;
    }
  *min_time = Min_t;
  *max_time = Max_t;
}

void ErsPointDelete(picking_record, horizon, point_to_delete)
PR_ *picking_record;
ErsHorizon *horizon;
ErsPoint *point_to_delete;
{
  if (point_to_delete != NULL) {
    /* remove the point */
    ErsPointRemove(horizon, point_to_delete);
    /* repaint area */
    ErsRedrawBoundingBox(picking_record, NULL, point_to_delete->previous,
                         point_to_delete, point_to_delete->next, True, True);
    /* we free the point structure */
    ErsPointFree(point_to_delete);
  }
}

void ErsPointInsert(picking_record, horizon, point, insertion_mode)
PR_ *picking_record;
ErsHorizon *horizon;
ErsPoint *point;
int insertion_mode;
{
  ErsPoint *wpoint, *ppt,*npt;
  Widget   widget;
  int      x, y;
  register int i;

  /* Add point to the proper point in the linked list */
  /* Update the horizon structure */
  ErsPointAdd(picking_record, horizon, point, insertion_mode);

  /* Now proceed to the drawing */
  if (horizon->first_point == NULL)
    {/* This is the 1st point */
     if(insertion_mode != ErsSILENT)
       ErsPointDrawAll(picking_record, horizon, point);
     return;
    }

  if (insertion_mode == ErsAPPEND || insertion_mode == ErsAUTO_PICK ||
      insertion_mode == ErsSILENT)
    { if(insertion_mode != ErsSILENT)
      ErsRedrawBoundingBox(picking_record, horizon, point->previous,
                           point, NULL, True, False);
    }
  else if (insertion_mode == ErsFIRST)
    { ErsRedrawBoundingBox(picking_record, horizon, NULL, point, point->next,
                          True, False);
    }
  else if (insertion_mode == ErsINSERT)
    { ErsRedrawBoundingBox(picking_record, NULL, point->previous, point,
                         point->next, True, True);
    }
  else if (insertion_mode == ErsINSERT_TIME)
    { ErsRedrawBoundingBox(picking_record, NULL, point->previous, point,
                          point->next, True, True);
    }
 return;
}

void ErsPointDraw(picking_record, horizon, widget, x, y)
PR_ *picking_record;
ErsHorizon *horizon;
Widget widget;
int x, y;
{
  register int d, l;

  if (horizon != picking_record->current_horizon)
    {if ((l = horizon->symbol_size) == 0) return; }
  else
    { if ((l = picking_record->selected_symbol_size) == 0) return; }

  if (l == ErsSYMBOL_AUTO_SIZED)
   {/* calculate size ourselves */
    if (horizon->line_width >= 3)
     { d = horizon->line_width;
       l = (d << 1) + 1;
     }
    else
     {d = 2;
      l = 5;
     }
   }
  else
   { d = (l >> 1); }

  XFillRectangle(XtDisplay(widget), XtWindow(widget),
                   picking_record->gc_symbol, x-d, y-d, l, l);
}

void ErsPointDrawAll(picking_record, horizon, point)
PR_ *picking_record;
ErsHorizon *horizon;
ErsPoint *point;
{
  int x, y;
  register int i;

  ErsHorizonSetGC(picking_record, horizon);

  for (i=0; i<picking_record->widget_count; i++)
   {ErsPKeyTimeToPoint(picking_record->widget_rec[i].widget,
       point->pkey, point->time, &x, &y);
    ErsPointDraw(picking_record, horizon,
       picking_record->widget_rec[i].widget, x, y);
   }
}

/**********************************************************************/
/*        translations that work with the seismic widget              */
/**********************************************************************/

/* global variables for rubberbanding */
static int      px, py;
static int      nx, ny;
static int      ox, oy;
static Bool     rubberband_flag = False;
static Bool     motion_flag;
static ErsPoint *rp, *rp1, *rp2;
static Bool     constraint_move_flag = False;

static void PointInsert(widget, event, insert_mode)
Widget widget;
XEvent *event;
char *insert_mode[];
{
  PR_ *picking_record;
  ErsPoint *point;
  float pkey, skey, tkey;
  long tn;
  float wx, time;

  if (insert_mode[0] == NULL) XtError(
   "You must specify an insert mode argument for the PointInsert translation.");

  picking_record = (PR_ *) ErsGetPickingRecord(widget);
  if (picking_record == NULL) return;

  if (picking_record->current_horizon == NULL) {
    printf("Please select a horizon before trying to insert a point.\n");
    return;
  }

  if (!ErsPointToKeyTime(widget, event->xbutton.x, event->xbutton.y,
                          &pkey, &skey, &tkey, &tn, &wx, &time)) {
    XBell(XtDisplay(widget), 0);
    return;
  }

  point = ErsPointCreate(time,tn,NULL,pkey,skey,tkey);

  /* if snap callback is defined, call it */
  if (picking_record->snap_callback != NULL)
    picking_record->snap_callback(widget, point, 
                                  picking_record->snap_callback_data);

  if (*insert_mode[0] == 'e')
    ErsPointInsert(picking_record, picking_record->current_horizon, point,
                   ErsAPPEND);
  else if (*insert_mode[0] == 'i')
    ErsPointInsert(picking_record, picking_record->current_horizon, point,
                   ErsINSERT);
  else if (*insert_mode[0] == 'b')
    ErsPointInsert(picking_record, picking_record->current_horizon, point,
                   ErsFIRST);
  else if (*insert_mode[0] == 't')
    ErsPointInsert(picking_record, picking_record->current_horizon, point,
                   ErsINSERT_TIME);
  else
   XtError(
     "Valid insert modes for the PointInsert action are (e), (i), (b), (t)");

  if (picking_record->point_add_callback != NULL)
    picking_record->point_add_callback(widget, 
                    picking_record->point_add_callback_data, point);
}

static void
PointStartMove(Widget widget,XEvent *event, char *move_mode[])
{ PR_ *picking_record;
  ErsHorizon *horizon;
  ErsPoint *point;
  Window   w;
  Display  *display;
  float  pkey, skey, tkey, wx;
  long   tn;
  float  time,*hd;
  float  sample_rate;
  int    frame,nhdrs,numv,Phdr;
  int ex, ey;
  float  p1,p2,xtol;

  if(move_mode[0] == NULL) return;
  picking_record = (PR_ *) ErsGetPickingRecord(widget);
  if (picking_record == NULL) return;

  horizon = picking_record->current_horizon;
  if (horizon == NULL) return;

  if (!ErsPointToKeyTime(widget, event->xbutton.x, event->xbutton.y,
                          &pkey, &skey, &tkey, &tn, &wx, &time)) return;

  if (horizon->first_point == NULL)
    {/* This is the 1st point */
     horizon->number_of_points++;
     point = ErsPointCreate(time,tn,NULL,pkey,skey,tkey);
     horizon->first_point = point;
     horizon->last_point = point;
     ErsPointDrawAll(picking_record, horizon, point);
     return;
    }

  motion_flag = False;
  if(*move_mode[0]=='m')
   {hd = NULL;
    motion_flag = True;
    ErsSeismicGethdDat(widget , &hd, &nhdrs, &numv);
    Phdr = SEQHDR;
    if(picking_record->Phdr != UNDEFINED_KEY) Phdr = picking_record->Phdr;
    xtol = 10.0;
    if(hd != NULL)
     { p1 = hd[Phdr-1];
       p2 = hd[Phdr-1 + nhdrs];
       xtol = p1-p2; /* difference between adjacent traces */
       xtol = (float) fabs((double) xtol);
       xtol = 4.0 *xtol;
     }
    ErsSeismicGetSampleRate(widget, &sample_rate);
    point = ErsPointFind(picking_record, horizon, pkey, time,
             xtol, 10.*sample_rate);
    if (point == NULL ) return;
    if (point->npicks > 0)
     { printf("PointStartMove: dont move autopick points\n");
       return; }
    rp1 = point->previous;
    rp2 = point->next;
   }
  else if(*move_mode[0]=='e')
   {point = ErsPointCreate(time,tn,NULL,pkey,skey,tkey);
    rp1   = horizon->last_point;
    rp2   = NULL;
    point->previous = horizon->last_point;
    point->user_data = point->previous->user_data;
    horizon->last_point->next = point;
    horizon->last_point = point;
   }
  else if(*move_mode[0]=='b')
   {point = ErsPointCreate(time,tn,NULL,pkey,skey,tkey);
    rp1   = NULL;
    rp2   = horizon->first_point;
    point->next = horizon->first_point;
    point->user_data = point->next->user_data;
    horizon->first_point->previous = point;
    horizon->first_point = point;
   }
  else if(*move_mode[0]=='i')
   {int nearn;
    float v1,v2,fac;
    point = ErsPointCreate(time,tn,NULL,pkey,skey,tkey);
    nearn = FindNNP(horizon,point,&rp1,&rp2);
    v1 = 0.0; v2 = 0.0; fac = 0.5;
    if(point->previous != NULL)
     {v1 = point->previous->user_data; fac = 1.0; }
    if(point->next     != NULL)
     {v2 = point->next->user_data; fac = 1.0; }
    point->user_data = fac*(v1 + v2);
   }
  else if(*move_mode[0]=='t')
   {int nearn;
    point = ErsPointCreate(time,tn,NULL,pkey,skey,tkey);
    nearn = FindNNP(horizon,point,&rp1,&rp2);
    point->user_data = 0.5*(point->previous->user_data +
                             point->next->user_data);
    rp1   = point->previous;
    rp2   = point->next;
   }
  else return;

  if(*move_mode[0] != 'm') horizon->number_of_points++;
  /* start rubberbanding */
  rubberband_flag = True;
  rp1 = point->previous;
  rp2 = point->next;
  rp  = point;

  if (rp1 != NULL)
   {if (!ErsPKeyTimeToPoint(widget, rp1->pkey, rp1->time, &px, &py))
      px = -1; }
  else
   { px = -1; }

  if (rp2 != NULL)
   { if (!ErsPKeyTimeToPoint(widget, rp2->pkey, rp2->time, &nx, &ny))
      nx = -1; }
  else
   { nx = -1; }

  ox = -1;

  /* set GC for rubber banding*/
  XSetFunction(XtDisplay(widget), picking_record->gc, GXinvert);
}

static void PointMove(widget, event, move_mode)
Widget widget;
XEvent *event;
char *move_mode[];
{
  PR_ *picking_record;
  Display *display;
  Window w;
  int ex, ey;

  if (!rubberband_flag) return;

  picking_record = (PR_ *) ErsGetPickingRecord(widget);
  if (picking_record == NULL) return;

  constraint_move_flag = False;
  display =  XtDisplay(widget);
  w = XtWindow(widget);

  /* draw the lines at old positions (except 1st time when ox=-1) */
  if (px != -1 && ox != -1)
   XDrawLine(display, w, picking_record->gc, px, py, ox, oy);
  if (nx != -1 && ox != -1) 
   XDrawLine(display, w, picking_record->gc, nx, ny, ox, oy);

  /* draw the lines at new positions */
  ex = event->xmotion.x;
  ey = event->xmotion.y;
  if (px != -1) XDrawLine(display, w, picking_record->gc, px, py, ex, ey);
  if (nx != -1) XDrawLine(display, w, picking_record->gc, nx, ny, ex, ey);

  ox = ex;
  oy = ey;
}

/****************************************************************************
 * - PointMoveConstraint -
 * Action to move a point along a trace.
 ***************************************************************************/
static void PointMoveConstraint(widget, event, move_mode)
Widget widget;
XEvent *event;
char *move_mode[];
{
  PR_ *picking_record;
  Display *display;
  Window w;
  int ex, ey;

  if (!rubberband_flag) return;

  picking_record = (PR_ *) ErsGetPickingRecord(widget);
  if (picking_record == NULL) return;

  constraint_move_flag = True;
  display =  XtDisplay(widget);
  w = XtWindow(widget);

  if (px != -1 && ox != -1) XDrawLine(display, w, picking_record->gc, px, py,
                                      ox, oy);
  if (nx != -1 && ox != -1) XDrawLine(display, w, picking_record->gc, nx, ny,
                                      ox, oy);

  if (ox == -1)
    ErsPKeyTimeToPoint(widget, rp->pkey, rp->time, &ox, &oy);

  ey = event->xmotion.y;
  if (px != -1) XDrawLine(display, w, picking_record->gc, px, py, ox, ey);
  if (nx != -1) XDrawLine(display, w, picking_record->gc, nx, ny, ox, ey);

  oy = ey;
}
 
static void PointEndMove(widget, event, move_mode)
Widget widget;
XEvent *event;
char *move_mode[];
{
  PR_ *picking_record;
  ErsPoint *point;
  ErsHorizon *horizon;
  Display *display;
  Window w;
  int x, y;
  float  pkey, skey, tkey, old_pkey, old_skey, old_tkey;
  long   tn, old_tn;
  float  wx, time, old_time;
  Bool    do_it,mflag;
  register int i;

  if (!rubberband_flag) return;
  rubberband_flag = False;
  mflag = motion_flag;
  motion_flag = False;
  picking_record = (PR_ *) ErsGetPickingRecord(widget);
  if (picking_record == NULL) return;

  /* Undraw the lines at the old positions */
  display =  XtDisplay(widget);
  w = XtWindow(widget);
  if (px != -1 && ox != -1)
   XDrawLine(display, w, picking_record->gc, px, py, ox, oy);
  if (nx != -1 && ox != -1)
   XDrawLine(display, w, picking_record->gc, nx, ny, ox, oy);
  /* Restore GC for normal drawing mode */
  XSetFunction(XtDisplay(widget), picking_record->gc, GXcopy);

  /* find new point key and time values */
  if (constraint_move_flag)
    x = ox;
  else
    x = event->xbutton.x;
  y = event->xbutton.y;

  if (!ErsPointToKeyTime(widget, event->xbutton.x, event->xbutton.y,
                         &pkey, &skey, &tkey, &tn, &wx, &time))
    { printf("PointEndMove: point is outside valid range\n");
      XBell(display, 0);
      return;
    }

  /* find which horizon point is connected to:rp=original point*/
  point = rp;
  while (point->next != NULL) point = point->next;
  for (i=0; i<picking_record->horizon_count; i++) 
    if (point == picking_record->horizon_list[i]->last_point) break;

  if (i == picking_record->horizon_count) 
   { printf("PointEndMove: can not find horizon\n"); }
  horizon = picking_record->horizon_list[i];

  /* Erase previous segment - Draw seismic but not horizon */
   horizon->horizon_active = False;
   ErsRedrawBoundingBox(picking_record, horizon, rp1, rp, rp2, False, True);
   horizon->horizon_active = True;

  /* Save old values & set new point values */
  old_time = rp->time;
  old_tn   = rp->tn;
  old_pkey = rp->pkey;
  old_skey = rp->skey;
  old_tkey = rp->tkey;
 
  rp->time = time;
  rp->tn   = tn;
  rp->pkey = pkey;
  rp->skey = skey;
  rp->tkey = tkey;

  /* if snap callback is defined, call it */
  if (picking_record->snap_callback != NULL)
    picking_record->snap_callback(widget, rp, 
                                  picking_record->snap_callback_data);

  /* call user callback if defined */
  do_it = True;
  if (picking_record->point_move_callback != NULL && mflag == True)
    picking_record->point_move_callback(widget,
                    picking_record->point_move_callback_data, rp, &do_it);

  /* Do we retain the move or the new point */
  if (do_it)
    {ErsHorizonUpdateMinMax(horizon);
     ErsRedrawBoundingBox(picking_record, horizon, rp1, rp, rp2, True, True);
    }
  else
    {/* restore old point values for a move*/
     rp->time = old_time;
     rp->tn   = old_tn;
     rp->pkey = old_pkey;
     rp->skey = old_skey;
     rp->tkey = old_tkey;
     if(mflag == False) ErsPointDelete(picking_record,horizon, rp);
     else
     ErsRedrawBoundingBox(picking_record, horizon, rp1, rp, rp2, True, True);
    }

}

ErsHorizon * ErsPointHorizonFind(picking_record, point)
PR_ *picking_record;
ErsPoint *point;
{
  register ErsPoint *tpoint;
  register int i;

  /* find which horizon point is connected to */
  tpoint = point;
  while (tpoint->next != NULL) tpoint = tpoint->next;
  for (i=0; i<picking_record->horizon_count; i++)
    if (tpoint == picking_record->horizon_list[i]->last_point) break;

  if (i == picking_record->horizon_count)
    XtError("- ErsPointHorizonFind - invalid point");

  return (picking_record->horizon_list[i]);
}

static void PointDelete(widget, event)
Widget widget;
XEvent *event;
{
  PR_ *picking_record;
  ErsHorizon *horizon;
  ErsPoint *point_to_delete;
  float  pkey, skey, tkey;
  long   tn;
  float  wx, time;
  int    Phdr,nhdrs,numv;
  float  sample_rate,xtol,*hd,p1,p2;
  Bool    delete_flag = True;

  picking_record = (PR_ *) ErsGetPickingRecord(widget);
  if (picking_record == NULL) return;

  horizon = picking_record->current_horizon;
  if (horizon == NULL) return;

  if (!ErsPointToKeyTime(widget, event->xbutton.x, event->xbutton.y,
                          &pkey, &skey, &tkey, &tn, &wx, &time)) return;

  ErsSeismicGetSampleRate(widget, &sample_rate);

  hd = NULL;
  ErsSeismicGethdDat(widget , &hd, &nhdrs, &numv);
  Phdr = SEQHDR;
  if(picking_record->Phdr != UNDEFINED_KEY) Phdr = picking_record->Phdr;
  xtol = 10.0;
  if(hd != NULL)
   { p1 = hd[Phdr-1];
     p2 = hd[Phdr-1 + nhdrs];
     xtol = p1-p2; /* difference between adjacent traces */
     xtol = (float) fabs((double) xtol);
     xtol = 4.0 *xtol;
   }
  point_to_delete = ErsPointFind(picking_record, horizon, pkey, time,
                                 xtol, 10. * sample_rate);

  if (picking_record->point_delete_callback != NULL)
    picking_record->point_delete_callback(widget, 
                    picking_record->point_delete_callback_data,
                    point_to_delete,&delete_flag);

  if (delete_flag)
    ErsPointDelete(picking_record, horizon, point_to_delete);
}
                  
static void PointSelect(widget, event)
Widget widget;
XEvent *event;
{
  PR_ *picking_record;
  ErsHorizon *horizon;
  ErsPoint *point;
  float  pkey, skey, tkey;
  long   tn;
  float  wx, time;
  int    Phdr,nhdrs,numv;
  float  sample_rate,xtol,*hd,p1,p2;

  if (!ErsPointToKeyTime(widget, event->xbutton.x, event->xbutton.y,
                          &pkey, &skey, &tkey, &tn, &wx, &time)) return;

  picking_record = (PR_ *) ErsGetPickingRecord(widget);
  if (picking_record == NULL) return;

  ErsSeismicGetSampleRate(widget, &sample_rate);
  hd = NULL;
  ErsSeismicGethdDat(widget , &hd, &nhdrs, &numv);
  Phdr = SEQHDR;
  if(picking_record->Phdr != UNDEFINED_KEY) Phdr = picking_record->Phdr;
  xtol = 10.0;
  if(hd != NULL)
   { p1 = hd[Phdr-1];
     p2 = hd[Phdr-1 + nhdrs];
     xtol = p1-p2; /* difference between adjacent traces */
     xtol = (float) fabs((double) xtol);
     xtol = 4.0 *xtol;
   }
  horizon = picking_record->current_horizon;
  point = ErsPointFind(picking_record, horizon, pkey, time,
          xtol, 10. * sample_rate);

  if (point == NULL) return;

  if (picking_record->point_select_callback == NULL)
    XtError("callback for PointSelect callback must be registered");

  /* we call callback */
  picking_record->point_select_callback(widget, 
                  picking_record->point_select_callback_data, point);
}

/*************************************************************
 * Save auto-track picks in a pick_list structure.         ***
 * The pick_list structure will contain the picks on the   ***
 * traces in between the two control points.               ***
 ************************************************************/
static void AutoPick(widget, event)
Widget widget;
XEvent *event;
{
  PR_ *picking_record;
  ErsHorizon   *horizon;
  ErsPoint     *point;
  ErsPointList *pick_list;
  float        pkey, skey, tkey,pkey2;
  long         tn,tn1,tn2,indexs,indexe;
  float        wx, time;
  int          npicks, Phdr;
  register int i;

  /* Convert mouse (x,y) to (trace number & pkey,time)  */
  if (!ErsPointToKeyTime(widget, event->xbutton.x, event->xbutton.y,
                          &pkey, &skey, &tkey, &tn, &wx, &time)) return;

  picking_record = (PR_ *) ErsGetPickingRecord(widget);
  if (picking_record == NULL) return;

  if (picking_record->tracker_callback == NULL)
   { printf("AutoPick: No tracker callback specified\n");
     return;
   }

  /* Make sure current horizon is defined and tn is greater than */
  /* tn value of last point.                                     */
  if (picking_record->current_horizon == NULL)
   {printf("AutoPick: No current horizon for auto tracking\n");
    return;
   }

  Phdr = picking_record->Phdr;
  if(Phdr == UNDEFINED_KEY)
   {printf("AutoPick: No primary key specified for auto tracking\n");
    return;
   }

  horizon = picking_record->current_horizon;
  if (horizon->last_point != NULL ) /* pick a direction and stick with it */
   { tn1 = horizon->last_point->tn;
     tn2 = tn1;
     if(horizon->last_point->previous != NULL)
       tn2 = horizon->last_point->previous->tn;
     if((tn-tn1)*(tn1-tn2) < 0)
      { XBell(picking_record->display, 0);
        return; }
   }

  /* Create a point structure for the control point*/
  point = ErsPointCreate(time,tn,NULL,pkey,skey,tkey);

  /* Allocate memory for picks in between the control points */
  if (horizon->last_point != NULL)
    { /* Figure number of traces & allocate space to save auto-picks */ 
      pkey2 = horizon->last_point->pkey;
      ErsSeismicTellSelectionP(widget,pkey,pkey2,Phdr,&indexs,&indexe);
      npicks = (int) labs(indexe-indexs) - 1;
      if(npicks < 0) npicks=0;
      pick_list = NULL;
      pick_list = (ErsPointList *) calloc(1,npicks * sizeof(ErsPointList));
    }
  else
    { /* Initialize, for the first pick in the horizon */
      pick_list = NULL;
      npicks = 0;
    }

  /* Connect point to the end of the horizon */
  point->previous = horizon->last_point; /* for tracker callback */

  /* Let the tracker callback fill in the pick_list */
  picking_record->tracker_callback(widget, point, pick_list, npicks,
                  picking_record->tracker_callback_data);

  /* Attach the pick_list to the Point structure  */
  if (npicks != 0) ErsPointAddPickList(horizon, point, pick_list, npicks);

  /* Free up the memory attached to the temporary pick_list */
  if (pick_list != NULL) free(pick_list);

  /* Insert point into the horizon structure */
  ErsPointInsert(picking_record, horizon, point, ErsAUTO_PICK);
}

static void DeleteLastPoint(widget, event)
Widget widget;
XEvent *event;
{
  PR_ *picking_record;
  ErsPoint *point_to_delete, *point;
  Bool    delete_flag = True;

  picking_record = (PR_ *) ErsGetPickingRecord(widget);
  if (picking_record == NULL) return;

  if (picking_record->current_horizon == NULL) return;

  /* remove point with the highest seqno from current horizon */
  point = picking_record->current_horizon->first_point;
  if (point == NULL) return;

  point_to_delete = point;

  while (point->next != NULL) {
    point = point->next;
    if (point->seqno > point_to_delete->seqno)
      point_to_delete = point;
  }

  if (picking_record->point_delete_callback != NULL)
    picking_record->point_delete_callback(widget,
                    picking_record->point_delete_callback_data,
                    point_to_delete,&delete_flag);

  if (delete_flag)
   {ErsPointRemove(picking_record->current_horizon, point_to_delete);
    ErsRedrawBoundingBox(picking_record, NULL, point_to_delete->previous,
                       point_to_delete, point_to_delete->next, True,True);
    ErsPointFree(point_to_delete);
   }
}

static void HorizonCreate(widget, event)
Widget widget;
XEvent *event;
{
  PR_ *picking_record;
  ErsHorizon *horizon;

  picking_record = (PR_ *) ErsGetPickingRecord(widget);
  if (picking_record == NULL) return;

  /* Default color and names used */
  horizon = ErsHorizonCreate(picking_record, NULL, NULL,
            picking_record->default_line_width, True);

  /* Allocate a named color */
  ErsHorizonSetColor(picking_record, horizon);

  /* New horizon will be made the selected one */
  PR_SetCurrentHorizon(picking_record, horizon);

  if (picking_record->horizon_add_callback != NULL)
    picking_record->horizon_add_callback(widget, horizon,
                                  picking_record->horizon_add_callback_data);
}

static void HorizonSelect(widget, event)
Widget widget;
XEvent *event;
{
  PR_ *picking_record;
  ErsHorizon *horizon;
  float  pkey, skey, tkey;
  long   tn;
  float  wx, time;
  float  ttol,keytol,*hd,p1,p2;
  int    Phdr,nhdrs,numv;

  if (!ErsPointToKeyTime(widget, event->xbutton.x, event->xbutton.y,
                          &pkey, &skey, &tkey, &tn, &wx, &time)) return;

  picking_record = (PR_ *) ErsGetPickingRecord(widget);
  if (picking_record == NULL) return;

  ErsSeismicGetSampleRate(widget, &ttol);
  ttol = 10.0* ttol;

  hd = NULL;
  ErsSeismicGethdDat(widget , &hd, &nhdrs, &numv);
  Phdr = SEQHDR;
  if(picking_record->Phdr != UNDEFINED_KEY) Phdr = picking_record->Phdr;
  keytol = 10.0;
  if(hd != NULL)
   { p1 = hd[Phdr-1];
     p2 = hd[Phdr-1 + nhdrs];
     keytol = p1-p2; /* difference between adjacent traces */
     keytol = (float) fabs((double) keytol);
     keytol = 4.0 *keytol;
   }
  else
   {ErsCoordInf *T;
    float uc[4];
    T  = (ErsCoordInf *) ErsGetCoordInf(widget);
    if(T==NULL)
     {Get_uc(T,uc);
      keytol = 0.01*(uc[0] - uc[1]);
      if(keytol < 0) keytol = -keytol;
     }
   }

  horizon = ErsHorizonFind(picking_record, pkey, time, keytol,ttol);
  if (horizon == NULL) return;

  PR_SetCurrentHorizon(picking_record, horizon);

  /* call horizon select callback if there is one */
  if (picking_record->horizon_select_callback != NULL)
    picking_record->horizon_select_callback(widget,
                    picking_record->horizon_select_callback_data, horizon);

}
/* Delete a single horizon-segment */
static void HorizonDelete(widget, event)
Widget widget;
XEvent *event;
{
  PR_ *picking_record;
  ErsHorizon *horizon[2];
  float  pkey, skey, tkey;
  long   tn;
  float  wx, time;
  float  ttol,keytol,*hd,p1,p2;
  int    Phdr,nhdrs,numv;

  if (!ErsPointToKeyTime(widget, event->xbutton.x, event->xbutton.y,
                          &pkey, &skey, &tkey, &tn, &wx, &time)) return;

  picking_record = (PR_ *) ErsGetPickingRecord(widget);
  if (picking_record == NULL) return;

  ErsSeismicGetSampleRate(widget, &ttol);
  ttol = 10.0* ttol;

  hd = NULL;
  ErsSeismicGethdDat(widget , &hd, &nhdrs, &numv);
  Phdr = SEQHDR;
  if(picking_record->Phdr != UNDEFINED_KEY) Phdr = picking_record->Phdr;
  keytol = 10.0;
  if(hd != NULL)
   { p1 = hd[Phdr-1];
     p2 = hd[Phdr-1 + nhdrs];
     keytol = p1-p2; /* difference between adjacent traces */
     keytol = (float) fabs((double) keytol);
     keytol = 4.0 *keytol;
   }

  horizon[0]=ErsHorizonFind(picking_record, pkey, time, keytol, ttol);
  horizon[1]=NULL;

  /* if this was the horizon selected, set current horizon to NULL */
  if (horizon[0]==picking_record->current_horizon)
    picking_record->current_horizon = NULL;

  if (horizon[0] != NULL) {
    Bool    delete_flag = True;
    if (picking_record->horizon_delete_callback != NULL)
      picking_record->horizon_delete_callback(widget, horizon[0], 
                  picking_record->horizon_delete_callback_data, &delete_flag);
    if (delete_flag) ErsHorizonDestroy(picking_record, horizon);
  }
}
/*******************************************************************
 * ErsHorizonSetColor() Will allocat a named color if it detects ***
 * that the horizon->pixel value is zero and if the horizon color***
 * name is set.                                                  ***
 ******************************************************************/
void ErsHorizonSetColor(PR_ *pikrec, ErsHorizon *horizon)
{ XColor cell, rgb_cell;
  int    status;

  if(horizon == NULL) return;
  /* Check for name match and set hid flag */
  /* pixel will be set if there is a name match */
  ErsHorizonCheckAttributes(pikrec, horizon);
  if(horizon->pixel !=0) return;
  if(horizon->color_name == NULL) return;
  if(pikrec->display != NULL)
   {/* allocate color cell for horizon color if there is a display*/
    status = XAllocNamedColor(pikrec->display,
             pikrec->colormap, horizon->color_name,
             &cell, &rgb_cell);
    if (status == 0)
     {/* color name is not correct */
      free(horizon->color_name);
      horizon->color_name = (char *) malloc(4);
      strcpy(horizon->color_name,"red");
      XAllocNamedColor(pikrec->display, pikrec->colormap,
                       horizon->color_name, &cell, &rgb_cell);
      if (status == 0)
        {/* very unlikely */
         printf("ErsHorizonSetColor: Error allocating color\n");
         cell.pixel = BlackPixel(pikrec->display, 0);
        }
     }
    horizon->pixel = cell.pixel;
   }
 return;
}

/****************************************************************/
/*                                                              */
/*  PR_SnapCallback - This is the sample callback  */
/*  that is provided for snapping. This callback is called      */
/*  each time a point is inserted or moved. Depending on the    */
/*  state of the snap_mode value in the picking_record          */
/*  structure, we adjust the time to max, Min, or zero crossing */
/*  or don't touch anything.                                    */
/*                                                              */
/****************************************************************/

static void PR_SnapCallback(widget, point, data)
Widget widget;
ErsPoint *point;
caddr_t data;
{
  PR_ *picking_record;
  float time_start, time_end;
  float *trace;
  int ntrace, nsample;
  float sample_rate;
  long traceno,Phdr;
  int wn = 12;
  float pick_index_time;
  register int pick_index, index, i, i1, i2;

  /* extract picking record */
  picking_record = (PR_ *) data;
  if (picking_record == NULL) 
    XtError("PR_SnapCallback: invalid widget ID");
  
  /* reset error code */
  picking_record->error_code = ErsNO_ERROR;

  /* if not snap return */
  if (picking_record->snap_mode == ErsNO_SNAP) return;

  /* select gate around point */
  ErsSeismicGetSampleRate(widget, &sample_rate);
  time_start = point->time - wn * sample_rate;
  time_end = point->time + wn * sample_rate;

  /* Get 1 trace worth of data  */
  /* This will also adjust time_start and time_end if out of bound.*/
  Phdr = picking_record->Phdr;
  ErsPKeyToTrace(widget, point->pkey, Phdr, &traceno);
  trace = ErsSeismicGetData(widget, traceno, traceno, &time_start,
                           &time_end, &ntrace, &nsample);
  if (trace == NULL) {
    picking_record->error_code = ErsERROR_READING_DATA;
    return;
  }

  if (picking_record->snap_mode == ErsSNAP_MINIMUM || 
      picking_record->snap_mode == ErsSNAP_PM_ZERO_CROSSING) 
    for (i=0; i<nsample; i++) trace[i] = -trace[i];

  pick_index = (point->time - time_start) / sample_rate + 0.5;
  pick_index_time = time_start + pick_index * sample_rate;

  if (picking_record->snap_mode == ErsSNAP_MAXIMUM ||
      picking_record->snap_mode == ErsSNAP_MINIMUM)
   {for (i1=pick_index; i1<nsample-1; i1++) 
     if (trace[i1+1] < trace[i1]) break;
    for (i2=pick_index; i2>0; i2--) 
     if (trace[i2-1] < trace[i2]) break;
    if (trace[i1] > trace[i2])
      index = i1;
    else 
      index = i2;
    /* if all constant values, stay there */    
    if (trace[index] == trace[pick_index]) index = pick_index;

    point->time = pick_index_time + (index - pick_index) * sample_rate;

  } else { /* zero crossing */

    index = -1;
    for (i1=0; i1<nsample-1; i1++) {
      if (trace[i1] <= 0 && trace[i1+1] > 0) {
        if (index == -1 || abs(index - pick_index) > abs(i1 - pick_index))
          index = i1;
      }
    }
    /* if we failed, keep original point, otherwise update */
    if (index != -1) {
      point->time = pick_index_time + (index - pick_index) * sample_rate;

      /* now adjust linearly */
      point->time += -trace[index]*sample_rate / (trace[index+1]-trace[index]);
    }
  }
  free(trace);
}

static void
PR_TrackerCallback(widget, point, pick_list, npicks, 
                                picking_record)
Widget       widget;
ErsPoint     *point;
ErsPointList *pick_list;
int npicks;
PR_ *picking_record;
{
  Widget twidget;
  struct PlotImage *PI;
  float *trace, *trace_ptr, *header;
  float ftime,tmin,tmax,time;
  float pkey , skey  ,tkey ;
  float time_start,time_end;
  int   ntrace, nsample, nhdrs;
  int   wn2 = 8;
  int   max_trace,block,nblocks;
  int   lower_index, upper_index;
  int   tn_start, tn_end;
  int   trace_counter,numtr;
  long  mem1,mem2,mems,meme,trs,tre;
  float fdelta = 0;
  float sample_rate;
  int pick_index, start_index, Phdr,Shdr,Thdr;
  float pick_index_time;
  int index, it, delta,incr;
  Bool    invert_flag = False;
  register int i, i1, i2;

  /* pick_list must be allocated prior to this point */
  if(pick_list == NULL || point == NULL) return;

  PI = (struct PlotImage *) ErsGetPlotInf(widget);
  if(PI==NULL) return;
  tmin = I_sdatTmin(PI);
  tmax = I_sdatTmax(PI);
  nhdrs= I_sdatnhd(PI);
  ErsSeismicGetSampleRate(widget,&sample_rate);
  wn2 = picking_record->max_dip_in_samples;

  if (wn2 < 1) wn2 = 1;
  /* read max_trace at a time */
  max_trace = 20;

  /* Perform a Snap and return if:
   * 1.  This is the very first point -or-
   * 2.  There are no traces between the control points
  */
  if (point->previous == NULL || npicks == 0)
   {int save_state;
    save_state = picking_record->snap_mode; 
    /* make sure we use the same mode for snapping as for tracking */
    picking_record->snap_mode = picking_record->track_mode + 1;
    picking_record->snap_callback(widget, point, 
                                  picking_record->snap_callback_data);
    picking_record->snap_mode = save_state;
    return;
   }

  /* Find the number of traces in between the two points  */
  /* Break into smaller chunks when there are many traces.*/
  /* Find traces nearest to the pkey values               */
  Phdr = picking_record->Phdr;
  if(Phdr == UNDEFINED_KEY) return;
  Shdr = picking_record->Shdr;
  Thdr = picking_record->Thdr;
  if(ErsSeismicTellSelectionP(widget,point->previous->pkey,
                           point->pkey,Phdr,&mem1,&mem2) != True)
    { printf(" TellSelectionP failed\n");
      return;  }
  /* Number of traces to pick = trace_counter = npicks+1 */
  /* End point repicked so it will be snapped to event   */
  trace_counter = (int) labs(mem2-mem1);

  it =0;
  ftime= point->previous->time;
  if(mem1 <= mem2)
    {  incr=  1; }
  else
    {  incr= -1; }

  nblocks = (labs(mem2-mem1)-1)/max_trace + 1;
  for(block=0;block<nblocks;block++)
   { mems = mem1 + incr + block*incr*max_trace;
     meme = mems + incr*max_trace - incr;
     if(incr == 1 && meme > mem2 ) meme = mem2;
     if(incr == -1 && meme < mem2 ) meme = mem2;

    time_start = ftime - max_trace*wn2*sample_rate;
    if (time_start < tmin) time_start = tmin;
    time_end   = ftime + max_trace*wn2*sample_rate;
    if (time_end > tmax) time_end = tmax;
    if(mems <= meme)
     { trace = ErsSeismicGetTrTiWin(widget,&mems,&meme,
               &time_start,&time_end,
               &header, &ntrace, &nsample, &sample_rate);
     }
    else
     { trace = ErsSeismicGetTrTiWin(widget,&meme,&mems,
               &time_start,&time_end,
               &header, &ntrace, &nsample, &sample_rate);
     }

    if (trace == NULL) 
     { printf("PR_TrackerCallback: error getting data\n");
       return;
      /*XtError("PR_TrackerCallback: error reading data");*/
     }
    trace_ptr = trace;

    if (picking_record->track_mode == ErsTRACK_MINIMUM || 
        picking_record->track_mode == ErsTRACK_PM_ZERO_CROSSING)
      for (i=0; i<nsample*ntrace; i++) trace[i] = -trace[i];

    /* Check if trace number increases from left to right or vice versa */
    if(mem1 > mem2)
     {invert_flag = True;
      trace  += (ntrace-1)*nsample;
      header += (ntrace-1)*nhdrs;
     }

    if (it != 0) 
      delta = start_index - pick_index;
    else
      delta = 0;
    pick_index = (ftime - time_start) / sample_rate + .5;
    pick_index_time =  time_start + pick_index * sample_rate;
    start_index = pick_index + delta;

 /* Loop over the traces in memory. Search for events */
    for (i=0; i<ntrace; i++)
     {if (picking_record->track_mode == ErsTRACK_MAXIMUM || 
          picking_record->track_mode == ErsTRACK_MINIMUM)
       {/* check we don't go out of bounds */
        if ((upper_index = start_index+wn2-1) > nsample-1)
          upper_index = nsample-1;
        if ((lower_index = start_index-wn2) < 0) lower_index = 0;
        for (i1=start_index; i1<upper_index; i1++) 
          if (trace[i1+1] < trace[i1]) break;
        for (i2=start_index; i2>lower_index; i2--) 
          if (trace[i2-1] < trace[i2]) break;
        if (trace[i1] > trace[i2])
          index = i1;
        else 
          index = i2;
        /* if all constant values, use start_index */    
        if (trace[index] == trace[start_index]) index = start_index;
        fdelta = 0;
       }
      else
       {/* look for zero crossing */
        index = -1;
        /* check we don't go out of bounds */
        if ((lower_index = start_index-wn2) < 0) lower_index = 0;
        if ((upper_index = start_index+wn2-1) > nsample-1)
          upper_index = nsample-1;
        for (i1=lower_index; i1<upper_index; i1++)
         {if (trace[i1] <= 0 && trace[i1+1] > 0)
           {if (index == -1 || abs(index-start_index) > abs(i1-start_index))
             index = i1;
           }
         }
        /* if we didn't find any zero crossing use start_index*/
        if (index == -1)
         {index = start_index;
          fdelta = 0;
         }
        else
         {/* now adjust zero crossing linearly */
          fdelta = -trace[index] * sample_rate / (trace[index+1]-trace[index]);
         }
       }

      time = time_start + index * sample_rate + fdelta;
      pkey = UNDEFINED_VAL;
      if(Phdr != UNDEFINED_KEY) pkey = header[Phdr-1];
      skey = UNDEFINED_VAL;
      if(Shdr != UNDEFINED_KEY) skey = header[Shdr-1];
      tkey = UNDEFINED_VAL;
      if(Thdr != UNDEFINED_KEY) tkey = header[Thdr-1];
      if (it < npicks)
       {pick_list[it].time = time;
        pick_list[it].traceno = header[SEQHDR-1];
        pick_list[it].pkey = pkey;
        pick_list[it].skey = skey;
        pick_list[it].tkey = tkey;
       }
      else if (it == npicks) 
       {point->time    = time;
        point->tn      = header[SEQHDR-1];
        point->pkey    = pkey;
        point->skey    = skey;
        point->tkey    = tkey;
       }
      else
       { printf("TrackerCallback: illegal it value=%d\n",it);
       }

      it++;
      if (picking_record->projection_mode == ErsLINEAR_PROJECTION)
        start_index =  index + (index - pick_index);
      else                      /* horizontal */
        start_index =  index;
      pick_index = index;
       
      if (invert_flag)     /* change data and header pointers */
       {trace  -= nsample;
        header -= nhdrs; }
      else
       {trace  += nsample;
        header += nhdrs; }
     }

    free(trace_ptr);
    ftime = pick_list[it-1].time;
  }
}

void PR_ToPostscript(widget, fp, color_model)
Widget widget;
FILE *fp;
int color_model;
{
  PR_ *picking_record;
  ErsPoint *point;
  struct PlotImage *PI;
  int px, py;
  int th;
  register int i, j;

  picking_record = (PR_ *) ErsGetPickingRecord(widget);
  if (picking_record == NULL) return;
  PI = (struct PlotImage *) ErsGetPlotInf(widget);
  th = I_gphHght(PI); /* see image.h */

  for (i=0; i<picking_record->horizon_count; i++)
   {if (color_model != ErsMONOCHROME)
      ErsPostscriptSetColor(fp, &widget, 
        (Pixel) picking_record->horizon_list[i]->pixel);
    fprintf(fp, "%d setlinewidth\n",
            picking_record->horizon_list[i]->line_width);
    fprintf(fp, "newpath\n");
    point = picking_record->horizon_list[i]->first_point;
    while (point != NULL)
     {if (point->npicks != 0)
        { for (j=0; j<point->npicks; j++)
             {if (!ErsPKeyTimeToPoint(widget, point->pick_list[j].pkey, 
                        point->pick_list[j].time, &px, &py)) break;
              fprintf(fp, "%d %d lineto\n", px, th-py);
             }
        }
      if (!ErsPKeyTimeToPoint(widget, point->pkey, point->time, &px, &py))
        break; /* if point is out, stop */
      if (point == picking_record->horizon_list[i]->first_point)
        fprintf(fp, "%d %d moveto\n", px, th-py);
      else
        fprintf(fp, "%d %d lineto\n", px, th-py);
      point = point->next;
     }
    fprintf(fp, "stroke\n");
   }

  /* we draw point symbols now */
  for (i=0; i<picking_record->horizon_count; i++)
   {int sz, szd2;
    sz = picking_record->horizon_list[i]->symbol_size;
    szd2 = sz / 2;
    if (sz != 0)
     {if (color_model != ErsMONOCHROME)
        ErsPostscriptSetColor(fp, &widget,
                     (Pixel)  picking_record->horizon_list[i]->pixel);
      point = picking_record->horizon_list[i]->first_point;
      while (point != NULL)
       {if (ErsPKeyTimeToPoint(widget, point->pkey, point->time,
                                       &px, &py))
          {fprintf(fp,
               "newpath %d %d moveto %d 0 rlineto 0 %d rlineto -%d 0 rlineto\n",
                px-szd2, th-py-szd2, sz, sz, sz);
           fprintf(fp, "closepath fill\n");
          }
        point = point->next;
       }
     }
   }
}
void ErsPostscriptSetColor(FILE *fp, Widget *widget,Pixel pixel)
{
  XColor cell;
  float red, green, blue;
/*
  cell.pixel = pixel;
  XQueryColor(XtDisplay(widget), widget->core.colormap, &cell);
  red = (float) cell.red / 65535;
  green = (float) cell.green / 65535;
  blue = (float) cell.blue / 65535;
  fprintf(fp, "%f %f %f setrgbcolor\n", red, green, blue);
*/
}


static ErsHorizon *horizon_to_move;
static XPoint     *point_list;
static ErsPoint   **point_reference;
static int        point_list_count;

static void HorizonStartMove(widget, event)
Widget widget;
XEvent *event;
{
  PR_ *picking_record;
  ErsHorizon *horizon;
  ErsPoint *point;
  float  pkey, skey, tkey;
  long   tn;
  float  wx, time;
  float sample_rate;
  int x, y;
  float  ttol,keytol,*hd,p1,p2;
  int    Phdr,nhdrs,numv;
  register int i;


  nx = event->xbutton.x;
  ny = event->xbutton.y;
  px = -1;
  py = -1;

  if (!ErsPointToKeyTime(widget, nx, ny, &pkey, &skey, &tkey,
                         &tn, &wx, &time))return;

  picking_record = (PR_ *) ErsGetPickingRecord(widget);
  if (picking_record == NULL) return;
  ErsSeismicGetSampleRate(widget,&ttol);
  ttol = 10.0*ttol;

  hd = NULL;
  ErsSeismicGethdDat(widget , &hd, &nhdrs, &numv);
  Phdr = SEQHDR;
  if(picking_record->Phdr != UNDEFINED_KEY) Phdr = picking_record->Phdr;
  keytol = 10.0;
  if(hd != NULL)
   { p1 = hd[Phdr-1];
     p2 = hd[Phdr-1 + nhdrs];
     keytol = p1-p2; /* difference between adjacent traces */
     keytol = (float) fabs((double) keytol);
     keytol = 4.0 *keytol;
   }
  horizon_to_move= ErsHorizonFind(picking_record, pkey, time, keytol, 
                                  ttol);
  if (horizon_to_move == NULL) return;
  rubberband_flag = True;

  /* set GC */
  XSetFunction(XtDisplay(widget), picking_record->gc, GXinvert);

  point_list = (XPoint *) malloc(sizeof(XPoint) * 
                                   horizon_to_move->number_of_points);
  point_reference = (ErsPoint **) malloc(sizeof(ErsPoint *) * 
                                   horizon_to_move->number_of_points);
  point_list_count = 0;
  point = horizon_to_move->first_point;

  for (i=0; i<horizon_to_move->number_of_points; i++)
   {if (ErsPKeyTimeToPoint(widget, point->pkey, point->time, &x, &y))
     { point_list[point_list_count].x = x;
       point_list[point_list_count].y = y;
       point_reference[point_list_count] = point;
       point_list_count++;
     }
    point = point->next;
   }
}

static void HorizonMove(widget, event)
Widget widget;
XEvent *event;
{
  PR_ *picking_record;
  Display *display;
  Window w;
  int ex, ey;
  register int i;

  if (!rubberband_flag) return;

  picking_record = (PR_ *) ErsGetPickingRecord(widget);
  if (picking_record == NULL) return;

  display =  XtDisplay(widget);
  w = XtWindow(widget);

  if (px != -1) XDrawLines(display, w, picking_record->gc, point_list,
                           point_list_count, CoordModeOrigin);

  px = nx;
  py = ny;
  nx = event->xmotion.x;
  ny = event->xmotion.y;

  for (i=0; i<point_list_count; i++) {
    point_list[i].x += (nx - px);
    point_list[i].y += (ny - py);
  }

  XDrawLines(display, w, picking_record->gc, point_list, point_list_count,
             CoordModeOrigin);
}

static void HorizonEndMove(widget, event)
Widget widget;
XEvent *event;
{
  PR_ *picking_record;
  ErsPoint *point;
  Display *display;
  Window w;
  float  pkey, skey, tkey;
  long   tn,tn_shift;
  float  wx, time;
  float  time_shift,pkey_shift;
  Bool    do_it;
  register int i;

  if (!rubberband_flag) return;
  rubberband_flag = False;
  picking_record = (PR_ *) ErsGetPickingRecord(widget);
  if (picking_record == NULL) return;

  display =  XtDisplay(widget);
  w = XtWindow(widget);

  XDrawLines(display, w, picking_record->gc, point_list, point_list_count,
             CoordModeOrigin);

  /* restore GC */
  XSetFunction(display, picking_record->gc, GXcopy);

  /* we must find a point that is visible to calculate the shift */

  for (i=0; i<point_list_count; i++) 
    if (ErsPointToKeyTime(widget, point_list[i].x, point_list[i].y, 
                                 &pkey, &skey, &tkey, &tn, &wx, &time)) break;
  
  if (i == point_list_count)
    {XBell(display, 0);
     return;
    }
/* We dont try to shift skey or tkey. This could cause problems */
  tn_shift   = tn - point_reference[i]->tn;
  time_shift = time - point_reference[i]->time;
  pkey_shift = pkey - point_reference[i]->pkey;

  point = horizon_to_move->first_point;
  for (i=0; i<horizon_to_move->number_of_points; i++) {
    point->time += time_shift;
    point->tn   += tn_shift;
    point->pkey += pkey_shift;
    point = point->next;
  }

  /* check to make sure we don't have negative time or tn */
  point = horizon_to_move->first_point;
  for (i=0; i<horizon_to_move->number_of_points; i++) {
    if (point->time < 0 || point->tn < 1) break;
    point = point->next;
  }

  if (i != horizon_to_move->number_of_points) {
    /* restore and quit */
    point = horizon_to_move->first_point;
    for (i=0; i<horizon_to_move->number_of_points; i++) {
      point->time -= time_shift;
      point->tn   -= tn_shift;
      point->pkey -= pkey_shift;
      point = point->next;
    }
    XBell(display, 0);
    return;
  }

  /* call user callback if defined */
  do_it = True;
  if (picking_record->horizon_move_callback != NULL)
    picking_record->horizon_move_callback(widget,
                  picking_record->horizon_move_callback_data,
                  horizon_to_move, &do_it);

  if (do_it)
    { /* update min and max */
      ErsHorizonUpdateMinMax(horizon_to_move);
      /* redraw all */
      PR_Draw(picking_record, NULL, NULL);
    }
  else
    { /* restore */
      point = horizon_to_move->first_point;
      for (i=0; i<horizon_to_move->number_of_points; i++)
       {point->time -= time_shift;
        point->tn   -= tn_shift;
        point->pkey -= pkey_shift;
        point = point->next;
       }
    }

}

/************************************************************************
 *                                                                      *
 *  - XmPickingRecordPointSnap -                                        *
 *    public function to manually snap a point.                         *
 *    Will call the SnapCallback function                               *
 *                                                                      *
 ************************************************************************/
Bool    PR_PointSnap(picking_record, point)
PR_ *picking_record;
ErsPoint *point;
{
  Widget widget;
  register int i;

  if (picking_record->widget_count == 0) 
    XtError("XmPickingRecordPointSnap: no seismic widget connected");

  if (picking_record->snap_callback == NULL) {
    XtWarning("XmPickingRecordPointSnap: no snap callback registered");
    return(False);
  }

  for (i=0; i<picking_record->widget_count; i++) {
    if (picking_record->snap_callback != NULL)
      picking_record->snap_callback(picking_record->widget_rec[i].widget, 
                        point, picking_record->snap_callback_data);
    if (picking_record->error_code == ErsNO_ERROR) return(True);
  }

  return(False);
}

/************************************************************************
 *
 * actions: InitAreaSelection, ExtendAreaSelection and
 *          EndAreaSelection
 *

***********************************************************************/
static int xso, yso, xse, yse;
static Bool wselection_on = False;

static void InitAreaSelection(widget, event)
Widget widget;
XEvent *event;
{
  PR_ *pikrec;
  static Cursor xhair;
  static Bool cursor_defined = False;
  xso = event->xbutton.x;
  yso = event->xbutton.y;
  xse = xso;
  yse = yso;

/**********************************************************
 * Get data necessary for continuing.                   ***
 *********************************************************/
  if(widget == NULL ) return;
  pikrec = (PR_ *) ErsGetPickingRecord(widget);
  if(pikrec == NULL) return;
  /* set GC */
  XSetFunction(XtDisplay(widget), pikrec->gc, GXinvert);
  wselection_on = True;

}

/* gc_rubber was used in seismic widget */
static void ExtendAreaSelection(widget, event)
Widget widget;
XEvent *event;
{
  PR_ *pikrec;
  int width, height;
  int x1, y1;

  if (!wselection_on) return;

  pikrec = (PR_ *) ErsGetPickingRecord(widget);
  if(pikrec == NULL) return;

  width = xse - xso;
  if (width < 0) {
    width = - width;
    x1 = xse;
  } else
    x1 = xso;

  height = yse - yso;
  if (height < 0) {
    height = - height;
    y1 = yse;
  } else
    y1 = yso;

  if (width > 0 && height > 0)
    XDrawRectangle(XtDisplay(widget), XtWindow(widget),
                   pikrec->gc, x1, y1, width, height);

  xse = event->xmotion.x;
  yse = event->xmotion.y;

  width = xse -xso;
  if (width < 0) {
    width = - width;
    x1 = xse;
  } else
    x1 = xso;

 height = yse - yso;
  if (height < 0) {
    height = - height;
    y1 = yse;
  } else
    y1 = yso;

  if (width > 0 && height > 0)
    XDrawRectangle(XtDisplay(widget), XtWindow(widget),
                   pikrec->gc, x1, y1, width, height);
}

static void EndAreaSelection(widget, event)
Widget widget;
XEvent *event;
{ PR_ *pikrec;
  ErsHorizon       *chorizon;
  ErsPoint         *point;
  int x1, y1,xs,ys,xe,ye;
  int width, height;
  long   tns,tne;
  float  pkeys,skeys,tkeys,wxs,wys;
  float  pkeye,skeye,tkeye,wxe,wye;
  struct PlotImage *PI;
  static ErsSeismicAreaSelectionCallbackStruct select_data;
  float  ftemp;
  register int temp;

  if (!wselection_on) return;

  width = xse - xso;
  if (width < 0) {
    width = - width;
    x1 = xse;
  } else
    x1 = xso;

  height = yse - yso;
  if (height < 0) {
    height = - height;
    y1 = yse;
  } else
    y1 = yso;
 
  pikrec = (PR_ *) ErsGetPickingRecord(widget);
  if(pikrec == NULL) return;
  if (width > 0 && height > 0)
    XDrawRectangle(XtDisplay(widget), XtWindow(widget),
                   pikrec->gc, x1, y1, width, height);
  /* restore GC */
  XSetFunction(XtDisplay(widget), pikrec->gc, GXcopy);
  wselection_on = False;

  xs = x1;
  ys = y1;
  xe = x1 + width;
  ye = y1 + height;

  /* if selection has been done upside down, reorder */
  if (xs > xe)
   {temp = xs;
    xs = xe;
    xe = temp;
   }
  if (ys > ye)
   {temp = ys;
    ys = ye;
    ye = temp;
   }

  /* convert points */
  if(!ErsPointToKeyTime(widget, xs, ys,
                 &pkeys, &skeys,&tkeys, &tns,  &wxs, &wys))
   { printf("EndAreaSelection: failure calling PointToKeyTime\n");
     return;
   }
  if(!ErsPointToKeyTime(widget, xe, ye,
                 &pkeye, &skeye,&tkeye, &tne,  &wxe, &wye))
   { printf("EndAreaSelection: failure calling PointToKeyTime\n");
     return;
   }
  /* if selected points were outside section, adjust */
/*
  PI = ErsGetPlotInf(widget);
  if(PI == NULL) return;
  if (wys == UNDEFINED_VAL) wys = I_sdatTmin(PI);
  if (wye == UNDEFINED_VAL) wye = I_sdatTmax(PI);
*/

  /* make sure selection is in proper order */
  if (tns > tne)
   {
    temp = tns;
    tns  = tne;
    tne  = temp;
    ftemp= pkeys;
    pkeys= pkeye;
    pkeye= ftemp;
    ftemp= skeys;
    skeys= skeye;
    skeye= ftemp;
    ftemp= tkeys;
    tkeys= tkeye;
    tkeye= ftemp;
  }

/*  call selection callback */
/*
  select_data.reason = ErsCR_AREA_SELECTION;
  select_data.event = event;
  XtCallCallbacks(widget, XmNareaSelectionCallback, &select_data);
*/
  chorizon = pikrec->current_horizon;
  if (chorizon == NULL || chorizon->number_of_points < 1) return;
  point = chorizon->first_point;
  while (point != NULL)
   { 
    if( ErsPKeyTimeToPoint(widget, point->pkey, point->time, &px, &py))
     {if( ErsPointInside(chorizon, px, py, xs, ys, width, height))
       {/*ErsPointDestroy(pikrec->current_horizon, point);*/
        ErsPointDelete(pikrec, chorizon, point);
       }
     }
    point = point->next;
   }
}

