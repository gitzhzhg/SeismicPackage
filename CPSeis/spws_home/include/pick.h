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
#ifndef _PICKING_INCLUDE
#define _PICKING_INCLUDE

#include <X11/Intrinsic.h>
#include "transform.h"

#ifdef __hpux
#include <spws_hpux.h>
#endif

/* define constants */
#define ErsAPPEND      1
#define ErsFIRST       2
#define ErsINSERT      3
#define ErsAUTO_PICK   4
#define ErsINSERT_TIME 5
#define ErsSILENT      6     /* suppresses draws in ErsPointInsert - for Input*/

#define TBORDER         50
#define LBORDER         50
#define UNDEFINED_HDR  -1
#define UNDEFINED_TN   -1
#define UNDEFINED_KEY   0
#define UNDEFINED_HID  -1
#define UNDEFINED_VAL  .123245

/*constants for snapping */
#define ErsNO_SNAP               0
#define ErsSNAP_MAXIMUM          1
#define ErsSNAP_MINIMUM          2
#define ErsSNAP_MP_ZERO_CROSSING 3
#define ErsSNAP_PM_ZERO_CROSSING 4

/* constants for tracking */
#define ErsTRACK_MAXIMUM          0
#define ErsTRACK_MINIMUM          1
#define ErsTRACK_MP_ZERO_CROSSING 2
#define ErsTRACK_PM_ZERO_CROSSING 3

#define ErsLINEAR_PROJECTION      0
#define ErsHORIZONTAL_PROJECTION  1

#define ErsOVERRIDE_TRANSLATIONS  1
#define ErsAUGMENT_TRANSLATIONS   2
#define ErsREPLACE_TRANSLATIONS   3

/* define reasons for seismic callbacks */
#define ErsCR_CHANGE_PANEL         1
#define ErsCR_NEW_PIXMAP           2
#define ErsCR_LOCATOR              3
#define ErsCR_SELECTION            4
#define ErsCR_AREA_SELECTION       5
#define ErsCR_TRACE_ANNOTATION     6
#define ErsCR_PROCESS_TRACE        7


/* constants for symbol attributes */
#define ErsSYMBOL_AUTO_SIZED     -1
#define ErsHORIZON_PIXEL         -1
#define DEFAULT_LINE_WIDTH  2
#define SELECTED_LINE_WIDTH 3

/* codes for operations */
#define NO_OP        1
#define AUTO_PICK_OP 2
#define INSERT_OP    3
#define DELETE_OP    4

/* error codes */
#define ErsNO_ERROR           0
#define ErsERROR_READING_DATA 1

#define XmNareaSelectionCallback            "areaSelectionCallback"

/* callback for resize and expose */
typedef struct
 {int reason;
  XEvent *event;
  Window window;
 } ErsSeismicCallbackStruct;

/* callback structure for area selection */
typedef struct
 {int reason;
  XEvent *event;
  int x_start, y_start;
  int x_end, y_end;
  float time_start, time_end;
  int pkey_start, pkey_end;
  int skey_start, skey_end;
  long tn_start, tn_end;
 } ErsSeismicAreaSelectionCallbackStruct;

/*****************************************
 * Definition of the ErsPoint structure***
 ****************************************/
typedef struct _ErsPointList
 { long  traceno;
   float pkey;
   float skey;
   float tkey;
   float time;
   float user_data;
 } ErsPointList;

typedef struct _ErsPoint
 { long  tn;
   float pkey;
   float skey;
   float tkey;
   float time;
   float user_data;
   long  seqno;
   int   npicks;
   ErsPointList *pick_list;
   struct _ErsPoint *next;
   struct _ErsPoint *previous;
 } ErsPoint;

/*******************************************
 * Definition of the ErsHorizon structure***
 ******************************************/
typedef struct _ErsHorizon
 {struct _ErsPoint *first_point;
  struct _ErsPoint *last_point;
  int number_of_points;
  int line_width;
  int symbol_size;
  unsigned long pixel;
  char *color_name;
  char *horizon_name;
  int  hid;
  float max_pkey, min_pkey;
  float max_time, min_time;
  long  max_tn, min_tn;
  int user_int;
  void *vector;
  Bool horizon_active;
 } ErsHorizon;

/**********************************************
 * Definition of ErsWidgetRecord structure. ***
 * Picking Records can be tied to more than ***
 * than one widget at a time! Thus we need a***
 * array of widget specific information.    ***
 *********************************************/
typedef struct _ErsWidgetRecord
 {Widget widget;
  caddr_t data;    /* a ErsCoordInf structure see Transform.h*/
  caddr_t PlotI;   /* a PlotImage structure   */
  XtCallbackProc proc;
 } ErsWidgetRecord;

typedef void (*PointFun)(Widget, void *, ErsPoint *, void *);
typedef void (*HorizDel)(Widget, ErsHorizon *, void *, void *);
typedef void (*HorizFun)(Widget, void *, ErsHorizon *, void *);
typedef void (*TrackFun)(Widget, ErsPoint *, ErsPointList *, int, void *);

/*******************************************
 * Define structure for a picking record ***
 ******************************************/
typedef struct _PR_
 {/* X-windows related variables : include Intrinsic.h */
  int widget_count;
  ErsWidgetRecord *widget_rec;
  Display *display;
  GC gc;
  GC gc_symbol;
  Colormap colormap;
  Pixel selected_pixel;
  XtTranslations translations;
  XtCallbackProc point_select_callback;
  caddr_t        point_select_callback_data;
  XtCallbackProc point_add_callback;
  caddr_t        point_add_callback_data;
  PointFun       point_delete_callback;
  caddr_t        point_delete_callback_data;
  XtCallbackProc horizon_select_callback;
  caddr_t        horizon_select_callback_data;
  XtCallbackProc horizon_add_callback;
  caddr_t        horizon_add_callback_data;
  HorizDel       horizon_delete_callback;
  caddr_t        horizon_delete_callback_data;
  XtCallbackProc snap_callback;
  caddr_t        snap_callback_data;
  TrackFun       tracker_callback;
  caddr_t        tracker_callback_data;
  HorizFun       horizon_move_callback;
  caddr_t        horizon_move_callback_data;
  PointFun       point_move_callback;
  caddr_t        point_move_callback_data;
  int install_translations_mode;
  /* Non X-windows variables */
  ErsHorizon *current_horizon;
  int horizon_count;
  ErsHorizon **horizon_list;
  int default_symbol_size;     /* symbol size assigned to a new segment*/
  int selected_symbol_size;    /* symbol size for a selected segment*/
  int default_line_width;      /* line width for a new segment */
  int selected_line_width;     /* line width for selected segment */
  int snap_mode;
  int track_mode, projection_mode, max_dip_in_samples;
  caddr_t user_data;
  int Phdr;                    /* Header word keys - same for all widgets*/
  int Shdr;
  int Thdr;
  ErsTransform *transx;        /* Units information for x vals */
  ErsTransform *transz;        /* Units information for z vals */
  int error_code;
 } PR_;

/* define macros */
#define PR_GetWidget(pr, n)  ((pr)->widget_rec[(n)-1].widget)
#define PR_WidgetCount(pr)   ((pr)->widget_count)
#define PR_HorizonCount(pr)  ((pr)->horizon_count)
#define PR_GetPhdr(pr)       ((pr)->Phdr)
#define PR_GetShdr(pr)       ((pr)->Shdr)
#define PR_GetThdr(pr)       ((pr)->Thdr)
#define PR_SetPhdr(pr,n)     ((pr)->Phdr = n )
#define PR_SetShdr(pr,n)     ((pr)->Shdr = n )
#define PR_SetThdr(pr,n)     ((pr)->Thdr = n )
#define ErsHorizonGetName(pr, n)          ((pr)->horizon_list[(n)-1]->horizon_name)
#define ErsHorizonSetDataErs(h, v)        ((h)->user_int = (int) v)
#define ErsHorizonGetDataErs(h)           ((h)->user_int);
#define ErsHorizonCount(pd)               ((pd)->horizon_count)
#define ErsHorizonGetFirstPoint(h)        ((h)->first_point)
#define ErsHorizonGetLastPoint(h)         ((h)->last_point)
/*****************************************
 * Macros for Point structure          ***
 ****************************************/
#define ErsPointNext(p)                   ((p)->next)
#define ErsPointPrev(p)                   ((p)->previous)
#define ErsPointTime(p)                   ((p)->time)
#define ErsPointTraceNumber(p)            ((p)->tn)
#define P_GetTr(P)      ( (P)->tn )
#define P_Getpkey(P)    ( (P)->pkey )
#define P_Getskey(P)    ( (P)->skey )
#define P_Gettkey(P)    ( (P)->tkey )
#define P_GetTime(P)    ( (P)->time )
#define P_GetUser(P)    ( (P)->user_data )
#define P_Getnpicks(P)  ( (P)->npicks )
#define P_GetPList(P)   ( (P)->pick_list )

/* define function type */
#ifdef __cplusplus
extern "C" {                 // for C++
#endif

 PR_        *PR_Create();
 void        PR_Destroy( PR_ *picking_record);
 void        PR_Free(PR_ *picking_record);
 void       *PR_getgui(void *pikrec);
 ErsCoordInf *ErsGetCoordInf(Widget W);
 Bool    ErsSetCoordInf0(Widget W, ErsCoordInf *T);
 Bool    ErsSetCoordInf(Widget W,
               float *xd1, float *yd1, float *xw1, float *yw1,
               float *xd2, float *yd2, float *xw2, float *yw2);

 void    ErsSetPickingRecord(Widget, caddr_t);
 PR_ *ErsGetPickingRecord(Widget widget);
 void              PR_SetCurrentHorizon(
                             PR_ *picking_record,
                             ErsHorizon *horizon);
 void              ErsWidgetSetTranslations(Widget, 
                         XtTranslations,int);   /* for one widget */
 void              PR_SetTranslations(
                             PR_ *picking_record,
                             XtTranslations translations, int mode);
 void              PR_SetSnapMode(
                             PR_ *picking_record, int
mode);
 void              PR_SetTrackMode(
                             PR_ *picking_record, int
track_mode,
                             int projection_mode, int
max_dip_in_samples);
 void              PR_SetTrackerCallback(
                             PR_ *picking_record,
                             TrackFun tracker_callback,
                             caddr_t tracker_callback_data);
 void              PR_SetSnapCallback(
                             PR_ *picking_record,
                             XtCallbackProc snap_callback,
                             caddr_t snap_callback_data);
 void              PR_SetHorizonMoveCallback(
                             PR_ *picking_record,
                             HorizFun horizon_move_callback,
                             caddr_t horizon_move_callback_data);
 void              PR_SetPointMoveCallback(
                             PR_ *picking_record,
                             XtCallbackProc point_move_callback,
                             caddr_t point_move_callback_data);
 void              ErsRedrawBoundingBox(PR_ *, ErsHorizon *,
                         ErsPoint *,ErsPoint *,ErsPoint *, Bool,Bool);

 void              PR_Draw(PR_
*picking_record
,
                             Widget widget, XEvent *event);
 void              PR_ConnectWidget( PR_ *picking_record,
                         Widget widget, caddr_t P);
 void              PR_DisconnectWidget(
                             PR_ *picking_record, Widget
widget);
 Bool              PR_Compare(
                             PR_ *picking_record1,
                             PR_ *picking_record2);
 Bool              PR_PointSnap(
                             PR_ *picking_record,
                             ErsPoint *point);
 void              PR_SetCurrentHorizonSymbolSize(
                             PR_ *picking_record, int
symbol_size)
;
 void              PR_SetCurrentHorizonSymbolPixel(
                             PR_ *picking_record, Pixel
pixel);

 void              PR_SetDefaultLineWidth(
                             PR_ *picking_record, int
line_width);
 void              PR_SetDefaultSymbolSize(
                             PR_ *picking_record, int
symbol_size);
 void              PRCounts(PR_ *,long *, long *);
 void              ErsPRMinMax(PR_ *,
                            float *, float *, float *,float *);
 void              PR_setx_transf(PR_ *,
                         ErsTransform *);
 void              PR_setz_transf(PR_ *,
                         ErsTransform *);
 ErsTransform     *PR_z_transf(PR_ *);
 ErsTransform     *PR_x_transf(PR_ *);

 ErsHorizon       *ErsHorizonCreate(PR_ *picking_record,
                             char *hname, char *cname,
                             int lwidth, Bool   select_horizon_flag);
 ErsHorizon       *ErsHorizonNew( char *hname, char
                  *cname, int lwidth,Bool select_horizon_flag);
 ErsHorizon       *ErsHorizonGet(
                            PR_ *picking_record, int n);
 void              ErsHorizonChangeAttributes(
                            PR_ *picking_record,
                            ErsHorizon *horizon, char *horizon_name,
                            char *color_name, int line_width);
 void              ErsSegmentChangeAttributes(
                            PR_ *picking_record,
                            ErsHorizon *horizon, char *horizon_name,
                            char *color_name, int line_width);
 void              ErsHorizonChangeName(
                            PR_ *picking_record,
                            ErsHorizon *horizon, char *horizon_name);
 void              ErsHorizonChangeHID(
                            PR_ *picking_record,
                            ErsHorizon *horizon, int hid);
 void              ErsHorizonCheckAttributes(PR_ *pikrec,
                            ErsHorizon *horizon);
 void              ErsHorizonDestroy(PR_ *picking_record,
                            ErsHorizon **horizon);
 void              ErsHorizonSegmentDestroy(PR_ *
                            picking_record, ErsHorizon *horizon);
 void              ErsHorizonDraw(PR_ *picking_record,
                            ErsHorizon *horizon, Widget widget,
                            int ex, int ey , int width, int height);
 ErsHorizon       *ErsHorizonGetFromPoint(
                            PR_ *picking_record, ErsPoint *point);
 ErsHorizon      **ErsHorizonGetByName(
                            PR_ * picking_record ,
                            char *horizon_name);
 ErsHorizon       *ErsHorizonGetSegmentN(PR_ *,
                          char *, int);
 int               ErsHorizonGetSegmentCount(PR_ *pikrec,
                            ErsHorizon *horizon);
 void              ErsHorizonGetSegments(PR_ *,
                         char *, int *, ErsHorizon **);
 void              ErsHorizonGetHList(PR_ *pikrec,
                            int *nhor , ErsHorizon **uhlist );
 void              ErsHorizonMemberList(PR_ *,
                         ErsHorizon *, int *, int *,ErsHorizon **);
 ErsHorizon       *ErsHorizonFind(PR_ *picking_record,
                             float tn, float time, float delta_key,
                             float delta_time);
 ErsPoint         *ErsHorizonGetPoint(ErsHorizon *horizon,
                             int point_number);
 ErsHorizon       *ErsHorizonDuplicate(
                            PR_ *picking_record ,
                            ErsHorizon *horizon);
 int               ErsHorizonGetPointCount(ErsHorizon *horizon);
 int               ErsHorizonGetNumPicks(ErsHorizon *horizon);
 void              ErsHorizonSetSymbolSize(
                             PR_ *picking_record,
                             ErsHorizon *horizon, int symbol_size);
 void              ErsHorizonSetPixel(
                            PR_ *picking_record,
                            ErsHorizon *horizon, Pixel pixel);
 void              ErsHorizonSetColor(
                            PR_ *picking_record,
                            ErsHorizon *horizon);
 void              ErsHorizonMinMax(ErsHorizon *,
                            float *, float *, float *,float *);
 void             *ErsHorizonGetVect(ErsHorizon *);
 void              ErsHorizonSetVect(ErsHorizon *, void *vector);

 ErsPoint          *ErsPointCreate(float pick_time, long tn,
                             float *user_data,
                             float pkey,float skey,float tkey);
 Bool               ErsPointDestroy(ErsHorizon *horizon, ErsPoint *point);
 Bool               ErsPointRemove(ErsHorizon *horizon, ErsPoint *point);
 void               ErsPointFree(ErsPoint *point);
 void               ErsPointDelete(PR_ *picking_record,
                             ErsHorizon *horizon, ErsPoint *point_to_delete);
 int                ErsPointCount(ErsPoint *point);
 ErsPoint          *ErsPointFind(PR_ *picking_record,
                             ErsHorizon *horizon, float pkey, float time,
                             float delta_key, float delta_time);
 int                FindNNP(ErsHorizon *, ErsPoint *,
                             ErsPoint **, ErsPoint **);
 void               ErsPointInsert(PR_ *picking_record,
                             ErsHorizon *horizon, ErsPoint *point,
                             int insertion_mode);
 void               ErsPointAdd(PR_ *, ErsHorizon *,
                             ErsPoint *, int);
 void               ErsPointDraw(PR_ *picking_record,
                             ErsHorizon *horizon, Widget widget, int x, int y);
 void               ErsPointDrawAll(PR_ *picking_record,
                             ErsHorizon *horizon, ErsPoint *point);
 void             IntRedrawBoundingBox(PR_ *picking_record ,
                           ErsHorizon *horizon, ErsPoint *point1,
                           ErsPoint *point2, ErsPoint *point3,
                           Bool    draw_seismic_flag);
 void               ErsPointAddPickList(ErsHorizon *horizon,
                             ErsPoint *point, ErsPointList *pick_list, int
npicks);
 ErsHorizon *       ErsPointHorizonFind(PR_ *picking_record,
                           ErsPoint *point);
 void               ErsPointMinMax(ErsPoint *,
                          float *,float *,float *,float*);
#ifdef __cplusplus
}                   // for C++
#endif

#endif
