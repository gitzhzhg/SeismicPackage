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


/*------------------------- vel_popups.c -------------------------------*/

/*----------------------- header files ---------------------------------*/

#include <stdio.h>
#include <math.h>
#include <Xm/PanedW.h>
#include <Xm/PushB.h>
#include <Xm/ArrowB.h>
#include <Xm/MainW.h>
#include <Xm/Form.h>
#include <Xm/DrawingA.h>
#include <Xm/RowColumn.h>
#include <Xm/CascadeB.h>
#include <Xm/Scale.h>
#include <Xm/ToggleB.h>
#include <Xm/DialogS.h>
#include <X11/StringDefs.h>
#include "va.h"
#include "wbox.h"
#include "wproc.h"
#include "cprim.h"
#include "vel_boxes.h"
#include "trslib.h"
#include "image.h"



/*---------------global static variables needed many places ------------*/
   /* these could be constants except that the addresses are needed */

static long P0 = 0L, P1 = 1L, P5 = 5L, M1 = -1L, M5 = -5L, M44 = -44L;
static long P3 = 3L, P4 = 4L, M4 = -4L;


/*-------------interfaces to fortran routines-------------------*/
       /* to avoid prototype mismatch warning messages */

static void ff_read_tempfile(void *data, long *error)
{
  f_read_tempfile_((long*)data, error);
}

static void ff_close_tempfile(void *data, long *error)
{
  f_close_tempfile_();
}



/*--------------------- quitting routine -------------------------------*/

void quit_vel_popups(void *data, long *error)
{
  VelStruct *vel = (VelStruct*)data;
  long zero = 0L;

  start_wait_cursor_(&vel);
  f_close_tempfile_();
  f_autosave_workfile_(&zero);
  stop_wait_cursor_(&vel);
  *error = 0L;
}



/*---------- helpers for automatic update routine ----------------------*/

static void fill_kode(long *kode, long nfun, long ifun)
{
  int i, n;

  n = nfun + 100; if(n > NFUNMAX) n = NFUNMAX;
  for(i = 0; i < n; i++) {  kode[i] = 0;  }
  if(ifun > 0 && ifun <= n) kode[ifun - 1] = 1;
}

static long fill_matches(float *semxloc, float *semyloc, long snfun,
                         float xcenter, float xwidth,
                         float ycenter, float ywidth,
                         float *xbin, float *ybin, long nfun, 
                         char *sflag, long ifun, long *skode, long *sifun)
{
  int i, j;
  long sembmatch;

  sembmatch = 0;
  for(i = 0; i < snfun; i++)
       {
       j = find_match(semxloc[i], semyloc[i], xcenter, xwidth,
                               ycenter, ywidth, xbin, ybin, nfun);
       if(j == 0) { sflag[i] = '*'; sembmatch++; }
       else         sflag[i] = ' ';
       }
  *sifun = find_match(xbin[ifun - 1], ybin[ifun - 1], xcenter, xwidth,
                              ycenter, ywidth, semxloc, semyloc, snfun);
  fill_kode(skode, snfun, *sifun);
  return sembmatch;
}


/*---------- automatic update routine ----------------------------------*/

static long sembmatch,cmpmatch;
static char sflag[NFUNMAX], cflag[NFUNMAX];
static long nfunmax = NFUNMAX, skode[NFUNMAX], ckode[NFUNMAX];


static void vel_update_from_everyone(void *data)
{
  VelStruct *vel = (VelStruct*)data;
  float ybin = 0.0;
  VbStruct *vb;

      get_vbstruct_pointer(&vb);
      get_select_counts_(&vb->nselect);
      get_errmsg_counts_(&vb->nerrmsg);
      get_raymsg_counts_(&vb->nraymsg);
      fill_kode(vb->kode, vel->vd->nfun, vel->vd->ifun);

     sembmatch = fill_matches(vel->semxloc, vel->semyloc, vel->snfun,
           vel->vd->xcenter, vel->vd->xwidth,
           vel->vd->ycenter, vel->vd->ywidth,
           vel->vd->xbin, vel->vd->ybin, vel->vd->nfun,
           sflag, vel->vd->ifun, skode, &vb->sifun);
      cmpmatch  = fill_matches(vel->cmpxloc, vel->cmpyloc, vel->cnfun,
           vel->vd->xcenter, vel->vd->xwidth,
           vel->vd->ycenter, vel->vd->ywidth,
           vel->vd->xbin, vel->vd->ybin, vel->vd->nfun,
           cflag, vel->vd->ifun, ckode, &vb->cifun);
}




/*--------------------- startup routine --------------------------------*/

void start_vel_popups(void *data, long am_in_va)
{
  VelStruct *vel = (VelStruct*)data;

  vel->vw.read_widget    = NULL;   vel->vw.read_box    = NULL;
  vel->vw.save_widget    = NULL;   vel->vw.save_box    = NULL; 

  vel->vw.pick_widget    = NULL;   vel->vw.pick_box    = NULL;
  vel->vw.fun_widget     = NULL;   vel->vw.fun_box     = NULL;
  vel->vw.set_widget     = NULL;   vel->vw.set_box     = NULL;
  vel->vw.vfid_widget    = NULL;   vel->vw.vfid_box    = NULL;
  vel->vw.head_widget    = NULL;   vel->vw.head_box    = NULL;
  vel->vw.res_widget     = NULL;   vel->vw.res_box     = NULL;
  vel->vw.lat_widget     = NULL;   vel->vw.lat_box     = NULL;
  vel->vw.lat2_widget    = NULL;   vel->vw.lat2_box    = NULL;
  vel->vw.ray_widget     = NULL;   vel->vw.ray_box     = NULL;
  vel->vw.del_widget     = NULL;   vel->vw.del_box     = NULL;
  vel->vw.mult_widget    = NULL;   vel->vw.mult_box    = NULL;
  vel->vw.misc_widget    = NULL;   vel->vw.misc_box    = NULL;
  vel->vw.offmute_widget = NULL;   vel->vw.offmute_box = NULL;

  vel->vw.info_widget    = NULL;   vel->vw.info_box    = NULL; 
  vel->vw.tol_widget     = NULL;

  vel->vw.mig_widget     = NULL;   vel->vw.mig_box     = NULL; 
  vel->vw.breadth_widget = NULL;   vel->vw.breadth_box = NULL; 
  vel->vw.err_widget     = NULL;   vel->vw.err_box     = NULL; 
  vel->vw.resid_widget   = NULL;   vel->vw.resid_box   = NULL; 
  vel->vw.freznel_widget = NULL;   vel->vw.freznel_box = NULL; 
  vel->vw.dip_widget     = NULL;   vel->vw.dip_box     = NULL; 

  vel->vw.prev           = NULL;
  vel->vw.next           = NULL; 
  vel->vw.pick_undo      = NULL; 
  vel->vw.res_undo       = NULL; 
  vel->vw.lat_undo       = NULL; 
  vel->vw.lat2_undo      = NULL; 
  vel->vw.del_undo       = NULL; 
  vel->vw.mult_undo      = NULL; 

  vel->vdDpyRef = (VdStruct *) NULL;

  start_vel_data_ (&vel->vd);
  start_vel_boxes_(vel, &am_in_va);
  register_update_function(vel_update_from_everyone, (void*)vel);
}



/*---------- defined constants for use below ---------------------------*/

#define     HH                vel->helpctx
#define     HHDD              vel->helpctx, vel
#define   SSHHDD        NULL, vel->helpctx, vel
#define LLSSHHDD  NULL, NULL, vel->helpctx, vel
#define   MM             &M1
#define   IIHHDD         &P0, vel->helpctx, vel
#define   MMHHDD         &M1, vel->helpctx, vel
#define LLIIHHDD  NULL,  &P0, vel->helpctx, vel

#define NOT_YET  "--- This dialog box is not yet implemented ---"
#define NOT_FUNC  "--- This dialog box is ---\n--- not yet functional ---"




/*---------- windowbox creation convenience routines -------------------*/

static void *windowbox(VelStruct *vel, long traptype,
              Widget popup, Widget bottom, String boxname,
              void (*reg)(), void (*trap)())
{
  void *box;
  Widget w;

  box = wbox_create1(trap, traptype);
  if(traptype <= 1) reg(box);
  else              reg(vel);
  w = wbox_create2(boxname, NULL, popup, NULL, HH, NULL, NULL);
  attach_widget(w,  popup,popup,popup,bottom, 0,0,0,0);
  XtManageChild(w);
  return box;
}



/*---------- standard box creation and management routines -------------*/

static void standard_box(VelStruct *vel, Widget *popup, String name,
                        void (*apply)(), void **box, String boxname, 
                        void (*reg)(), void (*trap)(), Boolean ats)
{
  Widget bottom;

  if(!*popup)
       {
       *popup  = make_shell   (vel->shell, name, NULL);
       bottom  = make_bottom  (*popup, "bottom");
                 make_push    (bottom, "button_ok"    ,LLSSHHDD, apply, *popup);
                 make_push    (bottom, "button_apply" ,LLSSHHDD, apply, NULL  );
                 make_push    (bottom, "button_cancel",LLSSHHDD, NULL , *popup);
       if(ats)   make_pushhelp(bottom, "button_autosave",NULL,NULL,HH);
                 make_pushhelp(bottom, "button_help"    ,NULL,NULL,HH);
       *box    = windowbox    (vel, 0L, *popup, bottom, boxname, reg, trap);
       }
  XtManageChild(*popup);
}


static void view_box(VelStruct *vel, Widget *popup, String name,
                        void **box, String boxname, 
      Boolean formquest, void (*topreg)(), void (*reg)(), void (*trap)())
{
  Widget top, middle, bottom;

  if(!*popup)
       {
       *popup = make_shell (vel->shell, name, NULL);
       if(formquest) top    = make_form  (*popup, "top"   );
       else          top    = make_column(*popup, "top"   );
                     middle = make_draw  (*popup, "middle");
                     bottom = make_bottom(*popup, "bottom");
       attach_widget(top   , *popup, *popup, *popup, NULL, 0,0,0,0);
       attach_widget(middle, *popup, *popup, top,  bottom, 0,0,0,0);

       topreg(vel, top);

       make_push    (bottom,"button_remove",LLSSHHDD ,NULL, *popup);
       make_pushhelp(bottom,"wbox_keyhelp",NULL,NULL, HH);
       make_pushhelp(bottom,"button_help" ,NULL,NULL, HH);

       *box = wbox_create1(trap, 3L);
       reg(vel);
       wbox_create2(boxname, NULL, *popup, middle, HH, NULL, NULL);
       }
  XtManageChild(*popup);
}


/*---------- undo-button sensitization management routines -------------*/

#define UndoA1     VelStruct **vel2, long *flag1, long *flag2
#define UndoA2     VelStruct **vel2, long *flag

#define UndoW(w)      (*vel2)->vw.w

#define Undo1(w)  if(UndoW(w)) XtSetSensitive(UndoW(w), (*flag1 == *flag2))
#define Undo2(w)  if(UndoW(w)) XtSetSensitive(UndoW(w), (*flag == 1))

void  res_undo_sensitize_(UndoA1)  {  Undo1( res_undo);  }
void  lat_undo_sensitize_(UndoA1)  {  Undo1( lat_undo);  }
void lat2_undo_sensitize_(UndoA1)  {  Undo1(lat2_undo);  }
void  del_undo_sensitize_(UndoA1)  {  Undo1( del_undo);  }
void mult_undo_sensitize_(UndoA1)  {  Undo1(mult_undo);  }
void pick_undo_sensitize_(UndoA2)  {  Undo2(pick_undo);  }



/*-------------------- read routine ------------------------------------*/

void manage_read_filebox_(VelStruct **vel2)
{
  VelStruct *vel = *vel2;
  if(XtIsManaged(vel->vw.fwidget)) XtUnmanageChild(vel->vw.fwidget);
  else                             manage_widget(vel->vw.fwidget);
}


void read_dialog(void *data)
{
  VelStruct *vel = (VelStruct*)data;

  standard_box(vel, &vel->vw.read_widget, "read_shell",
       read_apply_, &vel->vw.read_box   , "read_data",
       read_register_, read_trap_, True);

  if(!vel->vw.fwidget) vel->vw.fwidget =
    make_filebox(vel->vw.read_widget, "filebox",NULL,HHDD, read_getfile_,NULL);
}



/*-------------------- save routine ------------------------------------*/

void save_dialog(void *data)
{
  VelStruct *vel = (VelStruct*)data;

  standard_box(vel, &vel->vw.save_widget, "save_shell",
       save_apply_, &vel->vw.save_box   , "save_data",
       save_register_, save_trap_, True);
}



/*-------------------- info routine ------------------------------------*/

static void info_trap(void *box, long *ident, long *index, char *text,
                    long *nread, char *endkey)
{
  if     (!strcmp(endkey, "INSERT")) strcpy(endkey, " ");
  else if(!strcmp(endkey, "REMOVE")) strcpy(endkey, " ");
}


static void info_reg1(VelStruct *vel, long c)
{
  wbox_ireg3(NULL,0,"sembl panels"  ,&P0,&vel->snfun   ,&M5,1,c+3,5,0);
  wbox_rega(&vel->snfun, &vel->snfun, 2, 30);
  wbox_xrega(info_trap, 10, " "   , &P0, &nfunmax    , &M44 ,c, 4, 0);
  wbox_irega(info_trap, 19, " "    ,&P0, skode       , &M4  ,0, 2, 0);
  wbox_frega(info_trap, 11, "XBIN", &P0, vel->semxloc, &P5  ,0, 8, 4);
  wbox_frega(info_trap, 13, "YBIN", &P0, vel->semyloc, &P5  ,0, 8, 4);
  wbox_crega(info_trap, 12, "M"   , &P0, sflag       , &P5  ,0, 1, 0);
}


static void info_reg2(VelStruct *vel, long c)
{
  wbox_ireg3(NULL,0,"cmp panels"    ,&P0,&vel->cnfun   ,&M5,1,c+3,5,0);
  wbox_rega(&vel->cnfun, &vel->cnfun, 2, 30);
  wbox_xrega(info_trap, 20, " "   , &P0, &nfunmax    , &M44 ,c, 4, 0);
  wbox_irega(info_trap, 29, " "    ,&P0, ckode       , &M4  ,0, 2, 0);
  wbox_frega(info_trap, 21, "XBIN", &P0, vel->cmpxloc, &P5  ,0, 8, 4);
  wbox_frega(info_trap, 23, "YBIN", &P0, vel->cmpyloc, &P5  ,0, 8, 4);
  wbox_crega(info_trap, 22, "M"   , &P0, cflag       , &P5  ,0, 1, 0);
}

static void info_reg3(VelStruct *vel, long c)
{
  VbStruct *vb;

  get_vbstruct_pointer(&vb);
  wbox_ireg3(NULL,0,"velocity functions",&P0,&vel->vd->nfun,&M5,1,c+3,5,0);
  wbox_rega(&vel->vd->nfun, &vel->vd->nfun, 2, 30);
  wbox_xrega(info_trap, 30, " "    ,&P0,&nfunmax          ,&M44 ,c, 4, 0);
  wbox_irega(info_trap, 39, " "    ,&P0,vb->kode          ,&M4  ,0, 2, 0);
  wbox_crega(info_trap, 31, "NAME" ,&P0,vel->vd->vfid[0]  ,&P5  ,0, 8, 0);
  wbox_frega(info_trap, 33, "XBIN" ,&P0,vel->vd->xbin,     &P5  ,0, 8, 4);
  wbox_frega(info_trap, 34, "YBIN" ,&P0,vel->vd->ybin,     &P5  ,0, 8, 4);
  wbox_crega(info_trap, 36, "E"    ,&P0,vel->vd->errmsg[0],&P5  ,0, 1, 0);
  wbox_crega(info_trap, 37, "R"    ,&P0,vel->vd->raymsg[0],&P5  ,0, 1, 0);
  wbox_irega(info_trap, 38, "picks",&P0,vel->vd->n,        &P5  ,0, 5, 0);
}



#define LABEL_VEL "velocity functions selected:"
#define LABEL_SEM \
"sembl panels (marked with *) have NO matching velocity function"
#define LABEL_CMP \
" cmp  panels (marked with *) have NO matching velocity function"

static void info_topreg(VelStruct *vel, Widget parent)
{
  Widget w1, w2, w3, w4, w5, w6;
  Widget row = make_row(parent, "row");
  Widget row2 = make_form(parent, "row2");
  Widget row3 = make_form(parent, "row3");

  VbStruct *vb;

  get_vbstruct_pointer(&vb);
  make_itext2(row   ,"nselect"  , LABEL_VEL    ,IIHHDD,NULL,&vb->nselect ,6);
  make_itext2(row   ,"nerrmsg"  ,"with errors:",IIHHDD,NULL,&vb->nerrmsg ,6);
  make_itext2(row   ,"nraymsg"  ,"raytraced:"  ,IIHHDD,NULL,&vb->nraymsg ,6);
  make_itext2(row   ,"nhx"      ,"NHX:"        ,IIHHDD,NULL,&vel->vd->nhx,2);
  make_itext2(row   ,"nhy"      ,"NHY:"        ,IIHHDD,NULL,&vel->vd->nhy,2);
  w1=make_itext3(row2  ,"sembmatch", LABEL_SEM    ,IIHHDD,NULL,&sembmatch   ,6);
  w2=make_itext3(row3  ,"cmpmatch" , LABEL_CMP    ,IIHHDD,NULL,&cmpmatch    ,6);
  w3=make_ftext2(row2,"xcenter" ,"xcenter:",IIHHDD,NULL,&vel->vd->xcenter,7,3);
  w4=make_ftext2(row3,"ycenter" ,"ycenter:",IIHHDD,NULL,&vel->vd->ycenter,7,3);
  w5=make_ftext2(row2,"xwidth", "  xwidth:",IIHHDD,NULL,&vel->vd->xwidth ,5,3);
  w6=make_ftext2(row3,"ywidth", "  ywidth:",IIHHDD,NULL,&vel->vd->ywidth ,5,3);
  attach_widget(w1, row2,NULL ,row2,row2, 0,0,0,0);
  attach_widget(w3, NULL,w5   ,row2,row2, 0,0,0,0);
  attach_widget(w5, NULL,row2 ,row2,row2, 0,0,0,0);
  attach_widget(w2, row3,NULL ,row3,row3, 0,0,0,0);
  attach_widget(w4, NULL,w6   ,row3,row3, 0,0,0,0);
  attach_widget(w6, NULL,row3 ,row3,row3, 0,0,0,0);
}


void info_dialog(void *data)
{
  VelStruct *vel = (VelStruct*)data;
  Widget wb, top, popup, bottom;
  VbStruct *vb;

  if(!vel->vw.info_widget)
       {
       popup  = vel->vw.info_widget = make_shell(vel->shell,"info_shell",NULL);
       top    = make_column(popup, "top");
       bottom = make_bottom(popup, "bottom");
       attach_widget(top, popup,popup,popup,NULL  , 0,0,0,0);

       info_topreg(vel, top);

       make_push    (bottom,"button_remove",LLSSHHDD ,NULL,  popup);
       make_pushhelp(bottom,"wbox_keyhelp",NULL,NULL, HH);
       make_pushhelp(bottom,"button_help" ,NULL,NULL, HH);

       vel->vw.info_box = wbox_create1(info_trap, 3L);
       get_vbstruct_pointer(&vb);
       vb->iinfobox = *(long*)vel->vw.info_box;
       info_reg1(vel, 1);
       info_reg2(vel,33);   /* was 24 */
       info_reg3(vel,65);   /* was 47 */
       wb = wbox_create2("info_data", NULL, popup, NULL, HH, NULL, NULL);
       attach_widget(wb,  popup,popup,top,bottom, 0,0,0,0);
       XtManageChild(wb);
       }
  if(!XtIsManaged(vel->vw.info_widget))
       {
       XtManageChild(vel->vw.info_widget);
       }
}




/*--------------------- tol routine ------------------------------------*/

static void user_mm_trap(void *data, float *oldvar)
{
  VelStruct *vel = (VelStruct*)data;
  Display *dpy = XtDisplay(vel->shell);
  vel->user_mm = ConstrainValue(vel->user_mm, 0.1, 10.0);
  vel->x_pixel_tol = HorzPixelsFromMM(dpy, DefaultScreen(dpy), vel->user_mm);
  vel->y_pixel_tol = VertPixelsFromMM(dpy, DefaultScreen(dpy), vel->user_mm);
  vel->x_pixel_tol = MaximumValue(vel->x_pixel_tol, 1);
  vel->y_pixel_tol = MaximumValue(vel->y_pixel_tol, 1);
}


static void center_trap(void *data, float *oldvar)
{
  VelStruct *vel = (VelStruct*)data;
  broadcast(vel, SENDER_WBOX, MESSAGE_TOL, 0,0,0,0,0);
}


static void width_trap(void *data, float *oldvar)
{
  VelStruct *vel = (VelStruct*)data;
  vel->vd->xwidth = MaximumValue(vel->vd->xwidth, 0.001);
  vel->vd->ywidth = MaximumValue(vel->vd->ywidth, 0.001);
  broadcast(vel, SENDER_WBOX, MESSAGE_TOL, 0,0,0,0,0);
}


void tol_dialog(void *data)
{
  VelStruct *vel = (VelStruct*)data;
  Widget popup, rc2, rc3, lab2, lab3, lab4, bottom;
  Widget row2, row3;

  if(!vel->vw.tol_widget)
       {
       popup  = vel->vw.tol_widget = make_shell(vel->shell,"tol_shell",NULL);
       bottom = make_bottom(popup, "bottom");
       rc2    = make_framed_column(popup, "rc2");
       rc3    = make_framed_column(popup, "_rc3");
       lab2 = make_label(popup,"lab2","Picking Tolerances",NULL);
       lab3 = make_label(popup,"lab3","Xbin and Ybin Matching Tolerances",NULL);
       lab4 = make_label(popup,"lab4"," " ,NULL);
       attach_widget(lab2, popup,popup,popup,NULL  , 0,0,30, 0);
       attach_widget(rc2 , popup,popup,lab2 ,NULL  , 0,0, 0, 0);
       attach_widget(lab3, popup,popup,rc2  ,NULL  , 0,0,30, 0);
       attach_widget(rc3 , popup,popup,lab3 ,NULL  , 0,0, 0, 0);
       attach_widget(lab4, popup,popup,rc3  ,bottom, 0,0, 0,30);
       make_label(rc2,"tol1", "tolerance" , NULL);
       make_label(rc2,"tol2", "horizontal", MM);
       make_label(rc2,"tol3", "vertical"  , MM);
       make_ftext(rc2,"tol4", SSHHDD, user_mm_trap, &vel->user_mm   ,6,3);
       make_itext(rc2,"tol5", MMHHDD, NULL        , &vel->x_pixel_tol,6);
       make_itext(rc2,"tol6", MMHHDD, NULL        , &vel->y_pixel_tol,6);
       make_label(rc2,"tol7", "millimeters", NULL);
       make_label(rc2,"tol8", "pixels"     , MM);
       make_label(rc2,"tol9", "pixels"     , MM);

       make_label (rc3 ,"label",
                "header word      any bin center          bin width",NULL);
       row2 = make_row(rc3, "_row2");
       row3 = make_row(rc3, "_row3");
  make_itext2(row2,"nhx"    ,    "NHX:",MMHHDD,NULL,&vel->vd->nhx    ,3);
  make_itext2(row3,"nhy"    ,    "NHY:",MMHHDD,NULL,&vel->vd->nhy    ,3);
  make_ftext2(row2,"xcent","xcenter:",SSHHDD,center_trap,&vel->vd->xcenter,6,3);
  make_ftext2(row3,"ycent","ycenter:",SSHHDD,center_trap,&vel->vd->ycenter,6,3);
  make_ftext2(row2,"xwidth","xwidth:",SSHHDD,width_trap,&vel->vd->xwidth ,5,3);
  make_ftext2(row3,"ywidth","ywidth:",SSHHDD,width_trap,&vel->vd->ywidth ,5,3);
  XmRemoveTabGroup(row2);
  XmRemoveTabGroup(row3);

       make_push    (bottom,"button_remove",LLSSHHDD ,NULL,  popup);
       make_pushhelp(bottom,"button_help" ,NULL,NULL, HH);
       }
  if(!XtIsManaged(vel->vw.tol_widget))
       {
       XtManageChild(vel->vw.tol_widget);
       }
}




/*-------------------- pick routine ------------------------------------*/

void adjust_arrow_sensitivity_(VelStruct **vel2)
{
  VelStruct *vel = *vel2;
  long ifun, nfun;

  ifun = vel->vd->ifun;
  nfun = vel->vd->nfun;
  sensitize_arrow(vel->vw.prev, ifun > 1                      );
  sensitize_arrow(vel->vw.next, ifun <= nfun && ifun < NFUNMAX);
}


void pick_dialog(void *data)
{
  VelStruct *vel = (VelStruct*)data;
  Widget popup, bottom, arrowbox;

  if(!vel->vw.pick_widget)
       {
       popup  = vel->vw.pick_widget = make_shell(vel->shell,"pick_shell",NULL);
       bottom = make_bottom(popup, "bottom");

       arrowbox = XmCreateRowColumn(bottom, "arrowbox", NULL, 0);
       XmRemoveTabGroup(arrowbox);
       XtManageChild(arrowbox);
       vel->vw.prev = make_larrow(arrowbox,"arrow_prev",SSHHDD,pick_prev_,
                                                           NULL,NULL,NULL);
       vel->vw.next = make_rarrow(arrowbox,"arrow_next",SSHHDD,pick_next_,
                                                           NULL,NULL,NULL);
       vel->vw.pick_undo= make_push  (bottom ,"button_undo",LLSSHHDD,pick_undo_,
                                                                 NULL);
       make_push    (bottom,"button_remove",LLSSHHDD ,NULL,  popup);
       make_pushhelp(bottom,"wbox_keyhelp",NULL,NULL, HH);
       make_pushhelp(bottom,"button_help" ,NULL,NULL, HH);

       vel->vw.pick_box = windowbox(vel, 0L, popup, bottom, "pick_data",
                                           pick_register_, pick_trap_);
       }
  if(!XtIsManaged(vel->vw.pick_widget))
       {
       XtSetSensitive(vel->vw.pick_undo, False);
       XtManageChild(vel->vw.pick_widget);
       }
}



/*--------------------- fun routine ------------------------------------*/

void fun_redraw(void *data, long *error)
{
  VelStruct *vel = (VelStruct*)data;
  broadcast(vel, SENDER_WBOX, MESSAGE_NUMBER, 0,0,0,0,0);
}


void fun_dialog(void *data)
{
  VelStruct *vel = (VelStruct*)data;
  Widget popup, bottom;

  if(!vel->vw.fun_widget)
       {
       popup  = vel->vw.fun_widget = make_shell(vel->shell,"fun_shell",NULL);
       bottom = make_bottom(popup, "bottom");

       make_push    (bottom,"button_sort"    ,LLSSHHDD,sort_functions,NULL);
       make_push    (bottom,"button_deselect",LLSSHHDD,fun_deselect_ ,NULL);
       vel->iso.redraw_iso = make_push(bottom,"button_redraw",LLSSHHDD,
                                                         fun_redraw,NULL);
       make_push    (bottom,"button_remove" ,LLSSHHDD ,NULL, popup   );
       make_pushhelp(bottom,"wbox_keyhelp",NULL,NULL, HH);
       make_pushhelp(bottom,"button_help" ,NULL,NULL, HH);

       vel->vw.fun_box = windowbox(vel, 0L, popup, bottom, "fun_data",
                                             fun_register_, fun_trap_);
       XtUnmanageChild(vel->iso.redraw_iso);
       }
  XtManageChild(vel->vw.fun_widget);
}


/*--------------------- res routine ------------------------------------*/

void res_dialog(void *data)
{
  VelStruct *vel = (VelStruct*)data;
  Widget popup, bottom;

  if(!vel->vw.res_widget)
       {
       popup  = vel->vw.res_widget = make_shell(vel->shell,"res_shell",NULL);
       bottom = make_bottom(popup, "bottom");

       make_push    (bottom,"button_apply",LLSSHHDD ,res_apply_  ,NULL        );
       vel->vw.res_undo = make_push(bottom,"button_undo",LLSSHHDD,
                                                ff_read_tempfile,NULL);
       make_push    (bottom,"button_cancel",LLSSHHDD,ff_close_tempfile, popup );
       make_pushhelp(bottom,"button_help",NULL,NULL,HH);

       vel->vw.res_box  = windowbox(vel, 0L, popup, bottom, "res_data",
                                             res_register_, res_trap_);
       }
  if(!XtIsManaged(vel->vw.res_widget))
       {
       XtSetSensitive(vel->vw.res_undo, False);
       XtManageChild(vel->vw.res_widget);
       }
}


/*--------------------- lat routine ------------------------------------*/

void lat_dialog(void *data)
{
  VelStruct *vel = (VelStruct*)data;
  Widget popup, bottom;

  if(!vel->vw.lat_widget)
       {
       popup  = vel->vw.lat_widget = make_shell(vel->shell,"lat_shell",NULL);
       bottom = make_bottom(popup, "bottom");

       make_push    (bottom,"button_apply" ,LLSSHHDD,lat_apply_ ,NULL       );
       vel->vw.lat_undo = make_push(bottom,"button_undo",LLSSHHDD,
                                                     ff_read_tempfile,NULL);
       make_push    (bottom,"button_cancel",LLSSHHDD,ff_close_tempfile,popup);
       make_pushhelp(bottom,"button_help",NULL,NULL,HH);

       vel->vw.lat_box  = windowbox(vel, 0L, popup, bottom, "lat_data", 
                                             lat_register_, lat_trap_);
       }
  if(!XtIsManaged(vel->vw.lat_widget))
       {
       XtSetSensitive(vel->vw.lat_undo, False);
       XtManageChild(vel->vw.lat_widget);
       }
}



/*--------------------- lat2 routine ------------------------------------*/

void lat2_dialog(void *data)
{
  VelStruct *vel = (VelStruct*)data;
  Widget popup, bottom;

  if(!vel->vw.lat2_widget)
       {
       popup  = vel->vw.lat2_widget = make_shell(vel->shell,"lat2_shell",NULL);
       bottom = make_bottom(popup, "bottom");

       make_push    (bottom,"button_apply" ,LLSSHHDD,lat2_apply_ ,NULL       );
       vel->vw.lat2_undo = make_push(bottom,"button_undo",LLSSHHDD,
                                                     ff_read_tempfile,NULL);
       make_push    (bottom,"button_cancel",LLSSHHDD,ff_close_tempfile,popup);
       make_pushhelp(bottom,"button_help",NULL,NULL,HH);

       vel->vw.lat2_box  = windowbox(vel, 0L, popup, bottom, "lat2_data", 
                                             lat2_register_, lat2_trap_);
       }
  if(!XtIsManaged(vel->vw.lat2_widget))
       {
       XtSetSensitive(vel->vw.lat2_undo, False);
       XtManageChild(vel->vw.lat2_widget);
       }
}



/*--------------------- ray routine ------------------------------------*/

void ray_dialog(void *data)
{
  VelStruct *vel = (VelStruct*)data;

  standard_box(vel, &vel->vw.ray_widget, "ray_shell",
       ray_apply_,  &vel->vw.ray_box   , "ray_data",
       ray_register_, ray_trap_, False);
}



/*-------------------- set routine -------------------------------------*/

void set_dialog(void *data)
{
  VelStruct *vel = (VelStruct*)data;

  standard_box(vel, &vel->vw.set_widget, "set_shell",
       set_apply_,  &vel->vw.set_box   , "set_data",
       set_register_, set_trap_, False);
}



/*------------------- vfid routine -------------------------------------*/

void vfid_dialog(void *data)
{
  VelStruct *vel = (VelStruct*)data;

  standard_box(vel, &vel->vw.vfid_widget, "vfid_shell",
       vfid_apply_, &vel->vw.vfid_box   , "vfid_data",
       vfid_register_, vfid_trap_, False);
}



/*------------------- head routine -------------------------------------*/

void head_dialog(void *data)
{
  VelStruct *vel = (VelStruct*)data;

  standard_box(vel, &vel->vw.head_widget, "head_shell",
       head_apply_, &vel->vw.head_box   , "head_data",
       head_register_, head_trap_, False);
}



/*-------------------- del routine -------------------------------------*/

void del_dialog(void *data)
{
  VelStruct *vel = (VelStruct*)data;
  Widget popup, bottom;

  if(!vel->vw.del_widget)
       {
       popup  = vel->vw.del_widget = make_shell(vel->shell,"del_shell",NULL);
       bottom = make_bottom (popup, "bottom");

       make_pushquest(bottom,"button_apply" ,LLSSHHDD,del_apply_, NULL);
       vel->vw.del_undo = make_push(bottom,"button_undo",LLSSHHDD,
                                                ff_read_tempfile, NULL);
       make_push         (bottom,"button_cancel",LLSSHHDD,
                                                ff_close_tempfile, popup);
       make_pushhelp     (bottom,"button_help",NULL,NULL,HH);

       vel->vw.del_box  = windowbox(vel, 0L, popup, bottom, "del_data", 
                                             del_register_, del_trap_);
       }
  if(!XtIsManaged(vel->vw.del_widget))
       {
       XtSetSensitive(vel->vw.del_undo, False);
       XtManageChild(vel->vw.del_widget);
       }
}


/*-------------------- mult routine ------------------------------------*/

void mult_dialog(void *data)
{
  VelStruct *vel = (VelStruct*)data;
  Widget popup, bottom;

  if(!vel->vw.mult_widget)
       {
       popup  = vel->vw.mult_widget = make_shell(vel->shell,"mult_shell",NULL);
       bottom = make_bottom (popup, "bottom");

       make_push(bottom,"button_apply" ,LLSSHHDD,mult_apply_, NULL);
       vel->vw.mult_undo = make_push(bottom,"button_undo",LLSSHHDD,
                                                ff_read_tempfile, NULL);
       make_push    (bottom,"button_cancel",LLSSHHDD,ff_close_tempfile, popup);
       make_pushhelp(bottom,"button_help",NULL,NULL,HH);
       vel->vw.mult_box  = windowbox(vel, 0L, popup, bottom, "mult_data", 
                                             mult_register_, mult_trap_);
       }
  if(!XtIsManaged(vel->vw.mult_widget))
       {
       XtSetSensitive(vel->vw.mult_undo, False);
       XtManageChild(vel->vw.mult_widget);
       }
}


/*------------------- misc routine -------------------------------------*/

void misc_dialog(void *data)
{
  VelStruct *vel = (VelStruct*)data;

  standard_box(vel, &vel->vw.misc_widget, "misc_shell",
       misc_apply_, &vel->vw.misc_box   , "misc_data",
       misc_register_, misc_trap_, False);
}


/*----------------- offmute routine ------------------------------------*/


void offmute_dialog(void *data)
{
  VelStruct *vel = (VelStruct*)data;
  Widget popup, bottom;

  if(!vel->vw.offmute_widget)
       {
       popup = vel->vw.offmute_widget = make_shell(vel->shell,
                                               "offmute_shell",NULL);
       bottom = make_bottom (popup, "bottom");

       make_push    (bottom,"button_remove" ,LLSSHHDD,NULL, popup);
       make_pushhelp(bottom,"wbox_keyhelp",NULL,NULL,HH  );
       make_pushhelp(bottom,"button_help" ,NULL,NULL,HH  );

       vel->vw.offmute_box = windowbox(vel, 0L, popup, bottom, "offmute_data", 
                                       offmute_register_, offmute_trap_);
       }
  XtManageChild(vel->vw.offmute_widget);
}




/*--------------------- mig routine ------------------------------------*/

static float spacing = 110.0;
static long dipchoice = 1L;
static float dip1 = 10.0, dip2 = 10.0, dip3 = 25.0;
static long n = 0L;
static float aspacing[NMAX], atime1[NMAX], adip1[NMAX];
static float atime2[NMAX], adip2[NMAX], adip3[NMAX];
static float afeet[NMAX], anumtr[NMAX], adepth[NMAX];
static long nmax = NMAX;


static void mig_trap(void *box, long *ident, long *index, char *text,
                    long *nread, char *endkey)
{
  VelStruct *vel;

  get_velstruct_pointer_(&vel);
  if(!strcmp(endkey, "REDRAW"))
      {
      }
}


static void mig_reg(VelStruct *vel)
{
  wbox_creg(NULL,0,
"trace    --UNMIGRATED--   ---MIGRATED---    dip   LATERAL MIGRATION  migrated",
     &P0,1,6,0,0);
  wbox_rega(&n, &nmax, 2, 30);
  wbox_xrega(NULL    , 10, " "       , &P0, NULL    , &M44 ,0, 4, 0);
  wbox_frega(mig_trap, 11, "spacing ", &P0, aspacing, &P1  ,0, 5, 2);
  wbox_frega(mig_trap, 12, "time"    , &P0, atime1  , &P1  ,0, 7, 3);
  wbox_frega(mig_trap, 13, "dip(ms) ", &P0, adip1   , &P1  ,0, 5, 2);
  wbox_frega(mig_trap, 14, "time"    , &P0, atime2  , &P1  ,0, 7, 3);
  wbox_frega(mig_trap, 15, "dip(ms) ", &P0, adip2   , &P1  ,0, 5, 2);
  wbox_frega(mig_trap, 16, "(deg)  " , &P0, adip3   , &P1  ,0, 5, 2);
  wbox_frega(mig_trap, 17, "(feet)"  , &P0, afeet   , &M5  ,0, 8, 0);
  wbox_frega(mig_trap, 18, "(#tr)   ", &P0, anumtr  , &M5  ,0, 6, 0);
  wbox_frega(mig_trap, 19, "depth"   , &P0, adepth  , &M5  ,0, 8, 0);
}




static void spacing_trap(void*data, float*oldvar)
{
  VelStruct *vel = (VelStruct*)data;
  int i;
  for(i = 0; i < n; i++) { aspacing[i] = spacing; }
}


static void dipchoice_trap(void*data, long*oldvar)
{
  VelStruct *vel = (VelStruct*)data;
  int i;
  if(dipchoice == 1) { for(i = 0; i < n; i++) { adip1[i] = dip1; } }
  if(dipchoice == 2) { for(i = 0; i < n; i++) { adip2[i] = dip2; } }
  if(dipchoice == 3) { for(i = 0; i < n; i++) { adip3[i] = dip3; } }
}


static void dip1_trap(void*data, float*oldvar)
{
  VelStruct *vel = (VelStruct*)data;
  int i;
  dipchoice = 1;
  for(i = 0; i < n; i++) { adip1[i] = dip1; }
}


static void dip2_trap(void*data, float*oldvar)
{
  VelStruct *vel = (VelStruct*)data;
  int i;
  dipchoice = 2;
  for(i = 0; i < n; i++) { adip2[i] = dip2; }
}


static void dip3_trap(void*data, float*oldvar)
{
  VelStruct *vel = (VelStruct*)data;
  int i;
  dipchoice = 3;
  for(i = 0; i < n; i++) { adip3[i] = dip3; }
}


#define LABELA "Trace spacing in feet/meters:"
#define LABEL1 "Choose an unmigrated dip in ms/trace:"
#define LABEL2 "Choose a migrated dip in ms/trace:"
#define LABEL3 "Choose a dip in degrees:"

static void mig_topreg(VelStruct *vel, Widget parent)
{
  Widget wspacing, wlarge, wradio, wdips, w_not_func;

 wspacing=make_ftext2(parent,"spacing",LABELA,SSHHDD,spacing_trap,&spacing,6,2);
  wlarge = make_framed_row(parent, "large");
  wradio = make_radiobox  (wlarge, "radio");
  wdips  = make_column    (wlarge, "dips");
  make_radio(wradio, "choice1", LABEL1, SSHHDD, dipchoice_trap, &dipchoice, 1);
  make_radio(wradio, "choice2", LABEL2, SSHHDD, dipchoice_trap, &dipchoice, 2);
  make_radio(wradio, "choice3", LABEL3, SSHHDD, dipchoice_trap, &dipchoice, 3);
  make_ftext(wdips,"dip1",SSHHDD, dip1_trap, &dip1 , 6, 2);
  make_ftext(wdips,"dip2",SSHHDD, dip2_trap, &dip2 , 6, 2);
  make_ftext(wdips,"dip3",SSHHDD, dip3_trap, &dip3 , 6, 2);
  w_not_func = make_label(parent, "not_func", NOT_FUNC, NULL);

                      /*    left   right  top   bottom  */
  attach_widget(wspacing  ,parent,NULL,  parent  ,NULL, 10,0,10,0);
  attach_widget(wlarge    ,parent,NULL,  wspacing,NULL, 10,0,10,0);
  attach_widget(w_not_func,wlarge,NULL,  parent  ,NULL, 10,0,10,0);
}


void mig_dialog(void *data)
{
  VelStruct *vel = (VelStruct*)data;

  view_box(vel, &vel->vw.mig_widget, "mig_shell",
                &vel->vw.mig_box   , "mig_data" ,
                    True,    mig_topreg, mig_reg, mig_trap);
}



/*----------------- breadth routine ------------------------------------*/


static void breadth_trap(void *box, long *ident, long *index, char *text,
                    long *nread, char *endkey)
{
  VelStruct *vel;

  get_velstruct_pointer_(&vel);
  if(!strcmp(endkey, "REDRAW"))
      {
      }
}


static void breadth_reg(VelStruct *vel)
{
  static long n = 0L;
  static float adummy[NMAX];

  wbox_rega(&n, &nmax, 1, 2);
  wbox_xrega(NULL    , 10, " "    , &P0, NULL  , &M44 ,0, 4, 0);
  wbox_frega(mig_trap, 11, "dummy", &P0, adummy, &P1  ,0, 5, 2);
}


static void breadth_topreg(VelStruct *vel, Widget parent)
{
  make_label(parent, "not_yet", NOT_YET, NULL);
}


void breadth_dialog(void *data)
{
  VelStruct *vel = (VelStruct*)data;

  view_box(vel, &vel->vw.breadth_widget, "breadth_shell",
                &vel->vw.breadth_box   , "breadth_data" ,
                 False,      breadth_topreg, breadth_reg, breadth_trap);
}



/*----------------- err routine ----------------------------------------*/


static void err_trap(void *box, long *ident, long *index, char *text,
                    long *nread, char *endkey)
{
  VelStruct *vel;

  get_velstruct_pointer_(&vel);
  if(!strcmp(endkey, "REDRAW"))
      {
      }
}


static void err_reg(VelStruct *vel)
{
  static long n = 0L;
  static float adummy[NMAX];

  wbox_rega(&n, &nmax, 1, 2);
  wbox_xrega(NULL    , 10, " "    , &P0, NULL  , &M44 ,0, 4, 0);
  wbox_frega(mig_trap, 11, "dummy", &P0, adummy, &P1  ,0, 5, 2);
}


static void err_topreg(VelStruct *vel, Widget parent)
{
  make_label(parent, "not_yet", NOT_YET, NULL);
}


void err_dialog(void *data)
{
  VelStruct *vel = (VelStruct*)data;

  view_box(vel, &vel->vw.err_widget, "err_shell",
                &vel->vw.err_box   , "err_data" ,
               False,        err_topreg, err_reg, err_trap);
}



/*----------------- resid routine --------------------------------------*/


static void resid_trap(void *box, long *ident, long *index, char *text,
                    long *nread, char *endkey)
{
  VelStruct *vel;

  get_velstruct_pointer_(&vel);
  if(!strcmp(endkey, "REDRAW"))
      {
      }
}


static void resid_reg(VelStruct *vel)
{
  static long n = 0L;
  static float adummy[NMAX];

  wbox_rega(&n, &nmax, 1, 2);
  wbox_xrega(NULL    , 10, " "    , &P0, NULL  , &M44 ,0, 4, 0);
  wbox_frega(mig_trap, 11, "dummy", &P0, adummy, &P1  ,0, 5, 2);
}


static void resid_topreg(VelStruct *vel, Widget parent)
{
  make_label(parent, "not_yet", NOT_YET, NULL);
}


void resid_dialog(void *data)
{
  VelStruct *vel = (VelStruct*)data;

  view_box(vel, &vel->vw.resid_widget, "resid_shell",
                &vel->vw.resid_box   , "resid_data" ,
               False,        resid_topreg, resid_reg, resid_trap);
}



/*----------------- freznel routine ------------------------------------*/

typedef struct _FreznelStruct
  {
  float step, vel, freq;
  float t[NMAX], d[NMAX], v[NMAX], w[NMAX], x[NMAX];
  long n, nmax;
  Widget freznel_draw;
  } FreznelStruct;
static FreznelStruct fst;


static void freznel_trap(void *box, long *ident, long *index, char *text,
                    long *nread, char *endkey)
{
  VelStruct *vel;
  int i = *index - 1;

  vel = wbox_get_userdata(box);
/*
  get_velstruct_pointer_(&vel);
*/
  if(!strcmp(endkey, "REDRAW"))
      {
      }
  else if(!strcmp(endkey, "INSERTED"))
      {
      if(fst.t[i] == vel->vd->fnil) fst.t[i] = (i + 1) * fst.step;
      if(fst.d[i] == vel->vd->fnil) fst.d[i] = 0.5 * fst.vel * fst.t[i];
      if(fst.v[i] == vel->vd->fnil) fst.v[i] = fst.vel;
      fst.w[i] = fst.vel / fst.freq;
      fst.x[i] = sqrt(2.0 * fst.w[i] * fst.d[i]);
      do_plot(fst.freznel_draw);
      }
  else if(!strcmp(endkey, "REMOVED"))
      {
      do_plot(fst.freznel_draw);
      }
  else if(*nread >= 1)
      {
      switch(*ident)
           {
           case 11:
                     if(*index > fst.n) fst.v[i] = fst.vel;
                     fst.d[i] = 0.5 * fst.v[i] * fst.t[i];
                     break;
           case 12:
                     if(*index > fst.n) fst.v[i] = fst.vel;
                     fst.t[i] = 2.0 * fst.d[i] / fst.v[i];
                     break;
           case 13:
                     if(*index > fst.n) fst.t[i] = (i + 1) * fst.step;
                     fst.d[i] = 0.5 * fst.v[i] * fst.t[i];
                     break;
           }
      fst.w[i] = fst.v[i] / fst.freq;
      fst.x[i] = sqrt(2.0 * fst.w[i] * fst.d[i]);
      if(*index > fst.n) fst.n = *index;
      do_plot(fst.freznel_draw);
      }
  else if(!strcmp(endkey, "RETURN"))
      {
      do_plot(fst.freznel_draw);
      }
}


static void freznel_reg(VelStruct *vel)
{
  int i;

  fst.n = 7;
  fst.nmax = NMAX;
  for(i = 0; i < fst.n; i++) { fst.t[i] = (i + 1) * fst.step;   }
  for(i = 0; i < fst.n; i++) { fst.d[i] = 0.5 * fst.vel * fst.t[i]; }
  for(i = 0; i < fst.n; i++) { fst.v[i] = fst.vel;            }
  for(i = 0; i < fst.n; i++) { fst.w[i] = fst.v[i] / fst.freq; }
  for(i = 0; i < fst.n; i++) { fst.x[i] = sqrt(2.0 * fst.w[i] * fst.d[i]); }

  wbox_creg(NULL, 0, "freznel zone", &P0, 1, 42, 0, 0);
  wbox_rega(&fst.n, &fst.nmax, 2, 20);
  wbox_xrega(NULL        , 10, " "          , &P0, NULL , &M44 ,0, 4, 0);
  wbox_frega(freznel_trap, 11, "time"       , &P0, fst.t, &P1  ,0, 6, 3);
  wbox_frega(freznel_trap, 12, "depth"      , &P0, fst.d, &P1  ,0, 6, 0);
  wbox_frega(freznel_trap, 13, "velocity "  , &P0, fst.v, &P1  ,0, 5, 0);
  wbox_frega(NULL        , 14, "wavelength ", &P0, fst.w, &M1  ,0, 4, 0);
  wbox_frega(NULL        , 15, "diameter"   , &P0, fst.x, &M1  ,0, 5, 0);
}


static void fst_freq(void *data, float *oldvar)
{
  VelStruct *vel = (VelStruct*)data;
  int i;
  if(fst.freq < 1.0) fst.freq = 1.0;
  for(i = 0; i < fst.n; i++) { fst.w[i] = fst.v[i] / fst.freq; }
  for(i = 0; i < fst.n; i++) { fst.x[i] = sqrt(2.0 * fst.w[i] * fst.d[i]); }
  do_plot(fst.freznel_draw);
}


static void fst_vel(void *data, float *oldvar)
{
  VelStruct *vel = (VelStruct*)data;
  int i;
  float dummy;
  if(fst.vel < 1.0) fst.vel = 1.0;
  for(i = 0; i < fst.n; i++) { fst.d[i] = 0.5 * fst.vel * fst.t[i]; }
  for(i = 0; i < fst.n; i++) { fst.v[i] = fst.vel;            }
  fst_freq(vel, &dummy);
}


static void fst_step(void *data, float *oldvar)
{
  VelStruct *vel = (VelStruct*)data;
  int i;
  float dummy;
  if(fst.step < 0.001) fst.step = 0.001;
  for(i = 0; i < fst.n; i++) { fst.t[i] = (i + 1) * fst.step;   }
  fst_vel(vel, &dummy);
}


static void freznel_topreg(VelStruct *vel, Widget parent)
{
  Widget row = make_row(parent, "fst");

  fst.step = 0.5;
  fst.vel  = 10000.0;
  fst.freq = 35.0;

  make_ftext2(row,"fst", "time step:"  ,SSHHDD,fst_step,&fst.step,5,3);
  make_ftext2(row,"fst", "  velocity:" ,SSHHDD,fst_vel ,&fst.vel ,6,0);
  make_ftext2(row,"fst", "  frequency:",SSHHDD,fst_freq,&fst.freq,3,0);
}


void freznel_dialog(void *data)
{
  VelStruct *vel = (VelStruct*)data;
  Widget popup, top, paned, middle, draw, bottom;

  if(!vel->vw.freznel_widget)
       {
       popup  = make_shell (vel->shell, "freznel_shell", NULL);
       top    = make_column(popup, "top"   );
       paned = XmCreatePanedWindow(popup, "paned", NULL, 0);
       XtManageChild(paned);
       middle = make_draw  (paned, "middle");
       draw   = make_plot  (paned, "graph", HH);
       register_xaxis(draw, "diameter", fst.x, &fst.n, 0, 0);
       register_yaxis(draw, "time"    , fst.t, &fst.n, 3, 3);
       register_yaxis(draw, "depth"   , fst.d, &fst.n, 0, 0);
       bottom = make_bottom(popup, "bottom");
       attach_widget(top   , popup, popup, popup, NULL  , 0,0,0,0);
       attach_widget(paned , popup, popup, top  , bottom, 0,0,0,0);

       freznel_topreg(vel, top);

       make_push    (bottom,"button_remove",LLSSHHDD ,NULL, popup);
       make_pushhelp(bottom,"wbox_keyhelp",NULL,NULL, HH);
       make_pushhelp(bottom,"button_help" ,NULL,NULL, HH);

       vel->vw.freznel_box = wbox_create1(freznel_trap, 3L);
       freznel_reg(vel);
       wbox_create2("freznel_box", NULL, popup, middle, HH, NULL, NULL);
       wbox_put_userdata(vel->vw.freznel_box, vel);

       vel->vw.freznel_widget = popup;
       fst.freznel_draw   = draw ;
       }
  XtManageChild(vel->vw.freznel_widget);
}




/*----------------- dip routine ----------------------------------------*/


static void dip_trap(void *box, long *ident, long *index, char *text,
                    long *nread, char *endkey)
{
  VelStruct *vel;

  get_velstruct_pointer_(&vel);
  if(!strcmp(endkey, "REDRAW"))
      {
      }
}


static void dip_reg(VelStruct *vel)
{
  static long n = 0L;
  static float adummy[NMAX];

  wbox_rega(&n, &nmax, 1, 2);
  wbox_xrega(NULL    , 10, " "    , &P0, NULL  , &M44 ,0, 4, 0);
  wbox_frega(mig_trap, 11, "dummy", &P0, adummy, &P1  ,0, 5, 2);
}


static void dip_topreg(VelStruct *vel, Widget parent)
{
  make_label(parent, "not_yet", NOT_YET, NULL);
}


void dip_dialog(void *data)
{
  VelStruct *vel = (VelStruct*)data;

  view_box(vel, &vel->vw.dip_widget, "dip_shell",
                &vel->vw.dip_box   , "dip_data" ,
                    False,   dip_topreg, dip_reg, dip_trap);
}


/*--------------------------- end --------------------------------------*/

