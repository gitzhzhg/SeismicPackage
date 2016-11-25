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

/*------------------------- vel_popups.h --------------------------------*/

#ifndef _VEL_POPUPS_H
#define _VEL_POPUPS_H

#include "c2f_interface.h"

/*-------- fortran subroutine spelling adjustments for VMS --------------*/

#if (VMS || _AIX || __hpux)
#define manage_read_filebox_       manage_read_filebox
#define adjust_arrow_sensitivity_  adjust_arrow_sensitivity
#define pick_undo_sensitize_       pick_undo_sensitize
#define res_undo_sensitize_        res_undo_sensitize
#define lat_undo_sensitize_        lat_undo_sensitize
#define lat2_undo_sensitize_       lat2_undo_sensitize
#define del_undo_sensitize_        del_undo_sensitize
#define mult_undo_sensitize_       mult_undo_sensitize
#endif

/*-------- fortran subroutine spelling adjustments for CRAY -------------*/

#ifdef NEED_CAPITALS
#define manage_read_filebox_       MANAGE_READ_FILEBOX
#define adjust_arrow_sensitivity_  ADJUST_ARROW_SENSITIVITY
#define pick_undo_sensitize_       PICK_UNDO_SENSITIZE
#define res_undo_sensitize_        RES_UNDO_SENSITIZE
#define lat_undo_sensitize_        LAT_UNDO_SENSITIZE
#define lat2_undo_sensitize_       LAT2_UNDO_SENSITIZE
#define del_undo_sensitize_        DEL_UNDO_SENSITIZE
#define mult_undo_sensitize_       MULT_UNDO_SENSITIZE
#endif



/*-------------------- function prototypes -----------------------------*/

#ifdef __cplusplus
extern "C" {
#endif

void   quit_vel_popups          (void *vel, long *error);
void   start_vel_popups         (void *vel, long am_in_va);

void   manage_read_filebox_     ();
void   adjust_arrow_sensitivity_();
void   pick_undo_sensitize_     ();
void   res_undo_sensitize_      ();
void   lat_undo_sensitize_      ();
void   lat2_undo_sensitize_     ();
void   del_undo_sensitize_      ();
void   mult_undo_sensitize_     ();
 
void    read_dialog(void *vel);
void    save_dialog(void *vel);

void    pick_dialog(void *vel);
void     fun_dialog(void *vel);
void     set_dialog(void *vel);
void    vfid_dialog(void *vel);
void    head_dialog(void *vel);
void     res_dialog(void *vel);
void     lat_dialog(void *vel);
void    lat2_dialog(void *vel);
void     ray_dialog(void *vel);
void     del_dialog(void *vel);
void    mult_dialog(void *vel);
void    misc_dialog(void *vel);
void offmute_dialog(void *vel);

void    info_dialog(void *vel);
void     tol_dialog(void *vel);

void     mig_dialog(void *vel);
void breadth_dialog(void *vel);
void     err_dialog(void *vel);
void   resid_dialog(void *vel);
void freznel_dialog(void *vel);
void     dip_dialog(void *vel);

#ifdef __cplusplus
}
#endif



/*-------------- structure needed by vel_popups.c ----------------------*/

typedef struct _VwStruct
    {
    Widget    read_widget;     void *   read_box;
    Widget    save_widget;     void *   save_box;

    Widget    pick_widget;     void *   pick_box;
    Widget     fun_widget;     void *    fun_box;
    Widget     set_widget;     void *    set_box;
    Widget    vfid_widget;     void *   vfid_box;
    Widget    head_widget;     void *   head_box;
    Widget     res_widget;     void *    res_box;
    Widget     lat_widget;     void *    lat_box;
    Widget    lat2_widget;     void *   lat2_box;
    Widget     ray_widget;     void *    ray_box;
    Widget     del_widget;     void *    del_box;
    Widget    mult_widget;     void *   mult_box;
    Widget    misc_widget;     void *   misc_box;
    Widget offmute_widget;     void *offmute_box;

    Widget    info_widget;     void *   info_box;
    Widget     tol_widget;

    Widget     mig_widget;     void *    mig_box;
    Widget breadth_widget;     void *breadth_box;
    Widget     err_widget;     void *    err_box;
    Widget   resid_widget;     void *  resid_box;
    Widget freznel_widget;     void *freznel_box;
    Widget     dip_widget;     void *    dip_box;

    Widget           prev;
    Widget           next;
    Widget      pick_undo;
    Widget       res_undo;
    Widget       lat_undo;
    Widget      lat2_undo;
    Widget       del_undo;
    Widget      mult_undo;
    Widget        fwidget;
    } VwStruct; 


#endif

/*--------------------------- end --------------------------------------*/

