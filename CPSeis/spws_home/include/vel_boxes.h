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


/*------------------------- vel_boxes.h ---------------------------------*/

#ifndef _VEL_BOXES_H
#define _VEL_BOXES_H

#include "vel_data.h"
#include "c2f_interface.h"


/*-------- fortran subroutine spelling adjustments for VMS --------------*/

/*
#if (ultrix || sun || __sgi)
*/
#ifdef NEED_UNDERSCORE
#define get_vbstruct_pointer       get_vbstruct_pointer_
#endif

/*
#if (VMS || _AIX || __hpux)
*/
#ifdef REMOVE_UNDERSCORE
#define start_vel_boxes_           start_vel_boxes
#define pick_prev_                 pick_prev
#define pick_next_                 pick_next
#define pick_undo_                 pick_undo
#define fun_deselect_              fun_deselect
#define read_getfile_              read_getfile
#define register_active_velfun_    register_active_velfun
#define read_register_             read_register
#define save_register_             save_register
#define pick_register_             pick_register
#define fun_register_              fun_register
#define set_register_              set_register
#define vfid_register_             vfid_register
#define head_register_             head_register
#define res_register_              res_register
#define lat_register_              lat_register
#define lat2_register_             lat2_register
#define ray_register_              ray_register
#define del_register_              del_register
#define mult_register_             mult_register
#define misc_register_             misc_register
#define offmute_register_          offmute_register
#define read_trap_                 read_trap
#define save_trap_                 save_trap
#define pick_trap_                 pick_trap
#define fun_trap_                  fun_trap
#define set_trap_                  set_trap
#define vfid_trap_                 vfid_trap
#define head_trap_                 head_trap
#define res_trap_                  res_trap
#define lat_trap_                  lat_trap
#define lat2_trap_                 lat2_trap
#define ray_trap_                  ray_trap
#define del_trap_                  del_trap
#define mult_trap_                 mult_trap
#define misc_trap_                 misc_trap
#define offmute_trap_              offmute_trap
#define read_apply_                read_apply
#define save_apply_                save_apply
#define pick_apply_                pick_apply
#define fun_apply_                 fun_apply
#define set_apply_                 set_apply
#define vfid_apply_                vfid_apply
#define head_apply_                head_apply
#define res_apply_                 res_apply
#define lat_apply_                 lat_apply
#define lat2_apply_                lat2_apply
#define ray_apply_                 ray_apply
#define del_apply_                 del_apply
#define mult_apply_                mult_apply
#define misc_apply_                misc_apply
#define offmute_apply_             offmute_apply
#endif

/*-------- fortran subroutine spelling adjustments for CRAY -------------*/
/*
#ifdef CRAY
*/
#ifdef NEED_CAPITALS
#define start_vel_boxes_           START_VEL_BOXES
#define get_vbstruct_pointer       GET_VBSTRUCT_POINTER
#define pick_prev_                 PICK_PREV
#define pick_next_                 PICK_NEXT
#define pick_undo_                 PICK_UNDO
#define fun_deselect_              FUN_DESELECT
#define read_getfile_              READ_GETFILE
#define register_active_velfun_    REGISTER_ACTIVE_VELFUN
#define read_register_             READ_REGISTER
#define save_register_             SAVE_REGISTER
#define pick_register_             PICK_REGISTER
#define fun_register_              FUN_REGISTER
#define set_register_              SET_REGISTER
#define vfid_register_             VFID_REGISTER
#define head_register_             HEAD_REGISTER
#define res_register_              RES_REGISTER
#define lat_register_              LAT_REGISTER
#define lat2_register_             LAT2_REGISTER
#define ray_register_              RAY_REGISTER
#define del_register_              DEL_REGISTER
#define mult_register_             MULT_REGISTER
#define misc_register_             MISC_REGISTER
#define offmute_register_          OFFMUTE_REGISTER
#define read_trap_                 READ_TRAP
#define save_trap_                 SAVE_TRAP
#define pick_trap_                 PICK_TRAP
#define fun_trap_                  FUN_TRAP
#define set_trap_                  SET_TRAP
#define vfid_trap_                 VFID_TRAP
#define head_trap_                 HEAD_TRAP
#define res_trap_                  RES_TRAP
#define lat_trap_                  LAT_TRAP
#define lat2_trap_                 LAT2_TRAP
#define ray_trap_                  RAY_TRAP
#define del_trap_                  DEL_TRAP
#define mult_trap_                 MULT_TRAP
#define misc_trap_                 MISC_TRAP
#define offmute_trap_              OFFMUTE_TRAP
#define read_apply_                READ_APPLY
#define save_apply_                SAVE_APPLY
#define pick_apply_                PICK_APPLY
#define fun_apply_                 FUN_APPLY
#define set_apply_                 SET_APPLY
#define vfid_apply_                VFID_APPLY
#define head_apply_                HEAD_APPLY
#define res_apply_                 RES_APPLY
#define lat_apply_                 LAT_APPLY
#define lat2_apply_                LAT2_APPLY
#define ray_apply_                 RAY_APPLY
#define del_apply_                 DEL_APPLY
#define mult_apply_                MULT_APPLY
#define misc_apply_                MISC_APPLY
#define offmute_apply_             OFFMUTE_APPLY
#endif



/*-------------------- function prototypes -----------------------------*/

#ifdef __cplusplus
extern "C" {                          /* for C++ */
#endif


void   start_vel_boxes_();
void   get_vbstruct_pointer();
void   pick_prev_      ();
void   pick_next_      ();
void   pick_undo_      ();
void   fun_deselect_   ();
void   read_getfile_   ();
 
void    read_register_(),    read_trap_(),    read_apply_();
void    save_register_(),    save_trap_(),    save_apply_();
void    pick_register_(),    pick_trap_(),    pick_apply_();
void     fun_register_(),     fun_trap_(),     fun_apply_();
void     set_register_(),     set_trap_(),     set_apply_();
void    vfid_register_(),    vfid_trap_(),    vfid_apply_();
void    head_register_(),    head_trap_(),    head_apply_();
void     res_register_(),     res_trap_(),     res_apply_();
void     lat_register_(),     lat_trap_(),     lat_apply_();
void    lat2_register_(),    lat2_trap_(),    lat2_apply_();
void     ray_register_(),     ray_trap_(),     ray_apply_();
void     del_register_(),     del_trap_(),     del_apply_();
void    mult_register_(),    mult_trap_(),    mult_apply_();
void    misc_register_(),    misc_trap_(),    misc_apply_();
void offmute_register_(), offmute_trap_(), offmute_apply_();


#ifdef __cplusplus
}
#endif
 
 

/*-------------- structure which matches common block vb ---------------*/

typedef struct _VbStruct
     {
      long vel;          /* pointer to main application structure. */
      long autosave;     /* automatic workfile save  if not zero. */
      long am_in_va;     /* =1 if this is program va, =0 if is vel */

      long ireadbox;     /* windowbox number for read_box. */
      long isavebox;     /* windowbox number for save_box. */
      long ipickbox;     /* windowbox number for pick_box. */
      long ifunbox;      /* windowbox number for fun_box. */
      long isetbox;      /* windowbox number for fun_box. */
      long ivfidbox;     /* windowbox number for vfid_box. */
      long iheadbox;     /* windowbox number for head_box. */
      long iresbox;      /* windowbox number for res_box. */
      long ilatbox;      /* windowbox number for lat_box. */
      long ilat2box;     /* windowbox number for lat2_box. */
      long iraybox;      /* windowbox number for ray_box. */
      long idelbox;      /* windowbox number for del_box. */
      long imultbox;     /* windowbox number for mult_box. */
      long imiscbox;     /* windowbox number for misc_box. */
      long ioffmutebox;  /* windowbox number for offmute_box. */
      long iinfobox;     /* windowbox number for info_box. */

      long nselect,nerrmsg,nraymsg,kode[NFUNMAX],sifun,cifun;

      char read_filename[80];  /* name of velocity file to read. */
      char save_filename[80];  /* name of velocity file to save. */
      char work_filename[40];  /* name of automatic workfile. */
     } VbStruct;

  

#endif

/*--------------------------- end --------------------------------------*/

