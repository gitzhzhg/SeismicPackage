/*<CPS_v1 type="HEADER_FILE"/>
!------------------------------- memman.h ----------------------------------
!------------------------------- memman.h ----------------------------------
!------------------------------- memman.h ----------------------------------
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
! Name       : MEMMAN.H
! Category   : memory
! Written    : 2002-09-24   by: Charles C. Burch
! Revised    : 2002-09-24   by: Charles C. Burch
! Maturity   : production   2003-06-17
! Purpose    : Prototype for routines in memman_crou
! References : These routines are called from within memman and general use
!-------------------------------------------------------------------------------
!</brief_doc>

!<history_doc>
!-------------------------------------------------------------------------------
!                       HEADER FILE REVISION HISTORY
!     Date        Author          Description
!     ----        ------          -----------
!  1. 2003-06-17  Chuck C. Burch  Initial version.
!-------------------------------------------------------------------------------
!</history_doc>
*/

/*--------------------------- start of coding ------------------------------*/
/*--------------------------- start of coding ------------------------------*/
/*--------------------------- start of coding ------------------------------*/


#ifndef _MEMMAN_H_
#define _MEMMAN_H_

#include "c2f_interface.h"

#ifdef NEED_UNDERSCORE
#define memman_change_info_c           memman_change_info_c_
#define memman_check_variable_c        memman_check_variable_c_
#define memman_dump_tracking_c         memman_dump_tracking_c_
#define memman_exit_tracking_c         memman_exit_tracking_c_
#define memman_get_info_c              memman_get_info_c_
#define memman_get_memory_allocated_c  memman_get_memory_allocated_c_
#define memman_loc_char_c              memman_loc_char_c_
#define memman_loc_integer_c           memman_loc_integer_c_
#define memman_loc_real_c              memman_loc_real_c_
#define memman_loc_double_c            memman_loc_double_c_
#define memman_loc_complex_c           memman_loc_complex_c_
#define memman_loc_logical_c           memman_loc_logical_c_
#define memman_ptrloc_c1_c             memman_ptrloc_c1_c_
#define memman_ptrloc_c2_c             memman_ptrloc_c2_c_
#define memman_ptrloc_c3_c             memman_ptrloc_c3_c_
#define memman_ptrloc_c4_c             memman_ptrloc_c4_c_
#define memman_ptrloc_c5_c             memman_ptrloc_c5_c_
#define memman_ptrloc_i1_c             memman_ptrloc_i1_c_
#define memman_ptrloc_i2_c             memman_ptrloc_i2_c_
#define memman_ptrloc_i3_c             memman_ptrloc_i3_c_
#define memman_ptrloc_i4_c             memman_ptrloc_i4_c_
#define memman_ptrloc_i5_c             memman_ptrloc_i5_c_
#define memman_ptrloc_r1_c             memman_ptrloc_r1_c_
#define memman_ptrloc_r2_c             memman_ptrloc_r2_c_
#define memman_ptrloc_r3_c             memman_ptrloc_r3_c_
#define memman_ptrloc_r4_c             memman_ptrloc_r4_c_
#define memman_ptrloc_r5_c             memman_ptrloc_r5_c_
#define memman_ptrloc_d1_c             memman_ptrloc_d1_c_
#define memman_ptrloc_d2_c             memman_ptrloc_d2_c_
#define memman_ptrloc_d3_c             memman_ptrloc_d3_c_
#define memman_ptrloc_d4_c             memman_ptrloc_d4_c_
#define memman_ptrloc_d5_c             memman_ptrloc_d5_c_
#define memman_ptrloc_z1_c             memman_ptrloc_z1_c_
#define memman_ptrloc_z2_c             memman_ptrloc_z2_c_
#define memman_ptrloc_z3_c             memman_ptrloc_z3_c_
#define memman_ptrloc_z4_c             memman_ptrloc_z4_c_
#define memman_ptrloc_z5_c             memman_ptrloc_z5_c_
#define memman_ptrloc_l1_c             memman_ptrloc_l1_c_
#define memman_ptrloc_l2_c             memman_ptrloc_l2_c_
#define memman_ptrloc_l3_c             memman_ptrloc_l3_c_
#define memman_ptrloc_l4_c             memman_ptrloc_l4_c_
#define memman_ptrloc_l5_c             memman_ptrloc_l5_c_
#define memman_set_print_mode_c        memman_set_print_mode_c_
#define memman_tracking_all_c          memman_tracking_all_c_
#define memman_tracking_del_c          memman_tracking_del_c_
#define memman_tracking_free_c         memman_tracking_free_c_
#endif

#ifdef NEED_CAPITALS
#define memman_change_info_c           MEMMAN_CHANGE_INFO_C
#define memman_check_variable_c        MEMMAN_CHECK_VARIABLE_C
#define memman_dump_tracking_c         MEMMAN_DUMP_TRACKING_C
#define memman_exit_tracking_c         MEMMAN_EXIT_TRACKING_C
#define memman_get_info_c              MEMMAN_GET_INFO_C
#define memman_get_memory_allocated_c  MEMMAN_GET_MEMORY_ALLOCATED_C
#define memman_loc_char_c              MEMMAN_LOC_CHAR_C
#define memman_loc_integer_c           MEMMAN_LOC_INTEGER_C
#define memman_loc_real_c              MEMMAN_LOC_REAL_C
#define memman_loc_double_c            MEMMAN_LOC_DOUBLE_C
#define memman_loc_complex_c           MEMMAN_LOC_COMPLEX_C
#define memman_loc_logical_c           MEMMAN_LOC_LOGICAL_C
#define memman_ptrloc_c1_c             MEMMAN_PTRLOC_C1_C
#define memman_ptrloc_c2_c             MEMMAN_PTRLOC_C2_C
#define memman_ptrloc_c3_c             MEMMAN_PTRLOC_C3_C
#define memman_ptrloc_c4_c             MEMMAN_PTRLOC_C4_C
#define memman_ptrloc_c5_c             MEMMAN_PTRLOC_C5_C
#define memman_ptrloc_i1_c             MEMMAN_PTRLOC_I1_C
#define memman_ptrloc_i2_c             MEMMAN_PTRLOC_I2_C
#define memman_ptrloc_i3_c             MEMMAN_PTRLOC_I3_C
#define memman_ptrloc_i4_c             MEMMAN_PTRLOC_I4_C
#define memman_ptrloc_i5_c             MEMMAN_PTRLOC_I5_C
#define memman_ptrloc_r1_c             MEMMAN_PTRLOC_R1_C
#define memman_ptrloc_r2_c             MEMMAN_PTRLOC_R2_C
#define memman_ptrloc_r3_c             MEMMAN_PTRLOC_R3_C
#define memman_ptrloc_r4_c             MEMMAN_PTRLOC_R4_C
#define memman_ptrloc_r5_c             MEMMAN_PTRLOC_R5_C
#define memman_ptrloc_d1_c             MEMMAN_PTRLOC_D1_C
#define memman_ptrloc_d2_c             MEMMAN_PTRLOC_D2_C
#define memman_ptrloc_d3_c             MEMMAN_PTRLOC_D3_C
#define memman_ptrloc_d4_c             MEMMAN_PTRLOC_D4_C
#define memman_ptrloc_d5_c             MEMMAN_PTRLOC_D5_C
#define memman_ptrloc_z1_c             MEMMAN_PTRLOC_Z1_C
#define memman_ptrloc_z2_c             MEMMAN_PTRLOC_Z2_C
#define memman_ptrloc_z3_c             MEMMAN_PTRLOC_Z3_C
#define memman_ptrloc_z4_c             MEMMAN_PTRLOC_Z4_C
#define memman_ptrloc_z5_c             MEMMAN_PTRLOC_Z5_C
#define memman_ptrloc_l1_c             MEMMAN_PTRLOC_L1_C
#define memman_ptrloc_l2_c             MEMMAN_PTRLOC_L2_C
#define memman_ptrloc_l3_c             MEMMAN_PTRLOC_L3_C
#define memman_ptrloc_l4_c             MEMMAN_PTRLOC_L4_C
#define memman_ptrloc_l5_c             MEMMAN_PTRLOC_L5_C
#define memman_set_print_mode_c        MEMMAN_SET_PRINT_MODE_C
#define memman_tracking_all_c          MEMMAN_TRACKING_ALL_C
#define memman_tracking_del_c          MEMMAN_TRACKING_DEL_C
#define memman_tracking_free_c         MEMMAN_TRACKING_FREE_C
#endif

#ifdef __cplusplus
extern "C" {
#endif

/*char *memman_h_ident =*/
/*"$Id: memman.h,v 1.1 2003/06/16 14:20:21 Burch prod sps $";*/

/*------------------------ start of prototypes ---------------------------*/
/*------------------------ start of prototypes ---------------------------*/
/*------------------------ start of prototypes ---------------------------*/

/*        function prototypes for memman_crou  routines    */

void    memman_change_info_c(INTEGER*, INTEGER*, INTEGER*, char*);  
INTEGER memman_check_variable_c(INTEGER*, INTEGER*, INTEGER*, char*);  
void    memman_dump_tracking_c(char*); 
void    memman_exit_tracking_c(INTEGER*);
int     memman_free(void**, char*);
void    memman_get_info_c(INTEGER*, INTEGER*, INTEGER*, char*);
INTEGER memman_get_memory_allocated_c();
INTEGER memman_loc_char_c(void*);
INTEGER memman_loc_integer_c(void*);
INTEGER memman_loc_real_c(void*);
INTEGER memman_loc_double_c(void*);
INTEGER memman_loc_complex_c(void*);
INTEGER memman_loc_logical_c(void*);
int     memman_malloc(void**, long, char*);
void    memman_null(void**, char*);
INTEGER memman_ptrloc_c1_c(void*);
INTEGER memman_ptrloc_c2_c(void*);
INTEGER memman_ptrloc_c3_c(void*);
INTEGER memman_ptrloc_c4_c(void*);
INTEGER memman_ptrloc_c5_c(void*);
INTEGER memman_ptrloc_i1_c(void*);
INTEGER memman_ptrloc_i2_c(void*);
INTEGER memman_ptrloc_i3_c(void*);
INTEGER memman_ptrloc_i4_c(void*);
INTEGER memman_ptrloc_i5_c(void*);
INTEGER memman_ptrloc_r1_c(void*);
INTEGER memman_ptrloc_r2_c(void*);
INTEGER memman_ptrloc_r3_c(void*);
INTEGER memman_ptrloc_r4_c(void*);
INTEGER memman_ptrloc_r5_c(void*);
INTEGER memman_ptrloc_d1_c(void*);
INTEGER memman_ptrloc_d2_c(void*);
INTEGER memman_ptrloc_d3_c(void*);
INTEGER memman_ptrloc_d4_c(void*);
INTEGER memman_ptrloc_d5_c(void*);
INTEGER memman_ptrloc_z1_c(void*);
INTEGER memman_ptrloc_z2_c(void*);
INTEGER memman_ptrloc_z3_c(void*);
INTEGER memman_ptrloc_z4_c(void*);
INTEGER memman_ptrloc_z5_c(void*);
INTEGER memman_ptrloc_l1_c(void*);
INTEGER memman_ptrloc_l2_c(void*);
INTEGER memman_ptrloc_l3_c(void*);
INTEGER memman_ptrloc_l4_c(void*);
INTEGER memman_ptrloc_l5_c(void*);
int     memman_realloc(void**, long, char*);
void    memman_set_print_mode_c(INTEGER*);
INTEGER memman_tracking_all_c(INTEGER*, INTEGER*, INTEGER*, char*);
INTEGER memman_tracking_del_c(INTEGER*, INTEGER*, INTEGER*, char*);
void    memman_tracking_free_c(INTEGER*);

/*------------------------- end of prototypes ---------------------------*/
/*------------------------- end of prototypes ---------------------------*/
/*------------------------- end of prototypes ---------------------------*/

#ifdef __cplusplus
}
#endif

#endif

/*-------------------------------- end ------------------------------------*/
/*-------------------------------- end ------------------------------------*/
/*-------------------------------- end ------------------------------------*/
