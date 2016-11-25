/*<CPS_v1 type="HEADER_FILE"/>
!------------------------------- pcpsx.h ----------------------------------
!------------------------------- pcpsx.h ----------------------------------
!------------------------------- pcpsx.h ----------------------------------
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
! Name       : PCPSX.H
! Category   : main_prog
! Written    : 2002-04-03   by: Charles C. Burch
! Revised    : 2004-03-15   by: Charles C. Burch
! Maturity   : production
! Purpose    : Prototype for routines in pcpsx_crou
! References : These routines are called from within pcps
!-------------------------------------------------------------------------------
!</brief_doc>

!<history_doc>
!-------------------------------------------------------------------------------
!                       HEADER FILE REVISION HISTORY
!     Date        Author     Description
!     ----        ------     -----------
!  2. 2004-03-15  C C Burch  Eliminated routines moved to unix_crou.c.
!                            Add line_break/work_group to buffer link list data.
!  1. 2002-04-03  C C Burch  Initial version.
!-------------------------------------------------------------------------------
!</history_doc>
*/

/*--------------------------- start of coding ------------------------------*/
/*--------------------------- start of coding ------------------------------*/
/*--------------------------- start of coding ------------------------------*/


#ifndef _PCPSX_H_
#define _PCPSX_H_

#include "c2f_interface.h"
#ifndef CHARACTER
#define CHARACTER char
#endif

#ifdef NEED_UNDERSCORE
#define pcpsx_deleteall_buffer_data_c pcpsx_deleteall_buffer_data_c_
#define pcpsx_dump_buffer_data_c      pcpsx_dump_buffer_data_c_
#define pcpsx_dump_trace_file_c       pcpsx_dump_trace_file_c_
#define pcpsx_dump_trace_list_c       pcpsx_dump_trace_list_c_
#define pcpsx_extract_buffer_data_c   pcpsx_extract_buffer_data_c_
#define pcpsx_get_buffer_data_c       pcpsx_get_buffer_data_c_
#define pcpsx_put_buffer_data_c       pcpsx_put_buffer_data_c_
#define pcpsx_put_trace_list_c        pcpsx_put_trace_list_c_
#endif

#ifdef NEED_CAPITALS
#define pcpsx_deleteall_buffer_data_c PCPSX_DELETEALL_BUFFER_DATA_C
#define pcpsx_dump_buffer_data_c      PCPSX_DUMP_BUFFER_DATA_C
#define pcpsx_dump_trace_file_c       PCPSX_DUMP_TRACE_FILE_C
#define pcpsx_dump_trace_list_c       PCPSX_DUMP_TRACE_LIST_C
#define pcpsx_extract_buffer_data_c   PCPSX_EXTRACT_BUFFER_DATA_C
#define pcpsx_get_buffer_data_c       PCPSX_GET_BUFFER_DATA_C
#define pcpsx_put_buffer_data_c       PCPSX_PUT_BUFFER_DATA_C
#define pcpsx_put_trace_list_c        PCPSX_PUT_TRACE_LIST_C
#endif

/*------------------------ start of functions ----------------------------*/
/*------------------------ start of functions ----------------------------*/
/*------------------------ start of functions ----------------------------*/

#ifdef __cplusplus
extern "C" {
#endif

/*char *pcpsx_h_ident =*/
/*"$Id: pcpsx.h,v 1.2 2004/03/15 12:36:33 Burch prod sps $";*/

/*------------------------ start of prototypes ---------------------------*/
/*------------------------ start of prototypes ---------------------------*/
/*------------------------ start of prototypes ---------------------------*/

/*        function prototypes for pcpsx_crou  routines    */

void pcpsx_deleteall_buffer_data_c();
void pcpsx_dump_buffer_data_c(char *, INTEGER*, char *, INTEGER*);
void pcpsx_dump_trace_file_c(CHARACTER*, INTEGER*);
void pcpsx_dump_trace_list_c(CHARACTER*, INTEGER*);
void pcpsx_extract_buffer_data_c(INTEGER *entry, INTEGER *instance, 
  INTEGER *trace_group, INTEGER *ntr, DOUBLE *hdr, INTEGER *hdr_len, 
  REAL *trc, INTEGER *trc_len, INTEGER *work_group, INTEGER *lb);
void pcpsx_get_buffer_data_c(INTEGER *entry, INTEGER *instance, 
  INTEGER *trace_group, INTEGER *ntr, INTEGER *work_group);
void pcpsx_init_c();
void pcpsx_put_buffer_data_c(INTEGER *instance, INTEGER *trace_group,
  INTEGER *ntr, DOUBLE *hdr, INTEGER *hdr_len, REAL *trc, INTEGER *trc_len, 
  INTEGER *work_group, INTEGER *isw, INTEGER *lb);
void pcpsx_put_trace_list_c(DOUBLE*, INTEGER*, INTEGER*, INTEGER*, char*,
  INTEGER *);

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
