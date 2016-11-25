/*<CPS_v1 type="HEADER_FILE"/>
!------------------------------- pcps.h ----------------------------------
!------------------------------- pcps.h ----------------------------------
!------------------------------- pcps.h ----------------------------------
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
! Name       : PCPS.H
! Category   : main_prog
! Written    : 2002-04-22   by: Charles C. Burch
! Revised    : 2002-10-23   by: Donna K. Vunderink
! Maturity   : production   2003-02-27
! Purpose    : Prototype for routines in pcps_crou
! References : These routines are called from within pcps
!</brief_doc>

!<history_doc>
!-------------------------------------------------------------------------------
!                       HEADER FILE REVISION HISTORY
!     Date        Author          Description
!     ----        ------          -----------
!  3. 2003-02-27  C C Burch       Remove get/put_var which is now in lnklst
!  2. 2002-05-06  Vunderink       Fixed #define of get and set worker_info
!  1. 2002-04-22  Chuck C. Burch  Initial version.
!-------------------------------------------------------------------------------
!</history_doc>
*/

/*--------------------------- start of coding ------------------------------*/
/*--------------------------- start of coding ------------------------------*/
/*--------------------------- start of coding ------------------------------*/


#ifndef _PCPS_H_
#define _PCPS_H_

#include "c2f_interface.h"

#ifdef NEED_CAPITALS
#define pcps_abort_c         PCPS_ABORT_C
#define pcps_get_worker_info PCPS_GET_WORKER_INFO
#define pcps_kill_remote_job PCPS_KILL_REMOTE_JOB
#define pcps_print_f         PCPS_PRINT_F
#define pcps_set_worker_info PCPS_SET_WORKER_INFO
#endif

#ifdef NEED_UNDERSCORE
#define pcps_abort_c         pcps_abort_c_
#define pcps_get_worker_info pcps_get_worker_info_
#define pcps_kill_remote_job pcps_kill_remote_job_
#define pcps_print_f         pcps_print_f_
#define pcps_set_worker_info pcps_set_worker_info_
#endif

#ifdef __cplusplus
extern "C" {
#endif

/*char *pcps_h_ident =*/
/*"$Id: pcps.h,v 1.3 2003/02/26 16:27:55 Burch prod sps $";*/

/*------------------------ start of prototypes ---------------------------*/
/*------------------------ start of prototypes ---------------------------*/
/*------------------------ start of prototypes ---------------------------*/

/*        function prototypes for pcps_crou  routines    */
void pcps_abort_c(char*, INTEGER*); 
void pcps_get_worker_info(INTEGER*, INTEGER*, INTEGER*);
void pcps_kill_remote_job(char*, INTEGER*, INTEGER*, INTEGER*);
void pcps_print_f(char*, INTEGER*);
void pcps_set_worker_info(INTEGER*, INTEGER*, INTEGER*);

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
