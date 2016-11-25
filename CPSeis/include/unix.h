/*<CPS_v1 type="HEADER_FILE"/>
!------------------------------- unix.h ----------------------------------
!------------------------------- unix.h ----------------------------------
!------------------------------- unix.h ----------------------------------
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
! Name       : unix.h
! Category   : miscellaneous
! Written    : 2002-04-24   by: Charles C. Burch
! Revised    : 2004-12-15   by: Bill Menger
! Maturity   : production
! Purpose    : Prototype for routines in unix_crou
! References : These routines are called from within unix_crou.c, unix.f90
!-------------------------------------------------------------------------------
!</brief_doc>

!<history_doc>
!-------------------------------------------------------------------------------
!                       HEADER FILE REVISION HISTORY
!     Date        Author     Description
!     ----        ------     -----------
!  8. 2004-12-15  B Menger   Added unix_timeout_alarm and (Ried- errmsg)
!  7. 2004-08-12  B Menger   Added unix_is_process_active_f and a prototype for
!                            unix_system_timeout_c
!  6. 2004-01-21  W Menger   Added unix_utime and unix_get_cmdline
!  5. 2003-09-08  SMCook     Added unix_umask_c
!  4. 2003-06-25  C C Burch  Moved get_pid_c, get_user_name_c, get_user_dir_c
!                            from pcpsx_crou
!  3. 2003-05-15  C C Burch  Added unix_user_name_c, unix_get_hostname_c, 
!                            unix_is_process_active_c
!  2. 2002-09-19  C C Burch  Added unix_wtime_stamp_c
!  1. 2002-04-24  C C Burch  Initial version.
!-------------------------------------------------------------------------------
!</history_doc>
*/

/*--------------------------- start of coding ------------------------------*/
/*--------------------------- start of coding ------------------------------*/
/*--------------------------- start of coding ------------------------------*/


#ifndef _UNIX_H_
#define _UNIX_H_

#include <signal.h>
#include <sys/wait.h>
#include <unistd.h>

#include "c2f_interface.h"

#ifdef NEED_CAPITALS
#define unix_abort_c             UNIX_ABORT_C
#define unix_errmsg_c            UNIX_ERRMSG_C
#define unix_fork_c              UNIX_FORK_C
#define unix_get_hostname_c      UNIX_GET_HOSTNAME_C
#define unix_get_host_name_f     UNIX_GET_HOST_NAME_F
#define unix_mem_usage_c         UNIX_MEM_USAGE_C
#define unix_get_cmdline_c       UNIX_GET_CMDLINE_C
#define unix_get_pid_f           UNIX_GET_PID_F
#define unix_get_user_dir_f      UNIX_GET_USER_DIR_F
#define unix_get_user_name_f     UNIX_GET_USER_NAME_F
#define unix_get_wtime_stamp_c   UNIX_GET_WTIME_STAMP_C
#define unix_mem_usage_c         UNIX_MEM_USAGE_C
#define unix_initialize_c        UNIX_INITIALIZE_C
#define unix_is_process_active_c UNIX_IS_PROCESS_ACTIVE_C
#define unix_is_process_active_f UNIX_IS_PROCESS_ACTIVE_F
#define unix_sleep_c             UNIX_SLEEP_C
#define unix_system_c            UNIX_SYSTEM_C
#define unix_system_timeout_c    UNIX_SYSTEM_TIMEOUT_C
#define unix_umask_c             UNIX_UMASK_C
#define unix_utime_c             UNIX_UTIME_C
#define unix_user_name_c         UNIX_USER_NAME_C
#define unix_waitpid_c           UNIX_WAITPID_C
#define unix_wtime_c             UNIX_WTIME_C
#define unix_wtime_stamp_c       UNIX_WTIME_STAMP_C
#endif

#ifdef NEED_UNDERSCORE
#define unix_abort_c             unix_abort_c_
#define unix_errmsg_c            unix_errmsg_c_
#define unix_get_cmdline_c       unix_get_cmdline_c_
#define unix_get_hostname_c      unix_get_hostname_c_
#define unix_get_host_name_f     unix_get_host_name_f_
#define unix_mem_usage_c         unix_mem_usage_c_
#define unix_get_wtime_stamp_c   unix_get_wtime_stamp_c_
#define unix_fork_c              unix_fork_c_
#define unix_get_pid_f           unix_get_pid_f_
#define unix_get_user_dir_f      unix_get_user_dir_f_
#define unix_get_user_name_f     unix_get_user_name_f_
#define unix_mem_usage_c         unix_mem_usage_c_
#define unix_initialize_c        unix_initialize_c_
#define unix_is_process_active_c unix_is_process_active_c_
#define unix_is_process_active_f unix_is_process_active_f_
#define unix_sleep_c             unix_sleep_c_
#define unix_system_c            unix_system_c_
#define unix_system_timeout_c    unix_system_timeout_c_
#define unix_umask_c             unix_umask_c_
#define unix_utime_c             unix_utime_c_
#define unix_user_name_c         unix_user_name_c_
#define unix_waitpid_c           unix_waitpid_c_
#define unix_wtime_c             unix_wtime_c_
#define unix_wtime_stamp_c       unix_wtime_stamp_c_
#endif

#ifdef __cplusplus
extern "C" {
#endif

/*char *unix_h_ident =*/
/*"$Id: unix.h,v 1.8 2004/12/15 14:00:54 Menger prod sps $";*/

/*------------------------ start of prototypes ---------------------------*/
/*------------------------ start of prototypes ---------------------------*/
/*------------------------ start of prototypes ---------------------------*/

/*        function prototypes for unix_crou  routines    */
void    unix_onalarm();
void    unix_abort_c(char*);
INTEGER unix_fork_c();
int     unix_get_hostname_c(char*, int);
void    unix_get_host_name_f(char*, INTEGER*);
void    unix_get_pid_f(INTEGER*);
void    unix_get_user_dir_c(char*, int);
void    unix_get_user_dir_f(char*, INTEGER*);
void    unix_get_user_name_f(char*, INTEGER*);
DOUBLE  unix_get_wtime_stamp_c();

/* The following rely on the Linux /proc filesystem */
void    unix_mem_usage_c(char*, INTEGER*);
void    unix_get_cmdline_c(INTEGER *argc, char *argv, INTEGER *lenargv);
DOUBLE  unix_utime_c(DOUBLE*);
/*  End of /proc filesystem dependent calls */ 

void    unix_initialize_c();
int     unix_is_process_active_c(char*, int);
INTEGER unix_is_process_active_f(char*, int *);
void    unix_sleep_c(INTEGER*);
INTEGER unix_system_c(char*, char*);
INTEGER unix_system_timeout_c(char*,INTEGER*, char*);
void    unix_timeout_alarm(int);
void    unix_umask_c( int *imask );
char*   unix_user_name_c();
INTEGER unix_waitpid_c(INTEGER*);
DOUBLE  unix_wtime_c(DOUBLE*);
void    unix_wtime_stamp_c(char*);

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
