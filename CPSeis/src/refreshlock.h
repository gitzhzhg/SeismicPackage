/*<CPS_v1 type="HEADER_FILE"/>
!------------------------------ refreshlock.h --------------------------------!!
!------------------------------ refreshlock.h --------------------------------!!
!------------------------------ refreshlock.h --------------------------------!!
!
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
!
!<brief_doc>
!-------------------------------------------------------------------------------
!                       C P S   H E A D E R  F I L E
!
! Name       : refreshlock
! Category   : stand-alone
! Written    : 2003-05-23   by: Bill Menger/Charles C Burch
! Revised    : 2007-10-23   by: Bill Menger
! Maturity   : beta
! Purpose    : Checks for and refreshes stale locks on any CPS lock file.
! Portability: No known limitations.
!-------------------------------------------------------------------------------
!</brief_doc>
!
!<descript_doc>
!-------------------------------------------------------------------------------
!                          GENERAL DESCRIPTION
! This header file goes with refreshlock.c
!-------------------------------------------------------------------------------
!</descript_doc>
!
!<advice_doc>
!-------------------------------------------------------------------------------
!                          ADVICE FOR USERS
!-------------------------------------------------------------------------------
!</advice_doc>
!
!<history_doc>
!-------------------------------------------------------------------------------
!                             REVISION HISTORY
!
!     Date        Author      Description
!     ----        ------      -----------
!  1. 2007-10-23  Bill Menger First instance of this '.h' file.
!
!-------------------------------------------------------------------------------
!</history_doc>
!
!<portability_doc>
!-------------------------------------------------------------------------------
!                         PORTABILITY LIMITATIONS
! No known limitations.
!-------------------------------------------------------------------------------
!</portability_doc>
!
!<compile_doc>
!-------------------------------------------------------------------------------
!                     SPECIAL COMPILING REQUIREMENTS
!-------------------------------------------------------------------------------
!</compile_doc>
!
!!-------------------------- start of program ------------------------------!!
!!-------------------------- start of program ------------------------------!!
!!-------------------------- start of program ------------------------------!!
*/

#include <stdlib.h>
#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>
#include <fcntl.h>
#include <time.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <sys/time.h>
#include <pthread.h>
#include <signal.h>
#include <string.h>
#include <ctype.h>
#include <pwd.h>
#include <dirent.h>
#include <errno.h>

/*------------------------ start of functions ----------------------------*/
/*------------------------ start of functions ----------------------------*/
/*------------------------ start of functions ----------------------------*/

#ifdef __cplusplus
extern "C" {
#endif

static int  lock_held_time = 120;

void *refreshlock_open(void *);
int   refreshlock_copy_file(char*, char*);
int   refreshlock_create_new_file(char*);
void  refreshlock_lcktst(int, short, off_t , short , off_t , pid_t*, long*);
int   refreshlock_lock_reg(int, int, short, off_t, short, off_t, char*);
void  refreshlock_lock_test(int, off_t, short, off_t, char *, 
      pid_t*, long*, pid_t*, long*);
void  refreshlock_make_lockfile_name(char*, char*, int, int);
char* refreshlock_time_stamp(void);
char* refreshlock_yes_no(int);

#ifdef __cplusplus
}
#endif
