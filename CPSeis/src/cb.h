/*<CPS_v1 type="HEADER_FILE",pretag="!"/>
!------------------------------- cb.h ----------------------------------
!------------------------------- cb.h ----------------------------------
!------------------------------- cb.h ----------------------------------
! other files are:  cb.c
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
!                      C P S   H E A D E R   F I L E
!
! Name       : cb
! Category   : miscellaneous
! Written    : 2005-02-08   by: Bill Menger
! Revised    : 2005-12-05   by: Chuck Burch
! Maturity   : production
! Purpose    : Provides a circular buffer of pointers and functions with which
!              to access the buffer.
! Portability: Requires the pthreads (POSIX THREADS) library.
!
!-------------------------------------------------------------------------------
!</brief_doc>

!<history_doc>
!-------------------------------------------------------------------------------
!                       HEADER FILE REVISION HISTORY
!
!     Date        Author       Description
!     ----        ------       -----------
!  3. 2005-12-05  Chuck Burch  Added qsize to cb_t structure
!  2. 2005-07-26  Bill Menger  Added return variables on pthread calls, added
!                              cb_msg prototype.
!  1. 2005-02-08  Bill Menger  Initial version.
!
!-------------------------------------------------------------------------------
!</history_doc>

!--------------------------- start of coding ------------------------------
!--------------------------- start of coding ------------------------------
!--------------------------- start of coding ------------------------------
*/

#ifndef _CB_H_
#define _CB_H_

/************************** following added for threaded writes ************/
#include <string.h>
#include <stdio.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <pthread.h>
#include <malloc.h>
#include <signal.h>

#ifdef __cplusplus
extern "C" {
#endif

/*------------------------ start of prototypes ---------------------------*/
/*------------------------ start of prototypes ---------------------------*/
/*------------------------ start of prototypes ---------------------------*/
typedef struct {
  pthread_mutex_t buf_lock;             /* lock the structure */
  pthread_cond_t  notfull;              /* the FULL or NOT-FULL condition   */
  pthread_cond_t  notempty;             /* the EMPTY or NOT-EMPTY condition */
  int  start_idx;                       /* start of valid data              */
  int  num_full;                        /* number of full index locations   */
  int  qsize;                           /* size of circular buffer          */
  void **data;                          /* circular buffer of pointers      */
} cb_t;

cb_t * cb_create(int qsize);
void   cb_destroy(cb_t *);
void   cb_put(cb_t *, void *);
void * cb_get(cb_t *);
int    cb_numqueued(cb_t *);
int    cb_getqsize(cb_t *);
void   cb_msg(char * , int *);

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
