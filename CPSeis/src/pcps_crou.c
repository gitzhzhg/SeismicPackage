/*<CPS_v1 type="PRIMITIVE"/>
!----------------------------- pcps_crou.c -------------------------------
!----------------------------- pcps_crou.c -------------------------------
!----------------------------- pcps_crou.c -------------------------------
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
!------------------------------------------------------------------------------
! Name       : PCPS
! Category   : main_prog
! Written    : 2000-11-29   by: Charles C. Burch
! Revised    : 2006-11-14   by: Bill Menger
! Maturity   : production
! Purpose    : Provides access to PCPS system calls.
! References : These routines are mainly called from within PCPS
!------------------------------------------------------------------------------
!</brief_doc>

!<descript_doc>
!------------------------------------------------------------------------------
! Internal subroutines for PCPS usage
!------------------------------------------------------------------------------
!</descript_doc>

!<calling_doc>
!------------------------------------------------------------------------------
! Generally, do not call these routines-they are for internal PCPS usage
!------------------------------------------------------------------------------
! void pcps_get_worker_info(INTEGER *worker_num, INTEGER *num_workers, 
!   INTEGER *num_procs)            
! Used to get pcps_current_worker_num pcps_num_workers, and pcps_num_procs
! Callable from Fortran, but mainly for C 
!------------------------------------------------------------------------------
! void pcps_kill_remote_job(char *HOST, INTEGER *NHOST, INTEGER *PID,
!  INTEGER *SIGNAL) {
! Used to kill a specific job[PID] on host[HOST(1:NHOST)] using SIGNAL
! Callable from Fortran--could be called from C
!------------------------------------------------------------------------------
!</calling_doc>

!<history_doc>
!-------------------------------------------------------------------------------
!                     AUXILIARY FILE REVISION HISTORY
!     Date        Author    Description
!     ----        ------    -----------
!  5. 2006-11-14  Bill Menger Modified the call to pfio_remote_command
!  4. 2003-02-27  C C Burch Remove get/put_var(now in lnklst)
!  3. 2002-02-26  C C Burch Added pcps_kill_remote_job, pcps_get_worker_info
!  2. 2001-06-27  C C Burch Added use of c2f_interface.h
!                           Added pcps_get_var_c, pcsp_put_var_c
!  1. 2000-11-29  C C Burch Initial Version.
!-------------------------------------------------------------------------------
!</history_doc>

!<portability_doc>
!------------------------------------------------------------------------------
! No known portability problems
!------------------------------------------------------------------------------
!</portability_doc>

*/

/*
!--------------------------- start of coding ------------------------------
!--------------------------- start of coding ------------------------------
!--------------------------- start of coding ------------------------------
*/

char *pcps_crou_ident =
"$Id: pcps_crou.c,v 1.5 2006/11/14 14:32:58 Menge prod sps $";

#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>

#include "c2f_interface.h"
#include "pfio.h"
#include "pcps.h"

static int worker_num=0;
static int num_workers=0;
static int num_procs=1;

/*------------------------ start of functions ----------------------------*/
/*------------------------ start of functions ----------------------------*/
/*------------------------ start of functions ----------------------------*/

#ifdef __cplusplus
extern "C" {
#endif

/************ PCPS_SET_WORKER_INFO ************
*  Set worker info
*
* Written January 2002 by Charles C Burch
***********************************************/
void pcps_set_worker_info(INTEGER *WORKER_NUM, INTEGER *NUM_WORKERS, 
 INTEGER *NUM_PROCS) {
  worker_num=(*WORKER_NUM);
  num_workers=(*NUM_WORKERS);
  num_procs=(*NUM_PROCS);
  return;
}

/************ PCPS_GET_WORKER_INFO ************
*  Get worker info
*
* Written January 2002 by Charles C Burch
***********************************************/
void pcps_get_worker_info(INTEGER *WORKER_NUM, INTEGER *NUM_WORKERS, 
 INTEGER *NUM_PROCS) {
  (*WORKER_NUM)=worker_num;
  (*NUM_WORKERS)=num_workers;
  (*NUM_PROCS)=num_procs;
  return;
}

/***********************************************
*  Do a c abort function
*
* Written November 2000 by Donna K Vunderink
* Modified January 2002 by Charles C Burch to
*  add printouts
***********************************************/
void pcps_abort_c(char *mess, INTEGER *n_mess)
{
  char str[240];
  int i;

  for (i=0; i<(*n_mess); i++) str[i]=mess[i];
  str[*n_mess]='\0';

  sleep(1);
  fflush(stdout);

  printf("PCPS(cpu#%d) aborting for the following reason:\n",
   worker_num);
  printf("  %s\n",str);

  abort();
}

/***********************************************
* kill a given job on a remote host 
*
* Written January 2002 by Charles C Burch
***********************************************/
  void pcps_kill_remote_job(char *HOST, INTEGER *NHOST, INTEGER *PID,
   INTEGER *SIGNAL) {
    char hostname[80], cmd[256];
    int i, n;

    n=0;
    for(i=0; i<(*NHOST); i++) {
      if(HOST[i]!=' ') hostname[n++]=HOST[i];
    }
    hostname[n]='\0';
    sprintf(cmd,"\\kill -%d %d%c", *SIGNAL, *PID,'\0');
    pfio_remote_command(hostname, cmd);
    return;
  }

#ifdef __cplusplus
}
#endif

/*-------------------------------- end ------------------------------------*/
/*-------------------------------- end ------------------------------------*/
/*-------------------------------- end ------------------------------------*/
