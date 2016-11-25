/*<CPS_v1 type="AUXILIARY_FILE",pretag="!"/>
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
!                        C P S  P R I M I T I V E   F I L E
!
! Name       : CPS_CROU
! Category   : main_prog
! Written    : 2000-04-18   by: Donna K. Vunderink
! Revised    : 2007-03-27   by: Kruger Corn
! Maturity   : beta
! Purpose    : Provides access to CPS system calls.
! References : These routines are mainly called from within CPS
!------------------------------------------------------------------------------
!</brief_doc>

!<descript_doc>
!------------------------------------------------------------------------------
! Internal subroutines for CPS usage
!------------------------------------------------------------------------------
!</descript_doc>

!<calling_doc>
!------------------------------------------------------------------------------
! Generally, do not call these routines-they are for internal CPS usage
! uses pfio.h, pfio.c (two pfio.c routines are called from within this code)
!------------------------------------------------------------------------------
!</calling_doc>

!<history_doc>
!-------------------------------------------------------------------------------
!                     AUXILIARY FILE REVISION HISTORY
!
!     Date        Author       Description
!     ----        ------       -----------
! 18. 2007-03-27  Kruger Corn  Updated to 64 bit architecture. Basically
!                              changed long to INTEGER.
!     2006-01-31  Bill Menger  Removed cps_write_message_file routine call.
! 17. 2005-07-11  Bill Menger  Added calls to get/set cps project name.
! 16. 2005-04-26  Bill Menger  Replace 15 second timer with pthread timed wait.
! 15. 2005-04-07  Stoeckley    Fix to compile with C++.
! 14. 2005-02-08  Bill Menger  Added different thread return for mem thread.
! 13. 2004-04-27  Goodger      Added setlinebuf for stderr to setlinebuf
!                              routine.
! 12. 2003-12-17  Selzler      Added call backtrace to signal handler and
!                              notes regarding mysterious seg fault errors.
! 11. 2003-11-18  Selzler      Resolved SGI compiler warning (unused variable).
! 10. 2003-05-28  C C Burch    Minor changes for new Solaris compiler warnings.
!  9. 2002-08-12  Vunderink    Monitor memory usage using a separate pthread.
!  8. 2002-06-19  Vunderink    Print node name in cps_sig_handler message
!  7. 2002-04-22  Vunderink    Improved signal handling for job aborts
!  6. 2002-01-02  Vunderink    Added routines for signal handler and memory
!                                approxiation.
!  5. 2001-10-01  Vunderink    Fixed bug in cps_time_stamp
!  4. 2001-08-10  Vunderink    Added cps_time_stamp
!  3. 2001-04-19  Vunderink    Added cps_write_message_file
!  2. 2001-04-05  Vunderink    Added cps_setlinebuf
!  1. 2000-04-18  Vunderink    Initial version.
!
!-------------------------------------------------------------------------------
!</history_doc>

!<portability_doc>
!------------------------------------------------------------------------------
! No known portability problems
!------------------------------------------------------------------------------
!</portability_doc>

!<compile_doc>
!-------------------------------------------------------------------------------
!                      SPECIAL COMPILING REQUIREMENTS
!
! Requires -Dlinux on a linux platform.
!
!-------------------------------------------------------------------------------
!</compile_doc>
****/


/*--------------------------- start of coding ------------------------------*/
/*--------------------------- start of coding ------------------------------*/
/*--------------------------- start of coding ------------------------------*/


         /* The functions in this file are called from cps.f90  */
         /* and are considered to be part of the cps primitive. */
         /* These functions are called only from cps.f90 and    */
         /* should be considered private.                       */

char cps_crou_ident[100] = 
                   "$Id: cps_crou.c,v 1.18 2007/03/28 15:09:41 Corn beta sps $";

#include "c2f_interface.h"
#include "pfio.h"
#include "pcps.h"

#include <errno.h>
#include <pthread.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <signal.h>
#include <time.h>
#include <unistd.h>
#include <sys/resource.h>

#include <sys/time.h>
#ifdef linux
#include <asm/errno.h>
#else
#include <sys/errno.h>
#endif

#if ! sun && ! __sgi && LINUX
#define GNU_TRACEBACK_SUPPORTED 1
#include <execinfo.h>
#endif

#ifdef NEED_UNDERSCORE
#define cps_abort_c                cps_abort_c_
#define cps_diff_time              cps_diff_time_
#define cps_end_memory_thread      cps_end_memory_thread_
#define cps_error_exit_c           cps_error_exit_c_
#define cps_memory_c               cps_memory_c_
#define cps_memory_max             cps_memory_max_
#define cps_print_f                cps_print_f_
#define cps_printf                 cps_printf_
#define cps_setlinebuf             cps_setlinebuf_
#define cps_sig_handler            cps_sig_handler_
#define cps_sig_install_handler    cps_sig_install_handler_
#define cps_start_memory_thread    cps_start_memory_thread_
#define cps_time_stamp             cps_time_stamp_
#define cpssig_shutdown            cpssig_shutdown_
#define cps_get_project_name_c     cps_get_project_name_c_
#define cps_set_project_name_c     cps_set_project_name_c_
#endif

#ifdef NEED_CAPITALS
#define cps_abort_c                CPS_ABORT_C
#define cps_diff_time              CPS_DIFF_TIME
#define cps_end_memory_thread      CPS_END_MEMORY_THREAD
#define cps_error_exit_c           CPS_ERROR_EXIT_C
#define cps_memory_c               CPS_MEMORY_C
#define cps_memory_max             CPS_MEMORY_MAX
#define cps_print_f                CPS_PRINT_F
#define cps_printf                 CPS_PRINTF
#define cps_setlinebuf             CPS_SETLINEBUF
#define cps_sig_handler            CPS_SIG_HANDLER
#define cps_sig_install_handler    CPS_SIG_INSTALL_HANDLER
#define cps_start_memory_thread    CPS_START_MEMORY_THREAD
#define cps_time_stamp             CPS_TIME_STAMP
#define cpssig_shutdown            CPSSIG_SHUTDOWN
#define cps_get_project_name_c     CPS_GET_PROJECT_NAME_C
#define cps_set_project_name_c     CPS_SET_PROJECT_NAME_C
#endif

static FILE *time_stamp_file_ptr;
static pthread_t memory_thread;
static int memory_thread_flag;
static pthread_cond_t         mem_thread_cond=PTHREAD_COND_INITIALIZER;
static pthread_mutex_t        mem_thread_lock;
static struct timespec        mem_thread_timer;
static struct timespec        ts;
static struct timeval         tv;
static struct timezone        tz;

#ifdef __cplusplus
extern "C" {
#endif

extern void cpssig_shutdown(INTEGER *);
extern void cps_print_f(char *,INTEGER *);

void cps_abort_c(void);
void cps_diff_time(double *diff, INTEGER *yr0, INTEGER *mth0, INTEGER *day0,
                   INTEGER *hr0, INTEGER *min0, INTEGER *sec0, INTEGER *yr1,
                   INTEGER *mth1, INTEGER *day1, INTEGER *hr1, INTEGER *min1,
                   INTEGER *sec1);
void cps_error_exit_c(void);
void cps_memory_c (INTEGER *vm, INTEGER *rm);
void cps_printf(char *str);
void cps_setlinebuf(void);
void cps_sig_handler(int sig);
void cps_sig_install_handler(void);
void cps_time_stamp (int *ipn, double *hdr, int *worker);
void* cps_memory_thread(void *ptr);
void cps_get_project_name_c(char *);
void cps_set_project_name_c(char *);

typedef struct _memory {
  INTEGER vm_max;
  INTEGER rm_max;
} memory_info_struct;


static memory_info_struct memory_info;

/*------------------------------- abort_c ----------------------------------*/
/*------------------------------- abort_c ----------------------------------*/
/*------------------------------- abort_c ----------------------------------*/


void cps_abort_c(void)
{
  cps_error_exit_c();
  abort();
}


/*------------------------------ diff_time ---------------------------------*/
/*------------------------------ diff_time ---------------------------------*/
/*------------------------------ diff_time ---------------------------------*/


void cps_diff_time(double *diff, INTEGER *yr0, INTEGER *mth0, INTEGER *day0,
                                 INTEGER *hr0, INTEGER *min0, INTEGER *sec0,
                                 INTEGER *yr1, INTEGER *mth1, INTEGER *day1,
                                 INTEGER *hr1, INTEGER *min1, INTEGER *sec1)
{

  struct tm t0_struct = {0};
  struct tm t1_struct = {0};

  time_t t0_time;
  time_t t1_time;

  t0_struct.tm_year = *yr0;
  t0_struct.tm_mon  = *mth0;
  t0_struct.tm_mday = *day0;
  t0_struct.tm_hour = *hr0;
  t0_struct.tm_min  = *min0;
  t0_struct.tm_sec  = *sec0;

  t1_struct.tm_year = *yr1;
  t1_struct.tm_mon  = *mth1;
  t1_struct.tm_mday = *day1;
  t1_struct.tm_hour = *hr1;
  t1_struct.tm_min  = *min1;
  t1_struct.tm_sec  = *sec1;

  t0_time = mktime(&t0_struct);
  t1_time = mktime(&t1_struct);

  if ( t0_time != (time_t)-1 && t1_time != (time_t)-1 )
    *diff = difftime(t1_time,t0_time);
  else
    *diff = 0.0;         /*Error*/

}


/*---------------------------- error_exit_c --------------------------------*/
/*---------------------------- error_exit_c --------------------------------*/
/*---------------------------- error_exit_c --------------------------------*/


void cps_error_exit_c(void)
{
  FILE *fptr;

  fptr = fopen("core","a+");
  if (fptr != NULL) fclose(fptr);
  exit(1);
}


/*------------------------- start_memory_thread ----------------------------*/
/*------------------------- start_memory_thread ----------------------------*/
/*------------------------- start_memory_thread ----------------------------*/


void cps_start_memory_thread(void)
{
  char mess[133];
  int  istat=0;

#ifdef linux
  istat=-1;
#endif

  memory_thread_flag = 0;
  if(istat<0) {                                      /*only execs on linux*/
    istat = pthread_cond_init(&mem_thread_cond, NULL);
    istat+=pthread_mutex_init(&mem_thread_lock,NULL);
    istat+=pthread_mutex_lock(&mem_thread_lock);
    /* set timeout of 15 seconds on condition wait. */
    mem_thread_timer.tv_sec=15;
    mem_thread_timer.tv_nsec=0;
    /*******************************************/
    istat+=pthread_create(&memory_thread, NULL,cps_memory_thread, 
                           (void *) &memory_info);
    istat+=pthread_mutex_unlock(&mem_thread_lock);
  }

  if(istat != 0) {
    sprintf(mess,"Error: Unable to create memory thread in %s(%d)",
                 "cps_start_memory_thread",istat);
    cps_printf(mess);
    cps_abort_c();
  }
}


/*-------------------------- end_memory_thread -----------------------------*/
/*-------------------------- end_memory_thread -----------------------------*/
/*-------------------------- end_memory_thread -----------------------------*/


void cps_end_memory_thread(void)
{
  char mess[133];
  int  istat=0;

#ifdef linux
  istat=-1;
#endif

  if(istat<0) {
    istat=pthread_mutex_lock(&mem_thread_lock);
    memory_thread_flag =1;
    istat+=pthread_cond_signal(&mem_thread_cond);
    istat+=pthread_mutex_unlock(&mem_thread_lock);
    istat+=pthread_join(memory_thread,NULL);/*only execs on linux*/
    istat+=pthread_cond_destroy(&mem_thread_cond);
    istat+=pthread_mutex_destroy(&mem_thread_lock);
  }

  if(istat!=0) {
    sprintf(mess,"Error: pthread_join in cps_end_memory_thread(%d) [%d-%s]\n",
                  istat, errno, strerror(errno));
    cps_printf(mess);
    cps_abort_c();
  }
}


/*---------------------------- memory_thread -------------------------------*/
/*---------------------------- memory_thread -------------------------------*/
/*---------------------------- memory_thread -------------------------------*/


void* cps_memory_thread(void *ptr)
{
  memory_info_struct *memory_info;
  INTEGER vm = 0;
  INTEGER rm = 0;
  INTEGER vm_max = 0;
  INTEGER rm_max = 0;

  loopback:
    pthread_mutex_lock(&mem_thread_lock);
    cps_memory_c(&vm,&rm);
    vm_max = ((vm_max) > (vm)) ? (vm_max) : (vm);
    rm_max = ((rm_max) > (rm)) ? (rm_max) : (rm);
    if(memory_thread_flag == 0 ) {
      gettimeofday(&tv,&tz); 
      ts.tv_sec  = tv.tv_sec       + mem_thread_timer.tv_sec;
      ts.tv_nsec = tv.tv_usec*1000 + mem_thread_timer.tv_nsec;
      if(pthread_cond_timedwait(&mem_thread_cond,
                                &mem_thread_lock,
                                &ts) == ETIMEDOUT) {
        pthread_mutex_unlock(&mem_thread_lock);
        goto loopback;      
      }
    }
  pthread_mutex_unlock(&mem_thread_lock);
  memory_info = (memory_info_struct *) ptr;
  memory_info->vm_max = vm_max;
  memory_info->rm_max = rm_max;
  return (ptr);
}

/*------------------------------- memory_c ---------------------------------*/
/*------------------------------- memory_c ---------------------------------*/
/*------------------------------- memory_c ---------------------------------*/


void cps_memory_max (INTEGER *vm_max, INTEGER *rm_max)
{
#ifdef linux
  *vm_max = memory_info.vm_max;
  *rm_max = memory_info.rm_max;
#else
  int istat;
  int page_adjust;
  struct rusage ru;

  istat = getrusage(RUSAGE_SELF, &ru);
  if (istat == 0) {
    *vm_max = 0;
    page_adjust = getpagesize()/1024;
    *rm_max = ru.ru_maxrss*page_adjust;
  }
  else {
    *vm_max = 0;
    *rm_max = 0;
  }
#endif
}


/*------------------------------- memory_c ---------------------------------*/
/*------------------------------- memory_c ---------------------------------*/
/*------------------------------- memory_c ---------------------------------*/


void cps_memory_c (INTEGER *vm, INTEGER *rm)
{

#ifdef linux

  FILE     *statfile;
  int      pid;
  char     com[1024];
  char     state;
  int      ppid;
  int      pgrp;
  int      session;
  int      tty;
  int      tpgid;
  unsigned flags;
  unsigned minflt;
  unsigned cminflt;
  unsigned majflt;
  unsigned cmajflt;
  int      utime;
  int      stime;
  int      cutime;
  int      cstime;
  int      counter;
  int      priority;
  int      timeout;
  unsigned itrealvalue;
  int      starttime;
  unsigned vsize;
  unsigned rss;

  statfile = fopen("/proc/self/stat","r");
  if (statfile) {
    if (fscanf(statfile,
"%d (%[^)]) %c %d %d %d %d %d %u %u %u %u %u %d %d %d %d %d %d %d %u %d %u %u",
               &pid,com,&state,&ppid,&pgrp,&session,&tty,&tpgid,&flags,&minflt,
               &cminflt,&majflt,&cmajflt,&utime,&stime,&cutime,&cstime,&counter,
               &priority,&timeout,&itrealvalue,&starttime,&vsize,&rss)!= 24) {
      *vm = 0;
      *rm = 0;
    }
    else {
      *vm = vsize / 1024;
      *rm = rss * 4;
    }
    fclose(statfile);
  }
  else {
    *vm = 0;
    *rm = 0;
  }

#else

  *vm = 0;
  *rm = 0;

#endif
}


/*-------------------------------- printf ----------------------------------*/
/*-------------------------------- printf ----------------------------------*/
/*-------------------------------- printf ----------------------------------*/


void cps_printf(char *str)
{
  INTEGER len;

  len = strlen(str);
  cps_print_f(str,&len);
}


/*------------------------------ setlinebuf --------------------------------*/
/*------------------------------ setlinebuf --------------------------------*/
/*------------------------------ setlinebuf --------------------------------*/


void cps_setlinebuf(void)
{
  setlinebuf(stdout);
  setlinebuf(stderr);
}

/*----------------------------- sig_handler --------------------------------*/
/*----------------------------- sig_handler --------------------------------*/
/*----------------------------- sig_handler --------------------------------*/


void cps_sig_handler(int sig)
{
  INTEGER cps_signal, worker_num, num_workers, num_procs;
  static int num_times=0;
  char mess[120];
  char host[21];
#ifdef GNU_TRACEBACK_SUPPORTED
#define BT_MAX 100
  void *bt_addr[BT_MAX];
  int bt_count;
#endif

  pcps_get_worker_info(&worker_num, &num_workers, &num_procs);

  if ((num_times++)==0) {
    if(sig!=SIGUSR2) {
      cps_printf(" ");
      cps_printf("                ++++++++++++++++++++++++++++++++++++++++++");
      cps_printf("                +++++++ CPS Signal Handler Invoked +++++++");
      cps_printf("                ++++++++++++++++++++++++++++++++++++++++++");
      cps_printf(" ");
      cps_printf(" ");
#ifdef GNU_TRACEBACK_SUPPORTED
      bt_count = backtrace(bt_addr, BT_MAX);
      backtrace_symbols_fd(bt_addr, bt_count, fileno(stdout));
#endif
    }
  } else {
    cps_abort_c();
  }

  if (gethostname(host,21) == -1) strcpy(host,"????");

  if(num_procs>1) {
    sprintf(mess,"cps_sig_handler(PCPS cpu#%d host %s): %c", 
                 (int)worker_num,host,'\0');
  } else {
    sprintf(mess,"cps_sig_handler(host %s): %c",host,'\0');
  }

  switch(sig) { 
  case (SIGQUIT) :
    strcat(mess,"SIGQUIT - Quit from keyboard");
    cps_signal = 3;
    break;

  case (SIGILL) :
    strcat(mess,"SIGILL - Illegal Instruction");
    cps_signal = 4;
    break;

  case (SIGABRT) :
    strcat(mess,"SIGABRT - Abort signal from abort function");
    cps_signal = 6;
    break;

  case (SIGBUS) :
    strcat(mess,"SIGBUS - Bus Error");
    cps_signal = 7;
    break;

  case (SIGFPE) :
    strcat(mess, "SIGFPE - Floating point exception has occured");
    cps_signal = 8;
    break;

  case (SIGUSR1) :
    strcat(mess,"SIGUSR1 - User-defined signal 1");
    cps_signal = 10;
    break;

  case (SIGSEGV) :
    strcat(mess, "SIGSEGV - Invalid memory reference has occured");
    cps_signal = 11;
    break;

  case (SIGUSR2) :
    strcat(mess,"SIGUSR2 - User-defined signal 2"); 
    cps_signal = 12;
    break;
  
  case (SIGPIPE) :
    strcat(mess,"SIGPIPE - Broken pipe");
    cps_signal = 13;
    break;

  case (SIGTERM) :
    strcat(mess,"SIGTERM - Termination");
    cps_signal = 15;
    break;

  case (SIGTSTP) :
    strcat(mess,"SIGTSTP - Stop typed from keyboard");
    cps_signal = 20;
    break;

  case (SIGXCPU) :
    strcat(mess,"SIGXCPU - CPU time limit exceeded");
    cps_signal = 24;
    break;

  case (SIGXFSZ) :
    strcat(mess,"SIGXFSZ - File size limit exceeded");
    cps_signal = 25;
    break;
  }

  cps_printf(mess);
  fflush(stderr);

  cpssig_shutdown(&cps_signal);
  cps_abort_c();

}


/*------------------------- sig_install_handler ----------------------------*/
/*------------------------- sig_install_handler ----------------------------*/
/*------------------------- sig_install_handler ----------------------------*/


void cps_sig_install_handler()
{
  struct sigaction action;

  action.sa_handler   = cps_sig_handler;
  sigemptyset(&action.sa_mask);
  sigaddset(&action.sa_mask,SIGUSR1);
  sigaddset(&action.sa_mask,SIGUSR2);
  sigaddset(&action.sa_mask,SIGTERM);
  action.sa_flags     = SA_NODEFER;

  if(sigaction(SIGINT, &action, (struct sigaction *) NULL) < 0) {
   cps_printf("cps_sig_install_handler: signal call failed for SIGINT\n");
  }

  if(sigaction(SIGQUIT, &action, (struct sigaction *) NULL) < 0) {
   cps_printf("cps_sig_install_handler: signal call failed for SIGQUIT\n");
  }

  if(sigaction(SIGILL, &action, (struct sigaction *) NULL) < 0) {
   cps_printf("cps_sig_install_handler: signal call failed for SIGILL\n");
  }

  if(sigaction(SIGBUS, &action, (struct sigaction *) NULL) < 0) {
   cps_printf("cps_sig_install_handler: signal call failed for SIGBUS\n");
  }

  if(sigaction(SIGFPE, &action, (struct sigaction *) NULL) < 0) {
   cps_printf("cps_sig_install_handler: signal call failed for SIGFPE\n");
  }

  if(sigaction(SIGUSR1, &action, (struct sigaction *) NULL) < 0) {
   cps_printf("cps_sig_install_handler: signal call failed for SIGUSR1\n");
  }

  /***
   * 17 Dec 2003 Randy Selzler.
   * Seg faults occur under the following conditions:
   *   1) Linked with "-lmcheck" (GNU libc option, i.e. Linux).
   *       Helps detect malloc overruns and uninitialized variables.
   *   2) Linked with production libraries (as of 17 Dec 2003)
   *       Fails with RH7.2 (libc-2.2.5.so and/or libpthread-0.9.so)
   *       Works with RH9.0 (libc-2.3.2.so and/or libpthread-0.10.so)
   *   3) sigaction(SIGSEGV, ...) is invoked; i.e. via cps_start_processing.
   *       Other signal handlers are okay.
   *   4) pthread_create(...) is invoked; i.e. via cps_start_processing.
   *   5) popen(...) is invoked; i.e. via cps_start_processing
   *   6) pclose(...) is invoked; i.e. via cps_start_processing
   *       Seg Fault!
   *
   * We should probably resolve the problem by migrating to the new libraries,
   * after appropriate regression testing.
   * Until then, one of these six conditions must be eliminated.
   * Of these six, only 1 and 3 are optional at this point in time.
   *
   * If the SIGSEGV handler is not installed (3) and a seg fault does occur,
   * the default action is to terminate without any message and with a
   * zero return status.  This would be interpreted as a mysterious abend.
   *
   * This leaves us with only one option, eliminate "-lmcheck" links (1).
   * If "-lmcheck" is desired for debugging purposes, then the following
   * sigaction can be commented out while trying a special test.
  ***/
  if(sigaction(SIGSEGV, &action, (struct sigaction *) NULL) < 0) {
   cps_printf("cps_sig_install_handler: signal call failed for SIGSEGV\n");
  }

  if(sigaction(SIGUSR2, &action, (struct sigaction *) NULL) < 0) {
   cps_printf("cps_sig_install_handler: signal call failed for SIGUSR2\n");
  }

  if(sigaction(SIGPIPE, &action, (struct sigaction *) NULL) < 0) {
   cps_printf("cps_sig_install_handler: signal call failed for SIGPIPE\n");
  }

  if(sigaction(SIGTERM, &action, (struct sigaction *) NULL) < 0) {
   cps_printf("cps_sig_install_handler: signal call failed for SIGTERM\n");
  }

  if(sigaction(SIGTSTP, &action, (struct sigaction *) NULL) < 0) {
   cps_printf("cps_sig_install_handler: signal call failed for SIGTSTP\n");
  }

  if(sigaction(SIGXCPU, &action, (struct sigaction *) NULL) < 0) {
   cps_printf("cps_sig_install_handler: signal call failed for SIGXCPU\n");
  }

  if(sigaction(SIGXFSZ, &action, (struct sigaction *) NULL) < 0) {
   cps_printf("cps_sig_install_handler: signal call failed for SIGXFSZ\n");
  }

  action.sa_flags     = SA_RESETHAND;
  if(sigaction(SIGABRT, &action, (struct sigaction *) NULL) < 0) {
   cps_printf("cps_sig_install_handler: signal call failed for SIGABRT\n");
  }

}


/*------------------------------ time_stamp --------------------------------*/
/*------------------------------ time_stamp --------------------------------*/
/*------------------------------ time_stamp --------------------------------*/


void cps_time_stamp (int *ipn, double *hdr, int *worker)
{
  time_t       current;
  struct tm    *now;
  char         tstamp[20];
  char         filename[20];

  current = time(NULL);
  now = localtime(&current);
  strftime(tstamp,20,"%Y-%m-%d %H.%M.%S",now);

  if (time_stamp_file_ptr == NULL) {
    if (*worker > 0)
      sprintf(filename,"time_stamp_%04d",*worker);
    else
      strcpy(filename,"time_stamp");
    time_stamp_file_ptr = fopen(filename,"w");
    if (time_stamp_file_ptr) setlinebuf(time_stamp_file_ptr);
  }

  if (time_stamp_file_ptr) {
    fprintf(time_stamp_file_ptr,"%s ipn= %d hdr1= %f hdr3= %f hdr4= %f\n",
                                 tstamp,*ipn,hdr[0],hdr[2],hdr[3]);
  }
}

/*---------------------- get project name ---------------------------------*/
/*---------------------- get project name ---------------------------------*/
/*---------------------- get project name ---------------------------------*/

void cps_get_project_name_c(char * project_name){
  project_name = pfio_get_project_name();
}
  

/*---------------------- set project name ---------------------------------*/
/*---------------------- set project name ---------------------------------*/
/*---------------------- set project name ---------------------------------*/

void cps_set_project_name_c(char * project_name){
  pfio_set_project_name(project_name);
}

#ifdef __cplusplus
}
#endif
