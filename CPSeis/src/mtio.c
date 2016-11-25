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
/*<CPS_v1 type="PRIMITIVE", pretag="!"/>
!
!------------------------------- mtio.c ----------------------------------
!------------------------------- mtio.c ----------------------------------
!------------------------------- mtio.c ----------------------------------
!
!other files are:  mtio.h
!uses           :    cb.c, cb.h

!<brief_doc>
!-------------------------------------------------------------------------------
!                        C P S   P R I M I T I V E
!
! Name       : mtio (multi-threaded i/o)
! Category   : io
! Written    : 2005-02-08   by: Bill Menger
! Revised    : 2009-01-29   by: Bill Menger
! Maturity   : beta
! Purpose    : Asynchronous output using multiple threads.
! Portability: Relies on POSIX THREADS (pthreads).
!-------------------------------------------------------------------------------
!</brief_doc>
!<descript_doc>
!-------------------------------------------------------------------------------
!                         GENERAL DESCRIPTION
! The mtio package performs i/o directly with "c" system calls for fopen,
! fclose, fread, fwrite, fgets, fseek, and ftell.  It detects whether or not to
! use fseeko, ftello, and adjusts parameters accordingly using #ifdefs in
! mtio.h.  The output of data is never directly written to files, but is written
! to a memory buffer and placed on a circular queue.  The circular queue only
! holds the pointers to the memory buffers.   A set of write-behind threads in
! mtio pull items from the circular queue and write the data to the files.  In
! case a read or flush operation is requested on a file, all outputs from the 
! circular queue for that file are completed prior to returning with data.  This
! ensures integrity on the data read phase of i/o.
!-------------------------------------------------------------------------------
!</descript_doc>
!
!<calling_doc>
!-------------------------------------------------------------------------------
!                      INPUT AND OUTPUT ARGUMENTS
!
! For each subroutine or function documented below, each argument is
! flagged as follows:
!        i = value required upon INPUT.
!        o = value set by the routine upon OUTPUT.
!        b = value BOTH required upon input and changed upon output.
!
! For more information, go to the man pages for fopen, fclose, fread,...
!-------------------------------------------------------------------------------
!                          CALLING SEQUENCE
!
!                          i                     i                              
!  int    mtio_init(int *mtio_numthreads, int *mtio_qsize);
!         Purpose:  initialize queue and threads
!         mtio_numthreads = number of write-behind threads to use
!         mtio_qsize      = number of pointers allowed in the circular queue
!            
!  int    mtio_status();
!         Purpose:  Tell you either MTIO_ON or MTIO_OFF (on=1, off=0)
!                   This is used internally to detect system initialization,
!                   but can be called externally if desired.
!
!  void   mtio_exit();
!         Purpose:  Close all open files, flush the queue, shut down all 
!                   write-behind threads, and free memory.
!
!  FILE  *mtio_fopen(const char *path, const char *mode);
!         Purpose: To mimic the "fopen" call, but the opened file will use
!                  mtio functions.  The returned file descriptor is accessible
!                  from standard system calls, but no guarantee is made with
!                  respect to file status if using standard calls (i.e. fread,
!                  fwrite...).  If you mix mtio_xxx calls with system calls on
!                  a file, you run the risk of reading a file that does not
!                  have all of its i/o synchronized.
!                           i
!  int    mtio_fclose(FILE *stream);
!         Purpose: mimic the "fclose" system call.  close a file.
!
!                          i            i             i           i   
!  size_t mtio_fread(void *ptr, size_t size, size_t nmemb, FILE *stream);
!         Purpose: mimic the "fread" system call.
!
!                        i         i           i
!  char * mtio_fgets(char *s, int size, FILE *stream);
!         Purpose: mimic the "fgets" system call.
!
!                         i        i
!  int    mtio_fputs(char *s, FILE *stream);
!         Purpose: mimic the "fputs" system call.
!                                  i           i            i            i
!  size_t mtio_fwrite(const void *ptr, size_t size, size_t nmemb, FILE *stream);
!         Purpose: mimic the "fwrite" system call.
! 
!                            i
!  int    mtio_fflush(FILE *stream);
!         Purpose: mimic the system "fflush" call.
!
!                           i                  i            i
!  int    mtio_fseek(FILE *stream, mtio_off_t offset, int whence);
!         Purpose: mimic the system "fseek" and "fseeko" calls.
!         The type "mtio_off_t" is used so that a common call can be made, 
!         regardless of 32 or 64 bit system.  The "mtio.h" file uses #ifdefs
!         to set the type for mtio_off_t and to set the call internally to
!         either fseek or fseeko.
!
!                            i
!  mtio_off_t mtio_flsz(char *path);
!         Purpose: Return size of a file;
!
!                                i
!  mtio_off_t mtio_ftell(FILE *stream);
!         Purpose: mimic the system "ftell" "ftello" calls.  See mtio_fseek.
!
!                                    i 
!  void   *mtio_writebehind(void * ithread);
!         Purpose: Perform the write-behind. INTERNAL USE ONLY.  DO NOT CALL.
!
!  int    mtio_getnumthreads();
!         Purpose: Return the number of threads running.
!
!  int    mtio_getqsize();
!         Purpose: Return size of circular buffer
!
!  int    mtio_numqeued();
!         Purpose: Return number entried queued in circular buffer
!
!                         i
!  int    mtio_find_ifile(FILE *fd)
!         Purpose: INTERNAL USE:  Finds the array index within mtio for a
!                  file.
!
!                          i
!  FILE * mtio_find_fd(char *path)
!         Purpose: INTERNAL USE: Finds a file pointer from the name
!                  Returns either the pointer or NULL if not found.
!
!                                      i
!  void   mtio_internal_clean_fd(int ifile);
!         Purpose: INTERNAL USE:  From the internal mtio index (ifile), wipe
!                  out the information for the file, remove mutex locks, etc.
!         int ifile = index from mtio_find_ifile for a file within mtio system.
!
!                         i 
!  int    mtio_fileno(FILE *fd)
!         Purpose: Return file descriptor for stream *fd
!
!                          i
!  char * mtio_inquire(FILE *fd)
!         Purpose: Return file name for stream *fd
!         return NULL if not opened by mtio or if mtio not running.
!
!                                 i
!  void   mtio_set_print_stats(int isw)
!         Purpose: controls if thread statistics printed (i!=0 yes, i=0 no)
!         This is intended for diagnostic use only
!
!                                 i
!  void   mtio_set_debug_mode(int isw);
!         Purpose: sets debug level Isw-no debug printouts
!         This is intended for diagnostic use only
!         Debug printouts wilbe added as needed
!
!-------------------------------------------------------------------------------
!</calling_doc>
!
!<advice_doc>
!-------------------------------------------------------------------------------
!                            ADVICE FOR USERS
!  On mtio_init, you need at least 1 thread and 1 pointer in the circular
!  queue.  There are no defaults if these are not set by the user.
!-------------------------------------------------------------------------------
!</advice_doc>
!
!<history_doc>
!-------------------------------------------------------------------------------
!                             REVISION HISTORY
!
!     Date        Author      Description
!     ----        ------      -----------
! 16. 2009-01-29  Bill Menger Removed more unimportant print statements, and
!                             set the "do not use this" flag to disable the
!                             thread code.
! 15. 2009-01-27  Bill Menger Re-arranged destruction of mutex and condition so
!                             that condition is destroyed before mutex, removed
!                             print statements on unimportant errors.
! 14. 2007-03-27  Kruger Corn Updated to 64 bit architecture. Basically
!                             changed long to int32_t and long long to
!                             int64_t.
! 13. 2007-02-13  Bill Menger Added "unlock" statements within each "loopback"
!                             area when the conditioned timeout gives an unknown
!                             error.  Previously, this unlock was not performed
!                             and on the rare occasion when an unknown error
!                             was returned from the conditioned wait, the code
!                             would deadlock itself, asking for a lock which it
!                             had previously already taken and not released.
!                             It isn't allowed to take the lock again until it 
!                             releases it to itself.  Result?  Some programs
!                             could hang indefinitely.  Hopefully this fixes a
!                             potentially disastrous condition.
! 12. 2006-06-12  Bill Menger Added more unit tests.
! 11. 2006-04-04  Stoeckley   Another fix to compile with C++ (apparently 
!                             caused by an update since 2005-07-26).
! 10. 2005-12-05  Chuck Burch Added getqsize/fputs and more error checks.
!                             Changed fseek, ftell, flsz and append logic.
!                             Added unit test.
!  9. 2005-08-11  Bill Menger Removed timeout messages
!  8. 2005-07-26  Bill Menger Added return variables to pthread calls, added
!                             additional mutex around critical section of 
!                             memory reallocation code.
!  7. 2005-07-26  Stoeckley   Fix to compile with C++.
!  6. 2005-06-14  Bill Menger Change pthread_create calling arguments,removed
!                             some start/stop printout messages.
!  5. 2005-05-05  Bill Menger Fix a pointer error for gcc64, reduce number of
!                             warning messages sent on timeout.
!  4. 2005-05-03  Bill Menger Change timeout to longer for flsz, fflush, fclose.
!  3. 2005-04-26  Bill Menger Added timeout on condition waits.
!  2. 2005-02-24  Bill Menger fix compiler bugs on 64sgi73, add a method so
!                             mtio can accept previously opened files.
!  1. 2005-02-08  Bill Menger Initial version.
!-------------------------------------------------------------------------------
!</history_doc>
!
!<portability_doc>
!-------------------------------------------------------------------------------
!                         PORTABILITY LIMITATIONS
!
! Requires POSIX threads.
!
!-------------------------------------------------------------------------------
!</portability_doc>
!<compile_doc>
!-------------------------------------------------------------------------------
!                      SPECIAL COMPILING REQUIREMENTS
!
! Link with the system's pthreads library.
!-------------------------------------------------------------------------------
!</compile_doc>
!
!<algorithm_doc>
!-------------------------------------------------------------------------------
!                   ALGORITHM DESCRIPTION FOR DEVELOPERS
!
!-------------------------------------------------------------------------------
!</algorithm_doc>
!
!<programming_doc>
!-------------------------------------------------------------------------------
!                           PROGRAMMING NOTES
!
! This program contains a unit test that should be run with any changes 
! to the code.  If additional functionality is added, then a test for
! that functionality should be added to the unit test. If a bug is found with
! the code, then a test to catch the bug should be added to the unit test to
! prevent a future reoccurrence of the bug.
!
! To compile with the unit test: 
!       gcc -Wall -DmtioUnitTest -o mtioUnitTest mtio.c cb.c -l pthread
! To run the unit test: 
!       mtioUnitTest Number_pthreads size_circular_buffer number_records 
!
!-------------------------------------------------------------------------------
!</programming_doc>
!------------------------------------------------------------------------------!
!--------------------------START OF CODE --------------------------------------!
!------------------------------------------------------------------------------!
*/

char MTIO_IDENT[100] =
"$Id: mtio.c,v 1.14 2007/03/28 15:09:42 Corn beta sps $";

#include "mtio.h"
/*#include "cpslog.h"*/
#include <sys/stat.h>
#include <sys/types.h>
#include <unistd.h>
#include <ctype.h>
#include <sys/time.h>
#ifdef linux
#include <asm/errno.h>  /* for ETIMEDOUT */
#else
#include <sys/errno.h>  /* for ETIMEDOUT */
#endif

#ifdef __cplusplus
extern "C" {
#endif

static int              private_mtio_status=MTIO_OFF; /* either on=1 or off=0 */
static pthread_attr_t       threadattr;
static pthread_mutexattr_t  mutexattr;
static pthread_condattr_t   condattr;
static pthread_mutex_t      mtio_lock;
static pthread_mutex_t      mtio_front_lock;
static pthread_t            **mtio_thread=NULL;

/*note mtio_initialized set to 3 is for debugging to see if problems
 happen because of mtio on or happen even when mtio is off*/
static int mtio_initialized = 3;   /*0-off, 1 on, 3-off do not turn on*/
static int mtio_numfiles    = 0;
static int mtio_numthreads  = 0;
static struct timeval  tv;
static struct timezone tz;
static struct timespec ts;
static struct timespec *ts_sleeptime=NULL, *ts_remaining=NULL;
/* static int lctr = 0; *//* loop counter to limit the number of printouts */

static cb_t *cbp         = NULL; /* this is the circular queue        */
mtio_file_t **mtio_files = NULL; /* this will hold our mtio file info */
int32_t *mtio_threadnums = NULL; /*holds thread numbers               */

static int mtio_threadstats = 0; /*if 1 print performance stats       */
static int debug_mode       = 0; /*if !=0 print debug information     */
  

/****************************** MTIO_INIT *******************************
 * mtio_init: Bill Menger 12/17/04
 * Purpose:   setup memory and file pointers and start threads for a 
 *            multi-threaded write-behind i/o scheme.
*/
int mtio_init(int *numthreads, int *mtio_qsize) {
  int ithread, ifile, i, status;

  if(debug_mode>0) fprintf(stderr,
    "Enter mtio_init: mtio_status=%d, #threads=%d, qsize=%d\n",
    private_mtio_status, *numthreads, *mtio_qsize);
 
  /* mtio_initialized set to 3 means to not turn on mtio threads*/ 
  if(mtio_initialized == 1 || mtio_initialized == 3) {
    status=0;
    goto done;
  }

  private_mtio_status = MTIO_OFF; /* This does nothing currently, but 
                                     allows me to exit
                                     if needed with an error code. */
  status=MTIO_ERROR;              /*start off with error-change later*/
  mtio_numthreads = *numthreads;
  if(mtio_numthreads <= 0 ) {
    goto done;
  }
 
  if(cbp != NULL) free(cbp); 
  if( (cbp = cb_create(*mtio_qsize)) == NULL) {
    /* failure to allocate memory */
    fprintf(stderr,"mtio_init: cb_create failure with qsize=%d\n",*mtio_qsize);
    goto done;
  }

  /*** This wait time will occur if a cond_timedwait times out in any 
       routine within mtio.  The reason for this is to keep threads from
       sitting in a deadlock waiting for a cond_signal with no way to escape
       and retry locks, test variables, and reset the cond_timedwait.
       This sleep time will happen after the timeout and before re-attempting
       to acquire the locks, reset condition_timedwait, etc.
  ***/
  if(ts_sleeptime != NULL) free(ts_sleeptime);
  if( (ts_sleeptime=(struct timespec*)malloc(sizeof(struct timespec))) == NULL){
     fprintf(stderr,"Error-mtio_init:Unable to malloc ts_sleeptime\n");
     goto done;
  }
  ts_sleeptime->tv_sec  =      0; /* seconds     */
  ts_sleeptime->tv_nsec = 100000; /* 1e5 nanoseconds = 100ms */

  if(ts_remaining != NULL) free(ts_remaining);
  if( (ts_remaining=(struct timespec*)malloc(sizeof(struct timespec))) == NULL){
     fprintf(stderr,"Error-mtio_init:Unable to malloc ts_remaining\n");
     goto done;
  }
  
  ts_remaining->tv_sec  =      0; /* not used    */
  ts_remaining->tv_nsec =      0; /* not used    */
 

  /*** set up a space to hold file information ***/
  if(mtio_files != NULL) free(mtio_files);
  mtio_files = (mtio_file_t **) malloc
    ((size_t) MTIO_INIT_NUMFILES*sizeof(mtio_file_t *));
  if(mtio_files == NULL) {
    fprintf(stderr, "mtio_init: Unable to malloc mtio_files with size=%d\n",
      MTIO_INIT_NUMFILES);
    goto done;
  }
  
  /*** set the counter to initial array size ***/
  mtio_numfiles = MTIO_INIT_NUMFILES;
  /*** nullify the array of mtio_file pointers ***/
  for(ifile=0;ifile<mtio_numfiles;ifile++){ mtio_files[ifile]=NULL;}

  /*** set up the pthreads attribute initializers for mutex, cond, and threads*/
  /*fprintf(stderr,"mtio_init: setting pthread_attr_init\n");*/
  if( (i=pthread_attr_init(&threadattr)) != 0) {
    fprintf(stderr,"mtio_init: error(%d) calling pthread_attr_init\n", i);
    goto done;
  }
  
  /*
  fprintf(stderr,"mtio_init: setting pthread_attr to detachable\n");
  pthread_attr_setdetachstate(&threadattr,
    PTHREAD_CREATE_JOINABLE);
  */
  
  if( (i=pthread_mutexattr_init(&mutexattr)) != 0 ){ 
    fprintf(stderr,"mtio_init: error(%d) calling mutexattr init\n",i);
    pthread_attr_destroy(&threadattr);
    goto done;
  }
  
  if( (i=pthread_condattr_init(&condattr)) != 0 ){ 
    fprintf(stderr,"mtio_init: error(%d) calling condattr init\n",i);
    pthread_attr_destroy(&threadattr);
    pthread_mutexattr_destroy(&mutexattr); 
    goto done;
  }

  if( (i=pthread_mutex_init(&mtio_lock,&mutexattr)) != 0){
    fprintf(stderr,"mtio_init: Error(%d) initializing mtio_lock.\n",i);
    pthread_attr_destroy(&threadattr);
    pthread_mutexattr_destroy(&mutexattr); 
    pthread_condattr_destroy(&condattr); 
    goto done;
  }

  if( (i=pthread_mutex_init(&mtio_front_lock,&mutexattr)) != 0){
    fprintf(stderr,"mtio_init: Error(%d) initializing mtio_front_lock.\n",i);
    pthread_attr_destroy(&threadattr);
    pthread_mutexattr_destroy(&mutexattr); 
    pthread_condattr_destroy(&condattr); 
    goto done;
  }


  /*** set up the threads ***/
  if(mtio_thread != NULL) free(mtio_thread);
  mtio_thread = (pthread_t **) malloc (mtio_numthreads * sizeof(pthread_t *));
  if(mtio_thread == NULL) {
    fprintf(stderr, "mtio_init: Unable to malloc mtio_thread with size=%d\n",
      mtio_numthreads);
    goto done;
  }

  if(mtio_threadnums != NULL) free(mtio_threadnums);
  if( (mtio_threadnums=(int32_t *) malloc(mtio_numthreads*sizeof(int32_t))) ==
    NULL) {
    fprintf(stderr, "mtio_init: Unable to malloc threadnums with size=%d\n",
      mtio_numthreads);
    goto done;
  }
  
  for(ithread=0;ithread<mtio_numthreads;ithread++){
    /*fprintf(stderr,"mtio_init: starting thread %d.\n",ithread);*/
    mtio_threadnums[ithread]=(int32_t) ithread;
    mtio_thread[ithread] = (pthread_t *) malloc (sizeof (pthread_t));
    if(mtio_thread[ithread] == NULL) {
      fprintf(stderr, "mtio_init: Unable to malloc mtio_thread[%d]\n", 
        ithread);
      goto done;
    }
    
    if( (i=pthread_create((pthread_t *) mtio_thread[ithread],
                   (pthread_attr_t *) &threadattr, (*mtio_writebehind),
                   (void *) &mtio_threadnums[ithread])) != 0) {
      fprintf(stderr, "mtio_init: error(%d) in pthread_create\n", i);
      goto done;
    }
    
    if(debug_mode>0) 
      fprintf(stderr,"mtio_init: started thread %d\n", ithread);        
  }

  mtio_initialized = 1;
  private_mtio_status = MTIO_ON;
  status=0;

done:  
  if(debug_mode>0) fprintf(stderr,"  Exit mtio_init: status=%d\n",status);
  return status;
}

/************************* MTIO_SET_PRINT_STATS ************************
 * mtio_set_print_stats:  Chuck Burch 9/8/05
 * Purpose: control if thread statistics printed (i!=0 yes, i=0 no)
 *          This is intended for diagnostic use only
***********************************************************************/      
void mtio_set_print_stats(int isw) {
  mtio_threadstats=isw;
  if(mtio_threadstats != 0) mtio_threadstats=1;
  return;
}

/*********************** MTIO_SET_DEBUG_MODE ***************************
 * mtio_set_debug_mode: Chuck Burch 9/9/05
 * Purpose: set debug mode, 0-print no debug, !=0 print debug prints
 *          This is intended for diagnostic use only
***********************************************************************/      
void mtio_set_debug_mode(int isw) {
  debug_mode=isw;
  return;
}

/**************************** MTO_STATUS *******************************
 * mtio_status: Bill Menger 1/10/05
 * Purpose:     Return status (either on or off) of mtio.
 *              MTIO_ON=1, MTIO_OFF=0
*/
int mtio_status() {
  return private_mtio_status;
}

/****************** MTIO_GETNUMTHREADS ********************************
 * mtio_getnumthreads: Bill Menger 1/17/05
 * Purpose:         Return number of threads.
*/
int mtio_getnumthreads() {
  return mtio_numthreads;
}

/******************* MTIO_GETQSIZE ************************************
 * mtio_getqsize: Chuck Burch 9/5/05
 * Purpose:       Return qsize.
***********************************************************************/
int mtio_getqsize() {
  if(cbp == NULL) return(0);
  return(cb_getqsize(cbp));
}

/******************* MTIO_NUMQUEUED ************************************
 * mtio_numqueued: Chuck Burch 9/9/05
 * Purpose:       Return number items queued in circular buffer
***********************************************************************/
int mtio_numqueued() {
  if(cbp == NULL) return(0);
  return(cb_numqueued(cbp));
}

/**************************** MTIO_EXIT ******************************
 * mtio_exit: Bill Menger 12/17/04
 * Purpose:   This cleans up threads, frees memory, closes files, and quits.   
 *              
*/
void mtio_exit() {
  int i, ithread,ifile,threadsleft;
  mtio_data_t **data;
  void * retvar;
 
  if(debug_mode>0) fprintf(stderr,"Executing mtio_exit\n");
  
  if(mtio_initialized == 0 || mtio_initialized == 3) return;

  /*  Close all open files */
  for (ifile=0; ifile<mtio_numfiles; ifile++){
    if(mtio_files[ifile] != NULL ) {
      mtio_fclose(mtio_files[ifile]->fd);
    }
  }
  /*fprintf(stderr,"mtio_exit: All files closed\n");*/
  /*
    fprintf(stderr,
    "Number of queue entries with all files closed = %d\n",cb_numqueued(cbp));
  */

  free(mtio_files);
  mtio_files=NULL;
  mtio_numfiles = 0;
  
  data = (mtio_data_t ** ) malloc(mtio_numthreads*sizeof(mtio_data_t *));
  if(data == NULL) {
    fprintf(stderr, "mtio_exit: Unable to malloc mtio_data with size=%d\n", 
      mtio_numthreads);
    return;
  }
  
  threadsleft=mtio_numthreads;
  /* send signals to stop all threads */
  for (ithread=0; ithread<mtio_numthreads; ithread++){
    data[ithread] = (mtio_data_t *)  malloc (sizeof(mtio_data_t));
    if(data[ithread] == NULL) {
      fprintf(stderr, "mtio_exit: Unable to malloc mtio_data_t\n"); 
    } else {
      data[ithread]->fileposition=0;
      /* put the thread id into the nbytes area */ 
      data[ithread]->nbytes=(int32_t) *mtio_thread[ithread]; /* ignored! */
      data[ithread]->buff=NULL;
      /* signal the thread by sending an ifile = -1 */ 
      data[ithread]->ifile=-1; /* this is the signal to a thread to exit */

      cb_put(cbp, data[ithread]);
      data[ithread] = NULL; /* nullify to signal we are not re-using this.
                             We have copied this pointer to the cb. */
    }
  }
  /*
  fprintf(stderr,
  "Number of queue entries after sending queue stops = %d\n",cb_numqueued(cbp));
  */

  /* join with all the threads */
  while( threadsleft > 0) {
    for(ithread=0;ithread<mtio_numthreads;ithread++){
      if(mtio_thread[ithread] != NULL ) { 
        if(pthread_join(*mtio_thread[ithread], &retvar) != 0){
          fprintf(stderr,
            "mtio_exit: Warning: thread num %d (id=%"PRId32") not joined.\n",
            ithread,(int32_t) *mtio_thread[ithread]);
        } else {
            /*
            fprintf(stderr,"mtio_exit: stopped thread %d %"PRId32" return=%"PRId32"\n",
            ithread,*mtio_thread[ithread], *(int32_t *) retvar);
            */
          free(mtio_thread[ithread]);
          mtio_thread[ithread]=NULL;
          threadsleft--;
        }
      }
    }
  }
  /* 
  fprintf(stderr,
  "Number of queue entries after all work completed = %d\n",cb_numqueued(cbp));
  fprintf(stderr,"mtio_exit: All threads turned off\n");
  */

  if( (i=pthread_attr_destroy(&threadattr)) != 0) {
    fprintf(stderr,"mtio_exit: error(%d) calling pthread_attr_destroy\n", i);
  }

  if( (i=pthread_mutexattr_destroy(&mutexattr)) != 0 ){ 
    fprintf(stderr,"mtio_exit: error(%d) calling mutexattr destroy\n",i);
  }
  
  if( (i=pthread_condattr_destroy(&condattr)) != 0 ){ 
    fprintf(stderr,"mtio_exit: error(%d) calling condattr destroy\n",i);
  }

  if( (i=pthread_mutex_destroy(&mtio_lock)) != 0){
    fprintf(stderr,"mtio_exit: Error(%d) destroying mtio_lock.\n",i);
  }
  if( (i=pthread_mutex_destroy(&mtio_front_lock)) != 0){
    fprintf(stderr,"mtio_exit: Error(%d) destroying mtio_front_lock.\n",i);
  }

  free (data);
  free (mtio_thread);
  mtio_thread=NULL;
  
  free (ts_sleeptime);
  ts_sleeptime=NULL;
  free (ts_remaining);
  ts_remaining=NULL;
  free(mtio_threadnums);
  mtio_threadnums=NULL;

  cb_destroy(cbp); /* remove the circular queue */
  cbp=NULL;
  
  mtio_initialized = 0;
  mtio_numthreads=0;
  private_mtio_status = MTIO_OFF;
  return;
}

/******************************* MTIO_FOPEN ****************************
 * mtio_fopen: Bill Menger 12/17/04
 * Purpose:    Open a file, set up mutex locks, condition variables, and states
 *             so that the mtio package can "see" this file.
*/
FILE *mtio_fopen(const char *path, const char *mode){
    int ifile, jfile, ioldsize, prv, i, iplus, no_writes, no_reads, need_seek;
    mtio_file_t *mtfp; /* use this as a scratch pointer in subsequent arrays. */
    mtio_file_t **mtio_files_temp;
    mtio_off_t  flsz, pos;
    FILE       *fd;
    char        cmode, *cptr, newmode[4];

    if(debug_mode>0) 
      fprintf(stderr,"Enter mtio_fopen: mtio_status=%d, path=%s, mode=%s\n",
       mtio_status(), path, mode);

    fd=NULL;              /*start off with fd=an error status-change later*/

    /* if no mtio running, just work as normally fopen would */
    if(mtio_status() == MTIO_OFF) {
      if(debug_mode > 0) 
        fprintf(stderr,"Warning: mtio_fopen-MTIO_OFF for %s\n",path);
      fd=fopen(path,mode);
      goto done;
    }

    /* see if file read-only or write-only to--also get filesize*/
    strncpy(newmode,mode,3);
    cmode=tolower(newmode[0]);
    iplus=0;
    no_reads=0;
    no_writes=0;
    need_seek=0;
    if(strchr(mode,'+') != NULL) iplus=1;

    if(cmode == 'w') {
      flsz=0;
      pos=0;
      if(!iplus) no_reads=1;

    } else if(cmode == 'r') {
      cptr=(char*)path; 
      flsz=mtio_flsz(cptr);
      pos=0;
      if(!iplus) no_writes=1;

    } else if(cmode == 'a') {
      /*append mode seems to have problems in thread-simulate append mode*/   
      cptr=(char*)path; 
      flsz=mtio_flsz(cptr);
      pos=flsz;
      if(!iplus) no_reads=1;
      strcpy(newmode,"r+");   /*convert a mode to r+*/
      if(strchr(mode,'b')!=NULL) strcat(newmode,"b");
      need_seek=1;

    } else {
      fprintf(stderr, "Improper mode(%s) in mtio_fopen\n",mode);
      goto done;
    }
    if(debug_mode>0) fprintf(stderr, "mode=%s, noread=%d, nowrite=%d\n",
                             newmode,no_reads, no_writes);

    ioldsize=mtio_numfiles;
    /** scan array for unused file pointer **/
    for(ifile=0;ifile<mtio_numfiles;ifile++) {
      if(mtio_files[ifile]==NULL) break;
    }
    jfile = ifile; /* hold the ifile for use later... */
    if(jfile == mtio_numfiles) /* then we did not find a pointer */ {
      /*** I should use a global lock for all of mtio here so that no one
           uses these variables until I re-allocate memory for them
      ***/
      prv=pthread_mutex_lock(&mtio_lock);     /*mtio_lock locked*/
      mtio_errmsg("fopen-mtio-lock",&prv);
      mtio_numfiles += MTIO_INIT_NUMFILES;
      mtio_files_temp = (mtio_file_t **) realloc(mtio_files, (size_t) 
                        (mtio_numfiles)*sizeof(mtio_file_t *)); 
      mtio_files = mtio_files_temp;
      for(ifile=ioldsize;ifile<mtio_numfiles;ifile++) {
        mtio_files[ifile]=NULL;
      }
      prv=pthread_mutex_unlock(&mtio_lock);  /*mtio_lock unlocked*/
      mtio_errmsg("fopen-mtio-unlock",&prv);
      /*** out of critical section ***/

    }
    ifile = jfile; /* now restore ifile to a value <= ioldsize */
    /** now ifile points to the next available file pointer in the array **/
    /** allocate memory for the file pointer infor **/
    if( (mtfp=(mtio_file_t *) malloc (sizeof(mtio_file_t))) == NULL) {
      fprintf(stderr, "mtio_fopen: Unable to malloc mtio_file_t\n"); 
      goto done;
    }
    
    if( (i=pthread_mutex_init(&mtfp->lock,&mutexattr)) != 0){
      fprintf(stderr,"mtio_fopen: Error(%d) initializing mutex %d\n",i,ifile);
      goto done;
    }
    if( (i=pthread_cond_init(&mtfp->cond,NULL)) != 0){
      fprintf(stderr,"mtio_fopen: Error(%d) initializing cond %d\n",i,ifile);
      pthread_mutex_destroy(&mtfp->lock);
      goto done;
    }

    mtfp->fd                 = NULL;
    mtfp->curpos             = pos;
    mtfp->num_pending_writes = 0;
    mtfp->signal             = MTIO_SIGNAL_OK;
    mtfp->flsz               = flsz;
    mtfp->no_reads           = no_reads;
    mtfp->no_writes          = no_writes;
    mtfp->need_seek          = need_seek;

    if( (mtfp->fd = fopen(path,newmode)) != NULL ) {
      /** then no problem opening the file.  we have the "real" fp in
          our array mtio_files.
      **/
      mtfp->descriptor  = fileno(mtfp->fd);
      mtfp->path        = (char *) malloc ((strlen(path)+1));
      if(mtfp->path!=NULL) strcpy(mtfp->path, path);

      /*** into   critical section ***/
      prv=pthread_mutex_lock(&mtio_lock);     /*mtio_lock locked*/
      mtio_errmsg("fopen-mtio-lock",&prv);

      mtio_files[ifile] = mtfp;

      /*** instrument the code ***/ 
      /* fprintf(stderr,
         "mtio_fopen: Open file (%d) [%s] %p\n",ifile,mtfp->path,mtfp->fd);
      */
      fd=mtio_files[ifile]->fd;

      prv=pthread_mutex_unlock(&mtio_lock);  /*mtio_lock unlocked*/
      mtio_errmsg("fopen-mtio-unlock",&prv);
      /*** out of critical section ***/

    } else {
      /* if we are here, the file did not open */
      fd=NULL;
      pthread_cond_destroy(&mtfp->cond);
      pthread_mutex_destroy(&mtfp->lock);
    }
 
done:
    if(debug_mode>0) {
      if(fd == NULL) {
        fprintf(stderr,"  Exit mtio_fopen with error\n");
      } else {
        fprintf(stderr,"  Exit mtio_fopen OK\n");
      }
    }   
    return fd;
}

/************************* MTIO_FIND_IFILE *********************
 * mtio_find_ifile: Bill Menger 12/17/04
 * Purpose:      Find the ifile position in the array mtio_files     
 *               using the passed FILE* fp.              
*/
int mtio_find_ifile(FILE *fd) {
  int ifile, jfile, ioldsize, prv, i;
  mtio_file_t *mtfp; /* use this as a scratch pointer in subsequent arrays. */
  mtio_file_t **mtio_files_temp;
  char path[260];
  
  /* ensure the array exists first... if not, return error */
  /*fprintf(stderr,"mtio_find_ifile: numfiles=%d fd=%p\n",mtio_numfiles,fd);*/
  if(mtio_numfiles <= 0 ) return MTIO_ERROR;
  if(fd == NULL) return MTIO_ERROR; 
  /* scan the array for a match of fd                      */

  /*** ENTER Critical Section ***/
  prv=pthread_mutex_lock(&mtio_lock);               /*mtio_lock locked*/
  mtio_errmsg("find-ifile-mtio-lock",&prv);
  for (ifile=0;ifile<mtio_numfiles;ifile++){
    if(mtio_files[ifile] != NULL) {
      /*fprintf(stderr,
        "\t\tmtio_files[%d]->fd=%p\n",ifile,mtio_files[ifile]->fd);
      */
      if(mtio_files[ifile]->fd == fd) {
        /*** Exit Critical Section ***/
        prv=pthread_mutex_unlock(&mtio_lock);       /*mtio_lock unlocked*/
        mtio_errmsg("find-ifile-mtio-unlock",&prv);
        return ifile;
      }
    }
  }
  
  /* still holding global mtio-lock */
  /* Let's enter this file in our system and move forward */
  ioldsize=mtio_numfiles;
  /** scan array for unused file pointer **/
  for(ifile=0;ifile<mtio_numfiles;ifile++) {
    if(mtio_files[ifile]==NULL) break;
  }
  jfile = ifile; /* hold the ifile for use later... */
  if(jfile == mtio_numfiles) /* then we did not find a pointer */ {
    mtio_numfiles += MTIO_INIT_NUMFILES;
    mtio_files_temp = (mtio_file_t **) realloc(mtio_files, (size_t) 
                      (mtio_numfiles)*sizeof(mtio_file_t *)); 
    mtio_files = mtio_files_temp;
    for(ifile=ioldsize;ifile<mtio_numfiles;ifile++) {
      mtio_files[ifile]=NULL;
    }
  }
  
  ifile = jfile; /* now restore ifile to a value <= ioldsize */
  /** now ifile points to the next available file pointer in the array **/
  /** allocate memory for the file pointer info **/
  if( (mtfp = (mtio_file_t *) malloc (sizeof(mtio_file_t))) == NULL) {
    fprintf(stderr, "mtio_find_ifile: Unable to malloc mtio_file_t\n"); 
    return (MTIO_ERROR);
  }
  
  mtfp->curpos             = 0;
  mtfp->num_pending_writes = 0;

  if( (i=pthread_mutex_init(&mtfp->lock,&mutexattr)) != 0){
    fprintf(stderr,"mtio_fopen: Error(%d) initializing mutex %d\n",i,ifile);
    free(mtfp);
    return (MTIO_ERROR);
  }
  if( (i=pthread_cond_init(&mtfp->cond,NULL)) != 0){
    fprintf(stderr,"mtio_fopen: Error(%d) initializing cond %d\n",i,ifile);
    free(mtfp);
    pthread_mutex_destroy(&mtfp->lock);
    return (MTIO_ERROR);
  }
  /*
    fprintf(stderr,"mtio_fopen: setting file %d signal to READY\n",ifile);
  */
  mtfp->signal    = MTIO_SIGNAL_OK;
  mtfp->fd        = fd;
  mtfp->curpos    = mtio_internal_ftell(mtfp->fd);
  mtio_internal_fseek(mtfp->fd, 0, SEEK_END);
  mtfp->flsz      = mtio_internal_ftell(mtfp->fd);
  mtfp->no_reads  = 0;          /*mode not known, treat it as read/write*/
  mtfp->no_writes = 0;
  mtfp->need_seek = 1;

  mtfp->descriptor  = fileno(mtfp->fd);
  sprintf(path,"MTIO_UNKNOWN_FILE_%d",ifile);
  mtfp->path        = (char *) malloc ((strlen(path)+1));
  if(mtfp->path!=NULL) strcpy(mtfp->path, path);

  mtio_files[ifile] = mtfp;
  prv=pthread_mutex_unlock(&mtio_lock);             /*mtio_lock unlocked*/
  mtio_errmsg("find-ifile-mtio-unlock",&prv);
  /*** Exit critical section ***/

  /*** instrument the code ***/ 
  /*
  fprintf(stderr,
     "mtio_find_ifile: Accepted file ifile=(%d) name=[%s] fp=%p, fileno=%d\n",
     ifile,mtfp->path,mtfp->fd,mtfp->descriptor);
  */  
  return ifile;
}

/***********************MTIO_FIND_FD *********************
 *  mtio_find_fd: Bill Menger 2/3/2005
 * Purpose:      Find the fd from name
 * 
*/

FILE * mtio_find_fd ( char * path) {
  int ifile,prv;
  FILE *tmp=NULL;
  /* Search the list */ 
  /*** Enter critical section ***/
  prv=pthread_mutex_lock(&mtio_lock);           /*mtio_lock locked*/
  mtio_errmsg("find-fd-mtio-lock",&prv);
  
  for(ifile=0;ifile<mtio_numfiles;ifile++) {
    if(mtio_files[ifile] != NULL) {
     if(strcmp(mtio_files[ifile]->path,path) == 0 ) {
       tmp = mtio_files[ifile]->fd;
       goto done;
     }
    }
  }

done: 
  prv=pthread_mutex_unlock(&mtio_lock);        /*mtio_lock unlocked*/
  mtio_errmsg("find-fd-mtio-unlock",&prv);
  /*** Exit critical section ***/
  /* no match found, return error */
  return tmp;
}

/******************** MTIO_INTERNAL_CLEAN_FD ************************
 * mtio_internal_clean_fd: Bill Menger 12/17/04
 * Purpose:                Clean info from the internal file pointer array
 *                         entry.
 * This routine must be called within a critical section where mtio_lock
 * is set.
*/
void mtio_internal_clean_fd(int ifile) {
  int /* prv ,*/ i;
  mtio_file_t *mtfp; /* use this as a scratch pointer in subsequent arrays. */
  
  if((mtfp=mtio_files[ifile]) == NULL) return;

  mtfp->fd                 = NULL;
  mtfp->curpos             = 0;
  mtfp->flsz               = 0;
  mtfp->no_reads           = 0;
  mtfp->no_writes          = 0;
  mtfp->need_seek          = 0;
  mtfp->num_pending_writes = 0;
  mtfp->descriptor         = -1;
  if(mtfp->path != NULL) free(mtfp->path);
  mtfp->path               = NULL;

  /*
  fprintf(stderr,
  "mtio_internal_clean_fd: destroying mutex and file condition for %d\n",
  ifile);
  */

  if( (i=pthread_cond_destroy(&mtfp->cond)) != 0){
    /* fprintf(stderr,
      "mtio_internal_clean_fd: Error(%d) in pthread_cond_destroy\n",i);
    */
  }
  if( (i=pthread_mutex_destroy(&mtfp->lock)) != 0) {
    /* fprintf(stderr,
      "mtio_internal_clean_fd: Error(%d) in pthread_mutex_destroy\n",i);
    */
  }

  mtio_files[ifile]=NULL;

  return;
}


/************************ MTIO_FCLOSE *****************************
 * mtio_fclose: Bill Menger 12/17/04
 * Purpose:     Close a file, remove locks, conditions, and memory.  Ensure
 *              all i/o is completed from threads first. 
*/
int    mtio_fclose(FILE *fd) {
  int ifile, status, prv;
  mtio_file_t mtfp_local;
  mtio_file_t *mtfp=&mtfp_local;
   /* use this as a scratch pointer in subsequent arrays. */

  if(debug_mode>0) fprintf(stderr,"Enter mtio_close\n");
  
  /* if no mtio running, just work as normally fclose would */
  if(mtio_status() == MTIO_OFF) {
    status=fclose(fd);
    goto done;
  }

  /* if not an mtio file, just work as normally fclose would */
  if( (ifile = mtio_find_ifile(fd)) == MTIO_ERROR ){
    if(fd != NULL) {
      fprintf(stderr,"mtio_fclose: Warning...can't find file.\n");
      status=fclose(fd);
    } else {
      fprintf(stderr,"mtio_fclose: file pointer NULL.\n");
      status = MTIO_ERROR;
    }
    goto done;
  }

  /*** Enter critical section ***/
  prv=pthread_mutex_lock(&mtio_lock);          /*mtio_lock locked*/
  mtio_errmsg("mtio_fclose-mtio_lock",&prv);
  /*memcpy(mtfp,mtio_files[ifile],sizeof(mtio_file_t));*/

  mtfp=mtio_files[ifile];
  if(mtfp == NULL ) {
    prv=pthread_mutex_unlock(&mtio_lock);       /*mtio_lock unlocked*/
    mtio_errmsg("mtio_fclose-mtio_unlock",&prv);
    /*** Exit critical section ***/
    fprintf(stderr,"mtio_fclose: ifile NULL pointer.\n");
    status = -1;
    goto done;
  }
  /*fprintf(stderr,"%d:mtfp->path=%s\n",__LINE__,mtfp->path);*/

  prv=pthread_mutex_unlock(&mtio_lock);       /*mtio_lock unlocked*/
  mtio_errmsg("mtio_fclose-mtio_unlock",&prv);
  /*** Exit critical section ***/

loopback:
  /* wrap up threads, ensure all i/o completed */
  prv=pthread_mutex_lock(&mtfp->lock);        /*mtfp_lock locked*/
  mtio_errmsg("mtio_fclose-mtfp_lock",&prv);
  /*
    fprintf(stderr,
    "mtio_fclose: locking mutex for %d num pending i/o = %d\n",
    ifile,mtfp->num_pending_writes);
    fprintf(stderr,"mtio_fclose: closing file %d\n",ifile);
  */
  mtfp->signal= MTIO_SIGNAL_CLOSE | mtfp->signal;

  /* wait here until all outputs are finished for this file */ 
  while (mtfp->num_pending_writes > 0 ) { 
    /*
      fprintf(stderr,
      "mtio_fclose: waiting for cond to be set on file %d\n",ifile);
    */
    gettimeofday(&tv,&tz); 
    ts.tv_sec  = tv.tv_sec;
    ts.tv_nsec = tv.tv_usec*1000;
    ts.tv_nsec += 5*MTIO_WAIT_TIME; /* set a wait time of 500 ms */
    status=pthread_cond_timedwait(&mtfp->cond,&mtfp->lock, 
           &ts);                              /*mtfp_lock unlocked then locked*/
    if(status == ETIMEDOUT) {
      prv=pthread_mutex_unlock(&mtfp->lock);              /*mtfp_lock unlocked*/
      mtio_errmsg("mtio_fclose-mtfp_unlock",&prv);
      /*
      if((lctr++)%50==0)
        fprintf(stderr,"mtio:timeout on [fclose]:unit=%d\n",ifile);
      */
      nanosleep(ts_sleeptime,ts_remaining);
      goto loopback;      
    } else if(status != 0) {
      prv=pthread_mutex_unlock(&mtfp->lock);              /*mtfp_lock unlocked*/
      /* fprintf(stderr, "mtio_fclose: cond_timedwait error(%d)\n",status); */
      goto loopback;
    }
  }

  status=0;
  if ( (fflush(mtfp->fd)) != 0 ) status--;
  /*if ((fsync(mtfp->fd))  != 0 ) status--;*/
  status += fclose(fd);
  /* instrument the code */
  /* fprintf(stderr,"mtio_fclose: %s status=(%d) %p\n",
     mtfp->path,status,mtfp->fd);
  */
  prv=pthread_mutex_unlock(&mtfp->lock);            /*mtfp_lock unlocked*/
  mtio_errmsg("fclose-unlock",&prv);

  /*** Enter critical section ***/
  prv=pthread_mutex_lock(&mtio_lock);          /*mtio_lock locked*/
  mtio_errmsg("mtio_fclose-mtio_lock",&prv);
  mtio_internal_clean_fd(ifile);
  prv=pthread_mutex_unlock(&mtio_lock);       /*mtio_lock unlocked*/
  mtio_errmsg("mtio_fclose-mtio_unlock",&prv);
  /*** Exit critical section ***/

done:
  if(debug_mode>0) fprintf(stderr,"  Exit mtio_fclose: status=%d\n",status);
  return status;
}

/************************* MTIO_FWRITE **************************
 * mtio_fwrite: Bill Menger 12/17/04
 * Purpose:     Write data to circular buffer and return.  The write-behind
 *              threads will pick up the data and do the actual writing.
*/
size_t mtio_fwrite(const void *ptr, size_t size, size_t nmemb, FILE *fd) {
  int ifile,prv;
  int32_t nbytes; 
  mtio_file_t *mtfp; /* use this as a scratch pointer in subsequent arrays. */
  mtio_data_t *data;
  char *ctemp;

  if(debug_mode>0) 
    fprintf(stderr, "Entering mtio_fwrite: mtio_status=%d, size=%d, nmemb=%d\n",
     mtio_status(), (int) size, (int) nmemb);

  if((nbytes=(int32_t) size* (int32_t) nmemb) <= 0 )  {
    nbytes=0;
    goto done;
  }

  /* if no mtio running, just work as normally fwrite would */
  if(mtio_status()==MTIO_OFF){
    nbytes=fwrite(ptr,size,nmemb,fd);
    goto done;
  }

  /* if not an mtio file, just work as normally fwrite would */
  if( (ifile=mtio_find_ifile(fd)) == MTIO_ERROR){
    fprintf(stderr,"mtio_fwrite: MTIO_ERROR on %p\n",fd);
    nbytes=fwrite(ptr,size,nmemb,fd);
    goto done;
  }

  /*** Enter critical section ***/
  prv=pthread_mutex_lock(&mtio_lock);            /*mtio_lock locked*/
  mtio_errmsg("mtio_fwrite-mtio_lock",&prv);
  mtfp = mtio_files[ifile];
  prv=pthread_mutex_unlock(&mtio_lock);         /*mtio_lock unlocked*/
  mtio_errmsg("mtio_fwrite-mtio_unlock",&prv);
  /*** Exit critical section ***/

  if(mtfp->no_writes) {
    if(debug_mode>0) 
      fprintf(stderr, "Attempting to write to read-only file-return 0\n");
    nbytes=0;
    goto done;
  }
  
  /*** lock this file ***/
  prv=pthread_mutex_lock(&mtfp->lock);          /*mtfp_lock locked*/
  mtio_errmsg("mtio_fwrite-mtfp_lock",&prv);

  /*** check for errors on this unit ***/
  if(mtfp->signal & MTIO_SIGNAL_ERROR) {
    /* Clear the error for subsequent writes */
    mtfp->signal = MTIO_SIGNAL_OK;
    prv=pthread_mutex_unlock(&mtfp->lock);     /*mtfp_lock unlocked*/
    mtio_errmsg("mtio_fwrite-mtfp_unlock",&prv);
    ctemp=mtio_inquire(mtfp->fd);
    if(ctemp!=NULL)
      fprintf(stderr,"mtio_fwrite: file %s has a write error.\n",ctemp);
    nbytes=MTIO_ERROR;
    goto done;
  }

  /* get memory for the cb pointer and load it*/
  if( (data = (mtio_data_t *)malloc (sizeof(mtio_data_t))) == NULL) {
    prv=pthread_mutex_unlock(&mtfp->lock);     /*mtfp_lock unlocked*/
    mtio_errmsg("mtio_fwrite-mtfp_unlock",&prv);
    fprintf(stderr, "mtio_write: Unable to malloc mtio_data_t\n"); 
    nbytes=MTIO_ERROR;
    goto done;
  }
  
  data->fileposition=mtfp->curpos;
  data->nbytes      =nbytes;
  data->ifile       =ifile;
  data->buff        = (char *) malloc((size_t) nbytes);
  if(data->buff == NULL) {
    prv=pthread_mutex_unlock(&mtfp->lock);     /*mtfp_lock unlocked*/
    mtio_errmsg("mtio_fwrite-mtfp_unlock",&prv);
    fprintf(stderr, "mtio_write: Unable to malloc data->buff of size=%"PRId32"\n",
      nbytes);
    free(data); 
    nbytes=MTIO_ERROR;
    goto done;
  }
  
  /*** load data into data buffer ***/
  memcpy(data->buff,(char *) ptr,(size_t) nbytes);

  if(debug_mode>1) fprintf(stderr,
  "mtio_fwrite: data=%p ifile=%d nbytes=%"PRId32" buff=%p fileposition=%"PRId64"\n",
    &data, data->ifile, data->nbytes, data->buff, data->fileposition);

  mtfp->num_pending_writes++;
  mtfp->curpos += nbytes;
  if(mtfp->curpos > mtfp->flsz) mtfp->flsz=mtfp->curpos;
  mtfp->need_seek=1;
  
  /*** unlock the file ***/
  prv=pthread_mutex_unlock(&mtfp->lock);            /*mtfp_lock unlocked*/
  mtio_errmsg("mtio_fwrite-mtfp_unlock",&prv);
  
  /*** put the data on the circular output buffer ***/
  cb_put(cbp, (void *) data);

done:
  if(debug_mode>0)
    fprintf(stderr, "  mtio_fwrite exit: returning %"PRId32"\n", nbytes);
  return ((size_t) nbytes);
}
  
/******************************* MTIO_FREAD **************************
 * mtio_fread : Bill Menger 12/17/04
 * Purpose:     Read data from a file, ensure all data is written from the
 *              circular buffer prior to the read.              
*/
size_t mtio_fread(void *ptr, size_t size, size_t nmemb, FILE *fd) {
  size_t num_read;
  int ifile, prv, status;
  mtio_file_t *mtfp;

  if(debug_mode>0) fprintf(stderr,"Enter mtio_read: size=%"PRId32", nmemb=%"PRId32"\n",
    (int32_t) size, (int32_t) nmemb);

  /* if no mtio running, just work as normally fread would */
  if(mtio_status() == MTIO_OFF) {
    num_read=fread(ptr,size,nmemb,fd);
    goto done;
  }

  /* if file not in mtio, just work as normally fread would */
  if( (ifile=mtio_find_ifile(fd)) == MTIO_ERROR){
    fprintf(stderr,"mtio_fread: MTIO_ERROR %p\n",fd);
    num_read=fread(ptr,size,nmemb,fd);
    goto done;
  }

  /*** Enter critical section ***/
  prv=pthread_mutex_lock(&mtio_lock);        /*mtio_lock locked*/
  mtio_errmsg("mtio_fread-mtio_lock",&prv);
  mtfp = mtio_files[ifile];
  prv=pthread_mutex_unlock(&mtio_lock);      /*mtio_lock unlocked*/
  mtio_errmsg("mtio_fread-mtio_unlock",&prv);
  /*** Exit critical section ***/
  
  if(mtfp->no_reads) {
    /* no read allow, simply return 0 */
    if(debug_mode) fprintf(stderr,
      "Attempting to read file not allowing reads\n");
    num_read=0;
    goto done;
  }

  /* if timeout, come back here and try again */
  /*lctr=0;*/
loopback:
  prv=pthread_mutex_lock(&mtfp->lock);         /*mtfp_lock locked*/
  mtio_errmsg("mtio_fread-mtfp_lock",&prv);
  if(mtfp->num_pending_writes > 0 ) {
    /* wait for writes to complete, then check */
    gettimeofday(&tv,&tz); 
    ts.tv_sec  = tv.tv_sec;
    ts.tv_nsec = tv.tv_usec*1000;
    ts.tv_nsec += MTIO_WAIT_TIME; /* set a wait time of 1 nanosecond max */
    status=pthread_cond_timedwait(&mtfp->cond,&mtfp->lock, 
           &ts);                         /*mtfp_lock unlocked then locked*/
    if(status == ETIMEDOUT) {
      prv=pthread_mutex_unlock(&mtfp->lock);   /*mtfp_lock unlocked*/
      mtio_errmsg("mtio_fread-mtfp_unlock",&prv);
      /*
      if((++lctr)%50==0)
        fprintf(stderr,"mtio:timeout on [fread]:unit=%d\n",ifile);
      */
      nanosleep(ts_sleeptime,ts_remaining);
      goto loopback;      
    } else if(status != 0) {
      prv=pthread_mutex_unlock(&mtfp->lock);   /*mtfp_lock unlocked*/
      /*fprintf(stderr, "mtio_fread: cond_timedwait error(%d)\n",status);*/
      goto loopback;
    }
  }

  if(mtfp->need_seek) {
    mtfp->need_seek=0;
    if(mtio_internal_fseek(mtfp->fd, mtfp->curpos, SEEK_SET) != 0 ) {
      fprintf(stderr,
        "\t%s: Error on ifile#%d Could not SEEK to correct position.\n",
        "mtio_fread",ifile);
      mtfp->signal= MTIO_SIGNAL_ERROR | mtfp->signal;
      prv=pthread_mutex_unlock(&mtfp->lock);   /*mtfp_lock unlocked*/
      mtio_errmsg("mtio_fread-mtfp_unlock",&prv);
      num_read=MTIO_ERROR;
      goto done;
    }
  }
  
  if( (int) (num_read = fread(ptr,size,nmemb,mtfp->fd)) < 0) {
    mtfp->signal= MTIO_SIGNAL_ERROR | mtfp->signal;
  } else {
    mtfp->curpos += num_read;
  }
  
  prv=pthread_mutex_unlock(&mtfp->lock);        /*mtfp_lock unlocked*/
  mtio_errmsg("mtio_fread-mtfp_unlock",&prv);

done:
  if(debug_mode>0) 
    fprintf(stderr,"  Exit mtio_fread: status=%"PRId32"\n", (int32_t)num_read);
  
  return num_read;
}
  
/******************************* MTIO_FGETS ******************************
 * mtio_fgets : Bill Menger 12/17/04
 * Purpose:     Read data from a file, ensure all data is written from the
 *              circular buffer prior to the read, simulate the fgets call
*/
char * mtio_fgets(char *s, int size, FILE *fd){
  int ifile, prv, status;
  mtio_file_t *mtfp;
  char *result;

  /* if no mtio running, just work as normally fgets would */
  if(mtio_status() == MTIO_OFF) {
    result=fgets(s,size,fd);
    goto done;
  }

  /* if file not in mtio, just work like fgets would */
  if( (ifile=mtio_find_ifile(fd)) == MTIO_ERROR) {
    fprintf(stderr,"mtio_gets: MTIO_ERROR %p\n",fd);
    result=fgets(s,size,fd);
    goto done;
  }

  /*** Enter critical section ***/
  prv=pthread_mutex_lock(&mtio_lock);             /*mtio_lock locked*/
  mtio_errmsg("mtio_fgets-mtio_lock",&prv);
  mtfp = mtio_files[ifile];
  prv=pthread_mutex_unlock(&mtio_lock);           /*mtio_lock unlocked*/
  mtio_errmsg("mtio_fgets-mtio_unlock",&prv);
  /*** Exit critical section ***/
  /* if timeout, come back here and try again */
  /*lctr=0;*/

  if(mtfp->no_reads) {
    /* no read allow, simply return NULL */
    result=NULL;
    goto done;
  }
  
loopback:
  prv=pthread_mutex_lock(&mtfp->lock);           /*mtfp_lock locked*/
  mtio_errmsg("mtio_fgets-mtfp_lock",&prv);
  if(mtfp->num_pending_writes > 0 ) {
    /* wait for writes to complete, then check */
    gettimeofday(&tv,&tz); 
    ts.tv_sec  = tv.tv_sec;
    ts.tv_nsec = tv.tv_usec*1000;
    ts.tv_nsec += MTIO_WAIT_TIME; /* set a wait time of 1 nanosecond max */
    status=pthread_cond_timedwait(&mtfp->cond,&mtfp->lock, 
            &ts);                        /*mtfp_lock unlocked then locked*/
    if(status == ETIMEDOUT) {
      prv=pthread_mutex_unlock(&mtfp->lock);      /*mtfp_lock unlocked*/
      mtio_errmsg("mtio_fgets-mtfp_unlock",&prv);
      /*
      if((++lctr)%50==0)
        fprintf(stderr,"mtio:timeout on [fgets]:unit=%d\n",ifile);
      */
      nanosleep(ts_sleeptime,ts_remaining);
      goto loopback;      
    } else if(status != 0) {
      prv=pthread_mutex_unlock(&mtfp->lock);      /*mtfp_lock unlocked*/
      /*fprintf(stderr, "mtio_fgets: cond_timedwait error(%d)\n",status);*/
      goto loopback;
    }
  }

  if(mtfp->need_seek) {
    mtfp->need_seek=0;
    if(mtio_internal_fseek(mtfp->fd, mtfp->curpos, SEEK_SET) != 0 ) {
      fprintf(stderr,
        "\t%s: Error on ifile#%d Could not SEEK to correct position.\n",
        "mtio_fread",ifile);
      mtfp->signal= MTIO_SIGNAL_ERROR | mtfp->signal;
      prv=pthread_mutex_unlock(&mtfp->lock);      /*mtfp_lock unlocked*/
      mtio_errmsg("mtio_fgets-mtfp_unlock",&prv);
      result=NULL;
      goto done;
    }
  }
  
  if( (result = fgets(s,size,mtfp->fd)) ==NULL) {
    mtfp->curpos = mtio_internal_ftell(mtfp->fd);
  } else {
    mtfp->curpos += strlen(result);
  }

  prv=pthread_mutex_unlock(&mtfp->lock);       /*mtfp_lock unlocked*/
  mtio_errmsg("mtio_fgets-mtfp_unlock",&prv);

done:
  if(debug_mode>0) {
    if(result==NULL) {
      fprintf(stderr,"  Exit mtio_fgets with error\n");
    } else {
      fprintf(stderr,"  Exit mtio_fgets with ok result\n");
    }
  }
  
  return result;
}

/******************************* MTIO_FPUTS ******************************
* mtio_fputs : Chuck Burch 09/09/05
* Purpose:     Simulate the fputs call
*************************************************************************/
int  mtio_fputs(char *s, FILE *fd){
  int ifile, prv, size, result;
  mtio_file_t *mtfp;

  if(debug_mode>0) fprintf(stderr,"Enter mtio_fputs: s=%s\n",s);
  
  /* if no mtio running, just work as normally fputs would */
  if(mtio_status()==MTIO_OFF) {
    result=fputs(s, fd);
    goto done;
  }

  /* if file not in mtio, just work like fputs would */
  if( (ifile=mtio_find_ifile(fd)) == MTIO_ERROR) {
    fprintf(stderr,"mtio_fputs: MTIO_ERROR %p\n",fd);
    result=fputs(s,fd);
    goto done;
  }
  
  if( (size=strlen(s)) == 0) {
    result=0;
    goto done;
  }

  /*** Enter critical section ***/
  prv=pthread_mutex_lock(&mtio_lock);             /*mtio_lock locked*/
  mtio_errmsg("mtio_fgets-mtio_lock",&prv);
  mtfp = mtio_files[ifile];
  prv=pthread_mutex_unlock(&mtio_lock);           /*mtio_lock unlocked*/
  mtio_errmsg("mtio_fgets-mtio_unlock",&prv);
  /*** Exit critical section ***/

  if(mtfp->no_writes) return(-1); /* no writes allow, simply return -1 */
  
  result = mtio_fwrite(s,1,size,mtfp->fd);
  if(result != size) result=-1;

done:  
  if(debug_mode>0) fprintf(stderr,"  Exit mtio_fputs: result=%d\n", result);
  
  return result;
  
}

/***************************** MTIO_FFLUSH *******************************
 * mtio_fflush: Bill Menger 12/17/04
 * Purpose:     Flush the circular buffer to the file, then flush the file
 *              on the file system.
*/
int    mtio_fflush(FILE *fd){
  int ifile, prv, status;
  mtio_file_t *mtfp;

  /* if no mtio running, just work as normally fflush would */
  if(mtio_status() == MTIO_OFF) {
    return fflush(fd);
  }
  /* if file not in mtio, just work as normally fflush would */
  if( (ifile=mtio_find_ifile(fd)) == MTIO_ERROR){
    fprintf(stderr,"mtio_fflush: MTIO_ERROR %p\n",fd);
    return (int) fflush(fd);
  }

  /*** Enter critical section ***/
  prv=pthread_mutex_lock(&mtio_lock);         /*mtio_lock locked*/
  mtio_errmsg("mtio_fflush-mtio_lock",&prv);
  mtfp = mtio_files[ifile];
  prv=pthread_mutex_unlock(&mtio_lock);       /*mtio_lock unlocked*/
  mtio_errmsg("mtio_fflush-mtio_unlock",&prv);
  /*** Exit critical section ***/
  /*fprintf(stderr,"fflush: locking %p.\n",mtfp->fd);*/
  /*lctr=0;*/

loopback:
  prv=pthread_mutex_lock(&mtfp->lock);         /*mtfp_lock locked*/
  mtio_errmsg("mtio_fflush-mtfp_lock",&prv);
  /* fprintf(stderr,"fflush: locked %p. number pending writes=%d\n",mtfp->fd,
          mtfp->num_pending_writes);
  */
  if(mtfp->num_pending_writes > 0 ) {
    /* wait for writes to complete, then check */
    /*fprintf(stderr,"fflush: waiting on condition...%p.\n",mtfp->fd);*/
    gettimeofday(&tv,&tz); 
    ts.tv_sec  = tv.tv_sec;
    ts.tv_nsec = tv.tv_usec*1000;
    ts.tv_nsec += 5*MTIO_WAIT_TIME; /* set a wait time of 500ms */
    status=pthread_cond_timedwait(&mtfp->cond,&mtfp->lock, 
           &ts);                             /*mtfp_lock unlocked then locked*/
    if(status == ETIMEDOUT) {
      prv=pthread_mutex_unlock(&mtfp->lock);    /*mtfp_lock unlocked*/
      mtio_errmsg("mtio_fflush-mtfp_unlock",&prv);
      /*
      if((++lctr)%50==0)
        fprintf(stderr,"mtio:timeout on [fflush]:unit=%d\n",ifile);
      */
      nanosleep(ts_sleeptime,ts_remaining);
      goto loopback;      
    } else if(status != 0) {
      prv=pthread_mutex_unlock(&mtfp->lock);    /*mtfp_lock unlocked*/
      /*fprintf(stderr, "mtio_fflush: cond_timedwait error(%d)\n",status);*/
      goto loopback;
    }
  }
  
  /*fprintf(stderr,"fflush: condition met %p.\n",mtfp->fd);*/
  status=0;
  if ( (fflush(mtfp->fd)) != 0 ) {
    status=MTIO_ERROR;
    mtfp->signal= MTIO_SIGNAL_ERROR | mtfp->signal;
  }

  prv=pthread_mutex_unlock(&mtfp->lock);          /*mtfp_lock unlocked*/
  mtio_errmsg("mtio_fflush-mtfp_unlock",&prv);
  return(status);

}

/*************************** MTIO_FSEEK ************************
 * mtio_fseek : Bill Menger 12/17/04
 * Purpose: seek to specified file position   
 *              
*/
int    mtio_fseek(FILE* fd, mtio_off_t offset, int whence){
  int ifile, status, prv;
  mtio_file_t *mtfp;
  int64_t flsz;
  struct stat info;

  if(debug_mode>0) 
    fprintf(stderr,"Enter mtio_fseek: off=%"PRId64", whence=%d\n",
      (int64_t) offset, whence);
  
  /* if no mtio running, just work as normally fseek would */
  if(mtio_status() == MTIO_OFF) {
    status=mtio_internal_fseek(fd,(mtio_off_t) offset,whence);
    goto done;
  }

  /* if file not in mtio, just work as normally fseek would */
  if( (ifile=mtio_find_ifile(fd)) == MTIO_ERROR) {
    fprintf(stderr,"mtio_fseek: MTIO_ERROR %p\n",fd);
    status=mtio_internal_fseek(fd,(mtio_off_t) offset,whence);
    goto done;
  }

  /*** Enter critical section ***/
  prv=pthread_mutex_lock(&mtio_lock);          /*mtio_lock locked*/
  mtio_errmsg("mtio_fseek-mtio_lock",&prv);
  mtfp = mtio_files[ifile];
  prv=pthread_mutex_unlock(&mtio_lock);        /*mtio_lock unlocked*/
  mtio_errmsg("mtio_fseek-mtio_unlock",&prv);

  prv=pthread_mutex_lock(&mtfp->lock);         /*mtfp->lock locked*/
  mtio_errmsg("mtio_fseek-mtfp_lock",&prv);

  /* seek logic changed 9/2005 by CC Burch*/  
  status=0;
  if(whence == SEEK_END){
    flsz=mtfp->flsz;            /*size if all queued IO happens*/ 
    if(mtfp->path!=NULL) {      /*see if external io changed file size*/
      if(stat(mtfp->path, &info) == 0){
        if(info.st_size>flsz) flsz=info.st_size;
      }
    }
    mtfp->curpos=flsz+offset;

  } else if(whence == SEEK_CUR) {
    mtfp->curpos+=offset;

  } else if(whence == SEEK_SET) {
    mtfp->curpos=offset;

  } else {    
    status=MTIO_ERROR;
  }

  if(mtfp->curpos < 0) mtfp->curpos=0;
  mtfp->need_seek=1;
 
  prv=pthread_mutex_unlock(&mtfp->lock);       /*mtfp->lock unlocked*/
  mtio_errmsg("mtio_fseek-unlock",&prv);
  /*** Exit critical section ***/
 
done:
  if(debug_mode>0) fprintf(stderr,"  Exit mtio_fseek: status=%d\n",status);  
  return(status);
}

/************************* MTIO_FLSZ ****************************
 * mtio_flsz  : Bill Menger 2/3/2005
 * Purpose:     To return file size 
 *              If file is not opened by mtio or if mtio is not running
 *              then return results anyway.  If stat fails, then return
 *              a -1.
 *
 */

int64_t mtio_flsz(char *path) {
  FILE        *fd; 
  int         ifile, prv;
  mtio_file_t *mtfp;
  struct stat info;
  int64_t     flsz;
  if(debug_mode>0)fprintf(stderr," Enter mtio_flsz: %s\n",path);
  if(mtio_status() == MTIO_OFF) goto not_mtio;
  /* mtio is active--see if path being used in mtio*/
  if( (fd=mtio_find_fd(path)) == NULL) goto not_mtio;

  if( (ifile=mtio_find_ifile(fd)) == MTIO_ERROR){
    fprintf(stderr,"mtio_flsz: MTIO_ERROR, path=%s\n",path);
    goto not_mtio;
  }

  /*** Enter critical section ***/
  prv=pthread_mutex_lock(&mtio_lock);     /*mtio_lock locked*/
  mtio_errmsg("mtio_flsz-mtio_lock",&prv);
  mtfp = mtio_files[ifile];
  prv=pthread_mutex_unlock(&mtio_lock);   /*mtio_lock unlocked*/
  mtio_errmsg("mtio_flsz-mtio_unlock",&prv);

  prv=pthread_mutex_lock(&mtfp->lock);    /*mtfp->lock locked*/
  mtio_errmsg("mtio_flsz-mtfp_lock",&prv);
  flsz=mtfp->flsz; /*size if all queued IO happens*/
  prv=pthread_mutex_unlock(&mtfp->lock);  /*mtfp->lock unlocked*/
  mtio_errmsg("mtio_flsz-mtfp_unlock",&prv);
  /*** Exit critical section ***/

  /*now see if any external io changed file size*/
  if(stat(path, &info) == 0){
    if(info.st_size>flsz) flsz=info.st_size;
  }
  return (flsz);

  not_mtio:
  /* this may give incorrect sz if file open with many writes and no fflush*/
  if(stat(path, &info) != 0) return (int64_t) -1;
  if(debug_mode>0)fprintf(stderr,"Exit mtio_flsz: %d: %"PRId64" %"PRId32" \n",
    __LINE__,(int64_t) info.st_size,(int32_t) info.st_size);
  return (int64_t) (info.st_size);
}
  
/****************************** MTIO_FTELL **************************
 * mtio_ftell : Bill Menger 12/17/04
 * Purpose:     Tell the current file position (at least what we THINK
 *              it should be! May need some writes flushed before it
 *              actually gets here, but it will get here someday!
*/
mtio_off_t  mtio_ftell(FILE* fd){
  int ifile,prv;
  mtio_file_t *mtfp;
  mtio_off_t curpos;

  if(debug_mode>0) fprintf(stderr,"Enter mtio_ftell\n");
  
  /* if no mtio running, just work as normally ftell would */
  if(mtio_status() == MTIO_OFF){
    curpos=mtio_internal_ftell(fd);
    goto done;
  }

  /* if file not in mtio, just work as normally ftell would */
  if( (ifile=mtio_find_ifile(fd)) == MTIO_ERROR){
    fprintf(stderr,"mtio_ftell: fd not in MTIO %p\n",fd);
    curpos=mtio_internal_ftell(fd);
    goto done;
  }

  /*** Enter critical section ***/
  prv=pthread_mutex_lock(&mtio_lock);          /*mtio_lock locked*/
  mtio_errmsg("mtio_ftell-mtio_lock",&prv);
  mtfp = mtio_files[ifile];
  prv=pthread_mutex_unlock(&mtio_lock);        /*mtio_lock unlocked*/
  mtio_errmsg("mtio_ftell-mtio_lock",&prv);
  
  prv=pthread_mutex_lock(&mtfp->lock);         /*mtfp->lock locked*/
  mtio_errmsg("mtio_ftell-mtfp_lock",&prv);
  curpos = mtfp->curpos;
  prv=pthread_mutex_unlock(&mtfp->lock);       /*mtfp->lock unlocked*/
  mtio_errmsg("mtio_ftell-mtfp_unlock",&prv);
  /*** Exit critical section ***/

done:
  if(debug_mode>0) 
    fprintf(stderr,"  Exit mtio_ftell: result=%"PRId64"\n", (int64_t) curpos);  
  return curpos;
}

/************************* MTIO_FILENO *************************
 * mtio_fileno: Bill Menger 12/17/04
 * Purpose:   return the descriptor from fileno for stream *fd  
*/
int mtio_fileno(FILE *fd){
  int ifile,prv;
  mtio_file_t *mtfp;

  if(fd == NULL) return MTIO_ERROR;
  /* if no mtio running, just work as normally fileno would */
  if(mtio_status() == MTIO_OFF) return fileno(fd);

  /* if file not in mtio, just work as normally fileno would */
  if( (ifile=mtio_find_ifile(fd)) == MTIO_ERROR) return fileno(fd);

  /*** Enter critical section ***/
  prv=pthread_mutex_lock(&mtio_lock);          /*mtio_lock locked*/
  mtio_errmsg("mtio_fileno-mtio_lock",&prv);
  mtfp = mtio_files[ifile];
  prv=pthread_mutex_unlock(&mtio_lock);        /*mtio_lock unlocked*/
  mtio_errmsg("mtio_fileno-mtio_unlock",&prv);
  /*** Exit critical section ***/
  if(mtfp == NULL ) return MTIO_ERROR;
  return mtfp->descriptor;
}

/************************* MTIO_INQUIRE ********************
 * mtio_inquire: Bill Menger 12/17/04
 * Purpose:   return name of file.  
*/
char * mtio_inquire(FILE *fd) {
  int ifile,prv;
  mtio_file_t *mtfp;

  if(mtio_status() == MTIO_OFF) return NULL;
  if( (ifile=mtio_find_ifile(fd)) == MTIO_ERROR) return NULL;

  /*** Enter critical section ***/
  prv=pthread_mutex_lock(&mtio_lock);           /*mtio_lock locked*/
  mtio_errmsg("mtio_inquire-mtio_lock",&prv);
  mtfp = mtio_files[ifile];
  prv=pthread_mutex_unlock(&mtio_lock);         /*mtio_lock unlocked*/
  mtio_errmsg("inquire-mtio-unlock",&prv);
  /*** Exit critical section ***/
  if(mtfp == NULL) return NULL;
  return mtfp->path;
}

/************************** MTIO_WRITEBEHIND *****************
 * mtio_writebehind: Bill Menger 12/17/04
 * Purpose:     
 *              
*/
void   *mtio_writebehind(void * threadarg){

  mtio_data_t *data;
  /*pthread_t   thread_id=pthread_self();*/
  int         *mtio_threadid;
  int64_t     fileposition;
  int32_t     nbytes,nbytes_written=0;
  int         ifile;
  char        *buff;
  mtio_file_t *mtfp;
  mtio_thread_t *mythread;
  /*char        threadlog[260];*/

  mtio_threadid = (int *) threadarg;

  mythread = (mtio_thread_t*)malloc( (size_t) sizeof(mtio_thread_t) );
  if(mythread == NULL) {
    fprintf(stderr, "mtio_writebehind: Unable to malloc mtio_thread_t\n");
    pthread_exit((void *) (mtio_threadid));
  }

  /*sprintf(threadlog,"%s%2.2d",".threadlog.",*mtio_threadid);*/
  /*mythread->fp=fopen(threadlog,"w");*/
  mythread->number_written=0;
  mythread->depth_used=0;
  mythread->avg_data_size=0;
  mythread->avg_depth_used=0;
  mythread->threadid=(pthread_t) *mtio_threadid;

  /* 
    fprintf(stderr,"\tThread number %d id=%"PRId32" ENTER THREAD.\n",
    *mtio_threadid,thread_id);
  */

  /* stay here in a loop until you are signalled to exit */
  while(1) {
    mythread->internal_error=0;
    /*
      fprintf(stderr,
      "\tThread number %d id=%"PRId32" TOP OF WHILE\n",
      *mtio_threadid,thread_id);
    */

    /* get data from circular buffer and copy it to local area */
    data          = (mtio_data_t *) cb_get(cbp);
    mythread->depth = cb_numqueued(cbp);
    ifile         = data->ifile;
    nbytes        = data->nbytes;
    fileposition  = data->fileposition;
    buff          = data->buff;

    /* Look for signal to terminate thread by seeing -1 for ifile */
    if(ifile < 0 ) break;

    /*** Enter critical section ***/
    mythread->prv=pthread_mutex_lock(&mtio_lock);       /*mtio_lock locked*/
    mtio_errmsg("mtio_writebehind-mtio_lock",&mythread->prv);
    mtfp = mtio_files[ifile];
    mythread->prv=pthread_mutex_unlock(&mtio_lock);     /*mtio_lock unlocked*/
    mtio_errmsg("writebehind-mtio-unlock",&mythread->prv);
    /*** Exit critical section ***/
    if(mtfp == NULL ) {
      fprintf(stderr,"\tWBT:ifile=%d nbytes=%"PRId32" fileposition=%"PRId64"\n",
      ifile,nbytes,fileposition);
      break;
    }
    /*
      fprintf(stderr,
        "\tThread %"PRId32" waiting on file lock for file %d\n",thread_id,ifile);
    */
    /* lock the file */
      mythread->prv=pthread_mutex_lock(&mtfp->lock);   /*mtfp_lock locked*/
      if(mythread->prv!=0) mtio_errmsg("writebehind-lock",&mythread->prv);
    
    /*
      fprintf(stderr,"\tThread number %d dataptr=%p\n",*mtio_threadid,data);
      fprintf(stderr,
        "\tThr# %d dta=%p dta->ifl=%d nbytes=%"PRId32" fileposition=%"PRId64" buff=%p \n",
        *mtio_threadid,data, ifile,nbytes,fileposition, buff);
    */

    /* reset the pending writes variable ... even if we fail, this write
       won't be pending any more... it just won't ever happen!
    */
    mtfp->num_pending_writes--;
    /* position file to correct place */
    if(mtio_internal_fseek(mtfp->fd,(mtio_off_t) fileposition,SEEK_SET) != 0 ) {
      fprintf(stderr,
      "\t%s: Error on ifile#%d Could not SEEK to correct position.\n",
      "mtio_writebehindthread",ifile);
      mythread->internal_error=1;
    }

    if(mythread->internal_error == 0 ) {

      /* write the data to the file */
      /* 
        fprintf(stderr,
        "\tWriting %"PRId32" bytes to file %d at position %"PRId64"\n",
        nbytes,ifile,fileposition);
      */
      nbytes_written = fwrite(buff,1,nbytes,mtfp->fd);

      if(nbytes_written != nbytes) {
        fprintf(stderr,
        "\t%s: Error on ifile#%d nbytes_written=%"PRId32", nbytes_tried:%"PRId32"\n",
        "mtio_writebehindthread", ifile,nbytes_written, nbytes);
        mythread->internal_error=1;
      }
    }

    /*
      fprintf(stderr,
        "\tIfl %d, in thr %"PRId32", nbts_wrtn=%"PRId32" nbts=%"PRId32" pending=%d filesig=%d\n",
        ifile,thread_id,nbytes_written,nbytes,mtfp->num_pending_writes,
        mtfp->signal);
    */

    /* signal mtfp->cond if pending writes now 0*/
    if(mtfp->num_pending_writes <= 0) 
      mythread->prv=pthread_cond_signal(&mtfp->cond);
    mtio_errmsg("writebehind-cond_signal",&mythread->prv);

    /* free up the i/o area in the data buffer */
    if(buff != NULL) free(buff);
    /* free up the cb pointer */
    if(data != NULL) free(data);

    if(mythread->internal_error == 1) 
      mtfp->signal = MTIO_SIGNAL_ERROR | mtfp->signal ;

    /*** instrument the thread ***/
    
    mythread->number_written++;
    mythread->depth_used=
      (mythread->depth_used > mythread->depth ?
        mythread->depth_used : mythread->depth);
    mythread->avg_data_size+=nbytes;
    mythread->avg_depth_used += mythread->depth;

    /*** end instrumentation   ***/
    
    /* unlock the file */
    mythread->prv=pthread_mutex_unlock(&mtfp->lock);    /*mtfp_lock unlocked*/
    mtio_errmsg("mtio_writebehind-mtfp_unlock",&mythread->prv);

  }
  /*
    fprintf(stderr,
    "\tThread %d exiting due to ifile < 0 (== %d). thread_id=%"PRId32".\n",
    *mtio_threadid,ifile,thread_id);
  */
  if(buff != NULL) free(buff);
  if(data != NULL) free(data);
  data=NULL;
  buff=NULL;
  
  if(mythread->number_written > 0 ) {
    mythread->avg_data_size/= mythread->number_written;
    mythread->avg_depth_used /= mythread->number_written;
  }
  
  if(mtio_threadstats) {
    fprintf(stdout,
      "thr# %d\tnbr_wrts= %"PRId64"\tavgsize= %8.1f\tavgdpth= %5.0f\tmxqdpth= %d\n",
      (int)      mythread->threadid,
                 mythread->number_written,
      (float) 1.*mythread->avg_data_size,
      (float) 1.*mythread->avg_depth_used,
      (int)      mythread->depth_used);
    fflush(stdout);
  }
  
  free(mythread);
  pthread_exit((void *) (mtio_threadid));
  return(threadarg);          /*does not get executed*/
}

/************************ MTIO_ERRMSG *************************/
void  mtio_errmsg(char* caller, int *prv){
  char str[132];
  switch (*prv) {
    case(0): return;
    case(EINVAL):
      strcpy(str,"EINVAL:The value specified is invalid.");
      break;
    case(EBUSY):
      strcpy(str,"EBUSY:The lock is busy.");
      break;
    case(EAGAIN):
      strcpy(str,"EAGAIN:System lacks necessary resources.");
      break;
    case(EDEADLK):
      strcpy(str,"EDEADLK:Deadlock condition.");
      break;
    case(EPERM):
      strcpy(str,"EPERM:The caller does not have permission.");
      break;
    case(ESRCH):
      strcpy(str,"ESRCH:Not found.");
      break;
    case(ETIMEDOUT):
      strcpy(str,"ETIMEDOUT:Timeout.");
      break;
    case(EINTR):
      strcpy(str,"EINTR.");
      break;
    default:
    strcpy(str,"Unknown error code.");
  }
  fprintf(stderr,"Thread#%d:%s: %s %d\n",(int) pthread_self(),caller,str,*prv);
  return;
}

/************************ mtio_exclusive     **********************************
 *
 * Purpose: To lock/unlock all transactions from the front-end thread
 * Menger 2006/04/05
 *****************************************************************************/
int mtio_exclusive(char * action) {
  int prv;
  if (strcmp(action,"lock") == 0 ) {
    prv=pthread_mutex_lock(&mtio_front_lock);
    mtio_errmsg("mtio_exclusive-mtio_front_lock",&prv);
    return prv;
  } else {
    if (strcmp(action,"unlock") == 0 ) {
      prv=pthread_mutex_unlock(&mtio_front_lock); 
      mtio_errmsg("mtio_exclusive-mtio_front_unlock",&prv);
      return prv;
    }
  }
  fprintf(stderr,"mtio_exclusive: bad action-must be lock or unlock.\n");
  return -1;
}

/******************** End of I/O routines using the circular buffer ***********/

#ifdef __cplusplus
}
#endif

/*---------------------------- TESTS TESTS TESTS -----------------------------*/

/*************************** Unit test for mtio *******************************/
#ifdef mtioUnitTest

/* mtio unit test for mtio.c  Chuck Burch 9/7/2005
 * compile by gcc -DmtioUnitTest -Wall mtio.c cb.c -lpthread
 * format mtioUnitTest [#threads [qsize [nrecs]]] 
 * [ means optional
 * runs a series of tests with mtio inactive and active
 * stops whenever an error is encountered
 * shuld be ran with any change in mtio.c
 *****************************************************************/
#include "mtio.h"

int test4_errs[5];
int nthreads_4, qsize_4;

/************** checks state of mtio with expected state *************/
int mtio_check_mtio_state(int lineno, int state,int nthreads, int qsize) {
  int i;
  
  if((i=mtio_status())!=state) {
    fprintf(stderr,"Wrong mtio_status at line %d: expected %d, found %d\n",
        lineno,i, state);
    return(1);
  }

  if((i=mtio_getnumthreads())!=nthreads) {
    fprintf(stderr,
        "Wrong mtio_getnumthreads() at line %d: was %d, expected %d\n",
        lineno, i, nthreads);
    return(1);
  }
  
  if((i=mtio_getqsize())!=qsize) {
    fprintf(stderr,"Wrong mtio_getqsize() at line %d: was %d, expected %d\n",
        lineno, i, qsize);
    return(1);
  }

  return(0);
}

/**************** tests basic read/writes, positioning ************/
int mtio_test0(int lineno,int state, int nthreads, int qsize,
                          int nrecs) {
  int  i, j, k, ilen, errs;
  int64_t nsize, l, l1;
  char line[80], line1[80];
  char filename[]="test.dat";
  FILE * fd;
  char rtn[ ]="test0";
  char format[ ]="Line %6.6d\n";
  char format1[ ]="Line %d\n";
  int *rand_recs=NULL, *rchecks=NULL;
  
  fprintf(stderr,
   "\nPerforming %s: mtio_status=%d, nthreads=%d, qsize=%d, nrec=%d, line=%d\n",
    rtn, state, nthreads,qsize, nrecs, lineno);
  if(mtio_initialized==3) 
    fprintf(stderr,"  mtio_initialized is in do-not-turn-mtio_on mode\n");

  if(mtio_check_mtio_state(lineno, state, nthreads, qsize)!=0) return(1);
  
  errs=0;
  
  fprintf(stderr,
    "  Writing data using fwrite comparing with data read using mtio_fread\n");

  if( (fd=fopen(filename,"w+")) == NULL) {
    fprintf(stderr,"Error fopen(%s)\n", filename);
    errs++;
    goto done;
  }

  /*write data using fwrite*/
  for(i=0; i<nrecs; i++){
    ilen=sprintf(line, format1, i);
    j=fwrite(line, (size_t) 1, (size_t) ilen, fd);
    if(j != ilen) {
      errs++;
      fprintf(stderr,"fwrite error: status=%d, should be %d\n", j, ilen);
      goto done;
    }
  }

  if( (i=fclose(fd)) != 0) {
    errs++;
    fprintf(stderr,"fclose error(%d)\n",i);
    goto done;
  }
 
  /* now open with mtio to read data and check result*/ 
  if( (fd=mtio_fopen(filename,"r")) == NULL) {
    fprintf(stderr,"Error mtio_fopen(%s)\n", filename);
    errs++;
    goto done;
  }

  /* read and check data*/
  for(i=0; i<nrecs; i++){
    ilen=sprintf(line, format1, i);
    j=mtio_fread(line1, (size_t) 1, (size_t) ilen, fd);
    line1[ilen]='\0';
    if(j != ilen){ 
      errs++;
      fprintf(stderr,"mtio_fread error: status=%d, should be %d\n", j, ilen);
    } else if(strcmp(line,line1) != 0){
      errs++;
      fprintf(stderr,
        "mtio_fread data error: read=%s, should be %s\n", line1, line);
    }
  }
  
  if( (i=mtio_fclose(fd)) != 0) {
    errs++;
    fprintf(stderr,"mtio_close error(%d)\n",i);
    goto done;
  }
  /* mtio_fread seems to work so trust it*/
 

  fprintf(stderr,
  "  Writing data with mtio_fwrite-check positioning/results with mtio_read\n");
  if( (fd=mtio_fopen(filename,"w+")) == NULL) {
    fprintf(stderr,"Error opening file(%s)\n", filename);
    errs++;
    goto done;
  }

  /*write data using mtio_fwrite*/
  nsize=0;
  for(i=0; i<nrecs; i++){
    ilen=sprintf(line, format1, i);
    j=mtio_fwrite(line, (size_t) 1, (size_t) ilen, fd);
    if(j != ilen) {
      errs++;
      fprintf(stderr,"mtio_fwrite error: status=%d, should be %d\n", j, ilen);
    }
    nsize+=ilen;
  }

  /* check ftell position */
  if((l=mtio_ftell(fd)) != nsize) {
    fprintf(stderr, "mtio_tell error: got %"PRId64", expected %"PRId64"\n", l, nsize);
    errs++;
  }
  
  /* check file size */
  if((i=mtio_fflush(fd)) != 0 ) {
    fprintf(stderr,"mtio_fflush error(%d)\n",i);
    errs++;
  }

  if((l=mtio_flsz(filename)) != nsize) {
    fprintf(stderr,
      "%d mtio_flsz error: got %"PRId64", expected %"PRId64"\n",__LINE__, l, nsize);
    errs++;
  }
  
  /* test seek modes*/
  l1=nsize/2;
  if((i=mtio_fseek(fd,-l1,SEEK_CUR)) != 0) {
    fprintf(stderr,"mtio_fseek error(%d)\n", i);
    errs++;
  } else if( (l=mtio_ftell(fd)) != (nsize-l1)) {
    fprintf(stderr,
      "mtio_SEEK_CUR/ftell error:ftell=%"PRId64", should=%"PRId64"\n", l, nsize-l1);
    errs++;
  }
  
  if((i=mtio_fseek(fd,0,SEEK_END)) != 0) {
    fprintf(stderr,"mtio_fseek error-SEEK_END(%d)\n", i);
    errs++;
  } else if((l=mtio_ftell(fd)) != nsize) {
    fprintf(stderr,
      "mtio_SEEK_END/ftell error:ftell=%"PRId64", should=%"PRId64"\n", l,nsize);
    errs++;
  }
 
  /*rewind file*/ 
  if( (i=mtio_fseek(fd,0,SEEK_SET)) != 0) {
    fprintf(stderr,"mtio_fseek error-SEEK_SET(%d)\n", i);
    errs++;
  } else if( (l=mtio_ftell(fd)) != 0) {
    fprintf(stderr,"mtio_SEEK_SET/ftell error:ftell=%"PRId64", should=%d\n", l,0);
    errs++;
  }
  /*Basic positions seems to work*/
  
  /* read and check data*/
  for(i=0; i<nrecs; i++){
    ilen=sprintf(line, format1, i);
    j=mtio_fread(line1, (size_t) 1, (size_t) ilen, fd);
    line1[ilen]='\0';
    if(j != ilen){ 
      errs++;
      fprintf(stderr,"mtio_fread error: status=%d, should be %d\n", j, ilen);
    } else if(strcmp(line,line1) != 0){
      errs++;
      fprintf(stderr,
        "mtio_fread data error: read=%s, should be %s\n", line1, line);
    }
  }
  /*mtio_fwrite seems to basically work*/
  
  if( (i=mtio_fclose(fd)) != 0) {
    errs++;
    fprintf(stderr,"mtio_close error(%d)\n",i);
    goto done;
  }
  
  fprintf(stderr,"  Checking mtio_fflush\n"); 
  if( (fd=mtio_fopen(filename,"w+")) == NULL) {
    fprintf(stderr,"Error opening file(%s)\n", filename);
    errs++;
    goto done;
  }

  /*write data using mtio_fwrite*/
  for(i=0; i<nrecs; i++){
    ilen=sprintf(line, format1, i);
    j=mtio_fwrite(line, (size_t) 1, (size_t) ilen, fd);
    if(j != ilen) {
      errs++;
      fprintf(stderr,"mtio_fwrite error: status=%d, should be %d\n", j, ilen);
      goto done;
    }
  }
  
  j=mtio_numqueued();
  if(j>0) {
    if( (i=mtio_fflush(fd)) != 0) {
      fprintf(stderr,"mtio_flush error=%d\n",i);
      errs++;
      goto done;
    }
    if( (i=mtio_numqueued()) != 0) {
     fprintf(stderr,
       "  mtio_fflush error:queue size before fflush=%d, after=%d\n",j,i);
     errs++;
     goto done;
    } else {
      fprintf(stderr,
        "  fflush seemed to work, queue size before fflush=%d\n",j);
    }
  } else {
    fprintf(stderr,"  Note: Unable to test fflush since queue size was zero\n");
  }
  
  if( (i=mtio_fclose(fd)) != 0) {
    errs++;
    fprintf(stderr,"mtio_close error(%d)\n",i);
    goto done;
  }
  
  fprintf(stderr,"  Testing random writes and reads\n");
  if( (rand_recs=(int*)malloc(nrecs*sizeof(int))) == NULL) {
    fprintf(stderr,"Unable to malloc rand_recs with size=%d\n",nrecs);
    errs++;
    goto done;
  }

  if( (rchecks=(int*)malloc(nrecs*sizeof(int))) == NULL) {
    fprintf(stderr,"Unable to malloc rcheck with size=%d\n",nrecs);
    errs++;
    goto done;
  }
  
  if( (fd=mtio_fopen(filename,"w+")) == NULL) {
    fprintf(stderr,"Error opening file(%s)\n", filename);
    errs++;
    goto done;
  }

  /*write data using mtio_fwrite*/
  for(i=0; i<nrecs; i++) rand_recs[i]=i;  /*for generating rand rec#s*/
  for(i=0; i<nrecs; i++) rchecks[i]=0;    /*for ensuring rec written once*/
  ilen=sprintf(line, format, 0);

  for(k=0; k<nrecs; k++){
    j=(int)(1.*(nrecs-k)*rand()/(RAND_MAX+1.));
    i=rand_recs[j];
    rand_recs[j]=rand_recs[nrecs-1-k];
    sprintf(line, format, i);
    rchecks[i]++;        /*rec i about to be written*/
    /* fprintf(stderr,"random writes:k=%d, j=%d, rec#=%d\n",k,j, i); */
    
    l=i*ilen;     /*write rec i*/
    if( (i=mtio_fseek(fd, l, SEEK_SET)) != 0) {
      fprintf(stderr,"mtio_fseek error-SEEK_SET(%d)\n", i);
      errs++;
    } else if( (l1=mtio_ftell(fd)) != l) {
      fprintf(stderr,
        "mtio_SEEK_SET/ftell error:ftell=%"PRId64", should=%"PRId64"\n", l, l1);
      errs++;
    }
  
    j=mtio_fwrite(line, (size_t) 1, (size_t) ilen, fd);
    if(j != ilen) {
      errs++;
      fprintf(stderr,"mtio_fwrite error, rec=%d: status=%d, should be %d\n", 
        i, j, ilen);
      goto done;
    }
  }
  
  /* this is really test of random record generation logic*/
  for(i=0; i<nrecs; i++) {
    if(rchecks[i] != 1) {
      fprintf(stderr,"rchecks[%d]=%d\n",i, rchecks[i]);
      if(rchecks[i] < 1) fprintf(stderr,"Rec#%d not written to\n",i);
      if(rchecks[i] > 1) fprintf(stderr,"Rec#%d written more than once(%d)\n",
        i, rchecks[i]);
      errs++;
    }
  }
  if(errs > 0) goto done;

  /*rewind file*/ 
  if( (i=mtio_fseek(fd,0,SEEK_SET)) != 0) {
    fprintf(stderr,"mtio_fseek error-SEEK_SET(%d)\n", i);
    errs++;
  } else if( (l=mtio_ftell(fd)) != 0) {
    fprintf(stderr,"mtio_SEEK_SET/ftell error:ftell=%"PRId64", should=%d\n", l,0);
    errs++;
  }
  
  /* read and check data*/
  for(i=0; i<nrecs; i++) rand_recs[i]=i;  /*for generating rand rec#s*/
  for(i=0; i<nrecs; i++) rchecks[i]=0;    /*for ensuring rec written once*/

  for(k=0; k<nrecs; k++){
    j=(int)(1.*(nrecs-k)*rand()/(RAND_MAX+1.));
    i=rand_recs[j];
    rand_recs[j]=rand_recs[nrecs-1-k];
    sprintf(line, format, i);
    /*printf("random reads:k=%d, j=%d, rec#=%d\n", k, j, i);*/
    rchecks[i]++;
    
    l=i*ilen;  /*read rec i*/
    if( (i=mtio_fseek(fd, l, SEEK_SET)) != 0) {
      fprintf(stderr,"mtio_fseek error-SEEK_SET(%d)\n", i);
      errs++;
    } else if( (l1=mtio_ftell(fd)) != l) {
      fprintf(stderr,
        "mtio_SEEK_SET/ftell error:ftell=%"PRId64", should=%"PRId64"\n", l, l1);
      errs++;
    }
  
    j=mtio_fread(line1, (size_t) 1, (size_t) ilen, fd);
    line1[ilen]='\0';
    if(j != ilen){ 
      errs++;
      fprintf(stderr,
        "mtio_fread error, rec=%d: status=%d, should=%d\n", i, j, ilen);
    } else if(strcmp(line,line1) != 0){
      errs++;
      fprintf(stderr,"mtio_fread data error, rec=%d: read=%s, should be %s\n", 
        i, line1, line);
    }
  }
  
  /* this is really test of random record generation logic*/
  for(i=0; i<nrecs; i++) {
    if(rchecks[i] != 1) {
      if(rchecks[i] < 1) fprintf(stderr,"Rec#%d not read\n",i);
      if(rchecks[i] > 1) fprintf(stderr,"Rec#%d read more than once(%d)\n",
        i, rchecks[i]);
      errs++;
    }
  }

  if( (i=mtio_fclose(fd)) != 0) {
    errs++;
    fprintf(stderr,"mtio_close error(%d)\n",i);
    goto done;
  }
  
done:  
  if(errs > 0){
    fprintf(stderr,"  Errors=%d with this instance of %s\n", errs, rtn);
  } else {
    fprintf(stderr,"  No errors found in this instance of %s\n", rtn);
  }

  remove(filename);
  if(rand_recs != NULL) {
    free(rand_recs);
    rand_recs=NULL;
  }
  
  if(rchecks != NULL) {
    free(rchecks);
    rchecks=NULL;
  }
  
  return(errs);
}


/********* basic test using r,w,a,r+,w+,a+ modes and fputs/fgets **********/
int mtio_test1(int lineno,int state, int nthreads, int qsize,
                          int nrecs, char *text_bin_mode) {
  int  i, j, recl, errs, nerrs;
  int64_t nsize, l, pos, sizex;
  char line[80], line1[80], *cptr, open_mode[5];
  char filename[]="test.dat";
  FILE * fd;
  char rtn[ ]="test1";
  char format[]="Line %5.5d\n";
  
  fprintf(stderr,
    "\nPerforming %s:mtio_status=%d, nthreads=%d, qsize=%d, nrec=%d, line#%d\n",
    rtn, state, nthreads,qsize, nrecs, lineno);

  if(mtio_check_mtio_state(lineno, state, nthreads, qsize)!=0) return(1);
  
  nerrs=0;
  recl=sprintf(line,format,0);
  sizex=nrecs*recl;

/* Testing w mode*/
  errs=0;
  strcpy(open_mode,"w");
  strcat(open_mode,text_bin_mode);
  fprintf(stderr,"  Testing %s mode\n", open_mode);  
  if( (fd=mtio_fopen(filename,open_mode)) == NULL) {
    fprintf(stderr,"  Error opening file(%s)\n", filename);
    errs++;
    goto testr;
  }

  /* check opened file size and position */
  pos=mtio_ftell(fd);
  if( (i=mtio_fseek(fd,0,SEEK_END)) != 0) {
    fprintf(stderr,"  Error seeking file(%s), pos=END\n", filename);
    errs++;
  }
  
  nsize=mtio_ftell(fd);
  if( (i=mtio_fseek(fd,pos,SEEK_SET)) != 0){
    fprintf(stderr,"  Error seeking file(%s), pos=%"PRId64"\n", filename,pos);
    errs++;
  }
    
  if(nsize != 0 || pos != 0) {
    fprintf(stderr,
      "  Wrong size(%"PRId64"), pos(%"PRId64") in opening file(%s), expected 0/0\n",
      nsize,pos,filename);
    errs++;
    mtio_fclose(fd);
    goto testr;
  }
 
  /* write known data */ 
  for(i=0; i<nrecs; i++){
    sprintf(line,format,i);
    j=mtio_fwrite(line, (size_t) 1, (size_t) recl, fd);
    if(j != recl){ 
      errs++;
      fprintf(stderr,"  mtio_fwrite error: status=%d, should be %d\n", j, recl);
      mtio_fclose(fd);
      goto testr;
    }
  }

  /* check ftell position */
  if( (l=mtio_ftell(fd)) != sizex) {
    fprintf(stderr,"  mtio_tell error after write: got %"PRId64", expected %"PRId64"\n", 
      l, sizex);
    errs++;
  }
  
  if( (i=mtio_fseek(fd,0,SEEK_SET)) != 0){
    fprintf(stderr,"  Error seeking file(%s) after write pos=0\n", filename);
    errs++;
  }
    
  /* ensure file is not readable*/
  if( (j=mtio_fread(line1, (size_t) 1, (size_t) recl, fd)) != 0) {
    errs++;
    fprintf(stderr,"  mtio_fread error in w-mode: status=%d, should be 0\n", j);
  }
  
  if( (i=mtio_fclose(fd)) != 0) {
    errs++;
    fprintf(stderr,"  mtio_close error(%d)\n",i);
  }
  
  /* check file size */
  if( (l=mtio_flsz(filename)) != sizex) {
    fprintf(stderr,"  mtio_flsz error: got %"PRId64", expected %"PRId64"\n", l, sizex);
    errs++;
  }
  
/* Testing r mode */  
testr:
  if( (nerrs+=errs) > 0) goto done;
 
  errs=0;
  strcpy(open_mode,"r");
  strcat(open_mode,text_bin_mode);
  fprintf(stderr,"  Testing %s mode\n", open_mode);  
  if( (fd=mtio_fopen(filename,open_mode)) == NULL) {
    fprintf(stderr,"  Error opening file(%s)\n", filename);
    errs++;
    goto testa;
  }

  /* check opened file size and position */
  pos=mtio_ftell(fd);
  if( (i=mtio_fseek(fd,0,SEEK_END)) != 0) {
    fprintf(stderr,"  Error seeking file(%s), pos=END\n", filename);
    errs++;
  }
  
  nsize=mtio_ftell(fd);
  if( (i=mtio_fseek(fd,pos,SEEK_SET)) != 0){
    fprintf(stderr,"  Error seeking file(%s), pos=%"PRId64"\n", filename,pos);
    errs++;
  }
    
  if(nsize != sizex || pos != 0) {
    fprintf(stderr,
    "  Wrong size(%"PRId64"), pos(%"PRId64") in opening file(%s), expected %"PRId64"/0\n",
      nsize,pos,filename,sizex);
    errs++;
    mtio_fclose(fd);
    goto testa;
  }
  
  /* Ensure file is not writeable*/
  sprintf(line,format,i);
  if( (j=mtio_fwrite(line, (size_t) 1, (size_t) recl, fd)) != 0) {
    errs++;
    fprintf(stderr,
      "  mtio_fwrite error with r-mode: status=%d, should be 0\n", j);
  }

  if( (i=mtio_fseek(fd,0,SEEK_SET)) != 0){
    fprintf(stderr,"  Error seeking file(%s) after write pos=0\n", filename);
    errs++;
  }
  
  /* read file and check data*/
  for(i=0; i<nrecs; i++){
    sprintf(line,format,i);
    j=mtio_fread(line1, (size_t) 1, (size_t) recl, fd);
    line1[recl]='\0';
    if(j != recl){ 
      errs++;
      fprintf(stderr,"  mtio_fread error: status=%d, should be %d\n", j, recl);
      mtio_fclose(fd);
      goto testa;
    } else if(strcmp(line,line1) != 0){
      errs++;
      fprintf(stderr,
       "  mtio_fread data error: read=%s, should be %s\n", line1, line);
      mtio_fclose(fd);
      goto testa;
    }
  }
  
  if( (i=mtio_fclose(fd)) != 0) {
    errs++;
    fprintf(stderr,"  mtio_close error(%d)\n",i);
  }
  
  /* check file size */
  if( (l=mtio_flsz(filename)) != sizex) {
    fprintf(stderr,"  mtio_flsz error: got %"PRId64", expected %"PRId64"\n", l, sizex);
    errs++;
  }
  
/* Testing a mode */
testa:
  if( (nerrs+=errs) > 0) goto done;
   
  errs=0;
  strcpy(open_mode,"a");
  strcat(open_mode,text_bin_mode);
  fprintf(stderr,"  Testing %s mode\n", open_mode);  
  if( (fd=mtio_fopen(filename,open_mode)) == NULL) {
    fprintf(stderr,"  Error opening file(%s)\n", filename);
    errs++;
    goto testwplus;
  }

  if( (i=mtio_fseek(fd,0,SEEK_END)) != 0) {
    fprintf(stderr,"  Error seeking file(%s), pos=END\n", filename);
    errs++;
  }
  
  nsize=mtio_ftell(fd);
  if(nsize != sizex) {
    fprintf(stderr,
    "  Wrong size(%"PRId64") in opening file(%s), expected %"PRId64"\n",
      nsize,filename,sizex);
    errs++;
    mtio_fclose(fd);
    goto testwplus;
  }
  
  /* append some new data*/
  for(i=0; i<nrecs; i++){
    sprintf(line,format,nrecs+i);
    j=mtio_fwrite(line, (size_t) 1, (size_t) recl, fd);
    if(j != recl){ 
      errs++;
      fprintf(stderr,"  mtio_fwrite error: status=%d, should be %d, rec=%d\n", 
        j, recl, i);
      mtio_fclose(fd);
      goto testwplus;
    }
  }

  /* check ftell position */
  if( (l=mtio_ftell(fd)) != 2*sizex) {
    fprintf(stderr,"  mtio_tell error after write: got %"PRId64", expected %"PRId64"\n", 
      l, 2*sizex);
    errs++;
  }
  
  if( (i=mtio_fseek(fd,0,SEEK_SET)) != 0){
    fprintf(stderr,"  Error seeking file(%s) after write pos=0\n", filename);
    errs++;
  }
  
  /*Ensure file is not readable*/
  if( (j=mtio_fread(line1, (size_t) 1, (size_t) recl, fd)) != 0) {
    fprintf(stderr,
      "  Error reading file in 'a' mode, status=%d, should be 0\n", j);
    errs++;
  }

  if( (i=mtio_fclose(fd)) != 0){
    errs++;
    fprintf(stderr,"  mtio_close error(%d)",i);
  }
  
  /*Reopen file r mode and check file data*/
  if( (fd=mtio_fopen(filename,"r")) == NULL) {
    fprintf(stderr,"  Error opening file(%s) for read check\n", filename);
    errs++;
    goto testwplus;
  }
  
  for(i=0; i<2*nrecs; i++){
    sprintf(line,format,i);
    j=mtio_fread(line1, (size_t) 1, (size_t) recl, fd);
    line1[recl]='\0';
    if(j != recl){ 
      errs++;
      fprintf(stderr,"  mtio_fread error: status=%d, should be %d, rec=%d\n", 
        j, recl, i);
    } else if(strcmp(line,line1) != 0){
      errs++;
      fprintf(stderr,
       "  mtio_fread data error: rec=%d, read=%s, should be %s\n", 
       i, line1, line);
    }
  }
  
  if( (i=mtio_fclose(fd)) != 0){
    errs++;
    fprintf(stderr,"  mtio_close error(%d) after read check\n",i);
  }
  
  /* check file size */
  if( (l=mtio_flsz(filename)) != 2*sizex) {
    fprintf(stderr,"  mtio_flsz error: got %"PRId64", expected %"PRId64"\n", l, 2*sizex);
    errs++;
  }

/* Testing w+ mode*/
testwplus:
  if( (nerrs+=errs) > 0) goto done;
  
  errs=0;
  strcpy(open_mode,"w+");
  strcat(open_mode,text_bin_mode);
  fprintf(stderr,"  Testing %s mode\n", open_mode);  
  if( (fd=mtio_fopen(filename,open_mode)) == NULL) {
    fprintf(stderr,"  Error opening file(%s)\n", filename);
    errs++;
    goto testrplus;
  }

  /* check opened file size and position */
  pos=mtio_ftell(fd);
  if( (i=mtio_fseek(fd,0,SEEK_END)) != 0) {
    fprintf(stderr,"  Error seeking file(%s), pos=END\n", filename);
    errs++;
  }
  
  nsize=mtio_ftell(fd);
  if( (i=mtio_fseek(fd,pos,SEEK_SET)) != 0){
    fprintf(stderr,"  Error seeking file(%s), pos=%"PRId64"\n", filename,pos);
    errs++;
  }
    
  if(nsize != 0 || pos != 0) {
    fprintf(stderr,
      "  Wrong size(%"PRId64"), pos(%"PRId64") in opening file(%s), expected 0/0\n",
      nsize,pos,filename);
    errs++;
  }
  
  /*Write known data*/
  for(i=0; i<nrecs; i++){
    sprintf(line,format,i);
    j=mtio_fwrite(line, (size_t) 1, (size_t) recl, fd);
    if(j != recl){ 
      errs++;
      fprintf(stderr,"  mtio_fwrite error: status=%d, should be %d\n", j, recl);
      mtio_fclose(fd);
      goto testrplus;
    }
  }

  /* check ftell position */
  if( (l=mtio_ftell(fd)) != sizex) {
    fprintf(stderr,"  mtio_tell error after write: got %"PRId64", expected %"PRId64"\n", 
      l, sizex);
    errs++;
  }
  
  if( (i=mtio_fseek(fd,0,SEEK_SET)) != 0){
    fprintf(stderr,"  Error seeking file(%s) after write pos=0\n", filename);
    errs++;
  }
    
  /* read data and check it*/
  for(i=0; i<nrecs; i++){
    sprintf(line,format,i);
    j=mtio_fread(line1, (size_t) 1, (size_t) recl, fd);
    line1[recl]='\0';
    if(j != recl){ 
      errs++;
      fprintf(stderr,
        "  mtio_fread error: status=%d, should be %d\n", j, recl);
    } else if(strcmp(line,line1) != 0){
      errs++;
      fprintf(stderr,
       "  mtio_fread data error: read=%s, should be %s\n", line1, line);
    }
  }
  
  if( (i=mtio_fclose(fd)) != 0) {
    errs++;
    fprintf(stderr,"  mtio_close error(%d)\n",i);
  }
  
  /* check file size */
  if( (l=mtio_flsz(filename)) != sizex) {
    fprintf(stderr,"  mtio_flsz error: got %"PRId64", expected %"PRId64"\n", l, sizex);
    errs++;
  }

/*Testing r+ mode*/
testrplus:
  if( (nerrs+=errs) > 0) goto done;
  
  errs=0;
  strcpy(open_mode,"r+");
  strcat(open_mode,text_bin_mode);
  fprintf(stderr,"  Testing %s mode\n", open_mode);  
  if( (fd=mtio_fopen(filename,open_mode)) == NULL) {
    fprintf(stderr,"  Error opening file(%s)\n", filename);
    errs++;
    goto done;
  }

  /* check opened file size and position */
  pos=mtio_ftell(fd);
  if( (i=mtio_fseek(fd,0,SEEK_END)) != 0) {
    fprintf(stderr,"  Error seeking file(%s), pos=END\n", filename);
    errs++;
  }
  
  nsize=mtio_ftell(fd);
  if( (i=mtio_fseek(fd,pos,SEEK_SET)) != 0){
    fprintf(stderr,"  Error seeking file(%s), pos=%"PRId64"\n", filename,pos);
    errs++;
  }
    
  if(nsize != sizex || pos != 0) {
    fprintf(stderr,
      "  Wrong size(%"PRId64"), pos(%"PRId64") in opening file(%s), expected %"PRId64"/0\n",
      nsize,pos,filename,sizex);
    errs++;
  }
  
  /*write known data*/  
  for(i=0; i<nrecs; i++){
    sprintf(line,format,i);
    j=mtio_fwrite(line, (size_t) 1, (size_t) recl, fd);
    if(j != recl){ 
      errs++;
      fprintf(stderr,"  mtio_fwrite error: status=%d, should be %d\n", j, recl);
    }
  }

  /* check ftell position */
  if( (l=mtio_ftell(fd)) != sizex) {
    fprintf(stderr,"  mtio_tell error after write: got %"PRId64", expected %"PRId64"\n", 
      l, sizex);
    errs++;
  }
  
  if( (i=mtio_fseek(fd,0,SEEK_SET)) != 0){
    fprintf(stderr,"  Error seeking file(%s) after write pos=0\n", filename);
    errs++;
  }

  /*read and check data*/
  for(i=0; i<nrecs; i++){
    sprintf(line,format,i);
    j=mtio_fread(line1, (size_t) 1, (size_t) recl, fd);
    line1[recl]='\0';
    if(j != recl){ 
      errs++;
      fprintf(stderr,"  mtio_fread error: status=%d, should be %d\n", j, recl);
    } else if(strcmp(line,line1) != 0){
      errs++;
      fprintf(stderr,
       "  mtio_fread data error: read=%s, should be %s\n", line1, line);
    }
  }
  
  if( (i=mtio_fclose(fd)) != 0) {
    errs++;
    fprintf(stderr,"  mtio_close error(%d)\n",i);
  }
  
  /* check file size */
  if( (l=mtio_flsz(filename)) != sizex) {
    fprintf(stderr,"  mtio_flsz error: got %"PRId64", expected %"PRId64"\n", l, sizex);
    errs++;
  }
  
/* Testing a+ mode*/
/* testaplus: */
  if( (nerrs+=errs) > 0) goto done;
  
  errs=0;
  strcpy(open_mode,"a+");
  strcat(open_mode,text_bin_mode);
  fprintf(stderr,"  Testing %s mode\n", open_mode);  
  if( (fd=mtio_fopen(filename,open_mode)) == NULL) {
    fprintf(stderr,"  Error opening file(%s)\n", filename);
    errs++;
    goto testputs;
  }

  /* check opened file size and position */
  if( (i=mtio_fseek(fd,0,SEEK_END)) != 0) {
    fprintf(stderr,"  Error seeking file(%s), pos=END\n", filename);
    errs++;
  }
  
  nsize=mtio_ftell(fd);
  if(nsize != sizex) {
    fprintf(stderr,
    "  Wrong size(%"PRId64") in opening file(%s), expected %"PRId64"\n",
      nsize,filename,sizex);
    errs++;
    mtio_fclose(fd);
    goto testputs;
  }
  
  /*append known data*/
  for(i=0; i<nrecs; i++){
    sprintf(line,format,nrecs+i);
    j=mtio_fwrite(line, (size_t) 1, (size_t) recl, fd);
    if(j != recl){ 
      errs++;
      fprintf(stderr,"  mtio_fwrite error: status=%d, should be %d, rec=%d\n", 
        j, recl, i);
      mtio_fclose(fd);
      goto testputs;
    }
  }

  /* check ftell position */
  if( (l=mtio_ftell(fd)) != 2*sizex) {
    fprintf(stderr,"  mtio_tell error after write: got %"PRId64", expected %"PRId64"\n", 
      l, 2*sizex);
    errs++;
  }
  
  if( (i=mtio_fseek(fd,0,SEEK_SET)) != 0){
    fprintf(stderr,"  Error seeking file(%s) after write pos=0\n", filename);
    errs++;
  }
  
  /*read and check data*/
  for(i=0; i<2*nrecs; i++){
    sprintf(line,format,i);
    j=mtio_fread(line1, (size_t) 1, (size_t) recl, fd);
    line1[recl]='\0';
    if(j != recl){ 
      errs++;
      fprintf(stderr,"  mtio_fread error: status=%d, should be %d, rec=%d\n", 
        j, recl, i);
    } else if(strcmp(line,line1) != 0){
      errs++;
      fprintf(stderr,
       "  mtio_fread data error: rec=%d, read=%s, should be %s\n", 
       i, line1, line);
    }
  }
  
  if( (i=mtio_fclose(fd)) != 0){
    errs++;
    fprintf(stderr,"  mtio_close error(%d) after read check\n",i);
  }
  
  /* check file size */
  if( (l=mtio_flsz(filename)) != 2*sizex) {
    fprintf(stderr,"  mtio_flsz error: got %"PRId64", expected %"PRId64"\n", l, 2*sizex);
    errs++;
  }

/* Testing fputs and fgets*/
testputs:  
  if( (nerrs+=errs) > 0) goto done;
  
  errs=0;
  strcpy(open_mode,"w+");
  strcat(open_mode,text_bin_mode);
  fprintf(stderr,"  Testing fputs/fgets, %s mode\n", open_mode);  
  if( (fd=mtio_fopen(filename,open_mode)) == NULL) {
    fprintf(stderr,"  Error opening file(%s)\n", filename);
    errs++;
    goto done;
  }

  /* write known data with fputs*/
  for(i=0; i<nrecs; i++){
    sprintf(line,format,i);
    if( (j=mtio_fputs(line, fd)) < 0) {
      errs++;
      fprintf(stderr,"  mtio_fputs error: status=%d, should be >= 0\n", j);
      mtio_fclose(fd);
      goto done;
    }
  }

  /* check ftell position */
  if( (l=mtio_ftell(fd)) != sizex) {
    fprintf(stderr,"  mtio_tell error after write: got %"PRId64", expected %"PRId64"\n", 
      l, sizex);
    errs++;
  }
  
  /* read and check data using fgets*/
  if( (i=mtio_fseek(fd,0,SEEK_SET)) != 0){
    fprintf(stderr,"  Error seeking file(%s) after write pos=0\n", filename);
    errs++;
  }
    
  for(i=0; i<nrecs; i++){
    sprintf(line,format,i);
    if( (cptr=mtio_fgets(line1, (size_t) 80, fd)) == NULL) {
      errs++;
      fprintf(stderr,"  mtio_fgets error, rec=%d\n",i);
    } else if(strcmp(line,line1) != 0){
      errs++;
      fprintf(stderr,
       "  mtio_fgets data error:read=%s, should be %s, rec=%d\n",line1,line,i);
    }
  }
  
  if( (i=mtio_fseek(fd,0,SEEK_SET)) != 0){
    fprintf(stderr,"  Error seeking file(%s) after write pos=0\n", filename);
    errs++;
  }
    
  /* test partial fgets reads*/
  for(i=0; i<nrecs; i++){
    /*read all but 1 character*/
    if( (cptr=mtio_fgets(line1, (size_t) recl, fd)) == NULL) {
      errs++;
      fprintf(stderr,"  mtio_fgets2 error, rec#=%d\n",i);
    } else if(strlen(line1) != (recl-1)) {
      errs++;
      fprintf(stderr,
        "  mtio_fgets2 length error: read=%d, expected=%d, rec#=%d\n",
        (int) strlen(line1),recl-1,i);
    }

    /* read the remaining 1 character*/
    if( (cptr=mtio_fgets(line, (size_t) 80, fd)) == NULL) {
      errs++;
      fprintf(stderr,"  mtio_fgets2 LF error, rec#=%d\n",i);
    } else if(strlen(line) != 1) {
      errs++;
      fprintf(stderr,
        "  mtio_fgets2 LF length error: read=%d, expected=%d, rec#=%d\n",
        (int) strlen(line),1,i);
    }
    
    strcat(line1,line);
    sprintf(line,format,i);
    if(strcmp(line,line1) != 0){
      errs++;
      fprintf(stderr,
       "  mtio_fgets2 data error: read=%s, should be %s\n, rec=%d", 
       line1, line,i);
    }
  }
  
  if( (i=mtio_fclose(fd)) !=0) {
    errs++;
    fprintf(stderr,"  mtio_close error(%d)\n",i);
  }
  
  /* check file size */
  if( (l=mtio_flsz(filename)) != sizex) {
    fprintf(stderr,"  mtio_flsz error: got %"PRId64", expected %"PRId64"\n", l, sizex);
    errs++;
  }

done:  
  nerrs+=errs;
  if(nerrs > 0){
    fprintf(stderr,"  Errors=%d in this instance of %s\n", nerrs, rtn);
  } else {
    fprintf(stderr,"  No errors found in this instance of %s\n", rtn);
  }

  remove(filename);
  return(nerrs);
}

/**************** tests simultaneous writes reads *****************/
int mtio_test2(int lineno,int state, int nthreads, int qsize,
                          int nrecs) {
  int i, j, l, errs;
  int nsize, nsteps, recl, ibeg, iend;
  char format[]="Line %5.5d\n";
  char line[80], line1[80];
  char filename[]="test.dat", filename1[]="test1.dat";
  FILE * fd, *fd1;
  char rtn[ ]="test2";
  int64_t flsz, flsz1;
  
  fprintf(stderr,
  "\nPerforming %s: mtio_status=%d, nthreads=%d, qsize=%d, nrec=%d, line#=%d\n",
    rtn, state, nthreads,qsize, nrecs, lineno);

  if(mtio_check_mtio_state(lineno, state, nthreads, qsize)!=0) return(1);
  
  errs=0;

  fprintf(stderr,"  Testing multiple read/write cycles\n");
  if( (fd=mtio_fopen(filename,"w+")) == NULL) {
    fprintf(stderr,"Error opening file(%s)\n", filename);
    errs++;
    goto done;
  }

  /** do several cycles of some write, read written data backwards*/
  nsize=0;
  recl=sprintf(line,format,0);
  for(nsteps=0; nsize<nrecs; nsteps++){nsize+=(nsteps+1);}
  for(i=0;i<nsteps; i++) {
    ibeg=i*(i+1)/2;
    iend=ibeg+i;
    mtio_fseek(fd,ibeg*recl,SEEK_SET);
    for (j=ibeg; j<=iend; j++) {
      sprintf(line,format,j);
      if( (l=mtio_fwrite(line, (size_t) 1, (size_t) recl, fd)) !=recl) {
        errs++;
        fprintf(stderr,"mtio_fwrite error: status=%d, should be %d\n", l, recl);
      }
    }
    for (j=iend; j>=ibeg; j--) {
      mtio_fseek(fd,j*recl,SEEK_SET);
      sprintf(line,format,j);
      if( (l=mtio_fread(line1, (size_t) 1, (size_t) recl, fd)) !=recl) {
        errs++;
        fprintf(stderr,"mtio_fread error, rec=%d: status=%d, should be %d\n", 
          j, l, recl);
      } else {
        line1[recl]='\0';
        if(strcmp(line,line1)!=0) {
          errs++;
          fprintf(stderr,
            "mtio_fread data error, rec=%d: read=%s, should be %s\n",
            j, line1, line);
        }
      }
    }
  }
  
  if( (i=mtio_fclose(fd)) != 0) {
    errs++;
    fprintf(stderr,"mtio_close error(%d)\n",i);
  }

  fprintf(stderr,"  Testing simultaneous multiple write/read cycles\n");

  if( (fd=mtio_fopen(filename,"w+")) == NULL) {
    fprintf(stderr,"Error opening file(%s)\n", filename);
    errs++;
    goto done;
  }

  if( (fd1=mtio_fopen(filename1,"w+")) == NULL) {
    fprintf(stderr,"Error opening file(%s)\n", filename1);
    errs++;
    goto done;
  }

  /** do several cycles of some write, read written data backwards*/
  nsize=0;
  recl=sprintf(line,format,0);
  for(nsteps=0; nsize<nrecs; nsteps++){nsize+=(nsteps+1);}
  for(i=0;i<nsteps; i++) {
    ibeg=i*(i+1)/2;
    iend=ibeg+i;
    mtio_fseek(fd,  ibeg*recl, SEEK_SET);
    mtio_fseek(fd1, ibeg*recl, SEEK_SET);

    for (j=ibeg; j<=iend; j++) {
      sprintf(line,format,j);
      
      l=mtio_fwrite(line, (size_t) 1, (size_t) recl, fd);
      if(l != recl) {
        errs++;
        fprintf(stderr,
          "mtio_fwrite error: status=%d, should be %d, rec#=%d\n", l, recl,j);
      }
      
      l=mtio_fwrite(line, (size_t) 1, (size_t) recl, fd1);
      if(l != recl) {
        errs++;
        fprintf(stderr,
          "mtio_fwrite error1: status=%d, should be %d, rec#=%d\n", l, recl,j);
      }
    }
    
    /* read data backwards from order written*/
    for (j=iend; j>=ibeg; j--) {
      mtio_fseek(fd,  j*recl, SEEK_SET);
      mtio_fseek(fd1, j*recl, SEEK_SET);
      sprintf(line,format,j);
      
      l=mtio_fread(line1, (size_t) 1, (size_t) recl, fd);
      if(l != recl) {
        errs++;
        fprintf(stderr,
          "mtio_fread error: status=%d, should be %d, rec#=%d\n", l, recl,j);
      } else {
        line1[recl]='\0';
        if(strcmp(line,line1)!=0) {
          errs++;
          fprintf(stderr,
            "mtio_fread data error: read=%s, should be %s, rec#=%d\n",
            line1, line,j);
        }
      }
      
      l=mtio_fread(line1, (size_t) 1, (size_t) recl, fd1);
      if(l != recl) {
        errs++;
        fprintf(stderr,
          "mtio_fread error1: status=%d, should be %d, rec#=%d\n", l, recl,j);
      } else {
        line1[recl]='\0';
        if(strcmp(line,line1)!=0) {
          errs++;
          fprintf(stderr,
            "mtio_fread data error1: read=%s, should be %s, rec#=%d\n",
            line1, line,j);
        }
      }
    }
  }
  
  if( (i=mtio_fclose(fd)) != 0) {
    errs++;
    fprintf(stderr,"mtio_close error(%d)\n",i);
  }

  if( (i=mtio_fclose(fd1)) != 0) {
    errs++;
    fprintf(stderr,"mtio_close error1(%d)\n",i);
  }

  fprintf(stderr,"  Testing file size with simultaneous IO\n");
  if( (fd=mtio_fopen(filename,"w+")) == NULL) {
    fprintf(stderr,"mtio_fopen error, file(%s)\n", filename);
    errs++;
    goto done;
  }

  if((i=mtio_fwrite("abc",1,3,fd))!=3) {
    fprintf(stderr,"mtio_fwrite error ,status=%d, should =3\n",i);  
    errs++;
    goto done;
  }
  mtio_fflush(fd);
  
  if( (fd1=fopen(filename,"a+")) == NULL) {
    fprintf(stderr,"fopen error, file(%s)\n", filename);
    errs++;
    goto done;
  }
  
  if((i=fwrite("abc",1,3,fd1))!=3) {
    fprintf(stderr,"fwrite_error ,status=%d, should =3\n",i);  
    errs++;
    goto done;
  }
  fflush(fd1); 
  flsz1=ftell(fd1);
  if( (i=fclose(fd1)) != 0) {
    errs++;
    fprintf(stderr,"fclose error(%d)\n",i);
    goto done;
  }

  flsz=mtio_flsz(filename);
  if(flsz!=flsz1) {
    fprintf(stderr," file size error, got=%"PRId64", should=%"PRId64"\n",flsz,flsz1);
    errs++;
    goto done;
  }


  if( (i=mtio_fclose(fd)) != 0) {
    errs++;
    fprintf(stderr,"mtio_fclose error(%d)\n",i);
    goto done;
  }

done:  
  if(errs > 0) {
    fprintf(stderr,"  Errors=%d with this instance of %s\n", errs, rtn);
  } else {
    fprintf(stderr,"  No errors found in this instance of %s\n", rtn);
  }
  
  remove(filename);
  remove(filename1);
  return(errs);
}

int mtio_test3(int nthreads_arg, int qsize_arg) {
  char filename[]="test.dat", *inq;
  FILE *fd;
  int i, flno, errs, nthreads, qsize;
  char rtn[]="test3";

  nthreads=nthreads_arg;
  qsize=qsize_arg;
  fprintf(stderr,
    "\nPerforming %s:nthreads=%d, qsize=%d\n", rtn, nthreads,qsize);
  errs=0;
 
  fprintf(stderr,
    "  Testing mtio_fileno/mtio_inquire & activating mtio with opened files\n");
  if( (fd=mtio_fopen(filename,"w+")) == NULL){
    fprintf(stderr,"mtio_fopen(%s) error\n",filename);
    errs++;
    goto done;
  }

  flno=mtio_fileno(fd);
  if( (inq=mtio_inquire(fd)) != NULL){
    fprintf(stderr,"mtio_inquire error, nonNULL returned when expected NULL\n");
    errs++;
    goto done;
  }

  mtio_init(&nthreads, &qsize);
  if( (i=mtio_fileno(fd)) != flno){
    fprintf(stderr,"mtio_fileno error: expected=%d, got %d\n",flno,i);
    errs++;
    goto done;
  }

  /* mtio_find file should have been executed so fd should now be in mtio*/
  if( (inq=mtio_inquire(fd)) == NULL) {
    fprintf(stderr,"mtio_inquire error: NULL returned when not expected\n");
    errs++;
    goto done;
  }

  if( (i=mtio_fclose(fd)) != 0) {
    fprintf(stderr,"mtio_fclose error=%d\n",i);
    errs++;
    goto done;
  }

  if( (fd=mtio_fopen(filename,"w+")) == NULL){
    fprintf(stderr,"mtio_fopen(%s) error\n",filename);
    errs++;
    goto done;
  }
  
  if( (inq=mtio_inquire(fd)) == NULL) {
    fprintf(stderr,"mtio_inquire error: NULL returned when not expected\n");
    errs++;
    goto done;
  } else if(strcmp(filename,inq) != 0) {
    fprintf(stderr,
      "mtio_inquire wrong result:got=%s, should=%s\n",inq,filename);
    errs++;
    goto done;
  }

  if( (i=mtio_fclose(fd)) != 0) {
    fprintf(stderr,"mtio_fclose error=%d\n",i);
    errs++;
    goto done;
  }


done:
  mtio_exit();
  
  if(errs > 0) {
    fprintf(stderr,"  Errors=%d with this instance of %s\n", errs, rtn);
  } else {
    fprintf(stderr,"  No errors found in this instance of %s\n", rtn);
  }
  
  remove(filename);
  return(errs);
}



void *mtio_test4(void *threadarg) {
  char filename[64], *inq;
  char str[132];
  FILE *fd=NULL;
  int i,j, flno;
  void * test_threadid;
  int32_t errs=(int32_t) &test4_errs[(int32_t) threadarg];
  
  char rtn[]="test4";
  if(mtio_exclusive("lock") != 0) {
    fprintf(stderr,
    "\n\t%s[%"PRId32"]:Failed to get exclusive mtio frontend lock\n",
     rtn, (int32_t) threadarg);
    errs++;
    goto done;
  }
  test_threadid = threadarg;
  fprintf(stderr,
    "\n\t%s[%"PRId32"]:nthreads=%d, qsize=%d threadarg=%"PRId32"\n",
     rtn, (int32_t) threadarg,nthreads_4,qsize_4,(int32_t) threadarg);
  errs=0;
  sprintf(filename,"test4data_%"PRId32".dat",(int32_t) test_threadid);
  fprintf(stderr,"\t%s[%"PRId32"]:\tFilename=%s\n",rtn,(int32_t) threadarg,filename);

 
  if((flno=mtio_fileno(fd)) != MTIO_ERROR) {
    fprintf(stderr,
    "\t%s[%"PRId32"]:Testing mtio_fileno/mtio_inquire. flno=%d\n",
    rtn,(int32_t) threadarg,flno);
    errs++;
    goto done;
  }

  if((flno=mtio_fclose(fd)) != MTIO_ERROR) {
    fprintf(stderr,
    "\t%s[%"PRId32"]:Testing mtio_fclose with bad fdescriptor. status=%d\n", 
    rtn,(int32_t) threadarg,flno);
    errs++;
    goto done;
  }

  if((inq=mtio_inquire(fd)) != NULL) {
    fprintf(stderr,
    "\t%s[%"PRId32"]:Testing mtio_inquire with bad fdescriptor. status=%s\n", 
    rtn,(int32_t) threadarg,inq);
    errs++;
    goto done;
  }

  if((i=mtio_find_ifile(fd)) != MTIO_ERROR) {
    fprintf(stderr,
    "\t%s[%"PRId32"]:Testing mtio_find_ifile with bad fdescriptor. status=%d\n", 
    rtn,(int32_t) threadarg,i);
    errs++;
    goto done;
  }


  if( (fd=mtio_fopen(filename,"w+")) == NULL){
    fprintf(stderr,"\t%s[%"PRId32"]:mtio_fopen(%s) error\n",
    rtn, (int32_t) threadarg,filename);
    errs++;
    goto done;
  }

  if( (i=mtio_fileno(fd)) == MTIO_ERROR){
    fprintf(stderr,"\t%s[%"PRId32"]:mtio_fileno error: \n",
    rtn, (int32_t) threadarg);
    errs++;
    goto done;
  }

  /* mtio_find file should have been executed so fd should now be in mtio*/
  if( (inq=mtio_inquire(fd)) == NULL) {
    fprintf(stderr,
    "\t%s[%"PRId32"]:mtio_inquire error: NULL returned when not expected\n",
    rtn,(int32_t) threadarg);
    errs++;
    goto done;
  }
  
  if( (i=mtio_fclose(fd)) != 0){
    fprintf(stderr,"\t%s[%"PRId32"]:mtio_fclose %s error\n",
    rtn,(int32_t) threadarg,filename);
    errs++;
    goto done;
  }

  for(i=0;i<10000;i++){
    if((fd=mtio_fopen(filename,"w")) == NULL) errs++;
    if(errs > 0 ) {
      fprintf(stderr,"\t%s[%"PRId32"]:mtio_fopen error i=%d\n",
      rtn,(int32_t) threadarg,i);
      goto done;
    }
    if((inq=mtio_inquire(fd)) == NULL) errs++;
    if(errs > 0 ) {
      fprintf(stderr,"\t%s[%"PRId32"]:mtio_inquire error i=%d\n",
      rtn,(int32_t)threadarg,i);
      goto done;
    }
    if((flno=mtio_fileno(fd)) < 0 ) errs++;
    if(errs > 0 ) {
      fprintf(stderr,"\t%s[%"PRId32"]:mtio_fileno error i=%d\n",
      rtn,(int32_t)threadarg,i);
      goto done;
    }
    if(mtio_fclose(fd) != 0) errs++;
    if(errs > 0 ) {
      fprintf(stderr,"\t%s[%"PRId32"]:mtio_fclose error i=%d\n",
      rtn,(int32_t)threadarg,i);
      goto done;
    }
  }


  if( (fd=mtio_fopen(filename,"w+")) == NULL){
    fprintf(stderr,"\t%s[%"PRId32"]:mtio_fopen(%s) error\n",
    rtn,(int32_t)threadarg,filename);
    errs++;
    goto done;
  }
  

  mtio_exclusive("unlock");

  if( (inq=mtio_inquire(fd)) == NULL) {
    fprintf(stderr,
    "\t%s[%"PRId32"]:mtio_inquire error: NULL returned when not expected\n",
    rtn,(int32_t) threadarg);
    errs++;
    goto done;
  } else if(strcmp(filename,inq) != 0) {
    fprintf(stderr,
      "\t%s[%"PRId32"]:mtio_inquire wrong result:got=%s, should=%s\n",
      rtn,(int32_t) threadarg,inq,filename);
    errs++;
    goto done;
  }


  for (i=0;i<=1000;i++){
    sprintf(str,"%5.5d:%s: This is a sample data line for tests.\n",i,filename);

    if((j=mtio_fwrite(str,strlen(str),1,fd)) != strlen(str) ){
      fprintf(stderr,
        "\t%s[%"PRId32"]:mtio_fwrite wrong result:got=%d, should=%d\n",
        rtn,(int32_t) threadarg,j,(int) strlen(str));

      errs++;
      goto done;
    }
  }

  for (i=0;i<=1000;i++) {
    if((j=mtio_fflush(fd)) != 0 ) {
      fprintf(stderr,
       "\t%s[%"PRId32"]:mtio_fflush incorrect return variable = %d\n",
        rtn,(int32_t) threadarg,j);
      errs++;
      goto done;
    }
  }

  mtio_exclusive("lock");

  if( (i=mtio_fclose(fd)) != 0) {
    fprintf(stderr,"\t%s[%"PRId32"]:mtio_fclose error=%d\n",rtn,
      (int32_t) threadarg,i);
    errs++;
    goto done;
  }


done:
  /*** This is a good line for testing error return variables 
       *errs +=  (int32_t) threadarg;
  ***/
  
  if(errs > 0) {
    fprintf(stderr,"\t%s[%"PRId32"]:Errors=%"PRId32" with this instance of %s\n",
    rtn,(int32_t) threadarg, errs, rtn);
  } else {
    fprintf(stderr,"\t%s[%"PRId32"]:No errors found in this instance of %s\n",
    rtn,(int32_t) threadarg,rtn);
  }

  if(mtio_exclusive("unlock") != 0) {
    fprintf(stderr,
    "\n\t%s[%"PRId32"]:Failed to return exclusive mtio frontend lock\n",
     rtn, (int32_t) threadarg);
    errs++;
    goto done;
  }

  if (errs == 0 ) remove(filename);

  test4_errs[(int32_t) threadarg] = errs;
  /*sleep(3);*/
  return (void *) &test4_errs[(int32_t) threadarg]; 
}


/*********** MAIN DRIVER FOR TESTS ********************/
int main(int argc, char** argv) {
  int i,errs, errs4=0, nerrs, nrecs, nthreads, qsize, ithread;
  int nfgthreads;
  static pthread_t            **thread=NULL;
  int32_t threadnums[5]; /*holds thread numbers */
  void * thread_status;
  nthreads=5;
  if(argc > 1) nthreads=atoi(argv[1]);
  qsize=2*nthreads;
  if(argc > 2) qsize   =atoi(argv[2]);
  nrecs=10*qsize;
  if(argc > 3) nrecs   =atoi(argv[3]);

  nthreads_4 = nthreads;
  qsize_4    = qsize;
  fprintf(stderr,"Running mtioUnitTest: #threads=%d, qsize=%d, nrecs=%d\n",
         nthreads, qsize, nrecs);
  
  mtio_set_print_stats(1);
  mtio_set_debug_mode(0); 
  
  nerrs=0;
  errs=mtio_test0(__LINE__, MTIO_OFF, 0, 0, nrecs);
  mtio_init(&nthreads, &qsize);
  errs+=mtio_test0(__LINE__, MTIO_ON, nthreads,qsize,nrecs);
  mtio_exit();
  errs+=mtio_check_mtio_state(__LINE__, MTIO_OFF,0,0);
  
  mtio_initialized=3;  /* test no turn on mtio debug mode */
  mtio_init(&nthreads, &qsize);
  errs+=mtio_test0(__LINE__, MTIO_OFF, 0, 0, nrecs);
  mtio_exit();
  if(mtio_initialized!=3) {
    fprintf(stderr,
      "  Expected mtio_initialized=3, but was %d\n",mtio_initialized);
    errs++;
  }    
  mtio_initialized=0;  /*now set mtio in off mode but can be turned on*/
  
  
  if( (nerrs += errs) > 0) {
    fprintf(stderr,"Errors(%d) found with test0\n",errs);
    goto done; 
  } else {
    fprintf(stderr,"No errors found with test0\n");
  }
  
  errs=mtio_test1(__LINE__, MTIO_OFF, 0, 0, nrecs, "");
  errs=mtio_test1(__LINE__, MTIO_OFF, 0, 0, nrecs, "b");
  mtio_init(&nthreads, &qsize);
  errs+=mtio_test1(__LINE__, MTIO_ON, nthreads,qsize,nrecs,"");
  errs+=mtio_test1(__LINE__, MTIO_ON, nthreads,qsize,nrecs,"b");
  mtio_exit();
  errs+=mtio_check_mtio_state(__LINE__, MTIO_OFF,0,0);
  
  if( (nerrs += errs) > 0) {
    fprintf(stderr,"Errors(%d) found with test1\n",errs);
    goto done; 
  } else {
    fprintf(stderr,"No errors found with test1\n");
  }
  
  errs=mtio_test2(__LINE__, MTIO_OFF, 0, 0, nrecs);
  mtio_init(&nthreads, &qsize);
  errs+=mtio_test2(__LINE__, MTIO_ON, nthreads,qsize,nrecs);
  mtio_exit();
  errs+=mtio_check_mtio_state(__LINE__, MTIO_OFF,0,0);
  
  if( (nerrs += errs) > 0) {
    fprintf(stderr,"Errors(%d) found with test2\n",errs);
    goto done; 
  } else {
    fprintf(stderr,"No errors found with test2\n");
  }

  errs=mtio_test3(nthreads, qsize);
  if( (nerrs += errs) > 0) {
    fprintf(stderr,"Errors(%d) found with test3\n",errs);
    goto done; 
  } else {
    fprintf(stderr,"No errors found with test3\n");
  }
  
  nfgthreads=5;
  if(nfgthreads > 5 ) {
    fprintf(stderr,"mtio_test4:  This version does not support more than");
    fprintf(stderr," one foreground thread.\n");
    return -1;
  }
  /*** set up the threads ***/
  if(thread != NULL) free(thread);
  thread = (pthread_t **) malloc (nfgthreads * sizeof(pthread_t *));
  if(thread == NULL) {
    fprintf(stderr, "main: Unable to malloc thread with size=%d\n",
      mtio_numthreads);
    goto done;
  }
  /*** here we need to put test4 common code, and let the threads do 
       all the rest
  ***/

  mtio_init(&nthreads_4,&qsize_4);

  /*** end of common test4 code ***/

  for(ithread=0;ithread<nfgthreads;ithread++){
    fprintf(stderr,"main: starting thread %d",ithread);
    threadnums[ithread]=ithread ; /* store errors here */
    thread[ithread] = (pthread_t *) malloc (sizeof (pthread_t));
    fprintf(stderr," threadnums[%d]=%p=>%"PRId32" thread[%d]=%p.\n",
    ithread,&threadnums[ithread],threadnums[ithread],ithread,thread[ithread]);
    if(thread[ithread] == NULL) {
      fprintf(stderr, "main: Unable to malloc thread[%d]\n", 
        ithread);
      goto done;
    }
    
    if( (i=pthread_create((pthread_t *) thread[ithread],
                   (pthread_attr_t *) &threadattr, (*mtio_test4),
                    (void*) threadnums[ithread])) != 0) {
      fprintf(stderr, "main: error(%d) in pthread_create\n", i);
      goto done;
    }
    
    if(debug_mode>0) 
      fprintf(stderr,"main: started thread %d %p\n", 
      ithread, thread[ithread]);        
  }

  for(ithread=0;ithread<nfgthreads;ithread++){
    if(thread[ithread] != NULL ) { 
      if(pthread_join(*thread[ithread], &thread_status) != 0){
          fprintf(stderr,
            "main: Warning: thread num %d (id=%p) not joined.\n",
            ithread, thread[ithread]);
          errs=1;
      } else {
            threadnums[ithread] = *(int32_t *) thread_status;
            errs=(int) threadnums[ithread];
            fprintf(stderr,"main: stopped thread %d errs=%d\n",
            ithread, (int) threadnums[ithread]);
          free(thread[ithread]);
          thread[ithread]=NULL;
      }
      errs4 += errs;
    }
  }
  /*** more common code for test4 ***/

  mtio_exit();

  /*** end of common code         ***/

  if( (nerrs += errs4) > 0) {
    fprintf(stderr,"Errors (%d) found with test4\n",errs4);
    goto done; 
  } else {
    fprintf(stderr,"No errors found with test4\n");
  }
  
done:
  if(nerrs > 0) {
    fprintf(stderr,"\nErrors found in mtioUnitTest = %d.\n",nerrs);
  } else {
    fprintf(stderr,"\nNo errors found with mtioUnitTest.\n");
  }
  return (0);
}
  
/****************************** end of mtio unit test ************************/
  
#endif
