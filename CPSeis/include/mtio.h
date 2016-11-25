/*<CPS_v1 type="HEADER_FILE",pretag="!"/>
!------------------------------- mtio.h ----------------------------------
!------------------------------- mtio.h ----------------------------------
!------------------------------- mtio.h ----------------------------------
! other files are:  mtio.c
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
! Name       : mtio
! Category   : io
! Written    : 2005-02-08   by: Bill Menger
! Revised    : 2007-03-27   by: Kruger Corn
! Maturity   : beta
! Purpose    : Asynchronous output using multiple threads.
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
!  6. 2007-03-27  Kruger Corn  Updated to 64 bit architecture. Basically
!                              changed long to int32_t and long long to
!                              int64_t.
!  5. 2006-06-12  Bill Menger  Added mtio_exclusive function.
!  4. 2005-12-05  Chuck Burch  Added getqsize & flsz/no-r/w flags to mtio_file_t
!  3. 2005-07-26  Bill Menger  modified struct for mtio_thread_t, added the
!                              mtio_errmsg call.
!  2. 2005-04-26  Bill Menger  Added 100ms wait time for cond-waits
!  1. 2005-02-08  Bill Menger  Initial version.
!
!-------------------------------------------------------------------------------
!</history_doc>

!--------------------------- start of coding ------------------------------
!--------------------------- start of coding ------------------------------
!--------------------------- start of coding ------------------------------
*/

#ifndef _MTIO_H_
#define _MTIO_H_
/*** Add this to force CYGWIN fix, since Linux seems to share the bug ***/
/*      #define CYGWIN */
/************************** following added for threaded writes ************/

#define MTIO_OFF              0
#define MTIO_ON               1
#define MTIO_NUMTHREADS       1
#define MTIO_ERROR           -1
#define MTIO_SIGNAL_OK        0
#define MTIO_SIGNAL_ERROR     1
#define MTIO_SIGNAL_CLOSE     2
#define MTIO_INIT_NUMFILES   16
#define MTIO_WAIT_TIME   100000
#define MTIO_INNER_MAXVAL  3000

#ifdef NOFSEEKO

#define mtio_internal_fseek             fseek
#define mtio_off_t                      int32_t
#define mtio_internal_ftell             ftell

#else

#ifndef _LARGEFILE_SOURCE
#define _LARGEFILE_SOURCE
#endif

#ifndef _FILE_OFFSET_BITS
#define _FILE_OFFSET_BITS               64
#endif

#define mtio_internal_fseek             fseeko
#define mtio_off_t                      off_t
#define mtio_internal_ftell             ftello

#endif

#define mtio_clearerr                   clearerr
#define mtio_feof                       feof
#define mtio_ferror                     ferror
#define mtio_fcntl                      fcntl

#include <string.h>
#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>
#include <fcntl.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <pthread.h>
#include <malloc.h>
#include <signal.h>
#include "cb.h"

#include "upgrade264.h"

#ifdef __cplusplus
extern "C" {
#endif


/*------------------------ start of prototypes ---------------------------*/
/*------------------------ start of prototypes ---------------------------*/
/*------------------------ start of prototypes ---------------------------*/

typedef struct {
  int64_t fileposition;                /*  byte position from start of file */
  int32_t nbytes;                      /*  number of bytes to write */
  char *buff;                          /*  pointer to data area to be written*/
  int ifile;                           /*  file pointer */
} mtio_data_t;

typedef struct {
  FILE                *fd;         /*** actual file description from op sys. */
  pthread_mutex_t      lock;       /*** our personal lock on the file      ***/
  pthread_cond_t       cond;       /*** our personal condition var         ***/
  int                  num_pending_writes; /*** how many pending writes ?  ***/
  int                  signal;     /*** status and pass information        ***/
  int                  descriptor; /*** from fileno command                ***/
  mtio_off_t           curpos;     /*** where the cursor is in the file    ***/
  mtio_off_t           flsz;       /*** file size all IO happened          ***/
  int                  no_reads;   /*** 1 means file can not be read       ***/
  int                  no_writes;  /*** 1 means file can not be written to ***/
  int                  need_seek;  /*** 1 means perform seek before read   ***/
  char                *path;       /*** file name that was opened          ***/
} mtio_file_t;

typedef struct {
  pthread_t            threadid;
  int64_t              number_written;
  int32_t              depth_used;
  int32_t              depth;
  int64_t              avg_data_size;
  int32_t              avg_depth_used;
  int                  prv;
  int                  internal_error;
} mtio_thread_t;       /*** for instrumentation only ***/

/*** for fortran callability, the following ifdefs are added ***/
/*
#ifdef NEED_UNDERSCORE

#define mtio_init mtio_init_
#define mtio_status mtio_status_
#define mtio_exit mtio_exit_
#define mtio_errmsg mtio_errmsg_
#define mtio_fopen mtio_fopen_
#define mtio_fclose mtio_fclose_
#define mtio_fread mtio_fread_
#define mtio_fgets mtio_fgets_
#define mtio_fputs mtio_fputs_
#define mtio_fwrite mtio_fwrite_
#define mtio_fflush mtio_fflush_
#define mtio_fseek mtio_fseek_
#define mtio_ftell mtio_ftell_
#define mtio_flsz ftio_flsz_
#define mtio_writebehind mtio_writebehind_
#define mtio_getnumthreads mtio_getnumthreads_
#define mtio_getqsize mtio_getqsize_
#define mtio_find_ifile mtio_find_ifile_
#define mtio_find_fd mtio_find_fd_
#define mtio_fileno mtio_fileno_
#define mtio_inquire mtio_inquire_
#define mtio_internal_clean_fd mtio_internal_clean_fd_
#define mtio_set_print_stats mtio_set_print_stats_
#define mtio_set_debug_mode mtio_set_debug_mode_
#define mtio_numqueued mtio_numqueued_
#define mtio_exlusive mtio_exclusive_

#endif
#ifdef NEED_CAPITALS

#define mtio_init MTIO_INIT
#define mtio_status MTIO_STATUS
#define mtio_exit MTIO_EXIT
#define mtio_errmsg MTIO_ERRMSG
#define mtio_fopen MTIO_FOPEN
#define mtio_fclose MTIO_FCLOSE
#define mtio_fread MTIO_FREAD
#define mtio_fgets MTIO_FGETS
#define mtio_fputs MTIO_FPUTS
#define mtio_fwrite MTIO_FWRITE
#define mtio_fflush MTIO_FFLUSH
#define mtio_fseek MTIO_FSEEK
#define mtio_ftell MTIO_FTELL
#define mtio_flsz FTIO_FLSZ
#define mtio_writebehind MTIO_WRITEBEHIND
#define mtio_getnumthreads MTIO_GETNUMTHREADS
#define mtio_getqsize MTIO_GETQSIZE
#define mtio_find_ifile MTIO_FIND_IFILE
#define mtio_find_fd MTIO_FIND_FD
#define mtio_fileno MTIO_FILENO
#define mtio_inquire MTIO_INQUIRE
#define mtio_internal_clean_fd MTIO_INTERNAL_CLEAN_FD
#define mtio_set_print_stats MTIO_SET_PRINT_STATS
#define mtio_set_debug_mode MTIO_SET_DEBUG_MODE
#define mtio_numqueued MTIO_NUMQUEUED
#define mtio_exlusive MTIO_EXCLUSIVE

#endif
*/
int    mtio_init(int *mtio_numthreads, int *mtio_qsize);
int    mtio_status();
void   mtio_exit();
void   mtio_errmsg(char * , int *status ) ;
FILE  *mtio_fopen(const char *path, const char *mode);
int    mtio_fclose(FILE *stream);
size_t mtio_fread(void *ptr, size_t size, size_t nmemb, FILE *stream);
char * mtio_fgets(char *s, int size, FILE *stream);
int    mtio_fputs(char *s, FILE *stream);
size_t mtio_fwrite(const void *ptr, size_t size, size_t nmemb, FILE *stream);
int    mtio_fflush(FILE *stream);
int    mtio_fseek(FILE *stream, mtio_off_t offset, int whence);
mtio_off_t mtio_ftell(FILE *stream);
int64_t  mtio_flsz(char * path);
void   *mtio_writebehind(void * ithread);
int    mtio_getnumthreads();
int    mtio_getqsize();
int    mtio_find_ifile(FILE *fd);
FILE * mtio_find_fd(char *path);
int    mtio_fileno(FILE *fd);
char * mtio_inquire(FILE *fd);
void   mtio_internal_clean_fd(int ifile);
void   mtio_set_print_stats(int isw);
void   mtio_set_debug_mode(int isw);
int    mtio_numqueued();
int    mtio_exlusive(char *);

/****************************************************************************/

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
