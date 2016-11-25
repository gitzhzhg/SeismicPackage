/****
!<CPS_v1 type="HEADER_FILE"/>
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
! Name       : tapeio.h
! Category   : io
! Written    : 2000-07-24   by: R.S.Day
! Revised    : 2003-07-30   by: R.S.Day
! Maturity   : production   2003-09-29
! Purpose    : header for tapeio.c. low level tape functions
! Portability: Unix only
!
!-------------------------------------------------------------------------------
!</brief_doc>
 
 
!<history_doc>
!-------------------------------------------------------------------------------
!                       HEADER FILE REVISION HISTORY
!
!     Date        Author       Description
!     ----        ------       -----------
!  8. 2003-09-29  R.S.Day      user argument added to tapeio_open_base.
!  6. 2003-02-27  R.S.Day      argument added to tapeio_tapelib and 
!                              tapeio_load_open. Added prototype for
!                              tapeio_open_lab
!  5. 2000-10-20  R.S.Day      tapeio_tapelib,tapeio_load_open prototype change
!  4. 2000-09-28  R.S.Day      Added prototype for tapeio_verbose
!  3. 2000-08-21  R.S.Day      Added prototype for tapeio_tell, and increased
!                              io buffer size.
!  2. 2000-08-01  R.S.Day      Converted file descriptor in arguments to
!                              long to be consistent with pfio. Added volser
!                              argument to tapeio_open and removed from
!                              tapeio_dmnt. Added volser to tapeio_dat and
!                              removed path from this data structure. Increased
!                              the buffer size and eliminated bsize variable.
!  1. 2000-07-24  R.S.Day      Initial version.
!
!-------------------------------------------------------------------------------
!</history_doc>
****/

#ifndef _TAPEIO_
#define _TAPEIO_

#include "c2f_interface.h"
/*
 * Supporting functions for CPS tape IO
 */

typedef struct _tapeio_dat {
 char devnam[64];     /* device name    */
 char volser[8];      /* 6 byte external label */
 long  fd;            /* file descriptor */
 int  lnl;            /* lnl>0 for labeled tape */
 char label[3][88];   /* internal tape labels     */
 char type[16];       /* tape type, e.g. SEGY */
 char pdn[48];        /* tape name in catalogue */
 long opmode;
 int  nrd;
 char buf[120000];    /* char buf[65536]; */
 int  datafile;       /* file where trace data starts*/
 int  datablk;        /* record where trace data starts*/
 int  fileno;         /* current file position */
 int  blkno;          /* current record position in file*/
 long trcnt;          /* total traces in all files on tape */
 } tapeio_dat;


#ifdef NEED_CAPITALS
#define tapeio_load_open TAPEIO_LOAD_OPEN
#define tapeio_tapelib  TAPEIO_TAPELIB
#define tapeio_open     TAPEIO_OPEN
#define tapeio_open_lab TAPEIO_OPEN_LAB
#define tapeio_open_base TAPEIO_OPEN_BASE
#define tapeio_close    TAPEIO_CLOSE
#define tapeio_read     TAPEIO_READ
#define tapeio_write    TAPEIO_WRITE
#define tapeio_print    TAPEIO_PRINT
#define tapeio_status   TAPEIO_STATUS
#define tapeio_tell     TAPEIO_TELL
#define tapeio_rewind   TAPEIO_REWIND
#define tapeio_weof     TAPEIO_WEOF
#define tapeio_dmnt     TAPEIO_DMNT
#define tapeio_next     TAPEIO_NEXT
#define tapeio_get_vol  TAPEIO_GET_VOL
#define tapeio_set_pdn  TAPEIO_SET_PDN
#define tapeio_get_type TAPEIO_GET_TYPE
#define tapeio_set_type TAPEIO_SET_TYPE
#define tapeio_wr_label TAPEIO_WR_LABEL
#define tapeio_skip_blk TAPEIO_SKIP_BLK
#define tapeio_skip_mark TAPEIO_SKIP_MARK
#define tapeio_process_eofs TAPEIO_PROCESS_EOFS
#define tapeio_setverbose TAPEIO_SETVERBOSE
#endif
#ifdef NEED_UNDERSCORE
#define tapeio_load_open tapeio_load_open_
#define tapeio_tapelib   tapeio_tapelib_
#define tapeio_open      tapeio_open_
#define tapeio_open_lab  tapeio_open_lab_
#define tapeio_open_base tapeio_open_base_
#define tapeio_close     tapeio_close_
#define tapeio_read     tapeio_read_
#define tapeio_write    tapeio_write_
#define tapeio_print    tapeio_print_
#define tapeio_status   tapeio_status_
#define tapeio_tell     tapeio_tell_
#define tapeio_rewind   tapeio_rewind_
#define tapeio_weof     tapeio_weof_
#define tapeio_dmnt     tapeio_dmnt_
#define tapeio_next     tapeio_next_
#define tapeio_get_vol  tapeio_get_vol_
#define tapeio_set_pdn  tapeio_set_pdn_
#define tapeio_get_type tapeio_get_type_
#define tapeio_set_type tapeio_set_type_
#define tapeio_wr_label tapeio_wr_label_
#define tapeio_skip_blk tapeio_skip_blk_
#define tapeio_skip_mark tapeio_skip_mark_
#define tapeio_process_eofs tapeio_process_eofs_
#define tapeio_setverbose tapeio_setverbose_
#endif


#ifdef __cplusplus
extern "C" {                 // for C++
#endif

long  tapeio_load_open (char *tapelib, char *volser, char *mode, char *devtyp,
      char *user);
long  tapeio_tapelib(char *tapelib,
       char *volser,char *op,char *devtyp, char *pname,
      char *user);
long  tapeio_open (char *volser, char *devnam, char *mode);
long  tapeio_open_lab(char *volser, char *devnam,
      char *mode, int *lab_tape);
long  tapeio_open_base(char *volser, char *devnam,
      char *mode, int *lab_tape, char *user);
long  tapeio_close(long *fd);
long  tapeio_read (long *fd, long *nby, char *buf);
long  tapeio_write(long *fd, long *nby, char *buf);

long  tapeio_tell(long *fd, int *fileno, int * blkno);
void  tapeio_setverbose(int *verbose);
void  tapeio_print(long *fd);
void  tapeio_sprint(long *fd, char *obuf);
long  tapeio_status(long *fd, int *fileno, int *blkno);
long  tapeio_rewind(long *fd);
long  tapeio_dmnt(long *fd);
long  tapeio_next(long *fd);
long  tapeio_rd_label(long fd);
long  tapeio_wr_label(long *fd,char *vol, char *userid, char *pdn);
long  tapeio_rdorwr(long *fd);
void  tapeio_get_vol(long *fd, char *vol);
void  tapeio_set_vol(long *fd, char *vol);
void  tapeio_set_type(long *fd, char *type);
void  tapeio_get_type(long *fd, char *type);
void  tapeio_set_pdn(long *fd, char *pdn);
void  tapeio_get_pdn(long *fd, char *pdn);
long  tapeio_process_eofs(long *fd);
long  tapeio_weof(long *fd, long *count);
long  tapeio_skip_mark(long *fd, long count);
long  tapeio_skip_blk(long *fd, long count);
tapeio_dat *tapeio_dat_get(long fd);

#ifdef __cplusplus
}                   // for C++
#endif

#endif

