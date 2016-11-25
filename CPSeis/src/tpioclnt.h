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
! Name       : tpioclnt.h
! Category   : io
! Written    : 2000-08-31   by: R.S.Day
! Revised    : 2005-09-12   by: Tom Stoeckley
! Maturity   : production
! Purpose    : header for tpioclnt.c client side tape function
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
!  7. 2005-09-12  Stoeckley    Fix to compile with C++.
!  6. 2003-09-29  R.S.Day      Changed tpioclnt_open_base tpioclnt_close
!                              prototypes.
!  5. 2003-07-30  R.S.Day      Added new function and changed some old
!                              functions
!  4. 2003-03-13  R.S.Day      Added  tpioclnt_asread & tpioclnt_aswrite.
!  3. 2001-01-30  R.S.Day      Added port argument to tpioclnt_get_host_fd
!  2. 2000-11-08  R.S.Day      Added prototype for tpioclnt_finalize
!  1. 2000-08-31  R.S.Day      Initial version.
!
!-------------------------------------------------------------------------------
!</history_doc>
**/
#ifndef _TPIOCLNT
#define _TPIOCLNT

#include "c2f_interface.h"

#ifdef NEED_CAPITALS
#define tpioclnt_open  TPIOCLNT_OPEN
#define tpioclnt_open_server TPIOCLNT_OPEN_SERVER
#define tpioclnt_get_server_n TPIOCLNT_GET_SERVER_N
#define tpioclnt_get_servers TPIOCLNT_GET_SERVERS
#define tpioclnt_close TPIOCLNT_CLOSE
#define tpioclnt_read  TPIOCLNT_READ
#define tpioclnt_asread  TPIOCLNT_ASREAD
#define tpioclnt_write TPIOCLNT_WRITE
#define tpioclnt_aswrite TPIOCLNT_ASWRITE
#define tpioclnt_rew   TPIOCLNT_REW
#define tpioclnt_prnt  TPIOCLNT_PRNT
#define tpioclnt_weof  TPIOCLNT_WEOF
#define tpioclnt_peof  TPIOCLNT_PEOF
#define tpioclnt_gvol  TPIOCLNT_GVOL
#define tpioclnt_space TPIOCLNT_SPACE
#define tpioclnt_info  TPIOCLNT_INFO
#endif
#ifdef NEED_UNDERSCORE
#define tpioclnt_open  tpioclnt_open_
#define tpioclnt_open_server tpioclnt_open_server_
#define tpioclnt_get_server_n tpioclnt_get_server_n_
#define tpioclnt_get_servers tpioclnt_get_servers_
#define tpioclnt_close tpioclnt_close_
#define tpioclnt_read  tpioclnt_read_
#define tpioclnt_asread  tpioclnt_asread_
#define tpioclnt_write tpioclnt_write_
#define tpioclnt_aswrite tpioclnt_aswrite_
#define tpioclnt_rew   tpioclnt_rew_
#define tpioclnt_prnt  tpioclnt_prnt_
#define tpioclnt_weof  tpioclnt_weof_
#define tpioclnt_peof  tpioclnt_peof_
#define tpioclnt_gvol  tpioclnt_gvol_
#define tpioclnt_space tpioclnt_space_
#define tpioclnt_info  tpioclnt_info_
#endif

#ifdef __cplusplus
extern "C" {
#endif

/* public access functions, available to fortran */
int  tpioclnt_open (char*hostvol, char *mode, char *dvice);
int  tpioclnt_open_server(char *in_volser, char *rwu, char *dvice,
     char *server, char *tapehost, long *label, char *lname);
int  tpioclnt_close(int *fh, char *lname, char *volser);
long tpioclnt_read (int *fh, char *buf, long *nbytes);
long tpioclnt_write(int *fh, char *buf, long *nbytes);
long tpioclnt_asread(int *fh, char *buff,long *nbytes, int *next);
long tpioclnt_aswrite(int *fh, char *buf, long *nbytes,
     int *nowait, long *didwr);
void tpioclnt_finalize();
int  tpioclnt_space(int *fh, long *nrecs);
int  tpioclnt_weof (int *fh, long *neof);
int  tpioclnt_peof (int *fh);
int  tpioclnt_prnt (int *fh);
int  tpioclnt_rew  (int *fh);
int  tpioclnt_gvol (int *fh, char *volser);
void tpioclnt_info(int *ifile, int *tag);

/* private functions used internally by tpioclnt */
void tpioclnt_set_remote_access(int isw);
int  tpioclnt_decompose_filename(char *fn, char *hostn, char* base_filename);
int  tpioclnt_get_host_fd(char *hostn, short portno);
int  tpioclnt_get_fileno();
void tpioclnt_info(int *ifile, int *tag);
int  tpioclnt_check_file_no(int ifile, char *rtn_name);
void tpioclnt_segv(int sig);
void tpioclnt_sig(int sig);
int tpioclnt_get_server_n(char *server, int *n);
int tpioclnt_get_servers(char *servers);

#ifdef __cplusplus
}
#endif

#endif
