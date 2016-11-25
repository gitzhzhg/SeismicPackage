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
/*<CPS_v1 type="HEADER_FILE"/>
!------------------------------- bfio.h ----------------------------------
!------------------------------- bfio.h ----------------------------------
!------------------------------- bfio.h ----------------------------------
!other files are:                bfio.c

!<brief_doc>
!-------------------------------------------------------------------------------
! Name       : bfio.h
! Category   : io
! Written    : 1999-09-15   by: Charles C. Burch
! Revised    : 2009-01-27   by: Bill Menger
! Maturity   : beta
! Purpose    : Provides a method of "basic file i/o".
! References : These routines are called from within pfio.c
!-------------------------------------------------------------------------------
!</brief_doc>

!<history_doc>
!-------------------------------------------------------------------------------
!                       HEADER FILE REVISION HISTORY
!     Date        Author          Description
!     ----        ------          -----------
! 18. 2009-01-27  Bill Menger     Added bfio_chmod
! 17. 2007-03-27  Kruger Corn     Updated to 64 bit architecture. Bassically
!                                 changed long to int32_t and long long to
!                                 int64_t.
! 16. 2006-11-14  Bill Menger     Added bfio_readlink_to_end (from Kruger Corn)
!                                 and bfio_delete_link_chain.
! 15. 2005-12-05  Burch/Macy      Added add_desc_flags, is_file_nfs & get_fd
! 14. 2005-09-01  Bill Menger     Replaced statfs with statvfs call for linux.
! 13. 2004-01-21  Chuck C. Burch  Added bfio_dir and bfio_seek.
! 12. 2003-05-27  Chuck C. Burch  Added remount_file_disk, get_file_time,
!                                 delete_empty_directories, get_file_owner_name,
!                                 check_if_directory_empty, get_file_num_lines,
!                                 check_if_directory, expand_file_name,
!                                 get_base_filename, get_str, form_tmpnam, and 
!                                 lock file functions.
!                                 Modifications for extents >2Gb.
! 11. 2002-07-23  Chuck C. Burch  Added bfio_get_disk_space
! 10. 2002-06-18  Chuck C. Burch  Added bfio_gets
!  9. 2002-03-27  Chuck C. Burch  Added bfio_remove_file
!  8. 2001-12-21  Chuck C. Burch  Added bfio_exit
!  7. 2001-06-27  Chuck C. Burch  Added use of c2f_interface.h.
!  6. 2001-03-21  Chuck C. Burch  Adding file region lock entries.
!  5. 2001-02-13  Chuck C. Burch  Added new functions.
!  4. 2000-10-20  Chuck C. Burch  restructured code.
!  3. 2000-08-31  Chuck C. Burch  Modified for better socket i/o.
!  2. 2000-05-11  William Menger  Added bfio_extsize function.
!  1. 2000-04-10  Chuck C. Burch  Initial version.
!-------------------------------------------------------------------------------
!</history_doc>
*/

/*--------------------------- start of coding ------------------------------*/
/*--------------------------- start of coding ------------------------------*/
/*--------------------------- start of coding ------------------------------*/

#ifndef _BFIO_H_
#define _BFIO_H_

#include "c2f_interface.h"
#include "upgrade264.h" /* get _LARGEFILE_SOURCE */

/*BF_FLSZ is biggest file size allowed-2Tb or slightly less 2Gb*/
#ifdef NOFSEEKO
#define bfio_fseek             fseek
#define bfio_ftell             ftell
#define BF_FLSZ ( (int64_t) 2147482624)

#else
/* #define _LARGEFILE_SOURCE redundant with upgrade264.h */
#define _FILE_OFFSET_BITS 64
#define bfio_fseek             fseeko
#define bfio_ftell             ftello
#define BF_FLSZ (((int64_t)2000000)*1000000)
#endif

#include <sys/statvfs.h>
#define bfio_statfs_struct     statvfs
#define bfio_statfs            statvfs

#include <stdio.h>
#include <time.h>

#ifdef NEED_UNDERSCORE
#endif

#ifdef NEED_CAPITALS
#endif

#ifdef __cplusplus
extern "C" {
#endif

/*char *bfio_h_ident =*/
/*"$Id: bfio.h,v 1.17 2007/03/28 15:09:43 Corn beta sps $";*/


/*------------------------ start of prototypes ---------------------------*/
/*------------------------ start of prototypes ---------------------------*/
/*------------------------ start of prototypes ---------------------------*/

/****************************** bfio.h ********************
*      Include file for bfio routines                     *
*                                                         *
*      Written June 1999 by Charles C Burch               *
**********************************************************/

struct bfio_lock_struct {
  int  fd;
  char *buff;
  int  version;
  int  lrecl;
  char file_name[260];
  char lock_file[260];
}; 

int       bfio_add_desc_flags(int, int32_t);
int       bfio_check_if_directory(char*);
int       bfio_check_if_directory_empty(char*);
int       bfio_close(int);
int       bfio_delete_empty_directories(char*, int);
int       bfio_delete_link_chain(char*);
void      bfio_dir(char*, char*);
int       bfio_ensure_dir_exists(char*, int);
int       bfio_expand_file_name(char*, char*, int); 
int       bfio_exit();
int32_t   bfio_fetch(int, int64_t, int32_t);
int64_t  bfio_file_size(char*);
int       bfio_flush(int);
int       bfio_force_file_max_size(int, int64_t);
char*     bfio_form_tmpnam(char*);
char*     bfio_get_base_filename(char*);
int64_t   bfio_get_disk_space(char*);
FILE*     bfio_get_fd(int);
int       bfio_get_file_num_lines(char*); 
int       bfio_get_file_owner_name(char*, char*, int);
time_t    bfio_get_file_time(char*); 
int       bfio_get_str(FILE*, char*, int);
int32_t   bfio_gets(int, int64_t, char*, int32_t);
int       bfio_is_file_nfs(char*);
int       bfio_open(char*, char);
int32_t   bfio_read(int, int64_t, char*, int32_t);
int       bfio_readlink_to_end(const char*, char*, size_t);
int64_t   bfio_remount_file_disk(char*);
int       bfio_remove_file(char*);
int       bfio_rwx_to_chmod(char*);
int       bfio_chmod(int, int);
int       bfio_set_file_region_lock(int,int);
int       bfio_setbufsz(int, int32_t);
int32_t   bfio_write(int, int64_t, char*, int32_t);
void      bfio_touch_a_file(char*, char*);
int       bfio_change_lock_type(char*, char*, char);
char      bfio_check_lock_file(char*, char*);

int       bfio_lock_file(char*, char*, int32_t, char); 
int       bfio_lock_the_lockfile(char*, char*, char*);
char      bfio_try_locking_file(char*, char*, int32_t, char); 
int       bfio_unlock_file(char*, char*);
void      bfio_unlock_locked_files();
int       bfio_unlock_the_lockfile(int);
void      bfio_dump_lock_file(char*);

int       bfio_adjust_lockfile_name(char*, char*, int32_t);
void      bfio_get_lock_file_rec(struct bfio_lock_struct*, int, char**, char**, 
              char**, char*, char**, char**, char**);
void      bfio_put_lock_file_rec(struct bfio_lock_struct*, int, char*, char*, 
              char*, char, char*, char*, char*);
void      bfio_zap_lock_file_rec(struct bfio_lock_struct*, int);
int       bfio_read_lock_file(char*, struct bfio_lock_struct*);
int       bfio_write_lock_file(char*, struct bfio_lock_struct*, int);


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
