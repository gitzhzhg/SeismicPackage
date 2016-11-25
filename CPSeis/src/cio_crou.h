/*<CPS_v1 type="HEADER_FILE"/>

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
!--------------------------------------------------------------------------
! SEE cio.f90 for documentation, SEE also cio_crou.c for code.
! Name       : cio_crou
! Category   : io
! Written    : 2000-05-11   by: Bill Menger
! Revised    : 2007-03-27   by: Kruger Corn
! Maturity   : beta
! Purpose    : Header file for cio_crou.c
!--------------------------------------------------------------------------
!</brief_doc>

!<history_doc>
!--------------------------------------------------------------------------
!                          REVISION HISTORY
!     Date        Author       Description
!     ----        ------       -----------
! 30  2007-03-27  Kruger Corn  Updated to 64 bit arhitecture. Basically
!                              changed long to int32_t.
! 29  2005-12-05  Brian Macy   Added set_direct_io_c
! 28  2004-08-23  Bill Menger  Added cio_isfifo_c
! 27  2004-05-03  R Selzler    Removed "max" macro (redefined on Solaris).
! 26  2004-01-21  Bill Menger  Added cio_set_file_ext_size_i2_c to set file
!                              extents > 2 gigs.
! 25  2003-09-11  Bill Menger  Added fseeko, ftello, remove.
! 24  2003-08-27  Bill Menger  Added mimic functions for "c" to call that
!                              exactly duplicate fopen, fclose, setvbuf,
!                              ftell, fseek, rewind, feof, ferror, fileno...
! 23  2003-08-15  Bill Menger  Moved includes from here to cio_crou.c
! 22. 2003-05-28  C C Burch    Add get_lock_status_c
! 21. 2003-01-23  R.S.Day      cio_unlock_file_f was missing an underscore
!                              when NEED_UNDERSCORE was defined for solaris.
! 20. 2002-08-12  R.S.Day      Added prototypes of f77 functions
!                              cio_lock_file_f and cio_unlock_file_f 
!                              cio_normal_lock_f,  cio_extended_lock_f.
! 19. 2002-07-23  C C Burch    Added lock file name to lock/unlock calls
!                              Added cio_set_cpsdisk_control_c
! 18. 2002-05-24  C C Burch    Added cio_try_lock_file_c 
! 17. 2002-05-20  C C Burch    Added cio_set_lock_type
! 16. 2001-12-18  C C Burch    Added cio_get_fn_ext_size_c,cio_set_debug_mode_c
! 15. 2001-12-06  Bill Menger  Added cio_calc_crc routine
! 14. 2001-10-16  C C Burch    Changed _s names to _c names
!                              Reorder names alphabetically
!                              Added cio_system
! 13. 2001-08-09  C C Burch    Added cio_set_file_lock_control_c,
!                              cio_set_file_auto_delete_c, and
!                              cio_get_file_info_c
! 12. 2001-04-26  Ed Schmauch  Added prototypes for cio_checksum_c,
!                              cio_update_file_time_c, and
!                              cio_write_message_file_c.
! 11. 2001-04-03  Bill Menger  Buffer size changes allowed, max filename upped
!                              to 260 from 160. Added trace dump capability.
! 10. 2001-02-27  Bill Menger  Added prototypes for region_locks and err recover
!  9. 2001-01-03  Bill Menger  Removed constants and put in .c file.
!  8. 2000-12-12  Bill Menger  added cio_lock_file, cio_unlock_file
!  7. 2000-12-07  Bill Menger  Added new pfio functions.
!  6. 2000-11-20  Bill Menger  Made the buffer 64K instead of 4K
!  5. 2000-10-03  Bill Menger  Added rename function.
!  4. 2000-09-11  Bill Menger  Removed ident string and functions.
!  3. 2000-08-17  Bill Menger  got rid of pesky lint message on typedef,
!                              added remote access global, init to 0,
!                              set buffer size to 4K.
!  2. 2000-05-12  Bill Menger  Fixed bug in structure for cio_file.
!  1. 2000-05-11  Bill Menger  Extracted cio_crou.h to create header file.
!--------------------------------------------------------------------------
!</history_doc>
*/

#ifndef _CIO_CROU_H_
#define _CIO_CROU_H_

#include <string.h>
#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>
#include <fcntl.h>
#include <sys/types.h>
#include <sys/stat.h>

#include "named_constants.h"
#include "c2f_interface.h"
#include "upgrade264.h"

#ifdef __cplusplus
extern "C" {
#endif

#ifndef CHARACTER
#define CHARACTER          char
#endif

#define CIO_DEBUG           0

#define CIO_OK              0
#define CIO_ERROR          -7
#define CIO_OUT_OF_RANGE   -8
#define CIO_EOF            -1
#define CIO_FIRSTUNIT     110

/*#define CIO_BUFSIZE   1048576*/
/*#define CIO_BUFSIZE    524288*/
/*#define CIO_BUFSIZE    262144*/
#define CIO_BUFSIZE    131072
/*#define CIO_BUFSIZE     65536*/
/*#define CIO_BUFSIZE     32768*/
/*#define CIO_BUFSIZE     16384*/
/*#define CIO_BUFSIZE      8192*/
/*#define CIO_BUFSIZE      4096*/
/*#define CIO_BUFSIZE      2048*/
/*#define CIO_BUFSIZE      1024*/
/*#define CIO_BUFSIZE       512*/
/*#define CIO_BUFSIZE       256*/
/*#define CIO_BUFSIZE       128*/
/*#define CIO_BUFSIZE        64*/

#define CIO_CACHESIZE      10
#define CIO_MAX_FILENAME  260
/******* SEE BFIO.C for BF_FLSZ which should == this NUMBER */
/**********************************************************************
*********** max units will be allowed to dynamically grow as needed ***
**********************************************************************/
#define CIO_INITIAL_NUMUNITS 5
#define CIO_INCR_NUMUNITS    5

typedef struct cio_crou_struct {
  INTEGER  pfio_ifile;
  INTEGER  dispose;
  INTEGER  unit;
  FILE     *stream;
  INTEGER  error;
  INTEGER  eof;
  char     mode[3];
  char     *name;
  struct   stat fdstat;
} CioFile;

#ifdef NEED_UNDERSCORE

#define cio_set_direct_io_c         cio_set_direct_io_c_
#define cio_fbackspace_c            cio_fbackspace_c_
#define cio_checksum_c              cio_checksum_c_
#define cio_chmod_c                 cio_chmod_c_
#define cio_calc_crc_c              cio_calc_crc_c_
#define cio_calc_crci_c             cio_calc_crci_c_
#define cio_get_lock_status_c       cio_get_lock_status_c_
#define cio_dump_lock_file_c        cio_dump_lock_file_c_
#define cio_extsize_c               cio_extsize_c_
#define cio_fclose_c                cio_fclose_c_
#define cio_finalize_c              cio_finalize_c_
#define cio_finquire_c              cio_finquire_c_
#define cio_flsz_cn_c               cio_flsz_cn_c_
#define cio_flsz_cu_c               cio_flsz_cu_c_
#define cio_fopen_c                 cio_fopen_c_
#define cio_fflush_c                cio_fflush_c_
#define cio_fgetline_c              cio_fgetline_c_
#define cio_fread_char_c            cio_fread_char_c_
#define cio_fread_complex_c         cio_fread_complex_c_
#define cio_fread_double_c          cio_fread_double_c_
#define cio_fread_integer_c         cio_fread_integer_c_
#define cio_fread_integer1_c        cio_fread_integer1_c_
#define cio_fread_integer2_c        cio_fread_integer2_c_
#define cio_fread_logical_c         cio_fread_logical_c_
#define cio_fread_real_c            cio_fread_real_c_
#define cio_frewind_c               cio_frewind_c_
#define cio_fseek_block_and_byte_c  cio_fseek_block_and_byte_c_
#define cio_fseek_normal_c          cio_fseek_normal_c_
#define cio_fseek_recd_c            cio_fseek_recd_c_
#define cio_ftell_block_and_byte_c  cio_ftell_block_and_byte_c_
#define cio_ftell_normal_c          cio_ftell_normal_c_
#define cio_ftell_recd_c            cio_ftell_recd_c_
#define cio_fwrite_char_c           cio_fwrite_char_c_
#define cio_fwrite_complex_c        cio_fwrite_complex_c_
#define cio_fwrite_double_c         cio_fwrite_double_c_
#define cio_fwrite_integer_c        cio_fwrite_integer_c_
#define cio_fwrite_integer1_c       cio_fwrite_integer1_c_
#define cio_fwrite_integer2_c       cio_fwrite_integer2_c_
#define cio_fwrite_real_c           cio_fwrite_real_c_
#define cio_fwrite_logical_c        cio_fwrite_logical_c_
#define cio_get_file_ext_size_c     cio_get_file_ext_size_c_
#define cio_get_fn_ext_size_c       cio_get_fn_ext_size_c_
#define cio_get_file_info_c         cio_get_file_info_c_
#define cio_isfifo_c                cio_isfifo_c_
#define cio_lock_file_c             cio_lock_file_c_
#define cio_n_ext_c                 cio_n_ext_c_
#define cio_remove_c                cio_remove_c_
#define cio_rename_c                cio_rename_c_
#define cio_set_bufsz_all_c         cio_set_bufsz_all_c_
#define cio_set_bufsz_file_c        cio_set_bufsz_file_c_
#define cio_set_cpsdisk_control_c   cio_set_cpsdisk_control_c_
#define cio_set_debug_mode_c        cio_set_debug_mode_c_
#define cio_set_file_auto_delete_c  cio_set_file_auto_delete_c_
#define cio_set_file_ext_size_c     cio_set_file_ext_size_c_
#define cio_set_file_ext_size_i2_c  cio_set_file_ext_size_i2_c_
#define cio_set_file_lock_control_c cio_set_file_lock_control_c_
#define cio_set_file_space_commit_c cio_set_file_space_commit_c_
#define cio_set_filrgnlock_c        cio_set_filrgnlock_c_
#define cio_set_lock_type_c         cio_set_lock_type_c_
#define cio_set_remote_access_c     cio_set_remote_access_c_
#define cio_set_trace_mode_c        cio_set_trace_mode_c_
#define cio_set_wrt_err_rcvr_c      cio_set_wrt_err_rcvr_c_
#define cio_system_c                cio_system_c_
#define cio_trace_dump_c            cio_trace_dump_c_
#define cio_truncate_c              cio_truncate_c_
#define cio_try_lock_file_c         cio_try_lock_file_c_    
#define cio_unlink_c                cio_unlink_c_
#define cio_update_file_time_c      cio_update_file_time_c_
#define cio_unlock_file_c           cio_unlock_file_c_
#define cio_write_message_file_c    cio_write_message_file_c_
#define cio_lock_file_f             cio_lock_file_f_
#define cio_unlock_file_f           cio_unlock_file_f_
#define cio_normal_lock_f           cio_normal_lock_f_
#define cio_extended_lock_f         cio_extended_lock_f_

#endif

#ifdef NEED_CAPITALS

#define cio_set_direct_io_c         CIO_SET_DIRECT_IO_C
#define cio_fbackspace_c            CIO_FBACKSPACE_C
#define cio_checksum_c              CIO_CHECKSUM_C
#define cio_chmod_c                 CIO_CHMOD_C
#define cio_calc_crc_c              CIO_CALC_CRC_C
#define cio_calc_crci_c             CIO_CALC_CRCI_C
#define cio_get_lock_status_c       CIO_GET_LOCK_STATUS_C
#define cio_dump_lock_file_c        CIO_DUMP_LOCK_FILE_C
#define cio_extsize_c               CIO_EXTSIZE_C
#define cio_fclose_c                CIO_FCLOSE_C
#define cio_finalize_c              CIO_FINALIZE_C
#define cio_finquire_c              CIO_FINQUIRE_C
#define cio_flsz_cn_c               CIO_FLSZ_CN_C
#define cio_flsz_cu_c               CIO_FLSZ_CU_C
#define cio_fopen_c                 CIO_FOPEN_C
#define cio_fflush_c                CIO_FFLUSH_C
#define cio_fgetline_c              CIO_FGETLINE_C
#define cio_fread_char_c            CIO_FREAD_CHAR_C
#define cio_fread_complex_c         CIO_FREAD_COMPLEX_C
#define cio_fread_double_c          CIO_FREAD_DOUBLE_C
#define cio_fread_integer_c         CIO_FREAD_INTEGER_C
#define cio_fread_integer1_c        CIO_FREAD_INTEGER1_C
#define cio_fread_integer2_c        CIO_FREAD_INTEGER2_C
#define cio_fread_logical_c         CIO_FREAD_LOGICAL_C
#define cio_fread_real_c            CIO_FREAD_REAL_C
#define cio_frewind_c               CIO_FREWIND_C
#define cio_fseek_block_and_byte_c  CIO_FSEEK_BLOCK_AND_BYTE_C
#define cio_fseek_normal_c          CIO_FSEEK_NORMAL_C
#define cio_fseek_recd_c            CIO_FSEEK_RECD_C
#define cio_ftell_block_and_byte_c  CIO_FTELL_BLOCK_AND_BYTE_C
#define cio_ftell_normal_c          CIO_FTELL_NORMAL_C
#define cio_ftell_recd_c            CIO_FTELL_RECD_C
#define cio_fwrite_char_c           CIO_FWRITE_CHAR_C
#define cio_fwrite_complex_c        CIO_FWRITE_COMPLEX_C
#define cio_fwrite_double_c         CIO_FWRITE_DOUBLE_C
#define cio_fwrite_integer_c        CIO_FWRITE_INTEGER_C
#define cio_fwrite_integer1_c       CIO_FWRITE_INTEGER1_C
#define cio_fwrite_integer2_c       CIO_FWRITE_INTEGER2_C
#define cio_fwrite_real_c           CIO_FWRITE_REAL_C
#define cio_fwrite_logical_c        CIO_FWRITE_LOGICAL_C
#define cio_get_file_ext_size_c     CIO_GET_FILE_EXT_SIZE_C
#define cio_get_fn_ext_size_c       CIO_GET_FN_EXT_SIZE_C
#define cio_get_file_info_c         CIO_GET_FILE_INFO_C
#define cio_isfifo_c                CIO_ISFIFO_C
#define cio_lock_file_c             CIO_LOCK_FILE_C
#define cio_n_ext_c                 CIO_N_EXT_C
#define cio_remove_c                CIO_REMOVE_C
#define cio_rename_c                CIO_RENAME_C
#define cio_set_bufsz_all_c         CIO_SET_BUFSZ_ALL_C
#define cio_set_bufsz_file_c        CIO_SET_BUFSZ_FILE_C
#define cio_set_debug_mode_c        CIO_SET_DEBUG_MODE_C
#define cio_set_cpsdisk_control_c   CIO_SET_CPSDISK_CONTROL_C
#define cio_set_file_auto_delete_c  CIO_SET_FILE_AUTO_DELETE_C
#define cio_set_file_ext_size_c     CIO_SET_FILE_EXT_SIZE_C
#define cio_set_file_ext_size_i2_c  CIO_SET_FILE_EXT_SIZE_I2_C
#define cio_set_file_lock_control_c CIO_SET_FILE_LOCK_CONTROL_C
#define cio_set_file_space_commit_c CIO_SET_FILE_SPACE_COMMIT_C
#define cio_set_filrgnlock_c        CIO_SET_FILRGNLOCK_C
#define cio_set_lock_type_c         CIO_SET_LOCK_TYPE_C
#define cio_set_remote_access_c     CIO_SET_REMOTE_ACCESS_C
#define cio_set_trace_mode_c        CIO_SET_TRACE_MODE_C
#define cio_set_wrt_err_rcvr_c      CIO_SET_WRT_ERR_RCVR_C
#define cio_system_c                CIO_SYSTEM_C
#define cio_trace_dump_c            CIO_TRACE_DUMP_C
#define cio_truncate_c              CIO_TRUNCATE_C
#define cio_try_lock_file_c         CIO_TRY_LOCK_FILE_C  
#define cio_unlink_c                CIO_UNLINK_C
#define cio_update_file_time_c      CIO_UPDATE_FILE_TIME_C
#define cio_unlock_file_c           CIO_UNLOCK_FILE_C
#define cio_write_message_file_c    CIO_WRITE_MESSAGE_FILE_C
#define cio_lock_file_f             CIO_LOCK_FILE_F
#define cio_unlock_file_f           CIO_UNLOCK_FILE_F
#define cio_normal_lock_f           CIO_NORMAL_LOCK_F
#define cio_extended_lock_f         CIO_EXTENDED_LOCK_F

#endif


/******************** Prototypes ********************************/
/******************** Start Fortran-90 Callable prototypes*******/

INTEGER cio_set_direct_io_c(INTEGER *unit);
void    cio_fbackspace_c(INTEGER * unit);
void    cio_checksum_c(INTEGER*, INTEGER*, INTEGER*);
INTEGER cio_chmod_c (char *pathname, mode_t *perms);
INTEGER cio_calc_crc_c (char *buff, INTEGER *num_bytes);
INTEGER cio_calc_crci_c (INTEGER *buff, INTEGER *num_bytes);
INTEGER cio_get_lock_status_c(CHARACTER*, CHARACTER*);
void    cio_dump_lock_file_c();
INTEGER cio_extsize_c();
INTEGER cio_fclose_c (INTEGER * unit, INTEGER * remove);
INTEGER cio_fflush_c (INTEGER  * unit);
INTEGER cio_fgetline_c      (char *p, INTEGER * max, INTEGER * unit);
void    cio_finalize_c ();
INTEGER cio_finquire_c(INTEGER *unit, char *name);
void    cio_flsz_cn_c(char * fname, INTEGER * ext, INTEGER * off);
void    cio_flsz_cu_c(INTEGER * unit, INTEGER * ext, INTEGER * off);
INTEGER cio_fopen_c (char *filename, char *mode, INTEGER *dispose);
INTEGER cio_fread_char_c     (void *ptr, INTEGER *nbytes,  INTEGER * unit);
INTEGER cio_fread_complex_c  (void *ptr, INTEGER * nbytes, INTEGER * unit);
INTEGER cio_fread_double_c   (void *ptr, INTEGER * nbytes, INTEGER * unit);
INTEGER cio_fread_integer_c  (void *ptr, INTEGER * nbytes, INTEGER * unit);
INTEGER cio_fread_integer1_c (void *ptr, INTEGER * nbytes, INTEGER * unit);
INTEGER cio_fread_integer2_c (void *ptr, INTEGER * nbytes, INTEGER * unit);
INTEGER cio_fread_logical_c  (void *ptr, INTEGER * nbytes, INTEGER * unit);
INTEGER cio_fread_real_c     (void *ptr, INTEGER * nbytes, INTEGER * unit);
void    cio_frewind_c(INTEGER * unit);
int32_t cio_fseek_block_and_byte_c
          (INTEGER * unit, INTEGER * blocksize, INTEGER * whichblock,
           INTEGER * whichbyte, INTEGER * origin);
int32_t cio_fseek_normal_c (INTEGER * unit, INTEGER * offset, INTEGER * origin);
int32_t cio_fseek_recd_c
          (INTEGER * unit, INTEGER * start_byte, INTEGER * record_length,
           INTEGER * trace_number);
int32_t cio_ftell_block_and_byte_c
          (INTEGER * unit, INTEGER * blocksize, INTEGER * whichblock,
           INTEGER * whichbyte);
int32_t cio_ftell_normal_c(INTEGER * unit);
int32_t cio_ftell_recd_c
          (INTEGER * unit, INTEGER * start_byte, INTEGER * record_length);
INTEGER cio_fwrite_char_c     (void *ptr, INTEGER * nbytes, INTEGER * unit);
INTEGER cio_fwrite_complex_c  (void *ptr, INTEGER * nbytes, INTEGER * unit);
INTEGER cio_fwrite_double_c   (void *ptr, INTEGER * nbytes, INTEGER * unit);
INTEGER cio_fwrite_integer_c  (void *ptr, INTEGER * nbytes, INTEGER * unit);
INTEGER cio_fwrite_integer1_c (void *ptr, INTEGER * nbytes, INTEGER * unit);
INTEGER cio_fwrite_integer2_c (void *ptr, INTEGER * nbytes, INTEGER * unit);
INTEGER cio_fwrite_logical_c  (void *ptr, INTEGER * nbytes, INTEGER * unit);
INTEGER cio_fwrite_real_c     (void *ptr, INTEGER * nbytes, INTEGER * unit);
INTEGER cio_get_file_ext_size_c(INTEGER * unit);
INTEGER cio_get_fn_ext_size_c(char *fn);
void    cio_get_file_info_c(char *, char *, char *); 
INTEGER cio_get_iptr_c(INTEGER * unit);
void    cio_info_c(INTEGER iptr);
INTEGER cio_isfifo_c(INTEGER * unit); /* CIO_ERROR if bad file, 0=no, 1=yes */
INTEGER cio_lock_file_c(char*, char* , INTEGER *);
INTEGER cio_n_ext_c(char *filename);
INTEGER cio_pfio_read_c(char *buff, INTEGER nbytes, INTEGER unit);
INTEGER cio_pfio_write_c(char *buff, INTEGER nbytes, INTEGER unit);
INTEGER cio_remove_c (char *pathname);
int     cio_rename_c(char *, char *);
INTEGER cio_set_bufsz_all_c(INTEGER * bufsz);
INTEGER cio_set_bufsz_file_c(INTEGER * bufsz, INTEGER * unit);
void    cio_set_cpsdisk_control_c(INTEGER *isw);
void    cio_set_debug_mode(INTEGER *isw);
void    cio_set_file_auto_delete_c(INTEGER *isw);
void    cio_set_file_space_commit_c(INTEGER *isw);
INTEGER cio_set_file_ext_size_c(INTEGER * extsize);
INTEGER cio_set_file_ext_size_i2_c(INTEGER * blk, INTEGER * byt);
void    cio_set_file_lock_control_c(INTEGER *isw);
INTEGER cio_set_filrgnlock_c(INTEGER * unit, INTEGER * mode);
void    cio_set_lock_type(INTEGER*);
void    cio_set_remote_access_c(INTEGER * access);
void    cio_system_c(char * command);
void    cio_set_trace_mode_c(INTEGER * isw);
INTEGER cio_set_wrt_err_rcvr_c(INTEGER * unit,INTEGER * mode);
void    cio_tidy_iptr_c(INTEGER iptr);
void    cio_trace_dump_c(char * filename, INTEGER * lrecl, INTEGER *beg_rec,
                                       INTEGER * end_rec,INTEGER *offset);
INTEGER cio_truncate_c(char * fname, INTEGER * ext, INTEGER * off);
INTEGER cio_try_lock_file_c(char*, char*, INTEGER*, INTEGER *);
int     cio_unlock_file_c(char*, char*);
void    cio_update_file_time_c(char *);
void    cio_write_message_file_c(char*, char*);
void    cio_wipe_iptr_c(INTEGER iptr);

INTEGER cio_lock_file_f(char *file, INTEGER *lsec,INTEGER *ltype,
         INTEGER *lstat);
INTEGER cio_unlock_file_f(char *file, INTEGER *lstat);
INTEGER cio_normal_lock_f();
INTEGER cio_extended_lock_f();

/******************** Start "C" callable prototypes *************/

void cio_setvbuf_mimic(FILE *stream, char *buf, int mode, size_t size);
FILE *cio_fopen_mimic(const char *path, const char *mode);

size_t cio_fread_mimic(void *ptr, size_t size, size_t nmemb, FILE *stream);
size_t cio_fwrite_mimic(
                 const void *ptr, size_t size, size_t nmemb, FILE *stream);
int cio_fflush_mimic(FILE *stream);
int cio_fseek_mimic(FILE *stream, int32_t offset, int whence);
int32_t cio_ftell_mimic(FILE *stream);
void cio_rewind_mimic(FILE *stream);

void cio_clearerr_mimic(FILE *stream);
int cio_feof_mimic(FILE *stream);
int cio_ferror_mimic(FILE *stream);
int cio_fileno_mimic(FILE *stream);

int cio_fclose_mimic(FILE *stream);

int cio_fseeko_mimic(FILE *stream, off_t offset, int whence);
off_t cio_ftello_mimic(FILE *stream);
int cio_remove_mimic(const char *pathname);

/******************** END PROTOTYPES ****************************/


#ifdef __cplusplus
}
#endif

#endif
