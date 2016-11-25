/*<CPS_v1 type="PRIMITIVE"/>

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
! SEE cio.f90 for documentation, SEE also cio_crou.h for header info.
! Name       : cio_crou
! Category   : io
! Written    : 1999-09-13   by: Bill Menger
! Revised    : 2007-03-27   by: Kruger Corn
! Maturity   : beta
! Purpose    : Provides F90 with access to the pfio layer via common "C" I/O
!              library look-alikes, and provides "c" programs with look-alikes
!              for common functions, such as getline, putline,fseek,fopen,
!              fclose,fread,fwrite,ftell...
!--------------------------------------------------------------------------
!</brief_doc>

!<descript_doc>
!--------------------------------------------------------------------------
!  These are c support routines for internal use by cio
!--------------------------------------------------------------------------
!</descript_doc>

!<calling_doc>
!--------------------------------------------------------------------------
! These programs are meant to be used only by cio
!--------------------------------------------------------------------------
!</calling_doc>

!<history_doc>
!--------------------------------------------------------------------------
!                          REVISION HISTORY
!     Date        Author       Description
!     ----        ------       -----------
! 66. 2007-03-27 Kruger Corn   Updated for 64 bit architecture. This was
!                              done by replacing long with int32_t and
!                              long long with int64_t.
! 65. 2007-01-25 Bill Menger   Added error output on file open. (from LGC).
! 64. 2005-12-05 Macy/Burch    Added add_desc_flags
! 63. 2005-07-11 Stoeckley     Added some lines so can be compiled by C++.
! 62. 2005-01-11 Bill Menger   Modified S_ISSOCK call to not error out when
!                              opening a socket.
! 61. 2004-12-15 Bill Menger   Modified S_ISDIR call to only return error on
!                              update mode.
! 60. 2004-08-23 Bill Menger   Added named-pipe support.
! 59. 2004-01-21 Bill Menger   Added cio_set_file_ext_size_i2_c
! 58. 2003-09-11 Bill Menger   Added mimic functions for fseeko,ftello,remove.
! 57. 2003-08-27 Bill Menger   Added mimic functions to mimic c-callable
!                              i/o routines.
! 56. 2003-08-15 Bill Menger   * Fixed cio_set_bufsz_all function to act 
!                                properly on error condition(bufsz<0) and
!                              * Fixed open commands to use internal bufsz
!                                instead of constant, and
!                              * Removed a constant extent size and used a
!                                pfio constant instead.
!                              * Removed unused variables, changed info print
!                                to format for pointer.
! 55. 2003-08-07 Bill Menger   Modified cio_extsize_c to handle >2gig extsize.
! 54. 2003-05-28 C C Burch     Added cio_get_lock_status_c.
! 53. 2003-01-23 R.S.Day       Added string.h. The code should never have
!                              compiled without warnings if this was missing!
!                              Mystery?
! 52. 2002-08-12 C. C  Burch   Fix fgetline problem when size data=buffer sz
!                              retain /0 in getline data.
!                              Add lock_file_name to lock-calls.
!                              Add cio_set_cpsdisk_control_c
! 51. 2002-06-18 C. C. Burch   Replace cio_fgetline logic completely-6x faster
!     2002-05-24 C. C. Burch   Added cio_try_lock_file_c for migration rtns
! 50. 2002-05-20 C. C. Burch   Enhance cio_lock/unlock_file 
! 49. 2002-05-15 C. C. Burch   Added retry to access calls 
!                              print open and read errors
! 48. 2002-02-04  C C Burch    Changed to handle variable length file extents  
!                              Added cio_set_debug_mode_c
! 47. 2001-12-06  Bill Menger  Added cio_calc_crc_c function
! 46. 2001-11-06  Ed Schmauch  Fixed bug in cio_fopen_c; must call
!                              cio_wipe_iptr_c if open fails.
! 45. 2001-10-31  Ed Schmauch  Added necessary casts to cio_n_ext_c,
!                              cio_flsz_cu_c, and cio_flsz_cn_c.
!                              Commented out bytes read != bytes max error
!                              message in cio_pfio_read_c.
! 44. 2001-10-24  Ed Schmauch  Modified cio_fgetline_c to ignore characters
!                              with values > 127 since they screw up f90
!                              intrinsic index on Solaris.
!                              Also fixed off-by-one bug in cio_fgetline_c
!                              when determining if buffer is full.
! 43. 2001-10-16  C C Burch    Restructured code and simplified fread/fwrite
!                              Enhanced error checking for array overflow.
! 42. 2001-08-27  C C Burch    Added cio_set_file_lock_control_c
!                              Added cio_get_file_info_c
!                              Added cio_set_file_auto_delete_c
! 41. 2001-04-26  Ed Schmauch  Added cio_checksum_c, cio_update_file_time_c,
!                              and cio_write_message_file_c.
! 40. 2001-04-03  Bill Menger  Added pfio_exit() to cio_finalize routine,
!                              Added buffer size change capability.
!                              Added trace dump capability (calls pfio)
! 39. 2001-02-27  Bill Menger  Added error_recovery and region_locking.
! 38. 2001-02-21  Bill Menger  Changed pfio_write to return error only on <0
!                              return value, and to ignore requests to write
!                              zero-length data.
! 37. 2001-01-12  Bill Menger  Modified truncate call to use long-long arith
!                              for calc of fsize. (bug fix)
! 36. 2001-01-03  Bill Menger  Added constants that were in .h file.
! 35. 2000-12-14  Bill Menger  Added function to calculate number of extents
!                              within a file, this then is used in chmod and
!                              others to help return correct status code.
! 34. 2000-12-12  Bill Menger  The getline function can no longer skip embedded
!                              nulls.  An initialized file would simply get
!                              trapped within this function!!!
!                              Added cio_lock_file, cio_unlock_file
! 33. 2000-12-07  Bill Menger  Added new pfio functions.
! 32. 2000-10-24  Bill Menger  Modified open statement to NOT open file if the
!                              file does not exist. Old method called pfio_open.
! 31. 2000-10-19  Bill Menger  Removed print statements on failure to open file.
! 30. 2000-09-18  Bill Menger  Added rename function.
! 29. 2000-09-11  Bill Menger  Changed seek functions to use Chuck's new 
!                              seek_via_origin function.
! 28. 2000-08-21  Bill Menger  Modified the chmod call to use pfio.
! 27. 2000-09-18  Bill Menger  Commented out debug statements,add remote_access.
! 26. 2000-06-15  Bill Menger  On close, removed flush call if file is to be
!                              deleted anyway.
! 25. 2000-06-01  Bill Menger  Fixed bug in cio_get_iptr_c.
! 24. 2000-05-12  Bill Menger  Fixed bug in structure for cio_file.
! 23. 2000-05-11  Bill Menger  Replaced static parallel arrays with dynamic
!                              struct cio_file.
!                              Made all functions pointer-safe.
!                              Added finalize function to close all files.
!                              Split off header file.
!                              Put pfio back.
! 22. 2000-05-05  Bill Menger  Reverted to not using pfio.
! 21. 2000-04-27  Bill Menger  Commented out the cache.
! 20. 2000-04-26  Bill Menger  Corrected the buffer and cache sizes.
! 19. 2000-04-25  Bill Menger  Fixed the scratch and remove functions.
! 18. 2000-04-19  Bill Menger  Modified the ifdef compiler directives.
! 17. 2000-04-12  Bill Menger  Added CIO_FLSZ constant and function.
! 16. 2000-04-11  Bill Menger  Added diagnostic stderr prints when errors.
! 15. 2000-04-10  Bill Menger  Replaced "c" calls with "pfio" calls.
! 14. 2000-03-31  Bill Menger  Changed minunit=110, maxunits=999, changed
!                              backspace to backspace_c, rewind = rewind_c
! 13. 2000-02-15  Bill Menger  Modified pfread,pfwrite to return number of objs
!                              instead of number of bytes read/written.
! 12. 2000-02-07  Bill Menger  Added constants CIO_OK, CIO_ERROR, CIO_EOF
! 11. 2000-02-04  Bill Menger  Modified fopen to FAIL if trying to open a file
!                              where file EXISTS and mode = 'wn', added chmod.
!                              Also removed getline pre-fill of string to blank
! 10. 2000-01-11  Bill Menger  Added remove call.
!  9. 1999-12-27  Bill Menger  Added unlink call.
!                              ????? const char on filename to fopen?
!  8. 1999-12-09  Bill Menger  Added ident string.
!  7. 1999-10-28  Bill Menger  Added a long-long seek and tell (using blocks)
!  6. 1999-10-12  Bill Menger  Modified backspace function.
!  5. 1999-10-11  Bill Menger  Changed names to lower case,put back long-long.
!  4. 1999-10-07  Bill Menger  Removed long-long seek for now.
!  3. 1999-09-22  Bill Menger  Added long-long byte addressable seek.
!  2. 1999-09-21  Bill Menger  Modified interfaces, fixed bugs.
!  1. 1999-09-13  Bill Menger  Initial version.
!--------------------------------------------------------------------------
!</history_doc>

!<portability_doc>
!--------------------------------------------------------------------------
!                        PORTABILITY LIMITATIONS
! Portions of this code depend upon the ASCII collating sequence and/or ASCII
! variable definitions as defined in ANSI C Lexical conventions.
!--------------------------------------------------------------------------
!</portability_doc>

!<compile_doc>
!-------------------------------------------------------------------------------
!                     SPECIAL COMPILING REQUIREMENTS
! The module "cio.f90" must be included as part of this module.
! For now, to use long long, must compile with "cc" on solaris.
! Needs socket libraries on Solaris for linking (-lxnet).
!--------------------------------------------------------------------------
!</compile_doc>

!--------------------------"module" start ----------------------------------
!--------------------------"module" start ----------------------------------
!--------------------------"module" start ----------------------------------
*/

#define _GNU_SOURCE

#include "bfio.h"
#include "pfio.h"
#include "cio_crou.h"

#include <string.h>
#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>
#include <fcntl.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <sys/utsname.h>

#include "named_constants.h"
#include "c2f_interface.h"

/* Solaris has no support for O_DIRECT? */
#ifndef O_DIRECT
# define O_DIRECT 0
#endif

char *cio_crou_ident =
"$Id: cio_crou.c,v 1.66 2007/03/28 15:09:41 Corn beta sps $";


static INTEGER  fptr             =  -1;

static INTEGER cio_remote_access = 0;

static INTEGER cio_bufsz         = CIO_BUFSIZE;

static CioFile **cio_file; /**** here is where file information is stored ***/

/*** To cio, all files seem as having fixed length(DEFAULT_EXTENT_SIZE) ***/

/* the following was added by Tom Stoeckley on 2005-03-22: */
#ifdef __cplusplus
extern "C" {
#endif

/****************************************************************************** 
 *** Provide access funtion with retry after disk remount     ***
 ******************************************************************************/
int cio_access(char *file_name, int mode) {
  int istat;

  if((istat=access(file_name, mode))==0) return(istat);
  bfio_get_disk_space(file_name);   /*force remount of disk*/
  istat=access(file_name, mode);
  return(istat);
}

/******************************************************************************
*** set trace mode ***
*******************************************************************************/
void cio_set_trace_mode_c(INTEGER * isw) {
  pfio_set_trace_mode(*isw);
  return;
}

/******************************************************************************
*** dump trace information ***
*******************************************************************************/
void cio_trace_dump_c(CHARACTER * file_name, INTEGER * lrecl, INTEGER *beg_rec,
                                       INTEGER * end_rec,INTEGER *offset) {
  int64_t beg, end;
  char file[260];
  int i;  

  for (i=0;i<260; i++) {
    if(file_name[i]==' ' || file_name[i]=='\0') break;  
    file[i]=file_name[i];
  }
  file[i]='\0';

  beg=(*offset)+ (*beg_rec) * ((int64_t) (*lrecl));
  end=(*offset)+ ((*end_rec)+1) * ((int64_t) (*lrecl)) -1;
  pfio_trace_dump(file, beg, end);

  return;
}

/******************************************************************************
*** set file buffer size for all files ***
*******************************************************************************/
INTEGER cio_set_bufsz_all_c(INTEGER * bufsz) {

  if(*bufsz <= 0 ) {
    cio_bufsz = -1;
    return (INTEGER) CIO_ERROR;
  }
  cio_bufsz = *bufsz;
  return (INTEGER) CIO_OK;
}

/******************************************************************************
*** set file buffer size for this file ***
*******************************************************************************/
INTEGER cio_set_bufsz_file_c(INTEGER *bufsz , INTEGER * unit) {
  int iptr, ibuf;

  if((iptr = cio_get_iptr_c(unit)) < 0 ) return (INTEGER) CIO_ERROR;
  
  ibuf = pfio_setbufsz(cio_file[iptr]->pfio_ifile,(int32_t) *bufsz);
  if(ibuf < 0 ) {
    fprintf(stderr,"cio_set_bufsz_file_c: error-pfio_setbufsz status = %d\n",
    ibuf);
    return (INTEGER) CIO_ERROR;
  }
  return (INTEGER) CIO_OK;
}

/******************************************************************************
*** set file region locking *** mode=1 set, mode=0 unset
*******************************************************************************/
INTEGER cio_set_filrgnlock_c(INTEGER * unit, INTEGER * mode) {
  /* int iptr; */
  INTEGER status;

  if((cio_get_iptr_c(unit)) < 0 ) return (INTEGER) CIO_ERROR;
  status=CIO_OK;
  /* if((iptr = cio_get_iptr_c(unit)) < 0 ) return (INTEGER) CIO_ERROR; */
/* next statement disabled
  pfio_set_file_region_locking(cio_file[iptr]->pfio_ifile, *mode);
*/  
  return(status); 
}


/******************************************************************************
*** set write error recovery *** mode=1 set, mode=0 unset
*******************************************************************************/
INTEGER cio_set_wrt_err_rcvr_c(INTEGER * unit,INTEGER * mode) {
  /* int iptr; */

  INTEGER status;

  if((cio_get_iptr_c(unit)) < 0 ) return ((INTEGER) CIO_ERROR);
  status=CIO_OK;
  /* if((iptr = cio_get_iptr_c(unit)) < 0 ) return ((INTEGER) CIO_ERROR);
  status=CIO_OK; */
/* next statement disabled
  (INTEGER) pfio_set_write_error_recovery(cio_file[iptr]->pfio_ifile, *mode);
*/  
  return(status); 
}

/******************************************************************************
 *** get the number of file extents. ***
 ******************************************************************************/
 INTEGER cio_n_ext_c(char * fname) {
   int64_t extsz;
   int32_t n_ext;

   pfio_get_extent_info(fname,&n_ext,&extsz);
   return (INTEGER) n_ext;
}

/****************************************************************************** 
 *** dump contents of lock file-for diagnostic use 
 ******************************************************************************/
void cio_dump_lock_file_c() {
  pfio_dump_lock_file();
}

/****************************************************************************** 
 *** sets the type of lock for the next cio_lock_file call
 ******************************************************************************/
void cio_set_lock_type_c(INTEGER *type) {
  pfio_set_lock_type((char) (*type));
  return;
}

/****************************************************************************** 
 *** Lock a file for nnnn seconds.
 ******************************************************************************/
INTEGER cio_lock_file_c(CHARACTER *lock_file, CHARACTER *fname, 
 INTEGER *seconds) {
  INTEGER status;

  status =  pfio_lock_file(lock_file, fname,*seconds);
  return status;
}

/****************************************************************************** 
 *** Try to lock a file for nnnn seconds.
 ******************************************************************************/
INTEGER cio_try_lock_file_c(CHARACTER *lock_file, CHARACTER *fname, 
 INTEGER *seconds, INTEGER *lock_type) {
  char status;

  status=pfio_try_locking_file(lock_file, fname, *seconds, (char) (*lock_type));
  if(status==' ') return(0);
  if(status=='H' || status=='R' || status=='X') return(1);
  if(status=='N') return(2);
  if(status=='E') return(3);
  return(-1);
}

/****************************************************************************** 
 *** Get a file's lock status.
 ******************************************************************************/
INTEGER cio_get_lock_status_c(CHARACTER *lock_file, CHARACTER *fname) {
  char status;

  status=bfio_check_lock_file(lock_file, fname);
  if(status==' ') return(0);
  if(status=='H' || status=='R' || status=='X') return(1);
  if(status=='N') return(2);
  if(status=='E') return(3);
  return(-1);
}

/****************************************************************************** 
 *** unlock a file.
 ******************************************************************************/
INTEGER cio_unlock_file_c(CHARACTER *lock_file, CHARACTER * fname) {
  INTEGER status;

  status =  pfio_unlock_file(lock_file, fname);
  return status;
}

/****************************************************************************** 
 *** get ext size for an open unit.
 ******************************************************************************/
INTEGER cio_get_file_ext_size_c(INTEGER * unit) {
/*treat all files to cio as fixed extent length  */
  return (INTEGER) DEFAULT_EXTENT_SIZE;
}

/****************************************************************************** 
 *** get ext size for a file name.
 ******************************************************************************/
INTEGER cio_get_fn_ext_size_c(char *fn) {
/*treat all files to cio as fixed extent length  */
  return (INTEGER) DEFAULT_EXTENT_SIZE;
}

/****************************************************************************** 
 *** set ext size for next file to open
 ******************************************************************************/
INTEGER cio_set_file_ext_size_c(INTEGER * extsize) {
  return (INTEGER) pfio_set_ext_size( (int64_t) *extsize);
}

/****************************************************************************** 
 *** set ext size for next file to open (int64_t)
 ******************************************************************************/
INTEGER cio_set_file_ext_size_i2_c(INTEGER * blk, INTEGER * byt) {
  int64_t extsize;
  extsize = (int64_t) (*blk) * (int64_t) DEFAULT_EXTENT_SIZE + 
            (int64_t) (*byt);
  return (INTEGER) pfio_set_ext_size(extsize);
}

/****************************************************************************** 
 *** set file space commit ... 1 = yes, 0 = no
 ******************************************************************************/
void cio_set_file_space_commit_c(INTEGER *isw) {
  pfio_set_file_space_commit(*isw);
}

/****************************************************************************** 
 *** set file lock control ... 1 = allow locks on file, 0 = lock not allowed
 ******************************************************************************/
void cio_set_file_lock_control_c(INTEGER *isw) {
  pfio_set_file_lock_control(*isw);
}

/****************************************************************************** 
 *** set file auto delete  ... 1 = auto delete file, 0 = no auto delete file
 ******************************************************************************/
void cio_set_file_auto_delete_c(INTEGER *isw) {
  pfio_set_file_auto_delete(*isw);
}

/****************************************************************************** 
 *** sets the type of cpsdisk_control for next pfio_open
 ******************************************************************************/
void cio_set_cpsdisk_control_c(INTEGER *type) {
  pfio_set_cpsdisk_control(*type);
  return;
}

/****************************************************************************** 
 *** get file size
 ******************************************************************************/
void    cio_flsz_cu_c(INTEGER *unit, INTEGER * ext, INTEGER * off) {
  int       iptr;
  int64_t   fsize;
  int32_t   extsize;

  if((iptr = cio_get_iptr_c(unit) ) < 0 ) {
    fprintf(stderr,
      "cio_flsz_c: Error: no filename associated with unit %d\n",*unit);
    fprintf(stderr,"iptr=%d cio_get_iptr_c(unit)=%d\n",
     iptr,cio_get_iptr_c(unit));
    return;
  }

  fsize = pfio_get_current_file_size(cio_file[iptr]->pfio_ifile);

  extsize = cio_get_fn_ext_size_c(cio_file[iptr]->name);
  *ext   = (INTEGER) (fsize/extsize );
  *off   = (INTEGER) (fsize - (int64_t) *ext * (int64_t) extsize );
  
  /*printf("bill:cio_flsz_cu_c: ext: %d off: %d fsize: %lld\n",
     *ext,*off,fsize);*/
  return;
}

void   cio_flsz_cn_c(CHARACTER *fname, INTEGER * ext, INTEGER * off) {
  int64_t   fsize= -1;
  int       iptr;
  int32_t   extsize;

  /*** if the file is opened, then look there first ***/

  extsize = cio_get_fn_ext_size_c(fname);
  for(iptr=0;iptr<=fptr;iptr++){
    if (cio_file[iptr] != NULL ) {
      if(strcmp(fname,cio_file[iptr]->name) == 0 ) {

        /* iflush  = pfio_flush(cio_file[iptr]->pfio_ifile);
           fsize   = pfio_flsz(cio_file[iptr]->name);
        */

        fsize = pfio_get_current_file_size(cio_file[iptr]->pfio_ifile);
        break;
      }
    }
  }

  /*** else if the unit was not opened, then look here ***/
  if(fsize == -1 ) {
    fsize = pfio_flsz(fname);
  }
  *ext   = (INTEGER) (fsize/extsize );
  *off   = (INTEGER) (fsize - (int64_t) *ext * (int64_t) extsize );
  
  /*printf("bill:cio_flsz_cn_c: ext: %d off: %d fsize: %lld\n",
    *ext,*off,fsize);*/
  return;
}
 

/****************************************************************************** 
 *** truncate file to particular size.
 ******************************************************************************/
INTEGER cio_truncate_c(CHARACTER * fname, INTEGER * ext, INTEGER * off){
  int64_t   fsize;
  int         iptr;
  int32_t     extsize;

  /*** if the file is opened, then look there first ***/

  fsize=-1;
  extsize = cio_get_fn_ext_size_c(fname);

  for(iptr=0;iptr<=fptr;iptr++){
    if (cio_file[iptr] != NULL ) {
      if(strcmp(fname,cio_file[iptr]->name) == 0 ) {
        if(pfio_flush(cio_file[iptr]->pfio_ifile) <0);
        break;
      }
    }
  }

  fsize = (int64_t)(*off) + (int64_t)(*ext) * (int64_t)extsize;
  /* 
   printf("bill:cio_truncate_c: ext: %d off: %d fsize: %lld\n",*ext,*off,fsize);
  */

  if(fsize < 0 ) return (INTEGER) fsize; /* ERROR condition if return here */

  return (INTEGER) pfio_truncate(fname, fsize);
}

/****************************************************************************** 
 *** set pfio debug mode :0 none, 1-normal debug >1 heavier debug
 ******************************************************************************/
void cio_set_debug_mode_c(INTEGER *debug) {
  pfio_set_debug_mode(*debug);
}

/****************************************************************************** 
 *** set the remote access integer *** 1 = sockets, 0 = nfs 
 ******************************************************************************/
void cio_set_remote_access_c(INTEGER *access) {
  cio_remote_access = *access;
}

/****************************************************************************** 
 *** give user the value of a big file size 
 ******************************************************************************/
INTEGER cio_extsize_c () {
  if (BF_FLSZ > (int64_t) 2147482624 ){
    return ( (INTEGER) 2147483647);
  }else {
    return ((INTEGER) BF_FLSZ);
  }
}


/****************************************************************************** 
 ******** convert fortran unit number to correct position in cio_file vector
 ******** test to see if good unit number *****
 ******************************************************************************/
INTEGER cio_get_iptr_c(INTEGER * unit) {
  INTEGER iptr;

  for (iptr=0;iptr<=fptr;iptr++){
    if (cio_file[iptr] != NULL ) {
      if(cio_file[iptr]->unit == *unit) return iptr;
    }
  }
  return CIO_ERROR;
}

/****************************************************************************** 
 ******** finalize all units... flush and close all opened files ***
 ******************************************************************************/
void cio_finalize_c () {
  INTEGER dispose;
  INTEGER iptr;
  INTEGER unit;
  INTEGER ifile;

  if(fptr<0) return;

  for (iptr=0;iptr<fptr;iptr++){
    if(cio_file[iptr] != NULL ) {
      unit    = cio_file[iptr]->unit;
      dispose = cio_file[iptr]->dispose;
      ifile   = cio_file[iptr]->pfio_ifile;
      if(ifile > 0 ) cio_fclose_c(&unit, &dispose);
    }
  }
  free(cio_file);
  fptr = -1;
  pfio_exit();
  return;
}

/****************************************************************************** 
 ******** rename file-a to file-b *********************************************
 ******************************************************************************/
INTEGER cio_rename_c(CHARACTER *file_a, CHARACTER * file_b){
  INTEGER status;

/*printf("file_a=%s, file_b=%s\n",file_a, file_b);  */
  status = (INTEGER) pfio_rename_file(file_a,file_b);
  if(status >= 0 ) return CIO_OK;
  return CIO_ERROR;
}

/****************************************************************************** 
 *** This simulates the fopen "c" library call for FORTRAN 90.
 *** F90 gets back a unit number starting with 110 and going up to ????
 *** if a file is successfully opened.  Otherwise, F90 gets back zero.
 ***
 *** ARGUMENTS:
 *** character (len=*), intent(in) :: filename (must be trimmed and end in \0)
 *** character (len=1 or 2), intent(in) :: mode(must be trimmed and end in \0)
 ***
 *** RETURNS:
 *** integer      :: "unit" (minimum = 110, maximum= ????)
 ******************************************************************************/
INTEGER cio_fopen_c (CHARACTER *filename, CHARACTER *mode, INTEGER *dispose) {
    INTEGER ibuf, iptr ,old_size, status;
    /* INTEGER icache; */
    char local_mode[3], append_mode;
    INTEGER j;
    
    CioFile **cio_temp; /* for when we need to grow numunits*/

    strcpy(local_mode,mode);

    /**** First time only, initialize pointers for cio_file ****/
    if(fptr == -1 ) {
      pfio_set_remote_access(cio_remote_access);
      cio_file = (CioFile **) 
                 malloc( (size_t) CIO_INITIAL_NUMUNITS*sizeof(CioFile *));
      fptr += CIO_INITIAL_NUMUNITS;
      for (iptr=0;iptr<=fptr;iptr++) { cio_file[iptr]=NULL; };
    };

    /**** fptr is the highest unit that has been created so far ***/
    for (iptr=0;iptr<=fptr;iptr++){
      /*** if we find an unused or closed unit within the
           set of units, we can use it's pointer position.
      ***/
      if (cio_file[iptr] == NULL) break;
    };

    /** 
    *** increment the fptr (max value of all open units) if pristine pointer
    **/
    /** 
    *** if we did not find an unused pointer, we must increase the size
    *** of our cio_file array by one.
    **/
    if(iptr > fptr ) {
      old_size = fptr;
      fptr +=CIO_INCR_NUMUNITS;
      cio_temp = (CioFile **) 
        realloc(cio_file, (size_t) (fptr+1)*sizeof(CioFile *));
      cio_file = cio_temp;
      for (j=old_size+1;j<=fptr;j++) { cio_file[j]=NULL; };
      /***
      ****printf(" cio_file=%X cio_temp=%X\n",cio_file,cio_temp);
      ****for (j=0;j<fptr;j++) printf("%X ",cio_file[j]);
      ****printf("\n");
      ***/
    };

    /** 
    *** Allocate space for the information (even if previously used, since
    *** the space was freed upon file close.)
    **/
    cio_file[iptr] = (CioFile *) malloc(sizeof(CioFile));
    /*
     * Even though cio_tidy_iptr_c NULLs name, it 1st checks name for
     * NULL; so we must initially NULL it here.
     */
    cio_file[iptr]->name = NULL;
    cio_tidy_iptr_c(iptr);



    /*** add protective code to keep a file from stepping on a file of the
         same name, but only if opened with mode = "wn" or "an".
    ***/

    if(local_mode[1] == 'n' ) {
        /*** access will let us know if the base extent exists ***/
        if(cio_access(filename,F_OK) == 0 ) {
          /*** then the file exists ***/
          fprintf(stderr,
          "cio_fopen: Error using 'wn' or 'an' mode for '%s': file exists.\n",
           filename);
          cio_wipe_iptr_c(iptr);
          return CIO_ERROR;
        };
        local_mode[1] = '\0';
    }
    /***  Convert local mode to something that pfio can understand.  It must
          be r w or u. (if append, save in "append_mode" variable.)
    ***/
    append_mode = local_mode[0];
    switch (local_mode[0]){
      case('r'):
        if(cio_access(filename,R_OK) != 0 )
        {
          /*** we don't really need this...
          fprintf(stderr,
            "Error in cio_fopen: Do not have read permission on file '%s'.\n",
             filename);
          ***/
          cio_wipe_iptr_c(iptr);
          return CIO_ERROR;
        }
        break;
      case('w'):
        break;
      case('a'):
        if(cio_access(filename,F_OK) != 0 ) {
          local_mode[0] = 'w';
        } else {
          local_mode[0] = 'u';
        }
        break;
      default:
        fprintf(stderr,
          "Error in cio_fopen: Bad mode '%s' for file '%s'.\n",
           local_mode,filename);
        cio_wipe_iptr_c(iptr);
        return CIO_ERROR;
    }
    switch (local_mode[1]){
      case('+'):
        if(cio_access(filename,F_OK) != 0 ) {
          if(local_mode[0] == 'r' ) break;
          local_mode[0] = 'w';
        } else {
          local_mode[0] = 'u';
        }
        break;
      default:
        break;
    }
    local_mode[1] = '\0';

    /* If writing or appending to existing file, do we have write permission? */
    if(local_mode[0] != 'r') {
      if(cio_access(filename,F_OK) == 0 ) {
        if(cio_access(filename,W_OK) != 0 ) {
          /*** don't really need this...
          fprintf(stderr,
            "Error in cio_fopen: Do not have write permission on file '%s'.\n",
             filename);
          ***/
          cio_wipe_iptr_c(iptr);
          return CIO_ERROR;
        }
      }
    }

    /*** add code to do a stat on the file ***/
    status=stat(filename,&cio_file[iptr]->fdstat);
    /*fprintf(stderr,"file=%s stat.st_mode=%o\n",*/
    /*filename,cio_file[iptr]->fdstat.st_mode);*/

    if((status != 0) && local_mode[0] != 'w') {
        fprintf(stderr,
        "cio_fopen: Error getting status on file '%s'.\n",filename);
        cio_wipe_iptr_c(iptr);
      return CIO_ERROR;
    }

    if(S_ISDIR(cio_file[iptr]->fdstat.st_mode) &&
       local_mode[0] == 'u') {
       fprintf(stderr,
       "cio_fopen: Error opening Directory '%s' in update mode.\n",filename);
       cio_wipe_iptr_c(iptr);
       return CIO_ERROR;
    }

    if(S_ISSOCK(cio_file[iptr]->fdstat.st_mode)) {
       fprintf(stderr,
       "cio_fopen: Opening Socket '%s'.\n",filename);
       /*cio_wipe_iptr_c(iptr);*/
       /*return CIO_ERROR;*/
    }

    if(S_ISFIFO(cio_file[iptr]->fdstat.st_mode)) {
       fprintf(stderr,
       "cio_fopen: Opening named pipe '%s'.\n",filename);
       /*cio_wipe_iptr_c(iptr);*/
       /*return CIO_ERROR;*/
    }

    /*** Open the file, store the pfio ifile "pointer" in cio_file[iptr] ***/
    cio_file[iptr]->pfio_ifile = pfio_open(filename,*local_mode);
    /*fprintf(stderr,"cio_fopen_c: filename=%s ifile=%d mode=%s\n",filename,*/
      /*cio_file[iptr]->pfio_ifile,local_mode);*/
        
    if(cio_file[iptr]->pfio_ifile <= 0 ) {
        /*** upon error, free space for pointer and exit ***/
        fprintf(stderr,
          "cio_fopen_c: Error(%d) opening file(%s) with mode(%s)\n",
          cio_file[iptr]->pfio_ifile, filename, local_mode);
        
     /* cio_file[iptr]->name = NULL; cio_wipe_iptr_c NULLs name. */
        cio_wipe_iptr_c(iptr);
        return CIO_ERROR;
    } else {
        /*** upon success, save information about the file ***/
        /* save the file name (and allocate space for it) */
        cio_file[iptr]->name = 
          (char *)malloc(((int) strlen(filename)+1));
        if(cio_file[iptr]->name != NULL)
        {
           strcpy(cio_file[iptr]->name,filename);
        }
        else
        {
          fprintf(stderr, "cio_fopen_c: Error with malloc of cio_file space\n");
          cio_wipe_iptr_c(iptr);
          return CIO_ERROR;
        }

        strcpy(cio_file[iptr]->mode,mode);

        /* save the disposition of the file */
        cio_file[iptr]->dispose = *dispose;

        /* set up buffering for the file */
        if(S_ISFIFO(cio_file[iptr]->fdstat.st_mode)) {
          /* no buffering on fifo ?? */
          /*ibuf = pfio_setbufsz(cio_file[iptr]->pfio_ifile,(int32_t) 0);*/
          ibuf = pfio_setbufsz(cio_file[iptr]->pfio_ifile,(int32_t) cio_bufsz);
        } else {
          ibuf = pfio_setbufsz(cio_file[iptr]->pfio_ifile,(int32_t) cio_bufsz);
        }
        if(ibuf < 0 ) {
          fprintf(stderr,"cio_fopen_c: error-pfio_setbufsz status = %d\n",ibuf);
          cio_wipe_iptr_c(iptr);
          return CIO_ERROR;
        }

        /* set up the cache for the file */
        /********************************************************************
        icache = 0;
        *icache = pfio_setcachesz(cio_file[iptr]->pfio_ifile,
        *                         (int32_t) CIO_CACHESIZE);
        *if(icache < 0 ) {
        *  fprintf(stderr,"cio_fopen_c: error-pfio_setcachesz status = %d\n",
        *  icache);
        *}
        ********************************************************************/

        /*** if read or update, pre-fetch info into buffer ***/

        switch(local_mode[0]){
          case('r'): case('u'):
            if(pfio_fetch(cio_file[iptr]->pfio_ifile,
                                (int64_t) 0,
                                (int32_t) cio_bufsz));
            break;
        }

        /*** set up unit and store in structure cio_file ***/
        
        cio_file[iptr]->unit=iptr+CIO_FIRSTUNIT;

        /* in append mode, move to the back of the bus. */
        if(append_mode == 'a' )
          if(
             pfio_seek_via_origin(
               cio_file[iptr]->pfio_ifile,(int64_t) 0,SEEK_END)
          != 0)
            fprintf(stderr,"cio_fopen_c: error seeking to EOF for append.\n");
        /* set up a "stream" for mimic'd calls to use */
        cio_file[iptr]->stream = (FILE *) &cio_file[iptr]->unit;
        cio_file[iptr]->error = 0;
        cio_file[iptr]->eof   = 0;
        return cio_file[iptr]->unit; /* the unit number */
    }
}

/****************************************************************************** 
 ***    This simulates the fclose "c" library call for FORTRAN 90.
 ***    F90 gets back zero if close is successful, and EOF on error.
 ***
 ***    ARGUMENTS:
 ***    integer , intent(in)     :: close_fptr (unit)
 ***
 ***    RETURNS:
 ***    integer                  :: status (0 = ok, EOF = error)
 ******************************************************************************/
INTEGER cio_fclose_c (INTEGER * unit, INTEGER * remove) {
    /* if remove = 0 keep, if remove = 1 delete */
    INTEGER iflush, iclose, ifile, iptr, retvar;
    
    iflush = 0;
    retvar = CIO_OK;
    if ((iptr = cio_get_iptr_c(unit)) < 0 ) return CIO_ERROR;

    ifile   = cio_file[iptr]->pfio_ifile;

    if( (cio_file[iptr]->dispose | *remove) == 1 ) {
      /* remove the file(s) and don't bother with flush */
      iclose = pfio_close(ifile);
      if(iclose > 0 ) iclose = 0 ;
      if((iclose | iflush) != CIO_OK ) {
        fprintf(stderr,"cio_fclose_c: error-close= %d flush= %d\n",
        iclose,iflush);
      }
      retvar = cio_remove_c(cio_file[iptr]->name);
    } else {
      iflush = pfio_flush(ifile);
      iclose = pfio_close(ifile);
      if(iclose > 0 ) iclose = 0 ;
      if((iclose | iflush) != CIO_OK ) {
        retvar = CIO_ERROR;
        fprintf(stderr,"cio_fclose_c: error-close= %d flush= %d\n",
        iclose,iflush);
      }
    }
    cio_wipe_iptr_c(iptr);
    return retvar;
}

/****************************************************************************** 
 *** give user file name associated with unit 
 ******************************************************************************/
INTEGER cio_finquire_c(INTEGER *unit, char *name){
    INTEGER iptr;
    
    if((iptr = cio_get_iptr_c(unit)) < 0 ) return CIO_ERROR;
    strcpy(name,cio_file[iptr]->name);
    return CIO_OK;
}

/****************************************************************************** 
 ***  INTEGER cio_remove_c (const char *pathname) calls "remove" from fortran90
 ***  Return variable = 0 for success, else error.
 ******************************************************************************/
INTEGER cio_remove_c (CHARACTER *pathname) {
  INTEGER n_exts, retvar;
  
  n_exts  = cio_n_ext_c(pathname);
  retvar  = pfio_delete (pathname);
  if(retvar != n_exts) {
    retvar = CIO_ERROR;
  } else {
    retvar=CIO_OK;
  }
  return retvar;
}

/****************************************************************************** 
 *** INTEGER cio_chmod_c (const *pathname, mode_t *perms )
 *** calls "chmod" from fortran90
 *** Return variable = 0 for success, else error.
 ******************************************************************************/
INTEGER cio_chmod_c (CHARACTER *pathname, mode_t *perms) {
  int32_t n_exts;
  int mode;
  INTEGER retvar;

  n_exts     = cio_n_ext_c(pathname);
  mode        = *perms;
  retvar  = pfio_chmod (pathname, mode) - n_exts;
  /*** 
       The return variable must be 0 on success, and pfio_chmod will return the
       number of extents that were changed.
  ***/
  if(retvar != CIO_OK) retvar = CIO_ERROR;
  return retvar;
}

/****************************************************************************** 
 *** INTEGER cio_calc_crc_c (char *buff, INTEGER *num_bytes )
 *** Calculates the crc of a buffer "buff" with length "num_bytes"
 *** Return variable = INTEGER crc value.
 ******************************************************************************/

static uint32_t const crctab[256] = 
{ 0x0,
  0x04C11DB7, 0x09823B6E, 0x0D4326D9, 0x130476DC, 0x17C56B6B,
  0x1A864DB2, 0x1E475005, 0x2608EDB8, 0x22C9F00F, 0x2F8AD6D6,
  0x2B4BCB61, 0x350C9B64, 0x31CD86D3, 0x3C8EA00A, 0x384FBDBD,
  0x4C11DB70, 0x48D0C6C7, 0x4593E01E, 0x4152FDA9, 0x5F15ADAC,
  0x5BD4B01B, 0x569796C2, 0x52568B75, 0x6A1936C8, 0x6ED82B7F,
  0x639B0DA6, 0x675A1011, 0x791D4014, 0x7DDC5DA3, 0x709F7B7A,
  0x745E66CD, 0x9823B6E0, 0x9CE2AB57, 0x91A18D8E, 0x95609039,
  0x8B27C03C, 0x8FE6DD8B, 0x82A5FB52, 0x8664E6E5, 0xBE2B5B58,
  0xBAEA46EF, 0xB7A96036, 0xB3687D81, 0xAD2F2D84, 0xA9EE3033,
  0xA4AD16EA, 0xA06C0B5D, 0xD4326D90, 0xD0F37027, 0xDDB056FE,
  0xD9714B49, 0xC7361B4C, 0xC3F706FB, 0xCEB42022, 0xCA753D95,
  0xF23A8028, 0xF6FB9D9F, 0xFBB8BB46, 0xFF79A6F1, 0xE13EF6F4,
  0xE5FFEB43, 0xE8BCCD9A, 0xEC7DD02D, 0x34867077, 0x30476DC0,
  0x3D044B19, 0x39C556AE, 0x278206AB, 0x23431B1C, 0x2E003DC5,
  0x2AC12072, 0x128E9DCF, 0x164F8078, 0x1B0CA6A1, 0x1FCDBB16,
  0x018AEB13, 0x054BF6A4, 0x0808D07D, 0x0CC9CDCA, 0x7897AB07,
  0x7C56B6B0, 0x71159069, 0x75D48DDE, 0x6B93DDDB, 0x6F52C06C,
  0x6211E6B5, 0x66D0FB02, 0x5E9F46BF, 0x5A5E5B08, 0x571D7DD1,
  0x53DC6066, 0x4D9B3063, 0x495A2DD4, 0x44190B0D, 0x40D816BA,
  0xACA5C697, 0xA864DB20, 0xA527FDF9, 0xA1E6E04E, 0xBFA1B04B,
  0xBB60ADFC, 0xB6238B25, 0xB2E29692, 0x8AAD2B2F, 0x8E6C3698,
  0x832F1041, 0x87EE0DF6, 0x99A95DF3, 0x9D684044, 0x902B669D,
  0x94EA7B2A, 0xE0B41DE7, 0xE4750050, 0xE9362689, 0xEDF73B3E,
  0xF3B06B3B, 0xF771768C, 0xFA325055, 0xFEF34DE2, 0xC6BCF05F,
  0xC27DEDE8, 0xCF3ECB31, 0xCBFFD686, 0xD5B88683, 0xD1799B34,
  0xDC3ABDED, 0xD8FBA05A, 0x690CE0EE, 0x6DCDFD59, 0x608EDB80,
  0x644FC637, 0x7A089632, 0x7EC98B85, 0x738AAD5C, 0x774BB0EB,
  0x4F040D56, 0x4BC510E1, 0x46863638, 0x42472B8F, 0x5C007B8A,
  0x58C1663D, 0x558240E4, 0x51435D53, 0x251D3B9E, 0x21DC2629,
  0x2C9F00F0, 0x285E1D47, 0x36194D42, 0x32D850F5, 0x3F9B762C,
  0x3B5A6B9B, 0x0315D626, 0x07D4CB91, 0x0A97ED48, 0x0E56F0FF,
  0x1011A0FA, 0x14D0BD4D, 0x19939B94, 0x1D528623, 0xF12F560E,
  0xF5EE4BB9, 0xF8AD6D60, 0xFC6C70D7, 0xE22B20D2, 0xE6EA3D65,
  0xEBA91BBC, 0xEF68060B, 0xD727BBB6, 0xD3E6A601, 0xDEA580D8,
  0xDA649D6F, 0xC423CD6A, 0xC0E2D0DD, 0xCDA1F604, 0xC960EBB3,
  0xBD3E8D7E, 0xB9FF90C9, 0xB4BCB610, 0xB07DABA7, 0xAE3AFBA2,
  0xAAFBE615, 0xA7B8C0CC, 0xA379DD7B, 0x9B3660C6, 0x9FF77D71,
  0x92B45BA8, 0x9675461F, 0x8832161A, 0x8CF30BAD, 0x81B02D74,
  0x857130C3, 0x5D8A9099, 0x594B8D2E, 0x5408ABF7, 0x50C9B640,
  0x4E8EE645, 0x4A4FFBF2, 0x470CDD2B, 0x43CDC09C, 0x7B827D21,
  0x7F436096, 0x7200464F, 0x76C15BF8, 0x68860BFD, 0x6C47164A,
  0x61043093, 0x65C52D24, 0x119B4BE9, 0x155A565E, 0x18197087,
  0x1CD86D30, 0x029F3D35, 0x065E2082, 0x0B1D065B, 0x0FDC1BEC,
  0x3793A651, 0x3352BBE6, 0x3E119D3F, 0x3AD08088, 0x2497D08D,
  0x2056CD3A, 0x2D15EBE3, 0x29D4F654, 0xC5A92679, 0xC1683BCE,
  0xCC2B1D17, 0xC8EA00A0, 0xD6AD50A5, 0xD26C4D12, 0xDF2F6BCB,
  0xDBEE767C, 0xE3A1CBC1, 0xE760D676, 0xEA23F0AF, 0xEEE2ED18,
  0xF0A5BD1D, 0xF464A0AA, 0xF9278673, 0xFDE69BC4, 0x89B8FD09,
  0x8D79E0BE, 0x803AC667, 0x84FBDBD0, 0x9ABC8BD5, 0x9E7D9662,
  0x933EB0BB, 0x97FFAD0C, 0xAFB010B1, 0xAB710D06, 0xA6322BDF,
  0xA2F33668, 0xBCB4666D, 0xB8757BDA, 0xB5365D03, 0xB1F740B4
};

INTEGER cio_calc_crc_c (char *buff, INTEGER *num_bytes)
{
  const unsigned char * localbuff = (unsigned char *)buff;
  uint32_t localnumbytes = (uint32_t)*num_bytes;
  uint32_t crc;
  uint32_t i;

  for (crc = 0L, i = 0; i < localnumbytes; i++)
    crc = (crc << 8) ^ crctab[((crc >> 24) ^ localbuff[i]) & 0xFF];

  for (i = localnumbytes; i > 0; i >>= 8)
    crc = (crc << 8) ^ crctab[((crc >> 24) ^ i) & 0xFF];

  crc = ~crc & 0xFFFFFFFF;

  return (INTEGER) crc;
}

/****************************************************************************** 
 *** INTEGER cio_calc_crci_c (INTEGER *buff, INTEGER *num_bytes )
 *** Calculates the crc of a buffer "buff" with length "num_bytes"
 *** Return variable = INTEGER crc value.
 ******************************************************************************/

INTEGER cio_calc_crci_c (INTEGER *buff, INTEGER *num_bytes)
{
  const unsigned char * localbuff = (unsigned char *) buff;
  uint32_t localnumbytes = (uint32_t)*num_bytes;
  uint32_t crc;
  uint32_t i;

  for (crc = 0L, i = 0; i < localnumbytes; i++)
    crc = (crc << 8) ^ crctab[((crc >> 24) ^ localbuff[i]) & 0xFF];

  for (i = localnumbytes; i > 0; i >>= 8)
    crc = (crc << 8) ^ crctab[((crc >> 24) ^ i) & 0xFF];

  crc = ~crc & 0xFFFFFFFF;

  return (INTEGER) crc;
}

/****************************************************************************** 
 ***                                 o         i              i         
 ***   INTEGER cio_fgetline_c      (char *p, INTEGER * max, INTEGER * unit) {
 ***   get a line of text, up to max chars long.  if eof or no newline before
 ***   max chars, then go back to position at beginning of line, and return
 ***   with error message.
 ***   A more efficient method was implemented June 2002 by Charles C Burch
 ******************************************************************************/
INTEGER cio_fgetline_c      (char *p, INTEGER * MAX, INTEGER * unit) {
  INTEGER       iptr;              /* file pointer */
  INTEGER       ifile;
  int64_t       w;                 /* where am I in the file? */
  char          c;                 /* c = char being read in */
  int           n, n1, j, max;

  max=(*MAX);
  if ((iptr = cio_get_iptr_c(unit)) < 0 || max<=0) {
    memset(p,' ',max);
    return(CIO_ERROR);
  }

  ifile = cio_file[iptr]->pfio_ifile;
  w = pfio_tell(ifile);               /* save the current file position. */

  n1=pfio_gets(ifile,p,max);
  /**  printf("cio_fgetline_c, max=%d, n1=%d\n",(*MAX), n1);  **/
  if(n1==0) {
    memset(p,' ',max);
    return(CIO_EOF);
  }

  n=0;
  for (j=0; j<max; j++) {
    if((c=p[j])=='\n') {           /* done if /n */
      memset(p+n,' ',max-n);        
      return(n);
    }
    if(c!='\r' && (c & '\200')==0 && (c!='\0'|| j!=(max-1)) ) 
     p[n++]=c;    
  }

  while(1) {
    n1=pfio_read(ifile,&c,1);
    /** printf("cio_read, n=%d, n1=%d, c=%x\n",n,n1,(int)c); **/
    if(n1!=1) break;
        
    if(c=='\n') {
      if(n!=max) memset(p+n,' ',max-n);        
      return(n);
    }

    if(c!='\r' && (c & '\200')==0) {
      if(n>=max) break;
      p[n++]=c;
    }    
  }  

  if(pfio_seek_via_origin(ifile,w,SEEK_SET) != 0)
    fprintf(stderr,"cio_fgetline_c: error seeking\n"); 
  memset(p+n,' ',max-n);        
  return(CIO_ERROR);        
}

/****************************************************************************** 
 *** flush buffers for a unit ***
 ******************************************************************************/
INTEGER cio_fflush_c (INTEGER  * unit) {
    INTEGER iptr;
    INTEGER iflush;
    INTEGER ifile;
    
    if((iptr = cio_get_iptr_c(unit)) < 0 ) return CIO_ERROR;

    ifile = cio_file[iptr]->pfio_ifile;
    iflush = pfio_flush(ifile);

    if(iflush != CIO_OK) iflush = CIO_ERROR;
    return iflush;
}

/****************************************************************************** 
 *** wrapper around pfio_read ***
 ******************************************************************************/
INTEGER cio_pfio_read_c(char *buff,INTEGER nbytes,INTEGER unit){
    int nbytes_read, iptr, ifile, i;
    char *fifo_buffer=buff;
  
    if((iptr = cio_get_iptr_c(&unit)) < 0 ) return CIO_ERROR;
    if(nbytes<=0) return(0);
  
    ifile = cio_file[iptr]->pfio_ifile;
    /*
      fprintf(stderr,"%s: at %lld read %ld bytes. Got ",cio_file[iptr]->name,
      pfio_tell(ifile),nbytes);
    */

    if(S_ISFIFO(cio_file[iptr]->fdstat.st_mode)) {
      for(i=0;i<nbytes;i++){nbytes_read=pfio_read(ifile,fifo_buffer++,1);}
      nbytes_read = (INTEGER) (fifo_buffer-buff);
      return (INTEGER) nbytes_read;
    };
  
    nbytes_read = pfio_read(ifile, buff, nbytes);
  
    /* fprintf(stderr,"%d returned.\n",nbytes_read);  */
 
    if (nbytes_read <0) {
      fprintf(stderr,
      "cio_pfio_read_c: Error on unit#%d-%s, nbytes_read=%d, nbytes_tried:%d\n",
      (int) unit, cio_file[iptr]->name, nbytes_read, (int)nbytes);
    }

    return ((INTEGER) nbytes_read);
}

/****************************************************************************** 
 *** wrapper around pfio_write ***
 ******************************************************************************/
INTEGER cio_pfio_write_c(char* buff, INTEGER nbytes, INTEGER unit){
    INTEGER nbytes_written, iptr, temp_unit, i;
    char *fifo_buffer=buff;
    
    char fname[]="cio.00000000000";
    char  mode[]="w ";
    static INTEGER dispose = 0;
  
    /*** If user writes to unopened file, open it for him ***/

    if((iptr = cio_get_iptr_c(&unit)) < 0) {
        sprintf(fname,"cio.%11.11d",unit);
        temp_unit = cio_fopen_c(fname,mode,&dispose);
        if(temp_unit < 0 ) return CIO_ERROR;
        if((iptr = cio_get_iptr_c(&temp_unit)) < 0) return CIO_ERROR;
        cio_file[iptr]->unit=unit;
    }

    if(nbytes<=0) return(0);

    /*** To be here, the file must be opened ***/


    if(S_ISFIFO(cio_file[iptr]->fdstat.st_mode)) {
      for(i=0;i<nbytes;i++){
        nbytes_written=pfio_write(cio_file[iptr]->pfio_ifile,fifo_buffer++,1);}
      nbytes_written = (INTEGER) (fifo_buffer-buff);
      return (INTEGER) nbytes_written;
    };

    if(cio_file[iptr]->pfio_ifile);
    nbytes_written = pfio_write(cio_file[iptr]->pfio_ifile, buff, nbytes);

    if (nbytes_written != nbytes) {
      fprintf(stderr,
      "%s: Error on unit#%d-%s nbytes_written=%d, nbytes_tried:%d\n",
      "cio_pfio_write_c", (int)unit, cio_file[iptr]->name, 
       nbytes_written, (int)nbytes);
    }
    return ((INTEGER) nbytes_written);
}

/****************************************************************************** 
 *** Provide F90 interface to various types of reads ***
******************************************************************************/
INTEGER cio_fread_char_c     (void *ptr, INTEGER * nbytes, INTEGER * unit) {
        return cio_pfio_read_c((char *)ptr,*nbytes,*unit);}

INTEGER cio_fread_real_c     (void *ptr, INTEGER * nbytes, INTEGER * unit) {
        return cio_pfio_read_c((char*)ptr,*nbytes,*unit);}

INTEGER cio_fread_integer_c  (void *ptr,INTEGER * nbytes, INTEGER * unit) {
        return cio_pfio_read_c((char*)ptr,*nbytes,*unit);}

INTEGER cio_fread_integer2_c (void *ptr, INTEGER * nbytes, INTEGER * unit) {
        return cio_pfio_read_c((char*)ptr,*nbytes,*unit);}

INTEGER cio_fread_logical_c  (void *ptr,INTEGER * nbytes, INTEGER * unit) {
        return cio_pfio_read_c((char*)ptr,*nbytes,*unit);}

INTEGER cio_fread_double_c   (void *ptr, INTEGER * nbytes, INTEGER * unit) {
        return cio_pfio_read_c((char*)ptr,*nbytes,*unit);}

INTEGER cio_fread_complex_c  (void *ptr,INTEGER * nbytes, INTEGER * unit) {
        return cio_pfio_read_c((char*)ptr,*nbytes,*unit);}

/****************************************************************************** 
 *** Provide F90 interface to various types of writes
 ******************************************************************************/
INTEGER cio_fwrite_char_c     (void *ptr, INTEGER * nbytes, INTEGER * unit) {
        return cio_pfio_write_c((char*)ptr,*nbytes,*unit);}

INTEGER cio_fwrite_integer_c  (void *ptr, INTEGER * nbytes, INTEGER * unit){
        return cio_pfio_write_c((char*)ptr,*nbytes,*unit);}

INTEGER cio_fread_integer1_c  (void *ptr, INTEGER * nbytes, INTEGER * unit) {
        return cio_pfio_read_c((char*)ptr,*nbytes,*unit);}

INTEGER cio_fwrite_integer2_c (void *ptr, INTEGER * nbytes, INTEGER * unit){
        return cio_pfio_write_c((char*)ptr,*nbytes,*unit);}

INTEGER cio_fwrite_integer1_c (void *ptr, INTEGER * nbytes, INTEGER * unit){
        return cio_pfio_write_c((char*)ptr,*nbytes,*unit);}

INTEGER cio_fwrite_real_c     (void *ptr, INTEGER * nbytes, INTEGER * unit) {
        return cio_pfio_write_c((char*)ptr,*nbytes,*unit);}

INTEGER cio_fwrite_double_c   (void *ptr, INTEGER * nbytes, INTEGER * unit) {
        return cio_pfio_write_c((char*)ptr,*nbytes,*unit);}

INTEGER cio_fwrite_logical_c  (void *ptr, INTEGER * nbytes, INTEGER * unit){
        return cio_pfio_write_c((char*)ptr,*nbytes,*unit);}

INTEGER cio_fwrite_complex_c  (void *ptr, INTEGER * nbytes, INTEGER * unit){
        return cio_pfio_write_c((char*)ptr,*nbytes,*unit);}

/****************************************************************************** 
 *** Seek mimics the "c" language call
 ******************************************************************************/
int32_t cio_fseek_normal_c (INTEGER * unit, INTEGER * offset,
    INTEGER * origin) {
    INTEGER  iptr;
    INTEGER  ifile;
    int64_t  lloffset;

    lloffset  = (int64_t)*offset;
    if((iptr = cio_get_iptr_c(unit)) < 0 ) return (int32_t)CIO_ERROR;

    /* IF FIFO don't do anything */
    if(S_ISFIFO(cio_file[iptr]->fdstat.st_mode)) return (int32_t)0;

    ifile = cio_file[iptr]->pfio_ifile;

    return (int32_t)pfio_seek_via_origin(ifile,lloffset,*origin);
}

/****************************************************************************** 
 *** Seek a record number given a starting byte in the file for the section
 *** that contains fixed length records, and the record length.  The returned
 *** trace_number is the record number where the file is positioned to (first
 *** byte of that record (byte 0) )
 ******************************************************************************/
int32_t cio_fseek_recd_c 
   (INTEGER * unit, INTEGER * start_byte, INTEGER * record_length,
    INTEGER * trace_number) {
    INTEGER  iptr;
    INTEGER  ifile;
    int64_t  lloffset;

    lloffset = (int64_t)(*start_byte) +
              (int64_t)(*record_length)*(*trace_number - 1 ) ;

    if((iptr = cio_get_iptr_c(unit)) < 0 ) return CIO_ERROR;

    /* IF FIFO don't do anything */
    if(S_ISFIFO(cio_file[iptr]->fdstat.st_mode)) return (int32_t)0;

    ifile = cio_file[iptr]->pfio_ifile;

    return (int32_t)pfio_seek_via_origin(ifile,lloffset,SEEK_SET); }

/****************************************************************************** 
 *** mimic the "C" function call.
 ******************************************************************************/
int32_t cio_ftell_normal_c(INTEGER * unit) {
    /* Always tell where we are in the EXTENT ONLY */
    int64_t lloffset;
    int32_t pos_in_extent, extent_number;
    INTEGER  iptr, ifile, extsize;

    if((iptr = cio_get_iptr_c(unit)) < 0 ) return CIO_ERROR;

    /* IF FIFO don't do anything */
    if(S_ISFIFO(cio_file[iptr]->fdstat.st_mode)) return (int32_t)0;

    ifile = cio_file[iptr]->pfio_ifile;
    lloffset = pfio_tell(ifile);
    extsize = cio_get_fn_ext_size_c(cio_file[iptr]->name);
    pos_in_extent = lloffset%extsize;
    extent_number = lloffset/extsize;
    if(extent_number > 0 ) {
      fprintf(stderr,
      "cio_ftell_normal: ERROR:Filpos is greater than base file.");
      fprintf(stderr,"  You are in file extent <%"PRId32"> at byte <%"PRId32">.\n",
      extent_number,pos_in_extent);
      return CIO_ERROR;
    }
    return pos_in_extent; }

/****************************************************************************** 
 *** given a starting byte that tells where the fixed length records begin in
 *** a file, and a record length (in bytes), return the record number where
 *** the file is positioned. 1 = starting record (at position 0).
 ******************************************************************************/
int32_t cio_ftell_recd_c
   (INTEGER * unit, INTEGER * start_byte, INTEGER * record_length) {
    /*  Return the record number (starting record number = 1) */
    int64_t what_byte, local_recl;
    int32_t what_record;
    INTEGER  iptr, ifile;

    local_recl= *record_length;
    if((iptr = cio_get_iptr_c(unit)) < 0 ) return CIO_ERROR;

    /* IF FIFO don't do anything */
    if(S_ISFIFO(cio_file[iptr]->fdstat.st_mode)) return (int32_t)0;

    ifile = cio_file[iptr]->pfio_ifile;
    what_byte = pfio_tell(ifile) - (int64_t)*start_byte;
    what_record = 1 + what_byte / local_recl;
    return what_record;
}

/****************************************************************************** 
 *** Given an arbitrary blocksize for a file, return which block and byte the
 *** file is positioned on.
 ******************************************************************************/
int32_t cio_ftell_block_and_byte_c(
    INTEGER * unit, INTEGER * blocksize, INTEGER * whichblock, 
    INTEGER * whichbyte)
{
    int64_t pos;
    int64_t block,byte;
    INTEGER  iptr;
    INTEGER  ifile;
    INTEGER  extsize;

    if((iptr = cio_get_iptr_c(unit)) < 0 ) return CIO_ERROR;

    /* IF FIFO don't do anything */
    if(S_ISFIFO(cio_file[iptr]->fdstat.st_mode)) return (int32_t)0;

    ifile   = cio_file[iptr]->pfio_ifile;
    extsize = cio_get_fn_ext_size_c(cio_file[iptr]->name);

    if (*blocksize <= 0 ) return -1;
    pos = pfio_tell(ifile);
    block = (pos)/(*blocksize);
    byte = pos%(*blocksize);
    *whichblock = 1 * (INTEGER ) block;
    *whichbyte  = 1 * (INTEGER ) byte;
    if (*whichblock > extsize || *whichblock < 0 ) {
      fprintf(stderr,
      "cio_ftell_block_and_byte: *whichblock=%d bf_flsz=%d\n",*whichblock,
       extsize);
      return -1L;
    }
    return 0L;
}

/****************************************************************************** 
 *** given an arbitrary block size, seek to the particular block and byte in
 *** a file.
 ******************************************************************************/
int32_t cio_fseek_block_and_byte_c(
    INTEGER * unit, INTEGER * blocksize, INTEGER * whichblock, 
    INTEGER * whichbyte, INTEGER * origin)
{
    int64_t lloffset,block,byte,bsize;
    INTEGER  iptr;
    INTEGER  ifile;

    if((iptr = cio_get_iptr_c(unit)) < 0 ) return CIO_ERROR;

    /* IF FIFO don't do anything */
    if(S_ISFIFO(cio_file[iptr]->fdstat.st_mode)) return (int32_t)0;

    ifile = cio_file[iptr]->pfio_ifile;

    bsize = *blocksize;
    block = *whichblock;
    byte  = *whichbyte;
    lloffset = (block)*(bsize) + (byte);
    return (int32_t)pfio_seek_via_origin(ifile,lloffset,*origin);
}

/****************************************************************************** 
 *** mimic the fortran rewind command ***
 ******************************************************************************/
void cio_frewind_c(INTEGER * unit) {
    INTEGER  iptr;
    INTEGER  ifile;

    if((iptr = cio_get_iptr_c(unit)) < 0 ) {
      fprintf(stderr,"cio_frewind_c: cannot rewind unit %d - not open\n",
              *unit);
      return;
    }

    /* IF FIFO don't do anything */
    if(S_ISFIFO(cio_file[iptr]->fdstat.st_mode))return; 

    ifile = cio_file[iptr]->pfio_ifile;

    if(pfio_seek_via_origin(ifile,(int64_t)0,SEEK_SET) != 0 ) 
      fprintf(stderr,
      "cio_frewind_c: error seeking to beginning of file on unit %d\n",*unit);
}

/****************************************************************************** 
 *** mimic the fortran backspace command.
 ******************************************************************************/
void cio_fbackspace_c(INTEGER * unit) {
    int64_t last[2];
    int64_t onebyte = (int64_t)1;
    char  c;
    INTEGER  iptr;
    INTEGER  ifile;

    if((iptr = cio_get_iptr_c(unit)) < 0 ) {
      fprintf(stderr,"cio_fbackspace_c: cannot backspace unit %d - not open\n",
              *unit);
      return;
    }

    /* IF FIFO don't do anything */
    if(S_ISFIFO(cio_file[iptr]->fdstat.st_mode))return; 

    ifile = cio_file[iptr]->pfio_ifile;
    last[0] = (int64_t)0;
    last[1] = (int64_t)0;

    while (last[0] == (int64_t)0 && pfio_tell(ifile) > (int64_t)0) {
        if(pfio_seek_via_origin (ifile,-onebyte,SEEK_CUR) != 0)
          fprintf(stderr,"cio_fbackspace_c: error backspacing one char.\n");
        if(pfio_read(ifile,&c,1) != 1L)
          fprintf(stderr,"cio_fbackspace_c: error reading character\n");
        if(pfio_seek_via_origin (ifile,-onebyte,SEEK_CUR) != 0)
          fprintf(stderr,"cio_fbackspace_c: error backspacing one char.\n");
        if (c == '\n') {
            last[0] = last[1];
            last[1] = pfio_tell(ifile)+1;
        }
    }

    if(pfio_tell(ifile) > 0 ) {
      if(pfio_seek_via_origin(ifile,onebyte,SEEK_CUR) != 0 )
        fprintf(stderr,"cio_fbackspace_c: error spacing forward one char.\n");
    }
    return;
}

/****************************************************************************** 
 *** print out information about the file pointed to by iptr ***
 ******************************************************************************/
void  cio_info_c(INTEGER iptr) {
      if(cio_file[iptr] == NULL ) return;
      fprintf(stderr,"cio_info: Addr=%p", cio_file[iptr]);
      fprintf(stderr,"\tiptr=%d",iptr);
      fprintf(stderr,"\tifile=%d", cio_file[iptr]->pfio_ifile);
      fprintf(stderr,"\tunit=%d", cio_file[iptr]->unit);
      fprintf(stderr,"\tdispose=%d", cio_file[iptr]->dispose);
      fprintf(stderr,"\tstream=%p", cio_file[iptr]->stream);
      fprintf(stderr,"\tmode=%s", cio_file[iptr]->mode);
      fprintf(stderr,"\terror=%d", cio_file[iptr]->error);
      fprintf(stderr,"\teof=%d", cio_file[iptr]->eof);
      if(cio_file[iptr]->name != NULL ) 
        fprintf(stderr,"\tname=%s",cio_file[iptr]->name);
      fprintf(stderr,"\n");
      fflush(stderr);
}
/****************************************************************************** 
 *** is this a named pipe?                                   ***
 ******************************************************************************/
INTEGER cio_isfifo_c(INTEGER *unit) {
  int iptr;
  if((iptr = cio_get_iptr_c(unit)) < 0 ) { return CIO_ERROR; }
  if(S_ISFIFO(cio_file[iptr]->fdstat.st_mode)) return 1;
  return 0;   
}

/****************************************************************************** 
 *** clean up and free space held by file pointer iptr       ***
 ******************************************************************************/
void  cio_wipe_iptr_c(INTEGER iptr) {
    cio_tidy_iptr_c(iptr);
    free(cio_file[iptr]);
    cio_file[iptr]             = NULL;
}

/****************************************************************************** 
 *** clean up space held by file pointer iptr       ***
 ******************************************************************************/
void  cio_tidy_iptr_c(INTEGER iptr) {
    if(cio_file[iptr]->name != NULL) free(cio_file[iptr]->name);
    cio_file[iptr]->name       = NULL;
    cio_file[iptr]->mode[0]    = '\0'; 
    cio_file[iptr]->unit       =    0;
    cio_file[iptr]->stream     = NULL;
    cio_file[iptr]->error      =    0;
    cio_file[iptr]->eof        =    0;
    cio_file[iptr]->pfio_ifile =    0;
    cio_file[iptr]->dispose    =    0;
    cio_file[iptr]->fdstat.st_mode  = 0;
}

/****************************************************************************** 
 *** Provide fortran interface to pfio_checksum.     ***
 ******************************************************************************/
void cio_checksum_c(INTEGER *a, INTEGER *n, INTEGER *chksm)
{
   pfio_checksum(a, n, chksm);
}

/****************************************************************************** 
 *** Provide fortran interface to pfio_update_file_time.     ***
 ******************************************************************************/
void cio_update_file_time_c(char *file_name)
{
   pfio_update_file_time(file_name);
}

/****************************************************************************** 
 *** Provide fortran interface to pfio_write_message_file.     ***
 ******************************************************************************/
void cio_write_message_file_c(CHARACTER *file, CHARACTER *message)
{
   pfio_write_message_file((char*)file, (char*)message);
}

/****************************************************************************** 
 *** Provide fortran interface to pfio_get_file_info.     ***
 ******************************************************************************/
void cio_get_file_info_c(CHARACTER *f_in, CHARACTER *f_out, char *n_out) {
   pfio_get_file_info((char*)f_in, (char*)f_out, n_out);
   return;
}
/****************************************************************************** 
 *** Provide fortran interface to system     ***
 ******************************************************************************/
void cio_system_c(CHARACTER *command) {
   system(command);
   return;
}

/****************************************************************************** 
 *** Provide fortran interface to set O_DIRECT using pfio_add_desc_flags. ***
 ******************************************************************************/
INTEGER cio_set_direct_io_c(INTEGER *unit) {
    int iptr, ifile;
    struct utsname u;
  
    if (uname(&u) != 0) return CIO_ERROR;

    /* Use direct I/O (O_DIRECT) only for Linux kernel 2.6  and nfs files*/
    if (strncmp("2.6", u.release, 3) == 0 && strcmp(u.sysname,"Linux") == 0 ) {
      /* running linux 2.6*/
      if((iptr = cio_get_iptr_c(unit)) < 0 ) return CIO_ERROR;

      ifile = cio_file[iptr]->pfio_ifile;
      /**   printf("cio_set_direct_io_c: unit=%d, iptr=%d, ifile=%d\n",
        *unit, iptr, ifile); **/
      if(bfio_is_file_nfs(pfio_get_filename(ifile))==1) {
        /*Using a nfs file*/
        if (pfio_add_desc_flags(ifile, O_DIRECT) < 0) return CIO_ERROR;
      }
    }

    return CIO_OK;
}

/****************  Helper function for mimic routines *************************/

/****************************************************************************** 
 ******** convert C stream number to correct unit number  in cio_file vector
 ******** test to see if good unit number *****
 ******************************************************************************/
INTEGER cio_get_iptr_for_open_stream(FILE *stream) {
  INTEGER iptr;

  for (iptr=0;iptr<=fptr;iptr++){
    if (cio_file[iptr] != NULL ) {
      if(cio_file[iptr]->stream == stream) return iptr;
    }
  }
  return CIO_ERROR;
}

/***************** C Callable mimic routines **********************************/
void cio_setvbuf_mimic(FILE *stream, char *buf, int mode, size_t size) {
  /* set the buffer just like setvbuf does */
  /* but... we won't use his buffer or his mode    */
  int iptr = cio_get_iptr_for_open_stream(stream);
  if( iptr != CIO_ERROR ) {
    if( (int) pfio_setbufsz(iptr,(int32_t)size));
  }
}

FILE *cio_fopen_mimic(const char *path,const char *mode){
  INTEGER unit;
  INTEGER dispose=0;
  int iptr;
  unit = cio_fopen_c((char *) path, (char*) mode, &dispose);
  if((iptr = cio_get_iptr_c(&unit)) < 0 ) return (FILE *) NULL;
  /*printf("cio_fopen_mimic: debug call\n");*/
  /*cio_info_c(iptr);*/
  return cio_file[iptr]->stream; /* the stream number */
}

int cio_fclose_mimic(FILE *stream) {
  int iptr;
  INTEGER remove=0;
  /*printf("close\n");*/
  if((iptr = cio_get_iptr_for_open_stream(stream)) < 0 ) return CIO_ERROR;
  /*cio_info_c(iptr);*/
  return (cio_fclose_c(&cio_file[iptr]->unit,&remove));
}

int cio_fflush_mimic(FILE *stream) {
  int iptr;
  /*printf("flush\n");*/
  if((iptr = cio_get_iptr_for_open_stream(stream)) < 0 ) return CIO_ERROR;
  /*cio_info_c(iptr);*/
  return (pfio_flush(cio_file[iptr]->pfio_ifile));
}


void cio_rewind_mimic(FILE *stream) {
  int iptr;
  /*printf("rewind\n");*/
  if((iptr = cio_get_iptr_for_open_stream(stream)) < 0 ) return;
  /*cio_info_c(iptr);*/
  pfio_seek(cio_file[iptr]->pfio_ifile,(int64_t)0);
  return;
}

size_t cio_fread_mimic(void *ptr,size_t size, size_t nmemb, FILE *stream) {
  int      iptr;
  INTEGER  nbytes_read,ifile;
  size_t   nitems_read;
  int32_t  nbytes=size*nmemb;
  /*printf("fread\n");*/
  nitems_read=0;
  if((iptr = cio_get_iptr_for_open_stream(stream)) < 0 ) {
    return 0;
  }
  /*cio_info_c(iptr);*/
  ifile = cio_file[iptr]->pfio_ifile;
  nbytes_read = pfio_read(ifile, (char *) ptr, nbytes);
  nitems_read=nbytes_read/size;
  if(nitems_read < nmemb ) {
    /* Need to check w/ bfio for EOF or ERROR, set flags as appropriate. */
    cio_file[iptr]->eof=1;
  }
  return nitems_read;
}
  
size_t cio_fwrite_mimic(const void *ptr,size_t size, size_t nmemb,
  FILE *stream) {
  int      iptr;
  INTEGER  ifile;
  int32_t  nbytes_written;
  size_t   nitems_written;
  int32_t  nbytes=size*nmemb;
  /*printf("fwrite\n");*/
  nitems_written=0;
  if((iptr = cio_get_iptr_for_open_stream(stream)) < 0 ) {
    return 0;
  }
  /*cio_info_c(iptr);*/
  ifile = cio_file[iptr]->pfio_ifile;
  nbytes_written = pfio_write(ifile, (char *) ptr, nbytes);
  nitems_written=nbytes_written/size;
  if(nitems_written < nmemb ) {
    /* Need to check w/ bfio for EOF or ERROR, set flags as appropriate. */
    cio_file[iptr]->error=1;
  }
  return nitems_written;
}
  
/*** see fseeko below for off_t offset ***/
int cio_fseek_mimic(FILE * stream, int32_t offset, int whence) {
  int iptr;
  /*printf("fseek\n");*/
  if((iptr = cio_get_iptr_for_open_stream(stream)) < 0 ) 
    return (int) CIO_ERROR;
  /*cio_info_c(iptr)*/
  return pfio_seek_via_origin(cio_file[iptr]->pfio_ifile,offset ,whence);
}

/*** see ftello below for off_t return ***/
int32_t cio_ftell_mimic (FILE *stream) {
  int iptr;
  /*printf("ftell\n");*/
  if((iptr = cio_get_iptr_for_open_stream(stream)) < 0 ) 
    return (int64_t)CIO_ERROR;
  /*cio_info_c(iptr)*/
  return pfio_tell(cio_file[iptr]->pfio_ifile);
}

void cio_clearerr_mimic(FILE *stream) {
  int iptr;
  /*printf("clearerr\n");*/
  if((iptr = cio_get_iptr_for_open_stream(stream)) < 0 ) return;
  cio_file[iptr]->error=0;
  cio_file[iptr]->eof  =0;
  /*cio_info_c(iptr);*/
  return;
}

int cio_feof_mimic(FILE *stream) {
  int iptr;
  /*printf("feof\n");*/
  if((iptr = cio_get_iptr_for_open_stream(stream)) < 0 ) return CIO_ERROR;
  /*cio_info_c(iptr);*/
  return (int) cio_file[iptr]->eof;
}

int cio_ferror_mimic(FILE *stream) {
  int iptr;
  /*printf("ferror\n");*/
  if((iptr = cio_get_iptr_for_open_stream(stream)) < 0 ) return CIO_ERROR;
  /*cio_info_c(iptr);*/
  return (int) cio_file[iptr]->error;
}

int cio_fileno_mimic(FILE *stream) {
  int iptr;
  /*printf("fileno\n");*/
  if((iptr = cio_get_iptr_for_open_stream(stream)) < 0 ) return CIO_ERROR;
  /*cio_info_c(iptr);*/
  return (int) cio_file[iptr]->unit;
}

/* The below functions were added for 64 bit compatibility, please note
 * the following from the man page for fseeko:
 * On many architectures both off_t and long are 32#bit types, but
 * compilation with
 * #define _FILE_OFFSET_BITS 64
 *  will turn off_t into a 64#bit type.
 *
*/

/*** see above fseek for long offset ***/
int cio_fseeko_mimic(FILE *stream, off_t offset, int whence) {
  int iptr;
  /*printf("fseeko\n");*/
  if((iptr = cio_get_iptr_for_open_stream(stream)) < 0 ) 
    return (int) CIO_ERROR;
  /*cio_info_c(iptr)*/
  return pfio_seek_via_origin(cio_file[iptr]->pfio_ifile,
                              (int64_t)offset ,whence);
}

/*** see above ftell for long return ***/
off_t cio_ftello_mimic(FILE *stream) {
  int iptr;
  /*printf("ftello\n");*/
  if((iptr = cio_get_iptr_for_open_stream(stream)) < 0 ) 
    return (off_t) CIO_ERROR;
  /*cio_info_c(iptr)*/
  return (off_t) pfio_tell(cio_file[iptr]->pfio_ifile);
}

/*  The following functions were added to round out the set */

int cio_remove_mimic(const char *pathname){
  /*  Call the appropriate cio routine */
  return (int) cio_remove_c( (char *) pathname);
}

/****************** end c-callable mimic routines *****************************/

/****************************************************************************** 
 ********** END END END *********
 ******************************************************************************/
#ifdef __cplusplus
}
#endif
