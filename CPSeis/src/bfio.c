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
/*<CPS_v1 type="PRIMITIVE"/>
!----------------------------- bfio.c -------------------------------
!----------------------------- bfio.c -------------------------------
!----------------------------- bfio.c -------------------------------
!            other files are:  bfio.h mtio.h
!
!
!<brief_doc>
!-------------------------------------------------------------------------------
! Name       : bfio
! Category   : io
! Written    : 1999-09-15   by: Charles C. Burch
! Revised    : 2009-01-27   by: Bill Menger
! Maturity   : beta
! Purpose    : Provides basic file i/o functions.
! References : These routines are called from within pfio.c
!-------------------------------------------------------------------------------
!</brief_doc>
!
!<descript_doc>
!-------------------------------------------------------------------------------
! These are the IO primitives used by PFIO system
!     Multiple processes have a file opened simultaneously
!
!  int bfio_add_desc_flags(int ifile, int32_t flags)
!    adds flags to description flag for file associated with ifile
!
!  int bfio_check_if_directory (char *flnm) 
!    return 1 if flnm is a directory, 0 if not directory, -1 if nonexistent
!
!  int bfio_check_if_directory_empty( char *flnm)
!    returns 1 if directory flnm is empty, 0 if not  
!
!  int bfio_chmod(int ifile, int mode) Change permissions of a file.
!
!  int  bfio_close(int ifile) closes ifile
!
!  int bfio_delete_empty_directories(char *flnm, int pos)
!    deletes empty directories from flnm[1:pos]  
!
!  int bfio_delete_link_chain(char *fn)
!    deletes file fn and if fn is a link, deletes the file it links to, 
!    recursively until the last file in the link chain is deleted.
!
!  int  bfio_exit() wraps up bfio file and frees allocated memory
!
!  int bfio_expand_file_name(char *fn_in, char *fn_out, int fn_out_len) 
!    expands a filename
!
!  int32_t bfio_fetch(int ifile, int64_t offset, int32_t nbytes)
!    returns number of bytes fetched and inserts data into cache
!
!  int64_t bfio_file_size(char *filename) returns file size
!
!  int  bfio_flush(int ifile) flushes ifile's buffer
!
!  int  bfio_force_file_max_size(int ifile, int64_t size) 
!    makes filename max size
!
!  char* bfio_get_base_filename(char *flnm)
!    strips off any remote node name
!
!  int bfio_get_file_num_lines(char *file_name) 
!    returns the number of records in a file
!
!  int bfio_get_file_owner_name(char *flnm, char *owner, int owner_len)
!    returns owner of a file
!
!  time_t bfio_get_file_time(char *fn) 
!    returns the time stamp of a file
!
!  int bfio_get_str( FILE *fd, char *str, int n)
!    read a record from fd into str and replace control characters with blanks
!
!  int32_t bfio_gets(int ifile, int64_t offset, char *buff, int32_t nbytes)
!    returns bytes read, <0 means error
!
!  int bfio_is_file_nfs(char *filename)
!    return 1 if file is nfs file, 0 if not and <0 if error
!
!  int  bfio_open(char* filename) :returns file handle ifile
!
!  int32_t bfio_read(int ifile, int64_t offset, char *buff, int32_t nbytes)
!    returns bytes read, <0 means error
!
!  int  bfio_readlink_to_end(const char *path, char*buf, size_t bufsiz);
!       returns *buf with pointer to actual file the link points to that isn't
!       just another link, and returns length of *buf or -1 on error.
!
!  int  bfio_setbufsz(int ifile, int32_t bufsz) sets bufsz for ifile
!
!  int  bfio_set_file_region_lock(int ifile, int mode) locks file region
!
!  int32_t bfio_write(int ifile, int64_t offset, char *buff, int32_t nbytes)
!    return bytes written, <0 means error
!
!  void bfio_dir(char *dir, char *flags)
!    does a ls flags dir
!
! Support routines for file locking
!
!  void bfio_unlock_locked_files()
!  void bfio_dump_lock_file(char *lockfile)
!  char bfio_check_lock_file(char *lock_file, char *file_name_raw)
!  int  bfio_unlock_file(char *lock_file, char *file_name_raw)
!  int  bfio_lock_file(char *lock_file, char *file_name, 
!       int32_t timeout, char input_lock_type)
!  char bfio_try_locking_file(char *lock_file, char *file_name_raw, 
!       int32_t timeout, char lock_type)
!  int  bfio_change_lock_type(char *lock_file, char *file_name_raw,
!       char lock_type)
!  int  bfio_unlock_the_lockfile(int fd)
!  int  bfio_lock_the_lockfile(char *lock_file, char *file, char *lockfile)
!
!  Note: Although not tested, all the above routines should be thread-safe
!
!  Original version written June 1999 by Charles C Burch
!  New version written August 2000 by Charles C Burch
!
!-------------------------------------------------------------------------------
!</descript_doc>
!
!<calling_doc>
!-------------------------------------------------------------------------------
!    Do not call these routine--cps system internal use only
!-------------------------------------------------------------------------------
!</calling_doc>
!
!<history_doc>
!-------------------------------------------------------------------------------
!                     AUXILIARY FILE REVISION HISTORY
!     Date        Author      Description
!     ----        ------      -----------
! 33. 2009-01-27  Bill Menger Added bfio_chmod.
! 32. 2007-04-24  Bill Menger Modified remount_file_disk to properly handle the
!                             cases where directory names start with either
!                             "/" or "/." or have within their path something
!                             like /blah/2003.1.19.1/blah/blah in order to
!                             properly handle seisspace file names.
! 31. 2007-03-27  Kruger Corn Updated to 64 bit architecture. Basically
!                             changed long to int32_t and long long to
!                             int64_t.
! 30. 2006-11-14  Bill Menger Added bfio_readlink_to_end (Kruger Corn) and
!                             bfio_delete_link_chain.
! 29. 2006-04-04  Bill Menger Modified bfio_lock_the_lockfile and dump_lockfile
! 28. 2006-03-30  Brian Macy  Moved #include's out of extern "C" block.
! 27  2006-03-28  Bill Menger Removed print statement on cps_default_lock_info.
! 26  2005-12-05  Burch/Macy  Added add_desc_flags, get_fd, print_bfio_info &
!                             is_file_nfs.
! 25. 2005-09-01  Bill Menger Modified statvfs call .
! 24. 2005-07-11  Bill Menger Removed dependency on mth module, used standard
!                             random function instead.
! 23. 2005-04-26  Bill Menger Added mtio package.
! 22. 2005-03-24  Stoeckley   Modified a few lines so that this file can be
!                             compiled with C++.
! 21. 2005-01-11  Bill Menger Line 1694 bfio_gets: changed mutex_unlock to 
!                             mutex_lock.
! 20. 2004-08-26  Bill Menger Fixed the uname return value error in _initialize.
! 19. 2004-08-12  Bill Menger Added a getpid call to test for pid and the local
!                             host name before running a lock-check against a 
!                             process.
! 18. 2004-06-22  Bill Menger Changed some informative printout messages.
! 17. 2004-06-10  Bill Menger Added cpslog_message when lock/unlock files and
!                             took out logfile message for removing expired
!                             locks.
! 16  2004-01-21  RSDay       long long istat changed to long in 
!                             bfio_actual_write. bfio_force_file_max_size
!                             altered.
! 15. 2003-08-28  C. C. Burch Replaced use of FOPEN_MAX with sysconf call.
!                             Changed io activity logic for finding oldest io.
! 14. 2003-08-08  C. C. Burch Changes for SGI. Added bfio_dir diagnostic.
! 13  2003-05-27  C. C. Burch Added remount_file_disk, get_file_time,
!                             delete_empty_directories, get_file_owner_name,
!                             check_if_directory_empty, get_file_num_lines,
!                             check_if_directory, expand_file_name,
!                             get_base_filename, get_str.
!                             Made modifications file extents >2Gb.
!                             Made routines thread safe.
! 12  2002-07-23  C. C. Burch Retain /0's in gets. 
!                             Added get_disk_space from pfio.
! 11. 2002-06-18  C. C. Burch Added bfio_gets
! 10. 2002-03-27  C. C. Burch Added flush when going from read to write data.
!                             Added bfio_remove_file
!  9. 2001-12-10  C. C. Burch Number opened files determined from system.
!                             Eliminated limit on # files supportted.
!  8. 2001-04-30  C. C. Burch Added no buffering support
!  7. 2001-02-27  C. C. Burch Added file region locking.
!  6  2001-02-13  C. C. Burch Added functions.
!  5. 2000-10-20  C. C. Burch Added functions, restructured code,removed
!                             print on file open failure.
!  4. 2000-08-31  C. C. Burch modified for robust socket i/o
!  3. 2000-05-11  C. C. Burch changed if(is_struct->lastio[iext]='w') to ==.
!                             Added bfio_extsize function.
!  2. 2000-04-19  Bill Menger Commented out print statement when opening
!                             file that does not exist.
!  1. 2000-04-10  C. C. Burch Initial Version.
!-------------------------------------------------------------------------------
!</history_doc>
!
!<portability_doc>
!-------------------------------------------------------------------------------
!  On systems with 32-bit file systems use -DNOFSEEKO in the compile
!
!  sysconf is currently using _SC_OPEN_MAX where _SC_STREAM_MAX should be 
!    used.  _SC_STREAM_MAX is currently not correctly set for Houston as of 
!    8/2003.  When that is fixed, _SC_STREAM_MAX should be used.
!
! the #if in bfio_is_file_nfs may need to be changed for OS different than 
! Linux or Solaris
!-------------------------------------------------------------------------------
!</portability_doc>
!
*/
/*
!--------------------------- start of coding ------------------------------
!--------------------------- start of coding ------------------------------
!--------------------------- start of coding ------------------------------
*/

char *bfio_ident =
"$Id: bfio.c,v 1.32 2007/04/25 15:46:23 Menger beta sps $";

/*bfio.h must be first*/
#include "bfio.h"
#include "mtio.h"
#include "exptilde_crou.h"
#include <ctype.h>
#include <dirent.h>
#include <errno.h>
#include <fcntl.h>
#include <malloc.h>
#include <pthread.h>
#include <pwd.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <time.h>
#include <unistd.h>
#include <utime.h>
#include <sys/stat.h>
#include <sys/types.h>
#include <sys/utsname.h>  /* this is to allow the uname call */

#include <sys/mman.h>
#include <sys/syscall.h>

#include "cpslog.h"
#include "lnklst.h"
/*#include "mth.h"*/
#include "str.h"
#include "cnfg.h"
#include "unix.h"
#if (LINUX || LINUXA || LINUXI || LINUXP)
#include <sys/statfs.h>
#else
#include <sys/statvfs.h>
#endif

/*      Common variables and definitions for bfio system              */
/*      biggest 32-bit file size supportted LINUX is 2147482624       */

struct bfio_struct {
  int64_t flsz;          /*current file size of opened file           */
  int64_t curpos;        /*current offset                             */
  FILE  *fd;               /*fd of opened files                         */
  int32_t  setbufsz;          /* size of file buffer                       */
  int32_t  activity;          /*relative activity counter,-1closed,-2avail */
  int32_t  ntimes_opened;     /*number times given file currently opened   */
  int32_t  desc_flags;        /*description flags for opened files         */
  int   fileno;            /*fileno for opened file                     */
  char  *setbuf_ptr;       /*ptr to setbuf                              */
  char  fn[256];           /*file name of opened file                   */
  char  io_control[4];     /*io control                                 */
  char  lastio;            /*last io operation                          */
  char  region_lock;       /*lock file region during reads/writes       */
};

struct bfio_struct **bfio_info=NULL;   /*file info  struct              */
int    bfio_info_num=0;

int    number_opened_files=-1;    /*#fds opened,-1 means to initialize   */
int    bfio_server_sw=0;          /*set to 1 if server mode              */
int    bfio_open_max=16;          /* This  is changed later in bfio_init */
int32_t bfio_activity=0;           /*Used to age io operations            */
int32_t bfio_max_activity=10000000;/*max activity before normalizing      */
int32_t bfio_min_activity=25000;   /*min activity to normalize by         */

int    bfio_lock_mess_sw=0;       /*switch to only print lock error once */
char   bfio_error_string[280];    /*common sring for error status        */

int    bfio_default_lock_file_version=-1; /*set in bfio_init from cnfg   */

struct utsname thismachine;

/* the following was added by Tom Stoeckley 2005-03-22: */
#ifdef __cplusplus
extern "C" {
#endif

/******************** Internally called functions **********************/
int32_t bfio_actual_write        (int,  int64_t, char*, int32_t);
void bfio_close_file          (int);
int  bfio_dump_activity       (int, int);
int  bfio_dump_lock_file_rec  (struct bfio_lock_struct*, int);
int  bfio_get_file_num        ();
int  bfio_initialize          ();
int  bfio_lock_file_region    (int, int64_t, int32_t);
int  bfio_normalize_activity  ();
void bfio_open_file           (int);
int  bfio_open_read_file      (char*);
int  bfio_open_read_write_file(char*);
int  bfio_open_write_file     (char*);
void bfio_print_bfio_info     (char*, int);
int  bfio_unlock_file_region  (int, int64_t, int32_t);
int  bfio_update_activity     (int);

/*mutex lock for io subroutines*/
pthread_mutex_t bfio_io_mutex =PTHREAD_MUTEX_INITIALIZER;

/*mutex general lock for any specific subroutine*/
pthread_mutex_t bfio_sub_mutex=PTHREAD_MUTEX_INITIALIZER;

/*------------------------ start of functions ----------------------------*/
/*------------------------ start of functions ----------------------------*/
/*------------------------ start of functions ----------------------------*/

/*********************** General purpose io routines *************************/

/******************** BFIO_FORM_TMPNAM ***************************
* Form tmpnam
* 
* Written January 2003 by Charles C Burch
******************************************************************/
char* bfio_form_tmpnam(char *base) {
  static char work[260];
  char hostname[32];
  static int counter=0;
 
  unix_get_hostname_c(hostname,sizeof(hostname)); 
  pthread_mutex_lock(&bfio_sub_mutex);
  if(base!=NULL) {
    sprintf(work,"%s_%s_%d_%d",base, hostname,(int)getpid(),counter++);
  } else {   
    sprintf(work,"%s_%s_%d_%d","tmp",hostname,(int)getpid(),counter++);
  }
  pthread_mutex_unlock(&bfio_sub_mutex);
  return(work);
} 
 
/******************* BFIO_GET_BASE_FILENAME ***************
* Find pointer position of start of base file name
*   (that is without any explicit node name)
*
* Written by Charles C Burch  August 2000
**********************************************************/
char* bfio_get_base_filename(char *flnm) {
  int i;

  if(flnm[i=str_find_str(flnm,0,":")]!=':') {
    i=0;
  } else {
    i++;
  }
  return(flnm+i);
}

/******************** BFIO_EXPAND_FILE_NAME ***************
* expands file name fn_in to file name fn_out[fn_out_len]
*  return -1 if any errors
*  expands current working directory, ~/, ~user, ./ ../
*
* Written by Charles C Burch  August 2000
***********************************************************/
int bfio_expand_file_name(char *fn_in, char *fn_out, int fn_out_len) {
  int n_out, n_in, i, i_node, j, istat;
  char user_name[32];
  struct passwd *user_info;

  istat=0;
  if( (n_in=strlen(fn_in))>=fn_out_len) {
    istat=-1;
    strcpy(fn_out,"");
    goto EXIT;
  }

/*  Handle node name, if present */

  if((i_node=str_find_str(fn_in,0,":"))<n_in) {
    str_substr(fn_in,0,i_node,fn_out, fn_out_len);
    n_out=(++i_node);
  } else {
    i_node=n_out=0;
    strcpy(fn_out,"");
  }

  if(fn_in[i_node]=='/') {
/***          leading/ means explicit filename                    ***/
    strcat(fn_out,fn_in+i_node);
    n_out+=(n_in-i_node);

  } else if(fn_in[i_node]=='~') {
/***                expand user names directories                 ***/
    i=str_find_str(fn_in,i_node+1,"/"); /* ~/ is current user */
    if(i==(i_node+1) ) {
      strcpy(user_name,unix_user_name_c());
    } else {
      str_substr(fn_in,i_node+1,i-1,user_name,32);
    }
    user_info=getpwnam(user_name);
    n_out+=strlen(user_info->pw_dir)+n_in-i;
    if(n_out>=fn_out_len) return(-1);
    strcat(fn_out,user_info->pw_dir);
    strcat(fn_out,fn_in+i);

} else {
/***              expand using current working dir                ***/
    if( getcwd(fn_out+n_out, fn_out_len-n_out)==NULL) return(-1);
    if(fn_in[i_node]=='.' && fn_in[i_node+1]=='/')
     i_node+=2;         /*skip any leading ./  */
    n_out=strlen(fn_out)+n_in-i_node+1;
    if(n_out>= fn_out_len) return(-1);
    strcat(fn_out,"/");
    strcat(fn_out,fn_in+i_node);
  }

/*** printf("fn_out=%s, i_node=%d, n_out=%d, %d\n",
             fn_out, i_node, n_out, strlen(fn_out));   ***/

/***                      Now handle any ../                      ***/

  i=i_node;
  while( (i=str_find_str(fn_out,i,"../") )<n_out) {
/***             Handle any non/ followed by ../                  ***/
    if(i==i_node) {
      strcpy(fn_out+i_node,fn_out+i_node+3);
      n_out-=3;
      continue;
    }
    if(fn_out[i-1]!='/') {
      strcpy(fn_out+i,fn_out+i+2);
      n_out-=2;
      continue;
    }

/***                     have /../                               ***/

    fn_out[--i]='\0';            /*find and strip out previous dir */
    j=str_find_last_str(fn_out,0,"/");
    if(j>=i) j=i_node;
    strcpy(fn_out+j, fn_out+i+3);
    n_out-=(i+3-j);
    i=j;
  }

EXIT:
  return(istat);
}

/************* BFIO_GET_FILE_TIME ************
* Returns date file last modified (-1 if error)
*
* Written August 2000 by Charles C Burch
*********************************************/
time_t bfio_get_file_time(char *fn) {
  struct stat stbuf;
  char   full_fn[260];
  time_t result;
  int    istat;

/* get expanded file name */
  bfio_expand_file_name(fn, full_fn,sizeof(full_fn));
  fn=bfio_get_base_filename(full_fn);

  if((istat=stat(fn, &stbuf))<0) {
    bfio_remount_file_disk(fn);
    istat=stat(fn, &stbuf);
  } 
  
  if(istat<0) {
    result=-1;
  } else {
    result=stbuf.st_mtime;
  }
  return(result);
}

/**************** BFIO_GET_FILE_OWNER *********************
* obtains file owner name into owner,
*   owner_len=max size of owner
*  return 0 if successful or <0 if error
*
* Written by Charles C Burch  June 2001
***********************************************************/
int bfio_get_file_owner_name(char *flnm, char *owner, int owner_len) {
  struct stat stbuff;
  struct passwd *user_info;
  int istat;
  char full_fn[260];

  bfio_expand_file_name(flnm,full_fn,sizeof(full_fn));
  flnm=bfio_get_base_filename(full_fn);
  
  if((istat=stat(flnm, &stbuff))<0) {
    bfio_remount_file_disk(flnm);
    istat=stat(flnm, &stbuff);
  }  
  
  if(istat<0) {
    strcpy(owner,"");
    return(-1);
  }

  if((user_info=getpwuid(stbuff.st_uid))==NULL) {
    strcpy(owner,"");
    return(-1);
  }
  
  if(strlen(user_info->pw_name)>=owner_len) {
    strcpy(owner,"");
    return(-2);
  }

  strcpy(owner,user_info->pw_name);
  return(0);
}

/***************** BFIO_CHECK_IF_DIRECTORY ****************
* see if specified file is a directory
*  returns(1) if file is a directory,
*         -1  if file does not exist and
*          0  if file exist but is not a directory
*
* Written September 2000 by Charles C Burch
**********************************************************/
int bfio_check_if_directory (char *flnm) {
  struct stat stbuff;
  int istat;
  char full_fn[260];

  bfio_expand_file_name(flnm, full_fn, sizeof(full_fn));
  flnm=bfio_get_base_filename(full_fn);
  
  if((istat=stat(flnm, &stbuff))<0) {
    bfio_remount_file_disk(flnm);
    istat=stat(flnm, &stbuff);
  }  
  
  if(istat<0) {
    istat=-1;
  } else {  
    if(S_ISDIR(stbuff.st_mode)!=0) {
      istat=1;
    } else {  
      istat=0;
    }
  }  
  return(istat);
}

/*********** BFIO_CHECK_IF_DIRECTORY_EMPTY ************
* see if is directory and is empty
*  returns(1) if yes
*          -1 if not a directory
*           0 if directory and is nonempty
*           1 if directory and empty
*
* Written September 2000 by Charles C Burch
******************************************************/
int bfio_check_if_directory_empty(char *flnm) {
  DIR *cur_dir;
  struct dirent *dir_ent;
  int istat;
  char full_fn[260];

  bfio_expand_file_name(flnm, full_fn, sizeof(full_fn));
  flnm=bfio_get_base_filename(full_fn);
  istat=-1;
  if( (cur_dir=opendir(flnm))!=NULL) {
    istat=1;
    while( (dir_ent=readdir(cur_dir))!=NULL) {
      if(strcmp(dir_ent->d_name,"." )!=0 &&
         strcmp(dir_ent->d_name,"..")!=0) {
        istat=0;
        break;
      }        
    }
    closedir(cur_dir);
  }
  return(istat);
}

/*************** BFIO_DELETE_EMPTY_DIRECTORIES ************
* delete empty directories in flnm ending at pos
*  returns # directories deleted
*
* Written September 2000 by Charles C Burch
**********************************************************/
int bfio_delete_empty_directories(char *flnm, int pos) {
  char dir[260], file_owner[260];
  int i, n;

  n=0;
  strcpy(dir,flnm);
  while(1) {
    i=str_find_last_str(dir, pos, "/");
    if(dir[i]=='\0') break;

    dir[i]='\0';
    /***  printf("dir=%s\n",dir);  ***/
    if(bfio_check_if_directory_empty(dir)!=1) break;
    if(bfio_get_file_owner_name(dir, file_owner, sizeof(file_owner))<0)
      break;
    if(strcmp(unix_user_name_c(),file_owner)!=0) break;
    if(rmdir(dir)!=0) break;

    /***  printf("dir removed\n");  ***/
    n++;
  }
  return(n);
}

/***************** BFIO_ENSURE_DIR_EXISTS ******************
* Ensure directory exists
*
* Written August 2000 by Charles C Burch
**********************************************************/
int bfio_ensure_dir_exists(char *fn, int pos) {
  int    j, fn_len, istat, icheck;
  char   dir[260], work[260];
  mode_t mode;

  fn_len=strlen(fn);
  istat=0;
  j=str_find_str(fn,pos,"/");
  if(j==0) j=str_find_str(fn,j+1,"/");

  while(j<fn_len) {
    str_substr(fn,0,j-1,dir,260);
    icheck=bfio_check_if_directory(dir);
    if(icheck==0) {
      sprintf(work,
       "Warning:New directory(%s) exist as a file-file renamed as%s.file",
       dir,dir);
      printf("%s\n",work);
      cpslog_message(work);
      strcpy(work,dir);
      strcat(work,".file");
      remove(work);
      rename(dir,work);
      icheck=-1;
    }
    
    if(icheck<0) {
/*    printf("making directory %s\n",dir);*/
      mode=bfio_rwx_to_chmod("OrwxGrwxArx");
      if(mkdir(dir,mode)<0) {
        if(bfio_check_if_directory(dir)<=0) {
          printf("mkdir on (%s)failed in bfio_ensure_dir_exists\n", dir);
          perror("  mkdir");
          istat=-1;
          goto EXIT;
        }
      }
      chmod(dir, mode);
      istat++;
    }
    j=str_find_str(fn,j+1,"/");
  }
  
EXIT:
  return(istat);
}

/***************** BFIO_RWX_TO_CHMOD **************************
* convert chmod string to int value
*     String subset of A:rwx, G:rwx, O:rwx
*      : , and blank are optional
*      A-anyone, G-Group, O-owner, r-read, w-write, x-execute
*      No A G and O means to set each to specified value
*
* Written May 2000 by Charles C Burch
**************************************************************/
int bfio_rwx_to_chmod(char *rwx) {
  int mode, multiplier;

  mode=0;
  multiplier=64+8+1;
  while( 1) {
    switch ((int) (*rwx++) ) {
    case 'r':               /* Read */
    case 'R':
      mode += 4*multiplier;
      break;
    case 'w':              /* Write */
    case 'W':
      mode += 2*multiplier;
      break;
    case 'x':             /*Execute */
    case 'X':
      mode += multiplier;
      break;
    case 'a':             /* Anyone */
    case 'A':
      multiplier=1;
      break;
    case 'g':              /* Group */
    case 'G':
      multiplier=8;
      break;
    case 'o':              /* Owner*/
    case 'O':
      multiplier=64;
      break;
    case ',':
    case ':':
    case ' ':
      break;
    case 0:
      return(mode);
    }
  }
}

/****************** BFIO_GET_FILE_NUM_LINES ******************
* get number of lines of data in a file
*
* Written by Charles C Burch March 20001
*************************************************************/
int bfio_get_file_num_lines(char *file_name) {
  FILE *fd;
  char str[1024];
  int n;

  n=0;
  if ( (fd=mtio_fopen(file_name,"r"))==NULL) return(0);

  while(1) {
    if(mtio_fgets(str, sizeof(str), fd)==NULL) break;
    n++;
  }
  mtio_fclose(fd);
  return(n);
}

/**************** BFIO_GET_STR *******************************
* get line of data from a file removing control characters
*
* Written by Charles C Burch July 2000
*************************************************************/
int bfio_get_str( FILE *fd, char *str, int n) {
  int i, istat;
  char *buf;

  buf=mtio_fgets(str,n,fd);
  if(buf == NULL) {
    str[0]='\0';
    istat=-1;
  } else {
    istat=n;
    for (i=0; i<n; i++) {
      if(str[i]=='\0') {
        istat=i;
        break;
      }
      if(iscntrl((int)str[i]) != 0) str[i]=' ';
    };
  }
  if(buf != NULL && buf != str ) free(buf);
  return(istat);
}

/***************** BFIO_TOUCH_A_FILE ****************
* A basic file touch to try to reset its file time
*  to invalid NFS cache information.
* This can be done using pfio_update_file_time,
* but it used pfio_rsh_timeout which also needs the 
* same functionality so we use this routine.
*
* Written March 2002 by Charles C Burch
*****************************************************/
void bfio_touch_a_file(char *host, char *fn) {
  char cmd[512], full_fn[260];

  bfio_expand_file_name(fn, full_fn,260);
  fn=bfio_get_base_filename(full_fn);

  if(bfio_get_file_time(fn)==time(NULL)) sleep(1);
  sprintf(cmd, "rsh %s -n %s %s >/dev/null 2>/dev/null", host,"\\touch", fn);
  system(cmd);
  return;
}

/***************************************************************
*       Determines size of a generic file
*       fn is character string of file name to find its length
*
*       Written August 2000 by Charles C Burch
***************************************************************/
int64_t bfio_file_size(char *fn){
  return (int64_t) mtio_flsz(fn);
}

/**************************** BFIO_GET_DISK_SPACE ******************************
* Get available space on a given disk(dev)
*
* Note:bfio_statfs & bfio_statfs_struct defined differently for Linux & Solaris
*
* Written August 2000 by Charles C Burch
*******************************************************************************/
int64_t bfio_get_disk_space(char *dev){
  int stat, n;
  int64_t temp;
  struct bfio_statfs_struct f_statfs;
  char device[370];
  memset(device,0,370);
  n=bfio_readlink_to_end(dev, device, sizeof(device));
  if(n<=0) {
    strcpy(device, dev); 
  } else {
    device[n]='\0';
  }

  n=strlen(device);
  while(n>0) {
    if(device[n-1]!='/' && device[n-1]!='.') break;
    device[--n]='\0';
  }

  strcat(device,"/.");
  /*fprintf(stderr,"bfio_get_disk_space:%d: device=%s\n",__LINE__,device);*/

  stat=bfio_statfs(device, &f_statfs);
  if(stat<0) {
    /*retry in case of disk auto-dismount*/
    stat=bfio_statfs(device, &f_statfs);
    if(stat<0) {
      sleep(1);
      /*retry in case of disk auto-dismount*/
      stat=bfio_statfs(device, &f_statfs);
    }
  }  

  if(stat<0) {
    /*  perror("bfio_get_disk_space");  */
    temp=-1;
  } else {

    /*different machines do this differently-
        this may need to changed to something more robust*/
#if(SOLARIS)
    /* For Solaris */
    temp=512;
#else
    /* for Linux, cgywin and SGI */
    temp=(int64_t) f_statfs.f_bsize;
#endif

    temp*=((int64_t) f_statfs.f_bavail);
    /***fprintf(stderr,"device=%s dev=%s mount=%"PRId32" space=%"PRId64", %"PRId64", %"PRId64"\n",
        device,dev,(uint32_t) f_statfs.f_flag,
        (int64_t)f_statfs.f_bsize, (int64_t)f_statfs.f_bavail, temp);***/
  }
  return(temp);
}

/******************** BFIO_REMOUNT_FILE_DISK ******************************
* Remount disk where given file exist 
* IT is assumed file actually has a file name or ends with /
*
* Written September 2002 by Charles C Burch
***********************************************************************/
int64_t bfio_remount_file_disk(char *file){
  int  i;
  char fl[370];

  strcpy(fl,file);
  i=str_find_last_str(fl, 0, "/");  
  if (i > 1) fl[i]='\0';
  return(bfio_get_disk_space(fl));
}

/***************************************************************
*       Remove a known existing file and print any error info
*
*       Written March 2002 by Charles C Burch
***************************************************************/
int bfio_remove_file(char *fn) {
  int istat;

  istat=remove(fn);
  if(istat<0) {
    printf("Unable to remove file(%s) in bfio_remove [%d:%s]\n",
      fn,errno,strerror(errno));
  }
  return(istat);
}

/****************** Support routines for bfio user routines *******************/

/**************** BFIO_PRINT_BFIO_INFO **************
* prints contents of ifile index of bfio_info
*
* Written November 2005 by Chuck Burch
*******************************************************/
void bfio_print_bfio_info(char *rtn, int ifile) {
  if(ifile>=0 && ifile<bfio_info_num) {
    printf("%s: flsz          = %"PRId64"\n",  rtn, bfio_info[ifile]->flsz);
    printf("%s: curpos        = %"PRId64"\n",  rtn, bfio_info[ifile]->curpos);
    printf("%s: fd            = %p\n",   rtn, bfio_info[ifile]->fd);
    printf("%s: setbufsz      = %"PRId32"\n",  rtn, bfio_info[ifile]->setbufsz);
    printf("%s: activity      = %"PRId32"\n",  rtn, bfio_info[ifile]->activity);
    printf("%s: ntimes_opened = %"PRId32"\n",  rtn, bfio_info[ifile]->ntimes_opened);
    printf("%s: setbuf_ptr    = %p\n",   rtn, bfio_info[ifile]->setbuf_ptr);
    printf("%s: fn            = %s\n",   rtn, bfio_info[ifile]->fn);
    printf("%s: io_control    = %s\n",   rtn, bfio_info[ifile]->io_control);
    printf("%s: lastio        = %c\n",   rtn, bfio_info[ifile]->lastio);
    printf("%s: region_lock   = %c\n",   rtn, bfio_info[ifile]->region_lock);
  } else {
    printf("%s:bfio_info index(%d) is invalid\n", rtn,ifile);
  }
  return;
}

/******************************************************
* set bfio_server_mode_sw
*
* Written August 2000 by Charles C Burch
*******************************************************/
void bfio_set_server_sw(int sw) {
  bfio_server_sw=sw;
  return;
}

/***********************************************************
*                  Internal pf use only
*       Utility routine to initialize control arrays
*
*       Written August 2000 by Charles C Burch
***********************************************************/
int bfio_initialize() {
  char *s;
  int i, num_threads=3, queue_size=64;

/*printf("bfio_initialize: number_opened=%d, info_num=%d\n",
   number_opened_files, bfio_info_num); */

  if(number_opened_files>=0) return(0);

  pthread_mutex_lock(&bfio_sub_mutex);

  if(number_opened_files<0) {
    number_opened_files=0;
    bfio_info_num=0;

/* Note: _SC_STREAM_MAX should be used but as of 8/2003, it is not 
         correct in Houston */
    bfio_open_max=sysconf(_SC_OPEN_MAX);
    bfio_open_max=bfio_open_max-bfio_open_max/8;
  
    if((s=cnfg_get_value_c("cps_default_lock_file_ver"))==NULL) s="";
    if(s[0]=='\0') {
      /*
      printf(
"Warning cps_default_lock_file_ver not defined in cnfg file-default used\n");
      */
      bfio_default_lock_file_version=0;
    } else {  
      bfio_default_lock_file_version=str_atoi(s);
    }
  }

  /*  This gets machine information from unix system */

  if((i=uname(&thismachine)) < 0 ){
    fprintf(stderr,
    "bfio_initialize: Error calling uname, %d returned. aborting.\n",i);
    return (-1);
  }
  
  /*** Open the multi-threaded i/o package now ***/
  if(mtio_init(&num_threads,&queue_size) != 0){
    fprintf(stderr,
    "bfio_initialize: unable to initialize mtio, not using mtio\n");
  }

  pthread_mutex_unlock(&bfio_sub_mutex);
  /*** No need to keep printing this message **********************
  fprintf(stderr,"bfio: largest file size for this executable=%"PRId64" bytes.\n",
  (int64_t) BF_FLSZ);
  ****************************************************************/
  return(0);
}

/***************************************************************
*                  Internal pf use only
* Utility routine to  wrap up bfio and deallocate control arrays
*
*       Written November 2001 by Charles C Burch
***************************************************************/
int bfio_exit() {
  int i;

/*printf("bfio_exit: number_opened=%d, info_num=%d\n",
   number_opened_files, bfio_info_num); */

  if(number_opened_files<0) return(0);

  pthread_mutex_lock(&bfio_sub_mutex);
  for(i=0;i<bfio_info_num;i++) {
    if(bfio_info[i]->fd!=NULL) mtio_fclose(bfio_info[i]->fd);
    free(bfio_info[i]);
  }
  if(bfio_info!=NULL) free(bfio_info);
  bfio_info=NULL;
  bfio_info_num=0;
  number_opened_files=-1;
  mtio_exit();                          /* exit the mtio package */ 
  pthread_mutex_unlock(&bfio_sub_mutex);
  return(0);
}

/******************** BFIO_ZAP_INFO ***********************
* zap information in ith index of bfio_info
*
* Written November 2005 by Chuck Burch
*********************************************************/
void bfio_zap_info(int i) {
  if(i>=0 && i<bfio_info_num) {
    bfio_info[i]->flsz=0;
    bfio_info[i]->ntimes_opened=0;
    bfio_info[i]->desc_flags=0;
    bfio_info[i]->fileno=-1;
    bfio_info[i]->fd=NULL;
    bfio_info[i]->setbufsz=0;
    bfio_info[i]->setbuf_ptr=NULL;
    bfio_info[i]->curpos=0;
    bfio_info[i]->activity=-2;
    strcpy(bfio_info[i]->fn,"");
    strcpy(bfio_info[i]->io_control,"");
    bfio_info[i]->lastio=' ';
    bfio_info[i]->region_lock=0;
  }
  return;
}

/******************************************************
* get an available ifile, expand bfio_info if needed.
*
* Written August 2000 by Charles C Burch
******************************************************/
int bfio_get_file_num() {
  int i, n, ifile;

  bfio_initialize();

  for(i=0;i<bfio_info_num; i++) {
    if(bfio_info[i]->activity<(-1)) {
      bfio_zap_info(i);
      bfio_info[i]->activity=-1;
      return(i);
    }
  }

  ifile=bfio_info_num;
  n=5;
  bfio_info_num+=n;
  if( (bfio_info=(struct bfio_struct**)
    realloc(bfio_info,bfio_info_num*sizeof(struct bfio_struct*))) == NULL) {
    fprintf(stderr,"Unable to realloc bfio_info[%d] in bfio_get_num [%d:%s]\n",
      bfio_info_num, errno,strerror(errno));
    exit(1);
  }

  for (i=0; i<n;i++) {
      if((bfio_info[ifile+i]=
         (struct bfio_struct*)malloc(sizeof(struct bfio_struct)))==NULL) {
        fprintf(stderr,"Unable to malloc bfio_struct in bfio_get_num [%d:%s]\n",
            errno,strerror(errno));
        exit(1);
      }
      bfio_zap_info(ifile+i);
  }

  bfio_info[ifile]->activity=-1;
  return(ifile);
}

/*****************************************************************
* Dump the activity status of each file--for diagnostic purposes 
* The paramters if >0 override bfio_open_max and bfio_max activity
* for testing purposes to force files to be opened/closed when 
* maximum number of files are active and to force periodic activity 
* normalization.
*
* Written August 2003 by Charles C Burch
******************************************************************/
int bfio_dump_activity(int max_opened, int max_activity) {
  int i;
  int32_t activity;

  /*Allows overriding  max # files opened for testing purposes*/
  if(max_opened>0) {
    if(bfio_open_max!=max_opened) {
      printf("Warning bfio_open_max(%d) being overrode with %d\n",
       bfio_open_max, max_opened);
    }
    bfio_open_max=max_opened;
  }

  /*Allows overriding  max activity for testing purposes*/
  if(max_activity>0) {
    if(bfio_max_activity!=max_activity) {
      printf("Warning bfio_max_activity(%"PRId32") being overrode with %d\n",
       bfio_max_activity, max_activity);
    }
    bfio_max_activity=max_activity;
    bfio_min_activity=max_activity/4;
  }

  /* Print status of each bfio file slot */
  for(i=0;i<bfio_info_num;i++) {
    activity=bfio_info[i]->activity;
    if(activity<(-1)) {
      printf("bfio file#%d is currently inactive\n",i);
    } else if(activity==(-1)) {
      printf("bfio file#%d is currently active, but not opened\n",i);
    } else {
      printf("bfio file#%d is currently opened, age=%"PRId32"\n",i, activity);
    }
  }
  return(0);
}

/************************************************************************
* Update the age of the last io activity of a file activity. 
* activity indicates the relative age of the last io operation of a file. 
* Larger numbers mean more recent and small numbers mean older.  
* activity of -2 means the file slot is not active and available
* to be used for a new file.
* activity of -1 means the file slot is being used but the
* file is currently not opened (in fopen sense) and needs to
* be opened before any io is attempted on the file.
* When activity gets past a certain size(bfio_max_activity),
* the activities are normalize(shifted down by the smallest activity
* thresholded by bfio_min activity) so the activity number get smaller
* to avoid possible overflow with the activity numbers.
*
* Written August 2003 by Charles C Burch
*************************************************************************/
int bfio_update_activity(int ifile) {
  /*bfio_activity has activity of last io that happened with any file*/
  if((bfio_info[ifile]->activity=++bfio_activity)>=bfio_max_activity) {
    bfio_normalize_activity(); /*normalize if activity gets too large*/
  }
  return(0);
}

/*********************************************************************
* Normalize activities and get index of min activity which tells which
* file slot where the oldest io operation occurred.
* Note bfio_info->activity =-2 means inactive
*                           -1 means file not actually opened
*                          >0  means file opened and is age of last io
*
* Written August 2000 by Charles C Burch
***********************************************************************/
int bfio_normalize_activity() {
  int i;
  int32_t min_activity, max_activity, i_min_activity = 0, activity;

/**  printf("bfio_normalize_activity\n"); **/
  max_activity=-1;
  min_activity=bfio_max_activity;
  for(i=0;i<bfio_info_num;i++) {
    if((activity=bfio_info[i]->activity)>=0) {
      if(activity>max_activity) max_activity=activity;
      if(activity<min_activity) {
        i_min_activity=i;
        min_activity=activity;
      }
    }
  }
/** printf("min/max activity=%"PRId32",%"PRId32"\n",min_activity, max_activity); **/
  if(max_activity<0) return(-1);

  /*normalize if max activity>bfio_max_activity or 
                 min activity>bfio_min_activity  */
  if(max_activity>bfio_max_activity  && min_activity<bfio_min_activity)
    min_activity=bfio_min_activity;
  if(min_activity<bfio_min_activity) return(i_min_activity);

  /*Note: reduce all activities by at least bfio_min_activity
    This might cause multiple files to have an activity of zero and
    the relative age of this files get lost, but the thinking is all
    of them are old and any could be consider equivalent to having
    the oldest io. This is done to cut down the number of times
    normalization happens.  */

  for(i=0;i<bfio_info_num;i++) {
    /* skip normalizing files not opened or already with 0 activity*/
    if((activity=bfio_info[i]->activity)>0) { 
      if((activity-=min_activity)<0) activity=0;
      bfio_info[i]->activity=activity;
    }
  }
  if((bfio_activity-=min_activity)<0) bfio_activity=0;
  return(i_min_activity);
}

/***********************************************************
*                  Internal bfio use only
*       close given ifile and do needed book keeping
*
*       Written August 2000 by Charles C Burch
***********************************************************/
void bfio_close_file(int ifile) {

/** printf("bfio_close_file(%d)\n",ifile); **/
  if(bfio_info[ifile]->fd!=NULL) {
    mtio_fclose(bfio_info[ifile]->fd);
    bfio_info[ifile]->fd=NULL;
    bfio_info[ifile]->fileno=-1;
    number_opened_files--;
  }
  bfio_info[ifile]->curpos=0;
  bfio_info[ifile]->lastio='c';
  bfio_info[ifile]->activity=-1;     /* indicate not opened*/
  return;
}

/***********************************************************
*                  Internal bfio use only
*       open given ifile and do needed book keeping
*
*       Written August 2000 by Charles C Burch
***********************************************************/
void bfio_open_file(int ifile){
  FILE *fd;
  int i_min_activity, istat;
  int32_t fdflags;

/** printf("opening ifile=%d\n", ifile);  **/
  if(bfio_info[ifile]->fd!=NULL) return;

/* not opened-open it--see if needed to close a file */
  if(number_opened_files>=bfio_open_max) {
    i_min_activity=bfio_normalize_activity();
    if(i_min_activity>=0) bfio_close_file(i_min_activity);
  }

/* Now open it */
  number_opened_files++;
  fd=mtio_fopen(bfio_info[ifile]->fn,bfio_info[ifile]->io_control);
  bfio_info[ifile]->fd=fd;
  if(fd==NULL) return;

  bfio_info[ifile]->fileno=mtio_fileno(fd);
  bfio_update_activity(ifile); /*sets bfio_info->activity to indicate open*/
  bfio_info[ifile]->curpos=0;
  bfio_info[ifile]->lastio='o';
  if(bfio_info[ifile]->setbufsz>0) {
    if(bfio_info[ifile]->setbuf_ptr==NULL) {
      bfio_info[ifile]->setbuf_ptr=
       (char *) malloc(bfio_info[ifile]->setbufsz);
    }
    if( bfio_info[ifile]->setbuf_ptr!=NULL) {
      setvbuf(fd,bfio_info[ifile]->setbuf_ptr,
       _IOFBF,bfio_info[ifile]->setbufsz);
    }
  } else if(bfio_info[ifile]->setbufsz<0) {
    setvbuf(fd,NULL, _IONBF,0);  /* no buffering */
  }

  if(bfio_info[ifile]->desc_flags!=0) {
    fdflags = (int32_t) mtio_fcntl(bfio_info[ifile]->fileno, F_GETFL);
    if (fdflags == -1) {
      fprintf(stderr,"bfio_open_file(%s): get flags: ErrNo=%d [%s]\n",
       bfio_info[ifile]->fn, errno, strerror(errno));
    } else {
      istat = mtio_fcntl(bfio_info[ifile]->fileno, F_SETFL, 
        fdflags | bfio_info[ifile]->desc_flags);
      if (istat == -1) {
        fprintf(stderr,"bfio_open_file(%s): set flags: ErrNo=%d [%s]\n",
         bfio_info[ifile]->fn, errno, strerror(errno));
      }
    }
  }

  return;
}

/****************************************************
*  Sets file region locks on(mode=1) or off(mode=0)
*
*  Written February 2001 by Charles C Burch
****************************************************/
int bfio_set_file_region_lock(int ifile, int mode) {

/*** printf("bfio_set_file_region_lock, ifile=%d, mode=%d\n",ifile, mode); ***/

  if(ifile<0 || ifile>=bfio_info_num) {
    printf("Attempt to use invalid file id (%d) in bfio_set_file_region_lock\n",
      ifile);
    return(-1);
  }

  if(bfio_info[ifile]->activity<(-1)) {
    printf(
     "Attempt to set file region lock to unopened file (%d) in %s\n",
     ifile, "bfio_set_file_region_lock");
    return(-2);
  }
  if(mode!=0) mode=1;
  bfio_info[ifile]->region_lock=mode;
  return(0);
}

/****************************************************
*  Locks a file region(start:len)
*    returns 0 if successful, -1 if not
*
*  Written February 2001 by Charles C Burch
****************************************************/
int bfio_lock_file_region(int ifile, int64_t start, int32_t len) {
  int32_t count, check;
  struct flock lock_region;
  char string[260];

  strcpy(string,"");
  sprintf(string,
  "bfio lock, ifile=%d, start=%"PRId64", len=%"PRId32"\n",ifile,start,len);
  cpslog_message(string);

  lock_region.l_type   =F_WRLCK;      /*set up the file lock*/
  if(strcmp(bfio_info[ifile]->io_control,"rb")==0) lock_region.l_type=F_RDLCK;
  lock_region.l_whence =SEEK_SET;
  lock_region.l_start  =start;
  lock_region.l_len    =len;

  count=0;
  check=60;

  while(1) {
    if(mtio_fcntl(bfio_info[ifile]->fileno,F_SETLK, &lock_region)>=0) {  
      /*try and test the lock setting*/
      if(count>=60)                  /*print message if wait was > 15 mins*/
        printf(" %s region file lock became available after %"PRId32" seconds\n",
               bfio_info[ifile]->fn,count);
      return(0);         /*Lock successful*/
    }
/*** lock did not work-close the lockfile, print periodic messages and wait***/
/*** printf("lockfile locked...waiting, %s\n",file_name); ***/
    if( (++count)==check) {
      fprintf(stderr,"Waiting for region file lock on file %s\n",
              bfio_info[ifile]->fn);
      fprintf(stderr,"  waited %"PRId32" seconds so far.  ErrNo=%d [%s]\n",count,
      errno,strerror(errno));
      if(check==60) {
        check=900;
      } else if(check==900) {
        check=1800;
      } else if(check==1800) {
        check=3600;
      } else {
        check=check+3600;
      }
    }
    sleep(1);
  }
}

/****************************************************
*  Unlocks a file region lock
*    returns 0 if successful, -1 if not
*
*  Written February 2001 by Charles C Burch
****************************************************/
int bfio_unlock_file_region(int ifile, int64_t start, int32_t len) {
  struct flock lock_region;
  char string[260];

  strcpy(string,"");
  sprintf(string,"unlock_file_region, ifile=%d, start=%"PRId64", len=%"PRId32"\n",
       ifile, start, len);
  cpslog_message(string);

/*set up the file lock*/
  lock_region.l_type   =F_UNLCK;
  lock_region.l_whence =SEEK_SET;
  lock_region.l_start  =start;
  lock_region.l_len    =len;

  if(mtio_fcntl(bfio_info[ifile]->fileno,F_SETLK,&lock_region) < 0 ) {
    fprintf(stderr,"bfio_unlock_file_region(%s:%"PRId64":%"PRId32"): ErrNo=%d [%s]\n",
       bfio_info[ifile]->fn,start, len, errno, strerror(errno));
  }
  return(1);
}
/***********************************************************
*       Open a bf file for reading--fn is the base file name
*
*       Written August 2000 by Charles C Burch
***********************************************************/
int bfio_open_read_file(char *fn) {
  int64_t f_size;
  int ifile;

/*printf("bfio_open_read, fn=%s\n",fn);  */

/*see if already opened                                   */
  for (ifile=0; ifile<bfio_info_num; ifile++) {
    if(bfio_info[ifile]->activity >=(-1)){
/*    processing opened file-to see if same as fn             */
      if(strcmp(fn,bfio_info[ifile]->fn) == 0) {
/*      file already opened-adjust book keeping & return         */
        bfio_info[ifile]->ntimes_opened++;
        if(strcmp(bfio_info[ifile]->io_control,"wb")==0) {
          strcpy(bfio_info[ifile]->io_control,"r+b");
          bfio_close_file(ifile);
        }
        return(ifile);
      }
    }
  }

/*file not already opened--now open it*/

  f_size=bfio_file_size(fn);
/*printf("open, flsz=%"PRId32"\n",f_size); */
  if(f_size<0) {
    /*printf("bfio_open file(%s) opened as old does not exist\n", fn); */
    return(-1);
  }

  if( (ifile=bfio_get_file_num())<0) {
    printf("Number of opened files exceeds limit in bfio_open_read_file\n");
    return(-1);
  }

  bfio_info[ifile]->flsz = f_size;
  strcpy(bfio_info[ifile]->io_control, "rb");
  strcpy(bfio_info[ifile]->fn,fn);
  bfio_info[ifile]->ntimes_opened = 1;
  bfio_info[ifile]->setbufsz=0;
  bfio_info[ifile]->setbuf_ptr=NULL;
  bfio_info[ifile]->fd=NULL;
  bfio_info[ifile]->curpos=0;
  bfio_info[ifile]->lastio='o';
  bfio_info[ifile]->setbuf_ptr=NULL;
  bfio_info[ifile]->activity=-1; /*indicate not yet actually opened*/
  return(ifile);
}

/***********************************************************
*       Open a file-fn for read/write
*
*       Written August 2000 by Charles C Burch
***********************************************************/
int bfio_open_read_write_file(char *fn) {
  int ifile;
  FILE *fd;

/* create null file, if file does not exist */
  if(bfio_file_size(fn)<0) {
    if( (fd=mtio_fopen(fn,"wb")) !=NULL) {
      mtio_fclose(fd);
    } else {
      printf("Unable to create file(%s) in bfio_open_read_write_file\n",fn);
      return(-1);
    }
  }
  ifile=bfio_open_read_file(fn);
  if(ifile>=0) strcpy(bfio_info[ifile]->io_control,"r+b");
  return(ifile);
}

/**************************************************************
*       Opens a bf for writing--fn is the base file name
*
*       Written August 2000 by Charles C Burch
**************************************************************/
int bfio_open_write_file(char *fn) {
  int ifile;
  FILE *fd;

/*see if already opened                                    */
  for (ifile=0; ifile<bfio_info_num; ifile++) {
    if(bfio_info[ifile]->activity>(-2) ){
/*    processing an opened file-see if same as fn             */
      if(strcmp(fn,bfio_info[ifile]->fn) == 0) {
/*      File already already opened-adjust book keeping & return */
        bfio_info[ifile]->ntimes_opened++;
        if( strcmp(bfio_info[ifile]->io_control,"rb")==0) {
          strcpy(bfio_info[ifile]->io_control,"r+b");
          if(bfio_info[ifile]->fd != NULL) {
            bfio_close_file(ifile);
          }
        }
        return(ifile);
      }
    }
  }

/*File not already opened-do bf book keeping               */
  if((ifile=bfio_get_file_num())<0) {
    printf("Number of files opened exceeds limit in bfio_open_write_file\n");
    return(-1);
  }

  remove(fn);             /*delete old bf file if exists          */
  if( (fd=mtio_fopen(fn,"wb")) !=NULL) mtio_fclose(fd);  /*create null file*/

  bfio_info[ifile]->flsz = 0;
  bfio_info[ifile]->fd = NULL;
  strcpy(bfio_info[ifile]->fn, fn);
  bfio_info[ifile]->ntimes_opened = 1;
  bfio_info[ifile]->setbufsz=0;
  bfio_info[ifile]->setbuf_ptr=NULL;
  strcpy(bfio_info[ifile]->io_control,"r+b");
  bfio_info[ifile]->activity=-1;  /*Indicate file not actually opened*/
  bfio_info[ifile]->curpos=0;
  bfio_info[ifile]->lastio='o';
  return(ifile);
}

/**************************************************************
*       Actual write to file support routine
*       check of ifile assumed already done
*
*       Written August 2000 by Charles C Burch
**************************************************************/
int32_t bfio_actual_write(int ifile,  int64_t offset, char *buff,
  int32_t nwrite) {
  FILE  *fd;
  int32_t istat;
  
  if( (fd=bfio_info[ifile]->fd)==NULL) {
    bfio_open_file(ifile);
    if( (fd=bfio_info[ifile]->fd)==NULL) {
      printf("Unable to get a fd in bfio_actual_write, ifile=%d\n",ifile);
      return(-4);
    }
  }
  if(bfio_info[ifile]->lastio=='r'){
    /*
      fprintf(stderr,"bfio_actual_write: asking to flush before write on %s\n",
      mtio_inquire(fd));
    */
    mtio_fflush(fd);
  }

  if(offset!=bfio_info[ifile]->curpos) {
    if(mtio_fseek(fd,offset,SEEK_SET)<0) {
      printf("Unable to mtio_fseek to %"PRId64" [%d:%s] in bfio_actual_write\n",
        offset,errno,strerror(errno));
      return(-7);
    }
    bfio_info[ifile]->curpos=offset;
  }
  if(bfio_info[ifile]->region_lock!=0) {
    bfio_lock_file_region(ifile,offset,nwrite);
  }

  if( (istat=mtio_fwrite(buff,1,nwrite,fd)) <0) {
    printf("bfio write error on file=%d, istat=%"PRId64" [%d:%s]\n",
        ifile,(int64_t)istat,errno,strerror(errno));
    bfio_info[ifile]->curpos=mtio_ftell(fd);
  } else {
    bfio_info[ifile]->curpos += istat;
  }

  if(bfio_info[ifile]->region_lock!=0)  {
    mtio_fflush(bfio_info[ifile]->fd);
    bfio_unlock_file_region(ifile,offset,nwrite);
  }

  if(bfio_info[ifile]->curpos>bfio_info[ifile]->flsz)
    bfio_info[ifile]->flsz=bfio_info[ifile]->curpos;
  bfio_info[ifile]->lastio='w';
  bfio_update_activity(ifile);
 
  return(istat);
}

/************************* User callable routines *************************/

/************************************************************
*       Open file with given access
*         fn=file name, rwu=r:read, w:write, u:update
*
*       Written August 2000 by Charles C Burch
************************************************************/
int bfio_open (char* fn, char rwu){
  int istat;
  
/*printf("bfio_open:fn=%s, rwu=%c\n",fn, rwu);*/

  istat=-4;
  pthread_mutex_lock(&bfio_io_mutex);

  switch ( (int) rwu) {
  case 'r': case 'R':
    istat=bfio_open_read_file(fn);
    break;
  case 'w': case 'W':
    istat=bfio_open_write_file(fn);
    break;
  case 'u': case 'U':
    istat=bfio_open_read_write_file(fn);
    break;
  }

  pthread_mutex_unlock(&bfio_io_mutex);
  return(istat);
}

/**************************************************************
*       Close a bf file
*         ifile is the file identifier from opening the file
*
*       Written August 2000 by Charles C Burch
**************************************************************/
int bfio_close(int ifile) {
  int istat;

/*printf("bfio_close: ifile=%d\n",ifile);*/
  pthread_mutex_lock(&bfio_io_mutex);
  if(ifile<0 || ifile>=bfio_info_num) {
    printf("Invalid file id (%d) called in bfio_close\n",ifile);
    istat=-1;
    goto EXIT;
  }
  if(bfio_info[ifile]->activity<=(-2)) {
    printf("Attempting to close unopened file(%d) in bfio_close\n",ifile);
    istat=-2;
    goto EXIT;
  }
/*know file is opened-
    actually close if no one has it opened else adjust count         */

  if ((--bfio_info[ifile]->ntimes_opened) ==0) {
    bfio_close_file(ifile);
    bfio_info[ifile]->activity=-2;        /*indicate available for reuse*/

    if(bfio_info[ifile]->setbuf_ptr!=NULL) {
      free(bfio_info[ifile]->setbuf_ptr);
      bfio_info[ifile]->setbuf_ptr=NULL;
    }
  }
  istat=bfio_info[ifile]->ntimes_opened;

EXIT:  
  pthread_mutex_unlock(&bfio_io_mutex);
  return(istat);
}

/**************************************************************
*       Flush a bf file
*         ifile is the file identifier from opening the file
*
*       Written August 2000 by Charles C Burch
**************************************************************/
int bfio_flush(int ifile) {
  int istat;
  
  pthread_mutex_lock(&bfio_io_mutex);
  if(ifile<0 || ifile>=bfio_info_num) {
    printf("Invalid file id (%d) called in bfio_flush\n",ifile);
    istat=-1;
    goto EXIT;
  }
  if(bfio_info[ifile]->activity<(-1)) {
    printf("Attempt to flush unopened file (%d) in bfio_flush\n",ifile);
    istat=-2;
    goto EXIT;
  }
  
  if( (bfio_info[ifile]->fd) !=NULL) {
    mtio_fflush(bfio_info[ifile]->fd);
    bfio_update_activity(ifile);
  }
  istat=0;
  
EXIT:
  pthread_mutex_unlock(&bfio_io_mutex);
  return(istat);
}

/**************************************************************
*       chmod a bf file
*         ifile is the file identifier from opening the file
*
*       Written Jan 2009 By Bill Menger
**************************************************************/
int bfio_chmod(int ifile, int mode) {
  int istat;
  
  pthread_mutex_lock(&bfio_io_mutex);
  if(ifile<0 || ifile>=bfio_info_num) {
    printf("Invalid file id (%d) called in bfio_chmod\n",ifile);
    istat=-1;
    goto EXIT;
  }
  if(bfio_info[ifile]->activity<(-1)) {
    printf("Attempt to chmod unopened file (%d) in bfio_chmod\n",ifile);
    istat=-2;
    goto EXIT;
  }
  
  if( (bfio_info[ifile]->fd) !=NULL) {
    /* chmod returns 0 for success, -1 for failure */
    /* TODO --> replace chmod with fchmod and use filedes from
                lookup of the ->fd from struct
    */
    istat = chmod(bfio_info[ifile]->fn, (mode_t) mode);
    bfio_update_activity(ifile);
  }
  
EXIT:
  pthread_mutex_unlock(&bfio_io_mutex);
  return(istat);
}



/**************************************************************
*     fetch data from a bf file (so system cache has it)
*       ifile is the file identifier from opening the file
*       offset is the offset to begin reading
*       nread is the number of bytes to read
*
*     Written August 2000 by Charles C Burch
**************************************************************/
int32_t bfio_fetch(int ifile, int64_t offset, int32_t nread) {
  int32_t retvar;
  char *buff;
  
  if((buff=(char *) malloc ((size_t) nread))==NULL) return(-1);
  
  retvar = bfio_read(ifile,offset,buff,nread);
  free(buff);
  return retvar;
}

/**************************************************************
*       read a bf file
*       ifile is the file identifier from opening the file
*       offset is the offset to begin reading
*       buff is where to read the data to
*       nread is the number of bytes to read
*
*       Written August 2000 by Charles C Burch
*       Read ahead buffer added April 2000 by Charles C Burch
**************************************************************/
int32_t bfio_read(int ifile, int64_t offset, char *buff, int32_t nread) {
  int64_t istat;
  FILE *fd;

  pthread_mutex_lock(&bfio_io_mutex);
  if(ifile<0 || ifile>=bfio_info_num) {
    printf("Invalid file id(%d) called in bfio_read\n",ifile);
    istat=-1;
    goto EXIT;
  }
  
  if (bfio_info[ifile]->activity<(-1)) {
    printf("Attempt to read unopened file(%d) in bfio_read\n",ifile);
    istat=-2;
    goto EXIT;
  }
  
  if( (fd=bfio_info[ifile]->fd)==NULL) {
    bfio_open_file(ifile);
    if( (fd=bfio_info[ifile]->fd)==NULL) {
      printf("Unable to get a fd in bfio_read, ifile=%d\n",ifile);
      istat=-3;
      goto EXIT;
    }
  }
  if(bfio_info[ifile]->lastio=='w') {
    mtio_fflush(fd);
  }
/*****
  if( (istat=mtio_ftell(fd))!=bfio_info[ifile]->curpos) {
    printf("seek error, ifile=%d, mtio_ftell=%"PRId64", bfio=%"PRId64"\n",
     ifile, (int64_t)istat, (int64_t)bfio_info[ifile]->curpos);
  }
******/
  if(offset!=bfio_info[ifile]->curpos) {
/*  printf ("bfio read seek, ifile=%d, seek=%"PRId64", curpos=%"PRId64"\n",
     ifile, offset, (int64_t)bfio_info[ifile]->curpos);  */
    if(mtio_fseek(fd,offset,SEEK_SET)<0) {
      printf("Unable to bfio_fseek to %"PRId64" [%d:%s] in bfio_read\n",
        offset,errno,strerror(errno));
      return(-7);
    }
    bfio_info[ifile]->curpos=offset;
  }
  if(bfio_info[ifile]->region_lock!=0) {
    bfio_lock_file_region (ifile,offset,nread);
    mtio_fflush(bfio_info[ifile]->fd);
  }

  if( (istat=mtio_fread(buff,1,nread,fd)) <0) {
    printf("bfio_read error with file=%d,pos=%"PRId64",nb=%"PRId32"\n",
      ifile, (int64_t) offset, nread);
    bfio_info[ifile]->curpos=mtio_ftell(fd);
  } else {
    bfio_info[ifile]->curpos += istat;
  }

  if(bfio_info[ifile]->region_lock!=0)  {
    bfio_unlock_file_region(ifile,offset,nread);
  }

  bfio_info[ifile]->lastio='r';
  bfio_update_activity(ifile);

EXIT:
  pthread_mutex_unlock(&bfio_io_mutex);
  return(istat);
}
/*******************************************************************************
*    bfio_readlink_to_end: Purpose -- to readlink until we actually
*    find the file that is ultimately linked to.
*    Written August 15, 2006 Kruger Corn modified by Bill Menger
*    ACTION:  behaves just like the readlink system call, but always
*    finds that final file, doesn't stop after one link, which could
*    be pointing to another link!  
*    RETURNS:  len(base file name) or -1 on error (e.g. "nonexistant file")
*******************************************************************************/
int  bfio_readlink_to_end(const char *path, char *buf, size_t bufsiz) {
  int retval, n,i,lineno;
  char tmp[380],*scratch;
  if (path == NULL) {lineno=__LINE__;goto error;}
  retval = strlen (path);
  if (retval < 1 || retval > sizeof(tmp)) {lineno=__LINE__;goto error;}
  strcpy (tmp, path); /* copy the file name into tmp buffer*/
  exptilde_crou1(tmp); /* expand the tilde into home directory */
  retval = strlen (tmp); /* get length of the new expanded path name */
  for (n = retval; n > 0;) {
    memset(buf,0,bufsiz); /* zero out the buffer */
    n = readlink (tmp, buf, bufsiz); /* do the readlink.  return -1 on
                                        either reading a link or a bad
                                        (i.e. nonexistant) file.
                                     */
    if (n > 0) {     /* then we have a "link" and will follow it after cleanup*/
      while (strstr(buf,"./") == buf) {         /* first two chars are "./"   */
        for(i=0;i<strlen(buf)-1;i++)buf[i]=buf[i+2];           /* remove them */
      }                  /* ... keep it up until all successive ./'s are gone */
      if(strpbrk(buf,"/") != buf) {   /* then this is probably a relative path*/
        while (strstr(buf,"../") == buf) {       /* then it refers up a level */
          for(i=0;i<strlen(buf)-2;i++)buf[i]=buf[i+3];      /* remove the ../ */
          scratch=strrchr(tmp,'/');          /* find LAST / in the file name */
          if(scratch != NULL) tmp[abs(scratch-tmp)]='\0';/* find next dir up.*/
          scratch=strrchr(tmp,'/');    /*again seek LAST / to go up one level.*/
          if(scratch != NULL) tmp[abs(scratch-tmp)+1]='\0';/*Null next char. */
        }
        /* now it doesn't start with "./" or "../" and points to correct dir. */
        scratch=strrchr(tmp,'/');/* find that dir and terminate string there */
        if(scratch != NULL) tmp[abs(scratch-tmp)+1]='\0';  /* by adding NULL */
        strcat(tmp,buf);              /* now put the file name after the dir */
        n=strlen(tmp);      /* and set the length of this file name into 'n' */
      } else {
        strcpy (tmp, buf);         /* this is an absolute file path already */
        tmp[n] = '\0';             /* so just copy it to tmp and terminate it */
      }
    }
    /* now go back up and reset buf to null, do the readlink again, don't quit
       until you hit a -1 return on the readlink.                             */
  }
                                           /* put the last file name into buf */
  memset(buf,0,bufsiz); /* zero out the buffer */
  strncpy (buf, tmp,strlen(tmp));
  retval = strlen (buf); 
  /* returning the length of the filename ultimately pointed to by the original
     incoming path                                                            */
  return retval;
error:
  fprintf(stderr,"bfio.c[%d]:error file %s\n",lineno,path);
  return -1;
}

/*******************************************************************************
*    bfio_delete_link_chain: Purpose -- to delete each link in a chain until
*    we delete the base file itself.
*    ex.  link a points to b points to c points to d
*         bfio_delete_link_chain(a) deletes a then b then c then d.
*    Limitations:  Path names cannot exceed 380 chars.
*    Bill Menger Sept 2006
*    RETURNS:  -1 error, or 0 success
*******************************************************************************/
int  bfio_delete_link_chain(char *path) {
  int n,i,bufsiz, lineno;
  char tmp[380],buf[380],*scratch;
  if (path == NULL) {lineno=__LINE__;goto error;}
  i = strlen (path);
  if ( i < 1 || i > sizeof(tmp)) {lineno=__LINE__;goto error;}
  strcpy (tmp, path); /* copy the file name into tmp buffer*/
  exptilde_crou1(tmp); /* expand the tilde into home directory */
  bufsiz=sizeof(buf); 
  for (n = 1; n > 0;) {
    memset(buf,0,sizeof(buf)); /* zero out the buffer */
    n = readlink (tmp, buf, bufsiz); /* do the readlink.  return -1 on
                                        either reading a link or a bad
                                        (i.e. nonexistant) file.
                                     */
    if(remove(tmp) != 0) {lineno=__LINE__;goto error;}
    if (n > 0) {     /* then we have a "link" and will follow it after cleanup*/

      while (strstr(buf,"./") == buf) {         /* first two chars are "./"   */
        for(i=0;i<strlen(buf)-1;i++)buf[i]=buf[i+2];           /* remove them */
      }                  /* ... keep it up until all successive ./'s are gone */
      if(strpbrk(buf,"/") != buf) {   /* then this is probably a relative path*/
        while (strstr(buf,"../") == buf) {       /* then it refers up a level */
          for(i=0;i<strlen(buf)-2;i++)buf[i]=buf[i+3];      /* remove the ../ */
          scratch=strrchr(tmp,'/');          /* find LAST / in the file name */
          if(scratch != NULL) tmp[abs(scratch-tmp)]='\0';/* find next dir up.*/
          scratch=strrchr(tmp,'/');    /*again seek LAST / to go up one level.*/
          if(scratch != NULL) tmp[abs(scratch-tmp)+1]='\0';/*Null next char. */
        }
        /* now it doesn't start with "./" or "../" and points to correct dir. */
        scratch=strrchr(tmp,'/');/* find that dir and terminate string there */
        if(scratch != NULL) tmp[abs(scratch-tmp)+1]='\0';  /* by adding NULL */
        strcat(tmp,buf);              /* now put the file name after the dir */
        n=strlen(tmp);      /* and set the length of this file name into 'n' */
      } else {
        strcpy (tmp, buf);         /* this is an absolute file path already */
        tmp[n] = '\0';             /* so just copy it to tmp and terminate it */
      }
    } 
    /* now go back up and reset buf to null, do the readlink again, don't quit
       until you hit a -1 return on the readlink.                             */
  }

  return 0;

error:
  if (lineno) {} /* make 64sgi73 happy */
  /*fprintf(stderr,"bfio.c[%d]: error deleting %s\n",lineno,tmp);*/
  return -1;
}
/**************************************************************
*       read a bf file using gets
*       ifile is the file identifier from opening the file
*       offset is the offset to begin reading
*       buff is where to read the data to
*       nread is the maximum number of bytes to read
*       returned status
*        >0: valid data, #bytes read
*         0: EOF 
*        -1: Invalid file id
*        -2: Attempt to read unopened file
*        -3: Unable to get fd
*        -4: eof found but last record not empty
*        -5: no \n found, buffer full
*        -6: invalid nread specified (<=0)
*
*       Written June 2002 by Charles C Burch
**************************************************************/
int32_t bfio_gets(int ifile, int64_t offset, char *buff, int32_t nread) {
  int64_t istat;
  FILE  *fd;
  char  *s_ptr;

  pthread_mutex_lock(&bfio_io_mutex);
  if(ifile<0 || ifile>=bfio_info_num) {
    printf("Invalid file id(%d) called in bfio_gets\n",ifile);
    istat=-1;
    goto EXIT;
  }
  if (bfio_info[ifile]->activity<(-1)) {
    printf("Attempt to read unopened file(%d) in bfio_gets\n",ifile);
    istat=-2;
    goto EXIT;
  }
  if( (fd=bfio_info[ifile]->fd)==NULL) {
    bfio_open_file(ifile);
    if( (fd=bfio_info[ifile]->fd)==NULL) {
      printf("Unable to get a fd in bfio_gets, ifile=%d\n",ifile);
      istat=-3;
      goto EXIT;
    }
  }
  
  if(nread<=0) {
    istat=-6;
    goto EXIT;
  } 

  if(bfio_info[ifile]->lastio=='w') {
    mtio_fflush(fd);
  }

  if(offset!=bfio_info[ifile]->curpos) {
    if(mtio_fseek(fd,offset,SEEK_SET)<0) {
      printf("Unable to bfio_fseek to %"PRId64" [%d:%s] in bfio_gets\n",
        offset,errno,strerror(errno));
      return(-7);
    }
    bfio_info[ifile]->curpos=offset;
  }

  if(bfio_info[ifile]->region_lock!=0) {
    bfio_lock_file_region (ifile,offset,nread);
    mtio_fflush(bfio_info[ifile]->fd);
  }

  buff[0]='\0';
  istat=0;
  /*** printf("bfio_gets: nread=%"PRId32"\n",nread); ***/
  if(mtio_fgets(buff,nread,fd)==NULL) {
    /*** printf("gets(NULL), tell=%d, buff=|%s|\n",mtio_ftell(fd),buff); ***/
    if(buff[0]!=0) istat=-4;
    bfio_info[ifile]->curpos=mtio_ftell(fd);
  } else {
    /*** printf("gets(!NULL), tell=%d, buff=|%s|\n",mtio_ftell(fd),buff); ***/
    if((s_ptr=(char*)memchr(buff,'\n',nread-1))==NULL) {
      istat=-5;
      bfio_info[ifile]->curpos += (nread-1);
    } else {
      istat=(s_ptr-buff)+1;
      bfio_info[ifile]->curpos += istat;
    }
  }

  if(bfio_info[ifile]->region_lock!=0)  {
    bfio_unlock_file_region(ifile,offset,nread);
  }

  bfio_info[ifile]->lastio='r';
  bfio_update_activity(ifile);

EXIT:
  pthread_mutex_unlock(&bfio_io_mutex);
  return(istat);
}

/**************************************************************
*       Force a write so the file appears given size 
*
*       Written August 2000 by Charles C Burch
**************************************************************/
int bfio_force_file_max_size(int ifile, int64_t size) {
  char data0=0;
  int64_t psize;
  int istat;

  pthread_mutex_lock(&bfio_io_mutex);
  if(ifile<0 || ifile>=bfio_info_num) {
    printf("Attempt to use invalid file id (%d) in bfio_force_mxsz\n",ifile);
    istat=-1;
    goto EXIT;
  }

  if(bfio_info[ifile]->activity<(-1)) {
    printf("Attemp to write to unopened file (%d) in bfio_force_mxsz\n",ifile);
    istat=-2;
    goto EXIT;
  }
  if(strcmp(bfio_info[ifile]->io_control,"rb")==0) {
    printf("ERROR:Attempting to bfio_force_mxsz to a read-only file\n");
    istat=-3;
    goto EXIT;
  }
  if(bfio_info[ifile]->flsz==size) {
    istat=0;
  } else {
    psize = size - 1;
    if(psize< 0) psize=0;
    istat=bfio_actual_write(ifile, psize, &data0,1L);
  }
  
EXIT:
  pthread_mutex_unlock(&bfio_io_mutex);
  return(istat);
}

/**************************************************************
*       write to a bf file
*       ifile is the file identifier from opening the file
*       offset is the offset to begin writing
*       buff is where to read the data from to write
*       nwrite is the number of bytes to write
*
*       Written August 2000 by Charles C Burch
**************************************************************/
int32_t bfio_write(int ifile,  int64_t offset, char *buff, int32_t nwrite) {
  int64_t istat;

  pthread_mutex_lock(&bfio_io_mutex);
  if(ifile<0 || ifile>=bfio_info_num) {
    printf("Attempt to use invalid file id (%d) in bfio_write\n",ifile);
    istat=-1;
    goto EXIT;
  }

  if(bfio_info[ifile]->activity<(-1)) {
    printf("Attemp to write to unopened file (%d) in bfio_write\n",ifile);
    istat=-2;
    goto EXIT;
  }
  if(strcmp(bfio_info[ifile]->io_control,"rb")==0) {
    printf("ERROR: Attempting to write to a read-only file in bfio_write\n");
    istat=-3;
    goto EXIT;
  }

  istat=bfio_actual_write(ifile, offset, buff, nwrite);

EXIT:
  pthread_mutex_unlock(&bfio_io_mutex);
  return(istat);
}

/**************************************************************
*       set buffer size for a given ifile
*       ifile is the file identifier from opening the file
*       bufsz is the buffer size to use(<0 turns off buffering)
*
*       Written August 2000 by Charles C Burch
**************************************************************/
int bfio_setbufsz(int ifile, int32_t bufsz) {
  int istat;
 
/*printf("setbufsz: ifile=%d, info_num=%d\n",ifile, bfio_info_num);*/
 
  pthread_mutex_lock(&bfio_io_mutex);
  if(ifile<0 || ifile>=bfio_info_num) {
    printf("Attempt to use invalid file id(%d) in bfio_setbufsz\n",ifile);
    istat=-1;
    goto EXIT;
  }
  if(bfio_info[ifile]->activity<(-1)) {
    printf("Attempt to setvbuf to unopened file (%d) in bfio_setbufsz\n",
     ifile);
    istat=-2;
    goto EXIT;
  }
  
  if(bufsz>0 && bufsz<=bfio_info[ifile]->setbufsz) {
    istat=0;
    goto EXIT;
  }
/*           we have a new bufsz           */

  if(bfio_info[ifile]->setbuf_ptr != NULL) {  /*free previous setbuf */
    if(bfio_info[ifile]->fd != NULL) {
      if(bfio_info[ifile]->lastio!='o') mtio_fflush(bfio_info[ifile]->fd);
    }
    free(bfio_info[ifile]->setbuf_ptr);
    bfio_info[ifile]->setbuf_ptr=NULL;
  }

  bfio_info[ifile]->setbufsz=bufsz;

/*now setbufsz explicit if file just opened or implicit thru bfio_open_file*/
  if(bfio_info[ifile]->lastio=='o'  && bfio_info[ifile]->fd != NULL) {
    if(bufsz>0) {
      if( (bfio_info[ifile]->setbuf_ptr=(char *) malloc(bufsz)) !=NULL) {
        setvbuf(bfio_info[ifile]->fd, bfio_info[ifile]->setbuf_ptr,
          _IOFBF, (size_t) bufsz);
      }
    } else if(bufsz<0) {
      setvbuf(bfio_info[ifile]->fd,NULL, _IONBF,0);   /*no buffering */
    }
  } else {
    bfio_close_file(ifile);
    bfio_open_file(ifile);
  }
  istat=0;

EXIT:
  pthread_mutex_unlock(&bfio_io_mutex);
  return(istat);
}

/************************ Routines for file locking ***************************/

/************************** format of lock file  *******************************
version 0:
has no special information in first record
record length is 293
00000000011111111112222222222333333333344444444445555555555666666666677777777778
12345678901234567890123456789012345678901234567890123456789012345678901234567890
yyyy-mm-dd hh:mm:ss -lock exp- lock name(260)
type(T) is is position 292

version 1:
first record is "Version 1
record length is 320
00000000011111111112222222222333333333344444444445555555555666666666677777777778
01234567890123456789012345678901234567890123456789012345678901234567890123456789
yyyy-mm-dd hh:mm:ss -lock exp- T ----host---- --pid-- lock name(260)
*******************************************************************************/

/******************* BFIO_ADJUST_LOCKFILE *****************
*  Determines indexed lockfile info
*
*  Written January 2003 by Charles C Burch
************************************************************/
int bfio_adjust_lockfile_name(char *lock_file, char *lockfile, int32_t num) {
  int num_lock_files;
  char *ptr, work[4];
 
  /*printf("bfio_adjust_lockfile_name:lf=%s, num=%"PRId32"\n",lock_file,num);*/
  str_compress(lockfile, lock_file);
  if((ptr=strchr(lockfile,':'))!=NULL) {
    ptr[0]='\0';
    num_lock_files=str_atoi(ptr+1);
    if(num_lock_files>100) num_lock_files=100;
    if(num_lock_files<1)   num_lock_files=1;
  } else {
    num_lock_files=1;
  }  
  
  if(num_lock_files>1) {
    num=num%num_lock_files;
    work[0]='.';
    if(num<10) {
      work[1]='0'+num;
      work[2]='\0';
    } else {
      work[1]='0'+num/10;
      work[2]='0'+num%10;
      work[3]='\0';
    }
    strcat(lockfile,work);
  }
  
  return(num_lock_files);
}

/******************** BFIO_READ_LOCK_FILE ******************
* Shared code to read lock file
* returns number of records in lock file
*
* Written July 2002 by Charles C Burch
***********************************************************/
int bfio_read_lock_file(char *routine, struct bfio_lock_struct *lock_parms) {
  int i, lockfile_size, nrecs;
  char *buff, work[80];
  int max_lock_lrecl=320;

  /** printf("bfio_read_lock_file-enter: routine=%s\n",routine); **/
  lockfile_size=bfio_file_size(lock_parms->lock_file);
  if(lockfile_size<0) lockfile_size=0;

  /*allocating space for a new record and also possible version record*/
  i=2*max_lock_lrecl; /*size to accomodate 2 new records for any file version*/
  if((buff=(char*)malloc(lockfile_size+i))==NULL){
    sprintf(work, "Abort:Unable to malloc buff in %s [%d:%s]", 
        routine,errno,strerror(errno));
    cpslog_message(work);
    unix_abort_c(work);
  }
  
  lseek(lock_parms->fd,0L,SEEK_SET);
  if(lockfile_size>0) {
    if((i=read(lock_parms->fd,buff, lockfile_size))!=lockfile_size) {
      sprintf(work,"Error in reading cps_lockfile.dat, stat=%d [%d:%s]",
       i,errno,strerror(errno));
      cpslog_message(work);
      printf("%s\n",work);
      lockfile_size=i;
      if(i<0) lockfile_size=0;
    }  
  }

  buff[lockfile_size]='\0';
  if(lockfile_size>0) {
    if(str_sub_cmp(buff,0,-1,"Version")!=0) {
      lock_parms->version=0;
    } else if(str_sub_cmp(buff,0,-1,"Version 1")==0) {
      lock_parms->version=1;
    } else {
      lock_parms->version=-1;
    } 
    if(lockfile_size<=max_lock_lrecl && lock_parms->version>0) {
      /*file empty--allow default lock_file_version to change versions*/ 
      lock_parms->version=bfio_default_lock_file_version;
    } 
  } else {   
    /*file empty--allow default lock_file_version to change versions*/ 
    lock_parms->version=bfio_default_lock_file_version;
  }
 
  if(lock_parms->version==0) {
    lock_parms->lrecl=293;
  } else if(lock_parms->version==1) {
    lock_parms->lrecl=320;
  } else {
    unix_abort_c("Invalid lock file version in bfio_read_lock_file");
  }  
  
  nrecs=lockfile_size/lock_parms->lrecl;
  if(lock_parms->version>=1 && nrecs>0) nrecs--;

  lock_parms->buff=buff;
  return(nrecs);
}
/********** BFIO_WRITE_LOCK_FILE **********
* Shared code to write lock file
*
* Written July 2002 by Charles C Burch
*****************************************/
int bfio_write_lock_file(char *routine, struct bfio_lock_struct *lock_parms, 
    int nrecs) {
  int i, old_lockfile_size, lockfile_size;
  char work[80];

  /** printf("bfio_write_lock_file-enter: routine=%s\n",routine); **/
  lockfile_size=nrecs*lock_parms->lrecl;
  if(lock_parms->version>=1) {
    memset(lock_parms->buff,' ',lock_parms->lrecl);
    sprintf(lock_parms->buff,"Version %d", lock_parms->version);
    lockfile_size+=lock_parms->lrecl;
  }  
  old_lockfile_size=lockfile_size;
  
  while(lockfile_size>0) {         /*compress lock file */
    i=lockfile_size-lock_parms->lrecl;
    if(lock_parms->buff[i]==' ') {
      lockfile_size=i;
      if(lockfile_size<=0) {
        lockfile_size=0;
        break;
      }
    } else {
      break;
    }
  }
  /**   printf("bfio_write_lock_file, post_size=%d\n",lockfile_size);   **/

  lseek(lock_parms->fd,0L,SEEK_SET); /*write out the lockfile*/
  if(lockfile_size>0) {
    i=write(lock_parms->fd,lock_parms->buff,lockfile_size);
    if(i!=lockfile_size) {
      sprintf(work, "Error: writing lockfile in %s, stat=%d [%d:%s]",
       routine, i, errno,strerror(errno));
      cpslog_message(work);
    }
  }

  if(lockfile_size<old_lockfile_size) {
    /*truncated unused space in lockfile*/
    ftruncate(lock_parms->fd,(off_t)lockfile_size);
  }
  return(0);
}

/********** BFIO_GET_LOCK_FILE_REC **********
* Shared code to get lock file record 
*
* Written July 2002 by Charles C Burch
*********************************************/
void bfio_get_lock_file_rec(struct bfio_lock_struct *lock_parms, int irec, 
    char **p_date, char **p_time, char **expire_time,
    char *lock_type, char **host, char **pid, char **file) {
  char *ptr;
  static char *blank_data=" ";

  if(lock_parms->version==0) {
    ptr=lock_parms->buff+irec*lock_parms->lrecl;
    (*lock_type)=ptr[291];
    (*host)=blank_data;
    (*pid)=blank_data;
    
    (*p_date)=ptr;
    ptr[10]='\0';
    
    ptr+=11;
    (*p_time)=ptr;
    ptr[8]='\0';
    
    ptr+=9;
    (*expire_time)=ptr;
    ptr[10]='\0';
    
    ptr+=11;
    (*file)=ptr;
    ptr[259]='\0'; /*making file name max of 259 characters*/
    
  } else if(lock_parms->version==1) {  
    ptr=lock_parms->buff+(irec+1)*lock_parms->lrecl;
    (*p_date)=ptr;
    ptr+=11;
    (*p_time)=ptr;
    ptr+=9;
    (*expire_time)=ptr;
    ptr+=11;
    (*lock_type)=ptr[0];
    ptr+=2;
    (*host)=ptr;
    ptr+=13;
    (*pid)=ptr;
    ptr+=8;
    (*file)=ptr;
    
  } else {
    unix_abort_c("Invalid lock file version in bfio_get_lock_file_rec");
  }  
  return;
}  
  
/********** BFIO_PUT_LOCK_FILE_REC **********
* Shared code to put lock file record
*
* Written July 2002 by Charles C Burch
*********************************************/
void bfio_put_lock_file_rec(struct bfio_lock_struct *lock_parms, int irec, 
    char *p_date, char *p_time, char *expire_time,
    char lock_type, char *host, char *pid, char *file) {
  char z='\0';

  if(lock_parms->version==0) {
    sprintf(lock_parms->buff+irec*lock_parms->lrecl,
     "%10s %8s %10s %-260s%c",
     p_date,p_time,expire_time,file,lock_type);
  } else if(lock_parms->version==1) {  
    sprintf(lock_parms->buff+(irec+1)*lock_parms->lrecl,
     "%10s%c%8s%c%10s%c%c%c%-12s%c%7s%c%-260s%c",
     p_date,z,p_time,z,expire_time,z,lock_type,z,host,z,pid,z,file,z);
  } else  {
    unix_abort_c("Invalid lock file version in bfio_put_lock_file_rec");
  }
  return;
}  
  
/********** BFIO_ZAP_LOCK_FILE_REC **********
* Shared code to zap lock file record
*
* Written July 2002 by Charles C Burch
*********************************************/
void bfio_zap_lock_file_rec(struct bfio_lock_struct *lock_parms, int irec) {
  int offset;
  
  /**  printf("bfio_zap_lock_file_rec, irec=%d\n",irec); **/
  offset=irec*lock_parms->lrecl;
  if(lock_parms->version>=1) offset+=lock_parms->lrecl;
  
  memset(lock_parms->buff+offset,' ',lock_parms->lrecl);
  lock_parms->buff[offset+lock_parms->lrecl-1]='\0';
  return;
}  
  
/******************* BFIO_LOCK_THE_LOCKFILE *****************
*  Locks the lock file -returns fd of lockfile if successful,
*                              -1 if not successful
*  Returns actual lock file used in argument lockfile
*
*  Written November 2000 by Charles C Burch
*  Does not call mtio -- uses direct fcntl/open/close calls. wmm
*************************************************************/
int bfio_lock_the_lockfile(char *lock_file, char *file, char *lockfile) {
  int        fd, t_sleep, t_wait;
  static unsigned int t_seed=0;
  int32_t    count, check;
  struct     flock lock_region;
  int    try_open_lock_file_count;
  /*char string[260];*/
  
  /* get a seed for random_number generator */
  if(t_seed==0) {
    t_seed=(((unsigned int) time(NULL))+((unsigned int) getpid()))/2;
    bfio_initialize(); /*get any needed cnfg parameters*/
    srandom(t_seed);
  }
  
  lock_region.l_type   =F_WRLCK;  /*set up the file lock to lock entire file*/
  lock_region.l_whence =SEEK_SET;
  lock_region.l_start  =0L;
  lock_region.l_len    =0L;

  check=0;  /*hash filename*/
  /*fprintf(stderr,"%d:file=%s\n",__LINE__,file);*/
  for(count=0; file[count]!='\0'; count++) {
    if(file[count]!=' ') check=check+file[count];
  }
  bfio_adjust_lockfile_name(lock_file, lockfile, check);
  /*printf("%d: check=%"PRId32", lf=%s\n",__LINE__,check,lockfile);*/
  /*
  if(strcmp(file,"pfio_get_node_reserves")!=0){ 
    strcpy(string,"");
    sprintf(string,"bfio_lock_the_lockfile, lf=%s, file=%s\n",lockfile, file);
    cpslog_message(string);
  }
  */
  
  count=0;
  check=60;
  t_sleep=1;
  t_wait=0;
  while(1) {
    fd = -1;
    try_open_lock_file_count = 0;
    while(fd < 0 && try_open_lock_file_count++ < 3 ) {
      if((fd=open(lockfile,O_RDWR)) < 0 ) {  /*open the lockfile*/
        fprintf(stderr,
        "bfio.c:%d: Open lockfile %s failed\n",__LINE__,lockfile);
        /*** this attempts to remount disk volume with lock file on it ***/
        if(bfio_remount_file_disk(lockfile));
        sleep(1);
      }
    }

    if(fd<0) {
      if(bfio_lock_mess_sw==0) {
        fprintf(stderr,
         "NOTE:%s does not exist-File locking bypassed.\n",lockfile);
        bfio_lock_mess_sw=1;
      }
      return(-1);
    } else {
      /*** reset for next call ***/
      bfio_lock_mess_sw=0;
    }

    if(fcntl(fd,F_SETLK, &lock_region)>=0) {  
      /*try and test the lock setting*/
      if(count>30) {                      /*print message if wait was >30secs*/
        sprintf(bfio_error_string,
         "Information: lockfile became available after %"PRId32" seconds",count);
        fprintf(stderr,"%s\n",bfio_error_string);
        cpslog_message(bfio_error_string);
      }
      /**  printf("fd=%d\n",fd);  **/
      return(fd);
    }
    /* lock did not work-close the lockfile, print periodic messages and wait*/
    /*fprintf(stderr,"lockfile locked...waiting, %s\n",file);*/
    close(fd);

    /*count+=59; printf("Warning test code activated\n"); */

    /*** "check" is only here to control print statement frequency ***/
    if(count >= check) {
      sprintf(bfio_error_string,
       "bfio_lock_the_lockfile waiting:(%s-%"PRId32" secs), status= [%d:%s]",
       file, count, errno,strerror(errno));
      fprintf(stderr,"%s\n",bfio_error_string);
      cpslog_message(bfio_error_string);
      bfio_lock_mess_sw=1;

      if(check==60) {
         check=900; 
      } else if(check==900) {
         check=1800; 
      } else if(check==1800) {
         check=3600; 
      } else {
         check=check+3600; 
      }
      if(check > 43200 ) {
        /*** after 12 hours, reset ***/
        check = 60;
      }
    }
    /** sleep a bit based on some increasing randonness **/    
    if(t_sleep<=1) {
      t_wait=1;
    } else {
        t_wait=(int) (1+random()%t_sleep);
    }  
    if(t_sleep<10) t_sleep++;
    sleep(t_wait);
    count+=t_wait;
  }
}

/*************** BFIO_UNLOCK_THE_LOCKFILE *****************
*  Unlocks the lockfile: returns 0 if successful, -1 if not
*
*  Written November 2000 by Charles C Burch
*  Does not call mtio -- uses direct fcntl/close calls. wmm
**********************************************************/
int bfio_unlock_the_lockfile(int fd) {
  struct flock lock_region;
  /*char string[260];*/

  /*
    strcpy(string,"");
    sprintf(string,"bfio_unlock_the_lockfile, fd=%d\n",fd);
    cpslog_message(string);
  */

  if(fd<0) return(-1);                /*bypass if lockfile nonexistent*/

  lock_region.l_type   =F_UNLCK;      /*set up the file unlock*/
  lock_region.l_whence =SEEK_SET;
  lock_region.l_start  =0L;
  lock_region.l_len    =0L;

  if(fcntl(fd,F_SETLK,&lock_region) < 0 ) {
    sprintf(bfio_error_string,
     "Error: bfio_unlock_the_lockfile unlock error [%d:%s]",
     errno,strerror(errno));
    fprintf(stderr,"%s\n",bfio_error_string);
    cpslog_message( bfio_error_string);
  }
  return((int) close(fd));
}

/*******************************************************************
*  lock type
*  blank     file is not locked
*  N         return only: No lock file - locks are ignored
*  E         return only: filed got locked but by old lock expiring
*  H         hard lock-anyone can delete expired locks
*  R         Reserve file lock
*  X         Extended lock-only given file requests
*            can expire locks unless older than 90 days
********************************************************************/

/********** BFIO_CHANGE_LOCK_TYPE ********
* Change lock type
*
* Written November 2001 by Charles C Burch
*****************************************/
int bfio_change_lock_type(char *lock_file, char *file_name_raw, char lock_type){
  int n_lockfile_recs, i_rec, istat;
  char fn_compressed[260];
  char *p_date, *p_time, *expire_time, locktype, *host, *pid, *file;
  struct bfio_lock_struct lock_parms;

/***
  This works by first locking the lockfile, then seeing if the specified
  filename is in the lockfile and changing type.
***/

  str_compress(lock_parms.file_name, file_name_raw);
  /*lock the lockfile*/
  /*printf("2327: %s\n",lock_file);*/
  if((lock_parms.fd=bfio_lock_the_lockfile(lock_file, lock_parms.file_name, 
          lock_parms.lock_file))<0) {
    bfio_unlock_the_lockfile(lock_parms.fd);      /*bypass if no lockfile */
    return(0);
  }

  n_lockfile_recs=bfio_read_lock_file("bfio_change_lock_type", &lock_parms);
/****
  scan the lockfile-looking for file_name.  If found see if lock has timed out
****/
  istat=-1;
  for (i_rec=0; i_rec<n_lockfile_recs; i_rec++) {
    bfio_get_lock_file_rec(&lock_parms, i_rec, &p_date, &p_time, &expire_time,
        &locktype, &host, &pid, &file);
    str_compress(fn_compressed, file);
    if(strcmp(lock_parms.file_name,fn_compressed)==0) {
      bfio_put_lock_file_rec(&lock_parms, i_rec, p_date, p_time, expire_time,
        lock_type, host, pid, file);
      bfio_write_lock_file("bfio_change_lock_type",&lock_parms,n_lockfile_recs);
      istat=0;
      break;
    }
  }

  bfio_unlock_the_lockfile(lock_parms.fd); /*close the lockfile unlocking it */
  if(lock_parms.buff!=NULL) free(lock_parms.buff);
  return(istat);
}

/******* BFIO_TRY_LOCKING_FILE ***********
* try locking file_name for timeout seconds
* return lock type if locked
*
* Written November 2001 by Charles C Burch
*****************************************/
char bfio_try_locking_file(char *lock_file, char *file_name_raw,
  int32_t timeout, char lock_type) {
  int    t_check, n_lockfile_recs, i_rec, i_blank, i_pid;
  int32_t temp;
  time_t t_lock;
  struct tm *tm_ptr;
  char   type, l_type, fn_compressed[260];
  char   s_date[11], s_time[9], s_expire_time[11], s_host[13], s_pid[8];
  time_t file_time;
  char   *p_date, *p_time, *expire_time, *host, *pid, *file;
  struct bfio_lock_struct lock_parms;

  /*
  printf("bfio_try_locking_file: lf=%s, file=%s\n",
      lock_file, file_name_raw);
  */

  /***
  This works by first locking the lockfile, then seeing if the specified
  filename is in the lockfile.  If it is, someone else has locked the file,
  unlock the lockfile and return the current lock type.

  If the timeout has happened, consider the lock expired.

  If there is not an active lock on the file--insert the filename into the
  file signifying it is now locked, unlock the lockfile and return.
  ***/

  str_compress(lock_parms.file_name, file_name_raw);
  if((lock_parms.fd=bfio_lock_the_lockfile(lock_file, lock_parms.file_name, 
    lock_parms.lock_file))<0) {   
    bfio_unlock_the_lockfile(lock_parms.fd);      /*bypass if no lockfile */
    return('N');
  }

  n_lockfile_recs=bfio_read_lock_file("bfio_try_locking_file", &lock_parms);
  /****
  scan the lockfile-looking for file_name.  If found see if lock has timed out
  ****/
  type=' ';
  i_blank=-1;           /*track first empty slot-or where filename is */
  for (i_rec=0; i_rec<n_lockfile_recs; i_rec++) {
    bfio_get_lock_file_rec(&lock_parms, i_rec, &p_date, &p_time, &expire_time,
        &l_type, &host, &pid, &file);
    str_compress(fn_compressed, file);
    /**  printf("fn=%s, fnc=%s\n",file_name, fn_compressed);  **/
    if(strcmp(lock_parms.file_name,fn_compressed)==0) {
      temp=str_atoll(expire_time); /*file is in lock file*/
      t_check=0;                                         /*no delete */
      if(difftime(time(NULL), (time_t)temp)>0) t_check=1; /*can delete*/
      if(t_check!=0) {
        /*if actual file name and file activity less than 30 minutes ago, 
          do not delete*/
        file_time=bfio_get_file_time(fn_compressed);
        if(((int32_t)file_time)!=-1) {
          if(difftime(time(NULL), file_time)<1800) t_check=0;
        }
      }
      
      if(t_check==0) {          /*see if process active*/
        i_pid=str_atoi(pid);    /*handle pid not defined for file version 0*/
        /* see if host == my machine */
        if(strstr(thismachine.nodename,host) != NULL) { /* then "host" is me.*/
          if(getpid() == (pid_t) i_pid) { /* then "pid" = me */
          } else if ( (pid_t) i_pid > (pid_t) 0 ) {
            if(unix_is_process_active_c(host,i_pid)!=1) t_check=2;
          }
        } else if ( (pid_t) i_pid > (pid_t) 0 ) {
          if(unix_is_process_active_c(host,i_pid)!=1) t_check=2;
        }
      }  

      if(t_check!=0) {
        if(t_check==1) {
          sprintf(bfio_error_string,
           "%sfile lock for (%s) has timed out while locked by another process",
           "Warning: ",lock_parms.file_name);
          cpslog_message(bfio_error_string);
        }  
        i_blank=i_rec;       /*timeout */
        type='E';            /* lock expired */
        break;
      }
      free(lock_parms.buff);
      bfio_unlock_the_lockfile(lock_parms.fd);
      return(l_type); 
    }
    
    if(i_blank<0) {       /*keep track of first blank area */
      if(fn_compressed[0]=='\0') i_blank=i_rec;
    }
  }

/****
  either file not in lock file or has time out-insert into file, return
  as file_name is now locked for others
****/
  t_lock=time(NULL)+timeout;      /*when lock times out */
  tm_ptr=localtime(&t_lock);
  sprintf(s_date,"%4.4d-%2.2d-%2.2d",
   1900+tm_ptr->tm_year,tm_ptr->tm_mon+1, tm_ptr->tm_mday);
  sprintf(s_time,"%2.2d:%2.2d:%2.2d",
   tm_ptr->tm_hour, tm_ptr->tm_min, tm_ptr->tm_sec);
  sprintf(s_expire_time,"%10"PRId32"", (int32_t) t_lock);
  sprintf(s_pid,"%7d", (int) getpid());
  unix_get_hostname_c(s_host,sizeof(s_host));

  if(i_blank<0) {
    i_blank=n_lockfile_recs++; /*insert at end of buffer*/
  }
  
  bfio_put_lock_file_rec(&lock_parms, i_blank, s_date, s_time, s_expire_time,
        lock_type, s_host, s_pid, lock_parms.file_name);
  bfio_write_lock_file("bfio_try_locking_file", &lock_parms, n_lockfile_recs);
  free(lock_parms.buff);
  bfio_unlock_the_lockfile(lock_parms.fd); /*close the lockfile unlocking it */

  return(type);
}

/************** BFIO_LOCK_FILE ***********************
* lock file_name for timeout seconds
* returns 2  if lock file not present 
* returns 0  if file gets lock by file not locked-normal
* returns 1  if file gets locked but old lock expired
*
* Written     November 2000 by Charles C Burch
* Logic redid November 2001 by Charles C Burch
*****************************************************/
int bfio_lock_file(char *lock_file, char *file_name, 
    int32_t timeout, char input_lock_type) {
  int        secs;
  char       line[120], lock_type /*, string[260] */;
  int        t_sleep, t_wait;
  static unsigned int t_seed=0;

  /*
  strcpy(string,"");
  sprintf(string,"bfio_lock_file, lf=%s, file=%s\n",lock_file, file_name);
  cpslog_message(string);
  */
  

  /* get a seed for random_number generator so sleeps are semi-random */
  if(t_seed==0) {
    t_seed=(((unsigned int) time(NULL))+((unsigned int) getpid()))/2;
  }

  secs=0;
  t_sleep=1;
  while(1) {
    lock_type=bfio_try_locking_file(lock_file, file_name, 
        timeout, input_lock_type);

    if(lock_type=='N') { 
      return(2);
      
    } else if(lock_type==' ') {
      if(secs>0) {
        sprintf(bfio_error_string,
         "Locked file(%s) became available after %d secs", file_name,secs);
        cpslog_message(bfio_error_string);
      }
      return(0);
      
    } else if(lock_type=='E') {
      sprintf(line,
       "Locked file(%s) became available(expired) after %d secs",
       file_name,secs);
      cpslog_message(line);
      return(1);
    }

    if(secs==0) {
      sprintf(line,"Waiting for locked file(%s)",file_name);
      cpslog_message(line);
    }

/** sleep a bit based on some increasing randonness **/    
    if(t_sleep<=1) {
      t_wait=1;
    } else {
      /*t_wait=mth_random_number_c(&t_seed, &t_sleep);*/
        srandom(t_seed);
        t_wait=(int) (1+random()%t_sleep);
    }  
    if(t_sleep<10) t_sleep++;

    /**  printf("t_wait=%d\n",t_wait);  **/
    sleep(t_wait);                /*sleep and retry  */
    secs+=t_wait;
  }
}

/******** BFIO_UNLOCK_FILE ******************************
* unlock file_name
* returns 2  if lock file not present 
* returns 0  if file gets unlocked and was locked
* returns 1  if file was not locked
*
* Written November 2000 by Charles C Burch
*******************************************************/
int bfio_unlock_file(char *lock_file, char *file_name_raw) {
  int  n_lockfile_recs, i_rec, i_found;
  int32_t t_check;                    /*type may need to be changed*/
  char fn_compressed[260], work[300] /* ,string[260]*/;
  time_t file_time;
  char   *p_date, *p_time, *expire_time, locktype, *host, *pid, *file;
  struct bfio_lock_struct lock_parms;

/****
  This works by deleting the filename from the lockfile if it is present,
  thus unlocking the filename for others.
  In the process it also deletes any locks than have expired by more
  than 15 minutes and no disk activity for last 30 minutes.
  strcpy(string,"");
  sprintf(string,"bfio_unlock_file, lf=%s, file=%s\n",lock_file, file_name_raw);
  cpslog_message(string);
****/

  if(str_compress(lock_parms.file_name, file_name_raw)==0) return(1);

/**********************************************
  sprintf(bfio_error_string,"Unlocking file %s",file_name);
  cpslog_message(bfio_error_string);
**********************************************/

/* lock the lockfile, read contents */
  if((lock_parms.fd=bfio_lock_the_lockfile(lock_file, lock_parms.file_name, 
          lock_parms.lock_file))<0) {
     bfio_unlock_the_lockfile(lock_parms.fd);        /*bypass if no lockfile */
     return(2);
  }

  n_lockfile_recs=bfio_read_lock_file("bfio_unlock_file", &lock_parms);

/*Scan through lockfile looking for specified filename and expired locks*/
  i_found=-1;
  for (i_rec=0; i_rec<n_lockfile_recs;i_rec++) {
    bfio_get_lock_file_rec(&lock_parms, i_rec, &p_date, &p_time, &expire_time,
        &locktype, &host, &pid, &file);
    str_compress(fn_compressed, file);

    if(strcmp(lock_parms.file_name,fn_compressed)==0) {
      i_found=i_rec;       /*file_name found  */
      bfio_zap_lock_file_rec(&lock_parms, i_rec);
      /*
      strcpy(string,"");
      sprintf(string,
        "bfio_unlock_file, lockfile=[%s] record=[%d] unlock file=[%s]\n",
        lock_parms.lock_file, i_found, lock_parms.file_name);
      cpslog_message(string);
      */

    } else {

      /* not file_name, but unlock if expired, t_check=0 means do not delete*/
      t_check=str_atoll(expire_time);

      if(t_check!=0 && locktype=='X') {
        /*if expire time less than 10 days ago do not delete*/
        if(difftime(time(NULL), (time_t)t_check)<864000) t_check=0;
      }

      if(t_check!=0) {
        /*if expire time less than 15 minutes ago do not delete*/
        if(difftime(time(NULL), (time_t)t_check)<900) t_check=0;
      }

      if(t_check!=0) {
        /*if file activity less than 30 minutes ago, do not delete*/
        file_time=bfio_get_file_time(fn_compressed);
        if(((int32_t)file_time)!=-1) {
          if(difftime(time(NULL), file_time)<1800) t_check=0;
        }
      }
             
      if(t_check!=0) {
        /*** removed print to log... not deemed important. wmm 6/9/04
        **** sprintf(work,
        ****  "Warning: Lock on file (%s) has expired and being removed",
        ****  fn_compressed);
        **** cpslog_message(work);
        ***/
        bfio_zap_lock_file_rec(&lock_parms, i_rec);
        if(i_found<0) i_found=-2;
      }
    }
  }

  if(i_found!=-1) {
    bfio_write_lock_file("bfio_unlock_file", &lock_parms, n_lockfile_recs);
  }
  
  bfio_unlock_the_lockfile(lock_parms.fd);
  if(lock_parms.buff!=NULL) free(lock_parms.buff);

  if(i_found>=0) {
    return(0);
  } else {
    sprintf(work,
     "Warning:Requested file(%s)to be unlocked is not in lock file",
     lock_parms.file_name);
    cpslog_message(work);
    return(1);
  }
}

/********** BFIO_CHECK_LOCK_FILE *********
* check lock file  on specified file name
* returns lock_type
*  blank means file not in lock file
*  otherwise it is the lock type
*
* Written November 2001 by Charles C Burch
*****************************************/
char bfio_check_lock_file(char *lock_file, char *file_name_raw) {
  int n_lockfile_recs, i_rec;
  char lock_type, fn_compressed[260];
  char   *p_date, *p_time, *expire_time, locktype, *host, *pid, *file;
  struct bfio_lock_struct lock_parms;

  /*printf("check lock %s, lock file=%s\n",file_name_raw, lock_file);*/

  str_compress(lock_parms.file_name, file_name_raw);

  /* lock the lockfile, read contents */
  if((lock_parms.fd=bfio_lock_the_lockfile(lock_file, lock_parms.file_name, 
          lock_parms.lock_file))<0) {
    bfio_unlock_the_lockfile(lock_parms.fd);        /*bypass if no lockfile */
    return('N');
  }

  n_lockfile_recs=bfio_read_lock_file("bfio_check_lock_file", &lock_parms);
  bfio_unlock_the_lockfile(lock_parms.fd);

/*** Scan through lockfile looking for specified filename ***/
  lock_type=' ';
  for (i_rec=0; i_rec<n_lockfile_recs;i_rec++) {
    bfio_get_lock_file_rec(&lock_parms, i_rec, &p_date, &p_time, &expire_time,
        &locktype, &host, &pid, &file);
    /**  printf("file=%s, locktype=%c\n",file, lock_type);  **/
    str_compress(fn_compressed, file);
    if(strcmp(lock_parms.file_name,fn_compressed)==0){
      lock_type=locktype;
      break;
    }
  }
  free(lock_parms.buff);
  /**  printf("lock_type=%c\n",lock_type);  **/
  return(lock_type);
}

/********** BFIO_DUMP_LOCK_FILE_REC **********
* dump the lock file record
*
* Written December 2002 by Charles C Burch
*********************************************/
int bfio_dump_lock_file_rec(struct bfio_lock_struct *lock_parms, int i_rec) {
  char   fn_compressed[260], c_host[16], c_pid[9];
  char   *p_date, *p_time, *expire_time, locktype, *host, *pid, *file;

  bfio_get_lock_file_rec(lock_parms, i_rec, &p_date, &p_time, &expire_time,
    &locktype, &host, &pid, &file);
  str_compress(fn_compressed, file);
  if(fn_compressed[0]=='\0') return(0);
  
  str_compress(c_host,host);
  str_compress(c_pid, pid);
  printf("%s %s %c %-12s %5s %s\n",
    p_date, p_time, locktype,c_host,c_pid,fn_compressed);
  return(1);
}

/********** BFIO_DUMP_LOCK_FILE **********
* dump the lock file
*
* Written December 2000 by Charles C Burch
*****************************************/
void bfio_dump_lock_file(char *lockfile_in) {
  int    n_lockfile_recs, i_rec, num_lock_files, ifile, num_lines;
  char   lock_file[260];
  struct bfio_lock_struct lock_parms;

  num_lock_files=bfio_adjust_lockfile_name(lockfile_in, lock_file,0);
  num_lines=0;
  printf (
    "Dump of %s[-.%d]\n----expire time---- T ----host---- -pid- filename\n", 
    lock_file,num_lock_files-1);
 
  strcpy(lock_parms.file_name,"bfio_dump_lock_file");
  for(ifile=0; ifile<num_lock_files; ifile++) {
    bfio_adjust_lockfile_name(lockfile_in, lock_file, ifile);
    /*fprintf(stderr,"line=%d:LOCKFILE=%s\n",__LINE__,lock_file);*/
    lock_parms.fd=bfio_lock_the_lockfile(lock_file, lock_parms.file_name, 
        lock_parms.lock_file);
    n_lockfile_recs=bfio_read_lock_file(lock_parms.file_name, &lock_parms);
    bfio_unlock_the_lockfile(lock_parms.fd);
    
    for (i_rec=0; i_rec<n_lockfile_recs; i_rec++) {
      if(bfio_dump_lock_file_rec(&lock_parms, i_rec)>0) num_lines++;
    }

    free(lock_parms.buff);
  }  
  if(num_lines==0) printf("lock file is currently empty\n");
  return;
}

/*************** BFIO_DIR ****************
* do a dir command
*
* Written June 2003 by Charles C Burch
*****************************************/
void bfio_dir(char *dir, char* flags) {
  char dir_full[160], cmd[300];

  bfio_expand_file_name(dir, dir_full, sizeof(dir_full));
  if(flags[0]=='-') flags++;
  if(flags[0]=='\0') {
    sprintf(cmd,"ls %s",dir_full);
  } else {
    sprintf(cmd,"ls -%s %s", flags, dir_full);
  }
  system(cmd);
  return;
}

/*********************** BFIO_GET_FD ************************
* return fd of an opened file--return NULL if file not opened
*
* Written Novenber 2005 by Chuck Burch
************************************************************/
FILE *bfio_get_fd(int ifile) {
  if(ifile>=0 && ifile<bfio_info_num) {
   bfio_open_file(ifile);
   return(bfio_info[ifile]->fd);
  }
  return(NULL);
}

/******************** BFIO_ADD_DESC_FLAGS ************************
* add file descriptor flags
*   ifile is the file identifier from opening the file
*   flags is the value of flags to add to the file descriptor
*   returns <0 if error
*   returns =0 if no error
*
* Written October 2005 by Brian Macy/Chuck Burch
*****************************************************************/
int bfio_add_desc_flags(int ifile, int32_t flags) {
  int istat;
  int32_t fdflags;

  istat=0;
  pthread_mutex_lock(&bfio_io_mutex);

  if(ifile<0 || ifile>=bfio_info_num) {
    printf("Invalid file id(%d) called in bfio_add_desc_flags\n",ifile);
    printf("bfio_info_num = %d in bfio_add_desc_flags\n",bfio_info_num);
    istat=-1;
    goto EXIT;
  }

/* bfio_print_bfio_info("bfio_add_desc_flags", ifile); */

  if (bfio_info[ifile]->activity<(-1)) {
    printf("Attempt to access unopened file(%d) in bfio_add_desc_flags\n",
           ifile);
    istat=-2;
    goto EXIT;
  }

  if(flags==0) {
    istat=0;
    goto EXIT;
  }

  bfio_info[ifile]->desc_flags |= flags;

  /* Modify desc flags now if file actually opened otherwise 
     they will be changed when file gets opened*/

  if( bfio_info[ifile]->fd!=NULL) {
    fdflags = (int32_t) mtio_fcntl(bfio_info[ifile]->fileno, F_GETFL);
    if (fdflags == -1) {
      fprintf(stderr,"bfio_add_desc_flags(%s): get flags: ErrNo=%d [%s]\n",
       bfio_info[ifile]->fn, errno, strerror(errno));
      istat = -5;
      goto EXIT;
    }

    istat = mtio_fcntl(bfio_info[ifile]->fileno, F_SETFL, 
               fdflags | bfio_info[ifile]->desc_flags);
    if (istat == -1) {
      fprintf(stderr,"bfio_add_desc_flags(%s): set flags: ErrNo=%d [%s]\n",
       bfio_info[ifile]->fn, errno, strerror(errno));
    }
  }

/* bfio_print_bfio_info("bfio_add_desc_flags", ifile); */

EXIT:
  pthread_mutex_unlock(&bfio_io_mutex);
  return(istat);
}

/********************* BFIO_IS_FILE_NFS ***********************
* returns 1 if specified filename is a nfs file
*         0 if specified filename is not a nfs file
*         -1 if an error occurred
*         -2 if filename is invalid or inaccesible
*
* Note separate logic is used fopr linux versus other machines
* The #if maybe changed for differnt machines
* As of 11/17/2005 they seemto work for linux and solaris
*
* Written November 2005 by Charles Burch
**************************************************************/
int bfio_is_file_nfs(char * filename) {

#if (LINUX || LINUXA || LINUXI || LINUXP)
/******************** Code using linux statfs ***************/

#ifndef NFS_SUPER_MAGIC
/* including linux/nfs_fs.h created compile bugs */
#define NFS_SUPER_MAGIC 0x6969
#endif

  struct statfs fs;

  if(statfs(filename, &fs)<0) {
    if(errno==ENOENT) return(-2);
    fprintf(stderr,"bfio_is_file_nfs error in statfs:%s\n",strerror(errno));
    return(-1);
  }
  /* printf("ftype=%x\n",fs.f_type); */
  if(fs.f_type==NFS_SUPER_MAGIC) return(1);

#else
/******************** Code using solaris statvfs ***************/

  struct statvfs fs;

  if(statvfs(filename, &fs)<0) {
    if(errno==ENOENT) return(-2);
    fprintf(stderr,"bfio_is_file_nfs error in statvfs:%s\n",strerror(errno));
    return(-1);
  }

  /* printf("ftype=%s\n",fs.f_basetype); */
  if(strcmp(fs.f_basetype,"nfs")==0) return(1);

#endif
  return(0);
}

#ifdef __cplusplus
}
#endif


/*-------------------------------- end ------------------------------------*/
/*-------------------------------- end ------------------------------------*/
/*-------------------------------- end ------------------------------------*/
