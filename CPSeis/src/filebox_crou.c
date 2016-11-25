/***
!<CPS_v1 type="AUXILIARY_FILE"/>
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
!                           C P S  P R I M I T I V E
!
! Name       : filebox_crou
! Category   : io
! Written    : 1999-11-01   by: Donna K. Vunderink
! Revised    : 2005-05-31   by: Tom Stoeckley
! Maturity   : production
! Purpose    : File Selection Box Popup.
! Portability: No known limitations.
!
!-------------------------------------------------------------------------------
!</brief_doc>


!<history_doc>
!-------------------------------------------------------------------------------
!                                REVISION HISTORY
!
!     Date        Author       Description
!     ----        ------       -----------
!  9. 2005-05-31  Stoeckley    Fixed so will compile with C++ (again).
!  8. 2005-03-22  Stoeckley    Fixed so will compile with C++.
!  7. 2004-01-29  SMCook       Added function readdir_lstat, which stat's files
!                               using lstat instead of stat.  Front end does not
!                               seem to hang using lstat as it does with stat.
!  6. 2001-06-26  Vunderink    Moved from cfe into testlib. PRODUCTION.
!  5. 2000-09-14  Vunderink    Make sure path is deallocated and do not
!                                deallocate filtersav.
!  4. 2000-08-15  Vunderink    Added brief_doc
!  3. 2000-07-21  Vunderink    Added returns to eliminate compiler warnings
!  2. 2000-03-09  Vunderink    Fix filebox_sort_key to compare strings
!  1. 1999-11-01  Vunderink    Initial version.
!
!-------------------------------------------------------------------------------
!</history_doc>
****/


/*--------------------------- start of coding ------------------------------*/
/*--------------------------- start of coding ------------------------------*/
/*--------------------------- start of coding ------------------------------*/


         /* The functions in this file are called from filebox.f90  */
         /* and are considered to be part of the filebox primitive. */
         /* These functions are called only from filebox.f90 and    */
         /* should be considered private.                           */

char filebox_crou_ident[100] = 
"$Id: filebox_crou.c,v 1.9 2005/05/31 13:04:08 Stoeckley prod sps $";

#include "c2f_interface.h"
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <dirent.h>
#include <fnmatch.h>
#include <sys/types.h>
#include <sys/stat.h>

#ifdef NEED_UNDERSCORE
#define filebox_opendir         filebox_opendir_
#define filebox_readdir_general filebox_readdir_general_
#define filebox_readdir_lstat   filebox_readdir_lstat_
#define filebox_readdir         filebox_readdir_
#define filebox_filter          filebox_filter_
#define filebox_closedir        filebox_closedir_
#define filebox_sort            filebox_sort_
#endif

#ifdef NEED_CAPITALS
#define filebox_opendir         FILEBOX_OPENDIR
#define filebox_readdir_general FILEBOX_READDIR_GENERAL
#define filebox_readdir_lstat   FILEBOX_READDIR_LSTAT
#define filebox_readdir         FILEBOX_READDIR
#define filebox_filter          FILEBOX_FILTER
#define filebox_closedir        FILEBOX_CLOSEDIR
#define filebox_sort            FILEBOX_SORT
#endif



static DIR *dirp;
static char *dirsav;
static char *filtersav;

#ifdef __cplusplus
extern "C" {
#endif

INTEGER filebox_opendir (const char *dir)
{
  int i;

  dirp = opendir(dir);
  if (dirp == NULL) return -1;
  i = strlen(dir) + 1;
  dirsav = (char *) malloc(i);
  strcpy(dirsav,dir);
  return 0;
}


INTEGER filebox_filter (const char *filter)
{
  int i;

  i = strlen(filter) + 1;
  if (filtersav != NULL) free(filtersav);
  filtersav = (char *) malloc(i);
  strcpy(filtersav,filter);
  return 0;
}


/* generalized signature version -- see below for convenience versions that
   allow you to easily choose between stat and lstat */
INTEGER filebox_readdir_general (char *file, int *type, int use_lstat)
{
  struct dirent *direntp;
  int i;
  int val;
  struct stat buf;
  char *path;

  file[0] = 0;
  file[1] = 0;
  file[2] = 0;
  file[3] = 0;
  *type = -1;
  direntp = readdir(dirp);
  if (direntp == NULL) return 1;
  strcpy(file,direntp->d_name);
  i = strlen(dirsav) + strlen(direntp->d_name) + 2;
  path = (char *) malloc(i);
  strcpy(path,dirsav);
  strcat(path,"/");
  strcat(path,direntp->d_name);

/* using lstat seems to prevent front end lockups and seems really fast also! */
  if(use_lstat)
    val = lstat (path,&buf);
  else
    val = stat (path,&buf);

  if (val < 0) val = lstat (path,&buf);
  if (val < 0)
    *type = -1;
  else {
    if (S_ISDIR (buf.st_mode))
      *type = 1;
    else {
      if (filtersav != NULL) {
        val = fnmatch(filtersav,path,FNM_PERIOD);
        if (val != 0) {
          free(path);
          return 1;
        }
        *type = 0;
      }
    }
  }
  free(path);
  return 0;
}
INTEGER filebox_readdir_lstat (char *file, int *type)
{
  return filebox_readdir_general(file, type, 1);
}
INTEGER filebox_readdir (char *file, int *type)  /* old argument signature   */
{
  return filebox_readdir_general(file, type, 0); /* old behavior (not lstat) */
}


INTEGER filebox_closedir (void)
{
  free(dirsav);
  (void) closedir(dirp);
  return 0;
}

int filebox_sort_key (const void *i, const void *j)
{
/*
  return strcmp(i,j);
  fixed below by Tom Stoeckley 2005-03-22.
*/
  return strcmp((const char*)i,(const char*)j);
}

INTEGER filebox_sort (char *files, int *size, int *count)
{
  qsort (files, *count, *size, filebox_sort_key);
  return 0;
}

#ifdef __cplusplus
}
#endif

