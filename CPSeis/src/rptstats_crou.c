/****
!<CPS_v1 type=AUXILIARY_FILE"/>
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
!                        C P S  A U X I L I A R Y  F I L E
!
! Name       : rptstats_crou
! Category   : stand-alone
! Written    : 2002-10-09   by: Donna K. Vunderink
! Revised    : 2002-10-09   by: Donna K. Vunderink
! Maturity   : production
! Purpose    : Program to extract specific words from CPS report files.
! Portability: No known limitations.
!
!-------------------------------------------------------------------------------
!</brief_doc>


!<history_doc>
!-------------------------------------------------------------------------------
!                     AUXILIARY FILE REVISION HISTORY
!
!     Date        Author       Description
!     ----        ------       -----------
!  1. 2002-10-09  Vunderink    Initial version.
!
!-------------------------------------------------------------------------------
!</history_doc>
****/


char rptstats_crou_ident[100] = "$Id: rptstats_crou.c,v 1.1 2002/10/09 19:11:00 Vunderink prod sps $";


#include "c2f_interface.h"
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <dirent.h>
#include <fnmatch.h>
#include <sys/types.h>
#include <sys/stat.h>

#ifdef NEED_UNDERSCORE
#define rptstats_opendir     rptstats_opendir_
#define rptstats_readdir     rptstats_readdir_
#define rptstats_closedir    rptstats_closedir_
#endif

#ifdef NEED_CAPITALS
#define rptstats_opendir     RPTSTATS_OPENDIR
#define rptstats_readdir     RPTSTATS_READDIR
#define rptstats_closedir    RPTSTATS_CLOSEDIR
#endif

static DIR  *dirp;
static char *dirsav    = NULL;
static char *filtersav = NULL;

INTEGER rptstats_opendir (const char *filename)
{
  int   i;

  for (i=strlen(filename); i>0; i--) {
    if (filename[i-1] == 47) {
      dirsav = (char *) malloc(i);
      strncpy(dirsav,filename,i-1);
      filtersav = (char *) malloc(strlen(filename)+1);
      strcpy(filtersav,filename);
      break;
    }
  }
  if (dirsav == NULL) {
    dirsav = (char *) malloc(2);
    strcpy(dirsav,".");
    filtersav = (char *) malloc(strlen(filename)+3);
    strcpy(filtersav,"./");
    strcat(filtersav,filename);
  }

  dirp = opendir(dirsav);
  if (dirp == NULL) return -1;
  i = strlen(dirsav) + 1;
  return 0;
}

INTEGER rptstats_readdir (char *file)
{
  struct dirent *direntp;
  int i;
  int type;
  int val;
  struct stat buf;
  char *path;

  file[0] = 0;
  file[1] = 0;
  file[2] = 0;
  file[3] = 0;
  type = -1;
  while (type != 0) {
    direntp = readdir(dirp);
    if (direntp == NULL) return 1;
    strcpy(file,direntp->d_name);
    i = strlen(dirsav) + strlen(direntp->d_name) + 2;
    path = (char *) malloc(i);
    strcpy(path,dirsav);
    strcat(path,"/");
    strcat(path,direntp->d_name);
    val = stat (path,&buf);
    if (val < 0) val = lstat (path,&buf);
    if (val < 0)
      type = 1;
    else {
      if (S_ISDIR (buf.st_mode))
        type = 1;
      else {
        if (filtersav != NULL) {
          val = fnmatch(filtersav,path,FNM_PERIOD);
          if (val == 0)
            type = 0;
          else
            type = 1;
        }
      }
    }
    free(path);
  }
  return 0;
}


INTEGER rptstats_closedir (void)
{
  free(dirsav);
  free(filtersav);
  dirsav    = NULL;
  filtersav = NULL;
  (void) closedir(dirp);
  return 0;
}

