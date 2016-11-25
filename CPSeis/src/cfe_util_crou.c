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
! Name       : cfe_util_crou
! Category   : stand-alone
! Written    : 2000-02-16   by: Donna K. Vunderink
! Revised    : 2002-02-26   by: Donna K. Vunderink
! Maturity   : beta
! Purpose    : CFE Utilities Module.
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
!  8. 2002-02-26  Vunderink    Added CFE_UTIL_ALPHALIB.
!  7. 2001-09-10  Vunderink    Changes required to rename TESTLIB to BETALIB.
!  6. 2001-03-15  Vunderink    Added cfe_util_sleep_c
!  5. 2000-08-15  Vunderink    Added brief_doc
!  4. 2000-06-14  Vunderink    Include time.h
!  3. 2000-05-09  Vunderink    Fixed bug define for cfe_util_file_datecmp_c, and
!                                added routine cfe_util_file_date_c
!  2. 2000-04-24  Vunderink    Added cfe_util_file_datecmp_c
!  1. 2000-02-16  Vunderink    Initial version.
!
!
!
!-------------------------------------------------------------------------------
!</history_doc>
****/


char cfe_util_crou_ident[100] = "$Id: cfe_util_crou.c,v 1.8 2002/02/26 21:42:16 Vunderink beta sps $";



#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <time.h>
#include <unistd.h>
#include <sys/types.h>
#include <sys/stat.h>
#include "c2f_interface.h"


#ifdef __cplusplus
extern "C" {
#endif


#ifdef NEED_UNDERSCORE
#define cfe_util_library_c           cfe_util_library_c_
#define cfe_util_file_datecmp_c      cfe_util_file_datecmp_c_
#define cfe_util_file_date_c         cfe_util_file_date_c_
#define cfe_util_sleep_c             cfe_util_sleep_c_
#endif

#ifdef NEED_CAPITALS
#define cfe_util_library_c           CFE_UTIL_LIBRARY_C
#define cfe_util_file_datecmp_c      CFE_UTIL_FILE_DATECMP_C
#define cfe_util_file_date_c         CFE_UTIL_FILE_DATE_C
#define cfe_util_sleep_c             CFE_UTIL_SLEEP_C
#endif


#define  CFE_UTIL_UNKNOWN     0
#define  CFE_UTIL_PRODLIB     1
#define  CFE_UTIL_BETALIB     2
#define  CFE_UTIL_ALPHALIB    3

#ifdef PRODLIB
#define LNKLIB  CFE_UTIL_PRODLIB
#elif BETALIB
#define LNKLIB  CFE_UTIL_BETALIB
#elif ALPHALIB
#define LNKLIB  CFE_UTIL_ALPHALIB
#else
#define LNKLIB  CFE_UTIL_UNKNOWN
#endif


INTEGER cfe_util_library_c ()
{
  return (INTEGER)LNKLIB;
}


INTEGER cfe_util_file_datecmp_c (char *file1, char *file2)
{
  int i;
  double d;
  struct stat buf1;
  struct stat buf2;

  i = stat(file1,&buf1);
  i = stat(file2,&buf2);
  d = difftime(buf1.st_mtime,buf2.st_mtime);
  if (d < 0.0)
    return -1;
  else if (d > 0.0)
    return 1;
  else
    return 0;
}


INTEGER cfe_util_file_date_c (char *file, char *datestr)
{
  int i;
  struct stat buf;
  struct tm *mtime;
  time_t t;

  i      = stat(file,&buf);
  mtime  = localtime(&buf.st_mtime);
  t      = strftime(datestr,11,"%Y-%m-%d",mtime);
  return 0;
}

#ifdef __cplusplus
}
#endif

void cfe_util_sleep_c (INTEGER *seconds)
{
  sleep(*seconds);
}

/*-------------------------------- end ------------------------------------*/
/*-------------------------------- end ------------------------------------*/
/*-------------------------------- end ------------------------------------*/
