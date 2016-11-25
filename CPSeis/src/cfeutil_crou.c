/****
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
!                        C P S  P R I M I T I V E   F I L E
!
! Name       : cfeutil_crou
! Category   : cfe
! Written    : 2000-02-16   by: Donna K. Vunderink
! Revised    : 2005-10-24   by: Tom Stoeckley
! Maturity   : production
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
! 12. 2005-10-24  Stoeckley    Fix to compile with C++.
! 11. 2003-11-18  Selzler      Resolved SGI compiler warning (unused variable).
! 10. 2003-09-29  Stoeckley    Added cfeutil_get_platform_c.
!  9. 2003-09-15  Stoeckley    Changed name from cfe_util to cfeutil; change
!                               category to cfe; add cfeutil_is_custom_c.
!  8. 2002-02-26  Vunderink    Added CFEUTIL_ALPHALIB.
!  7. 2001-09-10  Vunderink    Changes required to rename TESTLIB to BETALIB.
!  6. 2001-03-15  Vunderink    Added cfeutil_sleep_c
!  5. 2000-08-15  Vunderink    Added brief_doc
!  4. 2000-06-14  Vunderink    Include time.h
!  3. 2000-05-09  Vunderink    Fixed bug define for cfeutil_file_datecmp_c, and
!                                added routine cfeutil_file_date_c
!  2. 2000-04-24  Vunderink    Added cfeutil_file_datecmp_c
!  1. 2000-02-16  Vunderink    Initial version.
!
!
!
!-------------------------------------------------------------------------------
!</history_doc>
****/


char cfeutil_crou_ident[100] = "$Id: cfeutil_crou.c,v 1.12 2005/10/24 11:32:17 Stoeckley prod sps $";



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
#define cfeutil_get_platform_c      cfeutil_get_platform_c_
#define cfeutil_is_custom_c         cfeutil_is_custom_c_
#define cfeutil_library_c           cfeutil_library_c_
#define cfeutil_file_datecmp_c      cfeutil_file_datecmp_c_
#define cfeutil_file_date_c         cfeutil_file_date_c_
#define cfeutil_sleep_c             cfeutil_sleep_c_
#endif

#ifdef NEED_CAPITALS
#define cfeutil_get_platform_c      CFEUTIL_GET_PLATFORM_C
#define cfeutil_is_custom_c         CFEUTIL_IS_CUSTOM_C
#define cfeutil_library_c           CFEUTIL_LIBRARY_C
#define cfeutil_file_datecmp_c      CFEUTIL_FILE_DATECMP_C
#define cfeutil_file_date_c         CFEUTIL_FILE_DATE_C
#define cfeutil_sleep_c             CFEUTIL_SLEEP_C
#endif


#define  CFEUTIL_UNKNOWN     0
#define  CFEUTIL_PRODLIB     1
#define  CFEUTIL_BETALIB     2
#define  CFEUTIL_ALPHALIB    3

#ifdef PRODLIB
#define LNKLIB  CFEUTIL_PRODLIB
#elif BETALIB
#define LNKLIB  CFEUTIL_BETALIB
#elif ALPHALIB
#define LNKLIB  CFEUTIL_ALPHALIB
#else
#define LNKLIB  CFEUTIL_UNKNOWN
#endif


void cfeutil_get_platform_c (char *platform)
{
#ifdef PLATFORM
  strcpy(platform, PLATFORM);
#else
  strcpy(platform, "");
#endif
}


INTEGER cfeutil_is_custom_c ()
{
#ifdef CUSTOM
  return (INTEGER)1;
#else
  return (INTEGER)0;
#endif
}


INTEGER cfeutil_library_c ()
{
  return (INTEGER)LNKLIB;
}


INTEGER cfeutil_file_datecmp_c (char *file1, char *file2)
{
  double d;
  struct stat buf1;
  struct stat buf2;

  stat(file1,&buf1);
  stat(file2,&buf2);
  d = difftime(buf1.st_mtime,buf2.st_mtime);
  if (d < 0.0)
    return -1;
  else if (d > 0.0)
    return 1;
  else
    return 0;
}


INTEGER cfeutil_file_date_c (char *file, char *datestr)
{
  struct stat buf;
  struct tm *mtime;

  stat(file,&buf);
  mtime  = localtime(&buf.st_mtime);
  strftime(datestr,11,"%Y-%m-%d",mtime);
  return 0;
}

void cfeutil_sleep_c (INTEGER *seconds)
{
  sleep(*seconds);
}

#ifdef __cplusplus
}
#endif

/*-------------------------------- end ------------------------------------*/
/*-------------------------------- end ------------------------------------*/
/*-------------------------------- end ------------------------------------*/
