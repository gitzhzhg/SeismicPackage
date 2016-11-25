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
!                        C P S   P R I M I T I V E               
!
! Name       : cgetsys.h
! Category   : miscellaneous
! Written    : 1999-09-28   by: Donna K. Vunderink
! Revised    : 2007-12-11   by: Bill Menger
! Maturity   : beta
! Purpose    : A collection of C routines for getting system information.
! Portability: No known limitations.
!
!-------------------------------------------------------------------------------
!</brief_doc>

!<history_doc>
!-------------------------------------------------------------------------------
!                       HEADER FILE REVISION HISTORY
!
!     Date        Author       Description
!     ----        ------       -----------
! 10. 2007-12-11  Bill Menger  Added cgetsys_strnlen prototype
!  9. 2002-06-24  Vunderink    Added cgetsys_my_pid_memory
!  8. 2002-03-12  Vunderink    Added ALPHALIB to libraries
!  7. 2002-02-13  Vunderink    Added getsys_memory
!  6. 2001-09-10  Vunderink    Changes required to rename TESTLIB to BETALIB
!  5. 2001-04-05  Vunderink    Added cgetsys_cpu_speed, cgetsys_cd_version and
!                                cgetsys_os_version
!  4. 2000-02-01  Vunderink    Added cgetsys_library
!  3. 1999-12-30  Brad Kruse   Added cgetsys_time, cgetsys_utime, cgetsys_stime 
!  2. 1999-11-30  Vunderink    Added cgetsys_pid.
!  1. 1999-09-28  Vunderink    Initial version.
!
!-------------------------------------------------------------------------------
!</history_doc>
****/


/*--------------------------- start of coding ------------------------------*/
/*--------------------------- start of coding ------------------------------*/
/*--------------------------- start of coding ------------------------------*/


#ifndef _CGETSYS_H_
#define _CGETSYS_H_


#ifdef __cplusplus
extern "C" {
#endif


#include "c2f_interface.h"
#include "cnfg.h"
#include <string.h>
#include <ctype.h>
#include <dirent.h>
#include <errno.h>
#include <limits.h>
#include <math.h>
#include <pwd.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sys/param.h>
#include <sys/resource.h>
#include <sys/time.h>
#include <sys/types.h>
#include <sys/utsname.h>
#include <time.h>
#include <unistd.h>


#ifdef NEED_UNDERSCORE
#define cgetsys_hostname           cgetsys_hostname_
#define cgetsys_username           cgetsys_username_
#define cgetsys_current_dir        cgetsys_current_dir_
#define cgetsys_env                cgetsys_env_
#define cgetsys_ostype             cgetsys_ostype_
#define cgetsys_machine            cgetsys_machine_
#define cgetsys_netinfo            cgetsys_netinfo_
#define cgetsys_seconds            cgetsys_seconds_
#define cgetsys_usage              cgetsys_usage_
#define cgetsys_pid                cgetsys_pid_
#define cgetsys_time               cgetsys_time_
#define cgetsys_utime              cgetsys_utime_
#define cgetsys_stime              cgetsys_stime_
#define cgetsys_library            cgetsys_library_
#define cgetsys_cpu_speed          cgetsys_cpu_speed_
#define cgetsys_os_version         cgetsys_os_version_
#define cgetsys_memory             cgetsys_memory_
#define cgetsys_my_pid_memory      cgetsys_my_pid_memory_
#endif

#ifdef NEED_CAPITALS
#define cgetsys_hostname           CGETSYS_HOSTNAME
#define cgetsys_username           CGETSYS_USERNAME
#define cgetsys_current_dir        CGETSYS_CURRENT_DIR
#define cgetsys_env                CGETSYS_ENV
#define cgetsys_ostype             CGETSYS_OSTYPE
#define cgetsys_machine            CGETSYS_MACHINE
#define cgetsys_netinfo            CGETSYS_NETINFO
#define cgetsys_seconds            CGETSYS_SECONDS
#define cgetsys_usage              CGETSYS_USAGE
#define cgetsys_pid                CGETSYS_PID
#define cgetsys_time               CGETSYS_TIME
#define cgetsys_utime              CGETSYS_UTIME
#define cgetsys_stime              CGETSYS_STIME
#define cgetsys_library            CGETSYS_LIBRARY
#define cgetsys_cpu_speed          CGETSYS_CPU_SPEED
#define cgetsys_os_version         CGETSYS_OS_VERSION
#define cgetsys_memory             CGETSYS_MEMORY
#define cgetsys_my_pid_memory      CGETSYS_MY_PID_MEMORY
#endif


#define  CGETSYS_UNKNOWN     0
#define  CGETSYS_CRAYPVP     1
#define  CGETSYS_VMS         2
#define  CGETSYS_ULTRIX      3
#define  CGETSYS_IBM         4
#define  CGETSYS_SOLARIS     5
#define  CGETSYS_HP          6
#define  CGETSYS_SGI         7
#define  CGETSYS_CRAYMPP     8
#define  CGETSYS_INTELSOL    9
#define  CGETSYS_LINUX      10


#ifdef _CRAY1
#define OSTYPE  CGETSYS_CRAYPVP 
#elif VMS
#define OSTYPE  CGETSYS_VMS     
#elif ultrix
#define OSTYPE  CGETSYS_ULTRIX   
#elif _AIX
#define OSTYPE  CGETSYS_IBM
#elif linux
#define OSTYPE  CGETSYS_LINUX
#elif i386                         /* must precede sun and follow linux */
#define OSTYPE  CGETSYS_INTELSOL   
#elif sun
#define OSTYPE  CGETSYS_SOLARIS 
#elif __hpux
#define OSTYPE  CGETSYS_HP   
#elif __sgi
#define OSTYPE  CGETSYS_SGI   
#elif _CRAYMPP
#define OSTYPE  CGETSYS_CRAYMPP
#else
#define OSTYPE  CGETSYS_UNKNOWN
#endif

#define  CGETSYS_PRODLIB     1
#define  CGETSYS_BETALIB     2
#define  CGETSYS_ALPHALIB    3

#ifdef PRODLIB
#define LNKLIB  CGETSYS_PRODLIB 
#elif BETALIB
#define LNKLIB  CGETSYS_BETALIB     
#elif ALPHALIB
#define LNKLIB  CGETSYS_ALPHALIB     
#else
#define LNKLIB  CGETSYS_UNKNOWN
#endif


/*------------------------ start of prototypes ---------------------------*/
/*------------------------ start of prototypes ---------------------------*/
/*------------------------ start of prototypes ---------------------------*/


char   *cgetsys_cd_version   (void);
INTEGER cgetsys_cpu_speed    (void);
INTEGER cgetsys_current_dir  (char *name, INTEGER *namelen);
INTEGER cgetsys_env          (const char *name, char *value, INTEGER *valuelen);
INTEGER cgetsys_hostname     (char *name, INTEGER *namelen);
INTEGER cgetsys_library      (void);
INTEGER cgetsys_machine      (char *machine);
void    cgetsys_memory       (INTEGER *ncpu, INTEGER *pagesz, INTEGER *physpgs, 
                              INTEGER *avphyspgs);
void    cgetsys_my_pid_memory(INTEGER *vm, INTEGER *rm);
INTEGER cgetsys_netinfo      (char *node, char *user, char *path);
INTEGER cgetsys_os_version   (char *verision);
INTEGER cgetsys_ostype       (void);
INTEGER cgetsys_pid          (void);
REAL    cgetsys_seconds      (void);
DOUBLE  cgetsys_stime        (void);
INTEGER cgetsys_time         (DOUBLE *utime, DOUBLE *stime);
INTEGER cgetsys_usage        (INTEGER *maxrss, INTEGER *nswap, INTEGER *minflt,
                              INTEGER *majflt, INTEGER *inblock, 
                              INTEGER *oublock, REAL *utime, REAL *stime);
INTEGER cgetsys_username     (char *name, INTEGER *namelen);
DOUBLE  cgetsys_utime        (void);

INTEGER cgetsys_strnlen(char*s, INTEGER maxlen);

/*------------------------- end of information ---------------------------*/
/*------------------------- end of information ---------------------------*/
/*------------------------- end of information ---------------------------*/


#ifdef __cplusplus
}
#endif


#endif


/*-------------------------------- end ------------------------------------*/
/*-------------------------------- end ------------------------------------*/
/*-------------------------------- end ------------------------------------*/

