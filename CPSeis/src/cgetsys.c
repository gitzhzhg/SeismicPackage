/****
!<CPS_v1 type="PRIMITIVE"/>
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
! Name       : cgetsys 
! Category   : miscellaneous
! Written    : 1999-07-01   by: Donna K. Vunderink
! Revised    : 2007-12-11   by: Bill Menger
! Maturity   : beta
! Purpose    : A collection of C routines for getting system information.
! Portability: No known limitations.
!
!-------------------------------------------------------------------------------
!</brief_doc>

!<descript_doc>
!-------------------------------------------------------------------------------
!                         GENERAL DESCRIPTION                   
!
! This primitive is a C wrapper for a collection of C routines for getting
! system information so that they can be called from FORTRAN 90.
!
! Function                       Description
! --------                       -----------
! cgetsys_cd_version             Get CD distribution for OS
! cgetsys_cpu_speed              Get speed of cpu
! cgetsys_current_dir            Get current working directory
! cgetsys_env                    Get value for an evironment variable
! cgetsys_hostname               Get current host name
! cgetsys_library                Get CPS library used for link
! cgetsys_machine                Get operating system type of a given node
! cgetsys_memory                 Get number of cpus and total memory
! cgetsys_my_pid_memory          Get memory usage of the current process
! cgetsys_netinfo                Get items from _netinfo card
! cgetsys_os_version             Get OS release
! cgetsys_ostype                 Get current operating system type
! cgetsys_pid                    Get process ID
! cgetsys_seconds                Get processor time used by the current process
! cgetsys_stime                  Get system clock value in double precision secs
! cgetsys_time                   Get combined user and system clock values 
!                                in double precision secs
! cgetsys_usage                  Get information about resource utilization
! cgetsys_username               Get current user name
! cgetsys_utime                  Get user clock value in double precision secs
!-------------------------------------------------------------------------------
!</descript_doc>

!<calling_doc>
!-------------------------------------------------------------------------------
!                      INPUT AND OUTPUT ARGUMENTS               
!
!
! For each subroutine or function documented below, each argument is
! flagged as follows:
!      i = value required upon INPUT.
!      o = value set by the routine upon OUTPUT.
!      b = value BOTH required upon input and changed upon output.
!
!-------------------------------------------------------------------------------
!                          CALLING SEQUENCE                      
!
!
! To get current host name
!
!                  o                           o       i
!                istat = cgetsys_hostname    (host, hostlen)
!
! char*    host     = current host name
! INTEGER* hostlen  = size of the host array
! INTEGER  istat    = return status
!                     0 = no error occurred
!                    -1 = an error occurred
!
!-------------------------------------------------------------------------------
! To get current user name
!
!                  o                           o       i
!                istat = cgetsys_username    (user, userlen)
!
! char*    user     = current user name
! INTEGER* userlen  = size of the user array
! INTEGER  istat    = return status
!                     0 = no error occurred
!                    -1 = an error occurred
!
!-------------------------------------------------------------------------------
! To get current working directory
!
!                  o                           o       i
!                istat = cgetsys_current_dir (cdir, cdirlen)
!
! char* cdir        = current directory name
! INTEGER* cdirlen  = size of the cdir array
! INTEGER  istat    = return status
!                     0 = no error occurred
!                    -1 = an error occurred
!
!-------------------------------------------------------------------------------
! To get value of an environment variable
!
!                  o                    i     o       i
!                istat = cgetsys_env (name, value, valuelen)
!
! char*    name     = environment variable name
! char*    value    = environment variable vlaue
! INTEGER* valuelen = size of the value array
! INTEGER  istat    = return status
!                      0 = no error occurred
!                     -1 = an error occurred
!
!-------------------------------------------------------------------------------
! To get current operating system type
!
!                  o
!                ostyp = cgetsys_ostype ()
!
! INTEGER  ostyp    = type of operating system (one of the defined macros
!                     listed here)
!
!                         CGETSYS_UNKNOWN   =  Unknown
!                         CGETSYS_CRAYPVP   =  Cray PVP
!                         CGETSYS_VMS       =  DEC VMS
!                         CGETSYS_ULTRIX    =  DEC Ultrix
!                         CGETSYS_IBM       =  IBM
!                         CGETSYS_SOLARIS   =  Solaris
!                         CGETSYS_HP        =  HP
!                         CGETSYS_SGI       =  SGI
!                         CGETSYS_CRAYMPP   =  Cray MPP
!                         CGETSYS_INTELSOL  =  Intel Solaris
!                         CGETSYS_LINUX     =  Linux
!
!-------------------------------------------------------------------------------
! To get operating system type of a given node
!
!                  o                         i
!                ostyp = cgetsys_machine (machine)
!
! char*    machine  = machine node name
! INTEGER  ostyp    = type of operating system (one of the defined macros
!                     listed here)
!
!                         CGETSYS_UNKNOWN   =  Unknown
!                         CGETSYS_CRAYPVP   =  Cray PVP
!                         CGETSYS_VMS       =  DEC VMS
!                         CGETSYS_ULTRIX    =  DEC Ultrix
!                         CGETSYS_IBM       =  IBM
!                         CGETSYS_SOLARIS   =  Solaris
!                         CGETSYS_HP        =  HP
!                         CGETSYS_SGI       =  SGI
!                         CGETSYS_CRAYMPP   =  Cray MPP
!                         CGETSYS_INTELSOL  =  Intel Solaris
!                         CGETSYS_LINUX     =  Linux
!
!-------------------------------------------------------------------------------
! To get items from _netinfo card
!
!                  o                         o         o         o
!                 num  = cgetsys_netinfo (net_node, net_user, net_path)
!
! char*    net_node = node name from _netinfo card
! char*    net_user = user name from _netinfo card
! char*    net_path = path name from _netinfo card
! INTEGER  num      = number of values returned
!
!-------------------------------------------------------------------------------
! To get processor time used by the current process
!
!                  o
!                ptime = cgetsys_seconds()
!
! REAL ptime  = processor time in seconds
!
!                o                  o      o
!                ier = cgetsys_time(utime, stime)
!
! DOUBLE utime = process user time, in seconds
! DOUBLE stime = process system time, in seconds
!
!                o
!                utime = cgetsys_utime()
!
! DOUBLE utime = process user time, in seconds
!
!                o
!                utime = cgetsys_stime()
!
! DOUBLE stime = process system time, in seconds
!-------------------------------------------------------------------------------
! To get information about process resource utilization
!
!                  o                       o     o     o      o       o
!                istat = cgetsys_usage (maxrss,nswap,minflt,majflt,inblock,
!                                          o       o     o
!                                       outblock,utime,stime)
!
! INTEGER* maxrss   = the maximum resident set size
! INTEGER* nswap    = the number of swaps
! INTEGER* minflt   = the number of page faults not requiring physical I/O
! INTEGER* majflt   = the number of page faults requiring physical I/O
! INTEGER* inblock  = the number of block input operations
! INTEGER* outblock = the number of block output operations
! REAL*    utime    = the user time used
! REAL*    stime    = the system time used
! INTEGER  istat    = return status
!                      0 = no error occurred
!                     -1 = an error occurred
!
!-------------------------------------------------------------------------------
! To get process ID of the current process
!
!                 o
!                pid = cgetsys_pid()
!
! INTEGER pid  = processor ID
!
!-------------------------------------------------------------------------------
! To get CPS library used for link
!
!                  o
!                lnklib = cgetsys_library ()
!
! INTEGER  lnklib   =  library (one of the defined macros listed here)
!
!                         CGETSYS_UNKNOWN   =  Unknown
!                         CGETSYS_PRODLIB   =  CPS production library
!                         CGETSYS_BETALIB   =  CPS beta library
!
!-------------------------------------------------------------------------------
! To get the CPU speed
!
!                  o
!                speed = cgetsys_cpu_speed ()
!
! INTEGER  speed  =  CPU speed in megahertz.
!                    returns 0 if CPU speed not available.
!
!-------------------------------------------------------------------------------
! To get the CD distribution number for the operating system
!
!                   o
!                version = cgetsys_cd_version ()
! 
! char *version  = CD distribution number
!
!-------------------------------------------------------------------------------
! To get the operating system release
!
!                  o                            o
!                istat = cgetsys_os_version (version)
! 
! char*    version  = Operating System release
! INTEGER  istat    = return status
!                     0 = no error occurred
!                    -1 = an error occurred
!
!-------------------------------------------------------------------------------
! To get the number of cpus and total memory
!
!                                  o      o        o
!                cgetsys_memory (ncpus, pagesz, physpgs, avphyspgs)
!
! INTEGER* ncpus     = number of cpus
! INTEGER* pagesz    = memory page size
! INTEGER* physpgs   = total number of pages of physical memory
! INTEGER* avphyspgs = number of currently available pages of physical memory
!
!-------------------------------------------------------------------------------
! To get the memory usage of the current process (LINUX only)
!
!                                         o     o 
!                cgetsys_my_pid_memory (vsize, rss)
!
! INTEGER* vsize = virtual memory size
! INTEGER* rss   = resident set size
!
!-------------------------------------------------------------------------------
!</calling_doc>

!<advice_doc>
!-------------------------------------------------------------------------------
!                            ADVICE FOR USERS                    
!
!
!-------------------------------------------------------------------------------
!</advice_doc>

!<history_doc>
!-------------------------------------------------------------------------------
!                           REVISION HISTORY                     
!     Date        Author       Description
!     ----        ------       -----------
! 19. 2007-12-11  Bill Menger  Added function cgetsys_strnlen and reinserted...
!     2007-11-29  Gunther      Add checks for the length of a string passed in
!                               from Fortran.  This fixed a bug when I tried
!                               starting OpenCPS in a directory with a long
!                               path name.
! 18. 2007-03-27  Corn         Upgrade to 64 bit architecture. Basically
!                              change long to INTEGER.
! 17. 2004-03-15  Goodger      Remove the code in cgetsys_netinfo that 
!                              converts the characters to lower case.  It
!                              is legitimate to have upper case characters.
! 16. 2003-10-20  Selzler      Removed hardwired references to "poep" and
!                              prefixes for machine names.
! 15. 2003-05-19  Stoeckley    Rearrange code in several places to eliminate
!                               benign messages related to #ifdef blocks and
!                               unused variables on sun and sgi.
! 14. 2003-05-16  Stoeckley    Modify cgetsys_memory for the SGI.
! 13. 2003-04-10  Vunderink    Fixed linux_types and solaris_types array
!                                dimensions.
! 12. 2002-09-24  Vunderink    Modified cgetsys_machine to check cps_config file
! 11. 2002-06-05  Vunderink    Added cgetsys_my_pid_memory
! 10. 2002-02-13  Vunderink    Added cgetsys_memory
!  9. 2001-09-10  Vunderink    Changes required to rename TESTLIB to BETALIB
!  8. 2001-04-05  Vunderink    Added cgetsys_cpu_speed, cgetsys_cd_version and
!                                cgetsys_os_version
!  7. 2001-04-03  Vunderink    Add new linux nodes
!  6. 2000-10-09  Vunderink    Eliminate compiler warnings, add new linux nodes,
!                                add strip tmpmnt from current_dir
!  5. 2000-02-01  Vunderink    Added cgetsys_library
!  4. 1999-12-30  Brad Kruse   Added cgetsys_time, cgetsys_utime, cgetsys_stime
!                              Removed obsolete XML tags 
!  3. 1999-11-30  Vunderink    Added cgetsys_pid
!  2. 1999-09-28  Vunderink    Fixed cgetsys_ostype.  Added cgetsys_env,
!                              cgetsys_machine, and cgetsys_usage.  Modified to
!                              use cgetsys.h header file.
!  1. 1999-07-01  Vunderink    Initial version.
!
!-------------------------------------------------------------------------------
!</history_doc>

!<portability_doc>
!-------------------------------------------------------------------------------
!                         PORTABILITY LIMITATIONS                
!
! No known limitations.
!
!-------------------------------------------------------------------------------
!</portability_doc>

!<compile_doc>
!-------------------------------------------------------------------------------
!                      SPECIAL COMPILING REQUIREMENTS             
!
! Requires -DLINUX on a linux platform.
! Requires -DPRODLIB when compiling for CPS production library.
! Requires -DBETALIB when compiling for CPS beta       library.
!
!-------------------------------------------------------------------------------
!</compile_doc>

!<algorithm_doc>
!-------------------------------------------------------------------------------
!                   ALGORITHM DESCRIPTION FOR DEVELOPERS          
!
!
!-------------------------------------------------------------------------------
!</algorithm_doc>

!<programming_doc>
!-------------------------------------------------------------------------------
!                           PROGRAMMING NOTES                    
!
!
!-------------------------------------------------------------------------------
!</programming_doc>
****/


#include "cgetsys.h"


#define PROC_CPUINFO "/proc/cpuinfo"
#define BUF_LENGTH 256
#define MAX_CNFG_ENTRIES 20

char cgetsys_ident[100] = 
                  "$Id: cgetsys.c,v 1.19 2007/12/12 15:06:40 Menger beta sps $";
 
static int  nlinux_types = 0;
static int  nsolaris_types = 0;
static char linux_types[MAX_CNFG_ENTRIES][41];
static char solaris_types[MAX_CNFG_ENTRIES][41];
static char readcnfg = 0;

INTEGER cgetsys_hostname (char *name, INTEGER *namelen)
{
  int istat1;

  istat1 = gethostname(name, (int) *namelen);
  return((INTEGER) istat1);
}


INTEGER cgetsys_username (char *name, INTEGER *namelen)
{
  INTEGER istat;
  struct passwd *p;

  p = getpwuid(geteuid());
  strcpy(name,p->pw_name);
  if (p->pw_name == NULL)
     istat = -1;
  else
     istat =  0;
  return(istat);
}


INTEGER cgetsys_current_dir (char *name, INTEGER *namelen)
{
  char tmp[PATH_MAX+1];
  char *tmpmnt;
  char *istat1;
  INTEGER istat;
  INTEGER len;

  istat1 = getcwd(tmp, PATH_MAX+1);
  if (istat1 == NULL)
     istat = -1;
  else
     istat =  0;
  tmpmnt = strstr(tmp,"/tmpmnt/");
  if (tmpmnt == NULL) {
    strncpy(name,tmp,*namelen);
  }
  else {
    tmpmnt = tmpmnt+7;
    strncpy(name,tmpmnt,*namelen-7);
  }
  len = cgetsys_strnlen(name,*namelen);
  *namelen = (INTEGER) len;
  return(istat);
}

 
INTEGER cgetsys_env (const char *name, char *value, INTEGER *valuelen)
{
  INTEGER istat;
  char *str;

  str = getenv(name);
  if (str == NULL) {
     istat = -1;
     strcpy(value," ");
  }
  else {
     istat =  0;
     strncpy(value,str,*valuelen);
  }
  return(istat);
}

 
INTEGER cgetsys_ostype ()
{
  return (INTEGER)OSTYPE;
}


INTEGER cgetsys_machine (char *machine)
{
  char *str;
  int  nstr;
  int  i;
  int  ostype;
  char cnfg_str[41];

  if (readcnfg == 0) {
    readcnfg = 1;
    for (i=0; i<MAX_CNFG_ENTRIES; i++) {
       sprintf(cnfg_str,"CGETSYS_LINUX_MACHINE%d",i+1);
       strcpy(linux_types[i],cnfg_get_value_c(cnfg_str));
       if (strlen(linux_types[i]) != 0) nlinux_types++;
    }
    for (i=0; i<MAX_CNFG_ENTRIES; i++) {
       sprintf(cnfg_str,"CGETSYS_SOLARIS_MACHINE%d",i+1);
       strcpy(solaris_types[i],cnfg_get_value_c(cnfg_str));
       if (strlen(solaris_types[i]) != 0) nsolaris_types++;
    }
  }

  nstr = strlen(machine);
  nstr++;
  str = (char *) malloc(nstr);
  i = 0;
  while (machine[i]) {
    str[i] = tolower(machine[i]);
    i++;
  }
  str[i] = '\0';

  if (nlinux_types > 0) {
    for (i=0; i<nlinux_types; i++) {
      if (!strncmp(str,linux_types[i],strlen(linux_types[i]))) {
        ostype = CGETSYS_LINUX;
        free(str);
        return (INTEGER)ostype;
      }
    }
  }
        
  if (nsolaris_types > 0) {
    for (i=0; i<nsolaris_types; i++) {
      if (!strncmp(str,solaris_types[i],strlen(solaris_types[i]))) {
        ostype = CGETSYS_SOLARIS;
        free(str);
        return (INTEGER)ostype;
      }
    }
  }
        
  ostype = CGETSYS_UNKNOWN;

  free(str);
  return (INTEGER)ostype;
}


INTEGER cgetsys_netinfo (char *node, char *user, char *path)
{
  FILE *filePtr = NULL;
  int i;
  char card[256];

  user[0] = '\0';
  node[0] = '\0';
  path[0] = '\0';

  filePtr = fopen("_netinfo","r");
  if (filePtr == NULL) return 0;

  i = fscanf(filePtr,"%255[^\n]",card);
  fclose(filePtr);

  /*  upper case is OK  rev 17
    i = 0;
  while (card[i] != 0) {
    card[i] = tolower(card[i]);
    i++;
    } */

  i = sscanf(card,"%s %s %s",user,node,path);
  if (i != 3) {
     user[0] = '\0';
     i = sscanf(card,"%s::%s",node,path);
     if (i != 2) {
        user[0] = '\0';
        node[0] = '\0';
        path[0] = '\0';
        return 0;
     }
     else
        return 2;
  }
  else
     return 3;
}


REAL cgetsys_seconds()
{
#ifdef CRAY
  float   div;
#endif

/*
  int     i_err = -1;
*/
  float   etime;

#ifndef CRAY
  struct rusage ru;
#else
  float   e,
  clock_t ct;
#endif

#ifdef CRAY
#ifdef VMS
  div=CLK_TCK;
#else
  div=CLOCKS_PER_SEC;
#endif
#endif

#ifndef CRAY
/*
  i_err = getrusage(RUSAGE_SELF, &ru);
*/
  getrusage(RUSAGE_SELF, &ru);
  etime = (float)ru.ru_utime.tv_sec + 0.000001*((float)ru.ru_utime.tv_usec);
#else
  ct = clock();
  e =  ((float) ct)/((float) div);
  etime = e;
#endif

  return (REAL)etime;

}


INTEGER cgetsys_usage (INTEGER *maxrss, INTEGER *nswap, INTEGER *minflt,
                       INTEGER *majflt, INTEGER *inblock, INTEGER *oublock,
                       REAL *utime, REAL *stime)
{
  int i_err=-1;

#ifndef CRAY
 struct rusage ru;
#endif
 
  *maxrss = 0;
  *nswap  = 0;
  *minflt = 0;
  *majflt = 0;
  *inblock= 0;
  *oublock= 0;
  *utime  = 0.;
  *stime  = 0.;

#ifndef CRAY
  i_err = getrusage(RUSAGE_SELF, &ru);
  *maxrss  = ru.ru_maxrss;
  *nswap   = ru.ru_nswap;
  *minflt  = ru.ru_minflt;
  *majflt  = ru.ru_majflt;
  *inblock = ru.ru_inblock;
  *oublock = ru.ru_oublock;
  *utime = (float)ru.ru_utime.tv_sec + 0.000001*((float)ru.ru_utime.tv_usec);
  *stime = (float)ru.ru_stime.tv_sec + 0.000001*((float)ru.ru_stime.tv_usec);
#endif

 return (INTEGER)i_err;
}


INTEGER cgetsys_pid()
{
  pid_t pid;

  pid = getpid();
  return (INTEGER)pid;
}


INTEGER cgetsys_time (DOUBLE *utime, DOUBLE *stime)
{
  INTEGER i_err=-1;

#ifndef CRAY
 struct rusage ru;
#endif

  *utime  = (DOUBLE)0.0;
  *stime  = (DOUBLE)0.0;

#ifndef CRAY
  i_err = getrusage(RUSAGE_SELF, &ru);
  *utime = (DOUBLE)ru.ru_utime.tv_sec 
           + 0.000001*((DOUBLE)ru.ru_utime.tv_usec);
  *stime = (DOUBLE)ru.ru_stime.tv_sec 
           + 0.000001*((DOUBLE)ru.ru_stime.tv_usec);
#endif

 return (INTEGER)i_err;
}


DOUBLE cgetsys_utime (void)
{
/*
  INTEGER i_err=-1;
*/
  DOUBLE utime = (DOUBLE)0.0;

#ifndef CRAY
 struct rusage ru;
#endif

#ifndef CRAY
/*
  i_err = getrusage(RUSAGE_SELF, &ru);
*/
  getrusage(RUSAGE_SELF, &ru);
  utime = (DOUBLE)ru.ru_utime.tv_sec 
           + 0.000001*((DOUBLE)ru.ru_utime.tv_usec);
#endif

 return (DOUBLE)utime;
}


DOUBLE cgetsys_stime (void)
{
/*
  INTEGER i_err=-1;
*/
  DOUBLE stime = (DOUBLE)0.0;

#ifndef CRAY
 struct rusage ru;
#endif

#ifndef CRAY
/*
  i_err = getrusage(RUSAGE_SELF, &ru);
*/
  getrusage(RUSAGE_SELF, &ru);
  stime = (DOUBLE)ru.ru_stime.tv_sec 
           + 0.000001*((DOUBLE)ru.ru_stime.tv_usec);
#endif

 return (DOUBLE)stime;
}


INTEGER cgetsys_library ()
{
  return (INTEGER)LNKLIB;
}


INTEGER cgetsys_cpu_speed(void) 
{
#ifdef linux
  /* The Linux routine was written by Roger Heflin */

  FILE *tf;
  static int init=0;
  static int cpu_speed=0;

  if (init == 1)
    return(cpu_speed);

  init = 1;
  /* Determine the current cpu speed */
  if ((tf = fopen(PROC_CPUINFO,"r")) == NULL) 
   {
   return(0);
   }

  /* Get the speed of the current cpu  */
  while (feof(tf) == 0)
    {
    double cpu_mhz;
    char linebuff[BUF_LENGTH];
  
    fgets(linebuff,BUF_LENGTH,tf);
    if (sscanf(linebuff,"cpu MHz : %lf",&cpu_mhz) == 1) 
      {
      cpu_mhz += 5;
      cpu_mhz /= 10;
      cpu_speed = ((int)(cpu_mhz))*10;
      break;
      }
    }
  fclose(tf);
  return(cpu_speed);
#else
  return(0);
#endif
}


char *cgetsys_cd_version()
{
#ifdef linux
  /* The Linux routine was written by Roger Heflin */

  DIR *dirinfo;
  struct dirent *direntry;
  static char cdversion[32];
  static int init=0;
  
  if (init == 1)
    return(cdversion);

  strcpy(cdversion,"unknown0");

  init = 1;
  dirinfo = opendir("/");
  if (dirinfo == NULL)
    {
    sprintf(cdversion,"Dir Open Failed %s",strerror(errno));
    return("unknown1");
    }
  else
    {
    while ((direntry = readdir(dirinfo)) != NULL)
      {
      if ((direntry->d_name[1] == '.') &&
          (direntry->d_name[3] == '.') &&
          (direntry->d_name[5] == '.') &&
          (strlen(direntry->d_name) > 5))
        { /* probably what we want */
        strcpy(cdversion,direntry->d_name);
        closedir(dirinfo);
        return(cdversion);
        }
      }
    closedir(dirinfo);
    strcpy(cdversion,"Dir Searched - Not found");   
    return("unknown2");
    }
  /* Should never get here */
  return("unknown3");
#else
  return("unkown4");
#endif
}

INTEGER cgetsys_os_version(char *version)
{
#ifdef linux
  char *cdversion;

  cdversion = cgetsys_cd_version();
  strcpy(version,cdversion);
#else
  struct utsname buf;

  if (uname(&buf) >= 0)
    strcpy(version,buf.release);
  else
    strcpy(version,"unknown");
#endif
  return(0);
}

void cgetsys_memory(INTEGER *ncpu, INTEGER *pagesz, INTEGER *physpgs, 
                    INTEGER *avphyspgs)
{
#ifdef __sgi
  *ncpu      = sysconf(_SC_NPROC_CONF);
  *pagesz    = sysconf(_SC_PAGESIZE);
  *physpgs   = 0;
  *avphyspgs = 0;
#else
  *ncpu      = sysconf(_SC_NPROCESSORS_CONF);
  *pagesz    = sysconf(_SC_PAGESIZE);
  *physpgs   = sysconf(_SC_PHYS_PAGES);
  *avphyspgs = sysconf(_SC_AVPHYS_PAGES);
#endif
}


void cgetsys_my_pid_memory (INTEGER *vm, INTEGER *rm)
{

#ifdef linux

  FILE     *statfile;
  int      pid;
  char     com[1024];
  char     state;
  int      ppid;
  int      pgrp;
  int      session;
  int      tty;
  int      tpgid;
  unsigned flags;
  unsigned minflt;
  unsigned cminflt;
  unsigned majflt;
  unsigned cmajflt;
  int      utime;
  int      stime;
  int      cutime;
  int      cstime;
  int      counter;
  int      priority;
  int      timeout;
  unsigned itrealvalue;
  int      starttime;
  unsigned vsize;
  unsigned rss;

  statfile = fopen("/proc/self/stat","r");
  if (statfile) {
    if (fscanf(statfile,
"%d (%[^)]) %c %d %d %d %d %d %u %u %u %u %u %d %d %d %d %d %d %d %u %d %u %u",
               &pid,com,&state,&ppid,&pgrp,&session,&tty,&tpgid,&flags,&minflt,
               &cminflt,&majflt,&cmajflt,&utime,&stime,&cutime,&cstime,&counter,
               &priority,&timeout,&itrealvalue,&starttime,&vsize,&rss)!= 24) {
      *vm = 0;
      *rm = 0;
    }
    else {
      *vm = vsize / 1024;
      *rm = rss * 4;
    }
    fclose(statfile);
  }
  else {
    *vm = 0;
    *rm = 0;
  }

#else

  *vm = 0;
  *rm = 0;

#endif
}

INTEGER cgetsys_strnlen(char *str, INTEGER maxlen) {
  int i;
  for (i=0;i<maxlen;i++){
    if(str[i]=='\0') return (size_t) i;
  }
  return maxlen;
}
