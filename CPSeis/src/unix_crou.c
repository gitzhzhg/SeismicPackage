/*!<CPS_v1 type="AUXILIARY_FILE"/>
!---------------------------- unix_crou.c -------------------------------
!---------------------------- unix_crou.c -------------------------------
!---------------------------- unix_crou.c -------------------------------

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
! Name       : unix_crou
! Category   : miscellaneous
! Written    : 2000-04-04   by: Bill Menger
! Revised    : 2005-01-31   by: Bill Menger
! Maturity   : production
! Purpose    : Provide simple interface to unix functionality.
! References : These routines are mainly for unix.f90 but can be used anywhere
!-------------------------------------------------------------------------------
!</brief_doc>

!<descript_doc>
!------------------------------------------------------------------------------
! Internal subroutines mainly for unix.f90 usage
!------------------------------------------------------------------------------
!</descript_doc>

!<calling_doc>
! void unix_initialize_c();
!  internally called to set utsname structure up.
!------------------------------------------------------------------------------
! void unix_abort_c(char *mess) 
!  if strlen(n_mess)>0 print an abort message in mess
!  after printing message, call c function abort
!------------------------------------------------------------------------------
! void unix_errmsg_c(char *mess) 
!  prints an error message
!------------------------------------------------------------------------------
! int unix_fork_c(void)
!   forks another process, returning +int PID of the child if
!       I am the parent, returning 0 if I am the child, and returning
!      -int if I have failed.
!------------------------------------------------------------------------------
!  void unix_mem_usage_c(char *message, INTEGER *n_message)
!   prints out current memory usage preceeded by message
!------------------------------------------------------------------------------
!  int unix_system_c(char *command,char *shell) 
!    runs a command as if from the shell. Return != 0 if error
!------------------------------------------------------------------------------
!  int unix_system_timeout_c(char *command,INTEGER *timeout, char *shell) 
!    runs a command as if from the shell. Return != 0 if error
!------------------------------------------------------------------------------
!  void unix_sleep_c( int *seconds ) 
!    calls the sleep function 
!------------------------------------------------------------------------------
!  void unix_umask_c( int *imask ) 
!    calls the umask function 
!------------------------------------------------------------------------------
!  double unix_utime_c(double *start_time) 
!    access the process user+system time using the most accurate timer available
!      to the system.
!------------------------------------------------------------------------------
!  double unix_wtime_c(double *start_time) 
!    access the system wall clock time using the most accurate timer available 
!      to the system.
!------------------------------------------------------------------------------
!  DOUBLE unix_get_wtimestamp_c() 
!    Get a wall clock time stamp time--it automatically initializes itself
!------------------------------------------------------------------------------
!  void   unix_get_cmdline(int *argc,char *argv,int *lenargv)
!    Return argc and argv to user just like a "c" main can do.
!    Pass in the length of argv string (lenargv) so I will not accidentally
!    overstep the bounds of the string length
!------------------------------------------------------------------------------
!  void unix_wtimestamp_c(char *title) 
!    Provide a wall clock time stamp
!------------------------------------------------------------------------------
!                                       I          I
!  int *unix_is_process_active_f(char *host, int *pid)
!  int  unix_is_process_active_c(char *host, int  pid)
!    returns 1 if process pid on node host active, 0 if not active, -1 on error 
!------------------------------------------------------------------------------
!  int unix_get_hostname_c(char *hostname, int n_hostname)
!    gets current hostname  
!------------------------------------------------------------------------------
!  char* unix_user_name_c()
!    gets username and returns pointer to it
!------------------------------------------------------------------------------
!  unix_get_pid_f(INTEGER* pid)
!    get pids and places imto pid
!------------------------------------------------------------------------------
!  unix_get_user_dir_f(char *name, INTEGER* n_name)
!    get user directory and places into name 
!------------------------------------------------------------------------------
!  unix_get_user_name_f(char *name, INTEGER* n_name)
!    get user name and places into name 
!------------------------------------------------------------------------------
!  unix_get_host_name_f(char *name, INTEGER* n_name)
!    get host name and places into name 
!------------------------------------------------------------------------------
!</calling_doc>

!
!<history_doc>
!-------------------------------------------------------------------------------
!                     AUXILIARY FILE REVISION HISTORY 
!
!     Date        Author       Description
!     ----        ------       -----------
! 13. 2005-01-31  Bill Menger  Modified calls to host, name, dir get routines,
!                              enabled the timeout function. (Ried-added 
!                              unix_errmsg_c to flush messages.)
! 12. 2004-08-12  Bill Menger  Added utsname struct to hold host info.
! 11. 2004-01-21  Bill Menger  Added utime and get_cmdline
! 10. 2003-09-04  SMCook       Added unix_umask
!  9. 2003-06-25  C C Burch    Moved various pcpsx_crou routines here
!  8. 2003-06-16  C C Burch    Minor changes for SGI warnings.
!  7. 2003-05-30  C C Burch    remove error file in unix_is_process_active.c
!  6. 2003-05-29  C C Burch    Made buffers larger in unix_is_process_active_c
!  5. 2003-05-28  C C Burch    Added is_process_active_c & get_hostname_c
!  4. 2002-09-23  C C Burch    Added unix_wtime_stamp_c
!  3. 2002-04-24  C C Burch    Added unix_abort_c, unix_waitpid, use of unix.h
!                              Added various .h includes so -Wall works
!  2. 2000-05-25  Bill Menger  Added c2f_interface.h and named_constants.h
!  1. 2000-04-04  Bill Menger  Initial version.
!-------------------------------------------------------------------------------
!</history_doc>

!<portability_doc>
!------------------------------------------------------------------------------
! No known portability problems, except unix_mem_usage_c is LINUX specific
! the structure "rusage" from the man page of "getrusage" follows:
!  struct rusage {
!    struct timeval ru_utime;  user time used 
!    struct timeval ru_stime;  system time used 
!    ...
!    long   ru_minflt;         page reclaims 
!    long   ru_majflt;         page faults 
!    long   ru_nswap;          swaps 
!    ...
!  };
!  This only shows the elements that LINUX currently handles.
!  Also:  get utime is Linux specific, as is get_cmdline
!
!------------------------------------------------------------------------------
!</portability_doc>
*/

/*
!--------------------------- start of coding ------------------------------
!--------------------------- start of coding ------------------------------
!--------------------------- start of coding ------------------------------
*/


char *UNIX_CROU_IDENT = 
"$Id: unix_crou.c 1.13 2005-01-31 10.32.51 Menger prod sps $";

#define _GNU_SOURCE
#include <stdlib.h>
#include <stdio.h>
#include <sys/types.h>
#include <unistd.h>
#include <time.h>
#include <pthread.h>
#include <sys/time.h>
#include <sys/stat.h>
#include <sys/wait.h>
#include <string.h>
#include <pwd.h>
#include <errno.h>
#include <sys/utsname.h>

#include "c2f_interface.h"
#include "unix.h"
#include "str.h"

extern char **environ;
static DOUBLE old_time=0.0e0;
static char   unix_host[]="\0                         ";

static struct utsname mach_info;
static struct passwd  ui;
static struct passwd  *user_info=&ui;
static char user_name[36];
static int initialized = 0;
static pid_t pid;

/*------------------------ start of functions ----------------------------*/
/*------------------------ start of functions ----------------------------*/
/*------------------------ start of functions ----------------------------*/

#ifdef __cplusplus
extern "C" {
#endif

/******************************************************************
* initialize some structs and don't need to do it again, so set the
* initialized static int variable above to "1".
* This saves a lot of time later on when "unix_xxx" is called repeatedly.
*******************************************************************/
void unix_initialize_c() {
  if(initialized > 0 ) return;
  initialized++;
  uname(&mach_info);
  user_info=getpwuid(geteuid());
}

/*******************************************************************
* print a message and abort
* 
* Written April 2002 by Charles C Burch
*******************************************************************/
void unix_abort_c(char *mess) {

  if(mess[0]!='\0') {
    sleep(1);

    fflush(stdout);
    fprintf(stderr,"Job aborting for the following reason:\n");
    fprintf(stderr,"  %s\n",mess);
  }
  abort();
}

/*******************************************************************
* print an error message to standard error
* 
*******************************************************************/
void unix_errmsg_c(char *mess) {

  /*sleep(1);*/
  fflush(stdout);
  fflush(stderr);

  fprintf(stderr,"The following error has occurred:\n");
  fprintf(stderr,"  %s\n",mess);
  fflush(stderr);
}

INTEGER unix_fork_c(void) {
  /* Purpose:  To fork another process, returning +int PID of the child if
               I am the parent, returning 0 if I am the child, and returning
               -int if I have failed.
  */
  return (INTEGER ) fork();
}
#ifdef LINUX
void unix_get_mem_usage(long *VmSize, long *VmRSS, long *VmData, long *VmStk){
  char proc_name[24], line[120];
  FILE *fptr;
  sprintf(proc_name,"/proc/%d/status",(int) getpid());
  if((fptr=fopen(proc_name,"r"))==NULL) {
    printf("unable to open(%s}\n",proc_name);
    exit(1);
  }
  (*VmSize) = -1;
  (*VmRSS ) = -1;
  (*VmData) = -1;
  (*VmStk ) = -1;
  while(fgets(line, sizeof(line), fptr)!=NULL) {
    if(strstr(line,"VmSize:")!=NULL) sscanf(line+7,"%ld",VmSize);
    if(strstr(line,"VmRSS:" )!=NULL) sscanf(line+6,"%ld",VmRSS );
    if(strstr(line,"VmData:")!=NULL) sscanf(line+7,"%ld",VmData);
    if(strstr(line,"VmStk:" )!=NULL) sscanf(line+6,"%ld",VmStk );
  }
  fclose(fptr);
  return;
}

void unix_mem_usage_c(char *text, INTEGER *N_TEXT) {
  long VmSize, VmRSS, VmData, VmStk;
  int n_text;
  char line[21];

  unix_get_mem_usage(&VmSize, &VmRSS, &VmData, &VmStk);

  if((n_text=(*N_TEXT))==0) n_text=strlen(text);
  if(n_text>20) n_text=20;
  memset(line,'\0',n_text+1);
  strncpy(line,text, n_text);

  printf("%-20s VmSize=%ldK, VmRSS=%ldK, VmData=%ldK, VmStk=%ldK\n",
     line,VmSize, VmRSS, VmData, VmStk);
  return;
}

void unix_get_cmdline_c(INTEGER *argc, char * argv, INTEGER *lenargv){
  FILE   *proc=NULL;
  char   *proc_name=NULL;
  char   **line=NULL;
  int    delim='\0';
  size_t lenarg;
  int    len,lenargv_so_far=0;

  /* embed the process id into the proc file name */
  proc_name=(char *) malloc(50);
  sprintf(proc_name,"/proc/%d/cmdline",(int) getpid());

  /* initialize the output */
  strcpy(argv,"\000");
  (*argc)=0;

  /* open the proc file system looking for the cmdline file */
  if((proc=fopen(proc_name,"r"))==NULL) {
    fprintf(stderr,"unable to open(%s)\n",proc_name);
    free(proc_name);
  return;
  }

  /*  Allocate memory to hold each command line argument */
  line = (char **) malloc (250);
  lenarg = 250;

  /*  Get each argument (one at a time) from the file */
  while( (len=getdelim(line,&lenarg,delim,proc)) > 0){
    /* test to see if we have space to insert the arg, if not, exit now */
    if(lenargv_so_far + len > *lenargv) goto exit;
    /* add the argument to our output string */
    strcat(argv,*line);
    /* increment our argument counter */
    (*argc)++;
    /* Adding vertical tab as delimiter into output string(replacing \000) */
    strcat(argv,"\013");
    /* add cumulative length of output string, so I can compare with size */
    lenargv_so_far += len;
  }

  /* close the proc filesystem, free up temporary memory */
exit:
  fclose(proc);
  free(line);
  free(proc_name);
  return;
}  
    
  
/*******************************************************************
* Get user and system times
* Written September 2003 Bill Menger
*******************************************************************/ 
DOUBLE unix_utime_c(DOUBLE * start_time) {
  static struct rusage usage;
  DOUBLE real_time;
  int status;
  
  status=getrusage(RUSAGE_SELF, &usage);
  if(status != 0 ) return (double) 0.0;
  real_time = 1.*usage.ru_utime.tv_sec + usage.ru_utime.tv_usec*.000001;
  real_time +=1.*usage.ru_stime.tv_sec + usage.ru_stime.tv_usec*.000001;
  real_time -= (*start_time);
  return (real_time);

}
#else
/* If no /proc file system with support for these, send back null data */
void unix_mem_usage_c(char *text, INTEGER *N_TEXT) {
}
void unix_get_cmdline_c(INTEGER *argc, char * argv, INTEGER *lenargv){
  (*argc)=0;
}
DOUBLE unix_utime_c(DOUBLE * start_time) {
  return (*start_time);
}
#endif

INTEGER unix_system_timeout_c(char * command, INTEGER * timeout, char * shell) {
  int timeout_internal = *timeout;
  int status;
  /* TIMEOUT NOT IMPLEMENTED YET */

  if( timeout_internal <= 0 ) { 
    return unix_system_c(command,shell);
  };

  if (signal(SIGALRM,unix_timeout_alarm) == SIG_ERR) {
    fprintf(stderr,
     "Error: unix_system_timeout had problem installing signal handler:");
    fprintf(stderr,"-error is %s\n", strerror(errno));
  }

  alarm(timeout_internal);
  pid = fork();
  if (pid == 0) {
    char *argv[4];   /*child*/
    argv[0] = shell;
    argv[1] = "-c";
    argv[2] = command;
    argv[3] = 0;
    execve(shell, argv,environ);
    exit(127);
  } else if (pid == -1) {
    status=-1;   /*error*/
  } else {       /*parent*/
    if (waitpid(pid, &status, 0) == -1) status=-1;
  }
  alarm(0);
  signal(SIGALRM,SIG_DFL);
  return((INTEGER) status);
}

INTEGER unix_system_c(char * command,char * shell) {
  /* Purpose:  To run a command as if from the shell. Return != 0 if error */
  int status;

  if (command == 0) return 1;

  pid = fork();

  if (pid == 0) {
    char *argv[4];   /*child*/
    argv[0] = shell;
    argv[1] = "-c";
    argv[2] = command;
    argv[3] = 0;
    execve(shell, argv,environ);
    exit(127);
  } else if (pid == -1) {
    status=-1;   /*error*/
  } else {       /*parent*/
    if (waitpid(pid, &status, 0) == -1) status=-1;
  }

  return((INTEGER) status);
}

void unix_sleep_c(INTEGER *seconds ) {
  /* Purpose:  To directly call the sleep function */
  sleep(*seconds);
}

void unix_umask_c(int *imask) {
  /* Purpose:  To directly call the umask function */
  switch(*imask) {
    case 22: umask((mode_t)022); break;    /* explicit calls needed! */ 
    case 27: umask((mode_t)027); break;
    default:
      umask((mode_t)*imask);
      printf("unix_crou.c: umask case %i not specifically handled\n", *imask);
  }
}

/*******************************************************************
* wait for a given pid to finish and return status 
* 
* Written April 2002 by Charles C Burch
*******************************************************************/
INTEGER unix_waitpid_c(INTEGER *passed_pid) {
  int status;

  if (waitpid(*passed_pid, &status, 0) == -1) status=-1; 
  return((INTEGER) status);
}

DOUBLE unix_wtime_c(DOUBLE * start_time) {
  /* Purpose:  To directly access the system wall clock time using the most
               accurate timer available to the system.
  */
  static struct timeval tod;
  DOUBLE real_time;

  gettimeofday(&tod,(struct timezone *) NULL);
  real_time = 1.*tod.tv_sec + tod.tv_usec*.000001 - (*start_time);
  return (real_time);
}

/*******************************************************************
* Get wall clock time stamp time--automatically initializes itself
* 
* Written September 2002 by Charles C Burch
*******************************************************************/
DOUBLE unix_get_wtime_stamp_c() {
  register DOUBLE temp;
  
  if(old_time==0.0e0) old_time=unix_wtime_c(&old_time);
  if((temp=unix_wtime_c(&old_time))<0) temp=0.; /*handle round-off*/
  return(temp);
}

/*******************************************************************
* Print wall clock time stamp
* 
* Written September 2002 by Charles C Burch
*******************************************************************/
void unix_wtime_stamp_c(char *title) {
  printf("unix_wtime_stamp: %s, %f\n", 
      title, (float)unix_get_wtime_stamp_c());
  fflush(stdout);
  return;
}

/**********************************************************
* check to see if given process active or not
* host is the node to check on, pid is the process id.
* returns  1 if active, 
*          0 if not active, 
*         -1 if error,
*         -2 if command timeouts.
*
* Written Decemeber 2002 by Charles C Burch
**********************************************************/
INTEGER unix_is_process_active_f(char *host, INTEGER * passed_pid) {
  return (int) unix_is_process_active_c(host, *passed_pid);
}

int unix_is_process_active_c(char *host, int passed_pid) {
  FILE *pf;
  char line[BUFSIZ], cmd[260], rsh_err[80];
  int istat, pid_check;
  struct stat info;

  unix_initialize_c(); 
  if(unix_host[0]=='\0') unix_get_hostname_c(unix_host, sizeof(unix_host));
  sprintf(rsh_err,"rsh_pidact_%s_%d_%d.err",
   unix_host,(int)getpid(),(int)pthread_self());

  if(str_cmp_nocase_skip_blanks(host,unix_host)==0) {
    
#ifdef CYGWIN    
    sprintf(cmd,"ps -e");
#else
    sprintf(cmd,"ps -p %d", passed_pid);
#endif
  } else {
    
#ifdef CYGWIN    
    sprintf(cmd,"rsh_timeout %s 300 ps -e 2>%s",host, rsh_err);
#else
    sprintf(cmd,"rsh_timeout %s 900 ps -p %d 2>%s",host, passed_pid, rsh_err);
#endif
    
  }  
  /**  printf("cmd=%s\n",cmd);  **/

  if((pf=popen(cmd,"r"))==NULL) {
    remove(rsh_err);  
    return(-1);
  }

  istat=0;
  /* scan the output of the "ps -p" command on "host" and see if
     the pid shows up.  If it does, set istat=1 and quit.
     Only scan the first item in the output line(s) (before a whitespace brk.)
  */
  while(fgets(line,sizeof(line),pf)!=NULL) {
    if( (sscanf(line,"%d",&pid_check)) == 1) {
      if(passed_pid==pid_check) {
        /*printf("pid=%d pid_check=%d\n",passed_pid,pid_check);*/
        istat=1;
        break;
      }
    }
  }  
  pclose(pf);

  if(istat==0) {
    if(stat(rsh_err, &info)==0) {
      if(info.st_size>0) istat=-2;
    }
  }

  remove(rsh_err);  
  return(istat);
}

/************* UNIX_GET_HOSTNAME ***************
* get hostname- return -1 if error;
*
* Written October 2000 by Charles C Burch
***********************************************/
int unix_get_hostname_c(char *hostname, int n_hostname){
  int i=0, istat=0;
  char *s;

  unix_initialize_c();
  /** printf("unix_get_hostname_c: len=%d\n",n_hostname); **/

  s=strchr(mach_info.nodename,'.');
  if(s != NULL ) {
    /* i will contain the length of the substring of nodename
       without the domain information
    */
    i=  (mach_info.nodename > s) ? (mach_info.nodename - s ) :
                                (s - mach_info.nodename ) ;
    if(i==n_hostname) i--;
    strncpy(hostname,mach_info.nodename,(size_t) i);
  } else {
    i=n_hostname;
    strncpy(hostname,mach_info.nodename,(size_t) i);
    if(i==n_hostname) i--;
  }
  hostname[i]='\0';
  return istat;
}

void unix_get_host_name_f(char *name, INTEGER *n_name) {
  unix_get_hostname_c(name, (int) *n_name);
  return;
}

/**************** UNIX_USER_NAME_C **********************
* returns pointer to user name
*
* Written by Charles C Burch  January 2003
*************************************************************/
char* unix_user_name_c(){
  unix_initialize_c();
  strncpy(user_name,user_info->pw_name,sizeof(user_name)); 
  return (user_name);
}

/**************** UNIX_GET_USER_NAME_F **********************
* returns user name
*
* Written by Charles C Burch  January 2003
*************************************************************/
void unix_get_user_name_f(char *name, INTEGER*N) {
  strncpy(name, unix_user_name_c(), (size_t) *N);
  return;
}

/****************** GET_PID ********************
* get process id
*  pid(integer)=return process id
*
* Written October 2000 by Charles C Burch
***********************************************/
void unix_get_pid_f(INTEGER *passed_pid){
  (*passed_pid)=(INTEGER) getpid();
  return;
}

/*************** GET_USER_DIR ******************
* get user dir
*   dir=array of char variables
*   n-dir(integer)=size of dir
*
* Written October 2000 by Charles C Burch
***********************************************/
void unix_get_user_dir_c(char *dir, int n_dir){
  unix_initialize_c(); 
  strncpy(dir,user_info->pw_dir,(size_t) n_dir);
  return;
}

void unix_get_user_dir_f(char *dir, INTEGER *n_dir){
  unix_get_user_dir_c(dir, (int) *n_dir);
  return;
}

void unix_timeout_alarm(int sig) {
  /*  for unix_system_timeout_c.  This will cause child to abort on sigalarm  */
  kill (pid,9);
  fprintf(stderr,"command timed out--aborted\n");
}

#ifdef __cplusplus
}
#endif

/*-------------------------------- end ------------------------------------*/
/*-------------------------------- end ------------------------------------*/
/*-------------------------------- end ------------------------------------*/
