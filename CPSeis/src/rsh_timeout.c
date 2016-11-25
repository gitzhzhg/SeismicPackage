/*<CPS_v1 type="PROGRAM"/>
!------------------------------- rsh_timeout.c --------------------------------
!------------------------------- rsh_timeout.c --------------------------------
!------------------------------- rsh_timeout.c --------------------------------

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
! Name       : rsh_timeout
! Category   : stand-alone
! Written    : 2002-03-21   by: Charles C. Burch/Roger Heflin
! Revised    : 2003-06-16   by: Charles C. Burch
! Maturity   : production
! Purpose    : A rsh command with timeout
!</brief_doc>

!<descript_doc>
!-------------------------------------------------------------------------------
! This does a rsh command with a specified timeout
! This is used with pfio to handle rsh to cpus that are hung or not running
!-------------------------------------------------------------------------------
!</descript_doc>

!<advice_doc>
!-------------------------------------------------------------------------------
!  program is invoked by "rsh_timeout node timeout_seconds command"
!-------------------------------------------------------------------------------
!</advice_doc>

!<history_doc>
!-------------------------------------------------------------------------------
!                     AUXILIARY FILE REVISION HISTORY
!     Date        Author           Description
!     ----        ------           -----------
!  3. 2003-06-16  C.C. Burch       Changes for rsh location for SGI
!  2. 2002-04-19  C.C.Burch        Fix some Sun compiler problems
!  1. 2002-03-21  C.C.Burch/Heflin Initial Version.
!-------------------------------------------------------------------------------
!</history_doc>

!<portability_doc>
!-------------------------------------------------------------------------------
!                         PORTABILITY LIMITATIONS
!
! The only known limitation is the location of the rsh command.
! rsh_command has the full expanded location of the the rsh command.
! For Linux and solaris this is /usr/bin/rsh.
! For SGI this is /usr/bsd/rsh.
! usr/bin/rsh is first checked and if not right, /usr/bsd/rsh is checked.
! Additional locations might need to be added for different systems.
!-------------------------------------------------------------------------------
!</portability_doc>

!<compile_doc>
!-------------------------------------------------------------------------------
!                     SPECIAL COMPILING REQUIREMENTS
!
! No special requirements
!
!-------------------------------------------------------------------------------
!</compile_doc>

*/

/*
!--------------------------- start of coding ------------------------------
!--------------------------- start of coding ------------------------------
!--------------------------- start of coding ------------------------------
*/

/*******************************************************************
 Note the basic structure of this program is based on code 
   written by Roger Heflin
*******************************************************************/

#include <errno.h>
#include <signal.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>
#include <sys/stat.h>
#include <sys/types.h>
#include <sys/wait.h>

void rsh_timeout_alarm(int);

char *rsh_timeout_ident =
"$Id: rsh_timeout.c,v 1.3 2003/06/16 18:16:29 Burch prod sps $";

/*------------------------ start of functions ----------------------------*/
/*------------------------ start of functions ----------------------------*/
/*------------------------ start of functions ----------------------------*/

static char  node_name[256];
static int   timeout;
static pid_t pid;

static char rsh_command[40];

/*********************** RSH_TIMEOUT_SIG **********************************/
void rsh_timeout_alarm(int sig) {

  kill(pid,9); 

/* Without this, the rsh will be left hanging around
   On the sun with the node out it will stay around for at least 3-4 minutes
   On the linux machines it will stay for at least 40 seconds (with the current
   settings, and only if the node is truly down 
*/ 

  fprintf(stderr,
   "TIMEOUT: rsh to %s timed out-timeout was %d - node may be hung\n",
   node_name,timeout);
  exit(-2);
}

/******************************* MAIN PROGRAM ****************************/
int main(int argc,char **argv,char *envp[]) {
  int i,status;
  char **argvl;
  struct stat stbuf;

  if (argc < 4) {
    fprintf(stderr,
     "%s: At least 3 arguments are required [hostname timeout command]\n",
     argv[0]);
    exit(-1);
  }

  strcpy(node_name,argv[1]);

  timeout=0;
  timeout = atoi(argv[2]);
  if (timeout <= 0) {
    fprintf(stderr,
     "Error: rsh_timeout, timeout has been set to 0 - exiting\n");
    exit(-1);
  }

  if (timeout > 86400) {
    fprintf(stderr,
    "Warning: rsh_timeout Timeout(%d) greater than 86400 seconds-continuing\n",
     timeout);
  }

  strcpy(rsh_command,"/usr/bin/rsh");    /** find location of rsh **/
  if(stat(rsh_command, &stbuf)<0) { 
    strcpy(rsh_command,"/usr/bsd/rsh");
    if(stat(rsh_command, &stbuf)<0) { 
       printf("Unable to find rsh-job aborted\n");
       exit(-1);
    }
  }

  if((argvl=(char**) malloc((argc+3)*sizeof(char**)))==NULL) {
    fprintf(stderr, "Unable to malloc argvl in rsh_timeout\n");
    exit(-1);
  }

  if (signal(SIGALRM,rsh_timeout_alarm) == SIG_ERR) {
    fprintf(stderr,
     "Error: rsh_timeout had problem installing signal handler-error is %s\n",
     strerror(errno));
  }

  alarm(timeout);

/* using the system call will leave rsh command running - 
  potentially causing harm or just hanging around forever */
           
  pid = fork();
  if (pid < 0) {           /*error*/
    fprintf(stderr,
     "Error: rsh_timeout had fork problem-error is %s\n",
     strerror(errno));
    status=-1;

  } else if (pid == 0) {   /*child*/
    argvl[0] = rsh_command;
    argvl[1] = node_name;
    argvl[2] = "-n";

    for (i=3;i<argc;i++) {
      argvl[i] = argv[i];
    }
 
    argvl[argc] = 0; 
 
    if (execve(rsh_command, argvl, envp) != 0) {
      fprintf(stderr,"Error: rsh_timeout, execve error [%s]\n",
       strerror(errno));
      status=-1;
    } else {
      status=0;
    }

  } else {                 /*parent*/

    if (waitpid(pid, &status, 0) == -1) {
      fprintf(stderr,
       "Error: rsh_timeout had a waitpid problem, error is %s\n",
       strerror(errno));
    }
    alarm(0);
    signal(SIGALRM,SIG_DFL);
  }

  free(argvl);
  exit(status);
}


/*-------------------------------- end ------------------------------------*/
/*-------------------------------- end ------------------------------------*/
/*-------------------------------- end ------------------------------------*/
