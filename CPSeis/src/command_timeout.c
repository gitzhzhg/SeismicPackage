/*<CPS_v1 type="PROGRAM"/>
!------------------------------ command_timeout.c ------------------------------
!------------------------------ command_timeout.c ------------------------------
!------------------------------ command_timeout.c ------------------------------

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
! Name       : command_timeout
! Category   : stand-alone
! Written    : 2002-04-19   by: Charles C. Burch
! Revised    : 2002-04-19   by: Charles C. Burch
! Maturity   : production
! Purpose    : A command executor with timeout
!</brief_doc>

!<descript_doc>
!-------------------------------------------------------------------------------
! This does a  command with a specified timeout
!-------------------------------------------------------------------------------
!</descript_doc>

!<advice_doc>
!-------------------------------------------------------------------------------
!  program is invoked by "command_timeout timeout_seconds command parameters"
!-------------------------------------------------------------------------------
!</advice_doc>

!<history_doc>
!-------------------------------------------------------------------------------
!                     AUXILIARY FILE REVISION HISTORY
!     Date        Author      Description
!     ----        ------      -----------
!  1. 2002-04-19  C. C. Burch Initial Version.
!-------------------------------------------------------------------------------
!</history_doc>

!<portability_doc>
!-------------------------------------------------------------------------------
!                         PORTABILITY LIMITATIONS
! No known limitations.
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

#include <stdio.h>
#include <signal.h>
#include <errno.h>
#include <stdlib.h>
#include <unistd.h>
#include <sys/types.h>
#include <sys/wait.h>
#include <string.h>

char *command_timeout_ident =
"$Id: command_timeout.c,v 1.1 2002/04/19 11:39:34 CCBurch prod sps $";

void command_timeout_alarm(int);

/*------------------------ start of functions ----------------------------*/
/*------------------------ start of functions ----------------------------*/
/*------------------------ start of functions ----------------------------*/

static int   timeout;
static pid_t pid;


/*********************** COMMAND_TIMEOUT_SIG ******************************/
void command_timeout_alarm(int sig) {

  kill(pid,9); 

/* Without this, the command will be left hanging around
   On the sun with the node out it will stay around for at least 3-4 minutes
   On the linux machines it will stay for at least 40 seconds (with the current
   settings, and only if the node is truly down 
*/ 

  fprintf(stderr,
   "TIMEOUT: command_timeout timed out-timeout was %d\n",
   timeout);
  exit(-2);
}

/******************************* MAIN PROGRAM ****************************/
int main(int argc, char **argv, char *envp[]) {
  int i, status;
  char **argvl;

  if (argc < 3) {
    fprintf(stderr,
     "%s: At least 2 arguments are required [timeout command parameters]\n",
     argv[0]);
    exit(-1);
  }

  timeout=0;
  timeout = atoi(argv[1]);
  if (timeout <= 0) {
    fprintf(stderr,
     "Error: commandtimeout, timeout has been set to 0 - exiting\n");
    exit(-1);
  }

  if (timeout > 86400) {
    fprintf(stderr,
    "Warning: %s Timeout(%d) greater than 86400 seconds-continuing\n",
     "command_timeout", timeout);
  }

  if((argvl=(char**) malloc(argc*sizeof(char**)))==NULL) {
    fprintf(stderr, "Unable to malloc argvl in command_timeout\n");
    exit(-1);
  }

  if (signal(SIGALRM, command_timeout_alarm) == SIG_ERR) {
    fprintf(stderr,
     "Error: %s had problem installing signal handler-error is %s\n",
     "command_timeout", strerror(errno));
  }

  alarm(timeout);

/* using the system call will leave command running - 
  potentially causing harm or just hanging around forever */
           
  pid = fork();
  if (pid < 0) {           /*error*/
    fprintf(stderr,
     "Error: command_timeout had fork problem-error is %s\n",
     strerror(errno));
    status=-1;

  } else if (pid == 0) {   /*child*/
    argvl[0]=argv[0];    

    for (i=3;i<argc;i++) {
      argvl[i-2] = argv[i];
    }
    argvl[argc-2] = 0; 

 
    if (execvp(argv[2], argvl) != 0) {
      fprintf(stderr,"Error: command_timeout, execve error [%s]\n",
       strerror(errno));
      status=-1;
    } else {
      status=0;
    }

  } else {                 /*parent*/

    if (waitpid(pid, &status, 0) == -1) {
      fprintf(stderr,
       "Error: command_timeout had a waitpid problem, error is %s\n",
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
