/*<CPS_v1 type="PRIMITIVE">
!----------------------------- dbug_crou.c -------------------------------
!----------------------------- dbug_crou.c -------------------------------
!----------------------------- dbug_crou.c -------------------------------

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
! Name       : DBUG_CROU
! Category   : main_prog
! Written    : 2002-07-18   by: Charles C. Burch
! Revised    : 2005-05-31   by: Tom Stoeckley
! Maturity   : production
! Purpose    : provides access to debug functions written in C
! References : These routines are for general use by c or Fortran routines
!-------------------------------------------------------------------------------
!</brief_doc>

!<descript_doc>
!-------------------------------------------------------------------------------
! These routines provide access certain debug-related functions
!
!-------------------------------------------------------------------------------
!</descript_doc>

!<calling_doc>
!-------------------------------------------------------------------------------
! void dbug_raise_signal(INTEGER *SIG) 
! Purpose raise a given signal: Used mainly for debugging dbug routines
!-------------------------------------------------------------------------------
! void dbug_set_message_c(char *msg) 
! Purpose: set the debug message to a c string
!-------------------------------------------------------------------------------
! void dbug_set_message_f(char *msg, INTEGER *N_MSG) 
! Purpose: set message using f string input
!-------------------------------------------------------------------------------
! void dbug_get_message_c(char *msg) 
! Purpose: get message using c string output
!-------------------------------------------------------------------------------
! void dbug_get_message_f(char *msg, INTEGER *N_MSG) 
! Purpose: set message using f string output
!-------------------------------------------------------------------------------
! void dbug_set_system_message_c(char *msg) 
! Purpose: set the debug message to a c string
!-------------------------------------------------------------------------------
! void dbug_set_system_message_f(char *msg, INTEGER *N_MSG) 
! Purpose: set message using f string input
!-------------------------------------------------------------------------------
! void dbug_get_system_message_c(char *msg) 
! Purpose: get message using c string output
!-------------------------------------------------------------------------------
! void dbug_get_system_message_f(char *msg, INTEGER *N_MSG) 
! Purpose: set message using f string output
!-------------------------------------------------------------------------------
! void dbug_install_signal_handler_c()
! Purpose: install default dbug signal handler
!-------------------------------------------------------------------------------
!</calling_doc>

!<history_doc>
!-------------------------------------------------------------------------------
!                     AUXILIARY FILE REVISION HISTORY
!     Date        Author         Description
!     ----        ------         -----------
!  3. 2005-05-31  Stoeckley      Fixed so will compile with C++.
!  2. 2002-08-12  C. C. Burch    Add system debug messages
!  1. 2002-07-18  Chuck C. Burch Initial Version.
!-------------------------------------------------------------------------------
!</history_doc>

!<portability_doc>
!-------------------------------------------------------------------------------
! No known portability problems
!-------------------------------------------------------------------------------
!</portability_doc>
*/

/*
!--------------------------- start of coding ------------------------------
!--------------------------- start of coding ------------------------------
!--------------------------- start of coding ------------------------------
*/

char *dbug_crou_ident =
"$Id: dbug_crou.c,v 1.3 2005/05/31 13:04:08 Stoeckley prod sps $";

#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>
#include <string.h>
#include <signal.h>

#include "c2f_interface.h"

#include "dbug.h"

/*--------------------------- start of functions ----------------------------*/
/*--------------------------- start of functions ----------------------------*/
/*--------------------------- start of functions ----------------------------*/


static char dbug_message[256]       ={"dbug message was never set"};
static char dbug_system_message[256]={"dbug system message was never set"};
static char dbug_work[80];

#ifdef __cplusplus
extern "C" {
#endif

/*************************************************************
* raise a given signal
*
* Written July 2002 by Charles C Burch
************************************************************/
void dbug_raise_signal(INTEGER *SIG) {
  raise(*SIG);
}

/*************************************************************
* set message using c string input
*
* Written July 2002 by Charles C Burch
************************************************************/
void dbug_set_message_c(char *msg) {
  int n;

  n=strlen(msg);
  dbug_set_message_f(msg,&n);
  return;
}

/*************************************************************
* set message using f string input
*
* Written July 2002 by Charles C Burch
************************************************************/
void dbug_set_message_f(char *msg, INTEGER *N_MSG) {
  int n;

  n=(*N_MSG);
  if(n>=sizeof(dbug_message)) n=sizeof(dbug_message)-1;

  if(n>0) memcpy(dbug_message, msg, n);
  dbug_message[n]='\0';
  return;
}

/*************************************************************
* get message using c string output
*
* Written July 2002 by Charles C Burch
************************************************************/
void dbug_get_message_c(char *msg) {
  strcpy(msg, dbug_message);
  return;
}

/*************************************************************
* set message using f string output
*
* Written July 2002 by Charles C Burch
************************************************************/
void dbug_get_message_f(char *msg, INTEGER *N_MSG) {
  int n, len;
 
  len=strlen(dbug_message);
  n=(*N_MSG);

  if(n<=len) {
    memcpy(msg,dbug_message,n);
  } else {
    memcpy(msg,dbug_message,len);
    memset(msg+len,' ',n-len);
  }
  return;
}

/*************************************************************
* set system message using c string input
*
* Written July 2002 by Charles C Burch
************************************************************/
void dbug_set_system_message_c(char *msg) {
  int n;

  n=strlen(msg);
  dbug_set_system_message_f(msg,&n);
  return;
}

/*************************************************************
* set system message using f string input
*
* Written July 2002 by Charles C Burch
************************************************************/
void dbug_set_system_message_f(char *msg, INTEGER *N_MSG) {
  int n;

  n=(*N_MSG);
  if(n>=sizeof(dbug_message)) n=sizeof(dbug_message)-1;

  if(n>0) memcpy(dbug_system_message, msg, n);
  dbug_system_message[n]='\0';
  return;
}

/*************************************************************
* get system debug message using c string output
*
* Written July 2002 by Charles C Burch
************************************************************/
void dbug_get_system_message_c(char *msg) {
  strcpy(msg, dbug_system_message);
  return;
}

/*************************************************************
* get system message using f string output
*
* Written July 2002 by Charles C Burch
************************************************************/
void dbug_get_system_message_f(char *msg, INTEGER *N_MSG) {
  int n, len;
 
  len=strlen(dbug_system_message);
  n=(*N_MSG);

  if(n<=len) {
    memcpy(msg,dbug_system_message,n);
  } else {
    memcpy(msg,dbug_system_message,len);
    memset(msg+len,' ',n-len);
  }
  return;
}

/*************************************************************
* default dbug signal handler
*
* Written July 2002 by Charles C Burch
************************************************************/
void dbug_sig_handler(int sig) {
  fprintf(stderr, "+++++++++++++++++++++++++++++++++++++++++++\n");
  fprintf(stderr, "+++++++ DBUG Signal Handler Invoked +++++++\n");
  fprintf(stderr, "+++++++++++++++++++++++++++++++++++++++++++\n");
  fprintf(stderr, "\n");

  fprintf(stderr, "  %s\n",dbug_sig_to_alpha(sig));
  fprintf(stderr, "  dbug message=%s\n",dbug_message);
  fprintf(stderr, "  dbug system message=%s\n",dbug_system_message);
  fflush(stderr);
  signal(SIGABRT,SIG_DFL);
  abort();
}

/*************************************************************
* install default dbug signal handler
*
* Written July 2002 by Charles C Burch
************************************************************/
void dbug_install_signal_handler_c()
{
  struct sigaction action;

  action.sa_handler   = dbug_sig_handler;
  sigemptyset(&action.sa_mask);
  sigaddset(&action.sa_mask,SIGUSR1);
  sigaddset(&action.sa_mask,SIGUSR2);
  sigaddset(&action.sa_mask,SIGTERM);
  action.sa_flags     = SA_NODEFER;

  if(sigaction(SIGINT, &action, (struct sigaction *) NULL) < 0) {
   fprintf(stderr, "dbug_install: signal call failed for SIGINT\n");
  }

  if(sigaction(SIGQUIT, &action, (struct sigaction *) NULL) < 0) {
   fprintf(stderr, "dbug_install: signal call failed for SIGQUIT\n");
  }

  if(sigaction(SIGILL, &action, (struct sigaction *) NULL) < 0) {
   fprintf(stderr, "dbug_install: signal call failed for SIGILL\n");
  }

  if(sigaction(SIGBUS, &action, (struct sigaction *) NULL) < 0) {
   fprintf(stderr, "dbug_install: signal call failed for SIGBUS\n");
  }

  if(sigaction(SIGFPE, &action, (struct sigaction *) NULL) < 0) {
   fprintf(stderr, "dbug_install: signal call failed for SIGFPE\n");
  }

  if(sigaction(SIGUSR1, &action, (struct sigaction *) NULL) < 0) {
   fprintf(stderr, "dbug_install: signal call failed for SIGUSR1\n");
  }

  if(sigaction(SIGSEGV, &action, (struct sigaction *) NULL) < 0) {
   fprintf(stderr, "dbug_install: signal call failed for SIGSEGV\n");
  }

  if(sigaction(SIGUSR2, &action, (struct sigaction *) NULL) < 0) {
   fprintf(stderr, "dbug_install: signal call failed for SIGUSR2\n");
  }

  if(sigaction(SIGPIPE, &action, (struct sigaction *) NULL) < 0) {
   fprintf(stderr, "dbug_install: signal call failed for SIGPIPE\n");
  }

  if(sigaction(SIGTERM, &action, (struct sigaction *) NULL) < 0) {
   fprintf(stderr, "dbug_install: signal call failed for SIGTERM\n");
  }

  if(sigaction(SIGTSTP, &action, (struct sigaction *) NULL) < 0) {
   fprintf(stderr, "dbug_install: signal call failed for SIGTSTP\n");
  }

  if(sigaction(SIGXCPU, &action, (struct sigaction *) NULL) < 0) {
   fprintf(stderr, "dbug_install: signal call failed for SIGXCPU\n");
  }

  if(sigaction(SIGXFSZ, &action, (struct sigaction *) NULL) < 0) {
   fprintf(stderr, "dbug_install: signal call failed for SIGXFSZ\n");
  }

  action.sa_flags     = SA_RESETHAND;
  if(sigaction(SIGABRT, &action, (struct sigaction *) NULL) < 0) {
   fprintf(stderr, "dbug_install: signal call failed for SIGABRT\n");
  }
  return;
}


/*************************************************************
* Convert an alpha string to corresponding SIG integer value
*
* Written July 2002 by Charles C Burch
************************************************************/
int dbug_alpha_to_sig(char *a) {
  if(strcmp(a,"SIGQUIT")==0) return(SIGQUIT);
  if(strcmp(a, "SIGILL")==0) return(SIGILL);
  if(strcmp(a, "SIGABRT")==0) return(SIGABRT);
  if(strcmp(a, "SIGBUS")==0) return(SIGBUS);
  if(strcmp(a, "SIGFPE")==0) return(SIGFPE);
  if(strcmp(a, "SIGUSR1")==0) return(SIGUSR1);
  if(strcmp(a, "SIGSEGV")==0) return(SIGSEGV);
  if(strcmp(a, "SIGUSR2")==0) return(SIGUSR2);
  if(strcmp(a, "SIGPIPE")==0) return(SIGPIPE);
  if(strcmp(a, "SIGTERM")==0) return(SIGTERM);
  if(strcmp(a, "SIGTSTP")==0) return(SIGTSTP);
  if(strcmp(a, "SIGXCPU")==0) return(SIGXCPU);
  if(strcmp(a, "SIGXFSZ")==0) return(SIGXFSZ);
  return(-1);
}

/*************************************************************
* Convert a SIG integer value to an alpha description
*
* Written July 2002 by Charles C Burch
************************************************************/
char* dbug_sig_to_alpha(int sig) {

  switch(sig) { 
  case (SIGQUIT) :
    return("SIGQUIT - Quit from keyboard");

  case (SIGILL) :
    return("SIGILL - Illegal Instruction");

  case (SIGABRT) :
    return("SIGABRT - Abort signal from abort function");

  case (SIGBUS) :
    return("SIGBUS - Bus Error");

  case (SIGFPE) :
    return("SIGFPE - Floating point exception has occured");

  case (SIGUSR1) :
    return("SIGUSR1 - User-defined signal 1");

  case (SIGSEGV) :
    return("SIGSEGV - Invalid memory reference has occured");

  case (SIGUSR2) :
    return("SIGUSR2 - User-defined signal 2"); 
  
  case (SIGPIPE) :
    return("SIGPIPE - Broken pipe");

  case (SIGTERM) :
    return("SIGTERM - Termination");

  case (SIGTSTP) :
    return("SIGTSTP - Stop typed from keyboard");

  case (SIGXCPU) :
    return("SIGXCPU - CPU time limit exceeded");

  case (SIGXFSZ) :
    return("SIGXFSZ - File size limit exceeded");

  default:
    sprintf(dbug_work, "Unknown signal - sig=%d",sig);
    return(dbug_work);
  }
}

#ifdef __cplusplus
}
#endif


/*-------------------------------- end ------------------------------------*/
/*-------------------------------- end ------------------------------------*/
/*-------------------------------- end ------------------------------------*/

/* removed by Tom Stoeckley 2005-03-22:
#ifdef __cplusplus
}
#endif
*/

/********************** Basic test driver ***********************
#include <stdlib.h>
#include <stdio.h>
#include <signal.h>

#include "dbug.h"
#include "c2f_interface.h"

int main() {
  char msg[80];
  INTEGER N;
  int sig;

  dbug_install_signal_handler_c(); 

  dbug_get_message_c(msg);
  printf("Msg=%s\n",msg);

  dbug_set_message_c("Message1");
  dbug_get_message_c(msg);
  printf("Msg=%s\n",msg);

  strcpy(msg,"Message2");
  N=strlen(msg);
  dbug_set_message_f(msg,&N);

  N=sizeof(msg);
  dbug_get_message_f(msg,&N);
  msg[79]='\0';
  printf("Msg=%s\n",msg);
  
  dbug_get_system_message_c(msg);
  printf("Msg=%s\n",msg);

  dbug_set_system_message_c("System Message1");
  dbug_get_system_message_c(msg);
  printf("Msg=%s\n",msg);

  strcpy(msg,"System Message2");
  N=strlen(msg);
  dbug_set_system_message_f(msg,&N);

  N=sizeof(msg);
  dbug_get_system_message_f(msg,&N);
  msg[79]='\0';
  printf("Msg=%s\n",msg);
  
  printf("signals are %s\n%s\n", 
    "  SIGQUIT SIGILL SIGABRT SIGBUS SIGFPE SIGUSR1",
    " SIGSEGV SIGUSR2 SIGPIPE SIGTERM SIGTSTP SIGXCPU SIGXFSZ");
  printf("Enter one of the above:");
  scanf("%s",msg);
  sig=dbug_alpha_to_sig(msg);
  if(sig<0) {
    printf("signal=%s\n",dbug_sig_to_alpha(sig));
  } else {
    raise(sig);
  }
  return(0);
}
********************************************************/

/*-------------------------------- end ------------------------------------*/
/*-------------------------------- end ------------------------------------*/
/*-------------------------------- end ------------------------------------*/
