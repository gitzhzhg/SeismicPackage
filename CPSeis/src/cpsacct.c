/*<CPS_v1 type="PRIMITIVE"/>
!----------------------------- cpsacct.c -------------------------------
!----------------------------- cpsacct.c -------------------------------
!----------------------------- cpsacct.c -------------------------------

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
! Name       : cpsacct
! Category   : main_prog
! Written    : 2006-01-26   by: Bill Menger
! Revised    : 2009-06-11   by: Bill Menger
! Maturity   : beta
! Purpose    : provides access to CPS accounting logging facility
! References : These routines are called by pfio and are for general system use
!-------------------------------------------------------------------------------
!</brief_doc>

!<descript_doc>
!-------------------------------------------------------------------------------
! These routines support writing messages to the cps accounting file 
!
!-------------------------------------------------------------------------------
!</descript_doc>

!<calling_doc>
!-------------------------------------------------------------------------------
! void cpsacct_init() 
!   Purpose: does set up for use of the cps_acct_file
!
! void cpsacct_message(char *message) 
!   Purpose: writes a message to the cps_acct_file
!
! void cpsacct_exit()
!   Purpose: does wrapup of the cps_acct_file
! 
!-------------------------------------------------------------------------------
!</calling_doc>

!<advice_doc>
  This set of calls will use the system logging facility in Linux/Unix.  It will
  use the LOCAL4 log which will by default send messages to /var/log/messages.
  If you set this up properly in the /etc/syslog.conf file, you can let linux
  forward the messages to a globally accessible server where it is routed to 
  a file.  This allows you to use the Linux/Unix system to do your logging
  rather than doing it yourself.  All you need is a program to parse your log
  file, sort/extract relevant data, and you're done.
  In /etc/sysconf make sure you have:
  local4.* @globalservername (where globalservername is the name of the server
  where you will be collecting all of the log messages)
  In cps we use local4 for accounting and local6 for error/status logging, so
  in Houston we have:
  local4.* @hoeplm03
  local6.* @hoeplm03

  Then, on hoeplm03 in /etc/syslog.conf, we have the following:
  local4.* /usr/app/cps_log/cps_acct_log
  local6.* /usr/app/cps_log/cps_syslog

  There is a standalone program to read the cps_acct_log (cpsacctprg.c) and
  another for the system log.
!</advice_doc>

!<history_doc>
!-------------------------------------------------------------------------------
!                     AUXILIARY FILE REVISION HISTORY
!     Date        Author         Description
!     ----        ------         -----------
!  6. 2009-06-11  Bill Menger    Modified local logging to go to .cpseis dir.
!  5. 2007-04-24  Bill Menger    Changed the name of the local log files.
!  4. 2007-02-15  Bill Menger    Modified message to log if sysacct not set up
!                                correctly.
!  3. 2007-01-25  Bill Menger    Modified to allow LGC changes.
!  2. 2006-10-31  Bill Menger    Added unit tests, documentation.
!  1. 2006-01-31  Bill Menger    Initial version
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

char *cpsacct_crou_ident =
"$Id: cpsacct.c,v 1.5 2007/04/25 15:46:23 Menger beta sps $";

int cpsacct_init_sw=0;

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>
#include <sys/types.h>
#include <pwd.h>
#include <syslog.h>

#include "cpsacct.h"
#include "cnfg.h"

char cpsacct_user_pid[300];
static struct passwd ui;
static FILE * acctout;

/*------------------------ start of functions ----------------------------*/
/*------------------------ start of functions ----------------------------*/
/*------------------------ start of functions ----------------------------*/


#ifdef __cplusplus
extern "C" {
#endif

/**********************************************************
* Set up to use cps_acctfile
*
* Written January 2006 by William M Menger
**********************************************************/
void cpsacct_init() {
  struct passwd *user_info=&ui;
  char unam[100],udir[200];
  char *user_name=unam;
  char *user_dir=udir;
  int i;

  if(cpsacct_init_sw!=0) return;

  /* If local4 is mentioned in /etc/syslog.conf, assume
   * we are at ConocoPhillips and keep original behavior.
   * If not, put all cpsacct messages in local file in home directory.
   */
  if(cpsacct_redirect())
    cpsacct_init_sw=3; /* 1 for openlog, 2 for logger, 3 no openlog */
  else {
    cpsacct_init_sw=4; /* 4 for local accounting file */
    /* use acctout when no logging available */
  }

  user_info=getpwuid(geteuid());
  i=0;
  while(user_info==NULL && i<20) {
    sleep(1);
    user_info=getpwuid(geteuid());
    i++;
  }

  if(user_info==NULL) {
    strcpy(user_name,"NONE");
    strcpy(user_dir,"./");
  } else {
    user_name=user_info->pw_name;
    user_dir =user_info->pw_dir;
  }

  if(cpsacct_init_sw==4) {
    strcpy(cpsacct_user_pid,user_dir);
    strcat(cpsacct_user_pid,"/.cpseis/cpseis.accounting");
    acctout = fopen(cpsacct_user_pid,"a+");
    if(acctout == NULL) {
     fprintf(stderr,"cpsacct:%d: Unable to open accounting file %s\n",__LINE__,cpsacct_user_pid);
     exit(-1);
    }
    /* 
    fprintf(acctout, "System accounting for cps is not turned on for your workstation.\n");
    fprintf(acctout, "In the /etc/syslog.conf file there should be a line such as:\n");
    fprintf(acctout, "local4.* @server (where server is the name of the server collecting accounting info.)\n");
   */
  }

  if(cpsacct_init_sw==3) strcat(cpsacct_user_pid," - ");

  if(cpsacct_init_sw==1) {
    openlog(cpsacct_user_pid,0, LOG_LOCAL4|LOG_NOTICE);
  }

  if(i>0) {
    if(user_info!=NULL) {
      cpsacct_message(
        "Warning: Multiple attempts made to getuid in cpsacct_init");
    } else {
      cpsacct_message(
        "Error: Unable to getuid in cpsacct_init");
    }
  }
  return;
}

/**********************************************************
* Write a message to cps logfile
*
* Written January 2006 by William M Menger
**********************************************************/
void cpsacct_message(char *message) {
  char cmd[1024];

  if(cpsacct_init_sw==0) cpsacct_init();

  if(cpsacct_init_sw==1) {
      syslog(LOG_LOCAL4|LOG_NOTICE,message);
  } else if(cpsacct_init_sw==2) {
      sprintf(cmd,"logger -i -p local4.notice \"%s\"", message);
      system(cmd);
  } else if (cpsacct_init_sw == 4) {
      fprintf(acctout,"%s\n",message);
  } else {    
      strcpy(cmd,cpsacct_user_pid);
      strcat(cmd,message);
      syslog(LOG_LOCAL4|LOG_NOTICE,cmd);
  }
  return;
}

/**********************************************************
* Wrap-up for cps_logfile usage
*
* Written January 2006 by William M Menger
**********************************************************/
void cpsacct_exit(){
  if(cpsacct_init_sw==0) return;

  if(cpsacct_init_sw != 4) {
    closelog();
  } else {
    fclose(acctout);
  }
  cpsacct_init_sw=0;
  return;
}
/**********************************************************
* Is cps_logfile redirected?
* If local4 is mentioned in /etc/syslog.conf,
* then answer is yes; otherwise, answer is no.
**********************************************************/
static int cpsacct_redirect() {
  FILE *fd;
  char line[300];
  int ians, success;
  struct passwd *user_info = &ui;

  ians = 0; /* cps_logfile is not redirected */

  /* Open file. If open fails, assume answer is no. */
  if((fd=fopen("/etc/syslog.conf","r"))==NULL) {
    ians = -1;  
  }

  if(ians == 0) { /* Read each line in file */
    while((cnfg_get_line(line,sizeof(line),fd))>=0) {
      /* Is local4 mentioned in this line? */
      if (strstr(line, "local4")) {
        /* It is. Don't care where local4 goes, just that it is mentioned. */
        ians = 1;
        break;
      }
    }
    fclose(fd);
  }

  if(ians < 1 ) { /* Then the file was NOT opened, and I need to do something */
    /* Create a directory for logging for the user in his/her home area */
    user_info = getpwuid(geteuid()); /* get user information */
    strcpy(line,user_info->pw_dir);  /* copy home directory into a string */
    strcat(line,"/.cpseis");      /* Append .cpseis to the user's home directory */
    if(access( line,W_OK) != 0) { /* Then I have trouble writing to this directory or
                                   it doesn't exist, so try to make it */
      success=mkdir(line, (mode_t) 0777); /* open access read write everyone */
      if(success != 0 ) fprintf(stderr,"Unable to create user accounting directory.\n");
      /* Don't fail, but send the error message and move on */
    }   
    ians=0; /* cps_acctfile is not being redirected, write to home .cpseis dir. */
  }

  return ians; /* either 1 (redirected) or 0 (write to .cpseis in home area)  */
}


/*** ADD UNIT TESTS FOR THESE ROUTINES (wmm) ***/
#ifdef TEST

#include <stdlib.h>
#include <stdio.h>
#include "cpsacct.h"

int main() {
  int i;
  char message1[100], message2[100];

  cpsacct_init();

  memset(message1,'1',sizeof(message1));
  message1[99]='\0';

  memset(message2,'2',sizeof(message2));
  message2[99]='\0';

  for(i=1; i<10; i++) {
    cpsacct_message(message1);
    cpsacct_message(message2);
  }
  cpsacct_exit();
  fprintf(stderr,"Check /var/log/messages for 11111 22222 messages\n");
  return(0);
}

#endif
/*** END OF TESTS                        ***/

#ifdef __cplusplus
}
#endif

/*-------------------------------- end ------------------------------------*/
/*-------------------------------- end ------------------------------------*/
/*-------------------------------- end ------------------------------------*/
