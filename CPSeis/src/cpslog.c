/*<CPS_v1 type="PRIMITIVE"/>
!----------------------------- cpslog.c -------------------------------
!----------------------------- cpslog.c -------------------------------
!----------------------------- cpslog.c -------------------------------

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
! Name       : CPSLOG
! Category   : main_prog
! Written    : 2002-06-18   by: Charles C. Burch
! Revised    : 2009-06-11   by: Bill Menger
! Maturity   : beta
! Purpose    : provides access to CPS logging facility
! References : These routines are called by pfio and are for general system use
!-------------------------------------------------------------------------------
!</brief_doc>

!<descript_doc>
!-------------------------------------------------------------------------------
! These routines support writing messages to the cps log_file 
!
!-------------------------------------------------------------------------------
!</descript_doc>

!<calling_doc>
!-------------------------------------------------------------------------------
! void cpslog_init() 
!   Purpose: does set up for use of the cps_logfile
!
! void cpslog_message(char *message) 
!   Purpose: writes a message to the cps_logfile
!
! void cpslog_exit()
!   Purpose: does wrap of the cps_log_file
! 
!-------------------------------------------------------------------------------
!</calling_doc>

!<history_doc>
!-------------------------------------------------------------------------------
!                     AUXILIARY FILE REVISION HISTORY
!     Date        Author         Description
!     ----        ------         -----------
!  7. 2009-06-10  Bill Menger    Modified to create loggind directory and put all
!                                of a user's log info there.
!  6. 2007-04-24  Bill Menger    Modified the name of the local log file.
!  5. 2007-02-15  Bill Menger    Modified the log message if syslog.conf not set up.
!  4. 2007-01-25  Bill Menger    Modified to match lgc version.
!  3. 2006-01-31  Bill Menger    Changed init_sw to cpslog_init_sw
!  2. 2004-01-21  C. C. Burch    Minor change for SGI warning.
!  1. 2002-06-18  Chuck C. Burch Initial Version.
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

char *cpslog_crou_ident =
"$Id: cpslog.c,v 1.6 2007/04/25 15:46:23 Menger beta sps $";

int cpslog_init_sw=0;

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>
#include <sys/stat.h>
#include <sys/types.h>
#include <pwd.h>
#include <syslog.h>

#include "cpslog.h"

static char cpslog_user_pid[300];
static struct passwd ui;
static FILE * logout;

/*------------------------ start of functions ----------------------------*/
/*------------------------ start of functions ----------------------------*/
/*------------------------ start of functions ----------------------------*/


#ifdef __cplusplus
extern "C" {
#endif

/**********************************************************
* Is cps_logfile redirected?
* If local6 is mentioned in /etc/syslog.conf,
* then answer is yes; otherwise, answer is no.
**********************************************************/
int cpslog_redirect() {
  FILE *fd;
  char line[300];
  int ians,success;
  struct passwd *user_info = &ui;

  ians = 0; /* cps_logfile is not redirected */

  /* Open file. If open fails, assume answer is no. */
  if((fd=fopen("/etc/syslog.conf","r"))==NULL) {
    ians = -1;
  }

  if(ians == 0 ) { /* Then the file WAS opened, so let's read it. */
    /* Read each line in file */
    while((cnfg_get_line(line,sizeof(line),fd))>=0) {
      /* Is local6 mentioned in this line? */
      if (strstr(line, "local6")) {
        /* It is. Don't care where local6 goes, just that it is mentioned. */
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
      if(success != 0 ) fprintf(stderr,"Unable to create user logging directory.\n");
      /* Don't fail, but send the error message and move on */
    }
    ians=0; /* cps_logfile is not being redirected, write to home .cpseis dir. */
  }

  return ians; /* either 1 (redirected) or 0 (write to .cpseis in home area)  */
}

/**********************************************************
* Set up to use cps_logfile
*
* Written June 2002 by Charles C Burch
**********************************************************/
void cpslog_init() {
  struct passwd *user_info=&ui;
  char unam[100],udir[200];
  char *user_name=unam;
  char *user_dir=udir;
  int i;

  if(cpslog_init_sw!=0) return;

  /* If local6 is mentioned in /etc/syslog.conf, assume
   * we are at ConocoPhillips and keep original behavior.
   * If not, put all cpslog messages in .cpseis directory.
   */
  if(cpslog_redirect())
    cpslog_init_sw=3; /* 1 for openlog, 2 for logger, 3 no openlog */
  else {
    cpslog_init_sw=4; /* 4 for stdout */
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
  
  if(cpslog_init_sw==4) {
    strcpy(cpslog_user_pid,user_dir); 
    strcat(cpslog_user_pid,"/.cpseis/cpseis.log");
    logout = fopen(cpslog_user_pid,"a+");
    if(logout == NULL) {
      fprintf(stderr,"cpslog:%d: Unable to open log file %s\n",__LINE__,cpslog_user_pid);
      exit(-1);
    }
    /*
    fprintf(logout, "System logging for cps is not turned on for your workstation.\n");
    fprintf(logout, "In the /etc/syslog.conf file there should be a line such as:\n");
    fprintf(logout, "local6.* @server (where server is the name of the server collecting log messages.)\n");
    */
  }

  if(cpslog_init_sw==3) strcat(cpslog_user_pid," - ");

  if(cpslog_init_sw==1) {
    openlog(cpslog_user_pid,0, LOG_LOCAL6|LOG_NOTICE);
  }

  if(i>0) {
    if(user_info!=NULL) {
      cpslog_message(
        "Warning: Multiple attempts made to getuid in cpslog_init");
    } else {
      cpslog_message(
        "Error: Unable to getuid in cpslog_init");
    }
  }
  return;
}

/**********************************************************
* Write a message to cps logfile
*
* Written June 2002 by Charles C Burch
**********************************************************/
void cpslog_message(char *message) {
  char cmd[1024];

  if(cpslog_init_sw==0) cpslog_init();

  if(cpslog_init_sw==1) {
    syslog(LOG_LOCAL6|LOG_NOTICE,message);
  } else if(cpslog_init_sw==2) {
    sprintf(cmd,"logger -i -p local6.notice \"%s\"", message);
    system(cmd);
  } else if (cpslog_init_sw == 4) {
     fprintf(logout, "%s\n",message);
  } else {    
    strcpy(cmd,cpslog_user_pid);
    strcat(cmd,message);
    syslog(LOG_LOCAL6|LOG_NOTICE,cmd);
  }
  return;
}

/**********************************************************
* Wrap-up for cps_logfile usage
*
* Written June 2002 by Charles C Burch
**********************************************************/
void cpslog_exit(){
  if(cpslog_init_sw==0) return;

  if(cpslog_init_sw != 4 ) {
    closelog();
  } else {
    fclose(logout);
  }
  cpslog_init_sw=0;
  return;
}

/************************ test driver ***********************  
#include <stdlib.h>
#include <stdio.h>
#include "cpslog.h"

int main() {
  int i;
  char message1[100], message2[100];

  cpslog_init();

  memset(message1,'1',sizeof(message1));
  message1[99]='\0';

  memset(message2,'2',sizeof(message2));
  message2[99]='\0';

  for(i=1; i<10; i++) {
    cpslog_message(message1);
    cpslog_message(message2);
  }
  cpslog_exit();
  return(0);
}

**************************************************************/

#ifdef __cplusplus
}
#endif

/*-------------------------------- end ------------------------------------*/
/*-------------------------------- end ------------------------------------*/
/*-------------------------------- end ------------------------------------*/
