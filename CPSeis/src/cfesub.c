/****
!<CPS_v1 type="PROGRAM"/>
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
!                       C P S   P R O G R A M
!
! Name       : cfesub
! Category   : stand-alone
! Written    : 1999-12-28   by: Donna K. Vunderink
! Revised    : 2004-07-22   by: Goodger
! Maturity   : beta
! Purpose    : Submit CPS Job Files
! Portability: No known limitations.
!
!-------------------------------------------------------------------------------
!</brief_doc>


!<descript_doc>
!-------------------------------------------------------------------------------
!                         GENERAL DESCRIPTION
!
! Submit CPS batch jobfiles into either a NQS or PBS batch queue for execution.
!
!   Usage:  cfesub [-n] [-p] [-a date-time] [-i] [-s seriesfile] jobfile
!
!           -a      Run request after stated time
!                       Format: DD-MMM-YYYY HH:MM
!                               where  DD       day
!                                      MMM      month (Jan, Feb, etc.)
!                                      YYYY     year
!                                      HH       hour
!                                      MM       minutes
!           -i      Independent series
!           -n      Use NQS
!           -p      Use PBS (default)
!           -s      Execute cfeseries at completion using seriesfile
!
!           jobfile     Name if CPS jobfile
!
!-------------------------------------------------------------------------------
!</descript_doc>


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
!
!     Date        Author       Description
!     ----        ------       -----------
! 30. 2004-07-22  Goodger      Use name ponylm01 for pony submits.
! 29. 2004-04-21  Goodger      Get full path for cfeseries from config file.
! 28. 2004-04-21  Goodger      Get paths from config file for qserver_nodes
!                              and retry script.
! 27. 2004-03-29  Goodger      Use ip address for remote submit to PONY rather
!                              than name ponylm01.
! 26. 2004-03-26  Goodger      Remote submit to PONY.
! 25. 2004-02-04  Goodger      Increase size of card array to accomodate
!                              remote submit strings.
! 24. 2004-02-03  Goodger      Change Alaska remote submit node to akeplx100.
! 23. 2004-01-07  Goodger      Remove sps from the rcp and rsh commands for
!                              for remote submits. Call cfesub_remotename to
!                              get a temporary remote name.
! 22. 2004-01-05  Goodger      Call routine cfesub_jobdatainfo.
! 21. 2004-01-05  Goodger      Determine alpha, beta, or production and
!                              execute appropriate version of cfeseries.
!                              Add call to routine cfesub_timestamp.
! 20. 2003-12-16  Goodger      Submit alaska remote job.
! 19. 2003-10-21  SMCook       Modified to return the error code from the system
!                               calls rather than ignore them -- heretofore the
!                               "caller" had no direct way of detecting that an
!                               error occurred.
! 18. 2002-01-30  Vunderink    Modified to use retry script for qsubs to PBS
! 17. 2001-11-26  Vunderink    Get PBS server from configuration file
! 16. 2001-08-02  Vunderink    Removed forcing poepsn03 to use NQS
! 15. 2001-05-06  Vunderink    Changed PBS node to poeplx184
! 14. 2001-04-11  Vunderink    Added -i option for independent series
! 13. 2001-03-29  Vunderink    Changed PBS node to poeplx05
! 12. 2001-03-01  Vunderink    Changed PBS node to poeplx184
! 11. 2001-02-26  Vunderink    Fixed using from poepsn03
! 10. 2001-02-13  Vunderink    Changed default to PBS
!  9. 2001-02-01  Vunderink    Increased size of str and str2 strings.
!  8. 2001-01-23  Vunderink    Added support for PBS queuing system, options
!                                to choose between NQS and PBS, and if full
!                                path not supplied, added current directory on
!                                to filename.
!  7. 2000-05-04  Vunderink    Fixed bug causing linux version to abort
!  6. 2000-02-27  Vunderink    Added -a option.
!  5. 2000-02-06  Vunderink    Modified cfeseries command to new format and
!                                fixed rsh by adding blank after -n.
!  4. 2000-02-06  Vunderink    Added -n to rsh and print return status from
!                                system call.
!  3. 2000-01-26  Vunderink    Initialize seriesfile and filename to NULL
!  2. 2000-01-25  Vunderink    Change job series executable to cfeseries and
!                                delete temp job after spooled
!  1. 1999-12-28  Vunderink    Initial version.
!
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
! No special requirements.
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


!!------------------------- start of program ------------------------------!!
!!------------------------- start of program ------------------------------!!
!!------------------------- start of program ------------------------------!!
****/

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <ctype.h>
#include <unistd.h>
#include <limits.h>

#include "c2f_interface.h"
#include "cgetsys.h"
#include "cnfg.h"

#ifdef NEED_UNDERSCORE
#define cfesub_timestamp        cfesub_timestamp_
#define cfesub_jobdatainfo      cfesub_jobdatainfo_
#define cfesub_remotename       cfesub_remotename_
#endif

#ifdef NEED_CAPITALS
#define cfesub_timestamp        CFESUB_TIMESTAMP
#define cfesub_jobdatainfo      CFESUB_JOBDATAINFO
#define cfesub_remotename       CFESUB_REMOTENAME
#endif

#define LEN_ARRAY    165

void cfesub_timestamp(char *cfilename);
void cfesub_jobdatainfo(char *cfilename, char *rlocation);
void cfesub_remotename(char *cfilename, char *rname);

char cfesub_ident[100] =
"$Id: cfesub.c,v 1.30 2004/07/22 21:23:59 Goodger beta sps $";

int main(argc,argv)
int argc;
char *argv[];
{

  int  i = 1;
  int  k = 0;
  int  iflag = 0;
  int  after_EOS_flag = 0;
  size_t len;
  int lentempfile;
  char *wrkdir;
  char *filename;
  char *tempfile;
  char *shfile;
  char *seriesfile;
  char *delayed;
  char *tmpmnt;
  char retry_script[80];
  char QSERVER_FILE[80];
  char str[1024];
  char str2[1024];
  char dir[1024];
  char queue[LEN_ARRAY]    = " ";
  char server[LEN_ARRAY]   = " ";
  char jobqueue[LEN_ARRAY] = " ";
  char jobname[LEN_ARRAY]  = " ";
  char machine[LEN_ARRAY]  = " ";
  char batchsystem[4]      = "PBS";
  char independent[4]      = "NO";
  char year[5];
  char month[5];
  char mm[3];
  char day[3];
  char hours[3];
  char minutes[3];
  char rlocation[9] = " ";
  char rname[81] = " ";
  char card[1024];
  char cfilename[1024];
  char serprog[81];
  FILE *cfp;
  FILE *ifp;
  FILE *ofp;

  if (argc == 1) {
    printf(
           "Usage:  cfesub [-n] [-p] [-a date-time] [-i] [-s seriesfile] ");
    printf("[-r remote_location] jobfile\n");
    printf("\n");
    printf("         -a      Run request after stated time\n");
    printf("         -i      Independent series\n");
    printf("         -n      Use NQS          \n");
    printf("         -p      Use PBS (default)\n");
    printf("         -s      Execute cfeseries at completion\n");
    printf("         -r      Submit to remote location indicated\n");
    printf(" Open Source Version 5/11/2011 Revision\n");
    printf("\n");
    exit(1);
  }

  filename   = NULL;
  seriesfile = NULL;
  delayed    = NULL;

  /*strcpy(QSERVER_FILE,cnfg_get_value_c("sps_home_dir"));
  strcat(QSERVER_FILE,"/");
  */
  strcpy(QSERVER_FILE,cnfg_get_value_c("sps_install_dir"));
  strcat(QSERVER_FILE,"/etc/qserver_nodes.dat");

  /*strcpy(retry_script,cnfg_get_value_c("sps_home_dir"));
  strcat(retry_script,"/");
  */
  strcpy(retry_script,cnfg_get_value_c("sps_install_dir"));
  strcat(retry_script,"/scripts/qsub_retry");

  k=cgetsys_ostype();
  switch(k) {
  case (CGETSYS_LINUX):
    strcpy(serprog,cnfg_get_value_c("bin_path_linux1"));
    break;
  case (CGETSYS_SOLARIS):
    strcpy(serprog,cnfg_get_value_c("bin_path_solaris1"));
    break;
  }
  strcat(serprog,"/");



  while (i < argc) {
    if (argv[i][0] != '-') {
      if (iflag == 2)
        delayed = argv[i];
      else if (iflag == 1)
        seriesfile = argv[i];
      else
        filename = argv[i];
      iflag = 0;
    }
    else {
      switch(toupper(argv[i][1])) {
         case 'A':
               iflag = 2;
               break;
         case 'I':
               strcpy(independent,"YES");
               break;
         case 'N':
               strcpy(batchsystem,"NQS");
               break;
         case 'P':
               strcpy(batchsystem,"PBS");
               break;
         case 'S':
               iflag = 1;
               break;
         case 'R':
               strcpy(rlocation,argv[i+1]);
               break;
         default:
               printf("Invalid option \n Type 'cfesub' to see options\n");
               exit(1);
      }
    }
  i++;
  }

  if (filename == NULL) {
    printf("Error no jobfile specified\n");
    exit(1);
  }

  cfp = fopen(QSERVER_FILE,"r");
  if (cfp == NULL) {
     printf("Error opening: qserver file\n");
     exit(1);
  }

  ifp = fopen(filename,"r");
  if (ifp == NULL) {
     printf("Error opening: %s \n",filename);
     exit(1);
  }

  while (fscanf(ifp,"%80[^\n]\n",str) == 1) {
    i = sscanf(str,"# JOBNAME=%s MACHINE=%s",jobname,machine);
    if (i == 2) break;
  }

  rewind(ifp);
  while (fscanf(ifp,"%80[^\n]\n",str) == 1) {
    i = sscanf(str,"#PBS -q %s",jobqueue);
    if (i == 1) break;
  }
  while (fscanf(cfp,"%80[^\n]\n",str) == 1) {
    if (str[0] != 35) {
      i = sscanf(str,"%s %s",server,queue);
      if (strcmp(jobqueue,queue) == 0) break;
    }
  }
  fclose(cfp);

  len = strlen(filename);
  lentempfile = PATH_MAX+133 ;
  tempfile = malloc(lentempfile);
  strcpy(tempfile,filename);
  shfile=malloc(len+4);
  strcpy(shfile,filename);
  strcat(shfile,".sh");

  if (seriesfile != NULL) {
    k=cgetsys_library();
    switch(k) {
    case (CGETSYS_BETALIB):
      strcat(serprog,"cfeseriesbeta");
      break;
    case (CGETSYS_ALPHALIB):
      strcat(serprog,"cfeseriesalpha");
      break;
    case (CGETSYS_PRODLIB):
      strcat(serprog,"cfeseries");
      break;
    }
    strcat(tempfile,".temp");
    ofp = fopen(tempfile,"w");
    if (ofp == NULL) {
      printf("Error opening: %s \n",tempfile);
      exit(1);
    }
    rewind(ifp);
    while (fgets(str,81,ifp) != NULL) {
      if (strncmp(str,"# JOBNAME",9) == 0) {
        if (strcmp(batchsystem,"NQS") == 0) {
          strcpy(str2,"# QSUB -d\n");
        }
        fprintf(ofp,"%s",str2);
      }
      if (strcmp(independent,"YES") == 0 && strstr(str,"bscript") != NULL  &&
          after_EOS_flag > 0) {
        if (strcmp(batchsystem,"NQS") == 0)
          sprintf(str2,"%s -n -s %s %s",serprog,seriesfile,jobname);
        else
          sprintf(str2,"%s -p -s %s %s 1>cfeseries.out 2>&1",
             serprog,seriesfile,jobname);
        fprintf(ofp,"%s",str);
        fprintf(ofp,"%s\n",str2);
      }
      else if (strcmp(independent,"NO") == 0 && strncmp(str,"/EOS",4) == 0) {
        if (strcmp(batchsystem,"NQS") == 0)
          sprintf(str2,"%s -n -s %s %s",serprog,seriesfile,jobname);
        else
          sprintf(str2,"%s -p -s %s %s 1>cfeseries.out 2>&1",
                       serprog,seriesfile,jobname);
        fprintf(ofp,"%s\n",str2);
        fprintf(ofp,"%s",str);
      }
      else
        fprintf(ofp,"%s",str);
      if (strncmp(str,"/EOS",4) == 0) after_EOS_flag = 1;
    }
    fclose(ofp);
  }

  fclose(ifp);

  memcpy(cfilename,tempfile,180);
  if(strcmp(rlocation," ") == 0) {
    cfesub_jobdatainfo(cfilename,rlocation);
  }


  if (tempfile[0] != '/' && tempfile[0] != '~') {
    wrkdir = getcwd(dir,PATH_MAX+1);
    if (wrkdir != NULL) {
      tmpmnt = strstr(dir,"/tmpmnt/");
      if (tmpmnt != NULL) wrkdir = tmpmnt+7;
      sprintf(str,"%s/%s",wrkdir,tempfile);
      strcpy(tempfile,str);
    }
  }

  if (strcmp(batchsystem,"NQS") == 0) {
    if (delayed != NULL) {
      sprintf(str,"ssh -n %s 'qsub -a \"%s\" %s'",machine,delayed,tempfile);
    }
    else {
      sprintf(str,"ssh -n %s 'qsub %s'",machine,tempfile);
    }
  }
  else {
    if (delayed != NULL) {
      sscanf(delayed,"%2s-%3s-%4s %2s:%2s",day,month,year,hours,minutes);
      if (strcmp(month,"Jan") == 0)
        strcpy(mm,"01");
      else if (strcmp(month,"Feb") == 0)
        strcpy(mm,"02");
      else if (strcmp(month,"Mar") == 0)
        strcpy(mm,"03");
      else if (strcmp(month,"Apr") == 0)
        strcpy(mm,"04");
      else if (strcmp(month,"May") == 0)
        strcpy(mm,"05");
      else if (strcmp(month,"Jun") == 0)
        strcpy(mm,"06");
      else if (strcmp(month,"Jul") == 0)
        strcpy(mm,"07");
      else if (strcmp(month,"Aug") == 0)
        strcpy(mm,"08");
      else if (strcmp(month,"Sep") == 0)
        strcpy(mm,"09");
      else if (strcmp(month,"Oct") == 0)
        strcpy(mm,"10");
      else if (strcmp(month,"Nov") == 0)
        strcpy(mm,"11");
      else if (strcmp(month,"Dec") == 0)
        strcpy(mm,"12");
      sprintf(str,"ssh -n %s 'cd /var/tmp;%s -a %s%s%s%s%s %s'",
                       server,retry_script,year,mm,day,hours,minutes,tempfile);
    }
    else if(strcmp(rlocation,"ALASKA") == 0) {
       cfesub_remotename(cfilename,rname);
       strcpy(card,"rcp ");
       strcat(card,filename);
       strcat(card," ");
       strcat(card,"akeplx100.conocophillips.net:");
       strcat(card,"\"""/tmp/");
       strcat(card,rname);
       strcat(card,"\"");
       i=system(card);
       if(i != 0) {
         printf(" Unable to rcp job file to remote node\n");
         printf(" cmd = %s\n",card);
         fflush(stdout);
         return i;
       }
       strcpy(card,"ssh akeplx100.conocophillips.net \"cd /var/tmp;");
       strcat(card,"/usr/app/vendors/sps/scripts/qsub_retry /tmp/");
       strcat(card,rname);
       strcat(card,"\"");
       sprintf(str,"%s",card);

    }
    else if(strcmp(rlocation,"PONY") == 0) {
       cfesub_remotename(cfilename,rname);
       strcpy(card,"scp ");
       strcat(card,filename);
       strcat(card," ");
       strcat(card,"ponylm01:"); 
       /*     strcat(card,"10.107.0.1:");  */
       strcat(card,"\"""/tmp/");
       strcat(card,rname);
       strcat(card,"\"");
       i=system(card);
       if(i != 0) {
         printf(" Unable to scp job file to remote node\n");
         printf(" cmd = %s\n",card);
         fflush(stdout);
         return i;
       }
       strcpy(card,"ssh ponylm01 \"cd /var/tmp;"); 
       /*  strcpy(card,"ssh 10.107.0.1 \"cd /var/tmp;"); */
       strcat(card,"/home/sps/usr/app/vendors/sps/scripts/qsub_retry /tmp/");
       strcat(card,rname);
       strcat(card,"\"");
       sprintf(str,"%s",card);

    }
    else {
      sprintf(str,"ssh -n %s 'cd /var/tmp;%s %s | tee %s_submitinfo'",
              server,retry_script,tempfile,tempfile);
    }
  }
  fflush(stdout);
  i = system(str);
  i=0;
  if(i != 0) {
    printf("cfesub: system call 1 return value was %i\n", i);
    printf(" cmd = %s\n",str);
    fflush(stdout);
    return i;
  }
    cfesub_timestamp(cfilename);



  if (strcmp(batchsystem,"PBS") == 0) {
    if (seriesfile != NULL) {
      sprintf(str,"rm %s",tempfile);
      fflush(stdout);
         i = system(str);
      if(i != 0) {
        printf("cfesub: system call 2 return value was %i\n", i);
        fflush(stdout);
        return i;
      }
    }
  }

  return 0;
}
