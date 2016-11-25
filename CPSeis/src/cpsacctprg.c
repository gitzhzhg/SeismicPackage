/*
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
! Name       : cpsacctprg
! Category   : stand-alone
! Written    : 2001-05-09   by: Donna K. Vunderink
! Revised    : 2006-03-16   by: Bill Menger
! Maturity   : production
! Purpose    : Generate a summary report from the CPS accounting files
! Portability: No known limitations.
!
!-------------------------------------------------------------------------------
!</brief_doc>


!<descript_doc>
!-------------------------------------------------------------------------------
!                          GENERAL DESCRIPTION
!
! Generate a summary report from the CPS accounting files.
!
! Usage:
!        cpsacct [-a account] [-j jobname] [-u userid]
!                [-s start_date] [-e end_date] [-m maturity] [-b] [-p|r|x]
!
!        cpsacct [-a account] [-l userlist]
!                [-s start_date] [-e end_date] [-m maturity] [-b] [-p|r|x]
!
!        cpsacct [-a account] [-n joblist]
!                [-s start_date] [-e end_date] [-m maturity] [-b] [-p|r|x]
!
!
!        -a account       List only jobs matching the accounting code given by
!                           account.  Default is all accounting codes.
!
!        -j jobname       List only jobs matching jobname.  jobname may
!                           contain wildcard characters.  Default is all
!                           jobs.
!
!        -u userid        List only jobs for the user ID given by userid
!                           Default is all user IDs.
!
!        -l userlist      List only jobs for the user IDs in the file userlist.
!
!        -n joblist       List only jobs matching the jobnames in the file
!                           joblist.

!        -s start_date    List only jobs between start_date and end_date,
!                           inclusive.  Default is the current date.
!                           Date format: YYYY-MM-DD
!
!        -e end_date      List only jobs between start_date and end_date,
!                           inclusive.  Default is the current date.
!                           Date format: YYYY-MM-DD
!
!        -m maturity      List only jobs in the BETA, PROD, or BOTH
!                           CPS account files.  Default is BOTH.
!
!        -b               If present, list only jobs having a batch job id.
!
!        -p               If present, summary report is list of CPS processes
!                           only used all jobs matching the criteria.
!
!        -r               If present, output is in a raw space delimited format
!                           for Jobmon.
!
!        -x               If present, output is in tab delimited format
!                           for Microsoft Excel.
!
!-------------------------------------------------------------------------------
!</descript_doc>


!<advice_doc>
!-------------------------------------------------------------------------------
!                          ADVICE FOR USERS
!
!
!-------------------------------------------------------------------------------
!</advice_doc>


!<history_doc>
!-------------------------------------------------------------------------------
!                             REVISION HISTORY
!
!     Date        Author      Description
!     ----        ------      -----------
! 10. 2006-03-16  Bill Menger Modified to use new file names also.
!  9. 2006-02-21  Bill Menger Modified again to use BETA and PROD flags 
!                              correctly. 
!  8. 2006-02-17  Bill Menger Modified to read new accounting file format.
!  7. 2002-09-30  Vunderink   Added running jobs and changed delete search to
!                              monthly files.
!  6. 2002-06-22  Vunderink   Added joblist and tab delimited format.
!  5. 2002-02-04  Vunderink   Added cpu usage by process, added type of abort,
!                              added userlist, and added deleted jobs
!  4. 2001-06-19  Vunderink   Process Job Abort card in accounting file.
!  3. 2001-06-04  Vunderink   Added percentages and sort to process list, and
!                              added total summary to job list
!  2. 2001-06-01  Vunderink   Added -a acount and -b options and fixed problem
!                              breaking cards into jobs.
!  1. 2001-05-09  Vunderink   Initial version.
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
!                     SPECIAL COMPILING REQUIREMENTS
!
! No special requirements.
!
!-------------------------------------------------------------------------------
!</compile_doc>


!<algorithm_doc>
!-------------------------------------------------------------------------------
!                  ALGORITHM DESCRIPTION FOR DEVELOPERS
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
*/


#include <ctype.h>
#include <fnmatch.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>
#include <time.h>
#include "cnfg.h"

#define MIN(a,b) ( (a) < (b) ? (a) : (b) )
#define MAX(a,b) ( (a) > (b) ? (a) : (b) )
#define ABS(a)   ( (a) > 0 ? (a) : -(a) )
#define MAX_CNFG_ENTRIES 10
#define DBG(a) (cpsacctprg_debug > a)


char cpsacctprg_ident[100] =
"$Id: cpsacctprg.c,v 1.10 2006/03/17 14:59:49 Menger prod sps $";


/* Protypes for internal functions */

void cpsacct_sub (char *account, char *jobname, char *job_listfile,
                  char *user_listfile, char *userid,
                  char *maturity, char *start_date, char *end_date,
                  char *batch_only, char *procsum, 
                  char *rawfmt, char *excelfmt);
void cpsacct_to_upper(char *newstring, char *oldstring);
static int cpsacct_sort_key_pid (const void *i, const void *j);
static int cpsacct_sort_key_jid (const void *i, const void *j);
static int cpsacct_sort_key_num (const void *i, const void *j);

static int cpsacctprg_debug=0;


/* Typedefs */

typedef struct _card {
  char   key[91];
  char   pid[11];
  char   jid[21];
  char   jobname[21];
  char   userid[11];
  char   date[11];
  char   time[9];
  char   message[161];
} cardinfo;

typedef struct _process {
  char  name[21];
  float system_cpu;
  float user_cpu;
} processinfo;

typedef struct _allprocesses {
  char name[21];
  int  count;
  float system_cpu;
  float user_cpu;
} allprocessesinfo;

typedef struct _job {
  char         jid[21];
  char         jobname[21];
  char         userid[11];
  char         account[21];
  int          num_cpus;
  char         node[21];
  char         start_date[11];
  char         start_time[9];
  char         end_date[11];
  char         end_time[9];
  char         status[10];
  int          aborted_setup;
  int          aborted_processing;
  int          aborted_other;
  int          deleted;
  int          running;
  float        system_cpu;
  float        user_cpu;
  float        elapse;
  int          nprocesses;
  processinfo  *processes;
} jobinfo;

typedef struct _abort {
  char   jid[11];
  char   userid[11];
  char   jobname[21];
} abortinfo;

typedef struct _delete {
  char   jid[11];
  char   node[11];
  char   date[11];
  char   time[9];
} deleteinfo;

typedef struct _running {
  char   jid[11];
  char   node[11];
  char   userid[11];
} runninginfo;

typedef struct _list {
  char name[21];
} listinfo;

int main(argc,argv)
int argc;
char *argv[];
{
  char   *account;
  char   *end_date;
  char   *job_listfile       = NULL;
  char   *jobname;
  char   *maturity;
  char   *start_date;
  char   *user_listfile      = NULL;
  char   *userid;

  char   batch_only[4]       = {"NO"};
  char   default_account[4]  = {"ALL"};
  char   default_end[11];
  char   default_jobname[4]  = {"ALL"};
  char   default_maturity[5] = {"BOTH"};
  char   default_start[11];
  char   default_userid[4]   = {"ALL"};
  char   excelfmt[4]         = {"NO"};
  char   procsum[4]          = {"NO"};
  char   rawfmt[4]           = {"NO"};
  char   upper_account[21];

  int    i                   = 0;
  int    iflag               = 0;

  struct tm *now;
  time_t current;


  current    = time(NULL);
  now        = localtime(&current);
  strftime(default_start,11,"%Y-%m-%d",now);
  strcpy(default_end,default_start);

  account    = &default_account[0];
  jobname    = &default_jobname[0];
  userid     = &default_userid[0];
  maturity   = &default_maturity[0];
  start_date = &default_start[0];
  end_date   = &default_end[0];

  while (i < argc) {
    if (argv[i][0] != '-') {
      if (iflag == 1)
        account = argv[i];
      else if (iflag == 2)
        jobname = argv[i];
      else if (iflag == 3)
        user_listfile = argv[i];
      else if (iflag == 4)
        userid = argv[i];
      else if (iflag == 5)
        job_listfile = argv[i];
      else if (iflag == 6)
        maturity = argv[i];
      else if (iflag == 7)
        start_date = argv[i];
      else if (iflag == 8)
        end_date = argv[i];
      iflag = 0;
    }
    else {
      switch(toupper(argv[i][1])) {
         case 'A':
               iflag = 1;
               break;
         case 'J':
               iflag = 2;
               break;
         case 'L':
               iflag = 3;
               break;
         case 'U':
               iflag = 4;
               break;
         case 'N':
               iflag = 5;
               break;
         case 'M':
               iflag = 6;
               break;
         case 'S':
               iflag = 7;
               break;
         case 'E':
               iflag = 8;
               break;
         case 'B':
               strcpy(batch_only,"YES");
               break;
         case 'P':
               strcpy(procsum,"YES");
               break;
         case 'R':
               strcpy(rawfmt,"YES");
               break;
         case 'X':
               strcpy(excelfmt,"YES");
               break;
         default:
               printf("\n");
               printf("Invalid option \n");
               printf("\n");
               printf("Usage:  cpsacct [-a account] [-j jobname] [-u userid]");
               printf(" [-m maturity] [-s start_date] [-e end_date] [-b]");
               printf(" [-p|r|x]\n");
               printf("\n");
               printf("Usage:  cpsacct [-a account] [-l userlist]");
               printf(" [-m maturity] [-s start_date] [-e end_date] [-b]");
               printf(" [-p|r|x]\n");
               printf("\n");
               printf("Usage:  cpsacct [-a account] [-n joblist]");
               printf(" [-m maturity] [-s start_date] [-e end_date] [-b]");
               printf(" [-p|r|x]\n");
               printf("\n");
               printf("\n");
               exit(1);
      }
    }
  i++;
  }

  cpsacct_to_upper(upper_account,account);

  if(DBG(4)){
    printf("account=%s jobname=%s job_listfile=%s user_listfile=%s ",
          upper_account,jobname,   job_listfile,   user_listfile);
    printf("userid =%s maturity=%s start_date=%s end_date=%s ",
          userid      ,maturity,   start_date,   end_date);
    printf("batch_only =%s procsum=%s rawfmt=%s excelfmt=%s\n",
            batch_only,    procsum,   rawfmt,   excelfmt);
  }

  if(DBG(4)){ printf("------------------------Begin\n"); }

  cpsacct_sub(upper_account,jobname,job_listfile,user_listfile,userid,maturity,
              start_date,end_date,batch_only,procsum,rawfmt,excelfmt);

  if(DBG(4)){ printf("------------------------Finish\n"); }

  return 0;
}


void cpsacct_sub (char *account, char *jobname, char *job_listfile,
                  char *user_listfile, char *userid,
                  char *maturity, char *start_date, char *end_date,
                  char *batch_only, char *procsum, char *rawfmt, char *excelfmt)
{
  FILE             *ifp;

  char             *c;
  char             *list;
  char             *token;

  char             account_tmp[21];
  char             cnfg_str[21];
  char             cps_log_dir[81];
  char             filename[81];
  char             mat[2][5]                = {"prod","test"};
  char             pbs_server[MAX_CNFG_ENTRIES][11];
  char             str[1025];
  char             this_date[11];
  char             this_jid[21];
  char             this_jobname[21];
  char             this_key[91];
  char             this_message[161];
  char             this_node[11];
  char             this_pid[11];
  char             this_time[11];
  char             this_userid[11];
  char             dummy[160];
  char             mature_orig[7];
  char             mature[5];

  float            tmp;
  float            total_allocated          = 0.0;
  float            total_elapse             = 0.0;
  float            total_system_cpu         = 0.0;
  float            total_user_cpu           = 0.0;

  int              i,j,k;
  int              card_day;
  int              card_month;
  int              card_year;
  int              end_day;
  int              end_month;
  int              end_year;
  int              file_month;
  int              file_year;
  int              iaccount                 = 1;
  int              ifound                   = 0;
  int              ijob_listfile            = 0;
  int              ijobname                 = 1;
  int              imat;
  int              imat_end;
  int              imat_start;
  int              indx_process_system      = 0;
  int              indx_process_user        = 0;
  int              iprocess_name            = 0;
  int              iprocess_system          = 0;
  int              iprocess_user            = 0;
  int              isavecard                = 0;
  int              iuser_listfile           = 0;
  int              iuserid                  = 1;
  int              naborts                  = 0;
  int              nallprocesses            = 0;
  int              ncards                   = 0;
  int              ndeletes                 = 0;
  int              njoblist                 = 0;
  int              njobs                    = 0;
  int              not_pid;
  int              npbs_server              = 0;
  int              nrunning                 = 0;
  int              nuserlist                = 0;
  int              start_day;
  int              start_month;
  int              start_year;
  int              total_aborted_other      = 0;
  int              total_aborted_processing = 0;
  int              total_aborted_setup      = 0;
  int              total_deleted            = 0;
  int              total_finished           = 0;
  int              total_jobs               = 0;
  int              total_processes          = 0;
  int              total_running            = 0;
  int              total_unknown            = 0;

  struct tm        end_tm                   = {0};
  struct tm        file_tm                  = {0};
  struct tm        start_tm                 = {0};

  time_t           start_time;
  time_t           end_time;
  time_t           file_time;

  abortinfo        *aborts                  = NULL;
  allprocessesinfo *allprocesses            = NULL;
  cardinfo         *cards                   = NULL;
  deleteinfo       *deletes                 = NULL;
  jobinfo          *jobs                    = NULL;
  listinfo         *joblist                 = NULL;
  listinfo         *userlist                = NULL;
  runninginfo      *running                 = NULL;


  strcpy(cps_log_dir, cnfg_get_value_c("cps_log_dir"));
  if(DBG(4))printf("cps_log_dir=%s\n",cps_log_dir);
  for (i=0; i<MAX_CNFG_ENTRIES; i++) {
     sprintf(cnfg_str,"CPSACCT_PBS_SERVER%d",i+1);
     strcpy(pbs_server[i],cnfg_get_value_c(cnfg_str));
     if (strlen(pbs_server[i]) != 0) npbs_server++;
     if(DBG(4))printf("cnfg_str=%s pbs_server[i]=%s\n",cnfg_str,pbs_server[i]);
  }

  if (strcmp(account,"ALL") != 0) iaccount = 0;
  if (strcmp(jobname,"ALL") != 0) ijobname = 0;
  if (strcmp(userid ,"ALL") != 0) iuserid  = 0;
  
  i = sscanf(start_date,"%4d-%2d-%2d",&start_year,&start_month,&start_day);
  if (i != 3) {
    printf("Invalid start date.  Format is YYYY-MM-DD \n");
    exit(1);
  }
  i = sscanf(end_date  ,"%4d-%2d-%2d",&end_year  ,&end_month  ,&end_day  );
  if (i != 3) {
    printf("Invalid start date.  Format is YYYY-MM-DD \n");
    exit(1);
  }

  start_tm.tm_mday  = start_day;
  start_tm.tm_mon   = start_month - 1;
  start_tm.tm_year  = start_year - 1900;
  start_time        = mktime(&start_tm);

  end_tm.tm_mday    = end_day;
  end_tm.tm_mon     = end_month - 1;
  end_tm.tm_year    = end_year - 1900;
  end_time          = mktime(&end_tm);

  if (strcmp(maturity,"PROD") == 0) {
    imat_start = 0;
    imat_end   = 0;
  }
  else if (strcmp(maturity,"BETA") == 0) {
    imat_start = 1;
    imat_end   = 1;
  }
  else if (strcmp(maturity,"BOTH") == 0) {
    imat_start = 0;
    imat_end   = 1;
  }
  else {
    printf("Invalid maturity.  Choices are BETA, PROD, or BOTH \n");
    exit(1);
  }

  if (job_listfile != NULL) {
    ifp = fopen(job_listfile,"r");
    if(DBG(4))
      printf("%d: Opening file %s for readonly\n",__LINE__,job_listfile);   
    if (ifp != NULL) {
      ijob_listfile = 1;
      while (fgets(str,257,ifp) != NULL) {
        if (njoblist == 0)
          joblist = (listinfo *) malloc(sizeof(listinfo));
        else
          joblist = (listinfo *) realloc(joblist,
                                                (njoblist+1)*sizeof(listinfo));
        i = sscanf(str,"%s\n",joblist[njoblist].name);
        njoblist++;
      }
      fclose(ifp);
    }
  }

  if (user_listfile != NULL) {
    ifp = fopen(user_listfile,"r");
    if(DBG(4))
      printf("%d: Opening file %s for readonly\n",__LINE__,user_listfile); 
    if (ifp != NULL) {
      iuser_listfile = 1;
      while (fgets(str,257,ifp) != NULL) {
        if (nuserlist == 0)
          userlist = (listinfo *) malloc(sizeof(listinfo));
        else
          userlist = (listinfo *) realloc(userlist,
                                                (nuserlist+1)*sizeof(listinfo));
        i = sscanf(str,"%s\n",userlist[nuserlist].name);
        nuserlist++;
      }
      fclose(ifp);
    }
  }

    file_year         = start_year;
    file_month        = start_month;
    file_tm.tm_mday   = 1;
    file_tm.tm_mon    = file_month - 1;
    file_tm.tm_year   = file_year - 1900;
    file_time         = mktime(&file_tm);

    while (difftime(end_time,file_time) >= 0.0) {
      sprintf(filename,"%s/cps_acct_log",cps_log_dir);
      if(DBG(4))printf("%d: Opening file %s for readonly\n",__LINE__,filename);
      ifp = fopen(filename,"r"); 
      if (ifp == NULL) break;
      while (fgets(str,257,ifp) != NULL) {
        strcpy(this_pid    ," ");
        strcpy(this_jid    ," ");
        strcpy(this_jobname," ");
        strcpy(this_userid ," ");
        strcpy(this_date   ," ");
        strcpy(this_time   ," ");
        strcpy(this_message," ");
        i = sscanf(str,
        "%s %s %s %s %s %s %s %s %s %s %s %s %s %s %s %160[^\n]\n",
        dummy,dummy,dummy,dummy,dummy,dummy,dummy,mature_orig,dummy,
        this_pid, this_jid,this_jobname,this_userid,this_date,this_time,
        this_message);
  
        /* Strip out any non-printable characters */

        for (i=0; i<strlen(this_message); i++) {
          if (this_message[i] < 32 || this_message[i] > 123) this_message[i]=32;
        }
        /* set maturity to uppercase .. strip off [ ] chars .. */
        mature_orig[5]='\0';
        strcpy(mature, &mature_orig[1]);
        cpsacct_to_upper(mature,mature);
        /*printf("%d:mature=%s maturity=%s\n",__LINE__,mature,maturity);*/
        /* set maturity to BETA if TEST*/
        if(strcmp(mature,"TEST") == 0) strcpy(mature,"BETA");
        /*printf("%d:mature=%s maturity=%s\n",__LINE__,mature,maturity);*/
        if(DBG(4))printf(
       "mat=%s pid=%s jid=%s jobname=%s userid=%s date=%s time=%s message=%s\n",
        mature,this_pid,   this_jid,   this_jobname,
        this_userid,   this_date, this_time,   this_message);


        if(isavecard==1 && DBG(4))printf(
       "mat=%s pid=%s jid=%s jobname=%s userid=%s date=%s time=%s message=%s\n",
        mature,this_pid,   this_jid,   this_jobname,
        this_userid,   this_date, this_time,   this_message);

        /* Ignore records that do not have a valid pid */

        not_pid = 0;
        for (j=0; j<strlen(this_pid); j++) {
          if (!isdigit(this_pid[j])) {
            not_pid = 1;
            break;
          }
        }
        if (not_pid) continue;

        /* See if card matches the criteria to be saved */

        isavecard = 0;
        if (iuser_listfile == 1) {
          for (j=0; j<nuserlist; j++) {
            if (strcmp(userlist[j].name,this_userid) == 0) {
              isavecard = 1;
              break;
            }
          }
        }
        else if (ijob_listfile == 1) {
          for (j=0; j<njoblist; j++) {
            if (strcmp(joblist[j].name,this_jobname) == 0) {
              isavecard = 1;
              break;
            }
          }
        }
        else {
          if (ijobname) {
            if (iuserid) {
              isavecard = 1;
            }
            else if (strcmp(userid,this_userid) == 0) {
              isavecard = 1;
            }
          } else if (fnmatch((const char*) jobname,(const char*) this_jobname,
                                                             FNM_PERIOD) == 0) {
            if (iuserid) {
              isavecard = 1;
            }
            else if (strcmp(userid,this_userid) == 0) {
              isavecard = 1;
            }
          }
        }

        if (file_month == start_month) {
          i = sscanf(this_date,"%d-%d-%d",&card_year,&card_month,&card_day);
          if (card_day < start_day) isavecard = 0;
        }
        if (file_month == end_month) {
          i = sscanf(this_date,"%d-%d-%d",&card_year,&card_month,&card_day);
          if (card_day > end_day) isavecard = 0;
        }
        
        /* Only save the card if it is PROD or TEST as user requested */
        if (strcmp(maturity,"BOTH") != 0 && strcmp(maturity,mature)!=0 ) {
          isavecard = 0;
        }

        /* If to be saved, add card */

        if (isavecard) {
          if (ncards == 0)
            cards = (cardinfo *) malloc(sizeof(cardinfo));
          else
            cards = (cardinfo *) realloc(cards,(ncards+1)*sizeof(cardinfo));
          sprintf(cards[ncards].key,"%s%s%s%s%s%s%020d",this_pid,this_jid,
                  this_jobname,this_userid,this_date,this_time,ncards);
          strcpy(cards[ncards].pid,this_pid);
          strcpy(cards[ncards].jid,this_jid);
          strcpy(cards[ncards].jobname,this_jobname);
          strcpy(cards[ncards].userid,this_userid);
          strcpy(cards[ncards].date,this_date);
          strcpy(cards[ncards].time,this_time);
          strcpy(cards[ncards].message,this_message);
          ncards++;
        }

      }
      fclose(ifp);

      /* Increment month */

      file_tm.tm_mon = file_tm.tm_mon + 1;
      if (file_tm.tm_mon == 12) {
        file_tm.tm_mon  = 0;
        file_tm.tm_year = file_tm.tm_year + 1;
      }
      file_time  = mktime(&file_tm);
      file_year  = file_tm.tm_year+1900;
      file_month = file_tm.tm_mon + 1;
    }

  for (imat=imat_start; imat<imat_end+1; imat++) {
    file_year         = start_year;
    file_month        = start_month;
    file_tm.tm_mday   = 1;
    file_tm.tm_mon    = file_month - 1;
    file_tm.tm_year   = file_year - 1900;
    file_time         = mktime(&file_tm);

    while (difftime(end_time,file_time) >= 0.0) {
      sprintf(filename,"%s/cps_acct_%s_%02d_%02d",cps_log_dir,mat[imat],
                                                  file_year,file_month);
      if(DBG(4))
        printf("%d: Opening file %s for readonly\n",__LINE__,filename);   
      ifp = fopen(filename,"r"); 
      if (ifp == NULL) break;
      while (fgets(str,257,ifp) != NULL) {
        strcpy(this_pid    ," ");
        strcpy(this_jid    ," ");
        strcpy(this_jobname," ");
        strcpy(this_userid ," ");
        strcpy(this_date   ," ");
        strcpy(this_time   ," ");
        strcpy(this_message," ");
        i = sscanf(str,"%s %s %s %s %s %s %160[^\n]\n",this_pid,this_jid,
                   this_jobname,this_userid,this_date,this_time,this_message);

        if(DBG(4))printf("str={%s}\n",str);

        /* Strip out any non-printable characters */

        for (i=0; i<strlen(this_message); i++) {
          if (this_message[i] < 32 || this_message[i] > 123) this_message[i]=32;
        }

        /* Ignore records that do not have a valid pid */

        not_pid = 0;
        for (j=0; j<strlen(this_pid); j++) {
          if (!isdigit(this_pid[j])) {
            not_pid = 1;
            break;
          }
        }
        if (not_pid) continue;

        /* See if card matches the criteria to be saved */

        isavecard = 0;
        if (iuser_listfile == 1) {
          for (j=0; j<nuserlist; j++) {
            if (strcmp(userlist[j].name,this_userid) == 0) {
              isavecard = 1;
              break;
            }
          }
        }
        else if (ijob_listfile == 1) {
          for (j=0; j<njoblist; j++) {
            if (strcmp(joblist[j].name,this_jobname) == 0) {
              isavecard = 1;
              break;
            }
          }
        }
        else {
          if (ijobname) {
            if (iuserid) {
              isavecard = 1;
            }
            else if (strcmp(userid,this_userid) == 0) {
              isavecard = 1;
            }
          } else if (fnmatch((const char*) jobname,(const char*) this_jobname,
                                                             FNM_PERIOD) == 0) {
            if (iuserid) {
              isavecard = 1;
            }
            else if (strcmp(userid,this_userid) == 0) {
              isavecard = 1;
            }
          }
        }

        if (file_month == start_month) {
          i = sscanf(this_date,"%d-%d-%d",&card_year,&card_month,&card_day);
          if (card_day < start_day) isavecard = 0;
        }
        if (file_month == end_month) {
          i = sscanf(this_date,"%d-%d-%d",&card_year,&card_month,&card_day);
          if (card_day > end_day) isavecard = 0;
        }

        /* If to be saved, add card */

        if (isavecard) {
          if (ncards == 0)
            cards = (cardinfo *) malloc(sizeof(cardinfo));
          else
            cards = (cardinfo *) realloc(cards,(ncards+1)*sizeof(cardinfo));
          sprintf(cards[ncards].key,"%s%s%s%s%s%s%020d",this_pid,this_jid,
                  this_jobname,this_userid,this_date,this_time,ncards);
          strcpy(cards[ncards].pid,this_pid);
          strcpy(cards[ncards].jid,this_jid);
          strcpy(cards[ncards].jobname,this_jobname);
          strcpy(cards[ncards].userid,this_userid);
          strcpy(cards[ncards].date,this_date);
          strcpy(cards[ncards].time,this_time);
          strcpy(cards[ncards].message,this_message);
          ncards++;
        }

      }
      fclose(ifp);

      /* Increment month */

      file_tm.tm_mon = file_tm.tm_mon + 1;
      if (file_tm.tm_mon == 12) {
        file_tm.tm_mon  = 0;
        file_tm.tm_year = file_tm.tm_year + 1;
      }
      file_time  = mktime(&file_tm);
      file_year  = file_tm.tm_year+1900;
      file_month = file_tm.tm_mon + 1;
    }
  }

  file_year         = start_year;
  file_month        = start_month;
  file_tm.tm_mday   = 1;
  file_tm.tm_mon    = file_month - 1;
  file_tm.tm_year   = file_year - 1900;
  file_time         = mktime(&file_tm);

  while (difftime(end_time,file_time) >= 0.0) {
    sprintf(filename,"%s/cps_acct_log_20%02d_%02d",cps_log_dir,
                                                file_year,file_month);
    if(DBG(4))
      printf("%d: Opening file %s for readonly\n",__LINE__,filename);   
    ifp = fopen(filename,"r"); 
    if (ifp == NULL) break;
    while (fgets(str,257,ifp) != NULL) {
      strcpy(this_pid    ," ");
      strcpy(this_jid    ," ");
      strcpy(this_jobname," ");
      strcpy(this_userid ," ");
      strcpy(this_date   ," ");
      strcpy(this_time   ," ");
      strcpy(this_message," ");
      i = sscanf(str,"%s %s %s %s %s %s %160[^\n]\n",this_pid,this_jid,
                 this_jobname,this_userid,this_date,this_time,this_message);

      if(DBG(4))printf("str={%s}\n",str);

      /* Strip out any non-printable characters */

      for (i=0; i<strlen(this_message); i++) {
        if (this_message[i] < 32 || this_message[i] > 123) this_message[i]=32;
      }

      /* Ignore records that do not have a valid pid */

      not_pid = 0;
      for (j=0; j<strlen(this_pid); j++) {
        if (!isdigit(this_pid[j])) {
          not_pid = 1;
          break;
        }
      }
      if (not_pid) continue;

      /* See if card matches the criteria to be saved */

      isavecard = 0;
      if (iuser_listfile == 1) {
        for (j=0; j<nuserlist; j++) {
          if (strcmp(userlist[j].name,this_userid) == 0) {
            isavecard = 1;
            break;
          }
        }
      }
      else if (ijob_listfile == 1) {
        for (j=0; j<njoblist; j++) {
          if (strcmp(joblist[j].name,this_jobname) == 0) {
            isavecard = 1;
            break;
          }
        }
      }
      else {
        if (ijobname) {
          if (iuserid) {
            isavecard = 1;
          }
          else if (strcmp(userid,this_userid) == 0) {
            isavecard = 1;
          }
        } else if (fnmatch((const char*) jobname,(const char*) this_jobname,
                                                           FNM_PERIOD) == 0) {
          if (iuserid) {
            isavecard = 1;
          }
          else if (strcmp(userid,this_userid) == 0) {
            isavecard = 1;
          }
        }
      }

      if (file_month == start_month) {
        i = sscanf(this_date,"%d-%d-%d",&card_year,&card_month,&card_day);
        if (card_day < start_day) isavecard = 0;
      }
      if (file_month == end_month) {
        i = sscanf(this_date,"%d-%d-%d",&card_year,&card_month,&card_day);
        if (card_day > end_day) isavecard = 0;
      }

      /* If to be saved, add card */

      if (isavecard) {
        if (ncards == 0)
          cards = (cardinfo *) malloc(sizeof(cardinfo));
        else
          cards = (cardinfo *) realloc(cards,(ncards+1)*sizeof(cardinfo));
        sprintf(cards[ncards].key,"%s%s%s%s%s%s%020d",this_pid,this_jid,
                this_jobname,this_userid,this_date,this_time,ncards);
        strcpy(cards[ncards].pid,this_pid);
        strcpy(cards[ncards].jid,this_jid);
        strcpy(cards[ncards].jobname,this_jobname);
        strcpy(cards[ncards].userid,this_userid);
        strcpy(cards[ncards].date,this_date);
        strcpy(cards[ncards].time,this_time);
        strcpy(cards[ncards].message,this_message);
        ncards++;
      }

    }
    fclose(ifp);

    /* Increment month */

    file_tm.tm_mon = file_tm.tm_mon + 1;
    if (file_tm.tm_mon == 12) {
      file_tm.tm_mon  = 0;
      file_tm.tm_year = file_tm.tm_year + 1;
    }
    file_time  = mktime(&file_tm);
    file_year  = file_tm.tm_year+1900;
    file_month = file_tm.tm_mon + 1;
  }
  
  if (ncards == 0) {
    if(DBG(4)) {
      printf("exiting...\n");
    }
    exit(0);
  }

  qsort ((cardinfo *) cards,ncards,sizeof(cardinfo),cpsacct_sort_key_pid);

  /* Create jobs from sorted cards */

  strcpy(this_key," ");
  for (i=0; i<ncards; i++) {
    sprintf(cards[i].key,"%s%s%s%s",cards[i].pid,cards[i].jid,cards[i].jobname,
                                                              cards[i].userid);
    if (strcmp(this_key,cards[i].key) != 0) {
      if (strncmp(cards[i].message,"Job started",11) != 0) continue;
      if (njobs == 0)
        jobs = (jobinfo *) malloc(sizeof(jobinfo));
      else
        jobs = (jobinfo *) realloc(jobs,(njobs+1)*sizeof(jobinfo));
      njobs++;
      strcpy(jobs[njobs-1].jid,       " ");
      strcpy(jobs[njobs-1].jobname,   " ");
      strcpy(jobs[njobs-1].userid,    " ");
      strcpy(jobs[njobs-1].account,   " ");
      strcpy(jobs[njobs-1].node,      " ");
      strcpy(jobs[njobs-1].start_date," ");
      strcpy(jobs[njobs-1].start_time," ");
      strcpy(jobs[njobs-1].end_date,  " ");
      strcpy(jobs[njobs-1].end_time,  " ");
      jobs[njobs-1].aborted_setup      = 0;
      jobs[njobs-1].aborted_processing = 0;
      jobs[njobs-1].aborted_other      = 0;
      jobs[njobs-1].deleted            = 0;
      jobs[njobs-1].num_cpus           = 0;
      jobs[njobs-1].system_cpu         = 0.0;
      jobs[njobs-1].user_cpu           = 0.0;
      jobs[njobs-1].elapse             = 0.0;
      jobs[njobs-1].nprocesses         = 0;
      jobs[njobs-1].processes          = NULL;
      strcpy(this_key,cards[i].key);
      indx_process_user = 0;
      indx_process_system = 0;
    }

    if (strncmp(cards[i].message,"Job started",11) == 0) {
      strcpy(jobs[njobs-1].jobname,cards[i].jobname);
      strcpy(jobs[njobs-1].userid,cards[i].userid);
      jobs[njobs-1].num_cpus = 0;
      if (strchr(cards[i].jid,46) == NULL) {
        strcpy(jobs[njobs-1].node,cards[i].jid);
        strcpy(jobs[njobs-1].jid,"NONE");
      }
      else {
        strcpy(jobs[njobs-1].node,strchr(cards[i].jid,46)+1);
        j = strlen(cards[i].jid) - strlen(jobs[njobs-1].node) - 1;
        if (j > 0) {
          strncpy(jobs[njobs-1].jid,cards[i].jid,j);
          jobs[njobs-1].jid[j] = '\0';
        }
        else
          strcpy(jobs[njobs-1].jid,"NONE");
      }
      strcpy(jobs[njobs-1].start_date,cards[i].date);
      strcpy(jobs[njobs-1].start_time,cards[i].time);
      strcpy(jobs[njobs-1].end_date,"NA");
      strcpy(jobs[njobs-1].end_time,"NA");
      jobs[njobs-1].system_cpu = 0.0;
      jobs[njobs-1].user_cpu = 0.0;
      jobs[njobs-1].elapse = 0.0;
      jobs[njobs-1].nprocesses = 0;
      jobs[njobs-1].processes = NULL;
      iprocess_name = 0;
      iprocess_user = 0;
      iprocess_system = 0;
      continue;
    }

    if (strncmp(cards[i].message,"PROCESS_LIST = (",16) == 0) {
      if (strchr(cards[i].message,')') == NULL) iprocess_name = 1;
      if (strcmp(procsum,"YES") == 0) {
        list = &cards[i].message[16];
        token = strtok(list," ,)");
        while (token != NULL) {
          if (jobs[njobs-1].nprocesses == 0)
            jobs[njobs-1].processes = 
                                    (processinfo *) malloc(sizeof(processinfo));
          else
          jobs[njobs-1].processes = 
                              (processinfo *) realloc(jobs[njobs-1].processes,
                              (jobs[njobs-1].nprocesses+1)*sizeof(processinfo));
          strcpy(jobs[njobs-1].processes[jobs[njobs-1].nprocesses].name,
                                                                       token);
          jobs[njobs-1].processes[jobs[njobs-1].nprocesses].user_cpu = 0.0;
          jobs[njobs-1].processes[jobs[njobs-1].nprocesses].system_cpu = 0.0;
          jobs[njobs-1].nprocesses++;
          token = strtok(NULL," ,)");
        }
      }
      continue;
    }

    if (strncmp(cards[i].message,"PROCESS_USER_TIME = (",21) == 0) {
      if (strchr(cards[i].message,')') == NULL) iprocess_user = 1;
      if (strcmp(procsum,"YES") == 0) {
        list = &cards[i].message[21];
        token = strtok(list," ,)");
        while (token != NULL) {
          if (jobs[njobs-1].nprocesses != 0 && 
                                 indx_process_user < jobs[njobs-1].nprocesses) {
            jobs[njobs-1].processes[indx_process_user].user_cpu = atof(token);
            indx_process_user++;
          }
          token = strtok(NULL," ,)");
        }
      }
      iprocess_name = 0;
      iprocess_system = 0;
      continue;
    }

    if (strncmp(cards[i].message,"PROCESS_SYSTEM_TIME = (",23) == 0) {
      if (strchr(cards[i].message,')') == NULL) iprocess_system = 1;
      if (strcmp(procsum,"YES") == 0) {
        list = &cards[i].message[23];
        token = strtok(list," ,)");
        while (token != NULL) {
          if (jobs[njobs-1].nprocesses != 0 && 
                              indx_process_system < jobs[njobs-1].nprocesses) {
            jobs[njobs-1].processes[indx_process_system].system_cpu=atof(token);
            indx_process_system++;
          }
          token = strtok(NULL," ,)");
        }
      }
      iprocess_name = 0;
      iprocess_system = 0;
      continue;
    }

    if (strncmp(cards[i].message,"Job Abort - Setup errors",24) == 0) {
      jobs[njobs-1].aborted_setup = 1;
      iprocess_name = 0;
      iprocess_user = 0;
      iprocess_system = 0;
      continue;
    }

    if (strncmp(cards[i].message,"Job Abort - Processing errors",29) == 0) {
      jobs[njobs-1].aborted_processing = 1;
      iprocess_name = 0;
      iprocess_user = 0;
      iprocess_system = 0;
      continue;
    }

    if (strncmp(cards[i].message,"Job finished",12) == 0) {
      strcpy(jobs[njobs-1].end_date,cards[i].date);
      strcpy(jobs[njobs-1].end_time,cards[i].time);
      iprocess_name = 0;
      iprocess_user = 0;
      iprocess_system = 0;
      continue;
    }

    if (strncmp(cards[i].message,"USER_TIME",9) == 0) {
      sscanf(cards[i].message,
            "USER_TIME = %f, SYSTEM_TIME = %f, ELAPSE_TIME = %f, NUM_CPUS = %d",
            &jobs[njobs-1].user_cpu,&jobs[njobs-1].system_cpu,
            &jobs[njobs-1].elapse,&jobs[njobs-1].num_cpus);
      iprocess_name = 0;
      iprocess_user = 0;
      iprocess_system = 0;
      continue;
    }
    if (strncmp(cards[i].message,"ACCOUNT",7) == 0) {
      sscanf(cards[i].message,"ACCOUNT = %s NUM_CPUS = %d",account_tmp,
                                                       &jobs[njobs-1].num_cpus);
      j = strlen(account_tmp);
      if (j > 0 && account_tmp[j-1] == ',') account_tmp[j-1] = '\0';
      cpsacct_to_upper(jobs[njobs-1].account,account_tmp);
    }

    if (iprocess_name == 1) {
      if (strchr(cards[i].message,')') != NULL) iprocess_name = 0;
      if (strcmp(procsum,"YES") == 0) {
        token = strtok(cards[i].message," ,)");
        while (token != NULL) {
          if (jobs[njobs-1].nprocesses == 0)
            jobs[njobs-1].processes = 
                                    (processinfo *) malloc(sizeof(processinfo));
          else
          jobs[njobs-1].processes = 
                              (processinfo *) realloc(jobs[njobs-1].processes,
                              (jobs[njobs-1].nprocesses+1)*sizeof(processinfo));
          strcpy(jobs[njobs-1].processes[jobs[njobs-1].nprocesses].name,
                                                                       token);
          jobs[njobs-1].processes[jobs[njobs-1].nprocesses].user_cpu = 0.0;
          jobs[njobs-1].processes[jobs[njobs-1].nprocesses].system_cpu = 0.0;
          jobs[njobs-1].nprocesses++;
          token = strtok(NULL," ,)");
        }
      }
      continue;
    }

    if (iprocess_user == 1) {
      if (strchr(cards[i].message,')') != NULL) iprocess_user = 0;
      if (strcmp(procsum,"YES") == 0) {
        token = strtok(cards[i].message," ,)");
        while (token != NULL) {
          if (jobs[njobs-1].nprocesses != 0 && 
                                 indx_process_user < jobs[njobs-1].nprocesses) {
            jobs[njobs-1].processes[indx_process_user].user_cpu = atof(token);
            indx_process_user++;
          }
          token = strtok(NULL," ,)");
        }
      }
      continue;
    }

    if (iprocess_system == 1) {
      if (strchr(cards[i].message,')') != NULL) iprocess_system = 0;
      if (strcmp(procsum,"YES") == 0) {
        token = strtok(cards[i].message," ,)");
        while (token != NULL) {
          if (jobs[njobs-1].nprocesses != 0 && 
                              indx_process_system < jobs[njobs-1].nprocesses) {
            jobs[njobs-1].processes[indx_process_system].system_cpu=atof(token);
            indx_process_system++;
          }
          token = strtok(NULL," ,)");
        }
      }
      continue;
    }

  }
  free(cards);
  if (njobs == 0) exit(0);

  /* Merge in aborts */
  file_year         = start_year;
  file_month        = start_month;
  file_tm.tm_mday   = 1;
  file_tm.tm_mon    = file_month - 1;
  file_tm.tm_year   = file_year - 1900;
  file_time         = mktime(&file_tm);

  while (difftime(end_time,file_time) >= 0.0) {
    sprintf(filename,"%s/cps_aborted_%02d_%02d",cps_log_dir,file_year,
                                                             file_month);
    if(DBG(4))printf("%d: Opening file %s for readonly\n",__LINE__,filename);   
    ifp = fopen(filename,"r");
    if (ifp == NULL) break;
    while (fgets(str,257,ifp) != NULL) {
      strcpy(this_jid    ," ");
      strcpy(this_userid ," ");
      strcpy(this_jobname," ");
      i = sscanf(str,"%s %s %s",this_jid,this_userid,this_jobname);
      if (naborts == 0)
        aborts = (abortinfo *) malloc(sizeof(abortinfo));
      else
        aborts = (abortinfo *) realloc(aborts,(naborts+1)*sizeof(abortinfo));
      strcpy(aborts[naborts].jid,this_jid);
      strcpy(aborts[naborts].userid,this_userid);
      strcpy(aborts[naborts].jobname,this_jobname);
      naborts++;
    }
    fclose(ifp);

    /* Increment month */

    file_tm.tm_mon = file_tm.tm_mon + 1;
    if (file_tm.tm_mon == 12) {
      file_tm.tm_mon  = 0;
      file_tm.tm_year = file_tm.tm_year + 1;
    }
    file_time  = mktime(&file_tm);
    file_year  = file_tm.tm_year+1900;
    file_month = file_tm.tm_mon + 1;
  }

  if (naborts > 0) {
    for (i=0; i<njobs; i++) {
      for (j=0; j<naborts; j++) {
        if (strcmp(jobs[i].jid,aborts[j].jid) == 0 && 
            strcmp(jobs[i].jobname,aborts[j].jobname) == 0) {
          if (jobs[i].aborted_setup == 0 && jobs[i].aborted_processing == 0)
            jobs[i].aborted_other = 1;
          break;
        }
      }
    }
    free(aborts);
  }

  /* Merge in deleted jobs */
  file_year         = start_year;
  file_month        = start_month;
  file_tm.tm_mday   = 1;
  file_tm.tm_mon    = file_month - 1;
  file_tm.tm_year   = file_year - 1900;
  file_time         = mktime(&file_tm);

  while (difftime(end_time,file_time) >= 0.0) {
    sprintf(filename,"%s/cps_deleted_%02d_%02d",cps_log_dir,file_year,
                                                             file_month);
    if(DBG(4))printf("%d: Opening file %s for readonly\n",__LINE__,filename);   
    ifp = fopen(filename,"r");
    if (ifp == NULL) break;
    while (fgets(str,257,ifp) != NULL) {
      strcpy(this_jid  ," ");
      strcpy(this_node ," ");
      strcpy(this_date ," ");
      strcpy(this_time ," ");
      i = 0;
      token = strtok(str,";");
      while (token !=NULL) {
        i++;
        if (i == 1) {
          j = sscanf(token,"%2d/%2d/%4d %s",&card_month,&card_day,&card_year,
                                            this_time);
          sprintf(this_date,"%04d-%02d-%02d",card_year,card_month,card_day);
        }
        else if (i == 5) {
          strcpy(this_jid,token);
          break;
        }
        token = strtok(NULL,";");
      }
      if (ndeletes == 0)
        deletes = (deleteinfo *) malloc(sizeof(deleteinfo));
      else
        deletes = (deleteinfo *) realloc(deletes,(ndeletes+1)*
                                                  sizeof(deleteinfo));
      strcpy(deletes[ndeletes].jid,this_jid);
      c = strchr(this_jid,46);
      if (c != NULL) {
        j = strlen(this_jid) - strlen(c);
        if (j > 0) deletes[ndeletes].jid[j] = '\0';
        strcpy(this_node,strchr(this_jid,46)+1);
      }
      strcpy(deletes[ndeletes].node,this_node);
      strcpy(deletes[ndeletes].date,this_date);
      strcpy(deletes[ndeletes].time,this_time);
      ndeletes++;
    }
    fclose(ifp);
    /* Increment month */

    file_tm.tm_mon = file_tm.tm_mon + 1;
    if (file_tm.tm_mon == 12) {
      file_tm.tm_mon  = 0;
      file_tm.tm_year = file_tm.tm_year + 1;
    }
    file_time  = mktime(&file_tm);
    file_year  = file_tm.tm_year+1900;
    file_month = file_tm.tm_mon + 1;
  }


  if (ndeletes > 0) {
    for (i=0; i<njobs; i++) {
      for (j=0; j<ndeletes; j++) {
        if (strcmp(jobs[i].jid,deletes[j].jid) == 0 &&
            strcmp(jobs[i].node,deletes[j].node) == 0 &&
            jobs[i].user_cpu == 0.0) {
          jobs[i].deleted = 1;
          if (strcmp(jobs[i].end_date,"NA") == 0) {
            strcpy(jobs[i].end_date,deletes[j].date);
            strcpy(jobs[i].end_time,deletes[j].time);
          }
          break;
        }
      }
    }
    free(deletes);
  }


  /* Merge in running */
  file_month = end_month + 1;
  if (file_month == 13) {
    file_month = 1;
    file_year  = end_year + 1;
  }
  else {
    file_year = end_year;
  }

  for (j=0; j<npbs_server; j++) {
    sprintf(filename,"%s/cps_running_%02d_%02d.%s",cps_log_dir,file_year,
                                                   file_month,pbs_server[j]);
    if(DBG(4))printf("%d: Opening file %s for readonly\n",__LINE__,filename);
    ifp = fopen(filename,"r");
    if (ifp == NULL) break;
    while (fgets(str,257,ifp) != NULL) {
      strcpy(this_jid    ," ");
      strcpy(this_userid ," ");
      i = sscanf(str,"%s %s",this_jid,this_userid);
      if (i == 2 && strchr(this_jid,46) != NULL) {
        if (nrunning == 0)
          running = (runninginfo *) malloc(sizeof(runninginfo));
        else
          running = 
              (runninginfo *) realloc(running,(nrunning+1)*sizeof(runninginfo));
        strcpy(running[nrunning].node,strchr(this_jid,46)+1);
        k = strlen(this_jid) - strlen(running[nrunning].node) - 1;
        if (k > 0) {
          strncpy(running[nrunning].jid,this_jid,k);
          running[nrunning].jid[k] = '\0';
        }
        else {
          strcpy(running[nrunning].jid," ");
        }
        strcpy(running[nrunning].userid,this_userid);
        nrunning++;
      }
    }
    fclose(ifp);
  }

  if (nrunning > 0) {
    for (i=0; i<njobs; i++) {
      for (j=0; j<nrunning; j++) {
        if (strcmp(jobs[i].jid,running[j].jid) == 0 &&
            strcmp(jobs[i].node,running[j].node) == 0 &&
            strcmp(jobs[i].userid,running[j].userid) == 0 &&
            jobs[i].user_cpu == 0.0) {
          jobs[i].running = 1;
          break;
        }
      }
    }
    free(running);
  }



  /* Generate output */

  if (strcmp(procsum,"YES") == 0) {

    /* Generate a process list report */

    for (i=0; i<njobs; i++) {
      if (iaccount) {
        isavecard = 1;
      }
      else {
        if (strcmp(account,jobs[i].account) == 0)
          isavecard = 1;
        else
          isavecard = 0;
      }
      if (isavecard && (strcmp(batch_only,"YES") == 0)) {
        if (strcmp(jobs[i].jid,"NONE") == 0) isavecard = 0;
      }
      if (isavecard) {
        if (jobs[i].nprocesses < 1000) {
          total_processes = total_processes + jobs[i].nprocesses;
          for (j=0; j<jobs[i].nprocesses; j++) {
            if (nallprocesses == 0) {
              allprocesses = 
                          (allprocessesinfo *) malloc(sizeof(allprocessesinfo));
              strcpy(allprocesses[nallprocesses].name,
                                                     jobs[i].processes[j].name);
              allprocesses[nallprocesses].count = 1;
              allprocesses[nallprocesses].user_cpu = 
                                                  jobs[i].processes[j].user_cpu;
              allprocesses[nallprocesses].system_cpu = 
                                                jobs[i].processes[j].system_cpu;
              nallprocesses++;
            }
            else {
              ifound = 0;
              for (k=0; k<nallprocesses; k++) {
                if (strcmp(allprocesses[k].name,jobs[i].processes[j].name)==0) {
                  allprocesses[k].count++;
                  allprocesses[k].user_cpu = allprocesses[k].user_cpu + 
                                                  jobs[i].processes[j].user_cpu;
                  allprocesses[k].system_cpu = allprocesses[k].system_cpu + 
                                                jobs[i].processes[j].system_cpu;
                  ifound = 1;
                  break;
                }
              }
              if (!ifound) {
                allprocesses = (allprocessesinfo *) realloc(allprocesses,
                                    (nallprocesses+1)*sizeof(allprocessesinfo));
                strcpy(allprocesses[nallprocesses].name,
                                                     jobs[i].processes[j].name);
                allprocesses[nallprocesses].count = 1;
                allprocesses[nallprocesses].user_cpu = 
                                                  jobs[i].processes[j].user_cpu;
                allprocesses[nallprocesses].system_cpu = 
                                                jobs[i].processes[j].system_cpu;
                nallprocesses++;
              }
            }
          }
        }
        total_jobs++;
      }
    }
    printf("Number of jobs  = %d\n",total_jobs);
    printf("Total processes = %d\n",total_processes);
    printf("\n");
    printf("Process name            Used         Used       ");
    printf("Total User CPU Total System CPU\n");
    printf("                      (Times)    (Percentage)   ");
    printf("   (Hours)         (Hours)\n");
    printf("-------------------- ---------- --------------- ");
    printf("-------------- ----------------\n");

    qsort ((allprocessesinfo *) allprocesses,nallprocesses,
           sizeof(allprocessesinfo),cpsacct_sort_key_num);

    for (i=0; i<nallprocesses; i++) {
      tmp = allprocesses[i].count;
      tmp = tmp / total_processes * 100.0;
      if (strcmp(allprocesses[i].name,"JOB_DATA") == 0) {
        allprocesses[i].user_cpu = 0.0;
        allprocesses[i].system_cpu = 0.0;
      }
      else if (strcmp(allprocesses[i].name,"PROJECT_DATA") == 0) {
        allprocesses[i].user_cpu = 0.0;
        allprocesses[i].system_cpu = 0.0;
      }
      printf("%-20s %10d %14.6f %14.6f %14.6f\n",
                                             allprocesses[i].name,
                                             allprocesses[i].count,
                                             tmp,
                                             allprocesses[i].user_cpu/3600.0,
                                             allprocesses[i].system_cpu/3600.0);
    }
    free(allprocesses);
  }

  else if (strcmp(rawfmt,"YES") == 0) {

    /* Generate a raw report */

    qsort ((jobinfo *) jobs,njobs,sizeof(jobinfo),cpsacct_sort_key_jid);
    for (i=0; i<njobs; i++) {
      if (iaccount) {
        isavecard = 1;
      }
      else {
        if (strcmp(account,jobs[i].account) == 0)
          isavecard = 1;
        else
          isavecard = 0;
      }
      if (isavecard && (strcmp(batch_only,"YES") == 0)) {
        if (strcmp(jobs[i].jid,"NONE") == 0) isavecard = 0;
      }
      if (isavecard) {
        if (jobs[i].aborted_setup == 1 ||
            jobs[i].aborted_processing == 1 ||
            jobs[i].aborted_other == 1)
          printf("%s %s %s %d %s %s %s %f %f %f Aborted\n",jobs[i].jid,
                 jobs[i].jobname,jobs[i].userid,jobs[i].num_cpus,jobs[i].node,
                 jobs[i].start_date,jobs[i].end_date,jobs[i].user_cpu,
                 jobs[i].system_cpu,jobs[i].elapse);

        else if (jobs[i].deleted == 1)
          printf("%s %s %s %d %s %s %s %f %f %f Deleted\n",jobs[i].jid,
                 jobs[i].jobname,jobs[i].userid,jobs[i].num_cpus,jobs[i].node,
                 jobs[i].start_date,jobs[i].end_date,jobs[i].user_cpu,
                 jobs[i].system_cpu,jobs[i].elapse);

        else if (strcmp(jobs[i].end_date,"NA") != 0)
          printf("%s %s %s %d %s %s %s %f %f %f Completed\n",jobs[i].jid,
                 jobs[i].jobname,jobs[i].userid,jobs[i].num_cpus,jobs[i].node,
                 jobs[i].start_date,jobs[i].end_date,jobs[i].user_cpu,
                 jobs[i].system_cpu,jobs[i].elapse);
        else
          printf("%s %s %s %d %s %s %s %f %f %f UNKNOWN\n",jobs[i].jid,
                 jobs[i].jobname,jobs[i].userid,jobs[i].num_cpus,jobs[i].node,
                 jobs[i].start_date,jobs[i].end_date,jobs[i].user_cpu,
                 jobs[i].system_cpu,jobs[i].elapse);
      }
    }
  }

  else if (strcmp(excelfmt,"YES") == 0) {

    /* Generate a excel report */
    qsort ((jobinfo *) jobs,njobs,sizeof(jobinfo),cpsacct_sort_key_jid);

    printf("Job\tStart\tEnd\tStatus\tNum CPU\tElapse (hours)\t");
    printf("User CPU (hours)\tSystem CPU (hours)\n");

    for (i=0; i<njobs; i++) {
      if (iaccount) {
        isavecard = 1;
      }
      else {
        if (strcmp(account,jobs[i].account) == 0)
          isavecard = 1;
        else
          isavecard = 0;
      }
      if (isavecard && (strcmp(batch_only,"YES") == 0)) {
        if (strcmp(jobs[i].jid,"NONE") == 0) isavecard = 0;
      }
      if (isavecard) {
        if (jobs[i].aborted_setup == 1 ||
            jobs[i].aborted_processing == 1 ||
            jobs[i].aborted_other == 1)
          strcpy(jobs[i].status,"Aborted");
        else if (jobs[i].deleted == 1)
          strcpy(jobs[i].status,"Deleted");
        else if (strcmp(jobs[i].end_date,"NA") != 0)
          strcpy(jobs[i].status,"Completed");
        else
          strcpy(jobs[i].status,"Unknown");
        jobs[i].start_date[4] = '/';
        jobs[i].start_date[7] = '/';
        jobs[i].start_time[2] = ':';
        jobs[i].start_time[5] = ':';
        if (strcmp(jobs[i].end_date,"NA") != 0) {
          jobs[i].end_date[4] = '/';
          jobs[i].end_date[7] = '/';
          jobs[i].end_time[2] = ':';
          jobs[i].end_time[5] = ':';
          printf("%s\t%s %s\t%s %s\t%s\t%d\t%f\t%f\t%f\n",
                 jobs[i].jobname,
                 jobs[i].start_date,jobs[i].start_time,
                 jobs[i].end_date,jobs[i].end_time,
                 jobs[i].status,jobs[i].num_cpus,
                 jobs[i].elapse/3600.0,
                 jobs[i].user_cpu/3600.0,jobs[i].system_cpu/3600.0);
        }
        else {
          printf("%s\t%s %s\t\t%d\t%s\t%f\t%f\t%f\n",
                 jobs[i].jobname,
                 jobs[i].start_date,jobs[i].start_time,
                 jobs[i].num_cpus,jobs[i].status,
                 jobs[i].elapse/3600.0,
                 jobs[i].user_cpu/3600.0,jobs[i].system_cpu/3600.0);
        }
      }
    }
  }
  else {

    /* Generate a standard report */

    qsort ((jobinfo *) jobs,njobs,sizeof(jobinfo),cpsacct_sort_key_jid);
    printf("Job------------ Userid---- JID------------------ ");
    printf("Account------------- Start----- End------- User CPU---- ");
    printf("System CPU-- Elapse------ #CPUs Status\n");
    for (i=0; i<njobs; i++) {
      if (iaccount) {
        isavecard = 1;
      }
      else {
        if (strcmp(account,jobs[i].account) == 0)
          isavecard = 1;
        else
          isavecard = 0;
      }
      if (isavecard && (strcmp(batch_only,"YES") == 0)) {
        if (strcmp(jobs[i].jid,"NONE") == 0) isavecard = 0;
      }
      if (isavecard) {
        if (jobs[i].deleted == 1) {
          strcpy(jobs[i].status,"Deleted");
          total_deleted++;
        }
        else if (jobs[i].aborted_setup == 1) {
          strcpy(jobs[i].status,"Aborted");
          total_aborted_setup++;
        }
        else if (jobs[i].aborted_processing == 1) {
          strcpy(jobs[i].status,"Aborted");
          total_aborted_processing++;
        }
        else if (jobs[i].aborted_other == 1) {
          strcpy(jobs[i].status,"Aborted");
          total_aborted_other++;
        }
        else if (jobs[i].running == 1) {
          strcpy(jobs[i].status,"Running");
          total_running++;
        }
        else if (strcmp(jobs[i].end_date,"NA") == 0) {
          strcpy(jobs[i].status,"Unknown");
          total_unknown++;
        }
        else {
          strcpy(jobs[i].status,"Completed");
          total_finished++;
        }
        printf(
       "%-15s %-10s %10s.%-10s %-20s %-10s %-10s %12.2f %12.2f %12.2f %5d %s\n",
               jobs[i].jobname,jobs[i].userid,jobs[i].jid,jobs[i].node,
               jobs[i].account,jobs[i].start_date,jobs[i].end_date,
               jobs[i].user_cpu,jobs[i].system_cpu,jobs[i].elapse,
               jobs[i].num_cpus,jobs[i].status);
        total_jobs++;
        total_user_cpu   = total_user_cpu   + jobs[i].user_cpu;
        total_system_cpu = total_system_cpu + jobs[i].system_cpu;
        total_elapse     = total_elapse     + jobs[i].elapse;
        if (jobs[i].num_cpus > 1)
          total_allocated  = total_allocated  + jobs[i].num_cpus*jobs[i].elapse;
        else
          total_allocated  = total_allocated  + jobs[i].user_cpu +
                                                jobs[i].system_cpu;
      }
    }
    printf("\n\n");
    printf("Number of jobs   = %10d\n",total_jobs);
    printf("Total User   CPU = %13.2f seconds  %10.2f hrs %8.2f  days\n",
                               total_user_cpu,
                               total_user_cpu/3600.0,
                               total_user_cpu/86400.0);
    printf("Total System CPU = %13.2f seconds  %10.2f hrs %8.2f  days\n",
                               total_system_cpu,
                               total_system_cpu/3600.0,
                               total_system_cpu/86400.0);
    printf("Total CPU        = %13.2f seconds  %10.2f hrs %8.2f  days\n",
                               (total_user_cpu+total_system_cpu),
                               (total_user_cpu+total_system_cpu)/3600.0,
                               (total_user_cpu+total_system_cpu)/86400.0);
    printf("Total Elapse     = %13.2f seconds  %10.2f hrs %8.2f  days\n",
                               total_elapse,
                               total_elapse/3600.0,
                               total_elapse/86400.0);
    printf("Total Allocated  = %13.2f seconds  %10.2f hrs %8.2f  days\n",
                               total_allocated,
                               total_allocated/3600.0,
                               total_allocated/86400.0);
    printf("Total Completed  = %10d\n",total_finished);
    printf("Total Running    = %10d\n",total_running);
    printf("Total Aborted    = %10d  (%7.3f percent)\n",
                               total_aborted_setup +
                               total_aborted_processing +
                               total_aborted_other +
                               total_unknown,
                               100.0*(total_aborted_setup +
                               total_aborted_processing +
                               total_aborted_other +
                               total_unknown)/total_jobs);
    printf("                             Setup        = %10d\n",
                               total_aborted_setup);
    printf("                             Processing   = %10d\n",
                               total_aborted_processing);
    printf("                             Signal       = %10d\n",
                               total_aborted_other);
    printf("                             Unknown      = %10d\n",
                               total_unknown);
    printf("Total Deleted    = %10d  (%7.3f percent)\n",
                               total_deleted,
                               100.0*total_deleted/total_jobs);
    printf("Abort/Delete     = %10d  (%7.3f percent)\n",
                               total_aborted_setup +
                               total_aborted_processing +
                               total_aborted_other +
                               total_deleted +
                               total_unknown,
                               100.0*(total_aborted_setup +
                               total_aborted_processing +
                               total_aborted_other +
                               total_deleted +
                               total_unknown)/total_jobs);
  }
}


void cpsacct_to_upper(char *newstring, char *oldstring)
{
  int i = 0;
  while(oldstring[i])
       {
       newstring[i] = toupper(oldstring[i]);
       i++;
       }
  newstring[i] = '\0';
}


static int cpsacct_sort_key_pid (const void *i, const void *j)
{
  cardinfo *i2 = (cardinfo *) i;
  cardinfo *j2 = (cardinfo *) j;

  return strcmp(i2->key,j2->key);
}


static int cpsacct_sort_key_jid (const void *i, const void *j)
{
  char ikey[31];
  char jkey[31];

  jobinfo *i2 = (jobinfo *) i;
  jobinfo *j2 = (jobinfo *) j;

  sprintf(ikey,"%s%20s",i2->start_date,i2->jid);
  sprintf(jkey,"%s%20s",j2->start_date,j2->jid);
/*  sprintf(ikey,"%s%020s",i2->start_date,i2->jid);*/
/*  sprintf(jkey,"%s%020s",j2->start_date,j2->jid);*/

  return strcmp(ikey,jkey);
}


static int cpsacct_sort_key_num (const void *i, const void *j)
{
  allprocessesinfo *i2 = (allprocessesinfo *) i;
  allprocessesinfo *j2 = (allprocessesinfo *) j;

  if (i2->count < j2->count)
    return 1;
  else if (i2->count > j2->count)
    return -1;
  else
    return 0;
}
