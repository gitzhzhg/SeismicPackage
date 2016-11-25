/*<CPS_v1 type="PROGRAM"/>
!------------------------------ refreshlock.c --------------------------------!!
!------------------------------ refreshlock.c --------------------------------!!
!------------------------------ refreshlock.c --------------------------------!!
!
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
!
!<brief_doc>
!-------------------------------------------------------------------------------
!                       C P S   P R O G R A M
!
! Name       : refreshlock
! Category   : stand-alone
! Written    : 2003-05-23   by: Bill Menger/Charles C Burch
! Revised    : 2007-10-23   by: Bill Menger
! Maturity   : production
! Purpose    : Checks for and refreshes stale locks on any CPS lock file.
! Portability: No known limitations.
!
!-------------------------------------------------------------------------------
!</brief_doc>
!
!<descript_doc>
!-------------------------------------------------------------------------------
!                          GENERAL DESCRIPTION
! Occasionally, CPS lockfiles may be left in a locked state because of an 
! abnormal condition on a cluster node.  If this occurs, all I/O using the
! lock file will hang up, waiting for the lock to clear.
!
! This daemon checks a specified lock file (using the -f lockfile_path argument)
! at a given periodicity (specified using the -t time_period (seconds )) to see
! if the file has been left locked by the same process on the same node for the
! specified period.  If this is the case, then the file is effectively unlocked
! by renaming it and recreating a fresh lock file of the specified file name.
!
! If the lockfile does not exist, this program will create it.
!
! *** NOTE *** Unless the -c flag is used, this daemon does NOT copy the 
!              contents of the lockfile into the New file, but creates a new 
!              blank lock file.  
!
!-------------------------------------------------------------------------------
!</descript_doc>
!
!<advice_doc>
!-------------------------------------------------------------------------------
!                          ADVICE FOR USERS
!
! This must be run as the sps user in order to create a lockfile as owner sps.
!
! There are five arguments: -f, -m -c -l and -t (in any order) which are 
! followed by whitespace.  
!   -t "waitime" specifies the wait-time in seconds between checking the lock 
!     file.  Default is 120 seconds.
!   -f filename specifies the lock filename to check. 
!      default is "/usr/app/cps_log/cps_lockfile.dat".
!   -m "file_mod_count" indicates to use the file modification time in the 
!      checking for problem. Default is off. If>0, the file modification time
!      is allowed to change "file_mod_count" times before declaring the lock
!      is hung. 
!   -c has no associated arguments-it indicates to copy the contents of the
!      old file to the new file. Default is not copy but create a blank file.
!   -l has no associated arguments-it indicates to print contents of the 
!      test variables with each test.  This is mainly for testing.
!      Default if off. 
!
! All other arguments are printed out on stderr and ignored.  
!
! It is suggested that one uses -t 30 -m 3 -c
!
! The daemon should be included in a system startup script that will be run
! by the system's "init" process.
!
! Multiple instances of this daemon may be run for different lock files.
!
! Note: for multiple-lock_files use :nn after the lockfile name where
! nn is the number of multiple lock files.
!
! It is best to run this on the NFS SERVER machine of the lockfile_path.
!
!-------------------------------------------------------------------------------
!</advice_doc>
!
!<history_doc>
!-------------------------------------------------------------------------------
!                             REVISION HISTORY
!
!     Date        Author      Description
!     ----        ------      -----------
!  2. 2007-10-23  Bill Menger Created header file, cleaned up for newer C 
!                             compilers.
!  1. 2003-05-23  C C Burch   Initial version for multiple lockfiles.
!
!  This program replaces refresh_lock_daemon whose history is below:
!  4. 2002-04-18  C C Burch   Fix name of rename file when lock is refreshed
!  3. 2002-03-07  C C Burch   Change lock error detection to eliminated some 
!                             false refreshes. Added time stamp on printouts.
!                             Added options to use file time mod/access time
!                             as part of refresh test, to allow old file 
!                             to be copied, and to allow debug log messages
!  2. 2001-02-09  Bill Menger Added chmod function to creat.
!  1. 2000-12-08  Bill Menger Initial version.
!
!-------------------------------------------------------------------------------
!</history_doc>
!
!<portability_doc>
!-------------------------------------------------------------------------------
!                         PORTABILITY LIMITATIONS
! No known limitations.
!-------------------------------------------------------------------------------
!</portability_doc>
!
!<compile_doc>
!-------------------------------------------------------------------------------
!                     SPECIAL COMPILING REQUIREMENTS
!
! Use : -DSOLARIS flag on SOLARIS machines.
!       No other flags necessary on any other platforms.
!       Don't forget refreshlock.h 
!
! Example: gcc -o refreshlock -DSOLARIS -I. refreshlock.c
!
!-------------------------------------------------------------------------------
!</compile_doc>
!
!!-------------------------- start of program ------------------------------!!
!!-------------------------- start of program ------------------------------!!
!!-------------------------- start of program ------------------------------!!
*/

#include "refreshlock.h"
/*------------------------ start of functions ----------------------------*/
/*------------------------ start of functions ----------------------------*/

#ifdef __cplusplus
extern "C" {
#endif

char  *refreshlock_ident=
      "$Id: refreshlock.c,v 1.2 2007/10/23 21:12:31 Menger prod sps $";

  char   time_stamp[40];
  static int    my_errno;

/*******************************************************************************
*  Tests the cps_lockfile to see if a lock has been "hung", and if so, takes
*  action to rename the offending lockfile, create a new one, and move on.
*
*  Written December 2000 William M. Menger 
*  Modified May 2003 for multiple lock files by Charles Burch
*******************************************************************************/
int main(int argc, char **argv) {
  char   lock_file[260], new_file[260], base_lockfile[260], *ptr;
  int    i_lockfile, num_lockfiles;
  int    failure, file_created, log_sw, copy_sw;
  pid_t  pidr,   pidw;
  long   sysidr, sysidw;
  int    fd, i, open_count, rename_counter, file_mod_count, lock_active;
  time_t mod_time, access_time;
  long   pid_file, sysid_file;
  struct stat st_buf;

  struct lock_struct {
    long   sysidr;
    long   sysidw;
    int    file_time_changes;
    pid_t  pidr;
    pid_t  pidw;
    time_t mod_time;
    time_t access_time;
  };
  struct lock_struct *olds;

  printf(
"**************************************************************************\n");
  printf( "* %s:\n",argv[0]);
  printf( "*               Usage:\n");
  printf( "* $ %s -t wait_seconds -f lock_file_path -m file_mod_count -c\n",
          argv[0]);
  printf(
"**************************************************************************\n");
  printf( "* %s\n",refreshlock_ident);
  printf(
"**************************************************************************\n");

  rename_counter=0;
  mod_time=-1;
  access_time=-1;

  lock_held_time=120;
  file_mod_count=0;
  log_sw=0;
  copy_sw=0;
  strcpy(lock_file, "");

/*************** get command line arguments ********************************/
  for (i = 1;i<argc;i++) {                
    if(strncmp(argv[i],"-t",2) == 0 ) {
      lock_held_time = atoi(argv[++i]);

    } else if(strncmp(argv[i],"-f",2) == 0 ) {
        strcpy(lock_file, argv[++i]);

    } else if(strncmp(argv[i],"-m",2) == 0 ) {
      file_mod_count=atoi(argv[++i]);

    } else if(strncmp(argv[i],"-l",2) == 0 ) {
      log_sw=1;

    } else if(strncmp(argv[i],"-c",2) == 0 ) {
      copy_sw=1;

    } else {
      printf("Invalid argument[%d] [%s]-job aborted.\n",i,argv[i]);
      exit(-1);
    }
  }

  if(lock_file[0]=='\0') {
    printf("No lock file specified-job aborted\n");
    exit(-1);
  }
  
  strcpy(base_lockfile,lock_file);  /*get number lockfiles and base lockfile*/
  num_lockfiles=1;
  if((ptr=strstr(base_lockfile,":"))!=NULL) {
    (*ptr)='\0';
    if((num_lockfiles=atoi(ptr+1))<1) num_lockfiles=1;
  }

  if((olds=(struct lock_struct*)
   malloc(num_lockfiles*sizeof(struct lock_struct)))==NULL) {
    printf("Unable to malloc olds-job aborted\n");
     exit(-1);
  }

  for(i=0;i<num_lockfiles;i++) {
    olds[i].pidr=-1;
    olds[i].pidw=-1;
    olds[i].sysidr=-1;
    olds[i].sysidw=-1;
    olds[i].file_time_changes=0;
    olds[i].mod_time=0;
    olds[i].access_time=0;
  }
  
  printf("Wait time = %d seconds.\n", lock_held_time);
  printf("Lock file = %s.\n", lock_file);
  printf("Base lock file=%s, Number lock files=%d.\n",
      base_lockfile, num_lockfiles);
  printf("File modification time count=%d.\n",file_mod_count);
  printf("Copy old lockfile to any new lock file=%s.\n",
   refreshlock_yes_no(copy_sw));
  printf("Debug logging=%s.\n",refreshlock_yes_no(log_sw));
  printf(
"**************************************************************************\n");
  if(lock_held_time<1) {
    printf("*** ERROR *** Wait time must be greater than 0\n");
    exit(1);
  }
  
  /*  Ensure lockfiles exist   */
  for(i_lockfile=0; i_lockfile<num_lockfiles; i_lockfile++) {
    refreshlock_make_lockfile_name(base_lockfile,lock_file,i_lockfile,
        num_lockfiles);
    if(access(lock_file,F_OK)!=0) {
      sleep(1);                         /* sleep a bit and retry         */
      if(access(lock_file,F_OK)!=0) 
        refreshlock_create_new_file(lock_file);
    }
  }
 
  i_lockfile=0;
  while(1) {        /*This is the main test loop:test-sleep-loop*/
    refreshlock_make_lockfile_name(base_lockfile,lock_file,i_lockfile,
        num_lockfiles);

    /**** Try to open file-if unable see if file exist and create if not *****/ 
    open_count = 0;
    while((fd=open(lock_file,O_RDWR))<0) {  /***  Open the lockfile         */
      my_errno=errno;
      strcpy(time_stamp,refreshlock_time_stamp());
      fprintf(stderr, "%s: Could not open %s. Errno=%d [%s]\n",
       time_stamp,lock_file, my_errno, strerror(my_errno));

      if(access(lock_file,F_OK)!=0) {     /*  Ensure a lockfile exists     */
        sleep(1);                         /* sleep a bit and retry         */
        if(access(lock_file,F_OK)!=0) 
          refreshlock_create_new_file(lock_file);
      }
      
      if(open_count < lock_held_time){   /* slow down the retry if lots    */
        open_count++;
        sleep(1);
      } else {
        sleep (lock_held_time);          /* of failures                    */
      }
    }

/********************** Lock Failure Detection Logic **************************
 If we have a current lock on the lock file, we compare pid and sysid of the 
 read /write locks  with those seen last time period. If they are the same, we 
 have a potential problem, unless there were no active locks the last time
 checked.

   If -m is not specified and we have detected a potential problem  above, we
   declare we should try to make new lock file.

   If -m specified, check file access and modification times to see if file has 
   been written to or read within last time period check.  
     If not, we assume the lock is hung and we need to try to create a new lock 
     file.
     If so, assume we either have a new lock by same process or it is doing IO 
     to the file. If we find the same process has done IO "file_file_mod_count" 
     time period checks in a row (which should not happen with the CPS lock 
     code), assume it is hung for whatever reason, and declare we need to try 
     to create new lock file, otherwise we move on and keep track of the 
     number of times the same process has the file locked and is doing active 
     IO.
     
   Once we declare we want to try to create a new lock file, we attempt set a 
   lock on the file.  If we can lock and unlock the file, we know the potential
   problems has cleared itself, so we declare no lock problem and wait for the
   next lock check time period. If we can not create the lock or unlock it, we
   assume a problem exists with the lock file and proceed to create a new
   lock file.
*******************************************************************************/

/************************  Look for read/write locks  *************************/
/***** The next two lines do not seem to work to detect read locks
    refreshlock_lcktst(fd,F_RDLCK,SEEK_SET,0,0,&pidr,&sysidr);
    refreshlock_lcktst(fd,F_WRLCK,SEEK_SET,0,0,&pidw,&sysidw);
*******/

    refreshlock_lock_test(fd, SEEK_SET, 0, 0, lock_file, 
        &pidr, &sysidr, &pidw, &sysidw);
    failure=0;     /*No lock failures yet*/
    lock_active=0; /*no lock active yet  */

    if(pidr!=0 ) { 
      fprintf(stderr, "%s: Read Lock held on %s (sysid %ld) pid %d\n",
      refreshlock_time_stamp(), lock_file,sysidr, (int) pidr);
      if (olds[i_lockfile].pidr==pidr && olds[i_lockfile].sysidr==sysidr) 
       lock_active+=1;  /*read lock active*/
      fflush(stderr);
    }

    if(pidw!=0 ) {
      fprintf(stderr, "%s: Write Lock held on %s (sysid %ld) pid %d\n",
      refreshlock_time_stamp(), lock_file,sysidw, (int) pidw);
      if(olds[i_lockfile].pidw==pidw && olds[i_lockfile].sysidw==sysidw) 
       lock_active+=2;  /*write lock active*/
      fflush(stderr);
    }

/********************** Check file modification/access time ******************/
    if(file_mod_count>0 && lock_active>0) {
      if(stat(lock_file, &st_buf)==0) {
        mod_time=st_buf.st_mtime;
        access_time=st_buf.st_atime;
         /*note-first time lock active, should always get time change*/
        if(difftime(mod_time,olds[i_lockfile].mod_time)       >0 || 
           difftime(access_time,olds[i_lockfile].access_time) >0){ 
          /*increment #time file time changed*/
          olds[i_lockfile].file_time_changes++;
          olds[i_lockfile].mod_time=mod_time;
          olds[i_lockfile].access_time=access_time;
        } else {
          olds[i_lockfile].file_time_changes=0;  /*No new file time change*/
        }
      } else {
        my_errno=errno;
        strcpy(time_stamp,refreshlock_time_stamp());
        fprintf(stderr, "%s: Could stat %s. Errno=%d [%s]\n",
         time_stamp,lock_file, my_errno, strerror(my_errno));
        failure+=4;
      }
    } else {
      olds[i_lockfile].file_time_changes=0;
    }

/** printf("filechanges=%d, lock_active=%d\n",
     olds[i_lockfile].file_time_changes, lock_active); **/

    if (lock_active>0) {  

      if(olds[i_lockfile].file_time_changes>0) {
        if(olds[i_lockfile].file_time_changes>file_mod_count) {
          fprintf(stderr, 
           "%s: Lock held on %s-file modification count exceeded\n",
           refreshlock_time_stamp(), lock_file);
        } else {
          fprintf(stderr, 
           "%s: Lock held on %s-file modification count=%d\n",
           refreshlock_time_stamp(), lock_file, 
             olds[i_lockfile].file_time_changes);
        }
      }

      /*see if lock hung */
      if(olds[i_lockfile].file_time_changes==0 || 
        olds[i_lockfile].file_time_changes>file_mod_count) {

        if(refreshlock_lock_reg(fd,F_SETLK,F_WRLCK,0,SEEK_SET,0,
              lock_file)<0) {
          failure+=lock_active;
        } else {
          olds[i_lockfile].file_time_changes=0;
          if(refreshlock_lock_reg(fd,F_SETLK,F_UNLCK,0,SEEK_SET,0,
                lock_file)<0){
            failure+=lock_active;
          }
        }
      }
    }

    close(fd);       /*close current lock file-done with it or making new one*/

/***************** If failure, rename file, create new file ******************
 If -c is not specified, we rename the old lock file and make a new empty lock
 file.

 If -c is specified, we rename the old lock file, copy the contents of the old
 lock file to a temporay file, and when the copy is done, rename the temporary
 file to the lock file name.  If unable to do any of these steps, simple make
 a new empty lock file.

 In the above rename operations, if the rename fails, we try to delete the 
 file and make a new empty lock file.
******************************************************************************/

    if(failure>0) {

      olds[i_lockfile].file_time_changes=0;/*new file-no time changes with it*/
      pid_file=pidw;
      sysid_file=sysidw;
      if(pid_file==0) {
        pid_file=pidr;
        sysid_file=sysidr;
      }

      sprintf(new_file,"%s.%ld.%ld.%d",
       lock_file, sysid_file, pid_file, ++rename_counter);

      if(rename(lock_file,new_file)<0) { 
        my_errno=errno;
        strcpy(time_stamp,refreshlock_time_stamp());
        fprintf(stderr, "%s: Error in renaming (%s) to (%s). ErrNo=%d [%s]\n", 
         time_stamp, lock_file, new_file, my_errno, 
          strerror(my_errno));
        if(remove(lock_file)<0) { /*Unable to rename, try deleting old file*/
          my_errno=errno;
          strcpy(time_stamp,refreshlock_time_stamp());
          fprintf(stderr, "%s: Error in deleting (%s). ErrNo=%d [%s]\n", 
         time_stamp, lock_file, my_errno, strerror(my_errno));
        } else {
          fprintf(stderr, "%s: Lock file(%s) deleted. ErrNo=%d [%s]\n", 
           time_stamp, lock_file, my_errno, strerror(my_errno));
          refreshlock_create_new_file(lock_file);/*create new lockfile*/
        }
      } else {
        strcpy(time_stamp,refreshlock_time_stamp());
        fprintf(stderr, "%s: Renamed (%s) to (%s)\n",
          time_stamp,lock_file, new_file);

        file_created=0;
        if(copy_sw) {
          file_created=refreshlock_copy_file(new_file,lock_file);
        }

        if(file_created==0) { 
          /*create new lockfile*/
          file_created=refreshlock_create_new_file(lock_file);
        }

        if(file_created==0) {
          strcpy(time_stamp,refreshlock_time_stamp());
          fprintf(stderr, "%s: Unable to create new lock file (%s)\n",
            time_stamp,lock_file);
        }
      }
      olds[i_lockfile].pidw=0;
      olds[i_lockfile].pidr=0;
    } else {

/*****  keep read/write info to use with next test cycle *******/
      olds[i_lockfile].pidr   = pidr;
      olds[i_lockfile].pidw   = pidw;
      olds[i_lockfile].sysidr = sysidr;
      olds[i_lockfile].sysidw = sysidw;
    }

    if(log_sw) {             /*write debug info if requested*/
      printf("Done with this round(%s) at %s.\n", 
          lock_file, refreshlock_time_stamp());
      printf(
       "  mtime=%ld, atime=%ld, pidr/w=%d/%d, sysidr/w=%ld/%ld, #tchanges=%d\n",
       (long) mod_time, (long) access_time, 
       (int)pidr, (int)pidw, (long)sysidr, (long)sysidw, 
       olds[i_lockfile].file_time_changes);
    }
    
    /*go to next lockfile-if cycled through all lockfiles-sleep/start over   */
    if((++i_lockfile)>=num_lockfiles) {
      i_lockfile=0;
      if(log_sw) {
        printf("\n"); fflush(stdout);
      }
      sleep(lock_held_time);
    }
  }
  /* return (0); */
}

/******************************************************************
* Create a lockfile name from base name and lockfile number
*
* Written Feb 2003 by Charles C Burch
*******************************************************************/
void refreshlock_make_lockfile_name(char *base_lockfile, 
    char *lock_file, int i_lockfile, int n_lockfiles){
  if(n_lockfiles<=1) {
    strcpy(lock_file, base_lockfile);
  } else {
    sprintf(lock_file,"%s.%d",base_lockfile,i_lockfile);
  }
  return;
}

/******************************************************************
* Create a new file and copy old file to it 
*
* Written Feb 2002 by Charles C Burch
*******************************************************************/
int refreshlock_copy_file(char *old_file, char *new_file){
  char buff[260], tmp_file[260];
  FILE *fd_in, *fd_out;
  int n_in;

  strcpy(tmp_file, new_file);
  strcat(tmp_file,".t_m_p");
  if((fd_in=fopen(old_file,"r"))==NULL){
    my_errno=errno;
    strcpy(time_stamp,refreshlock_time_stamp());
    printf("%s:Copy File unable to open input %s, errno=%d [%s]\n",
     time_stamp, old_file,my_errno,strerror(my_errno));
     return(0);
  }
  remove(tmp_file);
  refreshlock_create_new_file(tmp_file); /*this does right chmod*/
  if((fd_out=fopen(tmp_file,"w+"))==NULL) {
    my_errno=errno;
    strcpy(time_stamp,refreshlock_time_stamp());
    printf("%s:Copy File unable to open output %s, errno=%d [%s]\n",
     time_stamp, tmp_file,my_errno,strerror(my_errno));
     fclose(fd_in);
     return(0);
  }

  while(1) {
    if((n_in=fread(buff,1,260, fd_in))<0) {
      my_errno=errno;
      strcpy(time_stamp,refreshlock_time_stamp());
      printf("%s:Copy File, error in reading input %s, errno=%d [%s]\n",
       time_stamp, old_file,my_errno,strerror(my_errno));
      fclose(fd_in);
      fclose(fd_out);
      return(0);
    }
    if(n_in==0) break;
    if(fwrite(buff,1, n_in,fd_out) != n_in) {
      my_errno=errno;
      strcpy(time_stamp,refreshlock_time_stamp());
      printf("%s:Copy File, error in writing output %s, errno-%d [%s]\n",
       time_stamp, tmp_file,my_errno,strerror(my_errno));
      fclose(fd_in);
      fclose(fd_out);
      return(0);
    }
  }
  fclose(fd_in);
  fclose(fd_out);
  if(rename(tmp_file, new_file) < 0 ) { 
    my_errno=errno;
    strcpy(time_stamp,refreshlock_time_stamp());
    fprintf(stderr, "%s: Error in renaming (%s) to (%s). ErrNo=%d [%s]\n", 
     time_stamp, tmp_file, new_file, my_errno, strerror(my_errno));
    return(0);
  }

  printf("%s: File(%s) created and copied to\n",
   refreshlock_time_stamp(), new_file);
  return(1);
}  
  
/******************************************************************
* Create a new lock file and chmod it
*
* Written Feb 2002 by Charles C Burch
*******************************************************************/
int refreshlock_create_new_file(char *lock_file) {

  if(creat(lock_file,S_IRUSR|S_IWUSR|S_IRGRP|S_IWGRP|S_IROTH|S_IWOTH) < 0){
    my_errno=errno;
    strcpy(time_stamp,refreshlock_time_stamp());
    fprintf(stderr,
     "%s: Could not create lockfile(%s): ErrNo=%d [%s]\n",
     time_stamp, lock_file, my_errno, strerror(my_errno));
    return(0);
  } 

  if(chmod(lock_file,S_IRUSR|S_IWUSR|S_IRGRP|S_IWGRP|S_IROTH|S_IWOTH) < 0){
    my_errno=errno;
    strcpy(time_stamp,refreshlock_time_stamp());
    fprintf(stderr,
     "%s: Could not chmod lockfile(%s): ErrNo=%d [%s]\n",
     time_stamp, lock_file, my_errno, strerror(my_errno));
    return(0);
  } 
  strcpy(time_stamp,refreshlock_time_stamp());
  fprintf(stderr, "%s: Created new lockfile(%s)\n",
   time_stamp, lock_file);
  return(1);
}

/******************************************************************
* get a time stamp string
*
* Written Feb 2002 by Charles C Burch
*******************************************************************/
char *refreshlock_time_stamp() {
  static char buff[80];
  time_t      now;
  struct tm   *tm_ptr;

  time(&now);
  tm_ptr=localtime(&now);
  sprintf(buff,"%4.4d-%2.2d-%2.2d %2.2d:%2.2d:%2.2d",
   1900+tm_ptr->tm_year, 1+tm_ptr->tm_mon, tm_ptr->tm_mday,
   tm_ptr->tm_hour, tm_ptr->tm_min, tm_ptr->tm_sec);
  return((char*)(&buff));
}

/******************************************************************
* returns char string yes if sw!=0 and no if sw==0
*
* Written Feb 2002 by Charles C Burch
*******************************************************************/
char *refreshlock_yes_no(int sw) {
  static char *yes="yes", *no="no";
  if(sw==0) return(no);
  return(yes);
}
/******************************************************************
* Performs a lock command
*
* Written Feb 2002 by Charles C Burch, patterned after R. Stevens.
*******************************************************************/
int refreshlock_lock_reg(int fd, int cmd, short type, off_t offset,
  short whence, off_t len, char *flnm) {

  struct flock lock;
  int    stat;
 
  lock.l_type   = type;          /* F_RDLCK, FWRLCK, F_UNLCK */
  lock.l_start  = offset;        /* byte offset relative to whence */
  lock.l_whence = whence;        /* SEEK_SET, SEEK_CUR, SEEK_END */
  lock.l_len    = len;           /* bytes to lock-o means to EOF */
  stat=fcntl(fd, cmd, &lock);
  if(stat<0) 
    my_errno=errno;
    strcpy(time_stamp,refreshlock_time_stamp());
    fprintf(stderr,"%s: Lock region(%s): ErrNo=%d [%s]\n",
     time_stamp, flnm, my_errno,strerror(my_errno));
  return(stat);
  }

/************************************************************
* Performs a lock test to tell if file locked, how locked and
* by whom
*
* Written February 2002 by Charles C Burch
*************************************************************/
void refreshlock_lock_test(int fd, 
     off_t offset, short whence, off_t len, char *lockfile, 
     pid_t *pid_r, long *sysid_r, pid_t *pid_w, long *sysid_w ){

  struct flock lock;

  lock.l_type   = F_WRLCK;
  lock.l_start  = offset;
  lock.l_whence = whence;
  lock.l_len    = len;

  *pid_r   = 0;
  *sysid_r = 0;
  *pid_w   = 0;
  *sysid_w = 0;

  if(fcntl(fd,F_GETLK,&lock) < 0 ) {
    my_errno=errno;
    strcpy(time_stamp,refreshlock_time_stamp());
    fprintf(stderr,"%s:lcktst(%s): ErrNo=%d [%s]\n",
     time_stamp, lockfile, my_errno,strerror(my_errno));

  } else {

    if(lock.l_type == F_RDLCK) {
#ifdef SOLARIS
      *sysid_r = lock.l_sysid;
#endif
      *pid_r   = lock.l_pid;
    } else if(lock.l_type == F_WRLCK) {
#ifdef SOLARIS
      *sysid_w = lock.l_sysid;
#endif
      *pid_w   = lock.l_pid;
    }
  }
}

/************************************************************
* Performs a lock test to tell us who has locked the file.
* Note: CCB 2/2002 this does not seem to work with read locks
* on the Albuquerque Linux system
*
* Written Nov 2000 by Bill Menger, patterned after R. Stevens.
*************************************************************/

void refreshlock_lcktst(int fd, short type,off_t offset,
     short whence, off_t len, pid_t *pid, long *sysid ){

  struct flock lock;

  lock.l_type   = type;
  lock.l_start  = offset;
  lock.l_whence = whence;
  lock.l_len    = len;

  *pid          = 0;
  *sysid        = 0;

  if(fcntl(fd,F_GETLK,&lock)<0) {
    my_errno=errno;
    strcpy(time_stamp,refreshlock_time_stamp());
    fprintf(stderr,"%s:lcktst: ErrNo=%d [%s]\n",
     time_stamp, my_errno,strerror(my_errno));

  } else {

    if(lock.l_type!=F_UNLCK) {
#ifdef SOLARIS
      *sysid=lock.l_sysid;
#endif
      *pid  =lock.l_pid;
    }
  }
}

#ifdef __cplusplus
}
#endif
