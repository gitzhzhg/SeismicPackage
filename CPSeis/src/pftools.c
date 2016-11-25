/*<CPS_v1 type="PROGRAM",PRETAG="!"/>
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
!
!<brief_doc>
!-------------------------------------------------------------------------------
!                       C P S   P R O G R A M 
!
! Name       : pftools (Tools to access CPS large files from the system.)
! Category   : stand-alone
! Written    : 2000-10-03   by: Bill Menger
! Revised    : 2003-10-10   by: Bill Menger
! Maturity   : beta
! Purpose    : Provide rm,cp,mv,chmod,ls,lfd,ulf,lkf,rcp functions for CPS files
! Portability: No known limitations.
!
!-------------------------------------------------------------------------------
!</brief_doc>
!
!
!<descript_doc>
!-------------------------------------------------------------------------------
!                          GENERAL DESCRIPTION
!
! CPS uses very large files that may exceed the capacity of either the
! operating system, file system, or both.  To accomodate this aspect of our
! data files, a package of routines called "pfio" is a part of the CPS
! system.  Pfio allows CPS to create and use files that exceed both the 
! capacity of an individual file system and the capacity of the operating
! system's file/directory structures.  
!
! Pftools provides an interface into the "pfio" package at the command-line
! (operating system) level.  Not all functions are provided, but those deemed
! most useful are available.  Included are:
!
! lfd    ==> dump the CPS lock file
! lkf    ==> lock a specific file in the CPS lock file
! pchmod ==> mimic the "chmod" function
! pcp    ==> mimic the "cp" function
! pmv    ==> mimic the "mv" function
! prcp   ==> mimic the "rcp" function
! prm    ==> mimic the "rm" function
! psz    ==> provide file size (in bytes, kbytes, mbytes, and gbytes)
! ulf    ==> remove specified files from CPS lock file (which unlocks them)
! f2pf   ==> copies a Unix file to a pf file
! pf2f   ==> copies a pf file to a Unix file
! tmpzap ==> deletes all cpstemp related files
!
! Not all modes and switches of these Unix/Linux functions are provided, but 
! only those deemed most likely to be useful for CPS users.
!
! For help on any pftool function, simply type in the function name at the
! command line with no arguments.
! Example: for help on pcp, type "pcp"
!
! For beta testing, use either "betapcp" or "pcpbeta" ... check with your
! CPS administrator to find out how the beta codes are loaded on the system.
!
!-------------------------------------------------------------------------------
!</descript_doc>
!
!
!<advice_doc>
!-------------------------------------------------------------------------------
!                          ADVICE FOR USERS
! 
! Usage: (the -i flag will prompt you for each action.)
!        (the -e flag allows you to set extent size in Mb of created files.)
!        (       default extent size is 250 Mbytes)
!        (usage of -e: -e NNNN where 1 <= NNNN <= infinity/1Mbyte )
!        (the -v flag is verbose mode to track progress of given command.)
!
! lfd: Displays the contents of the CPS lock file
!      Usage: lfd
!
! lkf: Lock a file in the CPS lock file.
!      Usage: lkf [-i] [-t timeout] [-w] [-x] file1 [file2 [file3...]]
!      -t specifies the time out(secs) to use with the lock-default is 900 secs
!      -w specified to wait to lock the file in case it is already locked
!      -x specified to use extended lock_type when locking the file
!
! pchmod: CPS I/O chmod 
!         Usage: $ "pchmod [i] mode file".
!         Purpose: change permissions on a file
!            Note: mode = OCTAL# IJK where I=0-7,J=0-7,K=0-7
!                  I=Owner, J=Group, K=All and I,J, and K are
!                  each the sum of the permission bits for the
!                  respective Ownership category(all,grp,own).
!                  0=none,1=execute 2=write 4=read permission.
!         Example: $ pchmod 642 myfile.dat means all have read and
!                  write perm, grp has read, owner has write.
!
! pcp: CPS I/O Copy
!      Usage: $ "pcp [-e NNNN] [-i] file1 file2".
!      Purpose: Copy file1 to file2
!      -e NNNN allows you to specify an "extent" size for the file.  The
!      value NNNN is in Megabytes, and can range from 1 to VERY LARGE.  An
!      "extent" can be as large as a few hundred gigabytes... Don't ask
!      for a large extent size if you are copying to CPSDATA, CPSTEMP, or
!      CPSWORK, because you can likely fill up one disk server in the
!      pre-allocation step that automatically runs in the background while
!      the copy is being made.  You can use the extent option to generate
!      a file with smaller extents so that it can fit in the "holes" on 
!      very full disk servers, or with larger extents that are as large as
!      the entire file... i.e. no extensions made.
!
! pmv: CPS I/O Rename
!      Usage: $ "pmv [-i] file1 file2".
!      Purpose: Rename file1 to file2
!
! prcp: CPS I/O Remote copy
!       Usage: $ "prcp [-i] file1 file2".
!       Purpose: Copy file1 to file2 where one of the files is on a
!                remote machine
!
! prm: CPS I/O Remove
!      Usage: $ "prm [-i] file1 [file2 [file3...]]".
!      Purpose: Remove file(s) and all extensions
!
! psz: CPS I/O File Size
       Usage: $ "psz file1 [file2 [file3...]]".
!      Output: Bytes Mbytes Gbytes Filename
!      User should note Mbytes are 1024*1024 bytes and
!                       Gbytes are 1024*1024*1024 bytes
!      The total sizes printed can possibly count file extensions multiple times
!      depending on how specifies the file names.
!
! ulf: Removes files from CPS lock file which unlocks them
!      Usage: ulf [-i] file1 [file2 [file3...]]
!
! f2pf:Converts a unix file to a pf file
!      Usage: $ "pcp [-e NNNN] [-i] file1 file2".
!      This would normally be used on unix versions supporting files > 2GB
!
! pf2f:Converts a pf file to a unix file
!      Usage: $ "pcp [-e NNNN] [-i] file1 file2".
!      This would normally be used on unix versions supporting files > 2GB
!
! tmpzap: Deletes all cpstemp related files.
!      Usage: $ "tmpzap [-v]
!-------------------------------------------------------------------------------
!</advice_doc>
!
!
!<history_doc>
!-------------------------------------------------------------------------------
!                           REVISION HISTORY
!
!     Date        Author       Description
!     ----        ------       -----------
! 16. 2003-10-10  Bill Menger  Changed so that names can be preceded with "beta"
! 15. 2003-10-08  Bill Menger  Modified to allow pcp to copy files with any
!                              extent size, even > 2 gigs.
! 14. 2003-06-05  Chuck Burch  Added tmpzap
! 13. 2003-05-05  Chuck Burch  Added f2pf and pf2f.
! 12. 2003-03-24  Bill Menger  Modified the default extent size to equal the
!                              original file extent size instead of 2048.
! 11. 2003-03-21  Bill Menger  Modified -e to use extent size up to 
!                              2048Mb - 1024 bytes by default. 
!                              Changed the buffer size to 262140 bytes from
!                              65535 to try to speed up copies.
! 10. 2003-02-24  Bill Menger  Added the -e (extent size) option (for pcp).
!  9. 2002-09-11  Vunderink    Commented out invoking signal handler in prcp.
!  8. 2002-08-26  Vunderink    Added prcp (remote file copy).
!  7. 2002-08-09  CC Burch     Added lkf (lock file).
!                              Made compatible with cnfg pfio-changes.
!  6. 2002-01-08  CC Burch     Changed logic so psz works with cpstemp files
!  5. 2001-10-03  CC Burch     Added header when psz used.  
!                              Print warning on total for pfio file segments.
!  4. 2001-09-18  CC Burch     Modified code for prm to work with cpstemp
!                              Added capital function names for testing
!                               on systems where pftools are already installed
!  3. 2001-08-16  Bill Menger  Added -i option and ptest program.
!  2. 2001-04-11  CC Burch     Added lfd(lockfile dump) and ulf(unlock file)
!  1. 2000-10-03  Bill Menger  Initial version. (Original version 2000-09-19)
!
!-------------------------------------------------------------------------------
!</history_doc>
!
!
!<portability_doc>
!-------------------------------------------------------------------------------
!                         PORTABILITY LIMITATIONS
!
! No known limitations.
!
!-------------------------------------------------------------------------------
!</portability_doc>
!
!
!<compile_doc>
!-------------------------------------------------------------------------------
!                     SPECIAL COMPILING REQUIREMENTS
!
! After compiling and linking pftools please make links in the appropriate 
! install (bin) directory from pftools to the following file names:
! (ln -s pftools ) prm pcp pmv pchmod psz lfd ulf lkf f2pf pf2f rcp tmpzap
! The pftools package will key its behavior off of the "calling" name.  (If it
! is called prm, it will perform the prm function, etc.)
! The "calling name" can be preceded by "beta" or postfixed with "beta" and 
! the code will still operate the same.  (e.g. pcpbeta == pcp).
! DEPENDENCIES:
! pftools depends on pfio, bfio, and skio
!
!-------------------------------------------------------------------------------
!</compile_doc>
!
!
!<algorithm_doc>
!-------------------------------------------------------------------------------
!                  ALGORITHM DESCRIPTION FOR DEVELOPERS
!
!
!-------------------------------------------------------------------------------
!</algorithm_doc>
!
!
!<programming_doc>
!-------------------------------------------------------------------------------
!                           PROGRAMMING NOTES
!
! The program is simply a series of stand-alone functions, each
! of which is found by peering at the basename of the calling
! program.  If the program is named psz, then the psz function will
! execute and the program will exit.  If pcp, then the appropriate
! if-block below will execute.  The only reason for doing this is to
! keep the code in one file for easy maintenance.
! 
!-------------------------------------------------------------------------------
!</programming_doc>
*/

#define _LARGEFILE_SOURCE
#define _FILE_OFFSET_BITS 64

#include <ctype.h>
#include <errno.h>
#include <fcntl.h>
#include <limits.h>
#include <signal.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>
#include <pthread.h>

#include <sys/stat.h>
#include <sys/types.h>
#include <sys/wait.h>

#ifdef __cplusplus
extern "C" {
#endif

#include "c2f_interface.h"
/*#include "pfiocodes.h"*/
#include "pfio.h"
#include "cnfg.h"
#include "bfio.h"
#include "str.h" 
#include "unix.h"  

void pfio_init();
void pfio_form_fn_ext(char*, int, char*);

#ifdef __cplusplus
}
#endif

#define RCP_COMMAND "/usr/bin/rcp"
#define MAX_RCP_TRIES 4

char *pftools_ident = 
"$Id: pftools.c,v 1.211 2008/02/15 19:05:02 mengewm Exp $";

extern char **environ;
int status;

int pftools_get_affirmation(){
  int a=0;
  if(tolower(getchar()) == 'y') a++;
  while(getchar() != 10); /* process the rest of the line and ignore it */
  return a;
}
  
int pftools_rcp(char *in, char *out)
{
  char *argv[5];
  pid_t pid;
  int status;

  pid = fork();
  if (pid < 0) {           /*error*/
    fprintf(stderr,"pftools_rcp: fork error is %s\n",strerror(errno));
    status = -1;

  } else if (pid == 0) {   /*child*/
    argv[0] = RCP_COMMAND;
    argv[1] = "-p";
    argv[2] = in;
    argv[3] = out;
    argv[4] = 0;

    if (execve(RCP_COMMAND, argv,environ) != 0) {
      fprintf(stderr,"pftools_rcp: execv error [%s]\n",strerror(errno));
      status = -1;
    } else {
      status = 0;
    }

  } else {                 /*parent*/

    if (waitpid(pid, &status, 0) == -1) {
      fprintf(stderr,"pftools_rcp: waitpid error is %s\n",strerror(errno));
    }
  }

  return status;

}

void pftools_sig_handler (int sig)
{
   printf("pftools_sig_handler called\n");
   fflush(stdout);
   return;
}

int pftools_tmpzap(int verbose_mode) {
  char cmd[360], user[40], nodes_file[120];

  printf("\n"); 
  printf("tmpzap should only be run when you have no CPS jobs running\n"); 
  printf("tmpzap is about to delete all your cpstemp info--is this OK(Y/N):");
  if(!pftools_get_affirmation()) return(0);
  
  strcpy(user,unix_user_name_c());
  sprintf(cmd,"~%s/tmpzap_%d_%d.nds",
    user,(int)getpid(),(int)pthread_self()); 
  bfio_expand_file_name(cmd,nodes_file,sizeof(nodes_file));

  printf("  Deleting cpstemp files from compute servers-please be patient\n"); 
  sprintf(cmd,"getallup >%s",nodes_file);
  /** printf("cmd=%s\n",cmd); **/
  system(cmd);

  sprintf(cmd,
   "multi_rsh 10 300 %s \\rm -r -v /tmp/*/%s/CPSTEMP/*",
   nodes_file, user); 
  if(!verbose_mode) strcat(cmd," >/dev/null");
  /** printf("cmd=%s\n",cmd); **/
  system(cmd);

  printf("  Deleting links from ~%s/cpstemp\n",user);
  sprintf(cmd,"\\rm -r ~%s/cpstemp/*",user);
  if(!verbose_mode) strcat(cmd," >/dev/null 2>/dev/null");
  /** printf("cmd=%s\n",cmd); **/
  system(cmd);

  printf("tmpzap completed\n");
  remove(nodes_file);
  return(1);
}

int main (int argc, char ** argv) {
  char      fnin_ext[PATH_MAX];
  char      fnout_ext[PATH_MAX+20];
  char      *buff = NULL, lock_type, current_lock;
  char      *progname, cps_lock_file[80], *newname;
  double    dfsiz = 0;
  int       i = 0, infile = -1, outfile = -1, imode, doit, ifile, isize;
  int       retvar = 0, interactive_mode, wait_mode, verbose_mode;
  long long fsize,totsiz=0, bar_work=0, extsize=-1;
  long      nread, nwrite, temp, timeout,bufsiz=4194304;
  FILE      *fd=NULL;
  struct    stat stat_info;
  
  /* UNUSED
  struct sigaction action;
  sigset_t zeroset;
  END OF UNUSED */

  /******** These are the functions that will be supported so far ******/
  char * ptest  = "ptest";  /* test function */
  char * psz    = "psz";    /* CPS file size */
  char * prm    = "prm";    /* CPS file remove */
  char * pcp    = "pcp";    /* CPS file copy */
  char * pmv    = "pmv";    /* CPS file rename */
  char * pchmod = "pchmod"; /* CPS file change permissions */
  char * lfd    = "lfd";    /* dump CPS lock file */
  char * ulf    = "ulf";    /* Unlock file */
  char * lkf    = "lkf";    /* Lock file */
  char * prcp   = "prcp";   /* CPS file remote copy */
  char * f2pf   = "f2pf";   /* Copy a Unix file to a pf file*/
  char * pf2f   = "pf2f";   /* Copy a pf file to a unix file */
  char * tmpzap = "tmpzap"; /* Deletes all cpstemp-related files*/

/** The following are for testing on systems where pftools exist **/
  char * PTEST  = "PTEST";  /* test function */
  char * PSZ    = "PSZ";    /* CPS file size */
  char * PRM    = "PRM";    /* CPS file remove */
  char * PCP    = "PCP";    /* CPS file copy */
  char * PMV    = "PMV";    /* CPS file rename */
  char * PCHMOD = "PCHMOD"; /* CPS file change permissions */
  char * LFD    = "LFD";    /* dump CPS lock file */
  char * ULF    = "ULF";    /* Unlock file */
  char * LKF    = "LKF";    /* Lock file */
  char * PRCP   = "PRCP";   /* CPS file remote copy */
  char * F2PF   = "F2PF";   /* Copy a Unix file to a pf file*/
  char * PF2F   = "PF2F";   /* Copy a pf file to a unix file */
  char * TMPZAP = "TMPZAP"; /* Deletes all cpstemp-related files*/

  /******** End of functions *******************************************/

  pfio_init();

  /********* Find the function name **************/
  progname = strrchr(argv[0],'/');
  if(progname != NULL) {
    progname++;
  } else {
    progname = argv[0];
  }
  /***** Remove "beta" from the program name if it is in the front
   *     or back of the progname */

  if ( (newname=strstr(progname,"beta")) != NULL) {
    if(progname==newname) {
      progname+=4;
    } else {
      strcpy(newname,"\0");
    }
  }

  /*fprintf(stderr,"This program is <%s>\n",progname);*/
  argv++;argc--;

  interactive_mode=0;
  verbose_mode=0;
  timeout=900;
  wait_mode=0;
  lock_type='H';
  extsize=-1;
  /********* Parse the options out of arg list *******/
  while(argc>0) {
    if(strcmp(argv[0],"-i")==0) {
      interactive_mode=1;   /* -i interactive */

    } else if(strcmp(argv[0],"-t")==0) {
      if(argc==1) break;    /*see if timeout value present */
      argc--;               /* -t timeout */
      argv++;
      temp=atol(argv[0]);
      if(temp<1) {
        fprintf(stderr,"Invalid timeout(%ld), option ignored\n",temp);
      } else {
        timeout=temp;
      }

    } else if(strcmp(argv[0],"-w")==0) {
      wait_mode=1;

    } else if(strcmp(argv[0],"-v")==0) {
      verbose_mode=1;

    } else if(strcmp(argv[0],"-x")==0) {
      lock_type='X';

    } else if(strcmp(argv[0],"-e")==0) {
      argc--;  
      argv++;
      temp=atol(argv[0]);
      if(temp == 0 ) {
        extsize=temp;
      } else {
        if(temp < 1) {
          fprintf(stderr,"Invalid extent size (%ld), option ignored\n",temp);
        } else {
          extsize= temp;
        }
      } 

    } else if(argv[0][0]=='-') {
      fprintf(stderr,
      "-%c mode is not supported. (ignored)\n",argv[i][1]);

    } else {
      break;
    }
    argc--;
    argv++;
  }



  /***** TEST FUNCTION *****************/

  if(strcmp(progname,ptest) == 0  || strcmp(progname,PTEST) == 0 ) {
    if(argc == 0 ) {
      fprintf(stderr,"insufficient arguments message here\n");
      fprintf(stderr,"error code = %d\n",retvar+1);
      return retvar++;
    } else {
      fprintf(stderr,"program = %s argc = %d\n",progname, argc);
      for (i=0;i<argc;i++){
        fprintf(stderr,"argv[%d]=%s\n",i,argv[i]);
        if(interactive_mode) {
          fprintf(stderr,"Interactive mode is on. ok(y/n):");
          if(pftools_get_affirmation()) {
            fprintf(stderr,"YES!\n");
          } else {
            fprintf(stderr,"NO!\n");
          }
        }
      }
      fprintf(stderr,"error code = %d\n",retvar);
      return retvar; 
    }
  }

  /***** FILESIZE FUNCTION *****************/
  if(strcmp(progname,psz) == 0 || strcmp(progname,PSZ) == 0 ) {
    if(argc == 0 ) {
     fprintf(stderr,
     "%s: (CPS I/O File Size) Usage: $ \"%s file1 [file2 [file3...]]\".\n",
     progname, progname);
     fprintf(stderr,"    Output: Bytes Mbytes Gbytes Filename\n");
     return ++retvar;
    }
  
    printf("---Bytes---- ---Mbytes--- --Gbytes-- Filename\n");
    for (i=0;i<argc;i++){
      fsize = pfio_flsz(argv[i]);
      if(fsize<0) {
        fprintf(stderr,"%s: %s not found.\n",progname,argv[i]);
        retvar++;
      } else {
        totsiz += fsize;
        dfsiz=fsize/(1024.*((double) 1024.));
        printf("%12lld %12.5f %10.8f %s\n", 
          fsize, dfsiz, dfsiz/1024., argv[i]);
      }
    }

    if(--i > 0 ) {
      fsize = totsiz;
      dfsiz=fsize/(1024.*((double) 1024.));
      printf("\n%12lld %12.5f %10.8f %s\n", 
        fsize, dfsiz, dfsiz/1024., "Total");
      printf("Warning: Total may count file extensions multiple times\n");
    }

    return retvar;
  }

  /***** REMOVE FUNCTION *****************/
  if(strcmp(progname,prm) == 0 || strcmp(progname,PRM) == 0 ) {
    if(argc == 0 ) {
      fprintf(stderr,
       "%s: (CPS I/O Remove) Usage: $ \"%s [-i] file1 [file2 [file3...]]\".\n",
       progname, progname);
      fprintf(stderr,"Purpose: Remove file(s) and all extensions\n");
      return ++retvar;
    }
    for (i=0;i<argc;i++){
      if(interactive_mode) {
        fprintf(stderr,"Delete %s?(y/n):",argv[i]);
        if(pftools_get_affirmation()) {
          if(pfio_delete(argv[i]) <= 0 ) {
            fprintf(stderr,"%s: Error deleting file %s .\n",progname,argv[i]);
            retvar++;
          }
        }
      } else {
        if(pfio_delete(argv[i]) <= 0 ) {
          fprintf(stderr,"%s: Error deleting file %s .\n",progname,argv[i]);
          retvar++;
        }
      }
    }
    return retvar;
  }


  /***** RENAME FUNCTION *****************/
  if(strcmp(progname,pmv) == 0 || strcmp(progname,PMV) == 0 ) {
    if(argc != 2 ) {
      fprintf(stderr,
       "%s: (CPS I/O Move) Usage: $ \"%s [-i] file1 file2\".\n",
       progname, progname);
      fprintf(stderr,"Purpose: Rename file1 to file2\n");
      return ++retvar;
    }
    if(access(argv[0],F_OK) != 0 ) {
      fprintf(stderr,"%s: %s not found.\n",progname,argv[1]);
      retvar++;
    } else {
      if(interactive_mode) {
        fprintf(stderr,"Rename %s to %s?(y/n):",argv[0],argv[1]);
        if(pftools_get_affirmation()) {
          if(pfio_rename_file(argv[0],argv[1]) < 0 ) {
            fprintf(stderr,"%s: Error renaming file %s to %s .\n",
            progname,argv[0],argv[1]);
            retvar++;
          }
        }
      } else {
        if(pfio_rename_file(argv[0],argv[1]) < 0 ) {
          fprintf(stderr,"%s: Error renaming file %s to %s .\n",
          progname,argv[0],argv[1]);
          retvar++;
        }
      }
    }
    return retvar;
  }


  /***** COPY FUNCTION ******************/
  if(strcmp(progname,pcp) == 0 || strcmp(progname,PCP) == 0 ) {
    if(argc != 2) {
      fprintf(stderr,
       "%s: (CPS I/O copy) Usage: $ \"%s [-e NNNN] [-i] file1 file2\".\n",
       progname, progname);
      fprintf(stderr,"Purpose: Copy file1 to file2\n");
      return ++retvar;
    }
    if(interactive_mode) { 
      fprintf(stderr,"Copy %s to %s?(y/n):",argv[0],argv[1]);
      if(! pftools_get_affirmation()) return retvar;
    }

    if(( infile = pfio_open(argv[0],'r'))  < 0 ) {
      fprintf(stderr,"%s: Problem opening %s for read.\n", progname,argv[0]);
      ++retvar;
      goto pcp_err;
    }

    if (extsize == -1 ) {
      /* get the extent size from original file */
      extsize = pfio_file_size(argv[0]);
      /* fsize = pfio_flsz(argv[0]); */
    } else {
      /* set the extent size from user input */
      extsize*=1048576;
      /* if 2 gigs, subtract 1024 bytes */
      /* if(extsize > 2147482624 )extsize = 2147482624; */
      if(pfio_set_ext_size( (long long) extsize) != 0 ) { 
        fprintf(stderr,"Unable to set the extent size of output file.\n");
        /* Try again with maximum ext size for old 32 bit linux file systems */
        extsize=2147482624;
        if(pfio_set_ext_size( (long long) extsize) != 0 ) {return ++retvar;} 
      }; 
    }

    if((outfile = pfio_open(argv[1],'w'))  < 0 ) {
      fprintf(stderr,"%s: Problem opening %s for write.\n", progname,argv[1]);
      ++retvar;
      goto pcp_err;
    }
    
    /* set buffer size according to file size.  If file is smaller, don't
     * set up buffering with larger buffer than the file size! */
    if(extsize < (long long) bufsiz) bufsiz = (size_t) extsize;

    if((buff = (char *) malloc((size_t) bufsiz)) == NULL) {
      fprintf(stderr,"%s: Problem allocating buffer memory.\n", progname);
      ++retvar;
      goto pcp_err;
    } 

    /* Do we need to buffer an input file? I don't think so, but... */
    if(pfio_setbufsz(infile, bufsiz) < 0 ) {
      fprintf(stderr,"%s: Problem setting up file buffer on %s.\n",
        progname,argv[0]);
      retvar++;
      goto pcp_err;
    }

    if(pfio_setbufsz(outfile, bufsiz) < 0 ) {
      fprintf(stderr,"%s: Problem setting up file buffer on %s.\n",
        progname,argv[1]);
      retvar++;
      goto pcp_err;
    }

    fsize = pfio_flsz(argv[0]);

    pfio_completion_bar(fsize,totsiz,&bar_work);
    while ((nread = pfio_read(infile,buff,bufsiz)) > 0 ) {
      if( (nwrite = pfio_write(outfile,buff,nread)) != nread) {
        retvar++;
        break;
      }
      totsiz += nwrite;
      pfio_completion_bar(fsize,totsiz,&bar_work);
    }

  pcp_err:
    free(buff);
    if(pfio_close(outfile) < 0 ) retvar++;
    if(pfio_close(infile)  < 0 ) retvar++;
    if(retvar > 0 ) fprintf(stderr,"%s: Error copying file %s to %s.\n",
        progname,argv[0],argv[1]);
    return retvar;
  }

  /***** COPY UNIX FILE TO A PF FILE FUNCTION ******************/
  if(strcmp(progname,f2pf) == 0 || strcmp(progname,F2PF) == 0 ) {
    if(argc != 2) {
      fprintf(stderr,
       "%s: (copy unix to pf) Usage: $ \"%s [-e NNNN] [-i] file1 file2\".\n",
       progname, progname);
      fprintf(stderr,"Purpose: Copy file1(unix) to file2(pf)\n");
      return ++retvar;
    }
    
    if(interactive_mode) { 
      fprintf(stderr,"f2pf %s to %s?(y/n):",argv[0],argv[1]);
      if(! pftools_get_affirmation()) return retvar;
    }

    if(stat(argv[0], &stat_info)!=0) {
      fprintf(stderr,"%s: Problem getting file size of %s\n",progname,argv[0]);
      ++retvar;
      goto f2pf_err;
    }  
    fsize=stat_info.st_size;

    if((fd = fopen(argv[0],"rb")) == NULL) {
      fprintf(stderr,"%s: Problem opening %s for read.\n", progname,argv[0]);
      ++retvar;
      goto f2pf_err;
    }

    if (extsize == -1 ) extsize=256;
    extsize*=1048576;
    /* if 2 gigs, subtract 1024 bytes */
    /* if(extsize > 2147482624 )extsize = 2147482624; */

    if(pfio_set_ext_size( (long long) extsize) != 0 ) { 
      fprintf(stderr,"Unable to set the extent size of output file.\n");
      /* Try again with maximum ext size for old 32 bit linux file systems */
      extsize=2147482624;
      if(pfio_set_ext_size( (long long) extsize) != 0 ) {return ++retvar;} 
    }; 
    
    if((outfile = pfio_open(argv[1],'w'))  < 0 ) {
      fprintf(stderr,"%s: Problem opening %s for write.\n", progname,argv[1]);
      ++retvar;
      goto f2pf_err;
    }

    /* set buffer size according to file size.  If file is smaller, don't
     * set up buffering with larger buffer than the file size! */
    if(extsize < (long long) bufsiz) bufsiz = (size_t) extsize;

    if((buff = (char *) malloc((size_t) bufsiz)) == NULL) {
      fprintf(stderr,"%s: Problem allocating buffer memory.\n", progname);
      ++retvar;
      goto f2pf_err;
    } 

    if(setvbuf(fd, (char *) NULL, _IOFBF, (size_t) bufsiz) != 0 ) {
      fprintf(stderr,"%s: Problem setting up file buffer on %s.\n",
        progname,argv[0]);
      retvar++;
      goto pf2f_err;
    }

    if(pfio_setbufsz(outfile, bufsiz) < 0 ) {
      fprintf(stderr,"%s: Problem setting up file buffer on %s.\n",
        progname, argv[1]);
      retvar++;
      goto f2pf_err;
    }

    pfio_completion_bar(fsize,totsiz,&bar_work);
    
    while ((nread = fread(buff,1,bufsiz,fd)) > 0 ) {
      if( (nwrite = pfio_write(outfile,buff,nread)) != nread) {
        retvar++;
        break;
      }
      totsiz += nwrite;
      pfio_completion_bar(fsize,totsiz,&bar_work);
    }
    
  f2pf_err:
    free(buff);
    if(fclose(fd) < 0 ) retvar++;
    if(pfio_close(outfile)  < 0 ) retvar++;
    if(retvar > 0 ) fprintf(stderr,"%s: Error copying file %s to %s.\n",
        progname,argv[0],argv[1]);
    return retvar;
  }

  /***** COPY PF FILE TO A UNIX FILE FUNCTION ******************/
  if(strcmp(progname,pf2f) == 0 || strcmp(progname,PF2F) == 0 ) {
    if(argc != 2) {
      fprintf(stderr,
       "%s: (copy pf to unix) Usage: $ \"%s [-i] file1 file2\".\n",
       progname, progname);
      fprintf(stderr,"Purpose: Copy file1(pf) to file2(unix)\n");
      return ++retvar;
    }
    if(interactive_mode) { 
      fprintf(stderr,"pf2f %s to %s?(y/n):",argv[0],argv[1]);
      if(! pftools_get_affirmation()) return retvar;
    }

    if((infile = pfio_open(argv[0],'r')) < 0) {
      fprintf(stderr,"%s: Problem opening %s for read.\n", progname,argv[0]);
      ++retvar;
      goto pf2f_err;
    }
    
    fsize = pfio_flsz(argv[0]);
    if((fd = fopen(argv[1],"wb+"))  < (FILE *) 0 ) {
      fprintf(stderr,"%s: Problem opening %s for write.\n", progname,argv[1]);
      ++retvar;
      goto pf2f_err;
    }

    /* set buffer size according to file size.  If file is smaller, don't
     * set up buffering with larger buffer than the file size! */
    if(fsize < (long long) bufsiz) bufsiz = (size_t) fsize;


    if((buff = (char *) malloc((size_t) bufsiz)) == NULL) {
      fprintf(stderr,"%s: Problem allocating buffer memory.\n", progname);
      ++retvar;
      goto pf2f_err;
    } 

    if(pfio_setbufsz(infile, bufsiz) < 0 ) {
      fprintf(stderr,"%s: Problem setting up file buffer on %s.\n",
        progname,argv[0]);
      retvar++;
      goto pf2f_err;
    }

    if(setvbuf(fd, (char *) NULL, _IOFBF, (size_t) bufsiz) != 0 ) {
      fprintf(stderr,"%s: Problem setting up file buffer on %s.\n",
        progname,argv[0]);
      retvar++;
      goto pf2f_err;
    }

    pfio_completion_bar(fsize,totsiz,&bar_work);
    while ((nread = pfio_read(infile,buff,bufsiz)) > 0 ) {
      if( (nwrite = fwrite(buff,1,nread,fd)) != nread) {
        retvar++;
        break;
      }
      totsiz += nwrite;
      pfio_completion_bar(fsize,totsiz,&bar_work);
    }
    
  pf2f_err:
    free(buff);
    if(fclose(fd) < 0 ) retvar++;
    if(pfio_close(infile)  < 0 ) retvar++;
    if(retvar > 0 ) fprintf(stderr,"%s: Error copying file %s to %s.\n",
        progname,argv[0],argv[1]);
    return retvar;
  }


  /***** CHMOD FUNCTION ******************/
  if(strcmp(progname,pchmod) == 0 || strcmp(progname,PCHMOD) == 0 ) {
    if(argc < 1 ) {
      fprintf(stderr,
          "%s: (CPS I/O chmod) Usage: $ \"%s [-i] mode file\".\n",
          progname, progname);
      fprintf(stderr,"Purpose: change permissions on a file\n");
      fprintf(stderr,"   Note: mode = OCTAL# IJK where I=0-7,0-7,K=0-7\n");
      fprintf(stderr,"         I=Owner, Group, K=All and I,J, and K are\n");
      fprintf(stderr,"         each the sum of the permission bits for the\n");
      fprintf(stderr,"         respective Ownership category(all,grp,own).\n");
      fprintf(stderr,"         0=none,1=execute 2=write 4=read permission.\n");
      fprintf(stderr,"Example: $ %s 642 myfile.dat means all have read and\n",
      progname);
      fprintf(stderr,"         write perm, grp has read, owner has write.\n");
      return ++retvar;
    }

    if ((imode = strtol((const char*) argv[0],(char **)NULL,8)) == 0) {
      fprintf(stderr,"%s: Error changing permissions on file",progname);
      for (i=1;i<argc;i++,retvar++)fprintf(stderr," %s",argv[i]);
      fprintf(stderr,"\n");
      return retvar;
    }
    
    for (i=1;i<argc;i++){
      if(access(argv[i],F_OK) != 0 ) {
        fprintf(stderr,"%s: %s not found.\n",progname,argv[i]);
        retvar++;
      } else {
        if(interactive_mode) {
          fprintf(stderr,"chmod %s to %s?(y/n):",argv[i],argv[0]);
          if(pftools_get_affirmation()) {
            if(pfio_chmod(argv[i],imode) <= 0 ) {
              fprintf(stderr,"%s: Error changing permissions on file %s.\n",
              progname,argv[i]);
              retvar++;
            }
          }
        } else {
           if(pfio_chmod(argv[i],imode) <= 0 ) {
             fprintf(stderr,"%s: Error changing permissions on file %s.\n",
              progname,argv[i]);
              retvar++;
           }
        }
      }
    }
    return retvar;
  }

  /***** LOCK FILE DUMP FUNCTION ******************/
  if(strcmp(progname,lfd) == 0 || strcmp(progname,LFD) == 0 ) {
    doit=1;
    if(interactive_mode) {
      fprintf(stderr,"dump lock file?(y/n):");
      if(!pftools_get_affirmation()) doit=0;
    } 
    if(doit) pfio_dump_lock_file();
    return retvar;
  }

  /***** UNLOCK FILE FUNCTION ******************/
  if(strcmp(progname,ulf) == 0 || strcmp(progname,ULF) == 0 ) {
    if(argc == 0 ) {
      fprintf(stderr,
       "%s: (CPS Unlock file) Usage: $ \"%s [-i] file1 [file2 [file3...]]\".\n",
       progname, progname);
      fprintf(stderr,"Purpose: Remove file(s) from CPS lock file\n");
      return ++retvar;
    }
    strcpy(cps_lock_file,cnfg_get_value_c("cps_lock_file"));
    for (i=0;i<argc;i++){
      doit=1;
      if(interactive_mode) {
        fprintf(stderr,"Unlock file(%s)?(y/n):",argv[i]);
        if(!pftools_get_affirmation()) doit=0;
      }
      if(doit) {
        if(pfio_unlock_file(cps_lock_file, argv[i])==0) {
          printf("file(%s) unlocked\n",argv[i]);
        } else {
          fprintf(stderr,"Unable to unlock file(%s)\n",argv[i]);
        }
      }

    }
    return retvar;
  }
  /***** LOCK FILE FUNCTION ******************/
  if(strcmp(progname,lkf) == 0 || strcmp(progname,LKF) == 0 ) {
    if(argc == 0 ) {
      fprintf(stderr,
       "%s: (%s) Usage: \"%s [-t timeout] [-w] file1 [file2 [file3...]]\".\n",
       progname, "CPS Lock file",progname);
      fprintf(stderr,"Purpose: Insert file(s) into CPS lock file\n");
      return ++retvar;
    }
    strcpy(cps_lock_file,cnfg_get_value_c("cps_lock_file"));
    for (i=0;i<argc;i++){
      doit=1;
      if(interactive_mode) {
        fprintf(stderr,"Lock file(%s)?(y/n):",argv[i]);
        if(!pftools_get_affirmation()) doit=0;
      }        
      if(doit) {
        if(wait_mode) {
          current_lock=
           pfio_try_locking_file(cps_lock_file,argv[i], timeout,lock_type);
          if(current_lock==' ') {
            temp=0;
          } else if(current_lock=='E') {
            temp=1;
          } else {
            fprintf(stderr,"file(%s) is currently locked-waiting to lock it\n",
             argv[i]);
            pfio_set_lock_type(lock_type);
            temp=pfio_lock_file(cps_lock_file,argv[i],timeout);
          }
        } else {
          current_lock=
           pfio_try_locking_file(cps_lock_file,argv[i], timeout,lock_type);
          if(current_lock==' ') {
            temp=0;
          } else if(current_lock=='E') {
            temp=1;
          } else {
            temp=-999999;
          }
          if(interactive_mode && temp==-999999) {
            fprintf(stderr,
             "File(%s) is locked-do you want to wait to lock it?(y/n):",
             argv[i]);
            if(pftools_get_affirmation()) {
              pfio_set_lock_type(lock_type);
              temp=pfio_lock_file(cps_lock_file,argv[i],timeout);
            } else {
              temp=-999999;
            }
          }
        }
        if(temp==0 || temp==1) {
          printf("file(%s) locked\n",argv[i]);
        } else if(temp==-999999) {
          fprintf(stderr,"file(%s) locked and no_wait was specified\n",argv[i]);
        } else {
          fprintf(stderr,"Unable to lock file(%s)\n",argv[i]);
        }
      }

    }
    return retvar;
  }

  /***** REMOTE COPY FUNCTION *****************/
  if(strcmp(progname,prcp) == 0 || strcmp(progname,PRCP) == 0 ) {
    if(argc == 0 ) {
     fprintf(stderr,
     "%s: (CPS I/O File Extents) Usage: $ \"%s [-i] file1 file2\".\n",
     progname, progname);
     return ++retvar;
    }

    if(interactive_mode) {
      fprintf(stderr,"Remote copy %s to %s?(y/n):",argv[0],argv[1]);
      if(! pftools_get_affirmation()) return retvar;
    }

/*
**  action.sa_handler   = pftools_sig_handler;
**  sigemptyset(&action.sa_mask);
**  sigaddset(&action.sa_mask,SIGUSR1);
**  action.sa_flags     = SA_NODEFER;
**
**  sigemptyset(&zeroset);
**
**  if (sigaction(SIGUSR1, &action, (struct sigaction *) NULL) < 0) {
**    printf("pftoolsr: sigaction call failed for SIGUSR1\n");
**    return 1;
**  }
*/

    ifile=-1;
    while (1) {
      pfio_form_fn_ext(argv[0], ++ifile, fnin_ext);
      pfio_form_fn_ext(argv[1],   ifile, fnout_ext);
      isize=pfio_file_size(fnin_ext);
      if (isize<0) break;
      for (i=1; i<=MAX_RCP_TRIES; i++) {
        retvar = pftools_rcp(fnin_ext,fnout_ext);
        if (retvar == 0) {
          break;
        } else {
          if (i >= MAX_RCP_TRIES) {
/*
**          sigsuspend(&zeroset);
**          i = 0;
*/
            printf("Exceeded maximum tries.  Aborting\n");
          } else {
            sleep(10);
          }
        }
      }
    }

    return retvar;
  }

  if(strcmp(progname,tmpzap) == 0 || strcmp(progname,TMPZAP) == 0 ) {
    return(pftools_tmpzap(verbose_mode));
  }
  
  /***** HELP WITH PACKAGE FUNCTION *******/
  fprintf(stderr,
  "%s: (CPS File I/O tools package) Usage: make copies or links to this file\n",
  progname);
  fprintf(stderr,
    "      in your path, called: psz, pcp, prm, pmv, and pchmod.\n");
  fprintf(stderr,
    "      (Also rcp, f2pf, pf2f, ulf, tmpzap, lfd and lkf now available)\n");
  fprintf(stderr,"      Next, chmod +x on these files.  For ");
  fprintf(stderr,"help, type in the respective file name.\n");
  fprintf(stderr,"Example: $ ln -s %s ~me/bin/linux/psz\n",progname);
  fprintf(stderr,"         $ chmod +x ~me/bin/linux/psz\n");
  fprintf(stderr,"         $ psz (to get help on the psz function)\n");
  return ++retvar;
}
