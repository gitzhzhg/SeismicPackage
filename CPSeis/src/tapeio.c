/*<CPS_v1 type="AUXILIARY_FILE" PRETAG="!" />
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
!!<brief_doc>
!-------------------------------------------------------------------------------
!                        C P S   P R I M I T I V E
!
! Name       : tapeio
! Category   : io
! Written    : 2000-06-24   by: R.S.Day
! Revised    : 2004-01-21   by: R Selzler
! Maturity   : production
! Purpose    : Handle basic io operations for magnetic tape(open,close,r/w)
! Portability: unix io, On Sun platforms include -lposix4 in library list
!
!-------------------------------------------------------------------------------
!</brief_doc>
!
!!<descript_doc>
!-------------------------------------------------------------------------------
!                         GENERAL DESCRIPTION
!
!!!  The routines in this file are an interface to the unix ioctl commands
!!!  that control magnetic tape device drivers. At present only TTRIN and TTROT
!!!  use these routines, but they are designed for more general usage.
!
!-------------------------------------------------------------------------------
!</descript_doc>

!
!<calling_doc>
!
!                              i
!  long tapeio_load_open(char *tapelib,
!         b             i           i             i
!   char *volser, char *mode, char *devtyp, char *user)
!    - loads and opens a tape device.
!    tapelib= path to the tapelib program
!    volser = a 6 byte tape label. If volser="SCR" and op=w and a tape is
!             successfully mounted, then volser will return the tape label.
!    mode   = w or r  for write or read-only tape io.
!    devtyp = the device or media type(i.e. 3590,8mm,dlt,nr3590)
!    user   = logon id of the user wanting a tape loaded
!             (can be obtained from getenv("USER") )
!    return values: -1 if there is an error, or a file descriptor if OK.
!
!                             b            i         i             o
!  long tapeio_tapelib(char * tapelib, char *volser,char *op, char *devtyp,
!        i             i
!  char *devnam  char *user)
!    - executes an IBM3494 tape operation such as a mount or dismount.
!    tapelib= path to the tapelib program
!    volser = a 6 byte external tape label. If volser="SCR" and op=m
!             and a tape is successfully mounted, then volser will return
!             the external tape label.
!    op     = a command that is passed to the tapelib program
!      i.e. q(query),qa(query all),u(unload),m(mount)
!    devtyp = the device or media type(i.e. 3590,8mm,dlt,nr3590)
!    devnam = the return value of the physical device name. This
!             parameter is needed for the tapeio_open command.
!    user   = logon id of the user wanting a tape loaded
!             (can be obtained from getenv("USER") )
!    return values: -1 if there is an error
! 
! ***ALL CALLS BELOW ARE INDEPENDENT OF THE IBM3494***
!                          i            i             i
!  long  tapeio_open(char *volser,char *devnam, char *mode)
!    - Open the tape drive given by devnam(e.g. /dev/rmt/3stn)
!    devnam = the value of the physical device name.
!    mode   = w or r  for write or read-only tape io.
!    return values: -1 on error, or a file descriptor > 2 if successful.
!
!                          b
!  long tapeio_close(long *fd)
!    - Close the tape unit opened by tapeio_open and set fd = -1
!    fd     = the file descriptor returned from the tapeio_open command
!    return values: -1 on error, or 0 on success
!
!                           i        i          o
!  long  tapeio_read (long *fd, long *nby, char *buf)
!    - Read from tape unit opened with file descriptor fd.
!    fd     = the file descriptor returned from the tapeio_open command
!    nby    = the maximum number of bytes to read.
!    buf    = the buffer to recieve the tape record
!    return values: -1 is an error condition,
!                    0 is an EOF encountered
!                   >0 is the number of bytes read
!
!                           i        i          i
!  long  tapeio_write(long *fd, long *nby, char *buf)
!    - Write to tape unit opened with file descriptor fd.
!    fd     = the file descriptor returned from the tapeio_open command
!    nby    = the number of bytes to write from buf.
!    buf    = the buffer containing the data to write to a tape record
!    return values: -1 is an error condition,
!                  >=0 is the number of bytes written
!
!                            i         o
!  void tapeio_get_vol(long *fd, char *volser)
!    - return the internal tape label of a labeled tape.6 bytes or less
!    - valid only after a successful open of an old labeled tape
!    fd     = the file descriptor returned from the tapeio_open command
!    volser = the tape label of the tape associated with fd.
!
!                          i
!  long  tapeio_next(long *fd)
!    - rewind current tape and loads next tape(MTIOCOP:MTLOAD)
!    fd     = the file descriptor returned from the tapeio_open command
!    return values: -1 is an error condition, 0 is OK
!
!                         i
!  long tapeio_dmnt(long *fd)
!    - rewinds and unloads the tape via ioctl(MTIOCTOP: MTREW,MTOFFL)
!      Issues an tapeio_close after the rewind and unload!!!
!    fd     = the file descriptor returned from the tapeio_open command
!
!                           i
!  long tapeio_rewind(long *fd)
!    - rewinds the tape drive via an ioctl(MTIOCTOP: MTREW)
!    fd     = the file descriptor returned from the tapeio_open command
!    return values: -1 on error, or 0 on success
!
!  void tapeio_sprint(long *fd, char *obuf)
!  void tapeio_print(long *fd)
!    - Print tape status to standard out or to a buffer
!      i.e. label, file and record position, bytes in last read/write
!    fd     = the file descriptor returned from the tapeio_open command
!    obuf   = returns the tape status in a buffer(at least 512 bytes)
!
!                               i        i
!  long  tapeio_skip_mark(long *fd, long count)
!    - skip files on a tape via an ioctl(MTIOCTOP:MTFSF)
!    fd     = the file descriptor returned from the tapeio_open command
!    count  = number of files to skip
!      count = 0 positions to beginning of current file.
!      count = 1 positions to just after EOF(assuming EOF read attempt).
!    return values: the number of files skipped or -1 on error
!
!                              i        i
!  long  tapeio_skip_blk(long *fd, long count)
!    - skip physical records on a tape via an ioctl(MTIOCTOP:MTFSR || MTBSR)
!    fd     = the file descriptor returned from the tapeio_open command
!    count  = number of records to skip(count can be either + or -)
!    return values: the number of records skipped or -1 on error
!
!                          i         i
!  long  tapeio_weof(long *fd, long *count)
!    - write count EOF marks to tape via an ioctl(MTIOCTOP:MTWEOF)
!    - A zero count will just flush all the buffers and will not
!      write  any  file  marks.
!    return values: the number of EOFS written or -1 on error
!
!                                 i
!  long tapeio_process_eofs(long *fd)
!    - Takes action when an eof is detected after a tapeio_read
!    - Returns -1 if there is an error
!
!  long tapeio_tell(long *fd, int *fileno, int * blkno)
!    - Returns the current file number and block number within the file
!    - blkno/fileno start from 0 and are incremented at each EOF/record

!    return values: 0 if OK or -1 on error
!</calling_doc>
!--------------------------"module" start ----------------------------------

!<history_doc>
!--------------------------------------------------------------------------
!                          REVISION HISTORY
!     Date        Author       Description
!     ----        ------       -----------
! 20. 2004-01-21  R Selzler    Resolved SGI compiler warnings.
! 19. 2003-09-29  R.S.Day      Forced rewrite of existing label when
!                              drive density is different.
! 18. 2003-07-30  R.S.Day      tapeio_log_wr added to handle messages.
!                              user argument added to tapeio_open_base 
!                              for tape labels.
!                              Tape label writing has been turned on.
! 17. 2003-06-10  Goodger      Change vfork to fork.
! 16. 2003-05-27  R.S.Day      Fixes for the sgi compiler.
! 15. 2003-02-14  R.S.Day      Improved the way tapeio_open deals with
!                              tapes having label only..
! 14. 2003-02-10  R.S.Day      Altered arguments of tapeio_load_open and
!                              tapeio_tapelib.
! 13. 2003-01-30  R.S.Day      EINTR logic added to rd_label and wr_label.
!                              tapeio_open mods for linux and LTO support.
!                              Reduced MAXFD to 64. Added tapeio_open_lab
!                              for optional tape label writes.
! 12. 2002-10-10  Ed Schmauch  Included tapecnfg.h.
! 11. 2001-04-04  R.S.Day      restart open call if errno==EINTR. Signal
!                              handler may interupt the open. Messages from
!                              tapeio_open converted to unbuffered io.
! 10. 2000-11-27  R.S.Day      tapeio_dmnt now closes out a channell even if
!                              the rewind fails. This only seems to happen
!                              when the tape is already off the drive. In
!                              this case the channel should be reset.
!  9. 2000-10-25  R.S.Day      More precise clock used on SOLARIS. On solais
!                              platforms include -lposix4. Restart read or 
!                              write if errno==EINTR on interrupted call.
!                              More careful treatment of string variables
!                              in tapeio_get_vol, tapeio_sprint. More checks
!                              for integrity of stored data.
!  8. 2000-10-02  R.S.Day      Added argument, user, to tapeio_tapelib and
!                              tapeio_load_open. Use a userid in order to
!                              tie tape categories to userid.
!  7. 2000-09-28  R.S.Day      Added crude read/write rate monitor that can be
!                              turned on with the verbose switch(This is
!                              for all channels).
!  6. 2000-08-31  R.S.Day      Rearranged the order of the doc. sections
!                              Added a verbose switch to control output to 
!                              stdout. Added logic in tapeio_process_eofs
!                              to handle 80 byte records trailing EOFs.
!  5. 2000-08-21  R.S.Day      Added function tapeio_tell. Increased possible
!                              buffer size. Removed tapeio_tapelib call in
!                              tapeio_dmnt. It was unnecessary and will cause
!                              problems for socket based tape io. Modified
!                              tapeio_print and added tapeio_sprint.
!  4. 2000-08-01  R.S.Day      changed %d to %ld in printf statements to
!                              suppress warnings about long treated as int
!  3. 2000-08-01  R.S.Day      Converted file descriptor in arguments to
!                              long to be consistent with pfio. Converted
!                              some void functions to type long for better
!                              status checking. Passed volser to tapeio_open
!                              call which eliminates need to pass it to
!                              the tapeio_dmnt call. 
!  2. 2000-07-25  R.S.Day      Documentation changes. Removed the stub
!                              for tape catalogueing. Fixed a potential
!                              problem with tapeio_close. Changed the
!                              path for the location of tapelib. Altered
!                              the default display parameter for operator
!                              notification.
!  1. 2000-06-27  R.S. Day     Initial version.
!--------------------------------------------------------------------------
!</history_doc>
!
!<portability_doc>
!--------------------------------------------------------------------------
!                        PORTABILITY LIMITATIONS
! 1. This code is specific to Unix operating systems.
!    Also see man page for mtio, or sys/mtio.h. 
! 2. tapeio_tapelib will attempt to invoke the tapelib program, which
!    is currently only on node pospt1
! 3. When on a Solaris platform you must include -lposix4 in the list
!    of librarys to be linked against.
!--------------------------------------------------------------------------
!</portability_doc>
!<compile_doc>
!--------------------------------------------------------------------------
!                     SPECIAL COMPILING REQUIREMENTS
! No special requirements
!--------------------------------------------------------------------------
!</compile_doc>
*/
#include <string.h>
#include <stdio.h>
#include <stdlib.h>
#include <time.h>
#include <sys/types.h>
#include <sys/wait.h>
#include <sys/stat.h>
#include <sys/ioctl.h>
#include <sys/mtio.h>
#include <sys/uio.h>
#include <unistd.h>
#include <fcntl.h>
#include <time.h>
#include <signal.h>
#include <errno.h>

#include "tapeio.h"


#define MTIO_WRITE 0
#define MTIO_READ  1
#define MAXFD 64

static tapeio_dat *MTA[MAXFD];
static int tapeio_verbose;
static float rdrate;
static float wrrate;
static char wmsg[160];
static char emsg[160];
void tapeio_log_wr(char *msg);
/*
static FILE *fpntr;
static char sbuf[131072];
void tapeio_stream_close();
*/


/*
 * volser = volser identifier for a tape
 * op     = code for a tape operation that we want performed
 *          m - mount a tape
 *          u - unmount a tape
 *          q - query the tape library for status of a volser
 * user   = logon name
 * if mount is successful, devnam will contain the device name.
 */
long tapeio_tapelib(char *tapelib,
     char *volser,char *op,char *devtyp, char *devnam, char *user)
{
 int   nbuf=0,maxbuf=8192;
 char  *buff=0,*devpntr=0,*volpntr=0,scrvol[16];
 int    nrd,nr,ntord;
 int    pid,pipefds[2],status;
 char   lhost=64;
 char   *argv[14];
 char   VOL[8],OP[8],DISPLAY[32],BTIME[8];
 char   USER[16],DEVTYP[32];
 char   display[32],btime[16];
 char   prog[32],host[64];
 static char default_user[16];
 struct stat statbuf;
 int    host_ok;

 strcpy(prog, tapelib);
 strcpy(VOL,"-vol");
 strcpy(OP,"-op");
 strcpy(DISPLAY,"-display");
 strcpy(BTIME,"-ti");
 strcpy(USER,"-u");
 strcpy(DEVTYP,"-devtyp");
 strcpy(display,":0");
 strcpy(btime,"240000"); /* only used for operator notifications */
 argv[0]=prog;
 argv[1]=VOL;
 argv[2]=volser;
 argv[3]=OP;
 argv[4]=op;
 argv[5]=DISPLAY;
 argv[6]=display;
 argv[7]=BTIME;
 argv[8]=btime;
 argv[9]=USER;
 strcpy(default_user,"sps");
 argv[10]=user;
 if(!user) argv[10]=default_user;
 if(strlen(devtyp)==0) {
   argv[11]=0;
 } else {
   argv[11]=DEVTYP;
   argv[12]=devtyp;
   argv[13]=0;
 }
 devnam[0]='\0';
 scrvol[0]='\0';
/*
 * Check that we are on a valid host
 */
 status = gethostname(host,lhost);
 if(status == -1) return -1;

 host_ok = 1;
/*
 for (host_ok = i = 0; !host_ok && (i < tapecnfg_num_tape_type_nodes); i++)
   for (j = 0; !host_ok && (j < tapecnfg_num_nodes[i]); j++)
     if (!strcmp(host, tapecnfg_tape_node[i][j]))
       host_ok = 1;
*/

 if (!host_ok)
   return -1;
/*
 * Check device type. defaults to 3590s on the ibm3494
 */
 if(strlen(devtyp)==0) {
 }
/*
 * create the pipes
 */
 if(pipe(pipefds) < 0) {
   perror("#tapeio_tapelib: bad pipes - call the plumber");
   sprintf(emsg,"tapeio_tapelib: failure to create pipes");
   tapeio_log_wr(emsg);
   return -1;
 }
/*
 * fork a child process
 */
 if( (pid=fork()) < 0 ) {
   perror("#tapeio_tapelib: fork failed - try chopsticks,");
   sprintf(emsg,"tapeio_tapelib: failure to fork");
   tapeio_log_wr(emsg);
   return -1;
 }
/*
 * The child executes the command cmd
 * (don't background it since we want to capture its standard out)
 * Make childs stdout the pipes stdout
 */
 if(pid==0) {
   close(1);
   dup(pipefds[1]);
   execv(argv[0],argv);
   exit(1);
 }
/*
 * hang until child completes
 */
 while(wait(&status) != pid) {
 }
/*
 * Check to see how much data needs to be read from the pipe
 * Looks for return strings #tapelib_tpld:MT and devnam=
 */
 if( fstat(pipefds[0], &statbuf) < 0) {
   sprintf(emsg,"tapeio_tapelib: fstat failure");
   tapeio_log_wr(emsg);
   close(pipefds[0]);
   close(pipefds[1]);
   return -1;
 }
 ntord = statbuf.st_size;
 if(ntord <=0) {
   sprintf(emsg,"tapeio_tapelib: ntord<=0");
   tapeio_log_wr(emsg);
 }
/*
 sprintf(line,"tapeio_tapelib: Total number of bytes to read=%d\n",ntord);
 printf("%s",line);
 nwr = write(1,line, strlen(line)+1);
 */
 nbuf = (ntord < maxbuf) ? ntord :maxbuf;
 buff = (char *) malloc(nbuf+1);
 buff[0]='\0';
 nrd=0;
 while(nrd < ntord) {
   nr = (ntord-nrd<nbuf) ? ntord-nrd : nbuf;
   nrd += read(pipefds[0],buff,nr);
   buff[nr]='\0'; 
   printf("buff=%s\n",buff);
   devpntr = strstr(buff,"#tapelib_tpld:MT");
   if(devpntr) {
    volpntr = strstr(buff,"volser=");
    devpntr = strstr(devpntr+17,"devnam=");
    if(devpntr) {
     sscanf(devpntr+7,"%s",devnam);
    }
    if(volpntr) {
     memcpy(scrvol,volpntr+7,6);
     scrvol[6]='\0';
     if(strncmp(volser,"SCRATCH",3)==0) strcpy(volser,scrvol);
    }
   }
   
 }
 if(strlen(devnam)==0 && strcmp(op,"qa") != 0) {
   sprintf(emsg,"tapeio_tapelib: tape failure(op=%s) for volser=%s",
   op,volser);
   tapeio_log_wr(emsg);
   sprintf(wmsg,"tapeio_tapelib: see tapelib.log file for details");
   tapeio_log_wr(wmsg);
 } else {
   sprintf(wmsg,"tapeio_tapelib: devnam=%s op=%s",devnam,op);
   tapeio_log_wr(wmsg);
 }
 close(pipefds[0]);
 close(pipefds[1]);
 if(buff) free(buff);
 return nrd;
}

long tapeio_open(char *volser, char *devnam, char *mode)
{ /* assumes the labeled tape volser is mounted */
 int label = 1;
 char user[8];
 strcpy(user,"sps");
 return tapeio_open_base(volser, devnam, mode, &label,user);
}

long tapeio_open_lab(char *volser, char *devnam, char *mode, int *label)
{ /* assumes the labeled tape volser is mounted */
 char user[8];
 strcpy(user,"sps");
 return tapeio_open_base(volser, devnam, mode, label,user);
}

long tapeio_open_base(char *volser, char *devnam, char *mode, int *lab_tape,
     char *user)
{int  fileno,blkno;
 long fd,i_err;
 tapeio_dat *mt=0;
 char luser[16];
 char pdn[32];
 pid_t pid;
 struct mtop mt_command;
 if(!devnam) {
   sprintf(emsg,"tapeio_open: ERROR - devnam is null");
   tapeio_log_wr(emsg);
   return -1;
 }
 if(strlen(devnam)==0) {
   sprintf(emsg,"tapeio_open: ERROR - blank devnam");
   tapeio_log_wr(emsg);
   return -1;
 }
 if(strlen(user)>14) {
   strncpy(luser,user,14);
   luser[14]='\0';
 } else {
   strcpy(luser,"sps");
   if(strlen(user) > 0) strcpy(luser,user);
 }
 if(tapeio_verbose>0) {
   if(*lab_tape==1 ) sprintf(wmsg,"tapeio_open: flag for labeled tape");
   if(*lab_tape==0 ) sprintf(wmsg,"tapeio_open: flag for non-labeled tape");
   tapeio_log_wr(wmsg);
 }
 pid = getpid();
op_restart:
 if(mode[0]=='w' || mode[0]=='W') {
   fd = open(devnam, O_RDWR );
 } else {
   fd = open(devnam, O_RDONLY );
 }
 if(fd < 0) { /* deal with open error */
   if(errno==EINTR) goto op_restart;
   sprintf(emsg,"#tapeio_open:(%d)",pid);
   perror(emsg);
   sprintf(emsg,"tapeio_open: errno=%d devnam=%s",errno, devnam);
   tapeio_log_wr(emsg);
   sprintf(emsg,"tapeio_open: mode=%s volser=%s",mode, volser);
   tapeio_log_wr(emsg);
   if(errno == EACCES) {
     sprintf(emsg,"tapeio_open: EACCES , TAPE IS WRITE LOCKED?");
     tapeio_log_wr(emsg);
   }
   return -1;
 }
 if(fd > MAXFD) {
   sprintf(emsg,"tapeio_open: fd > %d",MAXFD);
   tapeio_log_wr(emsg);
   close(fd);
   return -1;
 }

 /* set IO for variable record length */
#ifdef sun
  mt_command.mt_op = MTSRSZ;
#else
#ifndef sgi
  mt_command.mt_op = MTSETBLK;
#endif
#endif
  mt_command.mt_count = 0;
  i_err = ioctl(fd, MTIOCTOP, &mt_command);
  if(i_err < 0) {
    sprintf(emsg,"tapeio_open: ioctl for MTSETBLK failed");
    tapeio_log_wr(emsg);
    close(fd);
    return -1;
  }
 mt = (tapeio_dat *) malloc(sizeof(tapeio_dat));
 if(!mt) {
   sprintf(emsg,"tapeio_open: bad allocate");
   tapeio_log_wr(emsg);
   close(fd);
   return -1;
 }

 MTA[fd] = mt;
 mt->volser[0]='\0';
 if(strlen(volser)<7) strcpy(mt->volser,volser);
 strcpy(mt->devnam,devnam);
 strcpy(mt->type,"UNKNOWN");
 strcpy(mt->pdn,"UNKNOWN");
 mt->opmode=MTIO_READ;
 if(mode[0]=='w') mt->opmode=MTIO_WRITE;
 mt->nrd  = 0;
 mt->lnl  = 0;
 mt->trcnt= 0;
 mt->fd   = fd;
 mt->blkno=0;
 mt->fileno=0;
 mt->datafile=1;
 mt->datablk=0;
 mt->label[0][0]='\0';
 mt->label[0][4]='\0';
 mt->label[1][0]='\0';
 mt->label[2][0]='\0';
/*
 if(!fpntr ) {
   fpntr = fdopen(mt->fd,"r");
   if(fpntr) i_err = setvbuf(fpntr,sbuf,_IOFBF,131072);
   if(i_err != 0) {
   }
 }
*/
 i_err = tapeio_status(&fd,&fileno,&blkno);
 if(i_err >= 0) {
   mt->fileno = fileno;
   mt->blkno  = blkno;
 }
 if(*lab_tape == 0 && mt->opmode==MTIO_WRITE)  return fd;

 /* check the labels which should be on the tape */
 i_err = tapeio_rd_label(fd);
 if(tapeio_rdorwr(&fd)==MTIO_READ) {
   if(i_err <= 0 ) {
     if(mt->lnl> 1) {
       sprintf(wmsg,
       "tapeio_open: WARNING - TAPE LABEL WITH NO EOF");
       tapeio_log_wr(wmsg);
       *lab_tape=1;
       return fd;
     } 
     if(fd > 0) {
       sprintf(wmsg,
       "tapeio_open: WARNING - TAPE OPEN BUT NO LABEL");
       tapeio_log_wr(wmsg);
       *lab_tape=0;
       return fd;
     } else {
       sprintf(emsg,
       "tapeio_open: ERROR - OPEN FAILED ON READ TAPE");
       tapeio_log_wr(emsg);
     }
     *lab_tape=0;
     if(i_err < 0) {
       i_err = tapeio_close(&fd);
       fd = -1;
       mt=0;
     }
   }
 } else {
   if(i_err <= 0) {
     if(mt->lnl> 1) {
       sprintf(wmsg,
       "tapeio_open: WARNING - TAPE LABEL WITH NO EOF");
       tapeio_log_wr(wmsg);
       return fd;
     } 
     sprintf(wmsg,
     "#tapeio_open: WARNING-NO TAPE LABELS ON WRITE TAPE");
     tapeio_log_wr(wmsg);
     if(i_err < 0) {
       sprintf(wmsg,"tapeio_open: bad error reading label");
       tapeio_log_wr(wmsg);
     }
     /* if we get here lab_tape=1 */
     if(*lab_tape==1) {
       strcpy(pdn,"UNKNOWN");
       i_err = tapeio_rewind(&fd);
       if(i_err < 0) {
         i_err = tapeio_close(&fd);
         fd = -1;
         mt=0;
         sprintf(emsg,"tapeio_open: error in rewind");
         tapeio_log_wr(emsg);
       }
       sprintf(wmsg,"tapeio_open: write missing label");
       tapeio_log_wr(wmsg);
       i_err = tapeio_wr_label(&fd,volser, luser, pdn);
       if(i_err < 0) {
         i_err = tapeio_close(&fd);
         fd = -1;
         mt=0;
         sprintf(emsg,"tapeio_open: error in wr_label");
         tapeio_log_wr(emsg);
       }
     }
   } else { /* there is an old label */
        
     sprintf(emsg,"tapeio_open_base: rewrite old label user=%s",luser);
     tapeio_log_wr(emsg);
     strcpy(pdn,"UNKNOWN");
     if(tapeio_rewind(&fd) < 0) {
       sprintf(emsg,"tapeio_open_base: rewind failed for vol=%s",volser);
       tapeio_log_wr(emsg);
       i_err = tapeio_close(&fd);
       fd = -1;
     }
     if(tapeio_wr_label(&fd,volser, luser, pdn) < 0) {
       sprintf(emsg,"tapeio_open_base: WARNING-label write failed for vol=%s"
       ,volser);
       tapeio_log_wr(emsg);
     }
   }
 }
 return fd;
}

long tapeio_close(long *fd)
{int  i_err;
 tapeio_dat *mt;
 if(*fd < 0 || *fd >= MAXFD) return 0;
 mt = MTA[*fd];
 if(!mt) return -1;
 if (mt->fd != *fd) {
   sprintf(emsg,"tapeio_close: stored fd=%ld != passed fd=%ld",mt->fd,*fd);
   tapeio_log_wr(emsg);
   return -1;
 }
 if(mt->fd < 0) return 0;
 i_err = close(mt->fd); 
 if(i_err < 0) {
   perror("tapeio_close:");
   sprintf(emsg,"tapeio_close: fd=%ld errno=%d",mt->fd, errno);
   tapeio_log_wr(emsg);
   return -1;
 }
 MTA[*fd]=0;
 free(mt);
 *fd = -1;
 return 0;
}

long tapeio_read(long *fd, long *nby, char *buf)
{ssize_t nrd;
 tapeio_dat *mt;
 int i_err, fileno,blkno;
 static long ntot;
 static time_t t1,t2;
 float delta;
 static float telaps;
#ifdef sun
   static int wsec1,nsec1;
   static int wsec2,nsec2;
   struct timespec  sunt;
#endif


 if(*fd < 0 || *fd >= MAXFD) return -1;
 mt = MTA[*fd];
 if(!mt) return -1;
 if (mt->fd != *fd) {
   sprintf(emsg,"tapeio_read: stored fd=%ld != passed fd=%ld",mt->fd,*fd);
   tapeio_log_wr(emsg);
   return -1;
 }
 if(mt->fd < 0 || *nby <=0 ) return -1;
#ifdef sun
   i_err = clock_gettime(CLOCK_REALTIME, &sunt);
   wsec1 =  sunt.tv_sec;
   nsec1 =  sunt.tv_nsec;
#endif
 if(ntot==0) {
   t1 =  time(NULL);
 }
 rd_restart:
 nrd = read(mt->fd,buf, *nby);

/* process errors, but restart interrupted system calls */
 if(nrd < 0) {
   if(errno==EINTR) goto rd_restart;
   perror("tapeio_read:");
   sprintf(emsg,"tapeio_read: errno %d", errno);
   tapeio_log_wr(emsg);
   i_err = tapeio_status(fd,&fileno, &blkno);
   i_err ++; /* supress sgi warning */
   return -1;
 }

/* return value of 0 indicates an eof */
 if(nrd == 0) {
   return 0;
 }

 mt->nrd = nrd;
/*
 nc = sizeof(mt->buf);
 if(nc >= nrd) {
   memcpy(mt->buf,buf,mt->nrd);
 }
*/
 mt->blkno++;
#ifdef sun
   i_err = clock_gettime(CLOCK_REALTIME, &sunt);
   wsec2=  sunt.tv_sec;
   nsec2=  sunt.tv_nsec;
   telaps += (wsec2-wsec1) +1.0e-9*(nsec2-nsec1);
#endif
 if(ntot < 10000000) {
   ntot += nrd;
 } else {
  t2 =  time(NULL);
  delta = difftime(t2,t1);
  if(telaps>0) delta=telaps;
  rdrate = ntot/delta;
  if(tapeio_verbose > 0) {
   sprintf(wmsg,"tapeio_read: bytes=%ld del_t=%f rate=%f by/sec",
   ntot,delta,rdrate);
   tapeio_log_wr(wmsg);
  }
  telaps=0;
  ntot = 0;
 }
 return (long) nrd; 
}

long tapeio_write(long *fd, long *nby, char *buf)
{ssize_t nwr;
 tapeio_dat *mt;
 int i_err, fileno,blkno;
 static long ntot;
 static time_t t1,t2;
 float delta;
 static float telaps;
#ifdef sun
   static int wsec1,nsec1;
   static int wsec2,nsec2;
   struct timespec  sunt;
#endif

 if(*fd < 0 || *fd >= MAXFD) return -1;
 mt = MTA[*fd];
 if(!mt) return -1;
 if (mt->fd != *fd) {
   sprintf(emsg,"tapeio_write: stored fd=%ld != passed fd=%ld",mt->fd,*fd);
   tapeio_log_wr(emsg);
   return -1;
 }
 if(mt->fd < 0 || *nby <=0 ) return -1;
#ifdef sun
   i_err = clock_gettime(CLOCK_REALTIME, &sunt);
   wsec1 =  sunt.tv_sec;
   nsec1 =  sunt.tv_nsec;
#endif
 wr_restart:
 nwr = write(mt->fd,buf, *nby);
/* process errors */
 if(nwr < 0) {
   if(errno==EINTR) goto wr_restart;
   perror("tapeio_write:");
   sprintf(emsg,"tapeio_write: errno %d", errno);
   tapeio_log_wr(emsg);
   i_err = tapeio_status(fd,&fileno, &blkno);
   i_err ++; /* supress sgi warning */
   return -1;
 }
#ifdef sun
   i_err = clock_gettime(CLOCK_REALTIME, &sunt);
   wsec2=  sunt.tv_sec;
   nsec2=  sunt.tv_nsec;
   telaps += (wsec2-wsec1) +1.0e-9*(nsec2-nsec1);
#endif


 if(nwr == 0) {
   return 0;
 }

 if(ntot==0) {
   t1 =  time(NULL);
 }
 if(ntot < 10000000) {
   ntot += nwr;
 } else {
  t2 =  time(NULL);
  delta = difftime(t2,t1);
  if(telaps>0) delta=telaps;
  wrrate = ntot/delta;
  if(tapeio_verbose > 0) {
    sprintf(wmsg,"tapeio_write: bytes=%ld del_t=%f wrrate=%f by/sec",
    ntot,delta,wrrate);
    tapeio_log_wr(wmsg);
  }
  ntot = 0;
  telaps =0;
 }
 mt->blkno++;
 return (long) nwr; 
}

/* Given a file descriptor return the tapeio_dat structure
 * PRIVATE
 */
tapeio_dat *tapeio_dat_get(long fd)
{ if(fd < 0 || fd>= MAXFD) return 0;
 if(!MTA[fd]) return 0;
 return MTA[fd];
}

/* Read 80 byte tape label records at tape beginning.
 * PRIVATE
 */
long tapeio_rd_label(long fd)
{int n;
 long fdloc = fd;
 int i_err;
 int i;
 tapeio_dat *mt;
 if(fd < 0 || fd>= MAXFD) return -1;
 mt = MTA[fd];
 if(!mt) {
   sprintf(emsg,"tapeio_rd_label: error - null mt");
   tapeio_log_wr(emsg);
   return -1;
 }
 if(mt->fd < 0) {
   sprintf(emsg,"tapeio_rd_label: mt->fd<0");
   tapeio_log_wr(emsg);
   return -1;
 }
 if(mt->fd != fd) {
   sprintf(emsg,"tapeio_rd_label: stored fd=%ld != passed fd=%ld",
   mt->fd,fd);
   tapeio_log_wr(emsg);
   return -1;
 }

 mt->lnl = 0;
 mt->label[0][0]='\0';
 mt->label[1][0]='\0';
 mt->label[2][0]='\0';
/* process tape labels */
 for(i=0;i<3;i++) {
   restart_rd_label:
   mt->nrd = read(fd,mt->buf,sizeof(mt->buf) );
   if(mt->nrd <  0) {
     if(errno==EINTR) goto restart_rd_label;
     perror("#tapeio_rd_label:");
     sprintf(emsg,"tapeio_rd_label: fd=%ld errno=%d",mt->fd, errno);
     tapeio_log_wr(emsg);
     return -1;
   }
   if(mt->nrd == 80) {
     memcpy(mt->label[mt->lnl],mt->buf,80);
     mt->label[mt->lnl][80]='\0';
     if(tapeio_verbose==1)  {
      sprintf(wmsg,"tapeio_rd_label: label(%d)=%s",
      mt->lnl,mt->label[mt->lnl]);
      tapeio_log_wr(wmsg);
     }
     mt->lnl++;
   }
   if(mt->nrd==0 || mt->nrd !=80) break;
   mt->blkno++;
 }


 sprintf(emsg,"tapeio_rd_label: mt->nrd=%d label count=%d",mt->nrd,mt->lnl);
 tapeio_log_wr(emsg);
 if(mt->nrd == 0) { /* hit a tape mark */
   i_err = tapeio_process_eofs(&mt->fd);
   if(i_err < 0) return 0;
 } else {
   n = tapeio_skip_blk(&fdloc,-1);
   if(n < 0) {
      sprintf(emsg,"tapeio_rd_label: skip_blk failed");
      tapeio_log_wr(emsg);
   }
 }
 return mt->lnl;
}

/* Write 80 byte tape label records at tape beginning.
 * PRIVATE
 */
long tapeio_wr_label(long *fd,char *vol, char *userid, char *pdn)
{tapeio_dat *mt;
 time_t tim;
 struct tm *gmt;
 int  pdnlen,seqno=1,fseqno=1,iday,iyr;
 int  blklen=32000,reclen=32000;
 char cdate[12],edate[12],sdate[32],*pdnpntr;
 int  i,nwr;


 if(*fd < 0 || *fd>= MAXFD) return -1;
 mt = MTA[*fd];
 if(!mt) {
   sprintf(emsg,"tapeio_wr_label: error - null mt");
   tapeio_log_wr(emsg);
   return -1;
 }
 if(mt->fd < 0) {
   sprintf(emsg,"tapeio_wr_label: mt->fd<0");
   tapeio_log_wr(emsg);
   return -1;
 }
 if(mt->fd != *fd) {
   sprintf(emsg,"tapeio_wr_label: stored fd=%ld != passed fd=%ld",
   mt->fd,*fd);
   tapeio_log_wr(emsg);
   return -1;
 }

 tim = time(NULL);
 gmt = gmtime(&tim);
 if(gmt) { /* build creation-expiration dates */
   iday = gmt->tm_yday;
   strftime(sdate,24,"%Y %j",gmt);
   sscanf(sdate,"%d %d",&iyr,&iday);
   sprintf(cdate,"0%02d%03d",(iyr-2000)/100,iday);
   strcpy(edate,sdate);
 }

 mt->label[0][0]='\0';
 mt->label[1][0]='\0';
 mt->label[2][0]='\0';
 memset(mt->label[1],' ',80);
 sprintf(mt->label[0],
   "VOL1%-33s%-14s                             ",vol,userid);
 pdnpntr=pdn;
 pdnlen = strlen(pdn);
 if(pdnlen > 17) pdnpntr = pdn + (pdnlen-17);
 sprintf(mt->label[1],
   "HDR1%-17s      %04d%04d      %6s%6s %06dCONPHI              ",
   pdnpntr,seqno,fseqno,cdate,edate,0);
 sprintf(mt->label[2],"HDR2 %05d%05d%65s",blklen,reclen," ");
/* process tape labels */
 mt->label[0][80]='\0';
 mt->label[1][80]='\0';
 mt->label[2][80]='\0';
 mt->lnl=0;
 for(i=0;i<2;i++) {
   restart_wr_label:
   nwr = write(mt->fd,mt->label[i], 80);
   if(nwr < 0) {
     perror("#tapeio_wr_label:");
     if(errno==EINTR) goto restart_wr_label;
     sprintf(emsg,"tapeio_wr_label: error %d,%s",
     errno, mt->label[1]);
     tapeio_log_wr(emsg);
     break;
   }
   if(tapeio_verbose>0) {
     sprintf(wmsg,"tapeio_wr_label:%d\n\tlabel=%s",i,mt->label[i]);
     tapeio_log_wr(wmsg);
   }
   mt->lnl++;
   mt->blkno++;
 }
 return mt->lnl;
}

/*
 * Called after read returns 0 bytes
 * process file marks 
 */
long tapeio_process_eofs(long *fd)
{int n,  neof=0, i_err;
 unsigned char lbuf[82];
 int fileno,blkno;
 tapeio_dat *mt;
 if(*fd < 0 || *fd>= MAXFD) return -1;
 mt = MTA[*fd];
 if(!mt) return -1;
 if (mt->fd != *fd) {
   sprintf(emsg, "tapeio_process_eofs: stored fd=%ld != passed fd=%ld",
   mt->fd,*fd);
   tapeio_log_wr(emsg);
   return -1;
 }
 neof ++;
 if(tapeio_verbose==1) {
   sprintf(wmsg,"tapeio_process_eofs: Single EOF?");
   tapeio_log_wr(wmsg);
 }
 i_err = tapeio_status(fd,&fileno, &blkno);
 if(i_err < 0) {
   sprintf(emsg,"tapeio_process_eofs: failed status check");
   tapeio_log_wr(emsg);
   return -1;
 }
 if(mt->fileno < fileno) {
   sprintf(wmsg,"tapeio_process_eofs: out of date fileno");
   tapeio_log_wr(wmsg);
   sprintf(wmsg,"tapeio_process_eofs:old fileno=%d true fileno=%d",
    mt->fileno,fileno);
   tapeio_log_wr(wmsg);
   blkno=0;
   sprintf(wmsg,"tapeio_process_eofs:old blkno=%d true  blkno=%d",
    mt->blkno,blkno);
   tapeio_log_wr(wmsg);
   mt->fileno=fileno;
   mt->blkno = blkno;
 } else {
   sprintf(wmsg,"tapeio_process_eofs: fileno=%d, issue an MTFSF",fileno);
   tapeio_log_wr(wmsg);
   n = tapeio_skip_mark(fd,1);
   if(n < 1) { 
     return -1;
   }
 }
 if(tapeio_rdorwr(&mt->fd) == MTIO_WRITE) {
  return 1;
 }
 loopback:
 mt->nrd = read(mt->fd,mt->buf, sizeof(mt->buf));
 if(mt->nrd < 0) {/* tape error */
   sprintf(emsg,"tapeio_process_eofs: read error");
   tapeio_log_wr(emsg);
   return -1;
 }
 if(mt->nrd == 0) {/* tape mark */
   neof ++;
   sprintf(emsg,"tapeio_process_eofs: Double EOF");
   tapeio_log_wr(emsg);
   return -1;
 }
 if(mt->nrd == 80) {/* tape special label */
   memcpy(lbuf,mt->buf,80);
   lbuf[80]='\0';
   sprintf(wmsg,"tapeio_process_eofs: special label, fileno=%d,",fileno);
   tapeio_log_wr(wmsg);
   sprintf(wmsg,"tapeio_process_eofs: %s",lbuf);
   tapeio_log_wr(wmsg);
   goto loopback;
 }

 i_err = tapeio_status(fd,&fileno, &blkno);
 if(mt->fileno>=1) {/* check 1st record after labels */
   if(mt->nrd==3200) {
     strcpy(mt->type,"SEGY");
     mt->datafile=1;
     mt->datablk=2;
     sprintf(wmsg,"tapeio_process_eofs: 3200 byte segy found");
     tapeio_log_wr(wmsg);
   } else {
     memcpy(lbuf,mt->buf,80);
     lbuf[80]='\0';
     if(strstr((char *)lbuf,"AS_HIST") != 0) {
       strcpy(mt->type,"CPS");
       mt->datafile=2;
       mt->datablk=0;
     }
     if(strstr((char *)lbuf,"PR=") != 0) {
       strcpy(mt->type,"CPS");
       mt->datafile=2;
       mt->datablk=0;
     }
   }
 }
/*
 * reposition to the beginning of the record
 */
 n = tapeio_skip_blk(fd,-1);
 if(n < 0) {
   sprintf(emsg,"tapeio_process_eofs: skip_blk failed");
   tapeio_log_wr(emsg);
 }
 return neof;
}

long  tapeio_next(long *fd)
{struct mtop mt_command;
 tapeio_dat *mt;
 long i_err;
 if(*fd < 0 || *fd >= MAXFD) return -1;
 mt = MTA[*fd];
 if(!mt) return -1;
 i_err = tapeio_rewind(fd);
 if(i_err < 0) {
   sprintf(emsg,"tapeio_next: fd=%ld rewind failed",mt->fd);
   tapeio_log_wr(emsg);
   return -1;
 }
 mt_command.mt_count = 1;
#ifdef sgi
 mt_command.mt_count ++; /* suppress warning */
 return -1;
#else
 mt_command.mt_op    = MTLOAD;
 mt_command.mt_count = 1;
 i_err = ioctl(mt->fd, MTIOCTOP, &mt_command);
 if(i_err < 0) {
   perror("tapeio_next: MTLOAD failure");
   sprintf(emsg,"tapeio_next: fd=%ld errno=%d",mt->fd, errno);
   tapeio_log_wr(emsg);
   if(*fd < 0 || *fd >= MAXFD) return -1;
 }
#endif
 return 0;
}

long tapeio_dmnt(long *fd)
{struct mtop mt_command;
 tapeio_dat *mt;
 long i_err;
 if(*fd < 0 || *fd >= MAXFD) return 0;
 mt = MTA[*fd];
 if(!mt) return -1;
 if (mt->fd != *fd) {
   sprintf(emsg,"tapeio_dmnt: stored fd=%ld != passed fd=%ld",mt->fd,*fd);
   tapeio_log_wr(emsg);
   return -1;
 }
 i_err = tapeio_rewind(fd);
 if(i_err < 0) { /* can fail if tape is already dismounted */
   mt->fileno=0;
   mt->blkno =0;
   i_err = tapeio_close(fd);
   return 0;
 }
 mt_command.mt_op = MTOFFL;
 mt_command.mt_count = 1;
 i_err = ioctl(mt->fd, MTIOCTOP, &mt_command);
 if(i_err < 0) {
   perror("tapeio_dmnt: dismount failure");
   sprintf(emsg,"tapeio_dmnt: fd=%ld errno=%d",mt->fd, errno);
   tapeio_log_wr(emsg);
   return -1;
 } else {
   mt->fileno=0;
   mt->blkno =0;
/* RSD - 00/08/09
   if(strlen(mt->volser)>0) tapeio_tapelib(mt->volser,"u",devtyp,pname);
*/
   i_err = tapeio_close(fd);
 }
 return 0;
}

/* Perform an explicit rewind
 * PRIVATE
 */
long tapeio_rewind(long *fd)
{tapeio_dat *mt;
 struct mtop mt_command;
 int i_err;
 if(*fd < 0 || *fd >= MAXFD) return 0;
 mt = MTA[*fd];
 if(!mt) return -1;
 if(mt->fd < 0) return -1;
 mt_command.mt_op = MTREW;
 mt_command.mt_count = 1;
 i_err = ioctl(mt->fd, MTIOCTOP, &mt_command);
 if(i_err < 0) {
   perror("tapeio_rewind: rewind failure");
   sprintf(emsg,"tapeio_rewind: fd=%ld errno=%d",mt->fd, errno);
   tapeio_log_wr(emsg);
   return -1;
 } else {
   mt->fileno=0;
   mt->blkno =0;
 }
 return 0;
}

long tapeio_rdorwr(long *fd)
{tapeio_dat *mt;
 if(*fd < 0 || *fd >= MAXFD) return -1;
 mt = MTA[*fd];
 if(!mt) return -1;
 return mt->opmode;
}

void tapeio_get_vol(long *fd, char *vol)
{tapeio_dat *mt;
 vol[0]='\0';
 if(*fd < 0 || *fd >= MAXFD) return;
 mt = MTA[*fd];
 if(!mt) return;
 if(mt->lnl < 1) return;
 if (mt->fd != *fd) {
   sprintf(emsg,"tapeio_get_vol: stored fd=%ld != passed fd=%ld",mt->fd,*fd);
   tapeio_log_wr(emsg);
   return;
 }
 memcpy(vol,&mt->label[0][4],6);
 vol[6]='\0';
}

void tapeio_set_vol(long *fd, char *vol)
{tapeio_dat *mt;
 int l;
 if(*fd < 0 || *fd >= MAXFD) return;
 mt = MTA[*fd];
 if(!mt) return;
 if (mt->fd != *fd) {
   sprintf(emsg,"tapeio_set_vol: stored fd=%ld != passed fd=%ld",mt->fd,*fd);
   tapeio_log_wr(emsg);
   return;
 }
 strncpy(mt->label[0],"VOL1",4);
 l = strlen(vol);
 if(l>16) l=16;
 memcpy(&mt->label[0][4],vol,l);
}

void tapeio_get_type(long *fd, char *type)
{tapeio_dat *mt;
 type[0]='\0';
 if(*fd < 0 || *fd >= MAXFD) return;
 mt = MTA[*fd];
 if(!mt) return;
 if (mt->fd != *fd) {
   printf("tapeio_get_type: stored fd=%ld != passed fd=%ld\n",mt->fd,*fd);
   return;
 }
 strcpy(type,mt->type);
}

void tapeio_set_type(long *fd, char *type)
{tapeio_dat *mt;
 if(*fd < 0 || *fd >= MAXFD) return;
 mt = MTA[*fd];
 if(!mt) return;
 strcpy(mt->type,type);
 if(strcmp(mt->type,"CPS")==0) {
   mt->datafile=2;
   mt->datablk=0;
 }
 if(strcmp(mt->type,"SEGY")==0) {
   mt->datafile=1;
   mt->datablk=2;
 }
}

void tapeio_get_pdn(long *fd, char *pdn)
{tapeio_dat *mt;
 pdn[0]='\0';
 if(*fd < 0 || *fd >= MAXFD) return;
 mt = MTA[*fd];
 if(!mt) return;
 strcpy(pdn,mt->pdn);
}

void tapeio_set_pdn(long *fd, char *pdn)
{tapeio_dat *mt;
 if(*fd < 0 || *fd >= MAXFD) return;
 mt = MTA[*fd];
 if(!mt) return;
 strcpy(mt->pdn,pdn);
}

/*
     The MTWEOF ioctl is used for writing  file  marks  to  tape.
     Not  only does this signify the end of a file, but also usu-
     ally has the side effect of flushing all buffers in the tape
     drive  to  the  tape  medium.  A zero count MTWEOF will just
     flush all the buffers and will not  write  any  file  marks.
     Because  a successful completion of this tape operation will
     guarantee that all tape data has been written  to  the  tape
     medium, it is recommended that this tape operation be issued
     before closing a tape device.
*/
long tapeio_weof(long *fd, long *count)
{ struct mtop mt_command;
  int i_err,neof=*count;
  tapeio_dat *mt;
  if(*fd < 0 || *fd >= MAXFD) return -1;
  mt = MTA[*fd];
  if(!mt) return -1;
  if(mt->fd < 0 || neof > 2) return -1;
  if(neof< 0 ) neof=0;
  if(neof> 2 ) neof=2;
  mt_command.mt_op = MTWEOF;
  mt_command.mt_count = neof;
  if(tapeio_verbose>0) {
    sprintf(wmsg,"tapeio_weof: fd=%ld count=%d",*fd, neof);
    tapeio_log_wr(wmsg);
  }
  i_err = ioctl(mt->fd, MTIOCTOP, &mt_command);
  if(i_err < 0) {
    perror("tapeio_weof:");
    sprintf(emsg,"tapeio_weof: errno %d mt->fd=%ld", errno,mt->fd);
    tapeio_log_wr(emsg);
    return -1;
  }
  mt->fileno += 1;
  mt->blkno = 0;
  return neof;
}

long tapeio_skip_mark(long *fd, long count)
{ struct mtop mt_command;
  int i_err;
  tapeio_dat *mt;
  if(*fd < 0 || *fd >= MAXFD) return -1;
  mt = MTA[*fd];
  if(!mt) return -1;
  if(mt->fd < 0 || count > 2) return -1;
  mt_command.mt_op = MTFSF;
  mt_command.mt_count = count;
  if(tapeio_verbose>0) {
    sprintf(wmsg,"tapeio_skip_mark: fd=%ld count=%ld",*fd, count);
    tapeio_log_wr(wmsg);
  }
  i_err = ioctl(mt->fd, MTIOCTOP, &mt_command);
  if(i_err < 0) {
    perror("tapeio_skip_mark:");
    sprintf(emsg,"tapeio_skip_mark: errno %d", errno);
    tapeio_log_wr(emsg);
    return -1;
  }
  mt->fileno += count;
  if(count==0) mt->blkno = 0;
/* status check is not reliable?
  i_err = tapeio_status(fd,&fileno,&blkno);
  if(i_err >=0) {
    mt->fileno = fileno;
    mt->blkno  = blkno; blkno is not reliable
  }
*/
  return count;
}

long tapeio_skip_blk(long *fd, long count)
{ struct mtop mt_command;
  tapeio_dat *mt;
  int i_err;
  if(*fd < 0 || *fd >= MAXFD) return -1;
  mt = MTA[*fd];
  if(!mt) return -1;
  if(mt->fd < 0 ) return -1;
  mt_command.mt_op = MTFSR;
  mt_command.mt_count = count;
  if(count < 0) {
    mt_command.mt_op = MTBSR;
    mt_command.mt_count = -count;
  }
  i_err = ioctl(mt->fd, MTIOCTOP, &mt_command);
  if(i_err < 0) {
    perror("tapeio_skip_blk:");
    printf("tapeio_skip_blk: errno %d\n", errno);
    return -1;
  }
  mt->blkno += count;
  if(mt->blkno < 0) mt->blkno=0;
  return mt_command.mt_count;
}

long tapeio_tell(long *fd, int *fileno, int * blkno)
{ tapeio_dat *mt;
 if(*fd < 0 || *fd >= MAXFD) return -1;
 mt = MTA[*fd];
 if(!mt) return -1;
 *fileno = mt->fileno;
 *blkno  = mt->blkno;
 return 0;
}

/*
 * check on file and block numbers
 */
long  tapeio_status(long *fd, int *fileno, int *blkno)
{tapeio_dat *mt;
 struct mtget mt_status;
 int i_err;
 if(*fd < 0 || *fd >= MAXFD) return -1;
 mt = MTA[*fd];
 if(!mt) return -1;
 if(mt->fd < 0 ) return -1;

 i_err = ioctl(mt->fd, MTIOCGET, (char *)&mt_status);
 if(i_err < 0) {
   perror("tapeio_status:");
   sprintf(emsg,"tapeio_status: errno %d", errno);
   tapeio_log_wr(emsg);
   return -1;
 } else {
   sprintf(wmsg,"tapeio_status: mt_type=%d fileno=%d blkno=%d i_err=%d",
     (int) mt_status.mt_type,
     (int) mt_status.mt_fileno ,(int)mt_status.mt_blkno,i_err);
   tapeio_log_wr(wmsg);
#ifdef sgi
   sprintf(wmsg,"tapeio_status: mt_resid=%d mt_erreg=%hd",
     (int) mt_status.mt_resid, mt_status.mt_erreg);
   tapeio_log_wr(wmsg);
#else
   sprintf(wmsg,"tapeio_status: mt_resid=%d mt_erreg=%ld",
     (int) mt_status.mt_resid, mt_status.mt_erreg);
   tapeio_log_wr(wmsg);
#endif
   sprintf(wmsg,"tapeio_status: cps fileno=%d cps blkno=%d",
     (int) mt->fileno,(int) mt->blkno);
   tapeio_log_wr(wmsg);
 }
 *fileno = mt_status.mt_fileno;
 *blkno  = mt_status.mt_blkno;
 return i_err;
}

/* print and string print */
void tapeio_print(long *fd)
{ char obuf[512];
  tapeio_sprint(fd,obuf);
  printf("%s",obuf);
}
void tapeio_sprint(long *fd, char *obuf)
{ char vol[32],line[120];
  tapeio_dat *mt;
  obuf[0]='\0';
  if(*fd < 0 || *fd >= MAXFD) return;
  mt = MTA[*fd];
  if(!mt) return;
  if(mt->fd < 0 || mt->fd > MAXFD) {
    printf("tapeio_sprint: stored descriptor looks bogus mt->fd=%ld\n",
     mt->fd);
    return;
  }
  obuf[0]='\0';
  /*sprintf(obuf,"\n tapeio_print: format=%s\n",mt->type);*/
  strcpy(vol,"no_label");
  if(mt->lnl>1) memcpy(vol,&mt->label[0][4],6);
  vol[6]='\0';
  sprintf(line," tapeio_print: vol =%s\n",vol);
  strcat(obuf,line);
  sprintf(line," tapeio_print: cps_fileno=%d cps_blkno=%d\n",
   mt->fileno,mt->blkno);
  strcat(obuf,line);
  sprintf(line," tapeio_print: last read size=%d bytes\n",mt->nrd);
  strcat(obuf,line);
}

/* this is a do nothing call - ttrin now takes
 * care of data records on tape that are before the trace data!
long  tapeio_process_headers(tapeio_dat *mt)
{
 int   n,off;
 char lbuf[640];

 if(!mt) return -1;
 if(mt->nrd <=0) return 0;
 if(strcmp(mt->type,"SEGY")==0) {
   if(mt->nrd==3200) {
   }
   if(mt->nrd==400) {
   }
   return 0;
 } else if(strcmp(mt->type,"CPS")==0) {
   memcpy(lbuf,mt->buf,640);
   lbuf[639]='\0';
   if(strstr((char *)lbuf,"PR=") != 0) {
     printf("ascii processing record\n");
     return 0;
   }
   return 0;
 }

 return -1;
}
 */

long tapeio_load_open(char *tapelib, char *volser, char *mode,
     char *devtyp,char *user)
{int statl, stato;
 char devnam[32];
 devnam[0]='\0';
 if(strcmp(devtyp,"3590")==0) {
   statl = tapeio_tapelib(tapelib, volser,"m",devtyp, devnam, user);
 } else {
   sprintf(emsg,"tapeio_load_open: devtyp=%s not supported",devtyp);
   tapeio_log_wr(emsg);
   statl = -1;
 }
 if(statl<0) return -1;
 if(strlen(devnam) <1) return -1;
 sprintf(wmsg,"tapeio_load_open: load is OK, device=%s",devnam);
 tapeio_log_wr(wmsg);

 stato = tapeio_open(volser,devnam, mode);
 return stato; /* the file descriptor */
}

void tapeio_setverbose(int *verbose)
{ tapeio_verbose = *verbose;
}

void tapeio_log_wr(char *msg)
{
  char tstamp[32],msglog[240];
  time_t current;
  struct tm *now;
  pid_t pid;

  pid = getpid();
  tstamp[0]='\0';
  current = time(NULL);
  now = localtime(&current);
  strftime(tstamp,32,"%Y/%m/%d %H.%M.%S",now);
  sprintf(msglog,"#%s#pid=%d#%s\n",tstamp,pid,msg);

  write(1,msglog,strlen(msglog));
}

/*
long tapeio_read_stream(long *fd, long *nby, char *buf)
{ssize_t nrd;
 int nc;
 tapeio_dat *mt;
 int i_err, fileno,blkno;

 if(*fd < 0 || *fd >= MAXFD) return -1;
 mt = MTA[*fd];
 if(!mt) return -1;
 if (mt->fd != *fd) {
   printf("tapeio_read_stream: stored fd=%ld != passed fd=%ld\n",mt->fd,*fd);
   return -1;
 }
 if(mt->fd < 0 || *nby <=0 ) return -1;
 if(!fpntr) {
   printf("tapeio_read_stream: no stream?\n");
   return -1;
 }
 nc = sizeof(mt->buf);
 nrd = fread(buf,1,*nby,fpntr);

 if(nrd < 0) {
   perror("tapeio_read_stream:");
   printf("tapeio_read_stream: errno %d\n", errno);
   i_err = tapeio_status(fd,&fileno, &blkno);
   return -1;
 }

 if(nrd == 0) {
   return 0;
 }

 mt->nrd = nrd;
 if(nc >= nrd) {
   memcpy(mt->buf,buf,mt->nrd);
 }
 mt->blkno++;
 return (long) nrd; 
}

void tapeio_stream_close()
{int ierr;
 if(fpntr) ierr = fclose(fpntr);
 fpntr=0;
}

*/
