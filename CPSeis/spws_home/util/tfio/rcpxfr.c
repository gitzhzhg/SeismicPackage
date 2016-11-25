/*<license>
 -------------------------------------------------------------------------------
  Copyright (c) 2007 ConocoPhillips Company
 
  Permission is hereby granted, free of charge, to any person obtaining a copy
  of this software and associated documentation files (the "Software"), to deal
  in the Software without restriction, including without limitation the rights
  to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
  copies of the Software, and to permit persons to whom the Software is
  furnished to do so, subject to the following conditions:
 
  The above copyright notice and this permission notice shall be included in all
  copies or substantial portions of the Software.
 
  THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
  IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
  FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
  AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
  LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
  OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
  SOFTWARE.
 -------------------------------------------------------------------------------
 </license>*/
#include "c2f_interface.h"
#include <stdio.h>
#if (VMS)
#include <file.h>
#include <stdlib.h>
#include <stddef.h>
#else
#include <unistd.h>
#include <sys/types.h>
#include <fcntl.h>
#include <sys/stat.h>
#endif
#include <signal.h>
#include <ctype.h>
#include "dskio.h"
#include "rcpxfr.h"
#include "cmpi.h"


#define NetIOsget 0
#define NetIOsput 1
#define NetIOaget 2
#define NetIOaput 3
#define NetIO_done   0
#define NetIO_failed 1
#define NetIO_active 2
#define NetIO_start  3
#define NetIO_killed 4

#ifdef NEED_CAPITALS
#define rcpxfr_get_bcast_ RCPXFR_GET_BCAST 
#define rcpxfr_set_bcast_ RCPXFR_SET_BCAST 
#endif
#if (VMS || _AIX || __hpux)
#define rcpxfr_get_bcast_ rcpxfr_get_bcast 
#define rcpxfr_set_bcast_ rcpxfr_set_bcast 
#endif

#ifdef __cplusplus  
extern "C" {                 // for C++ 
#endif

static int jumpover=0;
void rcpxfr_pfile(char *file);
void rcpxfr_del_req(NetIO_request *NIO);
NetIO_request *rcpxfr_bld_req(char *lfile,
     char *rfile, char *rnode, char *ruser, int dir);

void rcpxfr_set_bcast_(int *bflag);
void rcpxfr_get_bcast_(int *bflag);

int  rcpxfr_base(NetIO_request *NIO);
int  rcpxfr_do_vms(NetIO_request *NIO);
int  rcpxfr_do_unix(NetIO_request *NIO);
int  rcpxfr_save_scrip(NetIO_request *NIO);
char *rcpxfr_bld_scrip(NetIO_request *NIO);
char *rcpxfr_cmd(NetIO_request *NIO);
char *rcpxfr_ucmd(NetIO_request *NIO);
char *rcpxfr_vcmd(NetIO_request *NIO);
int  rcpxfr_rm_local(NetIO_request *NIO, char *line);
void rcpxfr_msg(char *msg, FILE *fp);

void rcpxfr_get_bcast_(int *bflag)
{
 *bflag = jumpover;
}
void rcpxfr_set_bcast_(int *bflag)
{
 jumpover = *bflag;
}

#ifdef __cplusplus  
}                   // for C++
#endif
/*------------------------------------------------------------------
C\USER DOC
 *Name   : rcpxfr_, rcpxfr_net_
 *Purpose: Transfer a disk file from the local node to a remote node
 *         or vice-versa
 *Author : R. Day
 *Last revised : 99/09/07
 *
 *Function Definition:        ( Language = C )
 * int rcpxfr_net_(char *lfile,char *netname, int *local, int *dir)
 * void rcpxfr_(char *lfile,char *rfile, char *rnode, char *ruser,
 *             int *dir, int *istat)
 * netname   input      Network file name. e.g. ruser@rnode:rfile
 * lfile     input/out  Name of the file on the local node.
 * rfile     input      Name of the file on the remote node.
 * rnode     input      Name of the remote node.
 *                      NONE is a legal value.
 * ruser     input      User name on the remote node. Can be NULL
 * dir       input      0 sequential get of file from rnode.
 *                      1 sequential put of file to rnode.
 *                      2 asynchronous get of file from rnode.
 *                      3 asynchronous put of file to rnode.
 * local     output     0 if netname does not point to current node.
 *                      1 if netname points to & exists on the current node.
 * *istat    output     0,1,2 transfer status
 *                      0 = done - no error , 1 = failure
 *                      2 = ACTIVE status (for asynch calls)
 *
 *NOTES:
 * 1. rcpxfr uses rcp and will only work between nodes that support
 *    rcp file transfer. The information in /etc/hosts must be
 *    correct on both hosts.
 * 2. User and node information must be defined for the local node in
 *    a .rhosts file on the remote node. See rcp man page.
 * 3. Transfers create temporary file names for the script
 *    and error file using the stdio tmpnam function.
 * 4. All cpus or no cpus should call rcpxfr_check_req
 *    There is an MPI broadcast call in this function. The
 *    same is true for rcpxfr and rcpxfr_con since these
 *    functions call rcpxfr_check_req. The broadcast call
 *    can be disabled by calling rcpxfr_set_bflag_(...)
 *
 * Include files:  rcpxfr.h   cmpi.h  dskio.h
 * functions    :
 *    -- next 9 functions are for public use --
 *          void  rcpxfr_net_cps_
 *          void  rcpxfr_net_
 *          void  rcpxfr_
 *           - rcps or copies a file(may call rcpxfr_check_req)
 *          void  rcpxfr_nb_
 *           - rcps or copies a file(will not call rcpxfr_check_req)
 * NetIO_request *rcpxfr_con
 *          int   rcpxfr_check_req
 *          void  rcpxfr_del_req
 *          void rcpxfr_set_bcast_(int *bflag);
 *           - set bflag=1 to enable mpi broadcast in rcpxfr_check_req
 *           - set bflag=0 to disable mpi broadcast in rcpxfr_check_req
 *          void rcpxfr_get_bcast_(int *bflag);
 *    -- functions below this point are private --
 *          int   rcpxfr_base
 *          int   rcpxfr_do_vms
 *          int   rcpxfr_do_unix
 *          int   rcpxfr_save_scrip
 *          char *rcpxfr_bld_scrip
 *          char *rcpxfr_cmd
 *          char *rcpxfr_ucmd
 *          char *rcpxfr_vcmd
 *
 *                        REVISION HISTORY
 * DATE      WHO         DESCRIPTION
 * --------  --------    --------------------------------------------
 * 99/09/07  Day         Forced t3e to invoke /bin/rcp version of
 *                       of the rcp command
 * 99/06/25  Day         More checking of rnode name to avoid removing
 *                       files that are on the same host.
 * 99/05/25  Day         Added nohup to command line for asynch
 *                       transfers. May cure a timing problem?
 * 99/05/24  Day         Updating Conlib
 * 99/05/18  Day         Added function rcpxfr_net_cps, added argument
 *                       to rcpxfr_net, and eliminated some prints.
 * 99/04/29  Day         Added function rcpxfr_net_ 
 * 99/02/01  Day         Updating Conlib
 * 99/01/29  Day         = to == in if tests in rcpxfr & rcpxfr_nb 
 * 99/01/08  Day         Increased char arrays for long file names
 * 99/01/06  Day         Removed some more extraneous print out
 * 98/12/09  Day         Commented out diagnostic print(rcpxfr_pfile)
 * 98/12/09  Day         Introduced static int jummpover and the
 *                       functions rcpxfr_get_bflag,rcpxfr_set_bflag.
 *                       provided to enable & disable broadcasts.
 * 98/11/30  Day         Altered internal details of rcp functions.
 *                       rcpxfr.h has changed but no change in rcpxfr
 *                       arguments. Synchronous and asynchronous
 *                       transfers are now supported.
 * 98/09/10  Day         rcpxfr_nb_ function added. More intelligence
 *                       to handle a parallel (mpp) environment.
 * 98/01/07  Day         When rnode = NONE do either an ls or a cp.
 * 97/12/08  Day         Use syetem rather than vfork and exec to run
 *                       the script. Calls to mpi wrappers added for
 *                       t3e so only root-cpu transfers the file.
 * 94/04/25  Day         ruser test corrected = to ==.
 * 94/02/14  Day         Corrected logic when ruser=NONE
 * 94/02/10  Day         Disallows leading blank in file name.
 * 94/02/03  Day         Overhauled for VMS support.
 *                       Name of file stored on VAX changed to
 *                       RCPUTIL.C
 * 94/02/01  Day         Overhauled for VMS support.
 * 93/09/30  Day         Changed fork to vfork and exit to _exit
 * 93/05/10  Day         Original creation date.
C\END DOC
 *------------------------------------------------------------------*/
/* blocking transfer */
int rcpxfr_net_cps_(char *lfile,char *netname, int *local, int *dir) {
 char rnode[32],ruser[32],rpath[120],rfile[160],bare[120]; 
 char ninfo[16],fullname[160];
 int istat,must=1;
 strcpy(ninfo,"_netinfo");
 istat = dskio_parse_fullx1_(netname, ninfo,
      rnode, ruser, rfile, bare,
      fullname);
 *local = dskio_is_local_(fullname,&must);
 if(*local==1) {
   strcpy(lfile,rfile);
   return 0;
 }
 rcpxfr_(lfile,rfile, rnode, ruser, dir, &istat);

 return istat;
}

int rcpxfr_net_(char *lfile,char *netname, int *local, int *dir) {
 char rnode[32],ruser[32],rpath[120],rfile[160],bare[120]; 
 int istat,must=1;
 dskio_parse_full_(netname,ruser,rnode,rpath,bare);
 rfile[0]='\0';
 strcpy(rfile,rpath);
 strcat(rfile,bare);
 *local = dskio_is_local_(netname,&must);
 if(*local==1) {
   strcpy(lfile,rfile);
   return 0;
 }
 rcpxfr_(lfile,rfile, rnode, ruser, dir, &istat);
 
 return istat;
}


void rcpxfr_(char lfile[],char *rfile, char *rnode, char *ruser, 
             int *dir, int *istat) {
 NetIO_request *NIO;
 int one=1,i_err;

 i_err=NetIO_failed;
 NIO = rcpxfr_bld_req(lfile,rfile, rnode, ruser, *dir);
 if(NIO) {
  i_err=NIO->status;
  i_err=rcpxfr_base(NIO);
 }

 if(NIO) NIO->nap_time=10; /* set nap longer for initial request*/
 i_err = rcpxfr_check_req(NIO);
 if(NIO) NIO->nap_time=5;

 if((i_err == NetIO_done) || (i_err==NetIO_failed) || (i_err==NetIO_active) ) {
   rcpxfr_del_req(NIO);
 }
 *istat = i_err;
 return;
}

/* non blocking transfer */
void rcpxfr_nb_(char *lfile,char *rfile, char *rnode, char *ruser, 
                int *dir, int *istat) {
 NetIO_request *NIO;
 int i_err;

 i_err=NetIO_failed;
 NIO = rcpxfr_bld_req(lfile,rfile, rnode, ruser, *dir);
 if(NIO) {
  i_err=NIO->status;
  i_err=rcpxfr_base(NIO);
 }

 if((i_err == NetIO_done) || (i_err==NetIO_failed) || (i_err==NetIO_active) ) {
    rcpxfr_del_req(NIO);
 } 
 *istat = i_err;
 return;
}

NetIO_request *rcpxfr_con(char *lfile,char *rfile, char *rnode, char *ruser, 
                int dir) {
 NetIO_request *NIO=0;
 int one=1,i_err;

 NIO = rcpxfr_bld_req(lfile,rfile, rnode, ruser, dir);
 if(NIO) {
  i_err=NIO->status;
  i_err=rcpxfr_base(NIO);
 }

 if(NIO) NIO->nap_time=10; /* set nap longer for initial request*/
 i_err = rcpxfr_check_req(NIO);
 if(NIO) {
  NIO->nap_time=5;
  NIO->status=i_err;
 }
 return NIO;
}


NetIO_request *rcpxfr_bld_req(char *lfile,char *rfile,
                 char *rnode, char *ruser, int dir) {
 NetIO_request *NIO;
 NIO = (NetIO_request *) malloc(sizeof(NetIO_request));
 if(!NIO) return 0;
 if(lfile == 0) return 0;
 if(rnode == 0)
  strcpy(NIO->rnode,"NONE");
 else
  strcpy(NIO->rnode,rnode);
 if(strcmp(NIO->rnode,"none")==0) strcpy(NIO->rnode,"NONE");
 if(lfile[0]==' ' && (dir==NetIOaget || dir==NetIOsget)) return 0;
 if(rfile[0]==' ' && (dir==NetIOaput || dir==NetIOsput)) return 0;
 strcpy(NIO->rfile,rfile);
 strcpy(NIO->ruser,ruser);
 strcpy(NIO->lfile,lfile);
 strcpy(NIO->mode,"noop");
 NIO->dir=dir;
 if(dir==NetIOsget || dir==NetIOaget)
  strcpy(NIO->mode,"pull");
 if(dir==NetIOsput || dir==NetIOaput)
  strcpy(NIO->mode,"push");
 if(strcmp(NIO->mode,"noop")==0) { free(NIO); return 0;}
 NIO->scrip[0]='\0';
 NIO->errfil[0]='\0';
/*
 * with MPI processes only the root
 * should have errfil & scrip set.
 */
 if(cmpi_i_pel_()==0) {
  strcpy(NIO->scrip,tmpnam(NIO->scrip));
  strcpy(NIO->errfil,tmpnam(NIO->errfil));
#ifdef VMS
  strcat(NIO->scrip,".com");
  strcat(NIO->errfil,".err");
#endif
 }
 NIO->status=NetIO_start;
 NIO->nap_time=5;
 return NIO;
}

void rcpxfr_del_req(NetIO_request *NIO) {
 char msg[88];
 if(!NIO) return;
 if(NIO->status == NetIO_active) {
  sprintf(msg," rcpxfr: warning - deleting active request %d\n",
          NIO->status);
  rcpxfr_msg(msg,stdout);
  sprintf(msg," rcpxfr: warning can not check status, NetIO_request=0\n");
  rcpxfr_msg(msg,stdout);
  NIO->status=NetIO_killed;
 }
 
 if(NIO) free(NIO);
}

/*
void rcpxfr_sigusr(int sig) {
 if(sig==SIGUSR1)
  printf("RCPXFR: transfer completed\n");
 if(sig==SIGUSR2)
  printf("RCPXFR: transfer failed\n");
 signal(SIGUSR1,rcpxfr_sigusr1);
} 
*/


int rcpxfr_check_req(NetIO_request *NIO) {
 FILE *ferr=0;
 char line[120];
 int  i,l,n=96,one=1,i_err;

 if(!NIO) {i_err=NetIO_failed; goto allwait;}
 i_err = NIO->status;
/*
 * with MPI processes only the root
 * should have errfil set. This function
 * forces all cpus to wait till root checks the
 * file transfer status. All cpus must call
 * this function, or none should.
 */

 if(cmpi_i_pel_() == 0) { /* Only root cpu checks files*/
  if(NIO->dir==NetIOaget || NIO->dir==NetIOaput) {
/*
    sprintf(line," rcpxfr: nap_time for %d seconds\n",NIO->nap_time);
    rcpxfr_msg(line,stdout);
*/
    i = sleep(NIO->nap_time);
  }
  if(strlen(NIO->errfil)==0) goto allwait;
  ferr = fopen(NIO->errfil,"r");
#ifdef VMS
   if(ferr) {
    while(fgets(line,n,ferr) != 0) {
      l=0;
      while(line[l] != 0) { line[l]=toupper(line[l]); l++; }
      if(strstr(line,"ERROR") != 0) {NIO->status=NetIO_failed;}
      if(strstr(line,"COPIED") != 0) {NIO->status=NetIO_done;}
 
    }
    fclose(ferr);
   }
#else
   if(ferr) {
     while(fgets(line,n,ferr) != 0) {
       l=0;
       while(line[l] != 0) { line[l]=toupper(line[l]); l++; }
       if(strstr(line,"TRANSFER COMPLETE") != 0) NIO->status=NetIO_done;
       if(strstr(line,"TRANSFER FAILED") != 0) NIO->status=NetIO_failed;
       if(strstr(line,"TRANSFER ACTIVE") != 0) NIO->status=NetIO_active;
     }
     fclose(ferr);
   }
#endif
  i_err = NIO->status;

  if (NIO->status==NetIO_failed) {
   sprintf(line," rcpxfr: transfer failed for script file %s\n",NIO->scrip);
   rcpxfr_msg(line,stderr);
   sprintf(line,"\n rcpxfr:--START DUMP OF MESSAGE FILE %s\n",NIO->errfil);
   rcpxfr_msg(line,stdout);
   /* rcpxfr_pfile(NIO->errfil); */
   sprintf(line," rcpxfr:--END  DUMP OF MESSAGE FILE %s\n",NIO->errfil);
   rcpxfr_msg(line,stdout);
   if(strlen(NIO->errfil)>0) remove(NIO->errfil);
   if(strlen(NIO->scrip)>0) remove(NIO->scrip);
   NIO->errfil[0]='\0';
   NIO->scrip[0]='\0';
  }
  if(NIO->status==NetIO_done) {
/*
   sprintf(line," rcpxfr: transfer succeeded for script file %s\n",NIO->scrip);
   rcpxfr_msg(line,stdout);
*/
   if(strlen(NIO->errfil)>0) remove(NIO->errfil);
   if(strlen(NIO->scrip)>0) remove(NIO->scrip);
   NIO->errfil[0]='\0';
   NIO->scrip[0]='\0';
  }
  if (NIO->status==NetIO_active) {
   sprintf(line," rcpxfr: transfer status is active asynchronous\n");
   rcpxfr_msg(line,stdout);
  }
 } /* cmpi_i_pel()==0 */
/* All cpus forced to wait here */
  allwait:
 i=0;
 if(jumpover !=1) cmpi_bcast_i_(&i,&one, &i_err);
 return i_err;
}

int rcpxfr_base(NetIO_request *NIO)
{
 char  msg[88];
 int   i_err;

 if(!NIO) return NetIO_failed;
/*******************************************************
 * output a script or command file for an rcp transfer *
 * syntax for sending file to remote system            *
 * rcp lfile ruser@rnode:"rfile"                       *
 * syntax for fetching file from remote system         *
 * rcp  ruser@rnode:"rfile" lfile                      *
 * vms nodes need a :: after rnode                     *
 ******************************************************/
 i_err=NetIO_active;
 if(cmpi_i_pel_() == 0) {

    i_err = rcpxfr_save_scrip(NIO);
    if(i_err == NetIO_failed)  return NetIO_failed;
/*
    sprintf(msg,"\n rcpxfr:--START OF SCRIPT FILE %s\n",NIO->scrip);
    rcpxfr_msg(msg,stdout);
    rcpxfr_pfile(NIO->scrip);
    sprintf(msg," rcpxfr:--END OF SCRIPT FILE %s\n",NIO->scrip);
    rcpxfr_msg(msg,stdout);
*/

#ifdef VMS
    i_err= rcpxfr_do_vms(NIO);
#else
    i_err= rcpxfr_do_unix(NIO);
#endif

 }
 return i_err;
}

void rcpxfr_pfile(char *file) {
 FILE *fscrip;
 char line[120];
 if(strlen(file)==0) return;
 fscrip = fopen(file,"r");
 if(fscrip) {
   while(fgets(line,120,fscrip) != 0) {
    rcpxfr_msg(line,stdout);
   }
   fclose(fscrip);
 }
}

int rcpxfr_do_vms(NetIO_request *NIO)
{
 int   iex;
  char line[120];

 NIO->status=NetIO_active;
 sprintf(line,"@%s",NIO->scrip);
 iex = system(line);

 return NIO->status;
}

int rcpxfr_do_unix(NetIO_request *NIO)
{
 int  iex;
 int  pid;
 char line[120];
 static char *args[2];
 int status, stde,stdo;

 switch(NIO->dir) {
 case NetIOsput:
 case NetIOsget:
    sprintf(line,"%s",NIO->scrip);
    break;
 case NetIOaput:
 case NetIOaget:
/*
    printf("rcpxfr:asynch mode is deactivated-using synchronous\n");
    sprintf(line,"%s",NIO->scrip);
*/
    sprintf(line,"nohup %s &",NIO->scrip);
    break;
 default:
    NIO->status=NetIO_failed;
    return NIO->status;
 }
 NIO->status=NetIO_active;

 iex = system(line);

 /* we are child and fork failed 
 if( (pid=vfork()) < 0 )
  { perror("fork");
    printf(" rcpxfr: fork failed no child \n");
    _exit(1);
  }
 */

 /* we are child and fork is ok 
 if(pid == 0)
  { args[0] = NIO->scrip;
    args[1] = 0;
    stdo = open(NIO->errfil,O_CREAT | O_RDWR, 0766);
    stde=stdo;
    if(stde != 2) {  close(2); dup(stde); }
    if(stdo != 1) {  close(1); dup(stdo); }
    iex = execvp(line,args);
    perror(NIO->scrip);
    _exit(1);
  }
 */

/* Parent is executing the remaining code
 while(wait(&status) != pid) {  }
 */
 return NIO->status;
}


int rcpxfr_save_scrip(NetIO_request *NIO)
{
 FILE *fscrip=0;
 int mode=0766;
 char *str;
 if(strlen(NIO->scrip)==0) strcpy(NIO->scrip,"rcpscrip.sh");
 fscrip = fopen(NIO->scrip,"w+");
 if(!fscrip) {
    perror("fopen");
    return NetIO_failed;
 } else {

    str = rcpxfr_bld_scrip(NIO);
    if(str) {
      fprintf(fscrip,"%s\n",str);
      free(str);
    }

    if(fscrip) {
      fclose(fscrip);
      chmod(NIO->scrip,mode);
    }
 }
 return NetIO_start;
}

char *rcpxfr_bld_scrip(NetIO_request *NIO)
{char line[600],*str,*str2;
 int vmsflag=0;
#ifdef VMS
 vmsflag = 1;
#endif
 str=(char *) malloc(1024);
 if(!str) return str;

 if(vmsflag==0) {
   sprintf(str,"#! /bin/sh\n");
   strcat(str,"# rcp shell-script\n");
   sprintf(line," echo \"TRANSFER ACTIVE PID: $$\" > %s\n",NIO->errfil);
   strcat(str,line);
 } else {
   sprintf(str,"$ DEFINE SYS$OUTPUT %s\n",NIO->errfil);
 }
 
 str2 = rcpxfr_cmd(NIO);
 if(str2) {
   strcat(str,str2);
   free(str2);
 }

 if(vmsflag==0) {
   strcat(str,"if(test $? = 0)\n");
   strcat(str,"then\n");
   sprintf(line," echo \"TRANSFER COMPLETE PID: $$\" > %s\n",NIO->errfil);
   strcat(str,line);
   if(rcpxfr_rm_local(NIO, line)==1) {
    strcat(str,line);
   }
   strcat(str,"else\n");
   sprintf(line," echo \"TRANSFER FAILED PID: $$\" > %s\n",NIO->errfil);
   strcat(str,line);
   sprintf(line," echo \"file %s not removed\"\n",NIO->lfile);
   strcat(str,line);
   strcat(str,"fi\n");
 }

 return str;
}

int rcpxfr_rm_local(NetIO_request *NIO, char *line) {
 int i,rmall=1;
 char host[104];
 if(!NIO) return 0;
 if(strcmp(NIO->rnode,"NONE")==0) return 0;
 if(strcmp(NIO->rnode,"none")==0) return 0;
 if(NIO->dir==NetIOaget || NIO->dir==NetIOsget) return 0;
 i = dskio_host_(host);
 if(strcmp(NIO->rnode,host)==0) rmall=0;
 if(rmall==1) {
  sprintf(line, " echo \" rcpxfr: removing files\n 1. %s\n 2. %s\n 3. %s\"\n",
  NIO->lfile,NIO->scrip,NIO->errfil);
 } else {
  sprintf(line, " echo \" rcpxfr: removing files\n 1. %s\n 2. %s\n\"\n",
  NIO->scrip,NIO->errfil);
 }
 i =strlen(line);
#ifdef CRAY
 if(rmall==1) {
    sprintf(line+i," rm -f %s %s %s\n",
    NIO->lfile,NIO->scrip,NIO->errfil);
 } else {
    sprintf(line+i," rm -f %s %s\n",
    NIO->scrip,NIO->errfil);
 }
#else
 if(rmall==1) {
    sprintf(line+i," rm %s %s %s\n",
    NIO->lfile,NIO->scrip,NIO->errfil);
 } else {
    sprintf(line+i," rm %s %s\n",
    NIO->scrip,NIO->errfil);
 }
#endif
 return 1;
}

char *rcpxfr_cmd(NetIO_request *NIO){
 int   vmsflag = 0;
 char  remote_file[120],lpath[120],lname[120];
#ifdef VMS
 vmsflag = 1;
#endif
 remote_file[0]='\0';
 if(NIO->rfile == 0) {
   dskio_parse_file(NIO->lfile,lpath,lname);
   strcpy(remote_file,lname);
 }
 else
   strcpy(remote_file,NIO->rfile);
 if(strlen(remote_file) == 0) return 0;

 if(vmsflag ==0)
    return  rcpxfr_ucmd(NIO);
 else
    return  rcpxfr_vcmd(NIO);

}

char *rcpxfr_ucmd(NetIO_request *NIO) {
 char *line=0,*user=NIO->ruser;
 char rcpcmd[16];
 line=(char *) malloc(240);
 if(!line) return line;
 if(user) {
  if(strncmp(NIO->ruser,"NONE",4) ==0) user = 0;
  if(NIO->ruser[0]==' ') user=0;
 }
 
#ifdef _CRAYMPP
 strcpy(rcpcmd,"/bin/rcp");
#else
 strcpy(rcpcmd,"rcp");
#endif
 strcpy(rcpcmd,"rcp");
 if(strcmp(NIO->rnode,"NONE")==0 ) {
   if( strcmp(NIO->rfile,NIO->lfile)==0) {
     sprintf(line," ls -l %s\n",NIO->lfile);
   } else {
     switch(NIO->dir) {
     case NetIOsput:
     case NetIOaput:
       sprintf(line," cp %s %s\n",NIO->lfile,NIO->rfile);
       break;
     case NetIOsget:
     case NetIOaget:
       sprintf(line," cp %s %s\n",NIO->rfile,NIO->lfile);
       break;
     default:
       sprintf(line," rcpxfr: Invalid parameter, dir=%d\n",NIO->dir);
       rcpxfr_msg(line,stdout);
       free(line);
       return 0;
     }
   }
 } else {
   switch(NIO->dir) {
   case NetIOsput: /* put local to remote file */
   case NetIOaput:
       if(user)
        sprintf(line,"%s %s %s@%s:\'%s\'\n",
         rcpcmd, NIO->lfile,user,NIO->rnode,NIO->rfile);
       else
        sprintf(line,"%s %s %s:\'%s\'\n",
         rcpcmd, NIO->lfile,NIO->rnode,NIO->rfile);
       break;
   case NetIOsget: /* pull remote to local file */
   case NetIOaget:
       if(user)
        sprintf(line,"%s %s@%s:\'%s\' %s\n",
         rcpcmd, user,NIO->rnode,NIO->rfile,NIO->lfile);
       else
        sprintf(line,"%s %s:\'%s\' %s\n",
         rcpcmd, NIO->rnode,NIO->rfile,NIO->lfile);
       break;
   default:
       sprintf(line," rcpxfr: Invalid parameter, dir=%d\n",NIO->dir);
       rcpxfr_msg(line,stdout);
       free(line);
       return 0;
   }
 }

 return line;
}


char *rcpxfr_vcmd(NetIO_request *NIO) {
 char *line=0,*user=NIO->ruser;
 line=(char *) malloc(120);
 if(!line) return line;
 if(user) {
   if(strncmp(NIO->ruser,"NONE",4) ==0) user = 0;
   if(NIO->ruser[0]==' ') user=0;
 }

 if(strcmp(NIO->rnode,"NONE")==0 ) {
   if( strcmp(NIO->rfile,NIO->lfile)==0) sprintf(line," dir %s\n",NIO->lfile);
   else {
     switch(NIO->dir) {
     case NetIOsput:
     case NetIOaput:
       sprintf(line," copy %s %s\n",NIO->lfile,NIO->rfile);
       break;
     case NetIOsget:
     case NetIOaget:
       sprintf(line," copy %s %s\n",NIO->rfile,NIO->lfile);
       break;
     default:
       sprintf(line," rcpxfr: Invalid parameter, dir=%d\n",NIO->dir);
       rcpxfr_msg(line,stdout);
       free(line);
       return 0;
     }
   }
 } else { /* rnode specified */
     switch(NIO->dir) {
     case NetIOsput: /* put local to remote file */
     case NetIOaput:
      if(user)
       sprintf(line,"$ rcp/log=size/user=%s %s %s::\"%s\"\n",
        NIO->ruser,NIO->lfile,NIO->rnode,NIO->rfile);
      else
       sprintf(line,"$ rcp/log=size %s %s::\"%s\"\n",
        NIO->lfile,NIO->rnode,NIO->rfile);
      break;
     case NetIOsget: /* pull remote to local file */
     case NetIOaget:
      if(user)
       sprintf(line,"$ rcp/log=size/user=%s %s::\"%s\" %s\n",
        NIO->ruser,NIO->rnode,NIO->rfile,NIO->lfile);
      else
       sprintf(line,"$ rcp/log=size %s::\"%s\" %s\n",
        NIO->rnode,NIO->rfile,NIO->lfile);
      break;
     default:
       sprintf(line," rcpxfr: Invalid parameter, dir=%d\n",NIO->dir);
       rcpxfr_msg(line,stdout);
       free(line);
       return 0;
     }
 }

 return line;
}

void rcpxfr_msg(char *msg, FILE *fp)
{
 if(!msg) return;
 if(cmpi_i_pel_()==0) {
   fprintf(fp,"%s",msg);
 }
}

