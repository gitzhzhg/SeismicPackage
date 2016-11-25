/****
!<CPS_v1 type="PRIMITIVE"/>
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
!                        C P S   P R I M I T I V E 
!
! Name       : tpioclnt
! Category   : io
! Written    : 2000-08-31   by: R.S.Day & C.C. Burch
! Revised    : 2007-05-10   by: Karen Goodger
! Maturity   : beta
! Purpose    : Support for client side tape io
! Portability: No known limitations.
!
!-------------------------------------------------------------------------------
!</brief_doc>


!<descript_doc>
!-------------------------------------------------------------------------------
!                         GENERAL DESCRIPTION   
!
! The functions in tpioclnt are used by TTRIN and TTROT as client side
! support for tape io. The functions in this source file support the basic
! operations for reading or writing a magnetic tape. TTRIN and TTROT
! interface to tpioclnt routines. In turn tpioclnt can either call functions
! from tapeio.c directly, or they can send IO requests to the tpiosrvr 
! program via reads and writes from a socket, and tpiosrvr performs the
! calls to tapeio and returns the results through the socket connection. 
!
!
!-------------------------------------------------------------------------------
!</descript_doc>


!<trace_io_doc>
!-------------------------------------------------------------------------------
!                     TRACE INPUT/OUTPUT REQUIREMENTS 
!
! tpioclnt reads/writes records on tape, but TTRIN and TTROT do the
! formatting of the buffer data to or from trace data in memory
!
!-------------------------------------------------------------------------------
!</trace_io_doc>


!<global_doc>
!-------------------------------------------------------------------------------
!                           GLOBAL PARAMETERS  
!
!  Does not use globals
!-------------------------------------------------------------------------------
!</global_doc>

 
!<header_word_doc>
!-------------------------------------------------------------------------------
!                          TRACE HEADER WORDS
!
! Does not use trace headers
!
!-------------------------------------------------------------------------------
!</header_word_doc>


!<calling_doc>
!-------------------------------------------------------------------------------
!                      INPUT AND OUTPUT ARGUMENTS
!
! For each subroutine or function documented below, each argument is
! flagged as follows:
!        i = value required upon INPUT.
!        o = value set by the routine upon OUTPUT.
!        b = value BOTH required upon input and changed upon output.
!
!  For pointers, the flag (i,o,b) refers to the contents pointed to
!  by the pointer, not to the value of the pointer itself.  The pointer
!  value is required upon INPUT in all cases.
!
!-------------------------------------------------------------------------------
!                          CALLING SEQUENCE
!
!
!
!                      b             i          i
!  int tpioclnt_open(char *volser, char *rwu, char *dvice)
!   -open a tape drive with given volser with read(r)-write(w)
!    volser ... The identifier label of the tape to be mounted.
!               (in the form host:label where host: is optional)
!               Whether sockets are used or not is determined by the form of
!               volser that is passed to the tpioclnt_open call. If volser is
!               a 6 character string the tpioclnt_* calls invoke the tapeio.c
!               functions directly. If volser is of the form host:vol, the
!               socket communications layer is invoked.
!    rwu    ... r or w
!    dvice  ... The device type(i.e. 3590,8MM,DLT- for future use)
!  return value: If there is an error -1 is returned. If successful
!                a file handle,ifile, is returned.
!
!                          i
!  int tpioclnt_close(int *ifile)
!   -close tape unit with file handle ifile
!
!                         i            o
!  int tpioclnt_gvol(int *ifile, char *volser);
!   - return the name of the currently mounted volser for file handle ifile
!    return values: -1 is an error condition,
!                   =0 is OK
!                           i            i           i
!  long tpioclnt_write(int *ifile, char *buff, long *nbytes)
!   -write tape data to tape with handle ifile
!    buff   ... buffer holding the data to write to a tape record
!    nbytes ... number of bytes in the buffer to write to tape
!    return values: -1 is an error condition,
!                  >=0 is the number of bytes written
!
!                             i            i           o
!  long tpioclnt_aswrite(int *ifile, char *buff, long *nbytes,
!       i             o
!  int *nowait, long *didwr)
!   ifile ... file id of tape device for write
!   nbytes... number of bytes to write
!   buff  ... buffer containing data to write
!   nowait... set to non-zero for asynchronous mode.
!   didwr ... number of bytes guaranteed written.
!             With asynch writes this can be the sum of 2 calls.
!             Qued requests do not contribute to didwr until
!             they complete.
!   returns <0 if error
!   return >=0 if no error and equals the number bytes written
!              or the number qued for write.
!   Note: error can be for the previous write since
!   the status is not received until we re-enter if we
!   have a qued asynchronous write..
!
!
!                          i            o           i
!  long tpioclnt_read(int *ifile, char *buff, long *nbytes)
!   -read tape data to tape with handle ifile
!    buff   ... buffer to hold the data read from a tape record
!    nbytes ... max number of bytes in the buffer
!    return values: -1 is an error condition,
!                    0 is an EOF encountered
!                   >0 is the number of bytes read
!
!                            i            o           i           i
!  long tpioclnt_asread(int *ifile, char *buff,long *nbytes, int *next)
!   ifile ... file id of tape device to read
!   nbytes... max number of bytes to try to read
!   buff  ... buffer to receive data
!   next  ... 1 to initiate a read ahead.
!             0 to read only the current record.
!   returns < 0 if error
!   returns >=0 if OK, the number of bytes read
!NOTE:
! 1 next is irrelevant and reads are synchronous if client
!   code is executed on the tape host.
! 2 Also, it is possible in asynch mode to finish a 
!   successful read but fail sending the read ahead request. 
!   
!                          i           i
!  int tpioclnt_space(int *ifile, long nrecs)
!   -move tape with handles ifile nrecs records
!    nrecs>0 move forward, <0 backspace, 0-null
!    return values: the number of records skipped or -1 on error
!
!                         i           i
!  int tpioclnt_weof(int *ifile, int *neofs)
!   -write neofs eofs to tape with handle ifile
!    return values: the number of EOFS written or -1 on error
!
!                         i
!  int tpioclnt_peof(int *ifile)
!   -handle situation when eofs are encountered on tape
!    (devices have differing behavior)
!    - Returns -1 if there is an error
!
!                        i
!  int tpioclnt_rew(int *ifile)
!   -rewind tape with handle ifile)
!-------------------------------------------------------------------------------
!</calling_doc>


!<advice_doc>
!-------------------------------------------------------------------------------
!                            ADVICE FOR USERS
!
!
!-------------------------------------------------------------------------------
!</advice_doc>


!<history_doc>
!-------------------------------------------------------------------------------
!                             REVISION HISTORY
!
!     Date        Author     Description
!     ----        ------     -----------
! 30. 2007-05-10  Goodger    Add 3592HD.
! 29. 2005-09-12  Stoeckley  More fixes to compile with C++.
! 28. 2005-05-17  Goodger    Add 3592.
! 27. 2005-05-09  Stoeckley  Fix to compile with C++.
! 26. 2004-08-10  Goodger    Fix compiler warnings. This involved checking
!                            some function status variables and getting rid of
!                            one unused variable.
! 25. 2003-09-29  R.S.Day    Preserve sequence number when responding to
!                            BUSY reply from the server.
! 24  2003-08-08  R.S.Day    Changed tpioclnt_open_base tpioclnt_close
!                            prototypes. Getting logical drive name from
!                            the server. Added skio_set_alarm_time in open
!                            sequence.
! 23. 2003-08-06  R.S.Day    Trim one byte from tpioclnt_log_wr output.
! 22. 2003-07-30  R.S.Day    Added tpioclnt_log_wr for messages. Added
!                            tpioclnt_get_server_n, tpioclnt_get_servers.
!                            Changed tpioclnt_open_server,
!                            tpioclnt_tape_type_to_node. Increased sleep
!                            cycle for busy conditions.
! 21. 2003-06-17  R.S.Day    Added tpioclnt_open_server, tpioclnt_open_base
!                            Made drive wait time a configuration parameter.
!                            Default wait time was raised from 4 to 36 hours.
! 20. 2003-05-29  R.S.Day    3590HD media added(high density 3590)
! 19. 2003-03-12  R.S.Day    Added asynchronous read and write functions
!                            tpioclnt_asread, tpioclnt_aswrite. Changed the
!                            file_info_struct structure.
! 18. 2003-02-27  R.S.Day    initialized i in tpioclnt_tape_type_to_node
!                            to suppress compiler warning
! 17. 2003-02-10  R.S.Day    Converted to tpiocnfg from tapecnfg.
!                            Removed tpioclnt_dummy.
! 16. 2003-01-30  R.S. Day   Removed call to tpioclnt_port_to_drive.
! 15. 2002-10-10  Schmauch   Included tapecnfg.h.  Added function
!                            tpioclnt_tape_type_to_node.
! 14. 2002-09-16  Schmauch   Removed all signal handling.  CPS signal handling
!                            will call TTRIN/TTROT wrapup subroutine which
!                            eject the tape.
! 13. 2002-04-18  Schmauch   Truncate tpioclnt_write buffer if too big for
!                            packet.
! 12. 2001-10-18  Schmauch   Doesn't recursively try open if failure was
!                            due to operator cancellation.
! 11. 2001-08-01  Schmauch   Added skio_insert_packet_seq_number to
!                            tpioclnt_finalize.
! 10. 2001-06-11  Schmauch   If at first your open doesn't succeed, try, try
!                            again.
!  9. 2001-04-04  R.S.Day    End tag fix
!  8. 2001-02-01  R.S.Day    More diagnoistic print when a tape load fails.
!  7. 2001-01-30  R.S.Day    Increased wait time for free drive to 120s
!                            per attempt.
!                            tpioclnt_open echoes to stdout, the drive
!                            used for a tape mount(e.g. MT44). Function
!                            tpioclnt_port_to_drive added.
!  6. 2000-01-30  R.S.Day    Changed prototype of tpioclnt_get_host_fd to
!                            pass a port number. Open logic is changed since
!                            parent and child servers employ different ports.
!                            Changes to suppress purify UMR error
!  5. 2000-11-08  R.S.Day    Increased sleep duration to 40 seconds in
!                            open and close verify loops.
!  4. 2000-10-25  R.S.Day    Introduced tpioclnt_finalize to shut down tape
!                            io. Replaced tpioclnt_segv with tpioclnt_sig.
!                            Variable sinport renamed to tpport.
!  3. 2000-10-02  R.S.Day    Added a userid to some packets that are
!                            sent to the server. Argument user added to the
!                            tapeio_load_open call.
!  2. 2000-09-28  R.S.Day    Added sigsegv handler that tries to unload tapes
!                            when a client dies with a sigsegv. Removed offset
!                            variable from file_info_struct, and added a
!                            process id.
!  1. 2000-08-31  R.S.Day    Initial version.
!-------------------------------------------------------------------------------
!</history_doc>


!<portability_doc>
!-------------------------------------------------------------------------------
!                         PORTABILITY LIMITATIONS 
!
! Unix operating systems
!-------------------------------------------------------------------------------
!</portability_doc>


!<compile_doc>
!-------------------------------------------------------------------------------
!                      SPECIAL COMPILING REQUIREMENTS 
!
!-------------------------------------------------------------------------------
!</compile_doc>


!<algorithm_doc>
!-------------------------------------------------------------------------------
!                   ALGORITHM DESCRIPTION FOR DEVELOPERS
!
!-------------------------------------------------------------------------------
!</algorithm_doc>


!<programming_doc>
!-------------------------------------------------------------------------------
!                           PROGRAMMING NOTES 
!
! 1)
! The open and close operation may take significant time. In the case when
! sockets are used to communicate with the tape server, the open and close
! operation is broken into a 2 stage process(initiation and verification).
!
! 2)
! Reads of tape records that are large are more complicated when sockets are
! used. There is a max size for a packet length that can be transmitted. The
! server reads into cache,and the client read keeps keeps draining the cache
! until all the data in the server cache has been received by the client.
!-------------------------------------------------------------------------------
!</programming_doc>
****/
char TPIOCLNT_IDENT[100] =
"$Id: tpioclnt.c,v 1.30 2007/05/11 13:59:59 Goodger beta sps $";

#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include <time.h>
#include <sys/types.h>
#include <sys/socket.h>
#include <netinet/in.h>
#include <arpa/inet.h>
#include <unistd.h>
#include <netdb.h>
#include <malloc.h>
#include <pwd.h>
#include <assert.h>
#include <sys/utsname.h>

#include "tpiosrvr.h"
#include "tpiocnfg.h"
#include "tpioclnt.h"
#include "skio.h"
#include "tapeio.h"

#ifdef __cplusplus
extern "C" {
#endif

int tpioclnt_port_to_drive(int port, char *drive, char *ttype);
char *tpioclnt_config_file();
TpioCnfg *tpioclnt_parse_config(char *cnfg_file);
int tpioclnt_tape_type_to_node(const char *type_volser, char *node_volser,
   size_t volser_size, char *dvice, TpioCnfg *cnfg_obj, char *tapehost);
int tpioclnt_open_base(char *in_volser, char *rwu, char *dvice,
      short tpport, TpioCnfg *cnfg_obj,char *tapehost, long *label,
      char *lname);
void tpioclnt_log_wr(char *msg);
long  skio_get_seq_num    (int fd);
void  skio_set_seq_num    (int fd, long seq_num);

/*------------------------- start of functions ----------------------------*/
/*------------------------- start of functions ----------------------------*/
/*------------------------- start of functions ----------------------------*/
#define NumberHosts 64
#define Number_tpio_files 100

struct host_fd_struct {
  char  hostname[32]; /* host name */
  short port;         /* a port for hostname */
  long  fd;           /* socket for hostname & port */
};
static struct host_fd_struct host_fds[NumberHosts];
static int n_active_hosts =0;
struct file_info_struct {
  long ifile;         /* file descriptor returned by server on open */
  long host_fd;       /* socket for a particular file */
  pid_t client_pid;
  char file_type;     /* irrelevant, T */
  char filler[3];
  int  rd_qued;
  int  rd_size;       /* size of read reply*/
  int  rd_bufr;       /* read buffer size  */
  struct tpio_tpread_send   tpread_put_packet;
  struct tpio_tpread_reply *tpread_get_packet;
  int  wr_qued;
  int  wr_size;       /* size of write send*/
  int  wr_bufr;       /* write buffer size */
  struct tpio_tpwrite_send  *tpwrite_put_packet;
  struct tpio_tpwrite_reply tpwrite_get_packet;
};
static struct file_info_struct file_info[Number_tpio_files];
static int tpio_init=0;
static int lhandle;
static int tpio_remote_sw=0;


/*******************************************************************
*  Set tpio_remote_sw isw=0:socket io off, 1: socket io on
*
*  Written Sept 1999 by Charles C Burch
********************************************************************/
void tpioclnt_set_remote_access(int isw) {
  tpio_remote_sw=isw;
  return;
}

/********************************************************************
* Decompose file name into host and base name      *
*   fn=input name, hostn=host name, base_filename=base     *
*            *
* Written June 1999 by Charles C Burch       *
********************************************************************/
int tpioclnt_decompose_filename(char *fn, char *hostn, char* base_filename)
{
  int i, j;

  if(tpio_remote_sw==1) {
    if(strncmp(fn,"/ptmp/",6) == 0 ) {
      for (i=6;fn[i]!='\0'; i++) {
        if(fn[i]=='/') {
          strncpy(hostn,fn+6,i-6);hostn[i-6]='\0';
          if(i==14 && hostn[6]=='0' && hostn[7]>='0' && hostn[7]<='8') {
            strcat(hostn,"a");
            printf("host=%s\n",hostn);
          }
          strcpy(base_filename,fn);
          return(0);
        }
      }
    }

    j=0;
    for (i=0; fn[i]!='\0'; i++) {
      if(fn[i]=='@') j=i;
      if( fn[i]==':') {
        if(j==0) {
          strncpy(hostn,fn,i); /*has host name   */
          hostn[i]='\0';
          strcpy(base_filename,fn+i+1);
        } else {
          strncpy(hostn,fn+j+1,i-j-1);
          hostn[i-j-1]='\0';
          if(fn[i+1]=='/') {
            strcpy(base_filename,fn+i+1);
          } else {
            strcpy(base_filename,"~");
            strncat(base_filename+1,fn,j);
            strcat(base_filename,"/");
            strcat(base_filename,fn+i+1);
          }
        }
        return(0);
      }
    }
  }
  strcpy(base_filename,fn);   /*has no host name */
  (*hostn)='\0';
  return(0);
}

/********************************************************************
* Get host fd-establish connection ifneeded     *
*   hostn=host name, returns fd for connection to host     *
*            *
* Written June 1999 by Charles C Burch       *
********************************************************************/

int tpioclnt_get_host_fd(char *hostn, short portno)
{ short port = portno;
  int i, fd;
  char msg[120];

  if(hostn[0]=='\0') return(0);
  if(port<1024) return(0);

/*
 * See if host & port has already been connected
 */
  for (i=0; i<n_active_hosts; i++) {
    if(strcmp(hostn,host_fds[i].hostname)==0 &&
       host_fds[i].port==port) return(host_fds[i].fd);
  }

  if(n_active_hosts==NumberHosts) {
    sprintf(msg,"tpioclnt_get_host_fd: host_fds array overflow");
    tpioclnt_log_wr(msg);
    return(0);
  }
/*
 * make new connection
 * a socket for each host & port combination 
 */
  fd=skio_init_connection(hostn,port);
  strcpy(host_fds[n_active_hosts].hostname, hostn);
  host_fds[n_active_hosts].fd=fd;
  host_fds[n_active_hosts].port=port;
  n_active_hosts++;

  return(fd);
}
/***********************************************************
* get fileno for use with tpio rtns
*   extracted from previous tpio_open rtn for common use
*
* Written July 2000 by Charles C Burch
***********************************************************/
int tpioclnt_get_fileno() {
  int ifile;

/*Initialize structures if not already done  */
  if(tpio_init==0) {
    tpio_init=1;
    for (ifile=0; ifile<Number_tpio_files; ifile++) {
      file_info[ifile].ifile=0;
      file_info[ifile].host_fd=0;
      file_info[ifile].client_pid=0;
      file_info[ifile].tpread_get_packet=0;
      file_info[ifile].tpwrite_put_packet=0;
      file_info[ifile].rd_qued=0;
      file_info[ifile].rd_size=0;
      file_info[ifile].rd_bufr=0;
      file_info[ifile].wr_qued=0;
      file_info[ifile].wr_size=0;
      file_info[ifile].wr_bufr=0;
    }
  }
/*find available file id       */
  for (ifile=0; ifile<Number_tpio_files; ifile++) {
    if(file_info[ifile].ifile==0) return(ifile);
  }
  printf("tpio file_info array overflow\n");
  return(-1);
}
/*******************************************************
*  check validity of file number ifile from rtn rtn_name
*    extracted form tpio routines for common use
*
*  Written July 2000 by Charles C Burch
********************************************************/
int tpioclnt_check_file_no(int ifile, char *rtn_name) {

  if(ifile<1 || ifile>Number_tpio_files) {
    printf("Invalid file number in %s (%d)\n",rtn_name,ifile);
    return (-1);
  }
  if(file_info[--ifile].ifile==0) {
    printf("Attempting to %s to unopened file (ifile=%d)\n",rtn_name,ifile+1);
    return(-2);
  }
  return(ifile);
}

/*
 * If type_volser is prepended by a tape type followed by a colon, the
 * tape type is replaced with the appropriate tape node in node_volser.
 * If the tape node is the local node, it is removed from node_volser.
 * If there is no valid tape type in type_volser, type_volser is simply
 * copied to node_volser.
 * You might wonder why I am putting the node into node_volser, only to
 * have tpioclnt_decompose_filename take it back out.  I choose this
 * approach in order to remove specific knowledge of node names from
 * TTRIN and TTROT while minimizing the changes to tpioclnt.
 * ehs --- 08oct02
 * type_volser ... media_type:volser   Input
 * node_volser ... host_name:volser    Output
 * tape_host   ... ANY -->use dvice to determine the tape host
 * tape_host   ... !=ANY we want to use a specific tape host
 */
int tpioclnt_tape_type_to_node(const char *type_volser, char *node_volser,
   size_t volser_size, char *dvice, TpioCnfg *cnfg_obj, char *tapehost)
{
   char  *ptr;
   char  *scr_volser;
   struct utsname node;
   int    i, j;
   int    local;
   char  *node_dev;
   char   msg[120];

   node_dev=0;
   local = 0;
   i= -1;
   if(!cnfg_obj) {
     sprintf(msg,"tpioclnt_tape_type_to_node: NULL cnfg_obj");
     tpioclnt_log_wr(msg);
     return -1;
   }
   if (uname(&node) == -1) { /* get node info */
     perror("tpioclnt_tape_type_to_node: uname error");
     sprintf(msg,"tpioclnt_tape_type_to_node: uname error");
     tpioclnt_log_wr(msg);
     return -1;
   }

   if(strcmp(tapehost,"ANY")!=0) {
     /* set node_dev if tapehost is any thing other than ANY*/
     node_dev = tapehost;
   }

   if(strcmp(dvice,"3590")==0 || strcmp(dvice,"3590HD")==0) {
     i= TAPECNFG_3590;
     if(!node_dev) node_dev = cnfg_obj->mt_3590_node[0];
     for (j = 0; j < cnfg_obj->mt_num_nodes[i]; j++)
       if (strcmp(node.nodename, cnfg_obj->mt_3590_node[j])==0){
        local = 1;
       }
   }

   if(strcmp(dvice,"3592")==0) {
     i= TAPECNFG_3592;
     if(!node_dev) node_dev = cnfg_obj->mt_3592_node[0];
     for (j = 0; j < cnfg_obj->mt_num_nodes[i]; j++)
       if (strcmp(node.nodename, cnfg_obj->mt_3592_node[j])==0){
        local = 1;
       }
   }

   if(strcmp(dvice,"3592HD")==0) {
     i= TAPECNFG_3592HD;
     if(!node_dev) node_dev = cnfg_obj->mt_3592HD_node[0];
     for (j = 0; j < cnfg_obj->mt_num_nodes[i]; j++)
       if (strcmp(node.nodename, cnfg_obj->mt_3592HD_node[j])==0){
        local = 1;
       }
   }

   if(strcmp(dvice,"8MM")==0) {
     i= TAPECNFG_8MM;
     if(!node_dev) node_dev = cnfg_obj->mt_8mm_node[0];
     for (j = 0; j < cnfg_obj->mt_num_nodes[i]; j++)
       if (strcmp(node.nodename, cnfg_obj->mt_8mm_node[j])==0){
        local = 1;
       }
   }
   if(strcmp(dvice,"DLT")==0) {
     i= TAPECNFG_DLT;
     if(!node_dev) node_dev = cnfg_obj->mt_dlt_node[0];
     for (j = 0; j < cnfg_obj->mt_num_nodes[i]; j++)
       if (strcmp(node.nodename, cnfg_obj->mt_dlt_node[j])==0){
        local = 1;
       }
   }
   if(strcmp(dvice,"NR3590")==0) {
     i= TAPECNFG_NR3590;
     if(!node_dev) node_dev = cnfg_obj->mt_nr3590_node[0];
     for (j = 0; j < cnfg_obj->mt_num_nodes[i]; j++)
       if (strcmp(node.nodename, cnfg_obj->mt_nr3590_node[j])==0){
        local = 1;
       }
   }
   if(strcmp(dvice,"LTO")==0) {
     i= TAPECNFG_LTO;
     if(!node_dev) node_dev = cnfg_obj->mt_lto_node[0];
     for (j = 0; j < cnfg_obj->mt_num_nodes[i]; j++)
       if (strcmp(node.nodename, cnfg_obj->mt_lto_node[j])==0){
        local = 1;
       }
   }
   if(strcmp(dvice,"3480")==0) {
     i= TAPECNFG_3480;
     if(!node_dev) node_dev = cnfg_obj->mt_3480_node[0];
     for (j = 0; j < cnfg_obj->mt_num_nodes[i]; j++)
       if (strcmp(node.nodename, cnfg_obj->mt_3480_node[j])==0){
        local = 1;
       }
   }
   if(i <0) {
     sprintf(msg,
     "tpioclnt_tape_type_to_node: error , dvice=%s",dvice);
     tpioclnt_log_wr(msg);
     return -1;
   }

   if (strchr(type_volser, (int) ':')) {
      if (!(scr_volser = (char*)malloc(volser_size))) {
         sprintf(msg,"tpioclnt_tape_type_to_node: Out of memory");
         tpioclnt_log_wr(msg);
         return -1;
      }

      strcpy(scr_volser, type_volser);
      ptr = strtok(scr_volser, ":");

      if (ptr == scr_volser) {

         if (i< cnfg_obj->mt_num_types) {
            ptr = strtok(0, "");

            if (uname(&node) == -1) { /* get node info */
               perror("tpioclnt_tape_type_to_node: uname error");
               free(scr_volser);
               return -1;
            }

            if (local == 1) { /* Job running on tape node.  */
               if (ptr)
                  strcpy(node_volser, ptr);
               else
                  node_volser[0] = '\0';
            } else {
               /*
                * This is where we should usually come.
                */
               strcpy(node_volser, node_dev);
               strcat(node_volser, ":");
               if (ptr)
                  strcat(node_volser, ptr);
            }
         } else { /*String in front of colon is not a valid type.*/
            strcpy(node_volser, type_volser);
         }
      } else { /* * Nothing in front of first colon.  */
         strcpy(node_volser, type_volser);
      }

      free(scr_volser);
   } else { /* No colon in type_volser. */
      strcpy(node_volser, type_volser);
   }

   return 0;
}

/**************************************************************
* tpioclnt_open open a tape unit with given volser
*   rwu=r:read, w:write, u:update
*   file id of opened file >0, if no error
*
* Written July 2000 by Charles C Burch
* An open is a multi step process
*  1. initiate open
*     - send open request to parent server on port tpport
*     - parent server logs the request
*       finds a free physical drive/channel
*       returns a port number (retport)for the drive/channel
*       retport returns as tpport if all drives are busy 
*        retport     req  meaning
*         tpport      0    all drives busy
*         tpport     <0    fatal error
*         != tpport  >=0   can mount tape on drive of req
*     - resend the open request to retport
        sleeps open_wait seconds before retry when drives are busy
*  2. verify open
*     - poll server on comport to verify that tape is mounted
*       record the server file descriptor when the mount completes
*   
**************************************************************/

int tpioclnt_open(char *in_volser, char *rwu, char *dvice) {
  int       i_err;
  short     sndport;      /* port for send-recv packet */
  TpioCnfg *cnfg_obj;
  char     *cnfg_file;
  char      msg[120];
  char      tapehost[16];
  char      lname[16];
  long      label = 1;

  cnfg_file=tpioclnt_config_file();
  if(strlen(cnfg_file)==0) {
    sprintf(msg,"tpioclnt_open: Warning - no config file\n");
    tpioclnt_log_wr(msg);
  }
  cnfg_obj = tpioclnt_parse_config(cnfg_file);
  if(!cnfg_obj) {
    sprintf(msg,"tpioclnt_open: ERROR null cnfg_obj\n");
    tpioclnt_log_wr(msg);
    return -1;
  }

  sndport = tpiocnfg_get_tpport(cnfg_obj);
  strcpy(tapehost,"ANY");
  i_err = tpioclnt_open_base(in_volser, rwu, dvice, sndport, cnfg_obj,
          tapehost, &label, lname);
  tpiocnfg_destroy(cnfg_obj);
  return i_err;
}

int tpioclnt_open_server(char *in_volser, char *rwu, char *dvice,
    char *server, char *tapehost, long *label, char *lname) {
  TpioCnfg *cnfg_obj;
  char     *cnfg_file;
  short     sndport;       /* port for send-recv packet */
  int       i_err;
  char      msg[120];

  cnfg_file=tpioclnt_config_file();
  printf("tpioclnt_open_server: file=%s\n",cnfg_file);
  if(strlen(cnfg_file)==0) {
    sprintf(msg,"tpioclnt_open_server: Warning - no config file\n");
    tpioclnt_log_wr(msg);
  }
  cnfg_obj = tpioclnt_parse_config(cnfg_file);
  if(!cnfg_obj) {
    sprintf(msg,"tpioclnt_open_server: ERROR null cnfg_obj\n");
    tpioclnt_log_wr(msg);
    return -1;
  }

/*
  printf("tpioclnt_open_server: DBG server=%s\n",server);
  if(strlen(server) > 0) {
    if(server[0]=='p' || server[0]=='P')
      tpiocnfg_set_ports(cnfg_obj, "PROD");
    if(server[0]=='b' || server[0]=='B')
      tpiocnfg_set_ports(cnfg_obj, "BETA");
  } else {
    tpiocnfg_set_ports(cnfg_obj, "CUST");
  }
*/

  if(strlen(tapehost)==0) strcpy(tapehost,"ANY");
  sndport = tpiocnfg_get_tpport(cnfg_obj);
  i_err = tpioclnt_open_base(in_volser, rwu, dvice, sndport, cnfg_obj,
          tapehost, label, lname);
  tpiocnfg_destroy(cnfg_obj);
  return i_err;
}

int tpioclnt_port_to_drive(int port, char *drive, char *ttype)
{/* int ich; */
  drive[0]='\0';
  ttype[0]='\0';
  strcpy(drive,"error");
  strcpy(ttype,"error");
/* have removed these tapecnfg variables
  for(ich=0;ich<tapecnfg_num_comm_ports; ich++) {
    if(port== tapecnfg_comm_ports[ich]) {
      strcpy(drive,tapecnfg_mt_drives[ich]);
      strcpy(ttype,tapecnfg_mt_types[ich]);
      return 0;
    }
  }
*/
 return -1;
}

/**************************************************************
* Close tpioclnt_close
*   ifile = file id of tape to close
*   returns <0 if error
*   return 0 if file closed and filenot opened by others
*   return >0 if file closed but file opened by others
*
* Written July 2000 by Charles C Burch
**************************************************************/
int tpioclnt_close(int *ifile, char *lname, char *volser) {
  int istat, fd, cnt=0;
  int iohandle=*ifile;
  long tpfd;
  char user[32];
  uid_t uid;
  struct passwd *pwd;
  struct tpio_tpiclose_send  tpiclose_put_packet;
  struct tpio_tpiclose_reply tpiclose_get_packet;
  struct tpio_tpvclose_send  tpvclose_put_packet;
  struct tpio_tpvclose_reply tpvclose_get_packet;
  /* get user name */
  uid = getuid();
  pwd = getpwuid(uid);
  if(pwd) {
    strcpy(user,pwd->pw_name);
  } else {
    strcpy(user,"sps");
  }

  if( (iohandle=tpioclnt_check_file_no(iohandle,"tpioclnt_close")) < 0)
    return(iohandle);

  if((fd=file_info[iohandle].host_fd)==0) {
    istat=tapeio_dmnt(&file_info[iohandle].ifile);
  } else {

    tpfd = file_info[iohandle].ifile;
    if(tpfd<0 && strlen(lname)<=0) return 0;
    tpiclose_put_packet.type=skio_convert_short(TPIO_TPICLOSE_SEND_CODE);
    tpiclose_put_packet.tpfd=skio_convert_long(tpfd);
    tpiclose_put_packet.lname[0] ='\0';
    tpiclose_put_packet.volser[0]='\0';
    strcpy(tpiclose_put_packet.user,user);
    if(strlen(lname)>0)  strcpy(tpiclose_put_packet.lname,lname);
    if(strlen(volser)>0) strcpy(tpiclose_put_packet.volser,volser);
    istat=skio_send_receive_packet(fd,
      (char*)&tpiclose_put_packet, sizeof(tpiclose_put_packet),
      (char*)&tpiclose_get_packet, sizeof(tpiclose_get_packet));
    if(istat<0) {
      printf("tpioclnt_close(i): skio sendrecv error, stat=%d,reply code=%d\n",
      istat, skio_convert_short(tpiclose_get_packet.type));
      return(-1);
    }
    istat=skio_convert_long(tpiclose_get_packet.status);
    if(istat < 0) {/* -1=failure, 0=qued, 999=already closed */
      printf("tpioclnt_close(i): could not queue request reply code=%d\n",
      istat);
      return(-1);
    }

 clverloop:
    tpvclose_put_packet.type=skio_convert_short(TPIO_TPVCLOSE_SEND_CODE);
    tpvclose_put_packet.tpfd=skio_convert_long(tpfd);
    istat=skio_send_receive_packet(fd,
      (char*)&tpvclose_put_packet, sizeof(tpvclose_put_packet),
      (char*)&tpvclose_get_packet, sizeof(tpvclose_get_packet));
    if(istat<0) {
     printf("tpioclnt_close(v): skio sendrecv error, stat=%d, reply code=%d\n",
      istat, skio_convert_short(tpvclose_get_packet.type));
      if(cnt < 6 && istat > -3) goto clverloop;
      printf("tpioclnt_close(v): cnt=%d\n",cnt);
      return(-1);
    }
    istat=skio_convert_long(tpvclose_get_packet.status);
    if(istat < 0) {/* -1=failure, 0=qued, 999=already closed */
      printf("tpioclnt_vclose(v): close failure,reply status=%d\n",
      istat);
      return(-1);
    }
    if(istat==0) {
      cnt ++;
      sleep(40);
      goto clverloop;
    }
    *ifile=-1;
    istat = 0;
  }
  file_info[iohandle].ifile=-1;
  file_info[iohandle].host_fd=0;
  file_info[iohandle].client_pid=0;
  if(file_info[iohandle].tpread_get_packet) {
    free(file_info[iohandle].tpread_get_packet);
    file_info[iohandle].tpread_get_packet=0;
  }
  if(file_info[iohandle].tpwrite_put_packet) {
    free(file_info[iohandle].tpwrite_put_packet);
    file_info[iohandle].tpwrite_put_packet=0;
  }
  file_info[iohandle].rd_qued=0;
  file_info[iohandle].rd_size=0;
  file_info[iohandle].rd_bufr=0;
  file_info[iohandle].wr_qued=0;
  file_info[iohandle].wr_size=0;
  file_info[iohandle].wr_bufr=0;
  return(istat);
}

/**************************************************************
* tpioclnt_read reads a tape record
*   ifile = file id of tape to read
*   nbytes=number of bytes to read
*   buff=buffer to read data into
*   returns <0 if error
*   returns >=0 if no error and nbytes read
*
* Written July 2000 by Charles C Burch
**************************************************************/
long tpioclnt_read(int *ifile, char *buff,long *nbytes) {
  int iohandle = *ifile;
  struct tpio_tpread_send  tpread_put_packet;
  struct tpio_tpread_reply *tpread_get_packet;
  struct tpio_cache_send  tpiocache_put;
  struct tpio_cache_reply *tpiocache_get;
  long ndidrd;
  long coff,nsent,ncpy;
  long ngot;
  int  lcntr=0;
  long reply_len;
  int fd, istat;
  long lstat;

  if( (iohandle=tpioclnt_check_file_no(iohandle,"tpioclnt_read")) <0) {
    printf("tpioclnt_read: bad iohandle\n");
    return(iohandle);
  }

  if( (fd=file_info[iohandle].host_fd) == 0) {
    lstat=tapeio_read(&file_info[iohandle].ifile,nbytes,buff);
    return lstat;
  }

  tpread_put_packet.type=skio_convert_short(TPIO_TPREAD_SEND_CODE);
  tpread_put_packet.tpfd=skio_convert_long(file_info[iohandle].ifile);
  tpread_put_packet.nbytes=skio_convert_long(*nbytes);
  tpread_put_packet.blkno=skio_convert_long(0);


  reply_len = sizeof(struct tpio_tpread_reply)-260+*nbytes;
  if(reply_len> MAX_SEND_SIZE) reply_len = MAX_SEND_SIZE;
  if( (reply_len&1)!=0) reply_len++;
  tpread_get_packet=(struct tpio_tpread_reply *)malloc(reply_len);
  if(tpread_get_packet==NULL) {
      printf("tpioclnt_read: malloc failed (%ld)\n",*nbytes);
      return (-1);
  }

  tpread_get_packet->nsent=0;
  istat=skio_send_receive_packet(fd,
      (char*)&tpread_put_packet,sizeof(tpread_put_packet),
      (char*)tpread_get_packet, reply_len);
  if(istat<0) {
    printf("tpioclnt_read: skio sendrecv error, stat=%d, reply code=%d\n",
    istat, skio_convert_short(tpread_get_packet->type));
    free (tpread_get_packet);
    return(-1);
  }

  lstat  = skio_convert_long(tpread_get_packet->status);
  nsent  = skio_convert_long(tpread_get_packet->nsent);
  coff   = 0;
  ndidrd = 0;
  ngot   = 0;
  if(lstat>0) {
    memcpy(buff+coff, tpread_get_packet->buff, (size_t) nsent);
    coff   += nsent;
    ndidrd = lstat;
    ngot   += nsent;
  } else { /* error(-1) or eof(0) encountered */
    /*
     printf("tpioclnt_read: lstat=%d (EOF)\n",lstat);
    */
    free (tpread_get_packet);
    return lstat;
  }
    
  tpiocache_get = (struct tpio_cache_reply *) tpread_get_packet;
  while(coff < ndidrd) {
    tpiocache_put.type=skio_convert_short(TPIO_CACHE_SEND_CODE);
    tpiocache_put.tpfd=skio_convert_long(file_info[iohandle].ifile);
    tpiocache_put.coff=skio_convert_long(coff);

    istat=skio_send_receive_packet(fd,
      (char*)&tpiocache_put,sizeof(tpiocache_put),
      (char*)tpiocache_get, reply_len);
    if(istat<0) {
      printf("tpioclnt_read: skio sendrecv error, stat=%d, reply code=%d\n",
      istat, skio_convert_short(tpiocache_get->type));
      free (tpread_get_packet);
      return(-1);
    }
    lstat  = skio_convert_long(tpiocache_get->status);
    nsent  = skio_convert_long(tpiocache_get->nsent);
    if(lstat < 0) {
      free (tpread_get_packet);
      return -1;
    }
    lcntr += 1;
    if(lcntr > 40) {
      printf("tpioclnt_read: loop counter logic error?coff=%ld\n",coff);
      free (tpread_get_packet);
      return -1;
    }
    if(lstat == 0 && nsent==0) {
      free (tpread_get_packet);
      return coff;
    }
    if(nsent>0) {
      ncpy=nsent;
      if(coff+ nsent > *nbytes ) ncpy=*nbytes - coff;
      if(ncpy>0) memcpy(buff+coff, tpiocache_get->buff, (size_t) ncpy);
      coff   += nsent;
    }
  }
  free (tpread_get_packet);
  return(coff);
}

/**************************************************************
* tpioclnt_write writes to a tape record
*   ifile = file id of tape to write
*   nbytes=number of bytes to write
*   buff=buffer to write data from
*   returns <0 if error
*   return >=0 if no error and number bytes written
*
* written July 2000 by Charles C Burch
**************************************************************/
long tpioclnt_write(int *ifile, char *buff, long *nbytes) {
  int iohandle = *ifile;
  struct tpio_tpwrite_send  *tpwrite_put_packet;
  struct tpio_tpwrite_reply tpwrite_get_packet;
  long send_len;
  int fd, istat;
  long lstat;
  if( (iohandle=tpioclnt_check_file_no(iohandle,"tpioclnt_write")) < 0)
    return(iohandle);

  if( (fd=file_info[iohandle].host_fd)==0)
  {
    lstat=tapeio_write(&file_info[iohandle].ifile,nbytes,buff);
  }
  else
  {
    static const long max_packet_nbytes =
      MAX_SEND_SIZE - sizeof(struct tpio_tpwrite_send) + 260;
    long packet_nbytes;

    if (*nbytes > max_packet_nbytes)
    {
      printf(
       "tpioclnt_write:  requested write too long, shortened from %ld to %ld\n",
       *nbytes, max_packet_nbytes);

      packet_nbytes = max_packet_nbytes;
    }
    else
    {
      packet_nbytes = *nbytes;
    }

    send_len=sizeof(struct tpio_tpwrite_send)-260+packet_nbytes;
 /* if(send_len> MAX_SEND_SIZE) send_len = MAX_SEND_SIZE; */
    assert(send_len <= MAX_SEND_SIZE);
    if( (send_len&1)!=0) send_len++;
    tpwrite_put_packet=(struct tpio_tpwrite_send *) malloc(send_len);
    if(tpwrite_put_packet==NULL) {
      printf("tpioclnt_write: malloc failed, (%ld)\n",packet_nbytes);
      return (-1);
    }

    tpwrite_put_packet->type=skio_convert_short(TPIO_TPWRITE_SEND_CODE);
    tpwrite_put_packet->tpfd=skio_convert_long(file_info[iohandle].ifile);
    tpwrite_put_packet->nbytes=skio_convert_long(packet_nbytes);
    memcpy(tpwrite_put_packet->buff, buff, (size_t) packet_nbytes);

    istat=skio_send_receive_packet(fd,
      (char*)tpwrite_put_packet, send_len,
      (char*)&tpwrite_get_packet,sizeof(tpwrite_get_packet));
    if(istat<0) {
      printf("tpioclnt_write: skio sendrecv error, stat=%d, reply code=%d\n",
      istat, skio_convert_short(tpwrite_get_packet.type));
      free (tpwrite_put_packet);
      return(-1);
    }

    free (tpwrite_put_packet);
    lstat=skio_convert_long(tpwrite_get_packet.status);
  }
  return(lstat);
}

/**************************************************************
* tpioclnt_space moves tape with handle ifile nrecs records
*    nrec>0 moves forward, <0 backspace
*
* Written July 2000 by Charles C Burch
* Modified Aug 2000 by R.S.Day
**************************************************************/
int tpioclnt_space(int *ifile, long *nrecs) {
  int iohandle = *ifile;
  int istat, fd;
  struct tpio_tpspace_send  tpspace_put_packet;
  struct tpio_tpspace_reply tpspace_get_packet;

  if( (iohandle=tpioclnt_check_file_no(iohandle,"tpioclnt_space")) < 0)
    return(iohandle);

  if((fd=file_info[iohandle].host_fd)==0) {
    istat=tapeio_skip_blk(&file_info[iohandle].ifile, *nrecs);
  } else {
    tpspace_put_packet.type=skio_convert_short(TPIO_TPSPACE_SEND_CODE);
    tpspace_put_packet.tpfd=skio_convert_long(file_info[iohandle].ifile);
    tpspace_put_packet.nrecs=skio_convert_long(*nrecs);

    istat=skio_send_receive_packet(fd,
      (char*)&tpspace_put_packet, sizeof(tpspace_put_packet),
      (char*)&tpspace_get_packet, sizeof(tpspace_get_packet));
    if(istat<0) {
      printf("tpioclnt_space: skio send recv error, stat=%d, reply code=%d\n",
      istat, skio_convert_short(tpspace_get_packet.type));
      return(-1);
    }
    istat=skio_convert_long(tpspace_get_packet.status);
  }
  return(istat);
}

/**************************************************************
* tpioclnt_weof writes neofs EOFs to tape with handle ifile
*
* Written July 2000 by Charles C Burch
*       Modified Aug 2000 by R.S. Day
**************************************************************/
int tpioclnt_weof(int *ifile, long *neofs) {
  int iohandle = *ifile;
  int istat, fd;
  struct tpio_tpweof_send tpweof_put_packet;
  struct tpio_tpweof_reply tpweof_get_packet;

  if( (iohandle=tpioclnt_check_file_no(iohandle,"tpioclnt_weof")) < 0)
  return(iohandle);

  if((fd=file_info[iohandle].host_fd)==0) {
    istat=tapeio_weof(&file_info[iohandle].ifile, neofs);
  } else {
    tpweof_put_packet.type=skio_convert_short(TPIO_TPWEOF_SEND_CODE);
    tpweof_put_packet.tpfd=skio_convert_long(file_info[iohandle].ifile);
    tpweof_put_packet.neofs=skio_convert_long(*neofs);

    istat=skio_send_receive_packet(fd,
      (char*)&tpweof_put_packet, sizeof(tpweof_put_packet),
      (char*)&tpweof_get_packet, sizeof(tpweof_get_packet));
    if(istat<0) {
      printf("tpioclnt_weof: skio error,stat=%d, reply code=%d\n",
      istat, skio_convert_short(tpweof_get_packet.type));
      return(-1);
    }
    istat=skio_convert_long(tpweof_get_packet.status);
  }
  return(istat);
}

/**************************************************************
* tpioclnt_peof process eofs Move past EOFS on reads
*
* Written August 2000 by  R.S. Day
**************************************************************/
int tpioclnt_peof(int *ifile) {
  int iohandle = *ifile;
  int istat, fd;
  struct tpio_tppeof_send tppeof_put_packet;
  struct tpio_tppeof_reply tppeof_get_packet;

  if( (iohandle=tpioclnt_check_file_no(iohandle,"tpioclnt_peof")) < 0)
    return(iohandle);

  if((fd=file_info[iohandle].host_fd)==0) {
    istat=tapeio_process_eofs(&file_info[iohandle].ifile);
  } else {
    tppeof_put_packet.type=skio_convert_short(TPIO_TPPEOF_SEND_CODE);
    tppeof_put_packet.tpfd=skio_convert_long(file_info[iohandle].ifile);

    istat=skio_send_receive_packet(fd,
      (char*)&tppeof_put_packet,sizeof(tppeof_put_packet),
      (char*)&tppeof_get_packet,sizeof(tppeof_get_packet));
    if(istat<0) {
      printf("tpioclnt_peof: skio sendrecv error, stat=%d, reply code=%d\n",
      istat, skio_convert_short(tppeof_get_packet.type));
      return(-1);
    }
    istat=skio_convert_long(tppeof_get_packet.status);
  }
  return(istat);
}


/**************************************************************
*      tprew rewinds tape with handle ifile
*
* Written July 2000 by Charles C Burch
*       Modified Aug 2000 by R.S. Day
**************************************************************/
int tpioclnt_rew(int *ifile) {
  int iohandle = *ifile;
  int istat, fd;
  struct tpio_tprew_send  tprew_put_packet;
  struct tpio_tprew_reply tprew_get_packet;

  if( (iohandle=tpioclnt_check_file_no(iohandle,"tpioclnt_rew")) < 0)
    return(iohandle);

  if((fd=file_info[iohandle].host_fd)==0) {
    istat=tapeio_rewind(&file_info[iohandle].ifile);
  } else {
    tprew_put_packet.type=skio_convert_short(TPIO_TPREW_SEND_CODE);
    tprew_put_packet.tpfd=skio_convert_long(file_info[iohandle].ifile);

    istat=skio_send_receive_packet(fd,
      (char*)&tprew_put_packet,sizeof(tprew_put_packet),
      (char*)&tprew_get_packet,sizeof(tprew_get_packet));
    if(istat<0) {
      printf("tpioclnt_rew: sendrecv error, stat=%d, reply code=%d\n",
      istat, skio_convert_short(tprew_get_packet.type));
      return(-1);
    }
    istat=skio_convert_long(tprew_get_packet.status);
  }
  return(istat);
}

/**************************************************************
*      tpgvol get internal tape label
*
* Written August 2000 by R.S. Day
**************************************************************/
int tpioclnt_gvol(int *ifile, char *volser) {
  int iohandle = *ifile;
  int istat, fd;
  struct tpio_tpgvol_send tpgvol_put_packet;
  struct tpio_tpgvol_reply tpgvol_get_packet;

  volser[0]='\0';
  if( (iohandle=tpioclnt_check_file_no(iohandle,"tpioclnt_gvol")) < 0)
    return(iohandle);

  if((fd=file_info[iohandle].host_fd)==0) {
    tapeio_get_vol(&file_info[iohandle].ifile, volser);
    istat=0;
  } else {
    memset(&tpgvol_put_packet,0,sizeof(struct tpio_tpgvol_send));
    tpgvol_put_packet.type=skio_convert_short(TPIO_TPGVOL_SEND_CODE);
    tpgvol_put_packet.tpfd=skio_convert_long(file_info[iohandle].ifile);

    istat=skio_send_receive_packet(fd,
      (char*)&tpgvol_put_packet,sizeof(tpgvol_put_packet),
      (char*)&tpgvol_get_packet,sizeof(tpgvol_get_packet));
    if(istat<0) {
      printf("tpioclnt_gvol: skio sendrecv error, stat=%d, reply code=%d\n",
      istat, skio_convert_short(tpgvol_get_packet.type));
      return(-1);
    }
    istat=skio_convert_long(tpgvol_get_packet.status);
    strcpy(volser,tpgvol_get_packet.volser);
  }
  return(istat);
}

void tpioclnt_info(int *ifile, int *tag)
{
  int iohandle= *ifile;
  if(*ifile== -1) {
    iohandle= lhandle;
  } else {
     lhandle = *ifile;
  }
  
  if( (iohandle=tpioclnt_check_file_no(iohandle,"tpioclnt_info")) < 0) {
    printf("tpioclnt_info: bad ifile %d\n",iohandle);
    return;
  }
  printf("tpioclnt_info:file_info[%d], tag=%d\n",iohandle,*tag);
  printf("tpioclnt_info: ifile= %ld\n",file_info[iohandle].ifile);
  printf("tpioclnt_info: host_fd= %ld\n",file_info[iohandle].host_fd);
}

/**************************************************************
*      tpprnt tells tapeio to print tape status to stdout
*
* Written August 2000 by R.S. Day
**************************************************************/
int tpioclnt_prnt(int *ifile) {
  int iohandle= *ifile;
  int istat, fd;
  long tpfd;
  struct tpio_tpprnt_send  tpprnt_put_packet;
  struct tpio_tpprnt_reply tpprnt_get_packet;

  if( (iohandle=tpioclnt_check_file_no(iohandle,"tpioclnt_prnt")) < 0)
    return(iohandle);

  if((fd=file_info[iohandle].host_fd)==0) {
    tapeio_print(&file_info[iohandle].ifile);
    istat=0;
  } else {
    tpprnt_put_packet.type=skio_convert_short(TPIO_TPPRNT_SEND_CODE);
    tpfd = file_info[iohandle].ifile;
    if(tpfd<0) return 0;
    tpprnt_put_packet.tpfd=skio_convert_long(file_info[iohandle].ifile);

    istat=skio_send_receive_packet(fd,
      (char*)&tpprnt_put_packet,sizeof(tpprnt_put_packet),
      (char*)&tpprnt_get_packet,sizeof(tpprnt_get_packet));
    if(istat<0) {
      printf("tpioclnt_prnt: skio sendrecv error,stat=%d, reply code=%d\n",
      istat, skio_convert_short(tpprnt_get_packet.type));
      return(-1);
    }
    istat=skio_convert_long(tpprnt_get_packet.status);
  }
  tpprnt_get_packet.buff[511]='\0';
  printf("%s",tpprnt_get_packet.buff);
  return(istat);
}

/*
 * shuts down all open tape channels for the current process.
 * Attempt to tell server to dismount tapes of dieing client.
 * Will send to server but won't wait for reply
 * Scan for tapes associated with the current process id.
 */
void tpioclnt_finalize()
{int i, fdhost;
 long tpfd;
 int status, istat, siz;
 pid_t pid;
 struct tpio_tpiclose_send  tpiclose_put_packet;
 
  pid = getpid();
  if(pid < 0) return;
  for(i=0;i<Number_tpio_files;i++) {
    if(file_info[i].client_pid == pid) {
      tpfd = file_info[i].ifile;
      if((fdhost=file_info[i].host_fd)!=0) {
        tpiclose_put_packet.type=skio_convert_short(TPIO_TPICLOSE_SEND_CODE);
        tpiclose_put_packet.tpfd=skio_convert_long(tpfd);
        skio_insert_packet_seq_number(fdhost, (char *) &tpiclose_put_packet);
        siz = sizeof(struct tpio_tpiclose_send);
        skio_form_packet((char *) &tpiclose_put_packet,
                  &siz);
        status = skio_write_packet(fdhost,
                  (char *) &tpiclose_put_packet,
                  sizeof(struct tpio_tpiclose_send));
        if(status < 0); /* mickey mouse to get rid of compiler warning */
      } else { /* local host */
        istat=tapeio_dmnt(&file_info[i].ifile);
        if(istat == -1); /* mickey mouse to get rid of compiler warning */
      }
      file_info[i].ifile=-1;
      file_info[i].host_fd=0;
      file_info[i].client_pid=0;
      if(file_info[i].tpread_get_packet) {
        free(file_info[i].tpread_get_packet);
        file_info[i].tpread_get_packet=0;
      }
      if(file_info[i].tpwrite_put_packet) {
        free(file_info[i].tpwrite_put_packet);
        file_info[i].tpwrite_put_packet=0;
      }
      file_info[i].rd_qued=0;
      file_info[i].rd_size=0;
      file_info[i].rd_bufr=0;
      file_info[i].wr_qued=0;
      file_info[i].wr_size=0;
      file_info[i].wr_bufr=0;
      
    }
  }
}

TpioCnfg *tpioclnt_parse_config(char *cnfg_file)
{ TpioCnfg *cnfg;
  if(!cnfg_file) return 0;
  if(strlen(cnfg_file)==0) return 0;

  cnfg = tpiocnfg_parse_config(cnfg_file);
  if(!cnfg) {
    printf("tpioclnt_set_config_file: ERROR, no config information\n");
    return 0;
  }
  tpiocnfg_print(cnfg);

  return cnfg;
}

char *tpioclnt_config_file()
{
  static char config_file[120];
  char *sp;
  sp =  tpiocnfg_find_config_file(0);
  if(sp) strcpy(config_file,sp);
  return config_file;
}


/**************************************************************
* tpioclnt_asread reads a tape record
*   ifile ... file id of tape device to read
*   nbytes... max number of bytes to try to read
*   buff  ... buffer to receive data
*   next  ... 1 to initiate a read ahead.
*             0 to read only the current record.
*   returns < 0 if error
*   returns >=0 if OK, the number of bytes read
*NOTE:
* 1 next is irrelevant and reads are synchronous if client
*   code is executed on the tape host.
* 2 Also, it is possible in asynch mode to finish a 
*   successful read but fail sending the read ahead request. 
*   
*
* Written Feb 2003 by R.S.Day
**************************************************************/
long tpioclnt_asread(int *ifile, char *buff,long *nbytes, int *next) {
  struct tpio_tpread_send   tpread_put_packet;
  struct tpio_tpread_reply *tpread_get_packet;
  struct tpio_cache_send    tpiocache_put;
  struct tpio_cache_reply  *tpiocache_get;
  int iohandle = *ifile;
  long ndidrd;
  long coff,nsent,ncpy;
  long ngot;
  int  lcntr=0;
  long reply_len;
  int  fd, istat;
  long lstat;

  if( (iohandle=tpioclnt_check_file_no(iohandle,"tpioclnt_read")) <0) {
    printf("tpioclnt_asread: bad iohandle\n");
    return(iohandle);
  }

  if( (fd=file_info[iohandle].host_fd) == 0) {
    lstat=tapeio_read(&file_info[iohandle].ifile,nbytes,buff);
    return lstat;
  }


  if(file_info[iohandle].rd_qued == 0) {

    /* no outstanding action or 1st read */
    tpread_get_packet = file_info[iohandle].tpread_get_packet;
    reply_len = sizeof(struct tpio_tpread_reply)-260+*nbytes;
    if(reply_len> MAX_SEND_SIZE) reply_len = MAX_SEND_SIZE;
    if( (reply_len&1)!=0) reply_len++;
    file_info[iohandle].rd_size = reply_len;
    if(file_info[iohandle].rd_bufr<reply_len) {
      /* increase the buffer size */
      tpread_get_packet=(struct tpio_tpread_reply *)malloc(reply_len);
      if(!tpread_get_packet) {
        printf("tpioclnt_asread:DBG1 malloc failed (%ld)\n",*nbytes);
        return (-1);
      }
      if(file_info[iohandle].tpread_get_packet) {
        free(file_info[iohandle].tpread_get_packet);
      }
      file_info[iohandle].rd_bufr = reply_len;
      file_info[iohandle].tpread_get_packet = tpread_get_packet;
    }
    if(!tpread_get_packet) {
      printf("tpioclnt_asread:rd_qued=0 ERROR? tpread_get_packet==NULL\n");
      return (-1);
    }

    tpread_get_packet->nsent=0;
    tpread_put_packet.type=skio_convert_short(TPIO_TPREAD_SEND_CODE);
    tpread_put_packet.tpfd=skio_convert_long(file_info[iohandle].ifile);
    tpread_put_packet.nbytes=skio_convert_long(*nbytes);
    tpread_put_packet.blkno=skio_convert_long(0);
    istat=skio_send_receive_packet_check(fd,
      (char*)&tpread_put_packet,sizeof(tpread_put_packet),
      (char*)tpread_get_packet, reply_len, 1);
/*
    printf("tpioclnt_asread:DBG rd_size=%d, rd_bufr=%d\n",
     file_info[iohandle].rd_size,file_info[iohandle].rd_bufr);
    printf("tpioclnt_asread:DBG istat=%d, reply code=%d\n",
     istat, skio_convert_short(tpread_get_packet->type));
    printf("tpioclnt_asread:DBG lstat=%ld, nsent=%ld\n",
     skio_convert_long(tpread_get_packet->status),
     skio_convert_long(tpread_get_packet->nsent));
*/

  } else {

    /* receive the reply for the packet that was sent earlier */
    tpread_get_packet = file_info[iohandle].tpread_get_packet;
    if(!tpread_get_packet) {
      printf("tpioclnt_asread:rd_qued!=0 ERROR? tpread_get_packet==NULL\n");
      return (-1);
    }
    tpread_get_packet->nsent=0;
    reply_len = file_info[iohandle].rd_size;
    istat = skio_send_receive_packet_finish(fd,
      (char*)&(file_info[iohandle].tpread_put_packet),sizeof(tpread_put_packet),
      (char*)tpread_get_packet, reply_len);

  }

  file_info[iohandle].rd_qued = 0;
  if(istat<0) {
    printf("tpioclnt_asread: error receiving packet, stat=%d, reply code=%d\n",
    istat, skio_convert_short(tpread_get_packet->type));
    /* do not free buffer until the close free (tpread_get_packet); */
    return(-1);
  }

 /*
  * if we get here we have received a packet
  */
  lstat  = skio_convert_long(tpread_get_packet->status);
  nsent  = skio_convert_long(tpread_get_packet->nsent);
  coff   = 0;
  ndidrd = 0;
  ngot   = 0;
  if(lstat>0) {
    memcpy(buff+coff, tpread_get_packet->buff, (size_t) nsent);
    coff   += nsent;
    ndidrd = lstat;
    ngot   += nsent;
  } else { /* error(-1) or eof(0) encountered */
    /* do not free buffer until the close free (tpread_get_packet); */
    return lstat;
  }
    
  /*
   * cache reading should only occur for cps blkd tapes
   */
  tpiocache_get = (struct tpio_cache_reply *) tpread_get_packet;
  while(coff < ndidrd) {
    tpiocache_put.type=skio_convert_short(TPIO_CACHE_SEND_CODE);
    tpiocache_put.tpfd=skio_convert_long(file_info[iohandle].ifile);
    tpiocache_put.coff=skio_convert_long(coff);

    istat=skio_send_receive_packet(fd,
      (char*)&tpiocache_put,sizeof(tpiocache_put),
      (char*)tpiocache_get, reply_len);
    if(istat<0) {
      printf("tpioclnt_read: skio sendrecv error, stat=%d, reply code=%d\n",
      istat, skio_convert_short(tpiocache_get->type));
      /* do not free buffer until the close free (tpread_get_packet); */
      return(-1);
    }
    lstat  = skio_convert_long(tpiocache_get->status);
    nsent  = skio_convert_long(tpiocache_get->nsent);
    if(lstat < 0) {
      /* do not free buffer until the close free (tpread_get_packet); */
      return -1;
    }
    lcntr += 1;
    if(lcntr > 40) {
      printf("tpioclnt_read: loop counter logic error?coff=%ld\n",coff);
      /* do not free buffer until the close free (tpread_get_packet); */
      return -1;
    }
    if(lstat == 0 && nsent==0) { /* hit an eof, do not try to read more */
      /* do not free buffer until the close free (tpread_get_packet); */
      return coff;
    }
    if(nsent>0) { /* append received data to the buffer */
      ncpy=nsent;
      if(coff+ nsent > *nbytes ) ncpy=*nbytes - coff;
      if(ncpy>0) memcpy(buff+coff, tpiocache_get->buff, (size_t) ncpy);
      coff   += nsent;
    }
  }

  /*
   * If we get here there was no error and no eof
   * Initiate a read ahead.
   */
  /* do not free buffer until the close
     if(tpread_get_packet) free (tpread_get_packet);
   */
  file_info[iohandle].rd_qued = 0;
  if(*next!=0) {

    file_info[iohandle].tpread_put_packet.type=
     skio_convert_short(TPIO_TPREAD_SEND_CODE);
    file_info[iohandle].tpread_put_packet.tpfd=
     skio_convert_long(file_info[iohandle].ifile);
    file_info[iohandle].tpread_put_packet.nbytes=
     skio_convert_long(*nbytes);
    file_info[iohandle].tpread_put_packet.blkno =
     skio_convert_long(0);

    reply_len = sizeof(struct tpio_tpread_reply)-260+*nbytes;
    if(reply_len> MAX_SEND_SIZE) reply_len = MAX_SEND_SIZE;
    if( (reply_len&1)!=0) reply_len++;
    file_info[iohandle].rd_size = reply_len;
    if(file_info[iohandle].rd_bufr<reply_len) {
      /* increase the buffer size */
      tpread_get_packet=(struct tpio_tpread_reply *)malloc(reply_len);
      if(!tpread_get_packet) {
        printf("tpioclnt_asread:DBG2 malloc failed (%ld)\n",*nbytes);
        return (-1);
      }
      if(file_info[iohandle].tpread_get_packet) {
        free(file_info[iohandle].tpread_get_packet);
      }
      file_info[iohandle].rd_bufr = reply_len;
      file_info[iohandle].tpread_get_packet = tpread_get_packet;
      file_info[iohandle].tpread_get_packet->nsent=0;
    }

    istat = skio_send_receive_packet_start(fd,
        (char*)&(file_info[iohandle].tpread_put_packet),
        sizeof(tpread_put_packet));
    if(istat < 0) {
      printf("tpioclnt_asread: skio_send_receive_packet_start failed (%ld)\n",
      *nbytes);
      return (-1);
    }
    /* set the read flag.  */
    file_info[iohandle].rd_qued = 1;
  }
  return(coff);
}



/**************************************************************
* tpioclnt_aswrite writes to a tape record
*   ifile ... file id of tape device for write
*   nbytes... number of bytes to write
*   buff  ... buffer containing data to write
*   nowait... set to non-zero for asynchronous mode.
*   didwr ... number of bytes guaranteed written.
*             With asynch writes this can be the sum of 2 calls.
*             Qued requests do not contribute to didwr until
*             they complete.
*   returns <0 if error
*   return >=0 if no error and equals the number bytes written
*              or the number qued for write.
*   Note: error can be for the previous write since
*   the status is not received until we re-enter if we
*   have a qued asynchronous write..
*
* written Feb 2003 by R.S.Day
**************************************************************/
long tpioclnt_aswrite(int *ifile, char *buff, long *nbytes,
  int *nowait, long *didwr)
{
  int iohandle = *ifile;
  struct tpio_tpwrite_send  *tpwrite_put_packet;
  struct tpio_tpwrite_reply tpwrite_get_packet;
  long send_len;
  int  fd;
  int  istat;
  long lstat;
  static const long max_packet_nbytes =
      MAX_SEND_SIZE - sizeof(struct tpio_tpwrite_send) + 260;
  long packet_nbytes;

  *didwr = 0;

  if( (iohandle=tpioclnt_check_file_no(iohandle,"tpioclnt_write")) < 0)
    return(iohandle);

  if( (fd=file_info[iohandle].host_fd)==0) {
    lstat=tapeio_write(&file_info[iohandle].ifile,nbytes,buff);
    *didwr = (lstat>=0) ? lstat : 0;
    return(lstat);
  }

  if(*nbytes > max_packet_nbytes) {
    printf(
    "tpioclnt_aswrite:  requested write too long(%ld bytes)\n",*nbytes);
    return -1;
  } else {
    packet_nbytes = *nbytes;
  }

  istat =0;
  if(file_info[iohandle].wr_qued==1) {

    istat=skio_send_receive_packet_finish(fd,
      (char*)file_info[iohandle].tpwrite_put_packet,
      file_info[iohandle].wr_size,
      (char*)&tpwrite_get_packet,sizeof(tpwrite_get_packet));
    if(istat<0) {
      printf("tpioclnt_aswrite: error in nowait write stat=%d, reply code=%d\n",
      istat, skio_convert_short(tpwrite_get_packet.type));
      file_info[iohandle].wr_qued=0;
      
      return(-1);
    }
    lstat=skio_convert_long(tpwrite_get_packet.status);
    *didwr = (lstat>=0) ? lstat : 0;
  }
  file_info[iohandle].wr_qued=0;
  if(*nbytes <= 0) {
    lstat = 0;
    return lstat;
  }

  /* determine packet size and allocate larger buffer if needed */
  tpwrite_put_packet = file_info[iohandle].tpwrite_put_packet;
  send_len=sizeof(struct tpio_tpwrite_send)-260+packet_nbytes;
  assert(send_len <= MAX_SEND_SIZE);
  if( (send_len&1)!=0) send_len++;
  file_info[iohandle].wr_size = send_len;
  if(file_info[iohandle].wr_bufr<send_len || !tpwrite_put_packet) {
    tpwrite_put_packet=(struct tpio_tpwrite_send *) malloc(send_len);
    if(!tpwrite_put_packet) {
      printf("tpioclnt_aswrite: malloc failed, (%ld)\n",packet_nbytes);
      return (-1);
    }
    if(file_info[iohandle].tpwrite_put_packet) {
      free(file_info[iohandle].tpwrite_put_packet);
    }
    file_info[iohandle].tpwrite_put_packet=tpwrite_put_packet;
    file_info[iohandle].wr_bufr = send_len;
  }

  tpwrite_put_packet->type=skio_convert_short(TPIO_TPWRITE_SEND_CODE);
  tpwrite_put_packet->tpfd=skio_convert_long(file_info[iohandle].ifile);
  tpwrite_put_packet->nbytes=skio_convert_long(packet_nbytes);
  memcpy(tpwrite_put_packet->buff, buff, (size_t) packet_nbytes);

  if(*nowait==0) {

    /* send the packet and wait to receive the reply */
    file_info[iohandle].wr_qued=0;
    istat=skio_send_receive_packet(fd,
      (char*)tpwrite_put_packet, file_info[iohandle].wr_size,
      (char*)&tpwrite_get_packet,sizeof(tpwrite_get_packet));
    if(istat<0) {
      printf(
      "tpioclnt_aswrite:(synch) error, istat=%d, send code=%d reply code=%d\n",
      istat, TPIO_TPWRITE_SEND_CODE,
      skio_convert_short(tpwrite_get_packet.type));
      return(-1);
    }
    lstat=skio_convert_long(tpwrite_get_packet.status);
    if(lstat >=0) *didwr += lstat;

  } else {

    /* send the packet and get the reply when we re-enter */
    istat=skio_send_receive_packet_start(fd,
      (char*)tpwrite_put_packet, file_info[iohandle].wr_size);
    if(istat<0) {
      printf("tpioclnt_aswrite:(asynch) error send stat=%d, send code=%d\n",
      istat, skio_convert_short(tpwrite_put_packet->type));
      file_info[iohandle].wr_qued=0;
      return(-1);
    }
    file_info[iohandle].wr_qued=1;
    lstat = packet_nbytes;
    /* Assumes the success of write. This is the number qued for write.
     * true status not known until we reenter and receive reply
     */

  }

  /* lstat is the status of the last write call when running in nowait mode*/
  return(lstat);
}

int tpioclnt_open_base(char *in_volser, char *rwu, char *dvice,
      short tpport, TpioCnfg *cnfg_obj, char *tapehost, long *label,
      char *lname) {
  int ifile;  /* the client io channel request identifier */
  int jfile;
  int istat;
  uid_t uid;
  struct passwd *pwd;
  int busy_cnt = 0;
  int busy_cnt_max = 20;
  int open_try = 0;
  int open_try_max;
  int open_wait = 240;
  long req;
  char hostn[260], base_fn[260];
  char user[32];
  short retport = -1;              /* port returned by server packet */
  short sndport;                   /* port for send-recv packet */
  int   socket;                    /* parent server socket */
  short ptype;                     /* return packet type */
  long tpfd;
  long old_seq_num;
  struct tpio_tpiopen_send  tpiopen_put_packet;
  struct tpio_tpiopen_reply tpiopen_get_packet;
  struct tpio_tpvopen_send  tpvopen_put_packet;
  struct tpio_tpvopen_reply tpvopen_get_packet;
  char volser[sizeof(hostn) + sizeof(base_fn)];
  char msg[120];

  if(!cnfg_obj) {
    sprintf(msg,"tpioclnt_open_base: ERROR null cnfg_obj object");
    tpioclnt_log_wr(msg);
    return -1;
  }
  sndport = tpport;
  if(sndport < 0) {
    sndport = 7736; /* default to production */
  }
  lname[0]='\0';
  open_try_max = (3600/open_wait)*tpiocnfg_drive_wait(cnfg_obj);

  if(tpioclnt_tape_type_to_node( in_volser,
     volser, sizeof(volser), dvice, cnfg_obj, tapehost ) == -1) {
     sprintf(msg,"tpioclnt_open_base: error in tpioclnt_tape_type_to_node");
     tpioclnt_log_wr(msg);
     sprintf(msg,"tpioclnt_open_base:       in_volser=%s",in_volser);
     tpioclnt_log_wr(msg);
     sprintf(msg,"tpioclnt_open_base:        tapehost=%s",tapehost);
     tpioclnt_log_wr(msg);
     return -1;
  }

  if(strstr(volser,":")) tpio_remote_sw=1;
  else tpio_remote_sw = 0;
  /*Get host and base file name*/
  tpioclnt_decompose_filename(volser,hostn,base_fn);

  /* print out input parameters */
  sprintf(msg,"tpioclnt_open_base: tapehost=%s tpport=%hd tpio_remote_sw=%d",
   tapehost,tpport,tpio_remote_sw);
  tpioclnt_log_wr(msg);
  sprintf(msg,"tpioclnt_open_base: in_volser=%s dvice=%s rwu=%s",
   in_volser,dvice,rwu);
  tpioclnt_log_wr(msg);
  sprintf(msg,"tpioclnt_open_base: volser=%s hostn=%s base_fn=%s",
  volser,hostn, base_fn);
  tpioclnt_log_wr(msg);

  /* get user name */
  uid = getuid();
  pwd = getpwuid(uid);
  if(pwd) {
    strcpy(user,pwd->pw_name);
  } else {
    strcpy(user,"sps");
  }

  /* get new index into the file_info table */
  sprintf(msg,"tpioclnt_open_base: hostn=%s  dvice=%s",hostn, dvice);
  tpioclnt_log_wr(msg);
  if ( (ifile=tpioclnt_get_fileno())<0) return(ifile);

/*
 * get socket for IO with parent
 */
 skio_set_alarm_time(20);
 ioploop:
  socket=tpioclnt_get_host_fd(hostn, sndport);
  if( socket == 0) {/* local file */


    jfile=tapeio_load_open(cnfg_obj->tapelib_path,
          base_fn, rwu, dvice, user); /* tape */
    if(jfile<=0) return(0);
    file_info[ifile].ifile = jfile;

  } else {/*  remote host     */

    /*  stuff to send to server here */
    file_info[ifile].file_type='T';
    file_info[ifile].host_fd=socket;
    file_info[ifile].client_pid= getpid();
    memset(&tpiopen_put_packet,0,sizeof(struct tpio_tpiopen_send));
    tpiopen_put_packet.type=skio_convert_short(TPIO_TPIOPEN_SEND_CODE);
    strcpy(tpiopen_put_packet.volser, base_fn);
    strcpy(tpiopen_put_packet.dvice, dvice);
    strcpy(tpiopen_put_packet.mode, rwu);
    strcpy(tpiopen_put_packet.user, user);
    tpiopen_put_packet.label= skio_convert_long(*label);

    old_seq_num = skio_get_seq_num(socket);
    istat=skio_send_receive_packet(socket,
      (char*)&tpiopen_put_packet, sizeof(tpiopen_put_packet),
      (char*)&tpiopen_get_packet, sizeof(tpiopen_get_packet));

    if(istat<0) {
      printf(
      "tpioclnt_open_base(i): skio sendrecv error, istat=%d,reply code=%d\n",
      istat, skio_convert_short(tpiopen_get_packet.type));
      return(-1);
    }

    ptype = skio_convert_short(tpiopen_get_packet.type);

    if(ptype==TPIO_BUSY_CODE) {  /* server is busy please wait */
      busy_cnt++;
      /* keep talking to current port */
      /* keep the sequence number from incrementing */
      /* sndport = tpport; */
      sprintf(msg,"tpioclnt_open_base: TPIO_BUSY_CODE port=%hd old seqno=%ld\n",
      sndport,old_seq_num);
      tpioclnt_log_wr(msg);
      if(busy_cnt == busy_cnt_max) {
        skio_set_alarm_time(2);
        return -1;
      }
      /* give server some breathing room */
      sleep(20);
      skio_set_alarm_time(20);
      /* preserve sequence number */
      skio_set_seq_num(socket, old_seq_num);
      goto ioploop;
    }

    req=skio_convert_long(tpiopen_get_packet.req);
    retport=skio_convert_short(tpiopen_get_packet.comport);
    if(req<0 || retport==0) {
      sprintf(msg,
      "tpioclnt_open_base:error! retport=%hd req=%ld\n",retport,req);
      tpioclnt_log_wr(msg);
      skio_set_alarm_time(2);
      return(-1); /*req= -1 failure, >=0 qued open */
    }
    sprintf(msg,"tpioclnt_open_base:(i) retport=%hd sndport=%hd req=%ld\n",
    retport,sndport,req);
    tpioclnt_log_wr(msg);
    if(retport==tpport) {
      /* failed,busy, or no free drive */
      sndport = retport;
      if(req==0) {  /* req=0 means request is queued-TPQUED */
        open_try++;
        if(open_try < open_try_max) {
          sleep(open_wait);
          goto ioploop;
        }  
        skio_set_alarm_time(2);
        return -1;
      }
    } else { /* repeat request 1 time for new port */
      if(strlen(tpiopen_get_packet.lname) < 12) {
        strcpy(lname,tpiopen_get_packet.lname);
      }
      lname[12]='\0';
      sprintf(msg,"tpioclnt_open_base:(i) sndport=%hd lname=%s",
      sndport,lname);
      tpioclnt_log_wr(msg);
      sndport = retport;
      open_try_max=1;
      if(open_try> 3) open_try=0;
      if(open_try< open_try_max) {
        open_try++;
        goto ioploop;
      }
    }

  /* now verify the open */
    sleep(15);
 opverloop:
    sprintf(msg,"tpioclnt_open_base:(v) lname=%s , req=%ld\n",lname,req);
    tpioclnt_log_wr(msg);
    tpvopen_put_packet.type=skio_convert_short(TPIO_TPVOPEN_SEND_CODE);
    tpvopen_put_packet.req = skio_convert_long(req);
    skio_set_alarm_time(20);
    istat=skio_send_receive_packet(socket,
      (char*)&tpvopen_put_packet, sizeof(tpvopen_put_packet),
      (char*)&tpvopen_get_packet, sizeof(tpvopen_get_packet));
    if(istat<0) {
      printf("tpioclnt_open(v): skio sendrecv error,istat=%d, reply code=%d\n",
      istat, skio_convert_short(tpiopen_get_packet.type));
      skio_set_alarm_time(2);
      return(-1);
    }

    tpfd=skio_convert_long(tpvopen_get_packet.tpfd);
    istat=skio_convert_long(tpvopen_get_packet.status);
    if(istat==0) {
      sleep(40);
      goto opverloop; /* request is still queed */
                      /* You must spend too much time at the pool hall,
                         the word is queued. */
    }

    /*
     * I moved the tpfd < 0 from below since tpfd only is a factor with remote.
     * I put it before the strncpy below so if we are going to make a
     * recursive call, volser is not changed.
     * ehs --- 06jun01
     */
    if ((istat < 0) || (tpfd < 0))
    {
      static int num_recursive_calls = 10;
      int retval;
      /*
       * Maintain original messages depending on whether istat or tpfd are bad.
       */
      if(istat < 0)
      {
        sprintf(msg,
        "tpioclnt_open_base:(v) open failed, istat=%d req=%ld port=%d\n",
        istat, req, sndport);
        tpioclnt_log_wr(msg);
        sprintf(msg,"tpioclnt_open_base:(v) lname=%s type=%s\n",lname,dvice);
        tpioclnt_log_wr(msg);
        file_info[ifile].ifile= -1;

        if (istat == -2) {
         sprintf(msg,
         "tpioclnt_open_base:(v) open failed, OPERATOR CANCELLED\n");
         tpioclnt_log_wr(msg);
        }
      }
      else
      {
        sprintf(msg,"tpioclnt_open_base:(v) error, istat=%d tpfd=%ld\n",
        istat,tpfd);
        tpioclnt_log_wr(msg);
      }
      /*
       * Instead of returning error, recursively try again.
       * ehs --- 06jun01
       * If istat == -2, failure was due to operator cancellation.
       * Don't recursively retry.
       * ehs --- 18sep01
       */
      if ((istat != -2) && num_recursive_calls)
      {
        num_recursive_calls--;

        sprintf(msg,"tpioclnt_open_base:(v) trying recursive reopen\n");
        tpioclnt_log_wr(msg);
        sprintf(msg,"tpioclnt_open_base:(v) you will get %d more trys\n",
          num_recursive_calls);
        tpioclnt_log_wr(msg);
        sprintf(msg,"tpioclnt_open:(v) volser = %s - rwu = %s - dvice = %s\n",
           volser, rwu, dvice);
        tpioclnt_log_wr(msg);
        /*
         * tpiosrvr_is_pqued considers an open request a resend if < 2500 ms
         * since the last.  I doubt if this code can turn around in < 2500 ms,
         * but just to be sure, let's sleep.
         */
        sleep(5);
        /*
         * volser, rwu, and dvice better not be changed anywhere.
         */
        retval = tpioclnt_open_base(in_volser, rwu, dvice,
                 tpport, cnfg_obj,tapehost, label,lname);

        num_recursive_calls++;
      }
      else
      {
        retval = -1;
      }

      skio_set_alarm_time(2);
      return retval;
    }

    strncpy(volser, tpvopen_get_packet.volser,6);
    volser[6]='\0';
    file_info[ifile].ifile=tpfd;
  }

  file_info[ifile].file_type='T';
  file_info[ifile].host_fd=socket;
  file_info[ifile].client_pid= getpid();
  skio_set_alarm_time(2);
  return(1+ifile);
}


void tpioclnt_log_wr(char *msg)
{
  char tstamp[32],msglog[160];
  time_t current;
  struct tm *now;
 /* pid_t pid; */
  int i;

 /*
  pid = getpid();
  */
  tstamp[0]='\0';
  current = time(NULL);
  now = localtime(&current);
  i = strftime(tstamp,32,"%Y/%m/%d %H.%M.%S",now);
  sprintf(msglog,"#%s#%s\n",tstamp,msg);

  i = write(1,msglog,strlen(msglog));
  if(i == -1)printf("tpioclnt_log_write - status = %d\n",i);
}


int tpioclnt_get_server_n(char *server, int *n)
{ /* packs results in servers seperated by 24 bytes */
  TpioCnfg *cnfg_obj;
  char     *cnfg_file;
  char      msg[120];
  char      *sp;
  int       num;

  cnfg_file=tpioclnt_config_file();
  if(strlen(cnfg_file)==0) {
    sprintf(msg,"tpioclnt_get_hosts: Warning - no config file\n");
    tpioclnt_log_wr(msg);
  }
  cnfg_obj = tpioclnt_parse_config(cnfg_file);
  if(!cnfg_obj) {
    sprintf(msg,"tpioclnt_get_hosts: ERROR null cnfg_obj\n");
    tpioclnt_log_wr(msg);
    return -1;
  }
  
  server[0]='\0';
  num = tpiocnfg_get_server_cnt(cnfg_obj);
  sp = tpiocnfg_get_server_n(cnfg_obj, *n);
  if(sp) {
      strcpy(server , sp);
  }

  tpiocnfg_destroy(cnfg_obj);
  return num;
}

int tpioclnt_get_servers(char *servers)
{ /* packs results in servers seperated by 24 bytes */
  TpioCnfg *cnfg_obj;
  char     *cnfg_file;
  char      msg[120];
  char      *sp;
  int       num,i;

  cnfg_file=tpioclnt_config_file();
  if(strlen(cnfg_file)==0) {
    sprintf(msg,"tpioclnt_get_hosts: Warning - no config file\n");
    tpioclnt_log_wr(msg);
  }
  cnfg_obj = tpioclnt_parse_config(cnfg_file);
  if(!cnfg_obj) {
    sprintf(msg,"tpioclnt_get_hosts: ERROR null cnfg_obj\n");
    tpioclnt_log_wr(msg);
    return -1;
  }
  
  servers[0]='\0';
  num = tpiocnfg_get_server_cnt(cnfg_obj);
  for(i=1;i<num+1;i++) {
    sp = tpiocnfg_get_server_n(cnfg_obj, i);
    if(sp) {
      strcpy(servers + (i-1)*24, sp);
    }
  }

  tpiocnfg_destroy(cnfg_obj);
  return num;
}

#ifdef __cplusplus
}
#endif

