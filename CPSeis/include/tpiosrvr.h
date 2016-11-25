/****
!<CPS_v1 type="HEADER_FILE"/>
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
!                      C P S   H E A D E R   F I L E
!
! Name       : tpiosrvr.h
! Category   : stand-alone
! Written    : 2000-08-31   by: R.S.Day
! Revised    : 2003-08-08   by: R.S. Day
! Maturity   : production   2003-09-29
! Purpose    : Server for cps tape io operations
! Portability: Unix only
!
!-------------------------------------------------------------------------------
!</brief_doc>


!<history_doc>
!-------------------------------------------------------------------------------
!                       HEADER FILE REVISION HISTORY
!
!     Date        Author       Description
!     ----        ------       -----------
! 10. 2003-09-29  R.S.Day      Changed packet headers for open and close.
!  9. 2003-07-30  R.S.Day      Added label parameter to tpio_tpiopen_send 
!  8. 2003-01-30  R.S.Day      changed struct tpio_tpiopen_reply .
!  7. 2002-10-10  Ed Schmauch  Moved tape configuration info to tapecnfg.h.
!  6. 2002-09-16  Ed Schmauch  Changed TESTLIB to BETALIB.
!  5. 2001-08-01  R.S.Day      Added DLT drives to mt_... arrays
!  4. 2001-01-02  R.S.Day      Added mt_drives, mt_types, comm_ports
!                              arrays.Added comport member to 
!                              struct tpio_tpiopen_reply
!  3. 2000-11-17  R.S.Day      Changed sinport to tpport to avoid
!                              name conflict with pfiocodes.h
!  2. 2000-10-02  R.S.Day      Added tests for TESTLIB and CUSTOM to switch
!                              the port setting. Added user information to
!                              the tpio_tpiopen_send structure. Switched
!                              codes for busy and verbose. Added a wakeup
!                              packet type(code=9995).
!  1. 2000-08-31  R.S.Day      Initial version.
!
!-------------------------------------------------------------------------------
!</history_doc>
**/
#ifndef _TPIOCODES_
#define _TPIOCODES_

#define MAX_SEND_SIZE 48012L

#define TPIO_TPIOPEN_SEND_CODE  ((short) 32)
#define TPIO_TPIOPEN_REPLY_CODE ((short) 33)
#define TPIO_TPVOPEN_SEND_CODE  ((short) 34)
#define TPIO_TPVOPEN_REPLY_CODE ((short) 35)
#define TPIO_TPICLOSE_SEND_CODE ((short) 36)
#define TPIO_TPICLOSE_REPLY_CODE ((short) 37)
#define TPIO_TPVCLOSE_SEND_CODE ((short) 38)
#define TPIO_TPVCLOSE_REPLY_CODE ((short) 39)
#define TPIO_TPREAD_SEND_CODE   ((short) 40)
#define TPIO_TPREAD_REPLY_CODE  ((short) 41)
#define TPIO_TPWRITE_SEND_CODE  ((short) 42)
#define TPIO_TPWRITE_REPLY_CODE ((short) 43)
#define TPIO_TPSPACE_SEND_CODE  ((short) 44)
#define TPIO_TPSPACE_REPLY_CODE ((short) 45)
#define TPIO_TPWEOF_SEND_CODE   ((short) 46)
#define TPIO_TPWEOF_REPLY_CODE  ((short) 47)
#define TPIO_TPREW_SEND_CODE    ((short) 48)
#define TPIO_TPREW_REPLY_CODE   ((short) 49)
#define TPIO_TPPRNT_SEND_CODE   ((short) 50)
#define TPIO_TPPRNT_REPLY_CODE  ((short) 51)
#define TPIO_TPGVOL_SEND_CODE   ((short) 52)
#define TPIO_TPGVOL_REPLY_CODE  ((short) 53)
#define TPIO_TPPEOF_SEND_CODE   ((short) 54)
#define TPIO_TPPEOF_REPLY_CODE  ((short) 55)
#define TPIO_ADMIN_SEND_CODE    ((short) 56)
#define TPIO_ADMIN_REPLY_CODE   ((short) 57)
#define TPIO_CACHE_SEND_CODE    ((short) 58)
#define TPIO_CACHE_REPLY_CODE   ((short) 59)

#define TPIO_WAKEUP_CODE      ((short) 9995)
#define TPIO_VERBOSE_CODE     ((short) 9996)
#define TPIO_PLEASE_WAIT_CODE ((short) 9997)
#define TPIO_BUSY_CODE        ((short) 9997)
#define TPIO_BAD_PACKET_CODE  ((short) 9998)
#define TPIO_KILL_CODE        ((short) 9999)

/*****************Tape IO packets***********************/
/* length ... the number of bytes actually transmitted in a packet
 *            Less than MAX_SEND_SIZE+2
 * seq_num...
 * chksum ... A check for data integrity of the packet
 * type   ... Packet code indicating the packet type
 * 
 * Notes: 
 *    1. For some packets that are sent to the tape server,
 *       the server does not send back a reply.
 *       TPIO_KILL_CODE
 *       TPIO_VERBOSE_CODE
 */

struct tpio_tpiopen_send{
  long length;
  long seq_num;
  short chksum;
  short type;
  char mode[4];
  char volser[8];
  char user[16];
  long label;
  char dvice[252]; /*e.g. 3590,DLT,8mm */
};
struct tpio_tpiopen_reply{
  long  length;
  long  seq_num;
  short chksum;
  short type;
  long  req;
  short comport; /* ephemeral communications port sent back by server */
  char  lname[12]; /* logical name for tape device */
};
struct tpio_tpvopen_send {
  long  length;
  long  seq_num;
  short chksum;
  short type;
  long  req;
};
struct tpio_tpvopen_reply {
  long  length;
  long  seq_num;
  short chksum;
  short type;
  long  tpfd;
  long  status;
  char  volser[16];
};

struct tpio_tpiclose_send {
  long length;
  long seq_num;
  short chksum;
  short type;
  long tpfd;
  char lname[8];
  char volser[8];
  char user[16];
};
struct tpio_tpiclose_reply {
  long length;
  long seq_num;
  short chksum;
  short type;
  long status;
};
struct tpio_tpvclose_send {
  long length;
  long seq_num;
  short chksum;
  short type;
  long tpfd;
};
struct tpio_tpvclose_reply {
  long length;
  long seq_num;
  short chksum;
  short type;
  long status;
};
struct tpio_tpread_send {
  long length;
  long seq_num;
  short chksum;
  short type;
  long tpfd;       /*tape file descriptor on server*/
  long nbytes;     /*max bytes to read from a tape record */
  long blkno;      /*the record position on the tape */
};
struct tpio_tpread_reply {
  long length;
  long seq_num;
  short chksum;
  short type;
  long status;     /*number of bytes actually read , EOF=0*/
  long nsent;      /*number of bytes sent in buff */
  char buff [260]; /*is extended as needed */
};
struct tpio_cache_send {
  long length;
  long seq_num;
  short chksum;
  short type;
  long tpfd;       /*tape file descriptor on server*/
  long coff;       /*cache offset */
  long blkno;
};
struct tpio_cache_reply {
  long length;
  long seq_num;
  short chksum;
  short type;
  long status;     /* -1/0 for OK/error */
  long nsent;      /*number of bytes sent in buff */
  char buff [260]; /*gets extended as needed */
};

struct tpio_tpwrite_send {
  long length;
  long seq_num;
  short chksum;
  short type;
  long tpfd;
  long nbytes;
  char buff[260]; /*gets extended as needed */
};
struct tpio_tpwrite_reply {
  long length;
  long seq_num;
  short chksum;
  short type;
  long status;
};
struct tpio_tpspace_send {
  long length;
  long seq_num;
  short chksum;
  short type;
  long tpfd;
  long nrecs;
};
struct tpio_tpspace_reply {
  long length;
  long seq_num;
  short chksum;
  short type;
  long status;
};
struct tpio_tpweof_send {
  long length;
  long seq_num;
  short chksum;
  short type;
  long tpfd;
  long neofs;
};
struct tpio_tpweof_reply {
  long length;
  long seq_num;
  short chksum;
  short type;
  long status;
};
struct tpio_tprew_send {
  long length;
  long seq_num;
  short chksum;
  short type;
  long tpfd;
};
struct tpio_tprew_reply {
  long length;
  long seq_num;
  short chksum;
  short type;
  long status;
};
struct tpio_tppeof_send {
  long length;
  long seq_num;
  short chksum;
  short type;
  long tpfd;
};
struct tpio_tppeof_reply {
  long length;
  long seq_num;
  short chksum;
  short type;
  long status;
};
struct tpio_tpprnt_send {
  long length;
  long seq_num;
  short chksum;
  short type;
  long tpfd;
};
struct tpio_tpprnt_reply {
  long length;
  long seq_num;
  short chksum;
  short type;
  long status;
  char buff[512];
};
struct tpio_tpgvol_send {
  long length;
  long seq_num;
  short chksum;
  short type;
  long tpfd;
  char volser[260];
};
struct tpio_tpgvol_reply {
  long length;
  long seq_num;
  short chksum;
  short type;
  long status;
  char volser[260];
};

struct tpio_admin_send {
  long length;
  long seq_num;
  short chksum;
  short type;
  char cmd[4];   /* an instruction code for the server */
  char user[16]; /* send userid for authorization */
  long nbytes;   /* maximum number of bytes allowed in reply buffer */
  char volser[8]; 
};

struct tpio_admin_reply {
  long length;
  long seq_num;
  short chksum;
  short type;
  long nbytes; /* number of bytes sent back in reply */
  char reply[8]; 
};

/******************General non IO packets*********************/

struct tpio_wakeup { /* send-reply to trigger suspended clients */
  long length;
  long seq_num;
  short chksum;
  short type;
  long  nclient;
  char  volser[8];
  char  user[16];
};
struct tpio_please_wait {
  long length;
  long seq_num;
  short chksum;
  short type;
};
struct tpio_verbose {
  long length;
  long seq_num;
  short chksum;
  short type;
  long  verbose;
};
struct tpio_bad_packet {
  long length;
  long seq_num;
  short chksum;
  short type;
};
struct tpio_kill_send {
  long length;
  long seq_num;
  short chksum;
  short type;
};

#endif
