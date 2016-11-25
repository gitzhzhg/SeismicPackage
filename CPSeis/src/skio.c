/*<CPS_v1 type="PRIMITIVE",pretag="!"/>
!----------------------------- skio.c -------------------------------
!----------------------------- skio.c -------------------------------
!----------------------------- skio.c -------------------------------
!            other files are:  skio.h
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
! Name       : SKIO
! Category   : io
! Written    : 1999-09-15   by: Charles C. Burch
! Revised    : 2005-09-12   by: Tom Stoeckley
! Maturity   : production
! Purpose    : Provides an interface into socket based i/o
! References : These routines are called from within pfio.c
!</brief_doc>
!<descript_doc>
!Skio is a set of routines that provide socket_based i/o.  They are not to be
!called from any codes except bfio, pfio, or tape i/o.  All other i/o calls into
!the "c" language should go through the cio.f90 or cio_crou.c functions.
!</descript_doc>
!<calling_doc>
! do not call these routines.
!</calling_doc>
!<history_doc>
!-------------------------------------------------------------------------------
!                     AUXILIARY FILE REVISION HISTORY 
!     Date        Author         Description
!     ----        ------         -----------
! 19. 2005-09-12  Stoeckley      More fixes to compile with C++.
! 18. 2005-05-31  Stoeckley      Fix to compile with C++.
! 17. 2004-01-21  R Selzler      Resolve SGI compiler warnings
! 16. 2003-09-29  R.S.Day        Added skio_set_alarm_time. Increased TWAIT_MIN
! 15. 2003-05-19  R.S.Day        Added new functions that support 
!                                overlapped I/O for tapes.
!                                skio_send_receive_packet_start
!                                skio_send_receive_packet_finish
!                                skio_send_receive_packet_check
! 14. 2001-08-01  Ed Schmauch    Clarify error messages in
!                                skio_send_receive_packet & skio_sndrecv_addr.
! 13. 2001-07-10  Ed Schmauch    Keep a different series of sequence numbers
!                                for each fd.
! 12. 2001-03-21  Bill Menger    Fixed file type in prolog.
! 11. 2001-02-20  R.S.Day        Increased TWAIT_MAX to 180.
! 10. 2001-01-02  R.S.Day        Added skio_sndrecv_addr for use with tape
!                                server
!  9. 2000-12-08  R.S.Day        Small change to suppress purify UMR error
!                                Changed timeouts to double alarm time when
!                                a time out occurs. Will try for longer time
!                                to get correct server reply.
!                                Max alarm time limit of 60sec.
1                                Added skio_init, skio_tstamp, skio_stop,
!                                skio_set_timeout functions.
!  8. 2000-11-02  R.S.Day        Better diagnostic output on errors in
!                                skio_send_receive_packet.
!  7. 2000-10-04  R.S.Day        Replacing setjmp and longjmp with
!                                sigsetjmp and siglongjmp. Also added
!                                some fixes for wakeup packets.
!  6. 2000-10-03  R.S.Day        The initialization of send_sw was accidently
!                                commented out in skio_send_receive_packet.
!  5. 2000-10-02  R.S.Day        Fix in send_receive_packet to cure a problem
!                                with hung clients.
!  4. 2000-09-28  R.S.Day        Added skio_get_packet_seq_number
!                                added byte swapping functions
!  3. 2000-08-17  Chuck C. Burch Robust socket i/o.
!  2. 2000-04-19  Bill Menger    Stub version
!  1. 2000-04-10  Chuck C. Burch Initial Version.
!-------------------------------------------------------------------------------
!</history_doc>
!<portability_doc>
! no known limitations.
!</portability_doc>
*/
/*
!--------------------------- start of coding ------------------------------
!--------------------------- start of coding ------------------------------
!--------------------------- start of coding ------------------------------
*/
char *skio_ident = 
"$Id: skio.c,v 1.19 2005/09/13 11:21:01 Stoeckley prod sps $";

/****************************** SKIO routines *******************
* Socket IO routines
*
* Written June 2000 by Charles C Burch
****************************************************************/

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sys/types.h>
#include <sys/time.h>
#include <sys/socket.h>
#include <netinet/in.h>
#include <arpa/inet.h>
#include <unistd.h>
#include <errno.h>
#include <netdb.h>
#include <signal.h>
#include <setjmp.h>

#include "skio.h"

typedef struct SRTimeStats {
  long  t_base;    /* reference time in seconds */
  float t_reply;   /* last reply time in seconds */
  float t_avg;     /* average reply time in seconds */
  float t_sigma;   /* smoothed reply time deviation in seconds */
  float t_alarm;   /* current alarm time in seconds */
  int   ntry;
} sr_time_stats;

#define MAXTRYS   10
#define TWAIT_MIN 4
#define TWAIT_MAX 180
#define SKIO_TWAIT(ptr)  ((ptr)->t_avg + (4.0 * (ptr)->t_sigma) )
#define FIRST_SEQ_NUM     1
#define  LAST_SEQ_NUM 32766

static int first_call;
static sr_time_stats t_stats;
static long endian_type=0;
static long packet_timeout_errors=0, packet_bad_errors=0;
static sigjmp_buf env2;
static int _max_fd_for_seq_num_ary = -1;
static long *_seq_num_ary;

#ifdef __cplusplus
extern "C" {
#endif

void  skio_init(sr_time_stats *ts);
void  skio_timeout(int);
int   skio_set_timeout(sr_time_stats *ts);
void  skio_stop(sr_time_stats *ts, long t_reply);
long  skio_tstamp(sr_time_stats *ts);
void  skio_chk_seq_num_ary(int fd);
long  skio_get_seq_num    (int fd);
void  skio_set_seq_num    (int fd, long seq_num);
long  skio_inc_seq_num    (int fd);

/**************** Format of packets are**************************
*  packet length (long bytes 0-3)
*  sequence number (long bytes 4-7)
*  checksum (short bytes 8-9)
*  packet code (short bytes 10-11)
*  response packet has code of sent packet +1
*/

/*------------------------ start of functions ----------------------------*/
/*------------------------ start of functions ----------------------------*/
/*------------------------ start of functions ----------------------------*/

/***********************************************************************
*       receive a packet from socket fd to buffer p of len packet_len  *
*                                                                      *
*       Written Sept 1999 by Charles C Burch                           *
***********************************************************************/
int skio_read_packet(int fd, char *p, int packet_len) {
  int istat;

/*printf("\npfread_packet, len=%d\n",packet_len);       */
  istat=recvfrom(fd, p, packet_len, (unsigned int) 0, 
      (struct sockaddr*) 0, (socklen_t*) 0);
/*printf("pfread_packet, istat=%d\n",istat);            */  
  return(istat);
}
/********************************************************************
*       send a packet to socket fd from buffer p of len packet_len  
*                                                                   
*       Written June 2000 by Charles C Burch                        
********************************************************************/
int skio_write_packet(int fd, char *p, int packet_len) {
  int istat;

 restart1:
  istat=sendto(fd,p,(int) packet_len, (unsigned int) 0, 
    (struct sockaddr*) 0, (int) 0 );
  if(istat < 0) {
    if(errno == EINTR) goto restart1;
    printf("skio_write_packet: errno=%d\n",errno);
  }

  return(istat);
}
/********************************************************************
*       Update skio sequence number and insert into packet
*                                                                   
*       Written June 2000 by Charles C Burch                        
*       Updated to use sequence number routines, ehs 03jul01
********************************************************************/
int skio_insert_packet_seq_number(int fd, char *p)
{
  long retval = skio_inc_seq_num(fd);

  (*(long*)(p+4))=skio_convert_long(retval);

  return (int) retval;
}
/********************************************************************
*       Checks validity of skio sequence number in a packet  
*         Return 1 if valid, -1 if invalid
*                                                                   
*       Written June 2000 by Charles C Burch                        
*       Updated to use sequence number routines, ehs 03jul01
********************************************************************/
int skio_check_packet_seq_number (int fd, char *p) {
 if( skio_convert_long((*(long*)(p+4))) == skio_get_seq_num(fd))
 {
    return(1);
 }
 else
 {
    return(-1);
 }
}
long skio_get_packet_seq_number (char *p) {
  long seqno;
  seqno = skio_convert_long((*(long *)(p+4)));
  return seqno;
}
/********************************************************************
*       Form a skio packet --Insert packet length and checksum
*                                                                   
*       Written June 2000 by Charles C Burch                        
********************************************************************/
int skio_form_packet(char *p, int *packet_len) {
  short chksum, *p_short;
  long i, n;

  p_short= (short *) p;
  /*force packet_len to even #bytes*/
  if ( ((*packet_len) & 0x1)==1) (*packet_len)++;
  /*place packet length in the packet */
  (*((long*)p))=skio_convert_long(*packet_len);
  /* generate a time stamp */
  skio_tstamp(&t_stats);
  /*place time stamp in the packet */

  (*(p_short+4))= 0;
  chksum=skio_convert_short(*(p_short+4));
  n=(*packet_len)/2;
  for (i=0; i<n; i++) {
    chksum ^= skio_convert_short(* (p_short+i));
  }
  (*(p_short+4))= skio_convert_short(chksum);
  return(0);
}

/********************************************************************
*       Check skio packet length and checksum
*         Returns 0 if OK, -1 length error, -2 checksum error
*                                                                   
*       Written June 2000 by Charles C Burch                        
********************************************************************/
int skio_check_packet(char *p, int packet_len) {
  short chksum, *p_short;
  long i, n;

  p_short= (short *) p; 
  if( skio_convert_long(*(long*)p)!=packet_len) {
  /*
  printf("packet length error, expected=%d, actual=%d\n",
            packet_len,(*((long*)p)));
   */
    return(-1);
  }

  chksum= 0;
  n=packet_len/2;
  for (i=0; i<n; i++) {
    chksum ^= skio_convert_short(* (p_short+i));
  }
  if(chksum!=0) {
/* 
    printf("checksum error, chksum=%d\n",chksum);
  */
    return(-2);
  }

  return(0);
}


/*****************************************************************
* Form a packet(p_send) with length, sequence number and check sum
*   Send it, receive response(p_rcv)
*   Check response checksum, sequence number, length, packet_code
*
* Do this with time out and redo up to 10 times
*   Return length of received packet or error condition
*   Returns 0 if received packet is busy
*   Errors -1:sendto error, -2:recvfrom error, -3:timeout error
*
* Timeout/bad packet/busy logic changed Sep 29, 2000 by CC Burch
* Written June 2000 by Charles C Burch
*****************************************************************/
int skio_send_receive_packet_old( int fd, char *p_send, int packet_len,
         char *p_rcv, int p_rcv_len) {
  int istat, istat1, istat2;
  static int itry,send_sw,i_delay;
  short packet_code_expected, packet_code_received;

  if(first_call == 0) skio_init(&t_stats);
  t_stats.ntry  = 0;
  skio_insert_packet_seq_number(fd, p_send);
  skio_form_packet(p_send, &packet_len);
  packet_code_expected=
   skio_convert_short(*((short *)(p_send+10)))+1;/*Expected code in rcv packet*/

/*Try sending and receiving up to ten times with increasing timeout */
  i_delay=TWAIT_MIN;
  send_sw=1;
  for (itry=1; itry<=MAXTRYS; itry++) {
    signal(SIGALRM, skio_timeout);    /*set up timeout trap */
    if(sigsetjmp(env2,1) == 0) {
      alarm(i_delay);                 /*set alarm timeout   */

      /* Send out packet, if last packet was not bad   */
      if(send_sw==1) {
        istat=sendto(fd, p_send, packet_len, (unsigned int) 0,
          (struct sockaddr*) 0,  0 );
          /*(struct sockaddr*) 0, 0 ); */
        if(istat!=packet_len) {
          alarm(0);
          signal(SIGALRM,SIG_DFL);
          printf("skio_send_receive_packet: sendto error %d\n",istat);
          return(-1);
        }
      }

      /*    Receiving packet */
      istat=recvfrom(fd, p_rcv, p_rcv_len, (unsigned int) 0,
      (struct sockaddr*) 0, (socklen_t*) 0);

      alarm(0);       /*Got a packet, turn off alarm signal */
      signal(SIGALRM,SIG_DFL);
      if(istat<0) {
        printf("skio_send_receive_packet: recvfrom error,istat=%d\n", istat);
        printf("skio_send_receive_packet: errno =%d\n", errno);
        return(-2);
      }

      /* Check packet len, checksum and sequence number--retry if bad  */
      packet_code_received=skio_convert_short((*((short *)(p_rcv+10))));
      istat1=skio_check_packet(p_rcv, istat);
      istat2=skio_check_packet_seq_number(fd, p_rcv);
      if(istat1>=0 && istat2>=0) {
        if(packet_code_received==packet_code_expected) return(istat);
        if(packet_code_received==PFIO_BUSY_CODE) return(0);
        if(packet_code_received==PFIO_WAKEUP_CODE) {
          printf("skio_send_receive_packet: wakeup received\n");
        }
      }

      /* Bad packet received */
      packet_bad_errors++;
      printf(
       "skio_send_receive_packet: bad packet , istat1=%d, istat2=%d\n",
       istat1, istat2);
      printf(
       "skio_send_receive_packet: code received=%d,code expected=%d\n",
        (int)packet_code_received, (int)packet_code_expected);

      if (istat2 < 0)
         printf(
            "skio_send_receive_packet: seq. # received=%d,seq. # expected=%d\n",
            (int) skio_get_packet_seq_number(p_rcv),
            (int) skio_get_seq_num(fd));

      /* let clients deal with busy logic */
      if(packet_code_received==PFIO_BUSY_CODE) return 0;
      send_sw=2;
      if(packet_code_received==PFIO_BAD_PACKET_CODE) send_sw=1;
      if(packet_code_received==PFIO_WAKEUP_CODE) {
        printf("skio_send_receive_packet: wakeup received\n");
      }

    } else {   /*Entry from timeout signal */
      printf("skio_send_receive_packet:(timeout-%d) expected code=%d\n",
      itry,packet_code_expected);
      send_sw=1;
      i_delay = skio_set_timeout(&t_stats);
    }

    /*-------------Timeout/retry-increase delay and retry-------------*/
    /* i_delay = skio_set_timeout(&t_stats); */
    fflush(stdout);
  }

  packet_timeout_errors++;
  printf("skio_send_receive_packet:(TIMEOUT) expected_code=%d\n",
   packet_code_expected);
  return(-3); /* bad response after max tries-return error       */
}
/*****************************************************************
*  Return number of timeout and bad packet errors
*
*  Written June 2000 by Charles C Burch
*****************************************************************/
int skio_get_packet_errors(long *timeout_errors, long *bad_packet_errors) {
  (*timeout_errors)=packet_timeout_errors;
  (*bad_packet_errors)=packet_bad_errors;
  return(1);
}
/*************************************************************
* skio timeout processing trap
*   turn alarm off, set alarm to default handling 
*   and return timeout occurred
*
* Written June 2000 by Charles C Burch
**************************************************************/
void skio_timeout(int sig) {
  signal(SIGALRM, SIG_IGN);
  alarm(0);
  signal(sig, SIG_DFL);
  siglongjmp(env2, 1);
}

/********************************************************************
*       Establish connection to given host-hostname                 *
*                                                                   *
*       Written June 1999 by Charles C Burch                        *
********************************************************************/
int skio_init_connection(char *hostname, int sinport) { 
  int server_socket_fd;
  struct sockaddr_in address;
  struct hostent *hp;

/*printf("Attempting to establish connection to [%s]\n", hostname);*/
  if ( (hp=gethostbyname(hostname)) == NULL) {
    perror("gethostbyname error in pf client");
    return(-1);
  }
  if ( (server_socket_fd=socket(PF_INET, SOCK_DGRAM, 0)) <0) {
    perror("socket error in pfclient");
    return(-1);
  }
  memset(&address, 0, sizeof(address));
  address.sin_family = PF_INET;
  memcpy(&address.sin_addr.s_addr, hp->h_addr, hp->h_length);
  address.sin_port=htons(sinport);
  if(connect(server_socket_fd,(struct sockaddr *)&address, 
    sizeof(address) )==-1) {
      perror("connect error in pfclient");
      return(-1);
  }
/*printf("connection made on socket fd=%d\n",server_socket_fd);*/
  return(server_socket_fd);
}

/***********************************************************
* return endian type of host machine
*  1-little endian
*  2-oddball-swap bytes in halfwords
*  3-oddball-swap halfwords
*  4-big endian
*
* Written July 2000 by Charles C Burch
************************************************************/
int skio_get_endian_type(){
  long test;
  char *test1;

  if(sizeof(long long) !=8 || sizeof(long)!=4 ||
     sizeof(short)!=2    || sizeof(char)!=1) {
    printf("skio endian logic makes assumption not valid on this machine\n");
    printf("sizeof long-long, long, short, char=%d, %d, %d %d\n",
      (int)sizeof(long long), (int)sizeof(long),
      (int)sizeof(short), (int)sizeof(char));
    printf("Contact CPS programming support\n");
    exit(1);
  }
  test=1+256L*(2+256L*(3+256L*4));
  test1=(char *) &test;
  return( (*test1));
}
/***********************************************************
* Convert native long into little endian
*    or little endian long to native long
* Makes assumption long is 4 bytes
*
* Written July 2000 by Charles C Burch
************************************************************/
long skio_convert_long(long in) {
  char *bytes_in, *bytes_out;
  long i4_in, i4_out;

  if(endian_type==0) endian_type=skio_get_endian_type();
  if(endian_type==1)  return(in);     /*native type is little endian*/

 /* inut needs byte swapping of some sort  */

  i4_in=in;
  bytes_out=(char *) (&i4_out);
  bytes_in=(char *) (&i4_in);

  switch(endian_type) {
  case 2:         /*swap bytes per halfword */
    (*bytes_out++)=(*(bytes_in+1));
    (*bytes_out++)=(*bytes_in);
    (*bytes_out++)=(*(bytes_in+3));
    (*bytes_out)  =(*(bytes_in+2));
    return(i4_out);

  case 3:        /*swap halfwords */
    (*bytes_out++)=(*(bytes_in+2));
    (*bytes_out++)=(*(bytes_in+3));
    (*bytes_out++)=(*(bytes_in));
    (*bytes_out++)=(*(bytes_in+1));
    return(i4_out);

  case 4:        /*swap bytes and halfwords*/
    (*bytes_out++)=(*(bytes_in+3));
    (*bytes_out++)=(*(bytes_in+2));
    (*bytes_out++)=(*(bytes_in+1));
    (*bytes_out++)=(*(bytes_in));
    return(i4_out);
  }
 return i4_in;
}
/***********************************************************
* Convert native short into little endian short
*  or little endian short to native short
* Makes assumption short is 2 bytes
*
* Written July 2000 by Charles C Burch
************************************************************/
short skio_convert_short(short in) {
  char *bytes_in, *bytes_out;
  short i2_in, i2_out;

  if(endian_type==0) endian_type=skio_get_endian_type();

  if(endian_type==1 || endian_type==3) return(in);  /*no byte swap */

/*byte swap */

  i2_in=in;
  bytes_in=(char *) (&i2_in);
  bytes_out=(char *)(&i2_out);
  (*bytes_out++)=(*(bytes_in+1));
  (*bytes_out)=(*bytes_in);
  return(i2_out);
}
/***********************************************************
* Convert native char to little endian char
*  or little endian to native char
* Makes assumption char is one byte
* This currently a no-op but included for completeness
*
* Written July 2000 by Charles C Burch
************************************************************/
char skio_convert_char(char in) {
  return(in);
}
/***********************************************************
* Convert native long long into little endian
*    or little endian long long to native long long
* Makes assumption long is 8 bytes
* !!!!CASES endian type 2 and 3 Untested & may need work!!!!
*
* Written July 2000 by Charles C Burch
************************************************************/
long long skio_convert_long_long(long long in) {
  char *bytes_in, *bytes_out;
  long long i8_in, i8_out;

  if(endian_type==0) endian_type=skio_get_endian_type();
  if(endian_type==1)  return(in);     /*native type is little endian*/

 /* input needs byte swapping of some sort  */

  i8_in=in;
  bytes_out=(char *) (&i8_out);
  bytes_in=(char *) (&i8_in);

  switch(endian_type) {
  case 2:         /*swap bytes per halfword */
    (*bytes_out++)=(*(bytes_in+1));
    (*bytes_out++)=(*bytes_in);
    (*bytes_out++)=(*(bytes_in+3));
    (*bytes_out)  =(*(bytes_in+2));
    (*bytes_out++)=(*(bytes_in+5));
    (*bytes_out++)=(*(bytes_in+4));
    (*bytes_out++)=(*(bytes_in+3));
    (*bytes_out)  =(*(bytes_in+2));
    return(i8_out);

  case 3:        /*swap halfwords */
    (*bytes_out++)=(*(bytes_in+6));
    (*bytes_out++)=(*(bytes_in+7));
    (*bytes_out++)=(*(bytes_in+4));
    (*bytes_out++)=(*(bytes_in+5));
    (*bytes_out++)=(*(bytes_in+2));
    (*bytes_out++)=(*(bytes_in+3));
    (*bytes_out++)=(*(bytes_in));
    (*bytes_out++)=(*(bytes_in+1));
    return(i8_out);

  case 4:        /*swap bytes and halfwords*/
    (*bytes_out++)=(*(bytes_in+7));
    (*bytes_out++)=(*(bytes_in+6));
    (*bytes_out++)=(*(bytes_in+5));
    (*bytes_out++)=(*(bytes_in+4));
    (*bytes_out++)=(*(bytes_in+3));
    (*bytes_out++)=(*(bytes_in+2));
    (*bytes_out++)=(*(bytes_in+1));
    (*bytes_out++)=(*(bytes_in));
    return(i8_out);
  }
 return i8_in;
}
/***********************************************************************
*       Junk a packet from socket fd using buffer p of len packet_len  *
*                                                                      *
*       Written Sept 1999 by Charles C Burch                           *
***********************************************************************/
long skio_trash_packet(int fd, char *p, long packet_len) {
    long nread=0;
    int istat, packet_size;
    long iread;
#ifdef SKIO_STUB
return 0L;
}
#else
/*  printf("pfjunk packet, len=%d\n",packet_len); */
    if(packet_len>32764) {
      packet_size=32764;
    } else {
      packet_size=packet_len;
    }
    while(nread<packet_len) {
      if( (iread=packet_len-nread) > packet_size) iread=packet_size;
      istat=recv(fd,p,(int)iread,0);
      if (istat>0) {
          if(nread==0) {
            packet_len = (*((long*) p));
            printf("skio_read_packet, packet_len=%ld\n",packet_len);
          }
      nread+=istat;
      } else {
      break;
      }
    }
    return(nread);
}
#endif


void skio_init(sr_time_stats *ts)
{
  ts->t_base  = 0;
  ts->t_alarm = TWAIT_MIN;
  ts->t_avg   = 0;
  ts->t_reply = 0;
  ts->t_sigma = .50;
  skio_tstamp(ts);
  ts->ntry  = 0;
  first_call += 1;

}

/* time stamp in milli seconds */
long skio_tstamp(sr_time_stats *ts)
{ struct timeval tv;
  long tstamp;
  int i_err;

  i_err =  gettimeofday(&tv, NULL);
  if(i_err < 0) return -1;
  /* tv.tv_sec;   seconds since Jan. 1, 1970 */
  /* tv.tv_usec;  and microseconds */
  if(first_call==0) ts->t_base = tv.tv_sec;
  if((tv.tv_sec - ts->t_base) > 1700000) {
    ts->t_base = tv.tv_sec;
  }
  tstamp = (tv.tv_sec - ts->t_base)*1000 + tv.tv_usec/1000;
  /* printf("tstamp: base=%ld usec=%ld\n",*tsec,*tusec); */
  return tstamp;
}

/* call this after a timeout */
int  skio_set_timeout(sr_time_stats *ts)
{ int itime;
  ts->ntry++;
  if(ts->ntry > MAXTRYS) {
    ts->t_alarm = TWAIT_MIN;
    return TWAIT_MIN;
  }
  /* double the time */
  ts->t_alarm = 2*ts->t_alarm;
  /* honor the limits */
  if(ts->t_alarm < TWAIT_MIN) ts->t_alarm = TWAIT_MIN;
  if(ts->t_alarm > TWAIT_MAX) ts->t_alarm = TWAIT_MAX;
  itime = ts->t_alarm;
  return itime;
}

void   skio_stop(sr_time_stats *ts, long t_reply)
{ double delta;
  ts->t_reply = t_reply/1000; /* Last reply time in msec */

  delta = ts->t_reply - ts->t_avg;
  ts->t_avg += delta/8;
  if(delta<0) delta = -delta;

  ts->t_sigma += (delta - ts->t_sigma)/4;
  ts->t_alarm = (ts->t_avg + (4.0 * ts->t_sigma) );
}
/*****************************************************************
* Form a packet(p_send) with length, sequence number and check sum
*   Send it, receive response(p_rcv)
*   Check response checksum, sequence number, length, packet_code
*
* Do this with time out and redo up to 10 times
*   Return length of received packet or error condition
*   Returns 0 if received packet is busy
*   Errors -1:sendto error, -2:recvfrom error, -3:timeout error
*
* Timeout/bad packet/busy logic changed Sep 29, 2000 by CC Burch
* Written Dec 2000 by R.S. Day
*****************************************************************/
int skio_sndrecv_addr( int fd, char *p_send, int packet_len,
         char *p_rcv, int p_rcv_len, struct sockaddr_in *addr) {
  int istat, istat1, istat2;
  static int itry,send_sw,i_delay;
  short packet_code_expected, packet_code_received;
  int len;

  if(first_call == 0) skio_init(&t_stats);
  t_stats.ntry  = 0;
  skio_insert_packet_seq_number(fd, p_send);
  skio_form_packet(p_send, &packet_len);
  packet_code_expected=
   skio_convert_short(*((short *)(p_send+10)))+1;/*Expected code in rcv packet*/

/*Try sending and receiving up to ten times with increasing timeout */
  i_delay=TWAIT_MIN;
  send_sw=1;
  for (itry=1; itry<=MAXTRYS; itry++) {
    signal(SIGALRM, skio_timeout);    /*set up timeout trap */
    if(sigsetjmp(env2,1) == 0) {
      alarm(i_delay);                 /*set alarm timeout   */

      /* Send out packet, if last packet was not bad   */
      if(send_sw==1) {
       restart3:
        istat=sendto(fd,p_send,(int) packet_len, (unsigned int) 0, 
          (struct sockaddr*) addr, (int) sizeof(struct sockaddr_in) );
        if(istat!=packet_len) {
          if(istat < 0) {
           if(errno==EINTR) goto restart3;
           printf("skio_...:sendto error, errno=%d\n",errno);
          }
          alarm(0);
          signal(SIGALRM,SIG_DFL);
          printf("skio_sndrecv_addr: sendto error %d\n",istat);
          return(-1);
        }
      }

      /*    Receiving packet */
      len = sizeof(struct sockaddr_in);
      istat=recvfrom(fd, p_rcv, p_rcv_len, (unsigned int) 0,
      (struct sockaddr*) &addr, (socklen_t*) &len);

      alarm(0);       /*Got a packet, turn off alarm signal */
      signal(SIGALRM,SIG_DFL);
      if(istat<0) {
        printf("skio_sndrecv_addr: recvfrom error,istat=%d\n", istat);
        printf("skio_sndrecv_addr: errno =%d\n", errno);
        return(-2);
      }

      /* Check packet len, checksum and sequence number--retry if bad  */
      packet_code_received=skio_convert_short((*((short *)(p_rcv+10))));
      istat1=skio_check_packet(p_rcv, istat);
      istat2=skio_check_packet_seq_number(fd, p_rcv);
      if(istat1>=0 && istat2>=0) {
        if(packet_code_received==packet_code_expected) return(istat);
        if(packet_code_received==PFIO_BUSY_CODE) return(0);
      }

      /* Bad packet received */
      packet_bad_errors++;
      printf(
       "skio_sndrecv_addr: bad packet , istat1=%d, istat2=%d\n",
       istat1, istat2);
      printf(
       "skio_sndrecv_addr: code received=%d,code expected=%d\n",
        (int)packet_code_received, (int)packet_code_expected);

      if (istat2 < 0)
         printf(
            "skio_sndrecv_addr: seq. # received=%d,seq. # expected=%d\n",
            (int) skio_get_packet_seq_number(p_rcv),
            (int) skio_get_seq_num(fd));

      /* let clients deal with busy logic */
      if(packet_code_received==PFIO_BUSY_CODE) return 0;
      send_sw=2;
      if(packet_code_received==PFIO_BAD_PACKET_CODE) send_sw=1;

    } else {   /*Entry from timeout signal */
      printf("skio_sndrecv_addr:(timeout-%d) expected code=%d\n",
      itry,packet_code_expected);
      send_sw=1;
      i_delay = skio_set_timeout(&t_stats);
    }

    /*-------------Timeout/retry-increase delay and retry-------------*/
    /* i_delay = skio_set_timeout(&t_stats); */
    fflush(stdout);
  }

  packet_timeout_errors++;
  printf("skio_sndrecv_addr:(TIMEOUT) expected_code=%d\n",
   packet_code_expected);
  return(-3); /* bad response after max tries-return error       */
}

/*
 * Functions to handle separate series of sequence numbers for each fd.
 * This enables skio users make sure they are getting packets in the right
 * order.  Prior to these functions, the sequence number was a single static
 * variable so if there were multiple skio channels, the best you could
 * do is check for a repeat packet.  Sequence numbers range from FIRST_SEQ_NUM
 * to LAST_SEQ_NUM, wrapping back to LAST_SEQ_NUM when FIRST_SEQ_NUM is
 * exceeded.  Sequence numbers for each fd start at FIRST_SEQ_NUM and are
 * continuous for the entire program; there is no effort to initialize when
 * an fd is opened so if the same fd is used multiple times in the same
 * program it will not restart at FIRST_SEQ_NUM with each open.
 * ehs 03jul01
 */
/*
 * Lazy allocation for _seq_num_ary.
 * Initialize to FIRST_SEQ_NUM - 1 since sequence numbers should be incremented
 * before each use.
 */
void skio_chk_seq_num_ary(int fd)
{
   if (fd > _max_fd_for_seq_num_ary)
   {
      int i;

      /*
       * fd + 1 since we need from 0 to fd.
       */
      long *tmp = (long*)malloc((size_t) (fd + 1) * sizeof(long));

      if (!tmp)
      {
         fprintf(stderr, "skio_chk_seq_num_ary:  out of memory\n");
         exit(1);
      }

      for (i = 0; i <= _max_fd_for_seq_num_ary; i++)
         tmp[i] = _seq_num_ary[i];

      for (; i <= fd; i++)
         tmp[i] = (long) (FIRST_SEQ_NUM - 1);

      if (_max_fd_for_seq_num_ary != -1)
         free(_seq_num_ary);

                 _seq_num_ary = tmp;
      _max_fd_for_seq_num_ary = fd ;
   }
}

long skio_get_seq_num(int fd)
{
   skio_chk_seq_num_ary(fd);

   return _seq_num_ary[fd];
}

void skio_set_seq_num(int fd, long seq_num)
{
   skio_chk_seq_num_ary(fd);

   _seq_num_ary[fd] = seq_num;
}

/*
 * Only sequence number routine callable from outside skio.
 * Given the current sequence number returns what will be the next sequence
 * number.  Does not update anything in _seq_num_ary.  Useful for routines
 * using skio to check sequence numbers in received packets.
 */
long skio_next_seq_num(long curval)
{
   long retval = curval + 1L;

   if (retval > LAST_SEQ_NUM)
      retval = FIRST_SEQ_NUM;

   return retval;
}

long skio_inc_seq_num(int fd)
{
   long curval = skio_get_seq_num (fd    );
   long retval = skio_next_seq_num(curval);

   skio_set_seq_num(fd, retval);

   return retval;
}

/*****************************************************************
* Form a packet-send it and wait for response while checking for
* errors/timeouts and resend if needed
*
* Written June 2000 by Charles C Burch
*****************************************************************/
int skio_send_receive_packet(int fd, char *p_send, int packet_len,
         char *p_rcv, int p_rcv_len) {

 return (skio_send_receive_packet_check(fd, p_send, packet_len,
        p_rcv, p_rcv_len,1));
}

/*****************************************************************
* Form a packet(p_send) with length, sequence number and check sum
*   and send it.
*
* Written February 2003 by Charles C Burch
*****************************************************************/
int skio_send_receive_packet_start( int fd, char *p_send, int packet_len) {
  int istat;

  if(first_call == 0) skio_init(&t_stats);
  t_stats.ntry  = 0;
  
  skio_insert_packet_seq_number(fd,p_send);
  skio_form_packet(p_send, &packet_len);

  istat=sendto(fd, p_send, packet_len, (unsigned int) 0,
      (struct sockaddr*) 0,  0 );
  if(istat!=packet_len) {
    if(istat>=0) {
      printf("skio_send_receive_packet_start: sendto error %d\n",istat);
    } else {
      perror("skio_send_receive_packet_start sendto error:");
   }
   return(-1);
  }
  return(0);
}

/*****************************************************************
* Wait for packet response after being sent by
* skio_send_receive_packet_start
*
* Written February 2003 by Charles C Burch
*****************************************************************/
int skio_send_receive_packet_finish(int fd, char *p_send, int packet_len,
         char *p_rcv, int p_rcv_len) {
  return (skio_send_receive_packet_check(fd, p_send, packet_len,
         p_rcv, p_rcv_len,0));
}

/*********************************************************************
* Wait for packet response to come back after packet was sent.
* send_sw=1 means to nitially send the packet before trying to
* receive.
* send_sw=0 means packet was initially send by skio_send_receive_start
*
* Do this with time out and redo with resend up to 10 times
*   Return length of received packet or error condition
*   Returns 0 if received packet is busy
*   Errors -1:sendto error, -2:recvfrom error, -3:timeout error
*
* Written February 2003 by Charles C Burch
********************************************************************/
int skio_send_receive_packet_check( int fd, char *p_send, int packet_len,
         char *p_rcv, int p_rcv_len, int send_sw) {
  int istat, istat1, istat2;
  static int itry, i_delay;
  static int send_sav;
  short packet_code_expected;
  short packet_code_received;
  float t_alarm_old;

  send_sav = send_sw;
  if(first_call == 0) {
    t_alarm_old = t_stats.t_alarm;
    skio_init(&t_stats);
    /* in case user specified an alarm time before first call */
    skio_set_alarm_time(t_alarm_old);
  }
  t_stats.ntry  = 0;

  if(send_sav == 1) { /* starting new packet */
    skio_insert_packet_seq_number(fd,p_send);
    skio_form_packet(p_send, &packet_len);

  }
  /*
   * caller must save the send packet!
   */
  packet_code_expected=
   skio_convert_short(*((short *)(p_send+10)))+1;/*Expected code in rcv packet*/

/*Try sending and receiving up to ten times with increasing timeout */
  i_delay=t_stats.t_alarm; /*TWAIT_MIN; */
  for (itry=1; itry<=MAXTRYS; itry++) {
    signal(SIGALRM, skio_timeout);    /*set up timeout trap */
    if(sigsetjmp(env2,1) == 0) {
      alarm(i_delay);                 /*set alarm timeout   */

      /* Send out packet, if last packet was not bad   */
      if(send_sav==1) {
        istat=sendto(fd, p_send, packet_len, (unsigned int) 0,
          (struct sockaddr*) 0,  0 );
          /*(struct sockaddr*) 0, 0 ); */
        if(istat!=packet_len) {
          alarm(0);
          signal(SIGALRM,SIG_DFL);
          if(istat>=0) {
            printf("skio_send_receive_packet_check: sendto error %d\n",istat);
          } else {
            perror("skio_send_receive_packet_check sendto error:");
          }
          return(-1);
        }
      }

      /*    Receiving packet */
      istat=recvfrom(fd, p_rcv, p_rcv_len, (unsigned int) 0,
      (struct sockaddr*) 0, (socklen_t*) 0);

      alarm(0);       /*Got a packet, turn off alarm signal */
      signal(SIGALRM,SIG_DFL);
      if(istat<0) {
        printf("skio_send_receive_packet_check: recvfrom error(%d),istat=%d\n",
         errno,istat);
        perror("skio_send_receive_packet_check recvfrom error:");
        return(-2);
      }

      /* Check packet len, checksum and sequence number--retry if bad  */
      packet_code_received=skio_convert_short((*((short *)(p_rcv+10))));
      istat1=skio_check_packet(p_rcv, istat);
      istat2=skio_check_packet_seq_number(fd,p_rcv);
      if(istat1>=0 && istat2>=0) {
        if(packet_code_received==packet_code_expected) return(istat);
        if(packet_code_received==PFIO_BUSY_CODE) return(0);
        if(packet_code_received==PFIO_WAKEUP_CODE) {
          printf("skio_send_receive_packet_check: wakeup received\n");
        }
      }

      /* Bad packet received */
      packet_bad_errors++;
      printf(
       "skio_send_receive_packet_check: bad packet , istat1=%d, istat2=%d\n",
       istat1, istat2);
      printf(
       "skio_send_receive_packet_check: code received=%d,code expected=%d\n",
        (int)packet_code_received, (int)packet_code_expected);
      printf(
       "skio_send_receive_packet_check: packet seqno=%ld, skio seqno=%ld\n",
        skio_convert_long((*(long*)(p_rcv+4))),
        skio_get_seq_num(fd));

      /* let clients deal with busy logic */
      if(packet_code_received==PFIO_BUSY_CODE) return 0;
      send_sav=2;
      if(packet_code_received==PFIO_BAD_PACKET_CODE) send_sav=1;
      if(packet_code_received==PFIO_WAKEUP_CODE) {
        printf("skio_send_receive_packet_check: wakeup received\n");
      }

    } else {   /*Entry from timeout signal */
      printf("skio_send_receive_packet_check:(timeout-%d) expected code=%d\n",
      itry,packet_code_expected);
      send_sav=1;
      i_delay = skio_set_timeout(&t_stats);
    }

    /*-------------Timeout/retry-increase delay and retry-------------*/
    /* i_delay = skio_set_timeout(&t_stats); */
    fflush(stdout);
  }

  packet_timeout_errors++;
  printf("skio_send_receive_packet_check:(TIMEOUT) expected_code=%d\n",
   packet_code_expected);
  return(-3); /* bad response after max tries-return error       */
}

void skio_set_alarm_time(int wait_time_in_sec)
{
  t_stats.t_alarm = wait_time_in_sec;
  if(t_stats.t_alarm< TWAIT_MIN) t_stats.t_alarm = TWAIT_MIN;
  if(t_stats.t_alarm> TWAIT_MAX) t_stats.t_alarm = TWAIT_MAX;
}

#ifdef __cplusplus
}
#endif

/*-------------------------------- end ------------------------------------*/
/*-------------------------------- end ------------------------------------*/
/*-------------------------------- end ------------------------------------*/
