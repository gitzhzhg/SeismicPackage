/*<CPS_v1 type="HEADER_FILE",pretag="!"/>
!------------------------------- skio.h ----------------------------------
!------------------------------- skio.h ----------------------------------
!------------------------------- skio.h ----------------------------------
!other files are:                skio.c
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
! Name       : SKIO.H
! Category   : io
! Written    : 1999-09-15   by: Charles C. Burch
! Revised    : 2003-08-08   by: R.S.Day
! Maturity   : production   2003-09-29
! Purpose    : Provides an interface into socket-based i/o
! References : These routines are called from within pfio.c
!</brief_doc>
!<history_doc>
!-------------------------------------------------------------------------------
!                       HEADER FILE REVISION HISTORY
!     Date        Author          Description
!     ----        ------          -----------
! 10. 2003-09-29  R.S.Day         Added skio_set_alarm_time
!  9. 2003-05-19 R.S.Day          Added prototypes for new functions that
!                                 support overlapped I/O for tapes.
!                                 skio_send_receive_packet_start
!                                 skio_send_receive_packet_finish
!                                 skio_send_receive_packet_check
!  8. 2001-08-01 Ed Schmauch      Added skio_insert_packet_seq_number because
!                                 tpioclnt_finalize needs to call it.
!  7. 2001-07-10 Ed Schmauch      Keep a different series of sequence numbers
!                                 for each fd.  Only the skio_next_seq_num is
!                                 callable from outside of skio and included
!                                 in this header file.
!  6. 2001-06-27 Chuck C. Burch   Added use of c2f_interface.h.  PRODUCTION.
!  5. 2001-03-21 R.S.Day          Added prototype for skio_sndrecv_addr
!  4. 2000-10-02 R.S.Day          Added busy code. Define kill,bad codes
!                                 if not defined.
!  3. 2000-09-28 R.S.Day          Added byte swapping function and the
!                                 function skio_get_packet_seq_number
!  2. 2000-08-17  Chuck C. Burch  Robust socket i/o.
!  1. 2000-04-10  Chuck C. Burch  Initial version.
!-------------------------------------------------------------------------------
!</history_doc>
*/

/*--------------------------- start of coding ------------------------------*/
/*--------------------------- start of coding ------------------------------*/
/*--------------------------- start of coding ------------------------------*/

#ifndef _SKIO_H_
#define _SKIO_H_

#include "c2f_interface.h"

#ifdef NEED_UNDERSCORE

#define skio_check_packet             skio_check_packet_
#define skio_check_packet_seq_num     skio_check_packet_seq_num_
#define skio_convert_long_long        skio_convert_long_long_
#define skio_convert_long             skio_convert_long_
#define skio_convert_short            skio_convert_short_
#define skio_convert_char             skio_convert_char_
#define skio_form_packet              skio_form_packet_
#define skio_get_endian_type          skio_get_endian_type_
#define skio_get_packet_errors        skio_get_packet_errors_
#define skio_init_connection          skio_init_connection_
#define skio_insert_packet_seq_num    skio_insert_packet_seq_num_
#define skio_read_packet              skio_read_packet_
#define skio_send_receive_packet      skio_send_receive_packet_
#define skio_send_receive_packet_start    skio_send_receive_packet_start_
#define skio_send_receive_packet_finish   skio_send_receive_packet_finish_
#define skio_send_receive_packet_check    skio_send_receive_packet_check_
#define skio_sndrecv_addr             skio_sndrecv_addr_
#define skio_write_packet             skio_write_packet_
#define skio_insert_packet_seq_number skio_insert_packet_seq_number_
#define skio_next_seq_num             skio_next_seq_num_
#endif

#ifdef NEED_CAPITALS
#define skio_check_packet             SKIO_CHECK_PACKET
#define skio_check_packet_seq_num     SKIO_CHECK_PACKET_SEQ_NUM
#define skio_convert_long_long        SKIO_CONVERT_LONG_LONG
#define skio_convert_long             SKIO_CONVERT_LONG
#define skio_convert_short            SKIO_CONVERT_SHORT
#define skio_convert_char             SKIO_CONVERT_CHAR
#define skio_form_packet              SKIO_FORM_PACKET
#define skio_get_endian_type          SKIO_GET_ENDIAN_TYPE
#define skio_get_packet_errors        SKIO_GET_PACKET_ERRORS
#define skio_init_connection          SKIO_INIT_CONNECTION
#define skio_insert_packet_seq_num    SKIO_INSERT_PACKET_SEQ_NUM
#define skio_read_packet              SKIO_READ_PACKET
#define skio_send_receive_packet      SKIO_SEND_RECEIVE_PACKET
#define skio_send_receive_packet_start    SKIO_SEND_RECEIVE_PACKET_START
#define skio_send_receive_packet_finish   SKIO_SEND_RECEIVE_PACKET_FINISH
#define skio_send_receive_packet_check    SKIO_SEND_RECEIVE_PACKET_CHECK
#define skio_sndrecv_addr             SKIO_SNDRECV_ADDR
#define skio_write_packet             SKIO_WRITE_PACKET
#define skio_insert_packet_seq_number SKIO_INSERT_PACKET_SEQ_NUMBER
#define skio_next_seq_num             SKIO_NEXT_SEQ_NUM
#endif

#ifndef PFIO_WAKEUP_CODE
#define PFIO_WAKEUP_CODE   ((short) 9995)
#endif

#ifndef PFIO_BUSY_CODE
#define PFIO_BUSY_CODE     ((short) 9997)
#endif

#ifndef PFIO_BAD_PACKET_CODE
#define PFIO_BAD_PACKET_CODE ((short) 9998)
#endif

#ifndef PFIO_KILL_CODE
#define PFIO_KILL_CODE     ((short) 9999)
#endif

/******************General non IO packets*********************/

struct pfio_busy_packet_struct {
  long length;
  long seq_num;
  short chksum;
  short type;
};

#ifdef __cplusplus
extern "C" {
#endif

/*char *skio_h_ident =*/
/*"$Id: skio.h,v 1.10 2003/09/24 18:47:31 R.S.Day prod sps $";*/


/*------------------------ start of prototypes ---------------------------*/
/*------------------------ start of prototypes ---------------------------*/
/*------------------------ start of prototypes ---------------------------*/

/****************************skio.h*******************/
int       skio_check_packet(char*, int);
int       skio_check_packet_seq_num(char*);
long long skio_convert_long_long(long long);
long      skio_convert_long(long);
short     skio_convert_short(short);
char      skio_convert_char(char);
int       skio_form_packet(char*, int*);
int       skio_get_endian_type();
int       skio_get_packet_errors(long*,long*);
long      skio_get_packet_seq_number (char *p);
int       skio_init_connection(char*, int);
int       skio_insert_packet_seq_num(char*);
int       skio_read_packet(int, char*, int);
int       skio_send_receive_packet(int, char*, int, char*, int);
int       skio_send_receive_packet_check( int fd, char *p_send, int packet_len,
         char *p_rcv, int p_rcv_len, int send_sw);
int       skio_send_receive_packet_start( int fd, char *p_send, int packet_len);
int       skio_send_receive_packet_finish(int fd, char *p_send, int packet_len,
         char *p_rcv, int p_rcv_len);
int       skio_sndrecv_addr( int fd, char *p_send, int packet_len,
             char *p_rcv, int p_rcv_len, struct sockaddr_in *addr);
int       skio_write_packet(int, char*, int);
int       skio_insert_packet_seq_number(int fd, char *p);
long      skio_next_seq_num(long curval);
void      skio_set_alarm_time(int wait_time_in_sec);


/*------------------------- end of prototypes ---------------------------*/
/*------------------------- end of prototypes ---------------------------*/
/*------------------------- end of prototypes ---------------------------*/

#ifdef __cplusplus
}
#endif

#endif

/*-------------------------------- end ------------------------------------*/
/*-------------------------------- end ------------------------------------*/
/*-------------------------------- end ------------------------------------*/
