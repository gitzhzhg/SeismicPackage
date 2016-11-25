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

/*----------------------- va_message.h ----------------------------------*/

/*
   the defined constants must exactly match corresponding parameters in the
   Fortran include file va_message.inc
*/

#ifndef _MESSAGE_H
#define _MESSAGE_H

#include "c2f_interface.h"

/*-------------- fortran subroutine spelling adjustments ---------------*/

#if (VMS || _AIX || __hpux )
#define fbroadcast_    fbroadcast
#define message_wbox_  message_wbox
#endif

#ifdef NEED_CAPITALS
#define fbroadcast_    FBROADCAST
#define message_wbox_  MESSAGE_WBOX
#endif



/*--------------------- function prototypes ----------------------------*/

#define ARGUMENTS   long sender, long message,                     \
              long one, long two, long three, long four, long five

#define FORTARGS    long *sender, long *message,                   \
          long *one, long *two, long *three, long *four, long *five

#ifdef __cplusplus  
extern "C" {                          /* for C++ */
#endif

void fbroadcast_   (void **vel2, FORTARGS);
void broadcast     (void *vel  , ARGUMENTS);
void message_semb  (void *vel  , ARGUMENTS);
void message_cmp   (void *vel  , ARGUMENTS);
void message_gvs   (void *vel  , ARGUMENTS);
void message_iso   (void *vel  , ARGUMENTS);
void message_wbox_ (             FORTARGS);

#ifdef __cplusplus  
}
#endif

#undef ARGUMENTS
#undef FORTARGS



/*-------------- defined constants for sender --------------------------*/

#define SENDER_WBOX  1L  /* sender of message is a windowbox popup   */
#define SENDER_SEMB  2L  /* sender of message is semblance window    */
#define SENDER_CMP   3L  /* sender of message is cmp window          */
#define SENDER_GVS   4L  /* sender of message is gvs window          */
#define SENDER_ISO   5L  /* sender of message is iso-velocity window */



/*-------------- defined constants for message -------------------------*/

#define MESSAGE_NEWFILE 1L   /* new velocity file (or none)            */
#define MESSAGE_NUMBER  2L   /* number of velocity functions changed   */
#define MESSAGE_SORT    3L   /* velocity functions sorted              */
#define MESSAGE_MODIFY  4L   /* velocity functions modified            */
#define MESSAGE_INSERT  5L   /* one velocity function inserted         */
#define MESSAGE_REMOVE  6L   /* one velocity function removed          */
#define MESSAGE_ACTIVE  7L   /* switched to different active function  */
#define MESSAGE_XHEAD   8L   /* changed X header word number           */
#define MESSAGE_YHEAD   9L   /* changed Y header word number           */
#define MESSAGE_XBIN   10L   /* changed X bin coordinate               */
#define MESSAGE_YBIN   11L   /* changed Y bin coordinate               */
#define MESSAGE_TOL    12L   /* changed bin tolerances (centers & widt)*/
#define MESSAGE_ORDER  13L   /* changed NMO order                      */


#endif

/*--------------------------- end --------------------------------------*/
