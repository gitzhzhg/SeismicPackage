/*<CPS_v1 type="HEADER_FILE"/>
!------------------------------- dbug.h ----------------------------------
!------------------------------- dbug.h ----------------------------------
!------------------------------- dbug.h ----------------------------------

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
! Name       : DBUG.H
! Category   : main
! Written    : 2002-07-18   by: Charles C. Burch
! Revised    : 2002-07-25   by: Charles C. Burch
! Maturity   : production   2002-08-12
! Purpose    : Provides an interface to debug routines in dbug_crou.c
! References : These routines are for general use from either c or Fortram 
!-------------------------------------------------------------------------------
!</brief_doc>

!<history_doc>
!-------------------------------------------------------------------------------
!                       HEADER FILE REVISION HISTORY
!     Date        Author          Description
!     ----        ------          -----------
!  2. 2002-08-12  C. C. Burch     Add system debug messages
!  1. 2002-07-18  C. C. Burch     Initial version.
!-------------------------------------------------------------------------------
!</history_doc>
*/

/*--------------------------- start of coding ------------------------------*/
/*--------------------------- start of coding ------------------------------*/
/*--------------------------- start of coding ------------------------------*/

#ifndef _DBUG_H_
#define _DBUG_H_

#include "c2f_interface.h"

#ifdef NEED_UNDERSCORE
#define dbug_get_message_c            dbug_get_message_c_
#define dbug_get_system_message_c     dbug_get_system_message_c_
#define dbug_get_message_f            dbug_get_message_f_
#define dbug_get_system_message_f     dbug_get_system_message_f_
#define dbug_install_signal_handler_c dbug_install_signal_handler_c_
#define dbug_raise_signal             dbug_raise_signal_
#define dbug_set_message_c            dbug_set_message_c_
#define dbug_set_system_message_c     dbug_set_system_message_c_
#define dbug_set_message_f            dbug_set_message_f_
#define dbug_set_system_message_f     dbug_set_system_message_f_
#endif

#ifdef NEED_CAPITALS
#define dbug_get_message_c            DBUG_GET_MESSAGE_C
#define dbug_get_system_message_c     DBUG_GET_SYSTEM_MESSAGE_C
#define dbug_get_message_f            DBUG_GET_MESSAGE_F
#define dbug_get_system_message_f     DBUG_GET_SYSTEM_MESSAGE_F
#define dbug_install_signal_handler_c DBUG_INSTALL_SIGNAL_HANDLER_C
#define dbug_raise_signal             DBUG_RAISE_SIGNAL
#define dbug_set_message_c            DBUG_SET_MESSAGE_C
#define dbug_set_system_message_c     DBUG_SET_SYSTEM_MESSAGE_C
#define dbug_set_message_f            DBUG_SET_MESSAGE_F
#define dbug_set_system_message_f     DBUG_SET_SYSTEM_MESSAGE_F
#endif


#ifdef __cplusplus
extern "C" {
#endif

/*char *dbug_h_ident =*/
/*"$Id: dbug.h,v 1.2 2002/08/07 21:29:34 CCBurch prod sps $";*/

/*------------------------ start of prototypes ---------------------------*/
/*------------------------ start of prototypes ---------------------------*/
/*------------------------ start of prototypes ---------------------------*/

/*        function prototypes for dbug  routines    */

int   dbug_alpha_to_sig(char*);
void  dbug_install_signal_handler_c();
void  dbug_get_message_c(char*);
void  dbug_get_system_message_c(char*);
void  dbug_get_message_f(char*, INTEGER*);
void  dbug_get_system_message_f(char*, INTEGER*);
void  dbug_raise_signal(INTEGER*);
void  dbug_set_message_c(char*);
void  dbug_set_system_message_c(char*);
void  dbug_set_message_f(char*, INTEGER*);
void  dbug_set_system_message_f(char*, INTEGER*);
char* dbug_sig_to_alpha(int);

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
