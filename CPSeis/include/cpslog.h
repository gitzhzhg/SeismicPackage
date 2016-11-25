/*<CPS_v1 type="HEADER_FILE"/>
!------------------------------- cpslog.h ----------------------------------
!------------------------------- cpslog.h ----------------------------------
!------------------------------- cpslog.h ----------------------------------

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
! Name       : CPSLOG.H
! Category   : io
! Written    : 2002-06-18   by: Charles C. Burch
! Revised    : 2007-01-25   by: Bill Menger
! Maturity   : beta
! Purpose    : Provides an interface to the CPS log file routines.
! References : These routines are for general use to write to the cps logfile
!-------------------------------------------------------------------------------
!</brief_doc>

!<history_doc>
!-------------------------------------------------------------------------------
!                       HEADER FILE REVISION HISTORY
!     Date        Author          Description
!     ----        ------          -----------
!  2. 2007-01-25  Bill Menger     Modified to match lgc version.
!  1. 2002-07-01  Chuck C. Burch  Initial version.
!-------------------------------------------------------------------------------
!</history_doc>
*/

/*--------------------------- start of coding ------------------------------*/
/*--------------------------- start of coding ------------------------------*/
/*--------------------------- start of coding ------------------------------*/

#ifndef _CPSLOG_H_
#define _CPSLOG_H_

#include "c2f_interface.h"
#include "cnfg.h"

#ifdef NEED_UNDERSCORE
#define cpslog_exit                 cpslog_exit_
#define cpslog_init                 cpslog_init_
#define cpslog_message              cpslog_message_
#define cpslog_redirect             cpslog_redirect_
#endif

#ifdef NEED_CAPITALS
#define cpslog_exit                 CPSLOG_EXIT        
#define cpslog_init                 CPSLOG_INIT
#define cpslog_message              CPSLOG_MESSAGE       
#define cpslog_redirect             CPSLOG_REDIRECT
#endif

#ifdef __cplusplus
extern "C" {
#endif

/*char *cpslog_h_ident =*/
/*"$Id: cpslog.h,v 1.2 2007/01/26 14:22:07 Menger beta sps $";*/

/*------------------------ start of prototypes ---------------------------*/
/*------------------------ start of prototypes ---------------------------*/
/*------------------------ start of prototypes ---------------------------*/

/*        function prototypes for cpslog  routines    */

int cpslog_redirect();
void cpslog_exit(); 
void cpslog_init(); 
void cpslog_message(char*);

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
