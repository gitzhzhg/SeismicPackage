/*<CPS_v1 type="HEADER_FILE"/>
!------------------------------- cpsacct.h ----------------------------------
!------------------------------- cpsacct.h ----------------------------------
!------------------------------- cpsacct.h ----------------------------------

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
! Name       : CPSACCT.H
! Category   : io
! Written    : 2006-01-26   by: Bill Menger
! Revised    : 2007-01-25   by: Bill Menger
! Maturity   : beta
! Purpose    : Provides an interface to the CPS accounting log file routines.
! References : These routines are for general use to write to the cps acct file
!-------------------------------------------------------------------------------
!</brief_doc>

!<history_doc>
!-------------------------------------------------------------------------------
!                       HEADER FILE REVISION HISTORY
!     Date        Author          Description
!     ----        ------          -----------
!  2. 2007-01-25  Bill Menger     Modified to match LGC's version.
!  1. 2006-01-31  Bill Menger     Initial version.
!-------------------------------------------------------------------------------
!</history_doc>
*/

/*--------------------------- start of coding ------------------------------*/
/*--------------------------- start of coding ------------------------------*/
/*--------------------------- start of coding ------------------------------*/

#ifndef _CPSACCT_H_
#define _CPSACCT_H_

#include "c2f_interface.h"

#ifdef NEED_UNDERSCORE
#define cpsacct_exit                 cpsacct_exit_
#define cpsacct_init                 cpsacct_init_
#define cpsacct_message              cpsacct_message_
#define cpsacct_redirect             cpsacct_redirect_
#endif

#ifdef NEED_CAPITALS
#define cpsacct_exit                 CPSACCT_EXIT        
#define cpsacct_init                 CPSACCT_INIT
#define cpsacct_message              CPSACCT_MESSAGE       
#define cpsacct_redirect             CPSACCT_REDIRECT
#endif

#ifdef __cplusplus
extern "C" {
#endif

/*char *cpsacct_h_ident =*/
/*"$Id: cpsacct.h,v 1.2 2007/01/26 14:22:07 Menger beta sps $";*/

/*------------------------ start of prototypes ---------------------------*/
/*------------------------ start of prototypes ---------------------------*/
/*------------------------ start of prototypes ---------------------------*/

/*        function prototypes for cpsacct  routines    */

void cpsacct_exit(); 
void cpsacct_init(); 
void cpsacct_message(char*);
static int cpsacct_redirect();

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
