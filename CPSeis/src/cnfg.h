/*<CPS_v1 type="HEADER_FILE"/>
!------------------------------- cnfg.h ----------------------------------
!------------------------------- cnfg.h ----------------------------------
!------------------------------- cnfg.h ----------------------------------

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
! Name       : CNFG.H
! Category   : main_prog
! Written    : 2002-06-20   by: Charles C. Burch
! Revised    : 2007-01-25   by: Bill Menger
! Maturity   : beta
! Purpose    : Provides an interface to the CPS configuration file.
! References : These routines are called from many of the CPS system routines
!</brief_doc>

!<history_doc>
!-------------------------------------------------------------------------------
!                       HEADER FILE REVISION HISTORY
!     Date        Author          Description
!     ----        ------          -----------
!  3. 2007-01-25  Bill Menger     Added cnfg_get_line to match lgc version.
!  2. 2003-04-21  C C Burch       Added cnfg_exit.
!  1. 2002-06-20  Chuck C. Burch  Initial version.
!-------------------------------------------------------------------------------
!</history_doc>
*/

/*--------------------------- start of coding ------------------------------*/
/*--------------------------- start of coding ------------------------------*/
/*--------------------------- start of coding ------------------------------*/

#ifndef _CNFG_H_
#define _CNFG_H_
#include <stdio.h>
#include "c2f_interface.h"

#ifdef NEED_UNDERSCORE
#define cnfg_dump                 cnfg_dump_
#define cnfg_exit                 cnfg_exit_
#define cnfg_get_tt_info_c        cnfg_get_tt_info_c_
#define cnfg_get_tt_names_c       cnfg_get_tt_names_c_
#define cnfg_get_tt_set_c         cnfg_get_tt_set_c_
#define cnfg_get_value_c          cnfg_get_value_c_
#define cnfg_get_value_f          cnfg_get_value_f_
#define cnfg_get_line             cnfg_get_line_
#endif

#ifdef NEED_CAPITALS
#define cnfg_dump                 CNFG_DUMP
#define cnfg_exit                 CNFG_EXIT
#define cnfg_get_tt_info_c        CNFG_GET_TT_INFO_C
#define cnfg_get_tt_names_c       CNFG_GET_TT_NAMES_C
#define cnfg_get_tt_set_c         CNFG_GET_TT_SET_C
#define cnfg_get_value_c          CNFG_GET_VALUE_C
#define cnfg_get_value_f          CNFG_GET_VALUE_F
#define cnfg_get_line             CNFG_GET_LINE
#endif

#define cnfg_tt_name_length          8
#define cnfg_tt_disk_length          40

#ifdef __cplusplus
extern "C" {
#endif

/*char *cnfg_h_ident =*/
/*"$Id: cnfg.h,v 1.3 2007/01/26 14:22:07 Menger beta sps $";*/

/*------------------------ start of prototypes ---------------------------*/
/*------------------------ start of prototypes ---------------------------*/
/*------------------------ start of prototypes ---------------------------*/

/*        function prototypes for cnfg  routines    */

void cnfg_dump(); 
void cnfg_exit(); 
void cnfg_get_tt_info_c(INTEGER*, INTEGER*);
void cnfg_get_tt_names_c(INTEGER*, char*, INTEGER*);
void cnfg_get_tt_set_c(char*, INTEGER*, char*, INTEGER*, INTEGER*);
char *cnfg_get_value_c(char *);
void cnfg_get_value_f(char*, char*, INTEGER*);
int cnfg_get_line(char *line_in, int n_line, FILE *fd);


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
