/*<CPS_v1 type="HEADER_FILE"/>
!------------------------------- lnklst.h ----------------------------------
!------------------------------- lnklst.h ----------------------------------
!------------------------------- lnklst.h ----------------------------------

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
! Name       : LNKLST.H
! Category   : miscellaneous
! Written    : 2002-09-23   by: Charles C. Burch
! Revised    : 2002-09-23   by: Charles C. Burch
! Maturity   : production   2003-02-27
! Purpose    : Provides definitions of functions in lnklst.c
! References : These routines are called from within pfio.c and for general use
!-------------------------------------------------------------------------------
!</brief_doc>

!<history_doc>
!-------------------------------------------------------------------------------
!                       HEADER FILE REVISION HISTORY
!     Date        Author          Description
!     ----        ------          -----------
!  1. 2003-02-27  Chuck C. Burch  Initial version.
!-------------------------------------------------------------------------------
!</history_doc>
*/

/*--------------------------- start of coding ------------------------------*/
/*--------------------------- start of coding ------------------------------*/
/*--------------------------- start of coding ------------------------------*/

#ifndef _LNKLST_H_
#define _LNKLST_H_

#include "c2f_interface.h"

#ifdef NEED_CAPITALS
#define lnklst_get_var_list  LNKLST_GET_VAR_LIST
#define lnklst_put_var_list  LNKLST_PUT_VAR_LIST
#endif

#ifdef NEED_UNDERSCORE
#define lnklst_get_var_list  lnklst_get_var_list_
#define lnklst_put_var_list  lnklst_put_var_list_
#endif

#define LNKLST_INITIALIZER      {NULL, NULL}

#ifdef __cplusplus
extern "C" {
#endif

/*char *lnklst_h_ident =*/
/*"$Id: lnklst.h,v 1.1 2003/02/26 20:16:53 Burch prod sps $";*/

struct   lnklst_struct {
  char   *name;
  char   *str;
  struct lnklst_struct *next;
};

/*------------------------ start of prototypes ---------------------------*/
/*------------------------ start of prototypes ---------------------------*/
/*------------------------ start of prototypes ---------------------------*/


void lnklst_delete_list_entry(struct lnklst_struct**, char*);
void lnklst_delete_list(struct lnklst_struct**);
void lnklst_delete_var_list();
void lnklst_dump_list(struct lnklst_struct**, char*);
void lnklst_get_list_entry(struct lnklst_struct**, char*, char*, int);
void lnklst_get_var_list(char*, char*);
void lnklst_initialize_list(struct lnklst_struct**);
void lnklst_put_var_list(char*, char*);
void lnklst_put_list_entry(struct lnklst_struct**, char*, char*);
void lnklst_replace_list_entry(struct lnklst_struct**, char*, char*, int);
void lnklst_scan_list(struct lnklst_struct**, char**, char**);
int  lnklst_search_list(struct lnklst_struct**, char*);

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
