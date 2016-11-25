/****
!<CPS_v1 type="HEADER_FILE"/>

!!------------------------------- str.h --------------------------------!!
!!------------------------------- str.h --------------------------------!!
!!------------------------------- str.h --------------------------------!!

! other files are:  str.c 

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
! Name       : STR.H
! Category   : character
! Written    : 1999-09-02   by: Tom Stoeckley
! Revised    : 2007-03-27   by: Kruger Corn
! Maturity   : beta
! Purpose    : Provides an interface to string routines in str_crou.c
! References : These routines are for general use from c
!-------------------------------------------------------------------------------
!</brief_doc>

!<history_doc>
!-------------------------------------------------------------------------------
!                       HEADER FILE REVISION HISTORY
!
!     Date        Author       Description
!     ----        ------       -----------
!  9. 2007-03-27  Corn         Updated to 64 bit architecture. Basically
!                              changed long to int32_t and long long to
!                              int64_t.
!  8. 2003-05-28  C C Burch    Added str_cmp_nocase, str_cmp_skip_blanks,
!                              str_cmp_nocase_skip_blanks, str_sub_atoi,
!                              str_sub_atol, str_sub_atoll, str_sub_cmp,
!                              str_sub_cmp_nocase, str_atoi, str_atol,
!                              str_is_string_blank, str_is_string_valid_integer,
!                              str_is_string_valid_date, and 
!                              str_is_string_valid_time.
!  7. 2002-07-18  C C Burch    Add str_compress, str_atoll, str_substr,
!                              str_find_str, str_find_last_str, str_find_not_str
!  6. 2002-01-02  Stoeckley    Add str_free and str_replace; improve some
!                               argument names for clarity and consistency.
!  5. 2001-05-10  Stoeckley    Add str_newstr and str_newstrcat from cprim.
!  4. 2000-10-06  Stoeckley    Move CPS_v1 tag to proper location.
!  3. 1999-10-20  Stoeckley    Add str_trimmed_length.
!  2. 1999-09-10  Stoeckley    Add reference to other files.
!  1. 1999-09-02  Stoeckley    Initial version.
!
!-------------------------------------------------------------------------------
!</history_doc>
****/


/*--------------------------- start of coding ------------------------------*/
/*--------------------------- start of coding ------------------------------*/
/*--------------------------- start of coding ------------------------------*/


#ifndef _STR_H_
#define _STR_H_

#ifdef SUN
#include <sys/int_types.h> /* for int32_t & int64_t */
#elif SGI64
#include <inttypes.h>      /* for int32_t & int64_t */
#else
#include <stdint.h>        /* for int32_t & int64_t */
#endif

#ifdef __cplusplus
extern "C" {
#endif


/*------------------------ start of information --------------------------*/
/*------------------------ start of information --------------------------*/
/*------------------------ start of information --------------------------*/

 
void str_ii2ss                (int    ivar, char  *hvar, int  nchar);
void str_ff2ss                (float  fvar, char  *hvar, int  nchar, int ndec);
void str_dd2ss                (double dvar, char  *hvar, int  nchar, int ndec);
void str_ss2ii                (char  *hvar, int   *ivar, int *istat);
void str_ss2ff                (char  *hvar, float *fvar, int *istat);
void str_ss2dd                (char  *hvar, double*dvar, int *istat);
 
void str_ii2ss_simple         (int    ivar, char  *hvar, int  nchar);
void str_ff2ss_simple         (float  fvar, char  *hvar, int  nchar, int ndec);
void str_dd2ss_simple         (double dvar, char  *hvar, int  nchar, int ndec);
void str_ss2ii_simple         (char  *hvar, int   *ivar, int *istat);
void str_ss2ff_simple         (char  *hvar, float *fvar, int *istat);
void str_ss2dd_simple         (char  *hvar, double*dvar, int *istat);
 
int  str_remove_trailing_blanks(char *newstring, char *oldstring);
int  str_remove_all_blanks   (char *newstring, char *oldstring);
int  str_trimmed_length      (const char *string);
 
int  str_to_upper            (char *newstring, char *oldstring);
int  str_to_lower            (char *newstring, char *oldstring);
 
const char *str_time_date    (void);
const char *str_time         (void);
const char *str_date         (void);

char *str_newstr             (const char *oldstring);
char *str_newstrcat          (const char *oldstring, ...);

char *str_free               (char *string);
char *str_replace            (char *string, const char *oldstring);


int       str_atoi           (char*);
int32_t   str_atol           (char*);
int64_t   str_atoll          (char*);

int       str_compress       (char*, char*);

int       str_cmp_nocase     (char*, char*);
int       str_cmp_nocase_skip_blanks(char*, char*);
int       str_cmp_skip_blanks(char*, char*);

int       str_is_string_blank(char*);
int       str_is_string_valid_integer(char*);
int       str_is_string_valid_date(char*);
int       str_is_string_valid_time(char*);

int       str_find_str       (char*, int, char*);
int       str_find_last_str  (char*, int, char*);
int       str_find_not_str   (char*, int, char*);

int       str_sub_atoi       (char*, int, int);
int32_t   str_sub_atol       (char*, int, int);
int64_t   str_sub_atoll      (char*, int, int);

int       str_sub_cmp        (char*, int, int, char*);
int       str_sub_cmp_no_case(char*, int, int, char*);

void      str_substr         (char*, int, int, char*, int);
int       str_trim           (char*, char*);

/*------------------------- end of information ---------------------------*/
/*------------------------- end of information ---------------------------*/
/*------------------------- end of information ---------------------------*/


#ifdef __cplusplus
}
#endif


#endif


/*-------------------------------- end ------------------------------------*/
/*-------------------------------- end ------------------------------------*/
/*-------------------------------- end ------------------------------------*/

