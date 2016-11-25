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
/****
!<CPS_v1 type="AUXILIARY_FILE"/>
****/

/*-------------------------- string_crou.c --------------------------------*/
/*-------------------------- string_crou.c --------------------------------*/
/*-------------------------- string_crou.c --------------------------------*/

                    /* other files are:  string.f90 */

/****
 
 
!<brief_doc>
!-------------------------------------------------------------------------------
!                   C P S   P R I M I T I V E   F I L E
!
! Name       : STRING_CROU
! Category   : character
! Written    : 1999-08-31   by: Tom Stoeckley
! Revised    : 2005-05-31   by: Tom Stoeckley
! Maturity   : production
! Purpose    : A collection of routines for manipulating character strings.
! Portability: No known limitations, but see comment in string_put_tokens.
!
!-------------------------------------------------------------------------------
!</brief_doc>


!<history_doc>
!-------------------------------------------------------------------------------
!                     AUXILIARY FILE REVISION HISTORY
!
!     Date        Author     Description
!     ----        ------     -----------
!  8. 2005-05-31  Stoeckley  Fix to compile with C++.
!  7. 2000-09-15  Stoeckley  Add string_crou_toupper and string_crou_tolower.
!  6. 2000-08-21  Stoeckley  Add string_crou_to_upper and string_crou_to_lower.
!  5. 1999-12-29  Stoeckley  Add functions to test a single character for
!                             certain conditions (interface to C functions).
!  4. 1999-11-17  Stoeckley  Add ident string for RCS.
!  3. 1999-09-22  Stoeckley  Add string_crou_chars_per_integer.
!  2. 1999-09-10  Stoeckley  Add reference to other files.
!  1. 1999-08-31  Stoeckley  Initial version.
!
!-------------------------------------------------------------------------------
!</history_doc>
****/


/*--------------------------- start of module ------------------------------*/
/*--------------------------- start of module ------------------------------*/
/*--------------------------- start of module ------------------------------*/


         /* The functions in this file are called from string.f90  */
         /* and are considered to be part of the STRING primitive. */
         /* These functions are called only from string.f90 and    */
         /* should be considered private.                          */


#include "c2f_interface.h"
#include "str.h"
#include <stdlib.h>
#include <string.h>
#include <ctype.h>


#ifdef NEED_UNDERSCORE
#define string_crou_char_into_buffer   string_crou_char_into_buffer_
#define string_crou_char_from_buffer   string_crou_char_from_buffer_
#define string_crou_chars_per_integer  string_crou_chars_per_integer_
#define string_crou_ii2hh_simple       string_crou_ii2hh_simple_
#define string_crou_ff2hh_simple       string_crou_ff2hh_simple_
#define string_crou_dd2hh_simple       string_crou_dd2hh_simple_
#define string_crou_hh2ii_simple       string_crou_hh2ii_simple_
#define string_crou_hh2ff_simple       string_crou_hh2ff_simple_
#define string_crou_hh2dd_simple       string_crou_hh2dd_simple_
#define string_crou_time_date          string_crou_time_date_
#define string_crou_time               string_crou_time_
#define string_crou_date               string_crou_date_
#define string_crou_is_alphanum        string_crou_is_alphanum_
#define string_crou_is_alpha           string_crou_is_alpha_
#define string_crou_is_control         string_crou_is_control_
#define string_crou_is_digit           string_crou_is_digit_
#define string_crou_is_graph           string_crou_is_graph_
#define string_crou_is_lower           string_crou_is_lower_
#define string_crou_is_print           string_crou_is_print_
#define string_crou_is_punct           string_crou_is_punct_
#define string_crou_is_space           string_crou_is_space_
#define string_crou_is_upper           string_crou_is_upper_
#define string_crou_is_hex             string_crou_is_hex_
#define string_crou_is_one             string_crou_is_one_
#define string_crou_to_upper           string_crou_to_upper_
#define string_crou_to_lower           string_crou_to_lower_
#define string_crou_toupper            string_crou_toupper_
#define string_crou_tolower            string_crou_tolower_
#endif

#ifdef NEED_CAPITALS
#define string_crou_char_into_buffer   STRING_CROU_CHAR_INTO_BUFFER
#define string_crou_char_from_buffer   STRING_CROU_CHAR_FROM_BUFFER
#define string_crou_chars_per_integer  STRING_CROU_CHARS_PER_INTEGER
#define string_crou_ii2hh_simple       STRING_CROU_II2HH_SIMPLE
#define string_crou_ff2hh_simple       STRING_CROU_FF2HH_SIMPLE
#define string_crou_dd2hh_simple       STRING_CROU_DD2HH_SIMPLE
#define string_crou_hh2ii_simple       STRING_CROU_HH2II_SIMPLE
#define string_crou_hh2ff_simple       STRING_CROU_HH2FF_SIMPLE
#define string_crou_hh2dd_simple       STRING_CROU_HH2DD_SIMPLE
#define string_crou_time_date          STRING_CROU_TIME_DATE
#define string_crou_time               STRING_CROU_TIME
#define string_crou_date               STRING_CROU_DATE
#define string_crou_is_alphanum        STRING_CROU_IS_ALPHANUM
#define string_crou_is_alpha           STRING_CROU_IS_ALPHA
#define string_crou_is_control         STRING_CROU_IS_CONTROL
#define string_crou_is_digit           STRING_CROU_IS_DIGIT
#define string_crou_is_graph           STRING_CROU_IS_GRAPH
#define string_crou_is_lower           STRING_CROU_IS_LOWER
#define string_crou_is_print           STRING_CROU_IS_PRINT
#define string_crou_is_punct           STRING_CROU_IS_PUNCT
#define string_crou_is_space           STRING_CROU_IS_SPACE
#define string_crou_is_upper           STRING_CROU_IS_UPPER
#define string_crou_is_hex             STRING_CROU_IS_HEX
#define string_crou_is_one             STRING_CROU_IS_ONE
#define string_crou_to_upper           STRING_CROU_TO_UPPER 
#define string_crou_to_lower           STRING_CROU_TO_LOWER 
#define string_crou_toupper            STRING_CROU_TOUPPER 
#define string_crou_tolower            STRING_CROU_TOLOWER 
#endif


char STRING_CROU_IDENT[100] =
"$Id: string_crou.c,v 1.8 2005/05/31 13:04:10 Stoeckley prod sps $";

#ifdef __cplusplus
extern "C" {
#endif

/*----------------------- char to and from buffer -------------------------*/
/*----------------------- char to and from buffer -------------------------*/
/*----------------------- char to and from buffer -------------------------*/


void string_crou_char_into_buffer(INTEGER* cvar, INTEGER* index, char* hvar)
{
  hvar[*index-1] = (char)(*cvar);
}

void string_crou_char_from_buffer(char* hvar, INTEGER* index, INTEGER* cvar)
{
  *cvar = (long)hvar[*index-1];
}


/*----------------------- chars per integer -------------------------------*/
/*----------------------- chars per integer -------------------------------*/
/*----------------------- chars per integer -------------------------------*/


INTEGER string_crou_chars_per_integer(void)
{
  return (INTEGER)sizeof(INTEGER);
}


/*------------------------ get time and date ------------------------------*/
/*------------------------ get time and date ------------------------------*/
/*------------------------ get time and date ------------------------------*/
 
 
void string_crou_time_date(char *buffer)
{
  strcpy(buffer, str_time_date());
}
 
 
void string_crou_time(char *buffer)
{
  strcpy(buffer, str_time());
}
 
 
void string_crou_date(char *buffer)
{
  strcpy(buffer, str_date());
}
 
 
/*--------------------- convert number to string ------------------------*/
/*--------------------- convert number to string ------------------------*/
/*--------------------- convert number to string ------------------------*/


void string_crou_ii2hh_simple(INTEGER *ivar, char *cvar, INTEGER *nchar)
{
#ifdef INTEGER_is_int
  str_ii2ss_simple(*ivar, cvar, *nchar);
#else
  assert(FALSE);
#endif
}


void string_crou_ff2hh_simple
                 (REAL *fvar, char *cvar, INTEGER *nchar, INTEGER *ndec)
{
#if (REAL_is_float && INTEGER_is_int)
  str_ff2ss_simple(*fvar, cvar, *nchar, *ndec);
#elif (REAL_is_double && INTEGER_is_int)
  str_dd2ss_simple(*fvar, cvar, *nchar, *ndec);
#else
  assert(FALSE);
#endif
}


void string_crou_dd2hh_simple
                 (DOUBLE *dvar, char *cvar, INTEGER *nchar, INTEGER *ndec)
{
#if (DOUBLE_is_float && INTEGER_is_int)
  str_ff2ss_simple(*dvar, cvar, *nchar, *ndec);
#elif (DOUBLE_is_double && INTEGER_is_int)
  str_dd2ss_simple(*dvar, cvar, *nchar, *ndec);
#else
  assert(FALSE);
#endif
}


/*--------------------- convert string to number ------------------------*/
/*--------------------- convert string to number ------------------------*/
/*--------------------- convert string to number ------------------------*/


void string_crou_hh2ii_simple(char *cvar, INTEGER *ivar, INTEGER *istat)
{
#ifdef INTEGER_is_int
  str_ss2ii_simple(cvar, ivar, istat);
#else
  assert(FALSE);
#endif
}


void string_crou_hh2ff_simple(char *cvar, REAL *fvar, INTEGER *istat)
{
#if (REAL_is_float && INTEGER_is_int)
  str_ss2ff_simple(cvar, fvar, istat);
#elif (REAL_is_double && INTEGER_is_int)
  str_ss2dd_simple(cvar, fvar, istat);
#else
  assert(FALSE);
#endif
}


void string_crou_hh2dd_simple(char *cvar, DOUBLE *dvar, INTEGER *istat)
{
#if (DOUBLE_is_float && INTEGER_is_int)
  str_ss2ff_simple(cvar, dvar, istat);
#elif (DOUBLE_is_double && INTEGER_is_int)
  str_ss2dd_simple(cvar, dvar, istat);
#else
  assert(FALSE);
#endif
}



/*---------------------- to test a single character -------------------------*/
/*---------------------- to test a single character -------------------------*/
/*---------------------- to test a single character -------------------------*/


INTEGER string_crou_is_alphanum (const INTEGER *c) { return isalnum (*c); }
INTEGER string_crou_is_alpha    (const INTEGER *c) { return isalpha (*c); }
INTEGER string_crou_is_control  (const INTEGER *c) { return iscntrl (*c); }
INTEGER string_crou_is_digit    (const INTEGER *c) { return isdigit (*c); }
INTEGER string_crou_is_graph    (const INTEGER *c) { return isgraph (*c); }
INTEGER string_crou_is_lower    (const INTEGER *c) { return islower (*c); }
INTEGER string_crou_is_print    (const INTEGER *c) { return isprint (*c); }
INTEGER string_crou_is_punct    (const INTEGER *c) { return ispunct (*c); }
INTEGER string_crou_is_space    (const INTEGER *c) { return isspace (*c); }
INTEGER string_crou_is_upper    (const INTEGER *c) { return isupper (*c); }
INTEGER string_crou_is_hex      (const INTEGER *c) { return isxdigit(*c); }

INTEGER string_crou_is_one (const char *c, const char *incl)
{
  const char *p = strpbrk(c, incl);
  return (p != NULL);
}


/*------------------- convert to upper and to lower case ----------------*/
/*------------------- convert to upper and to lower case ----------------*/
/*------------------- convert to upper and to lower case ----------------*/


void string_crou_to_upper(char *cvar)
{
  str_to_upper(cvar, cvar);
}


void string_crou_to_lower(char *cvar)
{
  str_to_lower(cvar, cvar);
}


INTEGER string_crou_toupper(INTEGER *cvar)      /* one character only */
{
  return (INTEGER)toupper((int)(*cvar));
}


INTEGER string_crou_tolower(INTEGER *cvar)      /* one character only */
{
  return (INTEGER)tolower((int)(*cvar));
}


#ifdef __cplusplus
}
#endif

/*-------------------------------- end -----------------------------------*/
/*-------------------------------- end -----------------------------------*/
/*-------------------------------- end -----------------------------------*/

