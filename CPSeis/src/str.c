/*
!<CPS_v1 type="PRIMITIVE"/>

!!------------------------------- str.c --------------------------------!!
!!------------------------------- str.c --------------------------------!!
!!------------------------------- str.c --------------------------------!!
 
!other files are:  str.h 

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
!                        C P S   P R I M I T I V E      
!
! Name       : STR
! Category   : character
! Written    : 1999-08-31   by: Tom Stoeckley
! Revised    : 2007-03-27   by: Kruger Corn
! Maturity   : beta
! Purpose    : Various string conversion routines for C language.
! Portability: No known limitations.
!
!-------------------------------------------------------------------------------
!</brief_doc>


!<descript_doc>
!-------------------------------------------------------------------------------
!                         GENERAL DESCRIPTION          
!
! Functions in this primitive are designed to provide various string
! conversion operations for C language callers.  There is a related
! primitive called STRING which contains similar routines explicitly
! designed for Fortran callers.
!
! This primitive provides the following operations:
!
!   (1) Convert a null-terminated string to upper or lower case.
!   (2) Remove all blanks or trailing blanks (white spaces)
!         out of a null-terminated string.
!   (3) Convert a number to a character string, or vice versa.
!         Can be used for various data types.
!         Nil values are optionally supported.
!
!-------------------------------------------------------------------------------
!             USEFUL STANDARD C CHARACTER TEST ROUTINES
!
! The following standard C character test routines are available.
! Their functionality is not duplicated in this module.
! The header file <ctype.h> must be included to use these functions.
! The argument to these functions is a single character.
! These functions return true or false as to whether the condition is true.
!
!         function      condition
!         --------      ---------
!         isalnum       letter or digit        
!         isalpha       letter (a-z or A-Z)     
!         iscntrl       control character
!         isdigit       digit (0-9)               
!         isgraph       printable char except space
!         islower       lower case letter        
!         isprint       printable char incl space 
!         ispunct       printable char except space or letter or digit
!         isspace       space character        
!         isupper       upper case letter       
!         isxdigit      hexadecimal digit        
!
!-------------------------------------------------------------------------------
!</descript_doc>


!<calling_doc>
!-------------------------------------------------------------------------------
!                      INPUT AND OUTPUT ARGUMENTS      
!
! For each subroutine or function documented below, each argument is
! flagged as follows:
!       i = value required upon INPUT.
!       o = value set by the routine upon OUTPUT.
!       b = value BOTH required upon input and changed upon output.
!
!  For pointers, the flag (i,o,b) refers to the contents pointed to
!  by the pointer, not to the value of the pointer itself.  The pointer
!  value is required upon INPUT in all cases.
!
!-------------------------------------------------------------------------------
! To allocate a new string:
!
!            o                          i
!        newstring = str_newstr     (oldstring)
!        newstring = str_newstrcat  (piece1, piece2, ..., NULL)
!
!        newstring = str_free       (string)
!        newstring = str_replace    (string, oldstring)
!            o                          i        i
!
! char       *string              = string to be deallocated (can be NULL).
! const char *oldstring           = old string (can be NULL) (unchanged).
! const char *piece1, piece2, ... = pieces to be concatenated (unchanged).
! char       *newstring           = new string which has been allocated.
!
! The last piece in the list to be concatenated must be NULL.
! The new string should be freed when no longer needed.
! str_free deallocates the string (if not NULL) and returns NULL.
!
! The usual way to free a string (whether or not it is already NULL) and
! to set the pointer to NULL:
!                  string = str_free(string);
!
! The usual way to replace the contents of a string (which might be NULL):
!                  string = str_replace(string, oldstring);
!
!-------------------------------------------------------------------------------
! To remove    all   blanks from a null terminated string:
! To remove trailing blanks from a null terminated string:
!
!                                            o           i
!       len = str_remove_all_blanks      (newstring, oldstring)
!       len = str_remove_trailing_blanks (newstring, oldstring)
!
! char *oldstring = string which may contain white space.
! char *newstring = new string which has had white spaces removed.
! int         len = length of new string.
!
! If oldstring is NULL, newstring will have zero length.
! If newstring is NULL, a zero will simply be returned.
! If oldstring and newstring point to the same string, the string
!   will be modified in place.  If they point to different strings,
!   oldstring will be unchanged, and newstring will be the altered
!   version of oldstring.
!
!-------------------------------------------------------------------------------
! To get the length of a null terminated string excluding trailing blanks:
!
!                                        i
!            len = str_trimmed_length (string)
!
! const char *string = string which might have trailing blanks before the
!                        null termination.
! int            len = length of the string excluding trailing blanks.
!
!-------------------------------------------------------------------------------
! To convert a null-terminated string to upper case:
! To convert a null-terminated string to lower case:
!
!                                    o           i
!            len = str_to_upper  (newstring, oldstring)
!            len = str_to_lower  (newstring, oldstring)
!
! char *oldstring = string which may contain upper and/or lower case.
! char *newstring = new string which contains only upper or lower case.
! int         len = length of new and old strings.
!
! If oldstring and newstring point to the same string, the string
! will be modified in place.  If they point to different strings,
! oldstring will be unchanged, and newstring will be the altered
! version of oldstring.
!
!------------------------------------------------------------------------------
! To get time and date strings:
!
!        const char *str_time_date (void)   returns time and date.
!        const char *str_time      (void)   returns time as HH.MM.SS
!        const char *str_date      (void)   returns date as YYYY/MM/DD
!
! The returned pointers point to static areas which should not
! be modified or freed.  These strings should be copied to wherever
! they are needed before the next call to these routines, because the
! same static areas are used each time.
!
!-------------------------------------------------------------------------------
! To convert a number to a character string:
!
!                             i    o     i    i
!     void str_ii2ss_simple (ivar,hvar,nchar)          for ints
!     void str_ff2ss_simple (fvar,hvar,nchar,ndec)     for floats
!     void str_dd2ss_simple (dvar,hvar,nchar,ndec)     for doubles
!
!     void str_ii2ss        (ivar,hvar,nchar)          for ints
!     void str_ff2ss        (fvar,hvar,nchar,ndec)     for floats
!     void str_dd2ss        (dvar,hvar,nchar,ndec)     for doubles
!
! To convert a character string to a number:
!
!                             i    o     o
!     void str_ss2ii_simple (hvar,ivar,istat)          for ints
!     void str_ss2ff_simple (hvar,fvar,istat)          for floats
!     void str_ss2dd_simple (hvar,dvar,istat)          for doubles
!
!     void str_ss2ii        (hvar,ivar,istat)          for ints
!     void str_ss2ff        (hvar,fvar,istat)          for floats
!     void str_ss2dd        (hvar,dvar,istat)          for doubles
!
! int     ivar = (input) number to convert to a text string.
! real    fvar = (input) number to convert to a text string.
! double  dvar = (input) number to convert to a text string.
! int    *ivar = (output) number converted from a text string.
! real   *fvar = (output) number converted from a text string.
! double *dvar = (output) number converted from a text string.
! char   *hvar = (input or output) null-terminated text string.
! int    nchar = (input) maximum number of characters to display (>0).
! int     ndec = (input) maximum number of decimals to display (>=0).
! int   *istat = (output) status of converted number.
!
!                              +++++++++
!
! When a string is converted to a number, the following status
! is returned:
!       (a) istat =  1 means the conversion was successful.
!       (b) istat =  0 means the string was blank.
!       (c) istat = -1 means the conversion failed.
!
! The routines without "_simple" appended to their name recognize
! nil values which are displayed as a blank string.  Nil values are
! defined in the named_constants.h header file.
!
! The following table summarizes the conversion procedures:
!
!     numeric value  status     string
!     -------------  ------     ------
!     nil value             --> blank          (not "_simple" routines)
!     all other values      --> valid string   (if conversion successful)
!     all other values      --> asterisk       (if conversion fails)
!
!     nil value        -1  <--  invalid string (not "_simple" routines)
!     zero             -1  <--  invalid string ("_simple" routines only)
!     nil value         0  <--  blank          (not "_simple" routines)
!     zero              0  <--  blank          ("_simple" routines only)
!     valid value       1  <--  valid string
!     zero              1  <--  + or - or .    (single character)
!
!-------------------------------------------------------------------------------
!                         o           i
! int str_compress(char *str2, char *str1) {
!  purpose: copy str1 to str2 removing spaces/control chars
!   returns len of resulting str2
!-------------------------------------------------------------------------------
!                         i        i          i
! int str_find_str(char *cin, int pos, char *srch) {
!  pupose: find location of srch in cin starting at position pos
!   returns len(cin) if srch not found
!-------------------------------------------------------------------------------
!                         i             i          i
! int str_find_last_str(char *cin, int pos, char *srch) {
!  purpose: find location of last srch in cin starting at position pos
!   returns len(cin) if srch not found
!-------------------------------------------------------------------------------
!                         i            i          i
! int str_find_not_str(char *cin, int pos, char *srch) {
!  purpose: find location in cin starting at pos where srch does not appear
!   returns len(cin) if srch not found
!-------------------------------------------------------------------------------
!                        i         i         i          o           i
! void str_substr(char *cin, int pos1, int pos2, char *cout, int cout_len){
! purpose: extracts substr in cin from pos1 and pos2
!  and place results into cout with strlen(cout)<=cout_len
!-------------------------------------------------------------------------------
!                            i
! int       str_atoi (char *cin) 
! int32_t   str_atol (char *cin) 
! int64_t   str_atoll(char *cin) 
!
! purpose convert string cin into int, int32_t, int64_t 
!  (nonumeric characters are skipped)
!-------------------------------------------------------------------------------
!                                i       i       i
! int       str_sub_atoi (char *cin, int i1, int i2)
! int32_t   str_sub_atol (char *cin, int i1, int i2)
! int64_t   str_sub_atoll(char *cin, int i1, int i2)
!
! purpose: convert substring c(i1:i2) into int, int32_t, int64_t 
!    (nonnumeric characters are skipped)
!-------------------------------------------------------------------------------
!                          i           i
! int str_cmp_nocase(char *str1, char *str2)
! purpose: do a strcmp with both strings capitalized
!-------------------------------------------------------------------------------
!                             i         i       i         i
! int str_sub_cmp_nocase(char *cin, int i1, int i2, char *cmp)
! purpose: do a strcmp of substring cin[i1:i2] & cmp, both capitalized
!-------------------------------------------------------------------------------
!                               i           i
! int str_cmp_skip_blanks(char *str1, char *str2)
! purpose: do a strcmp skipping any blanks
!-------------------------------------------------------------------------------
!                                      i           i
! int str_cmp_nocase_skip_blanks(char *str1, char *str2) 
! purpose: do a strcmp with both strings capitalized and blanks removed
!-------------------------------------------------------------------------------
!                            i
! int str_string_blank(char *str)
! purpose: returns 1 if str all blanks and 0 otherwise
!-------------------------------------------------------------------------------
!                                    i
! int str_string_valid_integer(char *str)
! purpose: returns 1 if str is a valid integer entry and 0 otherwise
!          leading and trailing blanks are allowed
!-------------------------------------------------------------------------------
!                                 i
! int str_string_valid_date(char *str)
! purpose: returns 1 if str is a valid date entry and 0 otherwise
!-------------------------------------------------------------------------------
!                                 i
! int str_string_valid_time(char *str)
! purpose: returns 1 if str is a valid time entry and 0 otherwise
!-------------------------------------------------------------------------------
!</calling_doc>


!<advice_doc>
!-------------------------------------------------------------------------------
!                            ADVICE FOR USERS           
!
!
!-------------------------------------------------------------------------------
!</advice_doc>


!<history_doc>
!-------------------------------------------------------------------------------
!                              REVISION HISTORY 
!
!     Date        Author     Description
!     ----        ------     -----------
! 13. 2007-03-27  Corn       Updated to 64 bit architecture. Basically
!                            changed long to int32_t and long long to
!                            int64_t.
! 12. 2003-05-28  C C Burch  Add separate buffers for str_date and str_time.
!                            Add cmp_nocase, cmp_skip_blanks, trim, sub_cmp,
!                            cmp_nocase_skip_blanks, sub_atoi,sub_atol, 
!                            sub_atoll, sub_cmp_nocase, is_string_blank, 
!                            is_string_valid_integer, is_string_valid_date, &
!                            is_string_valid_time.
! 11. 2002-07-18  C C Burch  Add str_compress, str_find_last_str, 
!                            str_find_str, str_find_not_str, str_atoll.
! 10. 2002-01-02  Stoeckley  Add PTESTING macro to allow further improvements
!                             in the ease of input of exponential notation
!                             in a GUI; add one significant figure to the
!                             maximum diplayed precision for floats and
!                             doubles; improve some variable names and
!                             documentation for clarity and consistency;
!                             add str_free and str_replace; improve significant
!                             figures in E format.
!  9. 2001-05-14  Stoeckley  Add str_newstr and str_newstrcat from cprim.
!  8. 2000-05-17  Stoeckley  Remove unused variables.
!  7. 2000-05-08  Stoeckley  Change dates to use dashes instead of slashes.
!  6. 2000-02-04  Stoeckley  Replace the str_convert function to improve
!                              the precision of encoded floating point values.
!  5. 1999-12-29  Stoeckley  Add smart limit on maximum number of significant
!                              figures to encode; add documentation about
!                              some useful standard C functions.
!  4. 1999-11-17  Stoeckley  Add ident string for RCS.
!  3. 1999-10-20  Stoeckley  Add str_trimmed_length, and remove reference
!                              to the c2f_interface header file.
!  2. 1999-09-10  Stoeckley  Add reference to other files.
!  1. 1999-08-31  Stoeckley  Initial version, converted from the following
!                              workstation code with slight changes to some
!                              function arguments, a deletion of special
!                              error values, and a change to the location
!                              where nil values are defined:
!                                convert_case.c
!                                convert_ii2ss.c
!                                remove_blanks.c
!                                time_date_string.c
!
!
! Revision history for convert_case:
!
!  1. 93/09/10  Stoeckley  Initial version.
!
!
! Revision history for convert_ii2ss:
!
!  2. 99/08/25  Stoeckley  Add ETESTING macro to allow easier input of
!                           exponential notation.
!  1. 93/06/03  Stoeckley  Initial version.
!
!
! Revision history for remove_blanks:
!
!  1. 93/09/10  Stoeckley  Initial version.
!
!
! Revision history for time_date_string:
!
!  2. 98/12/04  Day        Added time_date_stringf_
!  1. 94/12/12  Stoeckley  Initial version.
!
!-------------------------------------------------------------------------------
!</history_doc>


!<portability_doc>
!-------------------------------------------------------------------------------
!                        PORTABILITY LIMITATIONS        
!
! No known portabilty limitations
!-------------------------------------------------------------------------------
!</portability_doc>
****/


/*-------------------------- start of module ------------------------------*/
/*-------------------------- start of module ------------------------------*/
/*-------------------------- start of module ------------------------------*/


#include "str.h"
#include "named_constants.h"
#include <stdio.h>
#include <stdlib.h>
#include <stdarg.h>
#include <math.h>
#include <ctype.h>
#include <string.h>
#include <time.h>
#include <assert.h>
#include <limits.h>
#include <float.h>


char STR_IDENT[100] = "$Id: str.c,v 1.13 2007/03/28 15:09:43 Corn beta sps $";


/*-------------------- remove all blanks -------------------------------*/
/*-------------------- remove all blanks -------------------------------*/
/*-------------------- remove all blanks -------------------------------*/

 
int str_remove_all_blanks(char *newstring, char *oldstring)
{
  int i = 0;
  int j = 0;
  if(newstring)
       {
       if(oldstring)
            {
            while(oldstring[i])
                 {
                 if(!isspace((int) oldstring[i]))
                            { newstring[j] = oldstring[i]; j++; }
                 i++;
                 }
            }
       newstring[j] = '\0';
       }
  return j;
}
 
 
/*-------------------- remove trailing blanks --------------------------*/
/*-------------------- remove trailing blanks --------------------------*/
/*-------------------- remove trailing blanks --------------------------*/
 
 
int str_remove_trailing_blanks(char *newstring, char *oldstring)
{
  int j = 0;
  if(newstring)
       {
       if(oldstring)
            {
            if(newstring != oldstring) strcpy(newstring, oldstring);
            j = strlen(newstring);
            while(j > 0 && isspace((int)newstring[j-1]))
                 {
                 j--;
                 }
            }
       newstring[j] = '\0';
       }
  return j;
}


/*----------------------- get trimmed length ---------------------------*/
/*----------------------- get trimmed length ---------------------------*/
/*----------------------- get trimmed length ---------------------------*/
 
 
int str_trimmed_length(const char *string)
{
  int j = 0;
  if(string)
       {
       j = strlen(string);
       while(j > 0 && isspace((int) string[j-1]))
            {
            j--;
            }
       }
  return j;
}

/************************** str_trim **************************
* copy blank-trimmed version of oldstring to newstring
* return trimmed length of newstring
* This similar to str_remove_trailing blanks, but str_trim 
* requires newstring to be only big enough to store the trimmed
* version on oldstring rather than all of oldstring
*
* Written April 2003 by Charles C Burch
**************************************************************/ 
int str_trim(char *newstring, char *oldstring) {
  int i,j;
  
  j=-1;    /*find trimmed length of oldstring*/
  for(i=0;oldstring[i]!='\0';i++) {
    if(oldstring[i]!=' ') j=i;
  }
 
  if((++j)>0) memcpy(newstring,oldstring,j);
  newstring[j]='\0';
  return(j);
}

/*------------------- convert case to upper ----------------------------*/
/*------------------- convert case to upper ----------------------------*/
/*------------------- convert case to upper ----------------------------*/

 
int str_to_upper(char *newstring, char *oldstring)
{
  int i = 0;
  while(oldstring[i])
       {
       newstring[i] = toupper(oldstring[i]);
       i++;
       }
  newstring[i] = '\0';
  return i;
}
 
 
/*------------------- convert case to lower ----------------------------*/
/*------------------- convert case to lower ----------------------------*/
/*------------------- convert case to lower ----------------------------*/

 
int str_to_lower(char *newstring, char *oldstring)
{
  int i = 0;
  while(oldstring[i])
       {
       newstring[i] = tolower(oldstring[i]);
       i++;
       }
  newstring[i] = '\0';
  return i;
}
 

/*------------------------ try f format --------------------------------*/
/*------------------------ try f format --------------------------------*/
/*------------------------ try f format --------------------------------*/


static int str_try_f_format
               (double dvar, char *cvar, int nchar, int ndec, int precision)
{
  char buffer[300], charr;
  int i, len, iperiod, inonzero, ndec2;
 
  while(TRUE)
      {
      sprintf(buffer, "%-.*f", ndec, dvar);
      len = strlen(buffer);
      iperiod = len;
      inonzero = -1;
      for (i = 0; i < len; i++)
           {
           charr = buffer[i];
           if     (charr == '.') iperiod = i;
           else if(charr == '-') continue;
           else if(charr == '0') continue;
           else if(charr == ' ') { if(iperiod == len) iperiod = i; continue; }
           else if(inonzero == -1) inonzero = i;
           }
      if      (iperiod - inonzero > precision) return FALSE;
      if      (inonzero - iperiod >     6    ) return FALSE;
      if      (inonzero == -1)                 return FALSE;
      else if (inonzero > iperiod) ndec2 = precision + inonzero - iperiod - 1;
      else                         ndec2 = precision + inonzero - iperiod;
      if(ndec2 < 0) ndec2 = 0;
      if (ndec2 < ndec) ndec = ndec2;
      sprintf(buffer, "%-.*f", ndec, dvar);
      len = strlen(buffer);
      if(ndec > 0)
           {
           while(len > 1 && buffer[len-1] == '0') { len--; }
           if(buffer[len-1] == '.')               { len--; }
           if(len > nchar) { ndec--; continue; }
           buffer[len] = '\0';
           }
      if(len > 0 && len <= nchar && (len != 1 || buffer[0] != '0'))
                                   { strcpy(cvar, buffer); return TRUE; }
      break;
      }
  return FALSE;
}
 

/*------------------------ try e format --------------------------------*/
/*------------------------ try e format --------------------------------*/
/*------------------------ try e format --------------------------------*/

 
static void str_try_e_format
               (double dvar, char *cvar, int nchar, int ndec, int precision)
{
  char buffer[300];
  int len, i, j, iexp;
 
  if(ndec > precision - 1) ndec = precision - 1;
  while(TRUE)
      {
      sprintf(buffer, "%-.*E", ndec, dvar);
      len = strlen(buffer);
      if(len == 0)
          {
          strcpy(cvar, "*"); break;
          }
      else if(len <= nchar)
          {
          iexp = 0;
          for(i = 1; i < len; i++)
              {
              if(buffer[i] == 'E' || buffer[i] == 'e') { iexp = i; break; }
              }
          while(iexp > 0 && buffer[iexp-1] == '0')
                  { 
                  for(j = iexp; j <= len; j++) { buffer[j-1] = buffer[j]; }
                  iexp--; len--;
                  }
          strcpy(cvar, buffer); break;
          }
      else if(ndec > 0)
          {
          ndec--; continue;
          }
      else
          {
          strcpy(cvar, "*"); break;
          }
      }
}
 

/*---------------------- static conversion function --------------------*/
/*---------------------- static conversion function --------------------*/
/*---------------------- static conversion function --------------------*/

 
static void str_private_convert
               (double dvar, char *cvar, int nchar, int ndec, int precision)
{
  int success;

  if(dvar == 0.0) { strcpy(cvar, "0"); return; }
  if(nchar < 1) nchar = 1;  if(nchar >     80) nchar = 80;
  if(ndec  < 0) ndec  = 0;  if(ndec  >= nchar) ndec  = nchar - 1;

  success =    str_try_f_format (dvar, cvar, nchar, ndec, precision+1);
  if(!success) str_try_e_format (dvar, cvar, nchar,  99 , precision+1);
}
       /* we do not want to restrict number of decimals in E format. */

 
/*---------- convert number to string (simple method) ----------------*/
/*---------- convert number to string (simple method) ----------------*/
/*---------- convert number to string (simple method) ----------------*/

 
void str_ii2ss_simple(int ivar, char *cvar, int nchar)
{
  char buffer[100];
  int len;
 
  sprintf(buffer, "%-d", ivar);
  len = strlen(buffer);
  if(len > 0 && len <= nchar) strcpy(cvar, buffer);
  else                        strcpy(cvar, "*");
}
 
 
 
void str_ff2ss_simple(float fvar, char *cvar, int nchar, int ndec)
{
  str_private_convert((double)fvar, cvar, nchar, ndec, FLT_DIG);
}
 
 
 
void str_dd2ss_simple(double dvar, char *cvar, int nchar, int ndec)
{
  str_private_convert(dvar, cvar, nchar, ndec, DBL_DIG);
}
 
 
/*----------- convert string to number (simple method) ---------------*/
/*----------- convert string to number (simple method) ---------------*/
/*----------- convert string to number (simple method) ---------------*/

 
#define ITESTING (buffer[0] == '+' || buffer[0] == '-')
#define XTESTING  (  (len == 1 && (ITESTING  || buffer[0] == '.')) || \
                     (len == 2 && (ITESTING  && buffer[1] == '.'))  )
#define ETESTING  (  (len >= 2 && (buffer[len-1] == 'E' ||    \
                                   buffer[len-1] == 'e'))  )
#define PTESTING  (  (len >= 3 && (buffer[len-2] == 'E' ||    \
                                   buffer[len-2] == 'e') &&   \
                                  (buffer[len-1] == '+' ||    \
                                   buffer[len-1] == '-'))  )
 
 
void str_ss2ii_simple(char *cvar, int *ivar, int *istat)
{
  char buffer[200], string[200];
  int number, len /* , kount */ ;
 
  len = str_remove_all_blanks(buffer, cvar);
  if     (len == 0)             { *istat = 0; *ivar = 0; return; }
  else if(len == 1 && ITESTING) { *istat = 1; *ivar = 0; return; }
/****
       VMS ALWAYS RETURNS KOUNT=ZERO
  number = sscanf(buffer, "%d%n", ivar, &kount);
  if(number == 1 && kount == len) { *istat = 1; }
  else                            { *istat = -1; *ivar = 0; }
****/
  string[0] = '\0';
  number = sscanf(buffer, "%d%s", ivar, string);
  if(number == 1 && string[0] == '\0') { *istat = 1; }
  else                                 { *istat = -1; *ivar = 0; }
}
 
 
 
void str_ss2ff_simple(char *cvar, float *fvar, int *istat)
{
  char buffer[200], string[200];
  int number, len /* , kount */ ;

  len = str_remove_all_blanks(buffer, cvar);
  if     (len == 0)            { *istat = 0; *fvar = 0.0; return; }
  else if(XTESTING)            { *istat = 1; *fvar = 0.0; return; }
  else if(ETESTING)            { buffer[len-1] = '\0';            }
  else if(PTESTING)            { buffer[len-2] = '\0';            }
/****
       VMS ALWAYS RETURNS KOUNT=ZERO
  number = sscanf(buffer, "%f%n", fvar, &kount);
  if(number == 1 && kount == len) { *istat = 1; }
  else                            { *istat = -1; *fvar = 0.0; }
****/
  string[0] = '\0';
  number = sscanf(buffer, "%f%s", fvar, string);
  if(number == 1 && string[0] == '\0') { *istat = 1; }
  else                                 { *istat = -1; *fvar = 0.0; }
}
 
 
 
void str_ss2dd_simple(char *cvar, double *dvar, int *istat)
{
  char buffer[200], string[200];
  int number, len /* , kount */ ;
 
  len = str_remove_all_blanks(buffer, cvar);
  if     (len == 0)            { *istat = 0; *dvar = 0.0; return; }
  else if(XTESTING)            { *istat = 1; *dvar = 0.0; return; }
  else if(ETESTING)            { buffer[len-1] = '\0';            }
  else if(PTESTING)            { buffer[len-2] = '\0';            }
/****
       VMS ALWAYS RETURNS KOUNT=ZERO
  number = sscanf(buffer, "%lf%n", dvar, &kount);
  if(number == 1 && kount == len) { *istat = 1; }
  else                            { *istat = -1; *dvar = 0.0; }
****/
  string[0] = '\0';
  number = sscanf(buffer, "%lf%s", dvar, string);
  if(number == 1 && string[0] == '\0') { *istat = 1; }
  else                                 { *istat = -1; *dvar = 0.0; }
}
 
 
/*--------------------- convert number to string ---------------------*/
/*--------------------- convert number to string ---------------------*/
/*--------------------- convert number to string ---------------------*/

 
void str_ii2ss(int ivar, char *cvar, int nchar)
{
  if(ivar == INIL) strcpy(cvar, " ");
  else             str_ii2ss_simple(ivar, cvar, nchar);
}
 
 
void str_ff2ss(float fvar, char *cvar, int nchar, int ndec)
{
  if(fvar == FNIL) strcpy(cvar, " ");
  else             str_ff2ss_simple(fvar, cvar, nchar, ndec);
}
 
 
void str_dd2ss(double dvar, char *cvar, int nchar, int ndec)
{
  if(dvar == DNIL) strcpy(cvar, " ");
  else             str_dd2ss_simple(dvar, cvar, nchar, ndec);
}
 
 
/*--------------- convert string to number ---------------------------*/
/*--------------- convert string to number ---------------------------*/
/*--------------- convert string to number ---------------------------*/

 
void str_ss2ii(char *cvar, int *ivar, int *istat)
{
  str_ss2ii_simple(cvar, ivar, istat);
  if(*istat <= 0) *ivar = INIL;
}
 
 
void str_ss2ff(char *cvar, float *fvar, int *istat)
{
  str_ss2ff_simple(cvar, fvar, istat);
  if(*istat <= 0) *fvar = FNIL;
}
 
 
void str_ss2dd(char *cvar, double *dvar, int *istat)
{
  str_ss2dd_simple(cvar, dvar, istat);
  if(*istat <= 0) *dvar = DNIL;
}
 

/*------------------------ get time and date ------------------------------*/
/*------------------------ get time and date ------------------------------*/
/*------------------------ get time and date ------------------------------*/


static char buffer[100];   /* used to return time and date strings */

const char *str_time_date(void)
{
  time_t tp;
  char *string;
  int length, i;
 
  tp = time(NULL);
  string = ctime(&tp);
  strcpy(buffer, string);
  length = strlen(buffer);
  for (i = 0; i < length; i++) { if (buffer[i] == '/') buffer[i] = '-'; }
  if(length > 0 && buffer[length - 1] == '\n')
                   buffer[length - 1] = '\0';
  return buffer;
}
 
const char *str_time(void)
{
  time_t current;
  struct tm *now;
  static char buff[9];
  
  current = time(NULL);
  now = localtime(&current);
  strftime(buff, sizeof(buff), "%H.%M.%S", now);
  return buff;
}
 
const char *str_date(void)
{
  time_t current;
  struct tm *now;
  static char buff[11];
  
  current = time(NULL);
  now = localtime(&current);
  strftime(buff, sizeof(buff), "%Y-%m-%d", now);
  return buff;
}
 
 
/*------------------------------ newstr ----------------------------------*/
/*------------------------------ newstr ----------------------------------*/
/*------------------------------ newstr ----------------------------------*/


char *str_newstr( const char *oldstring)

{
  char *newstring;

  if (oldstring) {
     newstring= (char *)malloc( strlen(oldstring) +1 );
     strcpy(newstring,oldstring);
  }
  else
     newstring= NULL;

  return (newstring);
}

 
/*------------------------------ newstrcat ---------------------------------*/
/*------------------------------ newstrcat ---------------------------------*/
/*------------------------------ newstrcat ---------------------------------*/


/*
 *  malloc a newstr from a list of passed strings that is terminated by
 *  a NULL.
 *  example: str = newstrcat("This", " is", " a", " test.", NULL);
 *           str would equal "This is a test."
 *           The return value must be freed.
 *
 */

char *str_newstrcat(const char *oldstring, ...)
{
 char *workstring[100], *newstring= NULL;
 va_list   args;
 int newlen, i, j;
 int done;

 if(!oldstring) return NULL;

 va_start(args, oldstring);
 newlen= strlen(oldstring);

 for( i=0, done= False; ( (i<100) && (!done) ); i++)
   {
   workstring[i]= va_arg(args, char*);
   if (workstring[i])
         newlen+= strlen(workstring[i]);
   else
         done= True;
   }
 i--;

 newstring= (char *)malloc( newlen+1);
 strcpy(newstring, oldstring);
 for( j=0; (j<i); j++)
   {
   strcat( newstring, workstring[j] );
   }
 va_end(args);

 return (newstring);
}


/*------------------------------ free ----------------------------------*/
/*------------------------------ free ----------------------------------*/
/*------------------------------ free ----------------------------------*/


char *str_free(char *string)
{
  if (string) free(string);
  return NULL;
}

 
/*----------------------------- replace --------------------------------*/
/*----------------------------- replace --------------------------------*/
/*----------------------------- replace --------------------------------*/


char *str_replace(char *string, const char *oldstring)
{
  char *newstring = str_newstr(oldstring);
  str_free(string);
  return newstring;
}

/************************** str_compress **************************
* copy str1 to str2 removing spaces and control chars
*  returns len of resulting str2
*  Note str1 and str2 can be same variable
*
* Written August 2000 by Charles C Burch
******************************************************************/
int str_compress(char *str2, char *str1) {
  int j;
  char c;

  j=0;
  while( (c=(*(str1++)))!='\0') {
    if(c!=' ' && iscntrl((int)c)==0) str2[j++]=c;
  }
  str2[j]='\0';
  return(j);
}

/*************************** str_find_str *****************************
* find location of srch in cin starting at position pos
*  returns len(cin) if srch not found
*
* Written July 2000 by Charles C Burch
************************************************************************/
int str_find_str(char *cin, int pos, char *srch) {
  char *s;
  int cin_len;
  
  cin_len=strlen(cin);
  if(pos>=cin_len) return(cin_len);
  if(pos<0) pos=0;
  
  s=strstr(cin+pos,srch);
  if(s==NULL) return(cin_len);
  return((int) (s-cin));
}

/************************* str_find_last_str *************************
* find location of last srch in cin starting at position pos
*  returns len(cin) if srch not found
*
* Written July 2000 by Charles C Burch
**********************************************************************/
int str_find_last_str(char *cin, int pos, char *srch) {
  int i, cin_len, srch_len;

  cin_len=strlen(cin);
  if(pos>=cin_len) return(cin_len);
  if(pos<0) pos=0;
  
  srch_len=strlen(srch);
  for (i=cin_len-srch_len; i>=pos; i--) {
    if(memcmp(cin+i,srch,srch_len)==0) return(i);
  }  
  return(cin_len);
}

/************************* str_find_not_str **************************
* find first location in cin starting at position pos
*  where srch does not appear
*   returns len(cin) if srch not found
*
* Written July 2000 by Charles C Burch
*********************************************************************/
int str_find_not_str(char *cin, int pos, char *srch) {
  int i, cin_len, i_end, srch_len;

  cin_len=strlen(cin);
  if(pos>=cin_len) return(cin_len);
  if(pos<0) pos=0;
  
  srch_len=strlen(srch);
  i_end=cin_len-srch_len;
  
  for (i=pos; i<=i_end; i++) {
    if(memcmp(cin+i,srch,srch_len)!=0) return(i);
  }  
  return(i);
}

/*************************** str_substr ***********************
* extracts substr in cin from pos1 and pos2
*  and place results into cout with strlen(cout)<=cout_len
*
* Written August 2000 by Charles C Burch
**************************************************************/
void str_substr(char *cin, int pos1, int pos2, char *cout, int cout_len){

  if(cout_len<1) {
    (*cout)='\0';
    return;
  }
  
  if((pos2-pos1+2)>cout_len) pos2=pos1+cout_len-2;
  cin+=pos1;
  while(((pos1++)<=pos2) && ((*cin)!='\0')) {
    (*cout++)=(*cin++);
  }
  (*cout)='\0';
  return;
}

/****************************** str_atoi ***************************
* convert string cin into int and return results
*  nonumeric characters are skipped
*
* Written August 2000 by Charles C Burch
*******************************************************************/
int str_atoi(char *cin) {
  int temp;
  char chr;

  temp=0;
  while((chr=(*(cin++)))!='\0') {
    if(chr>='0' && chr<='9') temp=(chr-'0')+10*temp;
  }
  return(temp);
}

/****************************** str_atol ***************************
* convert string cin into int32_t and return results
*  nonumeric characters are skipped
*
* Written August 2000 by Charles C Burch
*******************************************************************/
int32_t str_atol(char *cin) {
  int32_t temp;
  char chr;

  temp=0;
  while((chr=(*(cin++)))!='\0') {
    if(chr>='0' && chr<='9') temp=(chr-'0')+10*temp;
  }
  return(temp);
}

/****************************** str_atoll ***************************
* convert string cin into int64_t and return results
*  nonumeric characters are skipped
*
* Written August 2000 by Charles C Burch
*******************************************************************/
int64_t str_atoll(char *cin) {
  int64_t temp;
  char chr;

  temp=0;
  while((chr=(*(cin++)))!='\0') {
    if(chr>='0' && chr<='9') temp=(chr-'0')+10*temp;
  }
  return(temp);
}

/************************* str_sub_atoi ****************************
* convert substr cin[i1:i2] into int and return results
*  nonumeric characters are skipped
*  if i2<0, it is set to strlen(cin)-1
*
* Written December 2002 by Charles C Burch
*******************************************************************/
int str_sub_atoi(char *cin, int i1, int i2) {
  int temp;
  char chr;
  int i;

  if(i2<0) i2=strlen(cin)-1;
  temp=0;
  for(i=i1; i<=i2; i++) {
    if((chr=cin[i])=='\0') break;
    if(chr>='0' && chr<='9') temp=(chr-'0')+10*temp;
  }
  return(temp);
}

/************************* str_sub_atol ***************************
* convert substr cin[i1:i2] into int32_t and return results
*  nonumeric characters are skipped
*  if i2<0, it is set to strlen(cin)-1
*
* Written December 2002 by Charles C Burch
*******************************************************************/
int32_t str_sub_atol(char *cin, int i1, int i2) {
  int32_t temp;
  char chr;
  int i;

  if(i2<0) i2=strlen(cin)-1;
  temp=0;
  for(i=i1; i<=i2; i++) {
    if((chr=cin[i])=='\0') break;
    if(chr>='0' && chr<='9') temp=(chr-'0')+10*temp;
  }
  return(temp);
}

/************************* str_sub_atoll ***************************
* convert substr cin[i1:i2] into int64_t and return results
*  nonumeric characters are skipped
*  if i2<0, it is set to strlen(cin)-1
*
* Written December 2002 by Charles C Burch
*******************************************************************/
int64_t str_sub_atoll(char *cin, int i1, int i2) {
  int64_t temp;
  char chr;
  int i;

  if(i2<0) i2=strlen(cin)-1;
  temp=0;
  for(i=i1; i<=i2; i++) {
    if((chr=cin[i])=='\0') break;
    if(chr>='0' && chr<='9') temp=(chr-'0')+10*temp;
  }
  return(temp);
}

/************************* str_sub_cmp ****************************
* compare substr cin[i1:i2] with str
* if i2<0, it is set so i2-i1+1=strlen(str)
*
* Written April 2003 by Charles C Burch
*******************************************************************/
int str_sub_cmp(char *cin, int i1, int i2, char *cmp) {
  int i, n;
  
  n=strlen(cin);
  if(i2==-1) i2=i1+strlen(cmp)-1;
  
  if(i2>=n || i1>i2) {
    if(cmp[0]=='\0') return(0);
    return(-1);
  }

  for(i=i1;i<=i2;i++) {
    if(cin[i]!=cmp[i-i1]) return(cin[i]-cmp[i-i1]); 
  }
  return(0);
}

/************************* str_sub_cmp_nocase *********************
* compare substr cin[i1:i2] with str
* if i2<0, it is set so i2-i1+1=strlen(str)
*
* Written April 2003 by Charles C Burch
*******************************************************************/
int str_sub_cmp_nocase(char *cin, int i1, int i2, char *cmp) {
  int i, n;
  
  n=strlen(cin);
  if(i2==-1) i2=i1+strlen(cmp)-1;
  
  if(i2>=n || i1>i2) {
    if(cmp[0]=='\0') return(0);
    return(-1);
  }

  for(i=i1;i<=i2;i++) {
    if(tolower(cin[i])!=tolower(cmp[i-i1])) 
      return(tolower(cin[i])-tolower(cmp[i-i1])); 
  }
  return(0);
}

/**************************** str_cmp_nocase **********************
*  strcmp with strings capitalized
*
* Written December 2002 by Charles C Burch
*******************************************************************/
int str_cmp_nocase(char *str1, char *str2) {
  int i;
  for(i=0;toupper(str1[i])==toupper(str2[i]); i++) {
    if(str1[i]=='\0') return(0);
  }
  return(toupper(str1[i])-toupper(str2[i]));
}

/********************** str_cmp_skip_blanks ************************
*  strcmp skipping blanks
*
* Written December 2002 by Charles C Burch
*******************************************************************/
int str_cmp_skip_blanks(char *str1, char *str2) {
  int i1,i2;
  i1=0;
  i2=0;
  while(1) {
    if(str1[i1]==' ') {
      i1++;
      if(str2[i2]==' ') i2++;
      continue;
    }  
    if(str2[i2]==' ') {
      i2++;
      continue;
    }
    if(str1[i1]!=str2[i2])  return(str1[i1]-str2[i2]);
    if(str1[i1]=='\0') return(0);
    i1++;
    i2++;
  }
}

/********************** str_cmp_nocase_skip_blanks *****************
*  strcmp capitalizing and skipping blanks
*
* Written December 2002 by Charles C Burch
*******************************************************************/
int str_cmp_nocase_skip_blanks(char *str1, char *str2) {
  int i1,i2;
  i1=0;
  i2=0;
  while(1) {
    if(str1[i1]==' ') {
      i1++;
      if(str2[i2]==' ') i2++;
      continue;
    }  
    if(str2[i2]==' ') {
      i2++;
      continue;
    }
    if(toupper(str1[i1])!=toupper(str2[i2]))  
      return(toupper(str1[i1])-toupper(str2[i2]));
    if(str1[i1]=='\0') return(0);
    i1++;
    i2++;
  }
}

/********************** str_is_string_blank *****************
*  returns 1 if str is all blanks and 0 otherwise
*
* Written December 2002 by Charles C Burch
**************************************************************/
int str_is_string_blank(char *str) {
  char ch;

  while((ch=(*str++))!='\0') {
    if(ch!=' ') return(0);
  }
  return(1);
}

/********************** str_is_string_valid_integer *****************
*  returns 1 if str is numeric and 0 otherwise
* leading or trailing blanks are allowed
*
* Written December 2002 by Charles C Burch
*********************************************************************/
int str_is_string_valid_integer(char *str) {
  char ch;
  int i;

  i=0;
  while((ch=str[i++])!='\0') {  /*skip leading blanks*/
    if(ch!=' ') break;
  }
  if(ch=='\0') return(0);  /*no non blanks*/
  i--;

  while((ch=str[i++])!='\0') {  /*check numeric part*/
    if(ch==' ') break;
    if(ch<'0' || ch>'9') return(0);
  }
  if(ch=='\0') return(1);

  while((ch=str[i++])!='\0') {  /*skip trailing blanks*/
    if(ch!=' ') return(0);      /*non blank found in trailing blanks*/
  }
  return(1);
}

/********************** str_is_string_valid_date *****************
*  returns 1 if str is a valid date and 0 otherwise
*
* Written December 2002 by Charles C Burch
******************************************************************/
int str_is_string_valid_date(char *str) {
  /*see if str[0:3] numeric*/
  if(str[4]!='-' || str[7]!='-' || str[10]!='\0') return(0);
  if(str[0]<'0' || str[0]>'9') return(0);
  if(str[1]<'0' || str[1]>'9') return(0);
  if(str[2]<'0' || str[2]>'9') return(0);
  if(str[3]<'0' || str[3]>'9') return(0);
  
  /*see if month 1 to 12*/
  if(str[5]=='0' || str[5]==' ') {
     if(str[6]<'1' || str[6]>'9') return(0);
  } else if(str[5]=='1') {
    if(str[6]<'0' || str[6]>'2') return(0);
  } else {
    return(0);
  }

  /*see if day 1 to 31--one could add month-specific check/leapyear*/
  if(str[8]==' ' || str[8]=='0') {
     if(str[9]<'1' || str[9]>'9') return(0);
  } else if(str[8]=='1' || str[8]=='2') {
     if(str[9]<'0' || str[9]>'9') return(0);
  } else if(str[8]=='3') {
     if(str[9]<'0' || str[9]>'1') return(0);
  } else {
    return(0);
  }
    
  return(1);
}

/********************** str_is_string_valid_time *****************
*  returns 1 if str is a valid time and 0 otherwise
*
* Written December 2002 by Charles C Burch
*****************************************************************/
int str_is_string_valid_time(char *str) {
  if(str[2]!=':' || str[5]!=':' || str[8]!='\0') return(0);

  /*see if hour 0 to 23*/
  if(str[0]==' ' || str[0]=='0' || str[0]=='1') {
     if(str[1]<'0' || str[1]>'9') return(0);
  } else if(str[0]=='2') {
    if(str[1]<'0' || str[1]>'3') return(0);
  } else {
    return(0);
  }

  /*see if minute 0 to 59*/
  if(str[3]==' ' || (str[3]>='0' && str[3]<='5') ) {
     if(str[4]<'0' || str[4]>'9') return(0);
  } else {
    return(0);
  }

  /*see if second 0 to 59*/
  if(str[6]==' ' || (str[6]>='0' && str[6]<='5') ) {
     if(str[7]<'0' || str[7]>'9') return(0);
  } else {
    return(0);
  }
    
  return(1);
}

/*-------------------------------- end ------------------------------------*/
/*-------------------------------- end ------------------------------------*/
/*-------------------------------- end ------------------------------------*/

