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
/*
C      string_copy.c
C************************* COPYRIGHT NOTICE ****************************
C*      CONFIDENTIAL AND PROPRIETARY INFORMATION OF CONOCO INC.        *
C*       PROTECTED BY THE COPYRIGHT LAW AS AN UNPUBLISHED WORK         *
C************************* COPYRIGHT NOTICE ****************************
C\USER DOC
C-----------------------------------------------------------------------
C                     SEISMIC PROCESSING WORKSTATION
C                             U T I L I T Y 
C              written in c -- designed to be called from c
C
C     Utility Name:  string_copy      (copy character strings) 
C          Written:  93/03/05  by:  Tom Stoeckley
C     Last revised:  94/12/01  by:  Tom Stoeckley
C
C  Purpose:  More general (and safer) string copy, compare, and
C            concatenation routines than the C-language equivalents.
C            Plus a routine which flexibly copies null-terminated or
C            blank-filled strings.  The blank-filled strings are 
C            normally encountered when they reside in an area also
C            referenced by Fortran.
C
C  Related Documentation:
C-----------------------------------------------------------------------
C                          THIS UTILITY
C
C  node:                   pospsv (ultrix)
C  source code directory:  ~spws/util/cprim   (shared)
C  library:                cprim.a            (shared)
C  header file:            cprim.h            (shared)
C  source file:            string_copy.c
C  documented functions:   listed below
C  static functions:       none
C
C  The user should include the above header file in his code.
C-----------------------------------------------------------------------
C                      EXTERNAL REFERENCES
C       (this utility does not reference X, Xt, and Motif)
C            (standard C references not listed here)
C
C  Libraries:     none
C  Header files:  cprim.h
C  Functions:     none
C-----------------------------------------------------------------------
C                           REVISION HISTORY
C
C     Date      Author     Description
C     ----      ------     -----------
C  7. 94/12/01  Stoeckley  Make string_copy do nothing if both strings
C                            have the same address.
C  6. 94/05/19  Stoeckley  Add strings_equal.
C  5. 94/02/28  Stoeckley  Fix bug in string_alloc_cat.
C  4. 94/01/18  Stoeckley  Add string_alloc_cat.
C  3. 93/12/30  Stoeckley  Add string_alloc and string_free.
C  2. 93/12/14  Stoeckley  Add string_length and string_compare.
C  1. 93/03/05  Stoeckley  Initial version.
C-----------------------------------------------------------------------
C-----------------------------------------------------------------------
C-----------------------------------------------------------------------
C-----------------------------------------------------------------------
C-----------------------------------------------------------------------
C  To do safe string compares:
C
C         int  safe_strcmp   (char *cs, char *ct)
C         int  safe_strncmp  (char  *s, char *ct, size_t n)
C
C  These are identical to their C library counterparts, except
C    they will work if either string is empty or has a NULL pointer.
C  These return  0 if cs = ct
C  These return <0 if cs < ct
C  These return >0 if cs > ct
C  A NULL pointer  is considered smaller than a valid pointer.  
C  An empty string is considered smaller than a non-empty string.
C-----------------------------------------------------------------------
C  To do safe string copies:
C
C         char *safe_strcpy   (char *s, char *ct)
C         char *safe_strncpy  (char *s, char *ct, size_t n)
C
C  These are identical to their C library counterparts, except
C    they will work if either string is empty or has a NULL pointer.
C  Nothing is copied if s is NULL.
C  String s is set to empty string if ct is null or empty.
C  The routine safe_strncpy is the same as safe_strcpy if n <= 0.
C-----------------------------------------------------------------------
C  To do safe string concatenations:
C
C         char *safe_strcat   (char *s, char *ct)
C         char *safe_strncat  (char *s, char *ct, size_t n)
C
C  These are identical to their C library counterparts, except
C    they will work if either string is empty or has a NULL pointer.
C  Nothing is concatenated if s is NULL.
C  String s is unchanged if ct is null or empty.
C  The routine safe_strncat is the same as safe_strcat if n <= 0.
C-----------------------------------------------------------------------
C  To do special string copies:
C
C                 s = string_copy(s, ns,   ct, nct)
C
C  char  *s = output string.
C  long  ns = length of output string.
C  char *ct = input string.
C  long nct = length of input string.
C
C  Properties of input string ct:
C  nct = 0 : null-terminated with any length.
C  nct > 0 : has length up to nct (+ null) and may be null-terminated.
C  nct < 0 : has length exactly -nct and is blank-filled.
C
C  Properties of output string s:
C  ns = 0 : null-terminated (no trailing blanks) with any length.
C  ns > 0 : null-filled (no trailing blanks) with length ns (+ last null).
C  ns < 0 : blank-filled with length exactly -ns.
C
C  If the input and output strings have the same address, nothing is done.
C  If the input and output strings do not have the same address, but
C    do overlap, the results are undefined.
C-----------------------------------------------------------------------
C  Get special string lengths:
C
C                 int   string_length(ct, nct)
C
C  char    *ct = input string.
C  long    nct = length of string.
C
C  Properties of string ct:
C  nct = 0 : null-terminated with any length.
C  nct > 0 : has length up to nct (+ null) and may be null-terminated.
C  nct < 0 : has length exactly -nct and is blank-filled.
C
C  The returned length is zero if the string has a NULL pointer.
C  The returned length is the actual length without trailing blanks.
C-----------------------------------------------------------------------
C  To do special string compares:
C
C               int   string_compare(cs, ncs,   ct, nct)
C
C  char *cs = first string.
C  long ncs = length of first string.
C  char *ct = second string.
C  long nct = length of second string.
C
C  Properties of each string cs and ct:
C  ncs or nct = 0 : null-terminated with any length.
C  ncs or nct > 0 : has length up to nct (+ null) and may be null-terminated.
C  ncs or nct < 0 : has length exactly -nct and is blank-filled.
C
C  This will work if either string is empty or has a NULL pointer.
C  This returns  0 if cs = ct
C  This returns <0 if cs < ct
C  This returns >0 if cs > ct
C  A NULL pointer  is considered smaller than a valid pointer.  
C  An empty string is considered smaller than a non-empty string.
C-----------------------------------------------------------------------
C  To find out if two strings are equal:
C
C              int strings_equal(cs, ct)
C
C  char *cs = first string (null-terminated).
C  char *ct = second string (null-terminated).
C
C  Returns TRUE  (1) if the two strings are equal.
C  Returns FALSE (0) if the two strings are not equal.
C  A NULL pointer is treated as a zero-length string.
C-----------------------------------------------------------------------
C  To copy one string to another, allocating space as needed, and
C  also to free the allocated space:
C
C         s = string_alloc    (s, ct, nct)   copy ct to s
C         s = string_alloc_cat(s, ct, nct)   concatenate ct to s
C         s = string_free     (s)            free s
C
C  char     *s = output string (can be NULL).
C  char    *ct = input string (can be NULL).
C  long    nct = length of string ct.
C
C  Properties of string s:
C  null-terminated with any length.
C
C  Properties of string ct:
C  nct = 0 : null-terminated with any length.
C  nct > 0 : has length up to nct (+ null) and may be null-terminated.
C  nct < 0 : has length exactly -nct and is blank-filled.
C
C  The FIRST function reallocates string s to exactly hold the contents
C  of string ct (plus a null termination), and then copies ct into s.
C
C  The SECOND function reallocates string s to exactly hold the original
C  contents of string s, followed by the contents of string ct (plus a
C  null termination), and then concatenates ct into s.
C
C  For the FIRST and SECOND functions, if the input argument s is NULL,
C  s will be allocated; otherwise reallocated.  The returned value (and
C  contents) will be the same as the argument s if the allocation or
C  reallocation fails.
C
C  The THIRD function deallocates string s, and always returns NULL.
C  String s must previously have been allocated by malloc, calloc,
C  realloc, or string_alloc.
C
C  The returned value for each function should be placed into the
C  same variable s that was supplied as the first argument in the
C  argument list, unless the first argument is NULL.
C-----------------------------------------------------------------------
C                                NOTES
C
C 1.
C-----------------------------------------------------------------------
C\END DOC
*/



/*----------------------- header files ---------------------------------*/

#include <string.h>
#include "cprim.h" 



/*------------- safe string compares -----------------------------------*/

int safe_strcmp(char *cs, char *ct)
{
  if(cs    == NULL && ct    == NULL) return  0;
  if(cs    == NULL)                  return -1;
  if(ct    == NULL)                  return  1;
  if(cs[0] == '\0' && ct[0] == '\0') return  0;
  if(cs[0] == '\0')                  return -1;
  if(ct[0] == '\0')                  return  1;
  return strcmp(cs, ct);
}


int safe_strncmp(char *cs, char *ct, size_t n)
{
  if(cs    == NULL && ct    == NULL) return  0;
  if(cs    == NULL)                  return -1;
  if(ct    == NULL)                  return  1;
  if(cs[0] == '\0' && ct[0] == '\0') return  0;
  if(cs[0] == '\0')                  return -1;
  if(ct[0] == '\0')                  return  1;
  return strncmp(cs, ct, n);
}




/*------------- safe string copies -------------------------------------*/


char *safe_strcpy(char *s, char *ct)
{
  if(s == NULL) return s;
  if(ct == NULL || ct[0] == '\0') { s[0] = '\0'; return s; }
  return strcpy(s, ct);
}


char *safe_strncpy(char *s, char *ct, size_t n)
{
  if(n <= 0) return safe_strcpy(s, ct);
  if(s == NULL) return s;
  if(ct == NULL || ct[0] == '\0') { s[0] = '\0'; return s; }
  return strncpy(s, ct, n);
}



/*------------- safe string concatenations -----------------------------*/


char *safe_strcat(char *s, char *ct)
{
  if(s == NULL || ct == NULL || ct[0] == '\0') return s;
  return strcat(s, ct);
}


char *safe_strncat(char *s, char *ct, size_t n)
{
  if(n <= 0) return safe_strcat(s, ct);
  if(s == NULL || ct == NULL || ct[0] == '\0') return s;
  return strncat(s, ct, n);
}




/*------------- special string copy ------------------------------------*/

char *string_copy(char *s, long ns,    char *ct, long nct)
{
  int i, zero = '\0', blank = ' ';

  if(s == ct) return s;
                         /* preset an empty output string */
  if(s == NULL) return s;
  if     (ns > 0) memset(s, zero,  ns +1);
  else if(ns < 0) memset(s,blank, -ns   );
  else if(nct> 0) memset(s, zero,  nct+1);
  else if(nct< 0) memset(s, zero, -nct+1);
  else            s[0] = zero;

                         /* return if input string is empty */
  if(ct == NULL) return s;
  if(nct >= 0 && ct[0] == zero) return s;

  if(ns == 0)
       {
       if     ( nct == 0) strcpy (s, ct);
       else if( nct >  0) strncpy(s, ct,  nct);
       else               memcpy (s, ct, -nct);
       }
  else if(ns > 0)
       {
       if     ( nct == 0) strncpy(s, ct,  ns );
       else if( nct > ns) strncpy(s, ct,  ns );
       else if( nct >  0) strncpy(s, ct,  nct);
       else if(-nct > ns) memcpy (s, ct,  ns );
       else               memcpy (s, ct, -nct);
       }
  else
       if     (nct <  ns) memcpy (s, ct, -ns );
       else if(nct <   0) memcpy (s, ct, -nct);
       else if(nct ==  0) strncpy(s, ct, -ns );
       else if(nct > -ns) strncpy(s, ct, -ns );
       else if(nct >   0) strncpy(s, ct,  nct);

                         /* remove trailing blanks */
  if(ns >= 0)
       {
       i = strlen(s) - 1;
       while(i >= 0 && s[i] == blank) { s[i] = zero; i--; }
       }
                         /* replace nulls with blanks */
  else if(ns < 0)
       {
       int ended = 0;
       for(i = 0; i < -ns; i++)
            {
            if(s[i] == zero || ended) {s[i] = blank; ended = 1; }
            }
       }
  return s;
}



/*---------------- get length of special string ------------------------*/

int string_length(char *cs, long ncs)
{
  int length, i, zero = '\0', blank = ' ';

  if(!cs) return 0;
  if(ncs == 0)
       {
       length = strlen(cs);
       }
  else if(ncs < 0)
       {
       length = -ncs;
       }
  else      /* ncs > 0 */
       {
       length = ncs;
       for(i = length - 1; i >= 0; i--)
            {
            if(cs[i] == zero) length = i;
            }
       }
  while(length > 0 && cs[length-1] == blank) { length--; }
  return length;
}



/*------------- special string compare ---------------------------------*/

int string_compare(char *cs, long ncs,    char *ct, long nct)
{
  int lencs, lenct, zero = '\0', blank = ' ';

  if(cs    == NULL && ct    == NULL) return  0;
  if(cs    == NULL)                  return -1;
  if(ct    == NULL)                  return  1;
  lencs = string_length(cs, ncs);
  lenct = string_length(ct, nct);
  if(lencs == 0 && lenct == 0) return  0;
  if(lencs < lenct)            return -1;
  if(lenct < lencs)            return  1;
  return strncmp(cs, ct, lencs);
}


/*------------- find out if strings are equal ----------------------*/

int strings_equal(char *cs, char *ct)
{
  if(cs == NULL && ct == NULL)    return TRUE;
  if(cs == NULL && ct[0] == '\0') return TRUE;
  if(ct == NULL && cs[0] == '\0') return TRUE;
  if(cs == NULL || ct == NULL)    return FALSE;
  if(strcmp(cs, ct) == 0)         return TRUE;
  return FALSE;
}


/*---- special string alloc/copy, alloc/cat, and free ------------------*/


char *string_alloc(char *s, char *ct, long nct)
{
  char *snew;
  long length = string_length(ct, nct);
  if(length == 0) return string_free(s);
  if(!s) snew = (char*)malloc (   length + 1);
  else   snew = (char*)realloc(s, length + 1);
  if(!snew) return s;
  s = snew;
  string_copy(s, length, ct, nct);
  return s;
}


char *string_alloc_cat(char *s, char *ct, long nct)
{
  char *snew, *ct_temp;
  long length, length2;
  length = string_length(ct, nct);
  if(length == 0) return s;
  length2 = strlen(s);
  if(length2 == 0) return string_alloc(s, ct, nct);
/*
  if(s) snew = (char*)malloc (   length + length2 + 2);
*/
  if(!s) snew = (char*)malloc (   length + length2 + 2);
  else   snew = (char*)realloc(s, length + length2 + 2);
  if(!snew) return s;
  s = snew;
  ct_temp = string_alloc(NULL, ct, nct);
  strcat(s, ct_temp);
  string_free(ct_temp);
  return s;
}


char *string_free(char *s)
{
  int len, free_it, k2;
  if (s) {
/* 
 * this is a shameful patch for getting around corrupted
 * strings that are erroneously passed in whose source
 * I haven't been able to track down
 */
    for (k2 = 0, len = strlen(s), free_it = 1;
      free_it != 0 && k2 < len; k2++) {
      free_it = isalnum (s[k2]);
    }
    if (len > 0 && free_it != 0) {
      free(s); /* else let it leak!!! */
    }
  }
  return NULL;
}

/*--------------------------- end --------------------------------------*/

