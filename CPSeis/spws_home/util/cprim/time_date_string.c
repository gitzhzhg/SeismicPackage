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
                         time_date_string.c

************************* COPYRIGHT NOTICE ****************************
*      CONFIDENTIAL AND PROPRIETARY INFORMATION OF CONOCO INC.        *
*       PROTECTED BY THE COPYRIGHT LAW AS AN UNPUBLISHED WORK         *
************************* COPYRIGHT NOTICE ****************************
C\USER DOC
-----------------------------------------------------------------------
                    SEISMIC PROCESSING WORKSTATION
                          C-LANGUAGE UTILITY 
                  designed to be called from C or C++

  Utility name:  time_date_string    (make a time/date string)

  Subdirectory:  cprim
  Library:       cprim.a
  Header file:   cprim.h
  Source file:   time_date_string.c

  Written:       94/12/12  by:  Tom Stoeckley
  Last revised:  98/12/04  by:  R. Day

  Purpose:       To return a string which has been set to the
                 current time and date.

  Related Documentation:
-----------------------------------------------------------------------
  To get a time/date string:

              char *time_date_string (void)

  The returned pointer points to a static area which should not
  be modified or freed.  This string should be copied to wherever
  it is needed before the next call to this routine, because the
  same static area is used each time.


  To get a special formated time or date string

              void time_date_stringf_(char *buff, int *flag)

  When flag = 0 the date is returned in buff as YYYY/MM/DD
  When flag !=0 the time is returned in buff as HH.MM.SS
-----------------------------------------------------------------------
                        REVISION HISTORY

     Date      Author     Description
     ----      ------     -----------
  2. 98/12/04  Day        Added time_date_stringf_
  1. 94/12/12  Stoeckley  Initial version.
-----------------------------------------------------------------------
C\END DOC
*/

#include <time.h>
#include <string.h>
#include <stdlib.h>
#include <assert.h>
#include "cprim.h"


    /* The returned pointer from ctime points to an area in
       static memory managed by ctime and other time-related
       functions referred to in the time.h header file.
       This pointer must not be deallocated. */


char *time_date_string(void)
{
  time_t tp;
  char *string;
  static char buffer[100];
  int length;

  tp = time(NULL);
  string = ctime(&tp);
  strcpy(buffer, string);
  length = strlen(buffer);
  if(length > 0 && buffer[length - 1] == '\n')
                   buffer[length - 1] = '\0';
  return buffer;
}

void time_date_stringf_(char *buff, int *flag)
{ size_t i;
  time_t current;
  struct tm *now;
 
  current = time(NULL);
  now = localtime(&current);
  if(*flag==0)
   i = strftime(buff,11,"%Y/%m/%d",now);
  else
   i = strftime(buff,9,"%H.%M.%S",now);
}



