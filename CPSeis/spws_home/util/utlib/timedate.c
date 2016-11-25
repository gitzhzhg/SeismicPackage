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
#include <time.h>
#include <stdio.h>
#include <string.h>
#define TIME_SIZE 24

/*-------------------------------------------------------------
C\USER DOC
C Name   : timedate_sec, timedate_date, timedate_time
C Purpose: portable time function
C Author : R. Day
C Last Revised: 98/12/04
C 
C--------------------------------------------------------------
C Function Definition:        ( Language = C )
C
C    void timedate_date_(char *buff)
C    void timedate_time_(char *buff)
C    void timedate_sec_(float *etime) 
C 
C--------------------------------------------------------------
C NOTES:
C  1. The date is returned as YYYY/MM/DD
C  2. The time is returned as HH.MM.SS
C
C--------------------------------------------------------------
C                         REVISION HISTORY
C
C   DATE     WHO         DESCRIPTION
C   -------- --------    --------------------------------------
C 1.98/12/04 R.S.Day     Original version
C
C\END DOC
 *------------------------------------------------------------*/

#ifdef CRAY
#define timedate_sec_  TIMEDATE_SEC
#define timedate_date_ TIMEDATE_DATE
#define timedate_time_ TIMEDATE_TIME
#endif

#if(VMS || _AIX || __hpux)
#define timedate_sec_  timedate_sec
#define timedate_date_ timedate_date
#define timedate_time_ timedate_time
#endif


void timedate_date_(char *buff)
{ size_t i;
  time_t current;
  struct tm *now;

  current = time(NULL);
  now = localtime(&current);
/*
  *imonth = now -> tm_mon + 1;
  *iday = now -> tm_mday;
  *iyear = now -> tm_year + 1900;
*/
  i = strftime(buff,11,"%Y/%m/%d",now);
}

void timedate_time_(char *buff)
{size_t i;
  time_t current;
  struct tm *now;

  current = time(NULL);
  now = localtime(&current);
  i = strftime(buff,9,"%H.%M.%S",now);
}

void timedate_sec_(float *etime) 
{int div;
 float e;
#ifdef __ultrix
 div = 60;
#else
 /* wmm div=CLK_TCK; */
 div=CLOCKS_PER_SEC;
#endif
 e = clock()/div;
 *etime = e;
}
