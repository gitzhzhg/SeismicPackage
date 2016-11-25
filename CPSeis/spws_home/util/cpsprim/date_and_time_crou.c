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

/*------------------- date_and_time_crou.c -----------------------------*/
/*------------------- date_and_time_crou.c -----------------------------*/
/*------------------- date_and_time_crou.c -----------------------------*/

          /* part of the Fortran CPS primitive date_and_time */

#include "c2f_interface.h"

#include <stdlib.h>
#include <string.h>
#include "str.h"


#if (VMS || _AIX || __hpux)
#define date_and_time_crou_   date_and_time_crou
#endif

#ifdef NEED_CAPITALS
#define date_and_time_crou_   DATE_AND_TIME_CROU
#endif



void date_and_time_crou_(char* buffer)
{
  const char *point = str_time_date();
  strcpy(buffer, point);
}


