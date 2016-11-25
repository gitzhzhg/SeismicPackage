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
#include "cenv.h"
#include "wproc.h"
#include <ctype.h>
#include <math.h>
#include <stdlib.h>
#include <limits.h>


Boolean check_int (char str[], int *val)
{
  Boolean retval;
  long lval;
  retval = check_long (str, &lval);
  lval = lval < INT_MIN ? INT_MIN : lval > INT_MAX ? INT_MAX : lval;
  *val = (int)lval;
  return retval;
}

Boolean check_long (char str[], long *val)
{
  int len = 1, i;
  boolean goodfld, allspace, more;
  char *strptr, *endptr;

  if (str) {
    len = strlen (str);
    goodfld = True;
    allspace = True;
    for (i=0; i < len; i++) {
      if (!isdigit(str[i]) && !isspace(str[i])) {
	goodfld = False;
      }
      if (!isspace(str[i])) {
	allspace = False;
      }
    }
  }
  else {
    goodfld = False;
    allspace = False;
  }
  if (goodfld && len > 0) {
    for (i = len-1, more = True; more; i--) {
      more = i >= 0 ? isspace(str[i]) : False; 
      if (more) str[i] = '\0';
    }
    len = strlen (str);
    strptr = str;
    endptr = strptr + len;
    *val = strtol (str, &strptr, 10);
    if ((*val == 0 && strptr == str) || strptr != endptr) {
      goodfld = False;
    }
    else {
      goodfld = True;
    }
  }
  else { 
    if (len == 0 || allspace) {
      *val = 0;
      goodfld = True;
    }
  }
  return goodfld;
}
