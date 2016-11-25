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
#include "wproc.h"
#include <X11/Intrinsic.h>
#include <Xm/Xm.h>



char *make_wtree( Widget w,
                  char   wstr[],
                  int    level )

/*
 * recursivly make the widget and return a string with that tree.
 */

{
   char *wname;
   char parstr[400];
   int  len;

   parstr[0]= '\0';

   if ( (w) && ( (level > 0) || (level == wprocALL ) ))  {
      level = (level == wprocALL) ? wprocALL : --level;
      make_wtree( XtParent(w), parstr, level );
      wname=  XtName(w);
      len= strlen(parstr);
      if (len > 0)
          if (parstr[len-1] != '*') strcat( parstr, ".");
      strcat( parstr, wname);
      strcpy( wstr, parstr);
   } /* end if */
   else if ( (w) && (level != wprocALL) )
      strcpy( wstr, "*");

   return(wstr);

}
