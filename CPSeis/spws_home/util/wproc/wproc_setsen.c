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
#include <stdarg.h>
#include <X11/Intrinsic.h>
#include <X11/StringDefs.h>
#include <Xm/Xm.h>
#include "wproc.h"
#include "cenv.h"



void wproc_setsen( wunion   *wary, 
                   Boolean  sensitive,
                   long     cnt,
                   ...)

/*
 *  the parameters are    wunion *    - widget array
 *                        Boolean     - for sensitive or insensitive
 *                        long        - how many widgets
 *                        int...      - indexes in widget array for each widget
 */
{ 

  va_list   args; 
  int     i, j;

  va_start(args, cnt);
  for(i=0; (i<cnt); i++) {
      j= va_arg(args, int); 
      XtSetSensitive( wary[j].any.w, sensitive );
      if (wary[j].any.wclass == TEXT)
          if (wary[j].textw.label)
                     XtSetSensitive( wary[j].textw.label, sensitive );
  }

  va_end(args);

}
