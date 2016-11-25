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



char *make_wctree( Widget w,
                  char   wstr[],
                  char   cstr[],
                  int    level )

/*
 * recursivly make the widget and return a string with that tree.
 */

{
   char *wname;
   char *cname;
   char w_parstr[400];
   char c_parstr[400];
   int  wlen, clen;

   w_parstr[0]= '\0';
   c_parstr[0]= '\0';

   if ( (w) && ( (level > 0) || (level == wprocALL ) ))  {
      level = (level == wprocALL) ? wprocALL : --level;
      make_wctree( XtParent(w), w_parstr, c_parstr, level );
      wname=  XtName(w);
      cname=  class_name(w);
      wlen= strlen(w_parstr);
      clen= strlen(c_parstr);
      if (wlen > 0)
          if (w_parstr[wlen-1] != '*') strcat( w_parstr, ".");
      if (clen > 0)
          if (c_parstr[clen-1] != '*') strcat( c_parstr, ".");
      strcat( w_parstr, wname);
      strcat( c_parstr, cname);
      strcpy( wstr, w_parstr);
      strcpy( cstr, c_parstr);
   } /* end if */
   else if ( (w) && (level != wprocALL) ) {
      strcpy( wstr, "*");
      strcpy( cstr, "*");
   }

   return(wstr);

}


/*
 * return the string value of an application created resource
 * this string is static and must be copied
 * w      - widget to build the resource on
 * inststr- instance string such as "myfont" that will be concatenated
 *          to the instance tree of the widget
 * clstr-   class string such as "MyFont" that will be concatenated
 *          to the class tree of the widget
 * default_res- the default value of this resource in case it is not found
 */
char *wproc_get_resource(Widget  w, 
                         char   *inststr, 
                         char   *clstr,
                         char   *default_res)
{
 char instance_res[1000];
 char class_res[1000];
 char *res;

 make_wctree( w, instance_res, class_res, wprocALL);
 strcat(instance_res, ".");
 strcat(instance_res, inststr);
 strcat(class_res, ".");
 strcat(class_res, clstr);
 res= DefGetStrValue( XtDisplay(w), instance_res, class_res);
 if (!res) res= default_res;
 return res;
}
