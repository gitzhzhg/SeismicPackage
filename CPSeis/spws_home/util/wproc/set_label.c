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
#include <assert.h>
#include <X11/Intrinsic.h>
#include <Xm/Xm.h>
#include <Xm/Label.h>
#include "wproc.h"
#include "cenv.h"




void set_label( Widget w,
                char   str[] )

{
  long n;
  XmString Xmstr;
  Arg arglist[3];
  static char zlenstr[2]=  "";

  if (!str) str= zlenstr;
  n=0;
  XtSetArg (arglist[n], XmNlabelString,
            Xmstr= XmStringCreateLtoR(str, 
            XmSTRING_DEFAULT_CHARSET ) ); n++;
  XtSetValues (w, arglist, n);
  XmStringFree( Xmstr);
}

char *get_simp_labelstrptr( Widget w )
{
  XmString          xmstr;
  char             *str;
  assert(w);
  XtVaGetValues(w, XmNlabelString, &xmstr,  NULL );

  str= get_string_from_xmstr(xmstr);
  if (xmstr) XmStringFree(xmstr);
  return str;
}


char *get_string_from_xmstr( XmString xmstr )
{
  XmStringContext   ctx;
  char              *retstr;
  char              *strptr;
  XmStringCharSet   cset;
  XmStringDirection dir;
  Boolean           stat = True;
  Boolean           sep;
  long              length = 0;
  long              num_lines = 0;
  int               i;

  assert(xmstr);

  if(!XmStringInitContext( &ctx, xmstr)) {
     XmStringFreeContext(ctx);
     retstr= NULL;
     return(retstr);
   }
  else {
     while(stat){
       stat= XmStringGetNextSegment( ctx, &strptr, &cset, &dir, &sep );
       if(stat) {
         length += strlen(strptr) + 1;
         ++num_lines;
         XtFree(strptr);
         XtFree(cset);
       }
     }
   }

  XmStringFreeContext(ctx);
  if(!XmStringInitContext( &ctx, xmstr)) {
     XmStringFreeContext(ctx);
     retstr= NULL;
     return(NULL);
   }

  if( (retstr = (char *) calloc( 1, length * sizeof(char) ) ) == NULL){
    printf("couldnt allocate memory for widget string in get_labelstrptr\n");
    return(NULL);
  }

  for(i=0;i<num_lines;i++){
    stat= XmStringGetNextSegment( ctx, &strptr, &cset, &dir, &sep );
    if(stat) {
      strcat(retstr,strptr);
      XtFree(strptr);
      XtFree(cset);
      if(i + 1 < num_lines)strcat(retstr,"\n");
    }
  }

  XmStringFreeContext(ctx);

  return (retstr);
}



char *get_simp_labelstr( Widget w,
                         char   str[] )

{
  char *strptr;
  
  strptr= get_simp_labelstrptr( w );
  if (strptr) {
       strcpy( str, strptr);
       XtFree(strptr);
  ENDif
  else
       str[0]= '\0';

  return (str);
}
