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
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <ctype.h>
#include <stdarg.h>

#define True  1
#define False 0

char *newstr( const char *s)

{

  char *s1;

  if (s) {
     s1= (char *)malloc( strlen(s) +1 );
     strcpy(s1,s);
  }
  else
     s1= NULL;

  return (s1);
}

/*
 *  malloc a newstr from a list of passed strings that is terminated by
 *  a NULL.
 *  example - str= newstrcat("This", " is", " a", " test.", NULL);
 *            str would equal "This is a test."
 *            the return value must be freed.
 *            
 */
char *newstrcat( char *s, ...)
{
 char *wkstr[100], *retstr= NULL;
 va_list   args;
 int newlen, i, j;
 int done;

 va_start(args, s);
 newlen= strlen(s);
 if (s) {
    for( i=0, done= False; ( (i<100) && (!done) ); i++) {
      wkstr[i]= va_arg(args, char*); 
      if (wkstr[i])
            newlen+= strlen(wkstr[i]);
      else
            done= True;
    }
    i--;
      
    retstr= (char *)malloc( newlen+1);
    strcpy(retstr, s);
    for( j=0; (j<i); j++) {
         strcat( retstr, wkstr[j] );
    }
    va_end(args);
 }

 return (retstr);
}


char *strToUpper( char *s)
{
 int i, len;
 
 if (s)
    for(i=0, len= strlen(s); (i<len); i++ ) s[i]= toupper(s[i]);

 return(s);
}

char *strToLower( char *s)
{
 int i, len;
 
 if (s)
    for(i=0, len= strlen(s); (i<len); i++ ) s[i]= tolower(s[i]);

 return(s);
}
