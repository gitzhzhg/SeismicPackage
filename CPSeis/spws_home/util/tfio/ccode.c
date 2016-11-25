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
C***************************** COPYRIGHT NOTICE *************************
C*                                                                      *
C*                 CONFIDENTIAL AND PROPRIETARY INFORMATION             *
C*                              OF CONOCO INC.                          *
C*                      PROTECTED BY THE COPYRIGHT LAW                  *
C*                          AS AN UNPUBLISHED WORK                      *
C*                                                                      *
C***************************** COPYRIGHT NOTICE *************************
c        1         2         3         4         5         6         7  |
c23456789012345678901234567890123456789012345678901234567890123456789012|
C\USER DOC
C-----------------------------------------------------------------------
C                       CONOCO PROCESSING SYSTEM
C                     EXPLORATION RESEARCH DIVISION
C                              CONOCO, INC.
C
C                       C P S   P R I M I T I V E
C
C  Primitive name:  CCODE    (Convert ascii to numeric)         
C         Library:  CONLIB & libtfio.a
C          Author:  Richard Day  
C    Date Written:  91/04/24
C    Last revised:  97/01/15  Day
C
C  Purpose:  Convert an ascii dcode list to numerical values
C
C_______________________________________________________________________
C                                NOTES
C Function Definition:        ( Language = C )
C  int  ccode_( char fmt[][8], char names[][32], int num,
C    char *mem, char *cards, char *msg )
C  fmt     in      An array of format statements for sscanf.
C                  "nn%d","nn%f","%nns","nn%g" are valid.
C  names   in      Array of keyword names to search for.
C  num     in      Number of members of fmt and names and the number
C                  of objects to return.
C  mem     out     Starting memory address where integers, floats,
C                  and strings will be returned.
C  cards   in      The character string with the input decode data.
C  msg     out     "OK" if all goes well. An error message.
C 
C  1. ccode returns the number of name fields converted. 
C  2. ccode will handle integer and float arrays.
C  3. nn indicates the number of bytes of memory to employ for storing
C     character data(including the null). Character strings are limited
C     to 120 bytes. nn indicates the number of array members for 
C     ints and floats.
C
C-----------------------------------------------------------------------
C Revisions:
C  DATE      WHO         DESCRIPTION
C  --------  --------    --------------------------------------------
C  01/15/97  R.Day       documentation change only
C  09/19/96  R.Day       long to int changes, and arg changes
C  07/13/93  R.Day       Added support for %g format
C  10/09/92  R.Day       An = replaced by == in an if statement
C  10/22/91  R.DAY       Parsed extra trailing blank from field.
C-----------------------------------------------------------------------
C   SUBROUTINE, FUNCTION, ENTRY, AND COMMON BLOCK NAMES IN THIS MODULE
C
C  Subroutines:     CCODE
C  Functions:       none
C_______________________________________________________________________
C                 EXTERNALS REFERENCED BY THIS MODULE
C
C                              none
C-----------------------------------------------------------------------
C                       MEMORY REQUIREMENTS
C
C  Storage         -
C  Heap (dynamic)  -
C-----------------------------------------------------------------------
C\END DOC
*/
#include <string.h>
#include <stdlib.h>


int  ccode_ch2f(char str[], int , float a[]);
int  ccode_ch2l(char str[], int , int   a[]);

int  ccode_( char fmt[][8], char names[][32], int off[], int  num,
     char *mem, char *cards, char *msg )
{/* assumes names and memory are in same order */
 /* if array parsing fails memory can get out of sync */
  char   *jp, *jeq, *jf, *jj;
  char   *maddr;
  char   parse[120], cr[2];
  int    sizl, sizf, i, l, ncon;
  int    n;
  strcpy(msg,"OK");

  sizf=sizeof(float);
  sizl=sizeof(int );
  ncon=0;
  cr[0] = '\n'; cr[1]='\0';
  for (i=0;i<num; i++)   /* crack the ith field */
   { parse[0]='\0';
     jp = strstr(cards,names[i]);
     if(jp==NULL) continue; /* skip missing names */
     jeq= strstr(jp,"=");
     if(jeq==NULL)
       { strcpy(msg,"02:ccode: bad ccode format");
         goto error; }
     jf= strstr(jp,",");
     if( jf==NULL )
       { if( (jf=strstr(jp, cr)) == NULL)
           { if(i== num-1)
              { l=strlen(jp);
                jf=jp+l;
              }
             else { strcpy(msg,"02:ccode: bad ccode format");
                     goto error; }
           }
       }

     if(jf-jeq-1 > 119)
       { strncpy(parse,jeq+1,119);
         parse[119]='\0';  /* make sure we null terminate */
       }
     else
       { strncpy(parse,jeq+1,jf-jeq-2); /* 10/22/91 R.DAY 1 to 2 */
         parse[jf-jeq-2]='\0';
       }

     if( (jj = strstr(fmt[i],"%d")) !=NULL)
      { n=1; 
        if(jj!=fmt[i]) n = atoi(fmt[i]);
        maddr = mem + off[i];
        n = ccode_ch2l(jeq+1,n,(int  *) maddr);
        ncon += 1;
      }
     else if( (jj=strstr(fmt[i],"%f")) !=NULL)
      { n=1; 
        if(jj!=fmt[i]) n = atoi(fmt[i]);
        maddr = mem + off[i];
        n = ccode_ch2f(jeq+1,n,(float *) maddr);
        ncon += 1;
      }
     else if( (jj=strstr(fmt[i],"%g")) !=NULL)
      { n=1;
        if(jj!=fmt[i]) n = atoi(fmt[i]);
        maddr = mem + off[i];
        n = ccode_ch2f(jeq+1,n,(float *) maddr);
        ncon += 1;
      }
     else if( (jj = strstr(fmt[i],"s")) !=NULL)
      { n=1;
        if(jj-fmt[i] >1 ) n = atoi(fmt[i]+1);
        l=n-1;
        if(n<1) { n=1; l=0; }
        if(n>119) l=119;
        strncpy(mem+off[i],parse,l);
        mem[off[i]+l]='\0';    /* make sure null terminates string*/
        ncon += 1;
      }
     else
       {strcpy(msg,"03:ccode: bad ccode format");
        goto error; }
   }

 return ncon;
 error:
 return ncon;

}

/*
 * Convert ascii numeric fields delimited by ',', ' ', '\t', or '\n'
 * to floats and return the number of floats found.
 * On input nv is the maximum number of fields to convert.
 * nv returns the actual number converted */
int  ccode_ch2f(char str[], int  nv, float a[])
{ int    n, num, nch;
  float  x;
  double xx;

  if(nv==0) return nv;
  num = nv;
  nv=0;

  nch=0;
  for(n=0;n<=strlen(str);n++)
    { if( str[n]==' ' || str[n]==',' || str[n]=='\t' || str[n]=='\n' )
         { if(nch>0) 
             { xx = atof(str+n-nch);
               nv += 1;
               x = xx;
               a[nv-1] = x ;
               if(nv >= num) break;
               nch=0; }
         }
         else { nch += 1; }
     }

 return nv;
}

int  ccode_ch2l(char str[], int  nv, int  a[])
{ int   n, num, nch;

  if(nv==0) return nv;
  num = nv;
  nv=0;

  nch=0;
  for(n=0;n<=strlen(str);n++)
    { if( str[n]==' ' || str[n]==',' || str[n]=='\t' || str[n]=='\n' )
         { if(nch>0) 
             { a[nv] = atoi(str+n-nch);
               nv += 1;
               if(nv >= num) break;
               nch=0; }
         }
         else { nch += 1; }
     }

 return nv;
}
