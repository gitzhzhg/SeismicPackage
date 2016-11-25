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
/*------------------------------------------------------------------
C\USER DOC
 *Name   : cncode_
 *Purpose: Convert numerical values to an ascii keyword=value list.
 *Author : R. Day
 *Date   : 04/24/91
 *Revised: 10/23/96  Day
 *
 *Function Definition:        ( Language = C )
 * int  cncode_( char fmt[][8], char names[][32], int num,
 *   char *mem, char *parse, char *msg )
 * fmt     in      An array of format statements for sprintf.
 *                 "nn%d","nn%f","%nns","nn%g" are valid.
 * names   in      Array of keyword names to search for.
 * num     in      Number of members of fmt and names,and the number
 *                 of objects to return.
 * mem     in      Starting memory address where integers, floats,
 *                 and strings will be returned.
 * parse   out     The output character string with the keyword list.
 * msg     out     "OK" if all goes well. An error message.
 *
 *NOTES:
 * 1. cncode returns 0 if all is OK.
 * 2. cncode will handle scalars and arrays.
 * 3. nn indicates the number of bytes of memory to employ for storing
 *    character data. Character strings are limited to 120 bytes.
 *    -OR- nn is the number of floats or ints following the keyword.
 *
 *Revisions:
 * DATE      WHO         DESCRIPTION
 * --------  --------    --------------------------------------------
 * 96/10/23  R.Day       long to int changes, and arg changes
 * 96/07/17  Vunderink   Inserted into the conlib library.
C\END DOC
 *------------------------------------------------------------------*/
#include <string.h>
#include <stdlib.h>
#include <stdio.h>

#define CSIZE 80

int  cncode_( char fmt[][8], char names[][32], int off[], int  num,
     char *mem, char *parse, char *msg )
{/* names and memory must be in sync */
  int    sizl, sizf, LT, LC, LN, i, l; 
  int    lw,n;
  float  fw;
  char   cr[2], tmp[120], card[120], tc[16], *jj;
  strcpy(msg,"OK");
/*
 * Process the list of values passed by the user */
 sizf=sizeof(float);
 sizl=sizeof(int);
 parse[0]='\0';
 cr[0]   = '\n'; cr[1] ='\0';
 card[0] =' '; card[1] ='\0';
 for (i=0;i<num; i++)
   { strcpy(tmp,"= ");
     if( (jj=strstr(fmt[i],"%d")) !=NULL)
      { n=1; 
        if(jj!=fmt[i]) n = atoi(fmt[i]);
        for(l=0;l<n;l++)
          { lw = *(int  *) (mem+off[i] + l*sizl);
            sprintf(tc,jj,lw);
            strcat(tmp,tc);
            if(l<n-1) strcat(tmp," , ");
          }
      }
     else if((jj=strstr(fmt[i],"%f")) !=NULL)
      { n=1; 
        if(jj!=fmt[i]) n = atoi(fmt[i]);
        for(l=0;l<n;l++)
          { fw = *(float *) (mem+off[i]+l*sizf);
            sprintf(tc,jj,fw);
            strcat(tmp,tc);
            if(l<n-1) strcat(tmp," , ");
          }
      }
     else if((jj=strstr(fmt[i],"%g")) !=NULL)
      { n=1;
        if(jj!=fmt[i]) n = atoi(fmt[i]);
        for(l=0;l<n;l++)
          { fw = *(float *) (mem+off[i]+l*sizf);
            sprintf(tc,jj,fw);
            strcat(tmp,tc);
            if(l<n-1) strcat(tmp," , ");
          }
      }
     else if( (jj = strstr(fmt[i],"s")) !=NULL)
      { n=1;
        if(jj-fmt[i] >1 ) n = atoi(fmt[i]+1);
        l = n-1;
        if(n<1) { n=1; l=0; }
        if(n>119) l=119;
        strncpy(tmp+1, mem+off[i], l); /* no blank for string */
        tmp[l+1]= '\0';
      }
     else
       {strcpy(msg,"01:cncode: bad cncode format");
        goto error; }
     n=strlen(tmp);
     for(l=n-1;l>=0;l--)   /*parse trailing blanks*/
         if(tmp[l]==' ') tmp[l]='\0';
         else break;

     if(i != num-1 ) strcat(tmp," , ");

     LT= strlen(tmp);      /* Numerical field + blanks + comma */
     LC= strlen(card);     /* Current card image size */
     LN= strlen(names[i]); /* Name size */
     if(LN+LT >= CSIZE ) 
       { for(l=LC;l<=CSIZE-1;l++) card[l] = ' ';
         card[CSIZE]='\n'; card[CSIZE+1]='\0';
         strcat(parse,card);
         strcat(parse," ");
         strcat(parse,names[i]);
         strcat(parse,tmp);
         strcat(parse,cr);
         card[0] =' '; card[1] ='\0'; }
     else if(LC + LN + LT >= CSIZE )
       { for(l=LC;l<=CSIZE-1;l++) card[l] = ' ';
         card[CSIZE]='\n'; card[CSIZE+1]='\0';
         strcat(parse,card);
         card[0] =' '; card[1] ='\0'; 
         strcat(card,names[i]);
         strcat(card,tmp);}
     else
       { strcat(card,names[i]);
         strcat(card,tmp);}

     if(i == num-1 )
       { LC= strlen(card);
         for(l=LC;l<=CSIZE-1;l++) card[l] = ' ';
         card[CSIZE]='\n'; card[CSIZE+1]='\0';
         strcat(parse,card); }
   }

  return 0;
  error:
  printf("cncode:%s\n",msg);
  return msg[1]-'0';
}
