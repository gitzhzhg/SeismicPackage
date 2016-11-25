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
#include <string.h>
#include <stdio.h>

void main()
{int i;
 char hdr[32001];
 char card[120];
 char timex[16],no[4];

 strcpy(timex,"Oct 16 96");
  for(i=0;i<40;i++)
   {/* sprintf(card,"C%-78d\n",i+1); */
    sprintf(card,"C%2d%76s\n",i+1," ");
    if(i==0) strncpy(card+7,"CONOCO SEGY FILE",16);
    if(i==1) strncpy(card+7,timex,strlen(timex)-1);
    if(i==2) strncpy(card+7,"IBM 32 BIT FLOAT",16);
    if(i==39) strncpy(card+4,"END EBCDIC",10);
    strcat(hdr,card);
   }
 hdr[3200]='\0';
 printf("hdr:\n%s\n",hdr);


}
