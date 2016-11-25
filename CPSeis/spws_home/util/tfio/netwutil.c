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
#include <stdlib.h>
#ifdef VMS
#include <stdlib.h>
#include <stddef.h>
#include <file.h>
#endif
#include "wrdcnvrt.h"
#include "netw.h"


/*------------------------------------------------------------------
C\USER DOC
C Name   : netw_lnode
C Purpose: Returns some information about the current node
C Author : R. Day
C Date   : 09/18/96
C Revised: 98/01/05   Day
C
C Function Definition:        ( Language = C )
C  int  netw_lnode( char *hostname, char *os )
C
C  *hostname    out   name of current node
C  *os          out   operating system UNIX,VMS,UNICOS
C
C NOTES:
C 1. Returns word type for current node.
C    See wrdcnvrt.h for the standards
C
C Revisions:
C DATE      WHO         DESCRIPTION
C --------  --------    --------------------------------------------
C 98/01/05  Day         Slight modifications for t3e support.
C 96/10/22  Vunderink   Inserted into the conlib library.
C
C-------------------------------------------------------------------
C Header Files:    netw.h wrdcnvrt.h
C
C\END DOC
 *------------------------------------------------------------------*/

int  netw_lnode(char *hostname, char *os)
{
 int  l;
 int  local_word;
 char *host;

 strcpy(os,"UNIX");
 local_word = WIEEE;
#ifdef VMS
 strcpy(os,"VMS");
 local_word = WVMS;
#endif
#ifdef _CRAY1
 strcpy(os,"UNICOS");
 local_word = WCRAY;
#endif
#ifdef _CRAYMPP
 strcpy(os,"UNICOS");
 local_word = WIEEE;
#endif

 host = getenv("HOST");
 if(host != NULL)
  {strcpy(hostname,host);
   l = 0;
   while(hostname[l]!=0) {hostname[l] = tolower(hostname[l]); l++; }
  }
 else
  strcpy(hostname,"NONE");
 return local_word;
}


int  netw_nodes_wrd(char *rnode)
{/* Provides a data base about our most common nodes */
 int  wdtyp;
 int  i;
 char node[32];

 if(rnode==NULL) return -1;
 strcpy(node,rnode);
 i = 0;
 while(node[i]!=0) {node[i] = tolower(node[i]); i++; }

 wdtyp = -1;
 if(strstr(node,"cray")  !=0) wdtyp=WCRAY;
 if(strstr(node,"sp")!=0) wdtyp=WCRAY;
 if(strstr(node,"sk")!=0) wdtyp=WCRAY;
 if(strstr(node,"ss")!=0) wdtyp=WIEEE;
 if(strstr(node,"pogun") !=0) wdtyp=WVMS;
 if(strstr(node,"poepsg14")!=0) wdtyp=WIEEE;
 if(strstr(node,"poepsn03")!=0) wdtyp=WIEEE;
 if(strstr(node,"pospsv")!=0) wdtyp=WIEEE;
 if(strstr(node,"poepsg01")!=0) wdtyp=WIEEE;
 if(strstr(node,"poepsg11")!=0) wdtyp=WIEEE;

 return wdtyp;
}
