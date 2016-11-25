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
#include "c2f_interface.h"

#include <stdlib.h>
#include <string.h>

#ifdef NEED_CAPITALS
#define cps_getenv_ CPS_GETENV
#define cps_system_ CPS_SYSTEM
#define cps_malloc_ CPS_MALLOC
#define cps_free_   CPS_FREE
#endif
#if(VMS || _AIX || __hpux)
#define cps_getenv_ cps_getenv
#define cps_system_ cps_system
#define cps_malloc_ cps_malloc
#define cps_free_   cps_free
#endif

int cps_getenv_(char *name, char *value);
long cps_malloc_(int *n);
void cps_system_(char *cmd);
void cps_free(int **pointer);

/*
C\USER DOC
C-----------------------------------------------------------------------
C                       CONOCO PROCESSING SYSTEM
C                     EXPLORATION RESEARCH DIVISION
C                              CONOCO, INC.
C
C                       C P S   P R I M I T I V E
C
C  Primitive name:  CPS_MALLOC, CPS_FREE, CPS_GETENV, CPS_SYSTEM
C         Library:  NEWLIB
C          Author:  Richard Day
C    Date Written:  97/12/16
C    Last revised:  98/12/01   Day
C
C  Purpose:         Wrappers around c system routines for fortran
C                   access.
C
C-----------------------------------------------------------------------
C   FUNCTION PROTOTYPES
C
C   long cps_malloc_(int *n);
C   void cps_free_(int **pointer);
C   int cps_getenv_(char *name, char *value);
C   void cps_system_(char *cmd);
C_______________________________________________________________________
C Revision History:
C 2. Day    98/12/01    Fixed initialization problem in cps_getenv
C 1. Day    97/12/16    Put into newlib
C-----------------------------------------------------------------------
C\END DOC

*/

long cps_malloc_(int *n) {
 long *np;
 int num = *n;
 np = (long *) malloc(num);
 return (long) np;
}
void cps_free_(int **pointer) {
 if(*pointer) free(*pointer);
}

/*
 * A wrapper for fortran around the C getenv function of stdlib
 */
int cps_getenv_(char *name, char *value) {
 char *tmp=0;
 if(!name) return 0;
 value[0]='\0';
 tmp = getenv(name);
 if(tmp) { strcpy(value,tmp);
  return 1;
 }
 return 0;
}

/*
 * A wrapper for fortran around the C system function of stdlib
 */
void cps_system_(char *cmd) {
 int i;
 if(!cmd) return;
 i=system(cmd);
}


