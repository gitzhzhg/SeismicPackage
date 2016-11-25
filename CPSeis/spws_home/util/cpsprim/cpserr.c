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
#include <string.h>
#include "cmpi.h"

#include "c2f_interface.h"

#ifdef NEED_CAPITALS
#define cpserr_ CPSERR
#endif
#if (VMS || _AIX || __hpux)
#define cpserr_ cpserr
#endif

/*
C\USER DOC
C-----------------------------------------------------------------------
C                       CONOCO PROCESSING SYSTEM
C                     EXPLORATION RESEARCH DIVISION
C                              CONOCO, INC.
C
C                       C P S   P R I M I T I V E
C
C  Primitive name:  CPSERR
C         Library:  NEWLIB
C          Author:  Richard Day
C    Date Written:  98/01/18
C    Last revised:  98/01/18   Day
C
C  Purpose:         Send a message to standard error. This routine is 
C                   aliased for the CRAY fortran compilers and provides
C                   a portable alternative to remark.
C
C-----------------------------------------------------------------------
C   FUNCTION PROTOTYPES
C
C   void cpserr_(char *proc_name)
C    proc_name ... A null terminated C-type character string
C_______________________________________________________________________
C Revision History:
C 2. Day    99/04/19    Name aliasing for fortran compiler on HPUX
C 1. Day    98/01/18    Put into newlib
C-----------------------------------------------------------------------
C\END DOC

*/
void cpserr_(char *proc_name) {
 if(!proc_name) return;
 if(cmpi_i_pel_() == 0) {
  fprintf(stderr,"UT009 - CPS PROCESS- %s CALLED-----\n",proc_name);
 }
}
