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

#if (!VMS)
#include <unistd.h>
#else
#include <stdio.h>
#include <stdlib.h>
#include <stddef.h>
#define F_OK  0
#endif

#ifdef CRAY
#define exist_fw_ EXIST_FW
#endif
#if(VMS || _AIX || __hpux)
#define exist_fw_ exist_fw
#endif
int exist_( char *path );
int exist_fw_(char *path);

/*------------------------------------------------------------------
C\USER DOC
C Name   : exist_ 
C Purpose: Checks to see if a file exists or not.
C Author : R. Day
C Date   : 91/04/10
C Revised: 97/12/02
C 
C Function Definition:        ( Language = C )
C  long exist_( char *path )
C  int exist_fw_(char *path)  (callable from fortran)
C  *path     input      Name of the file to test for existence
C 
C NOTES: 
C  1. exist  returns  -1 if file does not exit or 0 if file does exist 
C 
C                         REVISION HISTORY
C
C  DATE      WHO         DESCRIPTION
C  --------  --------    --------------------------------------------
C  97/12/02  Day         Added wrapper for fortran & simplified
C  96/07/17  Vunderink   Inserted into the conlib library$
C\END DOC
 *------------------------------------------------------------------*/
int exist_( char *path )
{ long   mode;
  mode = F_OK;
  return access(path,mode);
}

int exist_fw_(char *path)
{ return exist_(path); }

