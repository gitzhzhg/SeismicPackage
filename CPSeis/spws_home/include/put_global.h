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
#ifndef Put_Glob_
#define Put_Glob_

#include "c2f_interface.h"

#include "tfio.h"

#ifdef NEED_CAPITALS
#define put_global_to_file_   PUT_GLOBAL_TO_FILE
#define put_global_to_filet_  PUT_GLOBAL_TO_FILET
#define put_global_bld_dfile_ PUT_GLOBAL_BLD_DFILE
#endif

#if(VMS || _AIX || __hpux)
#define put_global_to_file_   put_global_to_file
#define put_global_to_filet_  put_global_to_filet
#define put_global_bld_dfile_ put_global_bld_dfile
#endif


#ifdef __cplusplus
extern "C" {                 // for C++
#endif
 
int  put_global_to_file_(TF_Global *g);
int  put_global_to_filet_(TF_Global *g, char *temp);
void put_global_bld_dfile_(int *ftype, char hfile[], char dfile[]);

#ifdef __cplusplus
}                   // for C++
#endif

#endif

