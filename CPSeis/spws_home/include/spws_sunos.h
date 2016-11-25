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
 * This include file is for running on Suns  - because suns are not ansi
 * standard
 */

                    /* suns are not ansi and have these constants defined */
#define SEEK_SET 0
#define SEEK_CUR 1
#define SEEK_END 2

#define RAND_MAX  0x7FFFFFFF

#ifdef __cplusplus
#include <stdlib.h>
#define labs abs         /* labs (long absolute values is not on suns) */
#else
#define labs abs         /* labs (long absolute values is not on suns) */
#endif

 /*
  * the follow macro simulates a Standard C routine that the sun does not
  * yet have.
  */

#if 0
                            extern char *sys_errlist[];                \
                            char *strptr;                              \
                            strptr= sys_errlist[(err)], strptr; } )
#endif
