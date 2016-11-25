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


/*------------------------- wrapper.h -----------------------------------*/

/*  This file contains function prototypes and spelling adjustments
    for subroutines in the source code file wrapper.f, which can be
    called from C.     */


#ifndef _WRAPPER_H
#define _WRAPPER_H

#include "c2f_interface.h"

/*-------- fortran subroutine spelling adjustments ----------------------*/

#if (VMS || _AIX || __hpux)
#define wrapper_                   wrapper
#define wrappers_                  wrappers
#endif

#if (ultrix || sun)
#define wrapper                    wrapper_
#define wrappers                   wrappers_
#endif

#ifdef NEED_CAPITALS
#define wrapper_                   WRAPPER
#define wrappers_                  WRAPPERS
#endif

#ifdef NEED_CAPITALS
#define wrapper                    WRAPPER
#define wrappers                   WRAPPERS
#endif


/*-------------------- function prototypes -----------------------------*/

void   wrapper_  (void(*)(), long*, long*, long*);
void   wrappers_ (void(*)(), long*, long*, float*, float*, long*, long*);
 

#endif

/*--------------------------- end --------------------------------------*/

