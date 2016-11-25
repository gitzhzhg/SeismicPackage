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
#ifndef cbprimdef

#define cbprimdef

#include <tfio.h>
#include <tfdefs.h>
#include <tfio.h>

/* define constants to control what fields are created */

/*
#define DOALL   0xFFFFFFF
*/
#define DOALL   ~0
#define DONTOT  (1<<0)
#define DOISKP  (1<<1)
#define DONDO   (1<<2)
#define DONSKP  (1<<3)
#define DOSCAN  (1<<4)
#define DOTMIN  (1<<5)
#define DOTMAX  (1<<6)
#define DOTDEC  (1<<7)
#define DORP    (1<<8)
#define DOCT    (1<<9)
#define DONORM  (1<<10)
#define DOGS    (1<<11)
#define DOWONLY (1<<12)
#define DOWFILL (1<<13)
#define DOIS    (1<<14)
#define DOTI    (1<<15)
#define DOFILE  (1<<16)
#define DOANNO  (1<<17)
#define DOSCALE (1<<18)
#define DORD    (1<<19)
#define DOINVY  (1<<20)
#define DOMAX   (1<<21)
#define DOMOVIE (1<<22)


#endif





