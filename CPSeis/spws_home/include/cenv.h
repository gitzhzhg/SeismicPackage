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

#ifndef cenvdef

/*#include stddef */

#define cenvdef

#define NOT !
#define AND &&
#define OR  ||

#define True 1
#define False 0


#define Odd(x) (((x) & 1) == 1)

#define notnull(x)    ( (x)!=NULL ? (x) : False)
#define Tst_null(x,y)  ( (x)==NULL ? (void *)(x) :  (void *)(y) )
#define tstnull(x,y)  ( (x)==NULL ?  (void *)(x) :  (void *)(y) )

#define END }
#define ENDif }
#define ENDelse }
#define ENDloop }
#define ENDswitch }

typedef unsigned short Uword;
typedef char           byte;
typedef unsigned char  Ubyte;
typedef unsigned char  boolean;
typedef struct { unsigned int L0;
                 int          L1; }  quadword;

#endif


