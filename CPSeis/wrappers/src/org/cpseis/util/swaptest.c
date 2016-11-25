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
/*--------------------------- swaptest.c -------------------------------*/
/*--------------------------- swaptest.c -------------------------------*/
/*--------------------------- swaptest.c -------------------------------*/

#include "swap.h"
#include <stdio.h>

/*---------------------------- main --------------------------------------*/
/*---------------------------- main --------------------------------------*/
/*---------------------------- main --------------------------------------*/

int main (int argc, char **argv)
{
  char    c   = 'A';
  short   s   = 12;
  int     i   = 123456;
  int32_t i32 = 123456;
  int64_t i64 = 123456;
  long    l   = 123456;
  float   f   = 123.456;
  double  d   = 123.456;

  char    cswapped    = c;
  short   sswapped    = s;
  int     iswapped    = i;
  int32_t i32swapped  = i32;
  int64_t i64swapped  = i64;
  long    lswapped    = l;
  long    lswapped2   = l;
  float   fswapped    = f;
  double  dswapped    = d;
  double  dswapped2   = d;

  int sizec  = sizeof(cswapped);
  int size32 = sizeof(i32swapped);
  int size64 = sizeof(i64swapped);
  int sizel  = sizeof(lswapped2);
  int sized  = sizeof(dswapped2);

  swap_unk     (&cswapped, &sizec);
  swap_short_2 (&sswapped);
  swap_int_4   (&iswapped);
  swap_unk     (&i32swapped, &size32);
  swap_unk     (&i64swapped, &size64);
  swap_long_8  (&lswapped);
  swap_unk     (&lswapped2, &sizel);
  swap_float_4 (&fswapped);
  swap_double_8(&dswapped);
  swap_unk     (&dswapped2, &sized);

  printf("swap_unk       char     = %c   swapped = %c\n" , c, cswapped);
  printf("swap_short_2   short    = %d   swapped = %d\n" , (int)s, (int)sswapped);
  printf("swap_int_4     int      = %d   swapped = %d\n" , i, iswapped);
  printf("swap_unk       int32_t  = %d   swapped = %d\n" , i32, i32swapped);
  printf("swap_unk       int64_t  = %ld  swapped = %ld\n", i64, i64swapped);
  printf("swap_long_8    long     = %ld  swapped = %ld\n", l, lswapped);
  printf("swap_unk       long     = %ld  swapped = %ld\n", l, lswapped2);
  printf("swap_float_4   float    = %g   swapped = %g\n" , f, fswapped);
  printf("swap_double_8  double   = %lg  swapped = %lg\n", d, dswapped);
  printf("swap_unk       double   = %lg  swapped = %lg\n", d, dswapped2);
  return 0;
}

/*-------------------------------- end ------------------------------------*/
/*-------------------------------- end ------------------------------------*/
/*-------------------------------- end ------------------------------------*/
