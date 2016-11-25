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
/*--------------------------- sizetest_crou.c -------------------------------*/
/*--------------------------- sizetest_crou.c -------------------------------*/
/*--------------------------- sizetest_crou.c -------------------------------*/

#include "c2f_interface.h"
#include <stdio.h>
#include <limits.h>
#include <float.h>
#include <jni.h>

/*------------------ fortran spelling adjustments ---------------------*/
/*------------------ fortran spelling adjustments ---------------------*/
/*------------------ fortran spelling adjustments ---------------------*/

#if NEED_UNDERSCORE
#define sizetest_crou      sizetest_crou_
#elif NEED_CAPITALS
#define sizetest_crou      SIZETEST_CROU
#endif

/*------------------------ start of functions ----------------------------*/
/*------------------------ start of functions ----------------------------*/
/*------------------------ start of functions ----------------------------*/

#ifdef __cplusplus
extern "C" {
#endif

/*---------------------------- sizetest_crou -----------------------------*/
/*---------------------------- sizetest_crou -----------------------------*/
/*---------------------------- sizetest_crou -----------------------------*/

void sizetest_crou (char *iii1, char *iii2, char *fff1, char *fff2, char *ddd1, char *ddd2)
{
  printf("size of Fortran integer     = %d\n", (int)(iii2 - iii1));
  printf("size of Fortran real        = %d\n", (int)(fff2 - fff1));
  printf("size of Fortran double      = %d\n", (int)(ddd2 - ddd1));
  printf("\n");
  printf("size of  C/C++  char        = %d   limits = %d %d\n"  , (int)sizeof(char)       ,   SCHAR_MIN,   SCHAR_MAX);
  printf("size of  C/C++  short       = %d   limits = %d %d\n"  , (int)sizeof(short)      ,    SHRT_MIN,    SHRT_MAX);
  printf("size of  C/C++  int         = %d   limits = %d %d\n"  , (int)sizeof(int)        ,     INT_MIN,     INT_MAX);
  printf("size of  C/C++  long        = %d   limits = %ld %ld\n", (int)sizeof(long)       ,    LONG_MIN,    LONG_MAX);
  printf("size of  C/C++  long long   = %d   limits = %ld %ld\n", (int)sizeof(long long)  ,          0L,          0L);
  printf("size of  C/C++  float       = %d   expons = %d %d\n"  , (int)sizeof(float)      , FLT_MIN_EXP, FLT_MAX_EXP);
  printf("size of  C/C++  double      = %d   expons = %d %d\n"  , (int)sizeof(double)     , DBL_MIN_EXP, DBL_MAX_EXP);
  printf("size of  C/C++  long double = %d   expons = %d %d\n"  , (int)sizeof(long double),           0,           0);
  printf("size of  C/C++  NULL        = %d\n", (int)sizeof NULL );
  printf("size of  C/C++  0           = %d\n", (int)sizeof 0 );
  printf("\n");
  printf("size of  java   jshort      = %d\n", (int)sizeof(jshort));
  printf("size of  java   jint        = %d\n", (int)sizeof(jint));
  printf("size of  java   jlong       = %d\n", (int)sizeof(jlong));
  printf("size of  java   jfloat      = %d\n", (int)sizeof(jfloat));
  printf("size of  java   jdouble     = %d\n", (int)sizeof(jdouble));
  printf("size of  java   jsize       = %d\n", (int)sizeof(jsize));
}

/*------------------------ end of functions ----------------------------*/
/*------------------------ end of functions ----------------------------*/
/*------------------------ end of functions ----------------------------*/

#ifdef __cplusplus
}
#endif

/*-------------------------------- end ------------------------------------*/
/*-------------------------------- end ------------------------------------*/
/*-------------------------------- end ------------------------------------*/
