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
/*--------------------------- ftimer_crou.c -------------------------------*/
/*--------------------------- ftimer_crou.c -------------------------------*/
/*--------------------------- ftimer_crou.c -------------------------------*/

#include <sys/time.h>
#include <stdio.h>
#include "c2f_interface.h"

/*------------------ fortran spelling adjustments ---------------------*/
/*------------------ fortran spelling adjustments ---------------------*/
/*------------------ fortran spelling adjustments ---------------------*/

#if NEED_UNDERSCORE
#define ftimer_crou_wtime      ftimer_crou_wtime_
#define ftimer_crou_print      ftimer_crou_print_
#elif NEED_CAPITALS
#define ftimer_crou_wtime      FTIMER_CROU_WTIME
#define ftimer_crou_print      FTIMER_CROU_PRINT
#endif

/*------------------------ start of functions ----------------------------*/
/*------------------------ start of functions ----------------------------*/
/*------------------------ start of functions ----------------------------*/

#ifdef __cplusplus
extern "C" {
#endif

/*------------------------------ wtime -----------------------------------*/
/*------------------------------ wtime -----------------------------------*/
/*------------------------------ wtime -----------------------------------*/

void ftimer_crou_wtime (DOUBLE *time)
{
  static struct timeval tod;

  gettimeofday(&tod,(struct timezone*)0);
  *time = 1.0 * tod.tv_sec + tod.tv_usec * 0.000001;
}

/*---------------------------- print --------------------------------*/
/*---------------------------- print --------------------------------*/
/*---------------------------- print --------------------------------*/

void ftimer_crou_print()
{
  int opt = 0;
#if FOPT0
  opt = 0;
#elif FOPT1
  opt = 1;
#elif FOPT2
  opt = 2;
#elif FOPT3
  opt = 3;
#endif

#if LINUXA
  printf("compiled with Absoft Fortran compiler  -O%d.\n", opt);
#elif LINUXI
  #if PARALLEL
    printf("compiled with Intel Fortran compiler (parallelizing)  -O%d.\n", opt);
  #else
    printf("compiled with Intel Fortran compiler (not parallelizing)  -O%d.\n", opt);
  #endif
#endif
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
