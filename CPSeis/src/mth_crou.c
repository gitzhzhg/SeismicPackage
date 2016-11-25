/*<CPS_v1 type="PRIMITIVE">
!----------------------------- mth_crou.c -------------------------------
!----------------------------- mth_crou.c -------------------------------
!----------------------------- mth_crou.c -------------------------------

!<license>
!-------------------------------------------------------------------------------
! Copyright (c) 2007 ConocoPhillips Company
!
! Permission is hereby granted, free of charge, to any person obtaining a copy
! of this software and associated documentation files (the "Software"), to deal
! in the Software without restriction, including without limitation the rights
! to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
! copies of the Software, and to permit persons to whom the Software is
! furnished to do so, subject to the following conditions:
!
! The above copyright notice and this permission notice shall be included in all
! copies or substantial portions of the Software.
!
! THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
! IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
! FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
! AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
! LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
! OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
! SOFTWARE.
!-------------------------------------------------------------------------------
!</license>

!<brief_doc>
!-------------------------------------------------------------------------------
! Name       : MTH_CROU
! Category   : math
! Written    : 2002-07-18   by: Charles C. Burch
! Revised    : 2007-03-27   by: Kruger Corn
! Maturity   : beta
! Purpose    : provides access to math functions written in C
! References : These routines are for general use by c or Fortran routines
!-------------------------------------------------------------------------------
!</brief_doc>

!<descript_doc>
!-------------------------------------------------------------------------------
! These routines provide access certain math-related functions
!
! INTEGER mth_random_number_c(INTEGER *SEED, INTEGER *MAX) 
!  Purpose: produces random numbers based on variation of
!           Schrage's approximate Park-Miller Minimum Standard generator
!  SEED in input/output-at first call it is the seed value to use to create
!    the random numbers and also used to stored the modified seed to use for the
!    next call. 
!    The starting value of SEED can be an arbitrary valuem but preferably >0
!  MAX is the maximum value you want the resulting numbers to have, 
!    the returned values fall in the interval [0,MAX].
!
!  Note this function allows multiple sequence of random numbers to be created 
!    using different seeds without them interferring with one another.
!-------------------------------------------------------------------------------
!
!  REAL mth_random_numberf_c(INTEGER *SEED)
!  Purpose: produces floating point random numbers based on variation of
!           Schrage's approximate Park-Miller Minimum Standard generator
!           in the range of 0. and 1. inclusively.
!
!  See mth_random_number_c for use of SEED
!-------------------------------------------------------------------------------
!</descript_doc>

!<calling_doc>
!-------------------------------------------------------------------------------
!
!-------------------------------------------------------------------------------
!</calling_doc>

!<history_doc>
!-------------------------------------------------------------------------------
!                     AUXILIARY FILE REVISION HISTORY
!     Date        Author         Description
!     ----        ------         -----------
!  3. 2007-03-27  Corn           Update to 64 bit architecture. Basically
!                                change long to INTEGER.
!  2. 2005-05-31  Stoeckley      Fix to compile with C++.
!  1. 2002-07-29  Chuck C. Burch Initial Version.
!-------------------------------------------------------------------------------
!</history_doc>

!<portability_doc>
!-------------------------------------------------------------------------------
! No known portability problems
!-------------------------------------------------------------------------------
!</portability_doc>
*/

/*
!--------------------------- start of coding ------------------------------
!--------------------------- start of coding ------------------------------
!--------------------------- start of coding ------------------------------
*/

char *mth_crou_ident =
"$Id: mth_crou.c,v 1.3 2007/03/28 15:09:42 Corn beta sps $";

#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>
#include <string.h>

#include "mth.h"

/*--------------------------- start of functions ----------------------------*/
/*--------------------------- start of functions ----------------------------*/
/*--------------------------- start of functions ----------------------------*/

/*The numbers below are for the Park-Miller-Schrage Mimumum Standard ran gen.*/
#define mth_ran_m 2147483647 
#define mth_ran_a 16807 
#define mth_ran_q 127773 
#define mth_ran_r 2836 

#ifdef __cplusplus
extern "C" {
#endif

/*************************************************************
* produces random numbers based on variation of
* Schrage's approximate Park-Miller Minimum Standard generator
*
* Written July 2002 by Charles C Burch
************************************************************/
INTEGER mth_random_number_c(INTEGER *SEED, INTEGER *MAX) {
  INTEGER k, max, seed;

  max=(*MAX);
  if(max<1) return(1);
 
  if((seed=(*SEED))<1) seed=1;
  k=seed/mth_ran_q;
  seed=mth_ran_a*(seed-k*mth_ran_q)-k*mth_ran_r;
  if(seed<0) seed+=mth_ran_m;
  (*SEED)=seed;
  return(1+seed%max);
} 

/*************************************************************
* produces floating point random numbers based on variation of
* Schrage's approximate Park-Miller Minimum Standard generator
* in the range of 0 and 1 inclusively.
*
* Written July 2002 by Charles C Burch
************************************************************/
REAL mth_random_numberf_c(INTEGER *SEED) {
  INTEGER ran;
  
  ran=mth_ran_m;
  ran=mth_random_number_c(SEED, &ran);
  return(((float) (ran-1))/(mth_ran_m-1));
}

#ifdef __cplusplus
}
#endif


/*-------------------------------- end ------------------------------------*/
/*-------------------------------- end ------------------------------------*/
/*-------------------------------- end ------------------------------------*/

/********************** Basic test driver ***********************
#include <stdlib.h>
#include <stdio.h>

#include "mth.h"
#include "c2f_interface.h"

int main() {
  int i;
  INTEGER seed, seed_f, max;
  max=10;
  seed=1;
  seed_f=1;
  for (i=0; i<300; i++) {
    printf("rand(%d)=%d, %f\n",
      i, (int)mth_random_number_c(&seed, &max), mth_random_numberf_c(&seed_f));
  }
  return(0);
}
********************************************************/

/*-------------------------------- end ------------------------------------*/
/*-------------------------------- end ------------------------------------*/
/*-------------------------------- end ------------------------------------*/
