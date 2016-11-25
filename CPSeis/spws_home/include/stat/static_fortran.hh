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

//--------------------- static_fortran.hh --------------------------//
//--------------------- static_fortran.hh --------------------------//
//--------------------- static_fortran.hh --------------------------//

//         header file containing spelling adjustments and
//         prototypes for the static_fortran.f fortran file.
//                       subdirectory stat


#ifndef _STATIC_FORTRAN_HH_
#define _STATIC_FORTRAN_HH_

#include "c2f_interface.h"


extern "C" {

//--------------------- fortran spelling adjustments -----------------//
//--------------------- fortran spelling adjustments -----------------//
//--------------------- fortran spelling adjustments -----------------//


#if NEED_UNDERSCORE
#define static_option_r        static_option_r_
#define static_option_i        static_option_i_
#define static_option_n        static_option_n_
#define static_option_nx       static_option_nx_
#define static_option_ny       static_option_ny_
#define static_option_nn       static_option_nn_
#define static_option_gg       static_option_gg_
#define static_reverse         static_reverse_
#define static_option_t        static_option_t_
#define static_option_j        static_option_j_
#endif

#if NEED_CAPITALS
#define static_option_r        STATIC_OPTION_R
#define static_option_i        STATIC_OPTION_I
#define static_option_n        STATIC_OPTION_N
#define static_option_nx       STATIC_OPTION_NX
#define static_option_ny       STATIC_OPTION_NY
#define static_option_nn       STATIC_OPTION_NN
#define static_option_gg       STATIC_OPTION_GG
#define static_reverse         STATIC_REVERSE
#define static_option_t        STATIC_OPTION_T
#define static_option_j        STATIC_OPTION_J
#endif


//------------------------ fortran prototypes -----------------------//
//------------------------ fortran prototypes -----------------------//
//------------------------ fortran prototypes -----------------------//


void static_option_r  (const int *ioption,
                       float *array, const int *nx, const int *ny,
                       const float *xinc, const float *yinc,
                       const float *tp,
                       const float *xrun, const float *yrun,
                       const int *endflag,
                       float *scratch);

void static_option_i  (float *array, const int *nx, const int *ny);

void static_option_n  (float *array, const int *nx, const int *ny);

void static_option_nx (float *array, const int *nx, const int *ny);

void static_option_ny (float *array, const int *nx, const int *ny);

void static_option_nn (float *array, const int *nx, const int *ny,
                       const int *ixdist, const int *iydist,
                       const int *require,
                       float *scratch);

void static_option_gg (float *array, const int *nx, const int *ny,
                       const int *ixa, const int *ixb,
                       const int *iya, const int *iyb);

void static_reverse   (float *x1, float *y1, float *xinc, float *yinc,
                       const int *nx, const int *ny, float *array);

void static_option_t  (int *ichoice, int *inil,
                       const float *array,
                       const float *x1  , const float *y1,
                       const float *xinc, const float *yinc,
                       const int   *nx  , const int   *ny,
                       float *array2,
                       const float *xx1  , const float *yy1,
                       const float *xxinc, const float *yyinc,
                       const int  *nxx   , const int   *nyy);

void static_option_j  (float *array, int *nx, int *ny);
          /////// join two datasets not finished...



//---------------------- end of prototypes --------------------------//
//---------------------- end of prototypes --------------------------//
//---------------------- end of prototypes --------------------------//

}

#endif


//------------------------------ end -------------------------------//
//------------------------------ end -------------------------------//
//------------------------------ end -------------------------------//

