/****
!<CPS_v1 type="HEADER_FILE"/>
****/
/*------------------------ binary_search.h -----------------------------------*/
/*------------------------ binary_search.h -----------------------------------*/
/*------------------------ binary_search.h -----------------------------------*/

/****
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


!<history_doc>
!-------------------------------------------------------------------------------
!                       HEADER FILE REVISION HISTORY 
!
!     Date        Author       Description
!     ----        ------       -----------
!  2. 2000-10-20  Stoeckley    Move CPS_v1 tag to proper location.
!  1. 1999-09-10  O'Brien      Initial version.
!
!-------------------------------------------------------------------------------
!</history_doc>
****/


#ifndef _BINARY_SEARCH_H_
#define _BINARY_SEARCH_H_

#ifdef __cplusplus
extern "C" {           /* for C++ */
#endif

/*------------------- typedefs, constants, structures -----------------------*/
/*------------------- typedefs, constants, structures -----------------------*/
/*------------------- typedefs, constants, structures -----------------------*/

typedef int BinarySearchFun (void *data, long i);

#define BIN_SEARCH_EXTRAP_DOWN  1
#define BIN_SEARCH_EXTRAP_UP    2
#define BIN_SEARCH_EXACT_MATCH  3
#define BIN_SEARCH_INTERPOLATE  4
#define BIN_SEARCH_NO_VALUES    5

/*--------------------------- prototypes ------------------------------------*/
/*--------------------------- prototypes ------------------------------------*/
/*--------------------------- prototypes ------------------------------------*/

int  binary_search_generic (BinarySearchFun *fun, void *data,
                                  long n, long *ia, long *ib);
int  binary_search_floats  (float *array, float want, float tolerance,
                                  long n, long *ia, long *ib);

/*----------------------- end of information --------------------------------*/
/*----------------------- end of information --------------------------------*/
/*----------------------- end of information --------------------------------*/

#ifdef __cplusplus
}                      /* for C++ */
#endif

#endif                 /* for _BINARY_SEARCH_H_ */
