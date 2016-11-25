/****
!<CPS_v1 type="PRIMITIVE"/>
****/
/*------------------------- binary_search.c --------------------------------*/
/*------------------------- binary_search.c --------------------------------*/
/*------------------------- binary_search.c --------------------------------*/

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


!<brief_doc>
!-------------------------------------------------------------------------------
!                        C P S   P R I M I T I V E               
!
! Name       : binary_search
! Category   : math
! Written    : 1994-12-28   by: Tom Stoeckley
! Revised    : 2002-02-11   by: Tom Stoeckley
! Maturity   : production   2002-02-25
! Purpose    : perform a binary search through ordered data
! Portability: No known limitations.
!
!-------------------------------------------------------------------------------
!</brief_doc>


!<descript_doc>
!-------------------------------------------------------------------------------
!                         GENERAL DESCRIPTION                   
!
!    Returns indices ia, ib and a flag, as follows:
!
!   if desired value  actual value at   ia     ib    return flag
!   ----------------  ---------------  -----  -----  ------------
!     comes before      index  0         0      0    BIN_SEARCH_EXTRAP_DOWN
!     comes after       index n-1       n-1    n-1   BIN_SEARCH_EXTRAP_UP
!     matches any       index  i         i      i    BIN_SEARCH_EXACT_MATCH
!     comes between     index  i
!               and     index i+1        i     i+1   BIN_SEARCH_INTERPOLATE
!           **** if n == 0 ****          0      0    BIN_SEARCH_NO_VALUES
!
!-------------------------------------------------------------------------------
!</descript_doc>


!<calling_doc>
!-------------------------------------------------------------------------------
!                      INPUT AND OUTPUT ARGUMENTS               
!
! For each routine or function documented below, each argument is
! flagged as follows:
!      i = value required upon INPUT.
!      o = value set by the routine upon OUTPUT.
!      b = value BOTH required upon input and changed upon output.
!
!  For pointers, the flag (i,o,b) refers to the contents pointed to
!  by the pointer, not to the value of the pointer itself.  The pointer
!  value is required upon INPUT in all cases.
!
!  If all of the arguments are INPUT, the flags may be omitted.
!
!-------------------------------------------------------------------------------
!                          CALLING SEQUENCE                      
!  To read one card (one line) from a text file:
!
!       o                              i    i    i   o    o
!     FLAG =  binary_search_generic  (fun, data, n, &ia, &ib)
!
!  BinarySearchFun *fun  ==>  Function indicating index position
!                             relative to desired value
!  void            *data ==>  Data being searched
!  long            n     ==>  Number of points in *data
!  long            *ia   ==>  Index of value preceding fun
!  long            *ib   ==>  Index of value following fun
!  int             FLAG  ==>  A return status indicating next step
!-----------------------------------------------------------------------
!  Utility functions within binary_search.c prototyped in binary_search.h
!-------------------------------------------------------------------------------
!</calling_doc>


!<advice_doc>
!-------------------------------------------------------------------------------
!                            ADVICE FOR USERS                    
!
!   The data searched through must be in ascending or descending
!   order for the binary search to work properly.
!
!   If there are any adjacent equal values in the ordered data,
!   any index corresponding to the equal values might be returned.
!
!   The function fun(data,i) must return an integer as follows:
!      -1 if desired value comes before actual value for index i
!       0 if desired value matches      actual value for index i
!       1 if desired value comes after  actual value for index i
!
!   The function fun(data,i) will never be called unless n >= 1.
!
!-------------------------------------------------------------------------------
!</advice_doc>


!<history_doc>
!-------------------------------------------------------------------------------
!                           REVISION HISTORY                     
!
!     Date        Author       Description
!     ----        ------       -----------
!  4. 2002-02-25  Stoeckley    Add static function get_first_matching_index
!                               so that the first matching index is always
!                               returned whenever there are two or more
!                               (consecutive) matching indices; this ability
!                               was already present in the ~spws version.
!  3. 2000-10-20  Stoeckley    Changed hist_doc tag to history_doc.
!  2. 1999-09-10  O'Brien      Added documentation during CPS conversion.
!  1. 1994-12-28  Stoeckley    Initial version.
!
!-------------------------------------------------------------------------------
!</history_doc>


!<portability_doc>
!-------------------------------------------------------------------------------
!                         PORTABILITY LIMITATIONS
!
! No known limitations.
!
!-------------------------------------------------------------------------------
!</portability_doc>


!<compile_doc>
!-------------------------------------------------------------------------------
!                      SPECIAL COMPILING REQUIREMENTS             
!
!    Header files needed are:
!       binary_search.h
!
!-------------------------------------------------------------------------------
!</compile_doc>
****/


/*-------------------------- start of module ------------------------------*/
/*-------------------------- start of module ------------------------------*/
/*-------------------------- start of module ------------------------------*/


char BINARY_SEARCH_IDENT[100] =
"$Id: binary_search.c,v 1.4 2002/02/21 21:11:48 Stoeckley prod sps $";


#include <stdlib.h>
#include <assert.h>
#include "binary_search.h"


/*---------------- get first matching index -------------------*/
/*---------------- get first matching index -------------------*/
/*---------------- get first matching index -------------------*/

/****
    This function is called if fun(data, index) is an exact match.
    This function lowers the index to the first exact match,
    in case there are more than one consecutive exact matches.
****/

static long get_first_matching_index(BinarySearchFun *fun, void *data,
                                              long index)
{
  while(index > 0 && fun(data, index - 1) == 0) { index--; }
  return index;
}



/*----------------- binary search generic ----------------------*/
/*----------------- binary search generic ----------------------*/
/*----------------- binary search generic ----------------------*/


int binary_search_generic(BinarySearchFun *fun, void *data,
                                  long n, long *ia, long *ib)
{
  int flag;
  assert(fun && n >= 0 && ia && ib);
  assert(data || n == 0);
  *ia = 0;
  *ib = n-1;
  if(n <= 0)            { *ib =   0; return BIN_SEARCH_NO_VALUES  ; }
  flag = fun(data, 0);
  if(flag <  0)         { *ib =   0; return BIN_SEARCH_EXTRAP_DOWN; }
  if(flag == 0)         { *ib =   0; return BIN_SEARCH_EXACT_MATCH; }
  flag = fun(data, n-1);
  if(flag >  0)         { *ia = n-1; return BIN_SEARCH_EXTRAP_UP  ; }
  if(flag == 0)         { *ia = get_first_matching_index(fun, data, n-1);
                                     return BIN_SEARCH_EXACT_MATCH; }
  while(*ib > *ia + 1)
      {
      long i = (*ia + *ib) / 2;
      flag = fun(data, i);
      if(flag <= 0) *ib = i;
      if(flag >= 0) *ia = i;
      if(*ia == *ib)
          {
          *ia = get_first_matching_index(fun, data, *ia);
          *ib = *ia;
          return BIN_SEARCH_EXACT_MATCH;
          }
      }
  assert(*ib - *ia == 1);
  return BIN_SEARCH_INTERPOLATE;
}



/*-------------------- binary search floats ------------------*/
/*-------------------- binary search floats ------------------*/
/*-------------------- binary search floats ------------------*/

     /* two values are considered equal if their difference
          is less than the specified tolerance */

     /* the array can be monotonically increasing or decreasing */
     /* if the array has only one value, it is considered increasing */

typedef struct _BinarySearchFloatsData
{
  float *array;
  int    direction;
  float  want1;
  float  want2;
} BinarySearchFloatsData;


static int binary_search_floats_fun(void *data, long i)
{
  BinarySearchFloatsData *data2 = (BinarySearchFloatsData*)data;
  if(data2->direction == 0) return 0;
  if(data2->want2 < data2->array[i]) return -data2->direction;
  if(data2->want1 > data2->array[i]) return  data2->direction;
  return 0;
}


int binary_search_floats(float *array, float want, float tolerance, long n,
                                         long *ia, long *ib)
{
  BinarySearchFloatsData data;
  data.array = array;
  if     (n == 0 || !array)      data.direction =  0;
  else if(n == 1)                data.direction =  1;
  else if(array[n-1] < array[0]) data.direction = -1;
  else                           data.direction =  1;
  data.want1 = want - tolerance;
  data.want2 = want + tolerance;
  return binary_search_generic(binary_search_floats_fun, &data, n, ia, ib);
}


/*------------------------- end ---------------------------------------*/
/*------------------------- end ---------------------------------------*/
/*------------------------- end ---------------------------------------*/

