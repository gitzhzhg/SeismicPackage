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

//---------------------- acc_sum.cc -----------------------//
//---------------------- acc_sum.cc -----------------------//
//---------------------- acc_sum.cc -----------------------//

//           implementation file for the AccSum class
//                derived from the AccBase class
//                     subdirectory oprim


#include "oprim/acc_sum.hh"
#include "cprim.h"
#include <iostream.h>
#include <stdlib.h>
#include <string.h>
#include <assert.h>


  // This class resets values in array elements in a class
  // derived from SmartArray.
  // The values to be set are set by calling _update_accum_sum.
  // The values to be set correspond to _ident.
  // These values are set to the accumulating sum of values obtained
  //   by calling _get_accum_sum.

  // This class calls the following functions in SmartArray:
  //
  //    void valuesWillChange  (int ident, long index, long nrem, long nins)
  //    void valuesHaveChanged (int ident, long index, long nrem, long nins)


//---------------------- constructor -----------------------//
//---------------------- constructor -----------------------//
//---------------------- constructor -----------------------//


AccSum::AccSum(SmartArray *array, int ident,
               GetAccumSum *get_accum_sum, UpdateAccumSum *update_accum_sum)
           : AccBase(array, ident),
             _get_accum_sum      (get_accum_sum),
             _update_accum_sum   (update_accum_sum)
{
  assert(_get_accum_sum);
  assert(_update_accum_sum);
}



//------------------------ destructor --------------------------//
//------------------------ destructor --------------------------//
//------------------------ destructor --------------------------//

AccSum::~AccSum()
{
}



//----------------------- get range ---------------------------//
//----------------------- get range ---------------------------//
//----------------------- get range ---------------------------//

                         // private

void AccSum::getRange(long n, long* /*index1*/, long *index2)
{
  *index2 = n - 1;
}



//-------------------- fix range ---------------------------------//
//-------------------- fix range ---------------------------------//
//-------------------- fix range ---------------------------------//

                     // private

void AccSum::fixRange(long n, long index1, long index2)
{
  if(n == 0) return;
  long sum = 0;
  if(index1 > 0) sum = _get_accum_sum(_array, index1 - 1);
  for(long index = index1; index <= index2; index++)
      {
      sum = _update_accum_sum(_array, index, sum);
      }
}



//-------------------------- end -------------------------------//
//-------------------------- end -------------------------------//
//-------------------------- end -------------------------------//

