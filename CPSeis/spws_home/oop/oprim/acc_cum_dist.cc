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

//---------------------- acc_cum_dist.cc -----------------------//
//---------------------- acc_cum_dist.cc -----------------------//
//---------------------- acc_cum_dist.cc -----------------------//

//         implementation file for the AccCumDist class
//                derived from the AccBase class
//                     subdirectory oprim


#include "oprim/acc_cum_dist.hh"
#include "cprim.h"
#include <iostream.h>
#include <math.h>
#include <stdlib.h>
#include <string.h>
#include <assert.h>


  // This class resets values in array elements in a class
  // derived from SmartArray, and flags these values as "dependent".
  // The values to be set are specified by _ident.  These values
  // are set by calculation from values specified by _x_ident and
  // _y_ident.


  // This class calls the following functions in SmartArray:
  //
  //    void valuesWillChange  (int ident, long index, long nrem, long nins)
  //    void valuesHaveChanged (int ident, long index, long nrem, long nins)



//---------------------- constructor -----------------------//
//---------------------- constructor -----------------------//
//---------------------- constructor -----------------------//


AccCumDist::AccCumDist(SmartArray *array, int ident,
                       UpdateCumDist *update_cum_dist)
           : AccBase(array, ident),
             _update_cum_dist     (update_cum_dist)
{
  assert(_update_cum_dist);
}



//------------------------ destructor --------------------------//
//------------------------ destructor --------------------------//
//------------------------ destructor --------------------------//

AccCumDist::~AccCumDist()
{
}



//----------------------- get range ---------------------------//
//----------------------- get range ---------------------------//
//----------------------- get range ---------------------------//

                         // private

void AccCumDist::getRange(long n, long *index1, long *index2)
{
  *index1 = 0;
  *index2 = n - 1;
}



//-------------------- fix range ---------------------------------//
//-------------------- fix range ---------------------------------//
//-------------------- fix range ---------------------------------//

                     // private

void AccCumDist::fixRange(long /*n*/, long index1, long index2)
{
  for(long index = index1; index <= index2; index++)
      {
      _update_cum_dist(_array, index);
      }
}



//-------------------------- end -------------------------------//
//-------------------------- end -------------------------------//
//-------------------------- end -------------------------------//

