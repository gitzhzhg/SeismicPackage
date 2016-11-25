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

//------------------------ acc_sum.hh ---------------------//
//------------------------ acc_sum.hh ---------------------//
//------------------------ acc_sum.hh ---------------------//

//              header file for the AccSum class
//               derived from the AccBase class
//                    subdirectory oprim

     // This class sets the values for _ident (by calling _update_accum_sum)
     // to the accumulating sum of values obtained by calling _get_accum_sum.


#ifndef _ACC_SUM_HH_
#define _ACC_SUM_HH_

#include "oprim/acc_base.hh"

class AccSum : public AccBase
{

//----------------------- data ------------------------------//
//----------------------- data ------------------------------//
//----------------------- data ------------------------------//

private:

  typedef long GetAccumSum    (void *data, long index);
  typedef long UpdateAccumSum (void *data, long index, long prev_accum_sum);

  GetAccumSum    * const _get_accum_sum;
  UpdateAccumSum * const _update_accum_sum;

//--------------------- functions ---------------------------//
//--------------------- functions ---------------------------//
//--------------------- functions ---------------------------//

public:

  AccSum (SmartArray *array, int ident,
          GetAccumSum *get_accum_sum, UpdateAccumSum *update_accum_sum);
  virtual ~AccSum();

private:

  virtual void  getRange (long n, long *index1, long *index2);
  virtual void  fixRange (long n, long  index1, long  index2);

//---------------------- end of functions -----------------------//
//---------------------- end of functions -----------------------//
//---------------------- end of functions -----------------------//

} ;

#endif

//---------------------------- end --------------------------------//
//---------------------------- end --------------------------------//
//---------------------------- end --------------------------------//
