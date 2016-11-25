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

//------------------------ acc_interp.hh ---------------------//
//------------------------ acc_interp.hh ---------------------//
//------------------------ acc_interp.hh ---------------------//

//             header file for the AccInterp class
//               derived from the AccBase class
//                    subdirectory oprim

     // This class interpolates values in array elements in
     //   a class derived from SmartArray.
     // One value (identified with an ident) can be 
     //   interpolated by this class.
     // The interpolation method is linear interpolation
     //   with sloping or flat extrapolation.
     // See the implementation file for documentation.


#ifndef _ACC_INTERP_HH_
#define _ACC_INTERP_HH_

#include "oprim/acc_base.hh"

class AccInterp : public AccBase
{

//----------------------- data ------------------------------//
//----------------------- data ------------------------------//
//----------------------- data ------------------------------//

private:

  typedef double GetValue (void *data, long index);
  typedef void   SetValue (void *data, long index, double value);

  GetValue * const _get_value;
  SetValue * const _set_value;
  const int        _sloping;   // whether extrap is sloping (TRUE) or flat.
  const double     _tolerance; // tolerance for setting values dependent.

//--------------------- functions ---------------------------//
//--------------------- functions ---------------------------//
//--------------------- functions ---------------------------//

public:   // constructor and destructor

  AccInterp (SmartArray *array, int ident,
                               GetValue *get_value, SetValue *set_value,
                               int sloping, double tolerance = 0.001);
  virtual ~AccInterp();

public:

  virtual void adjustDependencyFlags ();

private:

  virtual void getRange (long n, long *index1, long *index2);
  virtual void fixRange (long n, long  index1, long  index2);

  void findNonTerpRange(long *first, long *last);
  long findFirstNonTerpValue()  const;
  long findLastNonTerpValue()  const;
  void replaceByValue(long first, long last, double value);
  void doGrade(long first, long last,
                    double value1, double value2, long index1, long index2);
  void doInterpolations(long first, long last);

//---------------------- end of functions -----------------------//
//---------------------- end of functions -----------------------//
//---------------------- end of functions -----------------------//

} ;

#endif

//---------------------------- end --------------------------------//
//---------------------------- end --------------------------------//
//---------------------------- end --------------------------------//
