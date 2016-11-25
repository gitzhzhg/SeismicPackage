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

//---------------------------- limits_keeper.hh --------------------------//
//---------------------------- limits_keeper.hh --------------------------//
//---------------------------- limits_keeper.hh --------------------------//

//               header file for the LimitsKeeper class
//                     not derived from any class
//                          subdirectory oprim

   // This class maintains the minimum and maximum limits of values
   // residing in an externally-owned array.  It also maintains the
   // number of nil values, non-nil values, total number of values,
   // and the sum and average of all non-nil values.

   // Call insertValue each time a value is inserted into the array.
   // If the array is not initially empty, call insertValue for each
   // value currently in the array.  Or for convenience, call
   // getLimits(n), where n is the current number of values in the array.

   // Call removeValue each time a value is removed from the array.
   // For efficiency, call clearValues if you are removing all values
   // at the same time.

   // When changing a value in the array, call removeValue with the
   // old value, then call insertValue with the new value.

   // Nil values are not used in determining limits.

   // This class may occasionally need to access the externally-owned
   // array when minimumValue or maximumValue is called.  When this
   // is the case, this class will use _get_value and _data.


//------------------- start of coding ---------------------------//
//------------------- start of coding ---------------------------//
//------------------- start of coding ---------------------------//


#ifndef _LIMITS_KEEPER_HH_
#define _LIMITS_KEEPER_HH_


class LimitsKeeper
{

//--------------------------- data ------------------------------//
//--------------------------- data ------------------------------//
//--------------------------- data ------------------------------//

public:

  typedef float GetValue (void *data, long index);

private:

  GetValue *_get_value;    // function which returns a value for any index.
  void     *_data;         // user data to pass to function.
  long      _n;            // number of values.
  long      _nlive;        // number of live (non-nil) values.
  float     _xmin;         // minimum value (nil if no non-nil values).
  float     _xmax;         // maximum value (nil if no non-nil values).
  long      _nxmin;        // number of values matching _xmin.
  long      _nxmax;        // number of values matching _xmax.
  float     _sum;          // sum of all (non-nil) values.
 

//-------------------------- functions ---------------------------------//
//-------------------------- functions ---------------------------------//
//-------------------------- functions ---------------------------------//

public:

           LimitsKeeper (GetValue *get_value, void *data);
  virtual ~LimitsKeeper ();

  void     insertValue  (float value);
  void     removeValue  (float value);
  void     clearValues  ();
  void     getLimits    (long n);

  float    minimumValue ();   // returns minimum value in the array.
  float    maximumValue ();   // returns maximum value in the array.
              // the above return 0.0 if there are no non-nil values.

  long     numValues        ()  const  { return _n; }
  long     numLiveValues    ()  const  { return _nlive; }
  long     numNilValues     ()  const  { return _n - _nlive; }

  float    sumValues        ()  const  { return _sum; }
  float    averageValue     ()  const;

//---------------------- end of functions -----------------------//
//---------------------- end of functions -----------------------//
//---------------------- end of functions -----------------------//

} ;

#endif

//---------------------------- end --------------------------------//
//---------------------------- end --------------------------------//
//---------------------------- end --------------------------------//
