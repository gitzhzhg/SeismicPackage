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
// class that creates a resampler, wrap around will occur if necessary
#ifndef RESAMPLER_HH
#define RESAMPLER_HH

class Resampler {

public:
  enum Type {
    CONSTANT,					// hold value constant
    LINEAR,					// linearly resample
    EXPONENTIAL,				// exponentially resample
    LOGARITHMIC					// logarithmically resample
  };

  Resampler					// constructor
    (float value0,				//   starting value
     float incr,				//   increment
     int count,					//   number to generate
     Type type,					//   type of table to make
     float cycles = 1,				//   number of cycles
     float maximum = 1,				//   maximum value allowed
     float minimum = 0);			//   minimum value allowed

  ~Resampler ();				// destructor

  float value					// return a value at
    (int index);				//   given index (make currnt)

  float nextValue ();				// return value at next index

  float previousValue ();			// return vlu at previous index

  int index					// return an index
    (float index);				//   given value

  int status ()					// return status flag
    { return _status; }

private:
  float
    _incr,					// increment
    *_table;					// table of samples

  int
    _count,					// size of table
    _index,					// current index
    _status;					// status flag

};

#endif
