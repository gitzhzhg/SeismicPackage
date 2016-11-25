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

//----------------------- tred_data.hh --------------------//
//----------------------- tred_data.hh --------------------//
//----------------------- tred_data.hh --------------------//

#ifndef _TRED_DATA_HH_
#define _TRED_DATA_HH_

//  This class is derived from VectXdata.
//  This class minimizes index modification in the Trace Edit application.
//  A critical assumption in the use of this class is that the incoming x
//    arrays are sorted in ascending order and remain that way through the
//    life of the object!!!

#include "vect/vect_xdata.hh"

class TredData : public VectXdata
{

//------------------------- contents of class -----------------------//
//------------------------- contents of class -----------------------//
//------------------------- contents of class -----------------------//

public:

  TredData(int numPts, float *x, float y, long id = defaultId);
  virtual ~TredData();
  void replace (int numPts, float *x, float epsilon = 0.0);
  void insertValueRange (float strt_vlu, float end_vlu, float dx = 1.0,
    float epsilon = 0.0);
  void deleteValueRange (float strt_vlu, float end_vlu, float dx = 1.0,
    float epsilon = 0.0);
  int getNumSegs (float dx = 1.0, float epsilon = 0.0);
  int getSegments (int *numSegs, float *strt_vlus, float *end_vlus,
    float dx = 1.0, float epsilon = 0.0);

private:
        
/*
      The HP doesn't like these.  Since they are private in the
      base class, we probably don't need them here anyway.
      Commented out by Tom Stoeckley 12/22/94.
  TredData()
    { / * private, no access to default constructor * / }
  TredData(VectXdata &)
    { / * private, no access to copy constructor * / }
  TredData& operator=(TredData &p)
    { / * private, no access to = * / return p; }
*/
  void tredInsert (int *ptIndx, int *numIns, float *x, int insTo);
  void tredRemove (int *ptIndx, int *numRem, int remTo);

//----------------------- end of contents --------------------------//
//----------------------- end of contents --------------------------//
//----------------------- end of contents --------------------------//

};

#endif

//----------------------------- end --------------------------------//
//----------------------------- end --------------------------------//
//----------------------------- end --------------------------------//









