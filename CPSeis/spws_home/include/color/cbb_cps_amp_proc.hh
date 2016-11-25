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
// class that processes amplitudes for the color bar builder like CPS does
#ifndef CBB_CPS_AMP_PROC_HH
#define CBB_CPS_AMP_PROC_HH

#include "color/amplitude_processor.hh"

class CBBCPSAmpProc : public AmplitudeProcessor {

public:
  CBBCPSAmpProc ();				// constructor

  virtual ~CBBCPSAmpProc ();			// destructor

  virtual float minimumOfLevel			// return minimum of the
    (int level);				//   given level

  virtual float maximumOfLevel			// return maximum of the
    (int level);				//   given level

  virtual float valueOfLevel			// return value of given
    (float level);				//   level + fraction

private:
  virtual int initNonuniqueLevels ();		// init where levels nonunique

  virtual int initUniqueLevels ();		// init where levels unique

};

#endif
