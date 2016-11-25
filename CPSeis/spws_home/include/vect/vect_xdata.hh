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

//----------------------- vect_xdata.hh --------------------//
//----------------------- vect_xdata.hh --------------------//
//----------------------- vect_xdata.hh --------------------//

#ifndef _VECT_XDATA_HH_
#define _VECT_XDATA_HH_

//  This class is identical to the VectData class except that the
//      Y coordinate is assumed to be a constant passed to the constructor,
//      and no Y array is used inside the class.
//  See vect_xdata.cc for further rationale and documentation.

#include "oprim/base_data.hh"
#include <assert.h>

class VectXdata : public BaseData
{

//------------------------- contents of class -----------------------//
//------------------------- contents of class -----------------------//
//------------------------- contents of class -----------------------//

public:

	VectXdata(int numPts, float *x, float y, long id = defaultId);
	virtual ~VectXdata();
	virtual int getNumPts(long id = defaultId)
		{ assert(id == _id);  return _numPts; }
	virtual float getX(int i, long id = defaultId)
		{ assert(id == _id);  return _xData[i]; }
	virtual float getY(int /* i */, long id = defaultId)
		{ assert(id == _id);  return _yConst; }
	virtual void setYConst(float y, long id = defaultId)
		{ assert(id == _id);  _yConst = y; }
	void insert(int index, int numIns, float *x);
	void remove(int index, int numRem);
	void replace(int index, int numRep, float *x);
	void replace(int index, int numRem, int numIns, float *x);
	void setId(long id)
		{ _id = id; }
	long getId()
		{ return _id; }

protected:

	int _numPts;
	float *_xData;

private:

	long _id;
        float _yConst;

	VectXdata()
		{ /* private, no access to default constructor */ }
	VectXdata(VectXdata &)
		{ /* private, no access to copy constructor */ }
	VectXdata& operator=(VectXdata &p)
		{ /* private, no access to = */ return p; }

//----------------------- end of contents --------------------------//
//----------------------- end of contents --------------------------//
//----------------------- end of contents --------------------------//

};

#endif

//----------------------------- end --------------------------------//
//----------------------------- end --------------------------------//
//----------------------------- end --------------------------------//

