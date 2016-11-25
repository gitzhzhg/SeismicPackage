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
#ifndef _VECT_DATA_HH
#define _VECT_DATA_HH

#include "oprim/base_data.hh"

#include <assert.h>

class VectData : public BaseData
{
	public:

		VectData(int numPts, float *x, float *y, long id = defaultId);
		virtual ~VectData();
		virtual int getNumPts(long id = defaultId)
			{ assert(id == _id);  return _numPts; }
		virtual float getX(int i, long id = defaultId)
			{ assert(id == _id);  return _xData[i]; }
		virtual float getY(int i, long id = defaultId)
			{ assert(id == _id);  return _yData[i]; }
		void insert(int index, int numIns, float *x, float *y);
		void remove(int index, int numRem);
		void replace(int index, int numRep, float *x, float *y);
		void replace(int index, int numRem, int numIns,
			float *x, float *y);
		void setId(long id)
			{ _id = id; }
		long getId()
			{ return _id; }

	private:

		long _id;
		int _numPts;
		float *_xData;
		float *_yData;

		VectData()
			{ /* private, no access to default constructor */ }
		VectData(VectData &)
			{ /* private, no access to copy constructor */ }
		VectData& operator=(VectData &p)
			{ /* private, no access to = */ return p; }
};

#endif /* _VECT_DATA_HH */
