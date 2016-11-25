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
#ifndef _VECT_3D_DATA_HH
#define _VECT_3D_DATA_HH

#include "oprim/base_data.hh"

#include <assert.h>

class Vect3DData : public BaseData
{
	public:

		Vect3DData(int numPts, float *x, float *y, float *z,
			float nil = -1e-30, long id = defaultId);
		virtual ~Vect3DData();
		virtual int getNumPts(long id = defaultId)
			{ assert(id == _id);  return _numPts; }
		virtual float getX(int i, long id = defaultId)
			{ return getXYZ(i, _xData, 1, id); }
		virtual float getY(int i, long id = defaultId)
			{ return getXYZ(i, _yData, 1, id); }
		virtual float getZ(int i, long id = defaultId)
			{ return getXYZ(i, _zData, 1, id); }
		virtual float getXWithNil(int i, long id = defaultId)
			{ return getXYZ(i, _xData, 0, id); }
		virtual float getYWithNil(int i, long id = defaultId)
			{ return getXYZ(i, _yData, 0, id); }
		virtual float getZWithNil(int i, long id = defaultId)
			{ return getXYZ(i, _zData, 0, id); }
		void insert(int index, int numIns,
			float *x, float *y, float *z);
		void remove(int index, int numRem);
		void replace(int index, int numRep,
			float *x, float *y, float *z);
		void replace(int index, int numRem, int numIns,
			float *x, float *y, float *z);
		void setId(long id)
			{ _id = id; }
		long getId()
			{ return _id; }

	private:

		long _id;
		int _numPts;
		float *_xData;
		float *_yData;
		float *_zData;
		float  _nil;

		float getXYZ(int i, float *data, int interpolate, long id);
		void rangeToHardPts(int indexIn, int numIn,
			int *indexHard, int *numHard);

		Vect3DData()
			{ /* private, no access to default constructor */ }
		Vect3DData(Vect3DData &)
			{ /* private, no access to copy constructor */ }
		Vect3DData& operator=(Vect3DData &p)
			{ /* private, no access to = */ return p; }
};

#endif /* _VECT_3D_DATA_HH */
