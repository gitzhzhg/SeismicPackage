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
#ifndef _WIRE_DATA_HH
#define _WIRE_DATA_HH

#include "oprim/base_data.hh"

#include <assert.h>

class WireData : public BaseData
{
	public:

		WireData(int numPts, float *x, float *y, float *z,
			float degreesZ, float degreesY, long id = defaultId);
		virtual ~WireData();
		virtual int getNumPts(long id = defaultId)
			{ assert(id == _id);  return _numPts; }
		virtual float getX(int i, long id = defaultId);
		virtual float getY(int i, long id = defaultId);
		void getDataPoint(int i, float *x, float *y, float *z,
			long id = defaultId);
		void insert(int index, int numIns,
			float *x, float *y, float *z);
		void remove(int index, int numRem);
		void replace(int index, int numRep,
			float *x, float *y, float *z);
		void replace(int index, int numRem, int numIns,
			float *x, float *y, float *z);
		void setAngles(float degreesZ, float degreesY);
		void getAngles(float *degreesZ, float *degreesY)
			{ *degreesZ = _degreesZ;  *degreesY = _degreesY; }
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
		float _degreesZ;
		float _degreesY;
		float _x1, _x2;
		float _y1, _y2, _y3;
		static float _degreesToRadiansFactor;

		void setRotationFactors();
		float degreesToRadians(float degrees);

		WireData()
			{ /* private, no access to default constructor */ }
		WireData(WireData &)
			{ /* private, no access to copy constructor */ }
		WireData& operator=(WireData &p)
			{ /* private, no access to = */ return p; }
};

#endif /* _WIRE_DATA_HH */
