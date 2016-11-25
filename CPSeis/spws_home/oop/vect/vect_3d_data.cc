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
#include "vect/vect_3d_data.hh"

#include <string.h>
#include <assert.h>

Vect3DData::Vect3DData(int numPts, float *x, float *y, float *z, float nil,
	long id)
	: BaseData(), _numPts(numPts), _nil(nil), _id(id)
{
	assert(_numPts >= 0);

	if (_numPts)
	{
		_xData = new float[_numPts];
		_yData = new float[_numPts];
		_zData = new float[_numPts];

		for (int i = 0; i < _numPts; i++)
		{
			_xData[i] = x[i];
			_yData[i] = y[i];
			_zData[i] = z[i];
		}
	}
	else
	{
		_xData = (float *) NULL;
		_yData = (float *) NULL;
		_zData = (float *) NULL;
	}
}

Vect3DData::~Vect3DData()
{
	if (_xData)
	{
		delete [] _xData;
		delete [] _yData;
		delete [] _zData;
	}
}

float Vect3DData::getXYZ(int i, float *data, int interpolate, long id)
{
	assert(i >= 0 && i < _numPts && id == _id);

	float retval = data[i];

        int before, after, check;

	if (interpolate && retval == _nil)
	{
		for (before = i - 1;
			before >= 0       && data[before] == _nil;
			before--);

		for (after  = i + 1;
			after  <  _numPts && data[after ] == _nil;
			after++ );

		/* Depends on results of >= and < being 0 or 1. */
		check = (before >= 0) + 2 * (after < _numPts);

		switch (check)
		{
			case 0:		/* no non-nils in data */
				retval = 0.0;
				break;
			case 1:		/* only a non-nil before */
				retval = data[before];
				break;
			case 2:		/* only a non-nil after */
				retval = data[after ];
				break;
			case 3:		/* non-nils both before and after */
				retval = (data[before] + data[after]) / 2.0;
				break;
			default:
				assert(0);
		}
	}

	return retval;
}

void Vect3DData::insert(int index, int numIns, float *x, float *y, float *z)
{
	assert(index >= 0
		&& index <= _numPts
		&& numIns >= 0);

	if (numIns)
	{
		float *oldXData = _xData;
		float *oldYData = _yData;
		float *oldZData = _zData;

		_numPts += numIns;
		_xData = new float[_numPts];
		_yData = new float[_numPts];
		_zData = new float[_numPts];

		memcpy(_xData, oldXData, (size_t) index * sizeof(float));
		memcpy(_yData, oldYData, (size_t) index * sizeof(float));
		memcpy(_zData, oldZData, (size_t) index * sizeof(float));

		for (int i = 0; i < numIns; i++)
		{
			_xData[i + index] = x[i];
			_yData[i + index] = y[i];
			_zData[i + index] = z[i];
		}

		memcpy(_xData + index + numIns, oldXData + index,
			(size_t) (_numPts - index - numIns) * sizeof(float));
		memcpy(_yData + index + numIns, oldYData + index,
			(size_t) (_numPts - index - numIns) * sizeof(float));
		memcpy(_zData + index + numIns, oldZData + index,
			(size_t) (_numPts - index - numIns) * sizeof(float));

		delete [] oldXData;
		delete [] oldYData;
		delete [] oldZData;

		// check new points
		int hardIndex, hardNum;
		rangeToHardPts(index, numIns, &hardIndex, &hardNum);
		modIndicesAfter(hardIndex, hardNum, _id);
		modDone(_id);
	}
}

void Vect3DData::remove(int index, int numRem)
{
	assert(index >= 0
		&& (index < _numPts || (index == 0 && numRem == 0))
		&& numRem >= 0
		&& numRem <= _numPts - index);

	if (numRem)
	{
		// check old points
		int hardIndex, hardNum;
		rangeToHardPts(index, numRem, &hardIndex, &hardNum);
		modIndicesBefore(hardIndex, hardNum, _id);

		float *oldXData = _xData;
		float *oldYData = _yData;
		float *oldZData = _zData;

		_numPts -= numRem;
		_xData = new float[_numPts];
		_yData = new float[_numPts];
		_zData = new float[_numPts];

		memcpy(_xData, oldXData, (size_t) index * sizeof(float));
		memcpy(_yData, oldYData, (size_t) index * sizeof(float));
		memcpy(_zData, oldZData, (size_t) index * sizeof(float));

		memcpy(_xData + index, oldXData + index + numRem,
			(size_t) (_numPts - index) * sizeof(float));
		memcpy(_yData + index, oldYData + index + numRem,
			(size_t) (_numPts - index) * sizeof(float));
		memcpy(_zData + index, oldZData + index + numRem,
			(size_t) (_numPts - index) * sizeof(float));

		delete [] oldXData;
		delete [] oldYData;
		delete [] oldZData;

		modDone(_id);
	}
}

void Vect3DData::replace(int index, int numRep, float *x, float *y, float *z)
{
	assert(index >= 0
		&& (index < _numPts || (index == 0 && numRep == 0))
		&& numRep >= 0
		&& numRep <= _numPts - index);

	if (numRep)
	{
		// check old points
		int hardIndex, hardNum;
		rangeToHardPts(index, numRep, &hardIndex, &hardNum);
		modIndicesBefore(hardIndex, hardNum, _id);

		for (int i = 0; i < numRep; i++)
		{
			_xData[i + index] = x[i];
			_yData[i + index] = y[i];
			_zData[i + index] = z[i];
		}

		// check new points
		modIndicesAfter(hardIndex, hardNum, _id);
		modDone(_id);
	}
}

void Vect3DData::replace(int index, int numRem, int numIns,
	float *x, float *y, float *z)
{
	if (numRem == numIns)
	{
		replace(index, numRem, x, y, z);
	}
	else if (numRem == 0)
	{
		insert(index, numIns, x, y, z);
	}
	else if (numIns == 0)
	{
		remove(index, numRem);
	}
	else
	{
		assert(index >= 0
			&& index < _numPts
			&& numRem <= _numPts - index);

		// check old points
		int hardIndex, hardNum;
		rangeToHardPts(index, numRem, &hardIndex, &hardNum);
		modIndicesBefore(hardIndex, hardNum, _id);

		float *oldXData = _xData;
		float *oldYData = _yData;
		float *oldZData = _zData;

		_numPts += numIns - numRem;
		_xData = new float[_numPts];
		_yData = new float[_numPts];
		_zData = new float[_numPts];

		memcpy(_xData, oldXData, (size_t) index * sizeof(float));
		memcpy(_yData, oldYData, (size_t) index * sizeof(float));
		memcpy(_zData, oldZData, (size_t) index * sizeof(float));

		for (int i = 0; i < numIns; i++)
		{
			_xData[i + index] = x[i];
			_yData[i + index] = y[i];
			_zData[i + index] = z[i];
		}

		memcpy(_xData + index + numIns, oldXData + index + numRem,
			(size_t) (_numPts - index - numIns) * sizeof(float));
		memcpy(_yData + index + numIns, oldYData + index + numRem,
			(size_t) (_numPts - index - numIns) * sizeof(float));
		memcpy(_zData + index + numIns, oldZData + index + numRem,
			(size_t) (_numPts - index - numIns) * sizeof(float));

		delete [] oldXData;
		delete [] oldYData;
		delete [] oldZData;

		// check new points
		rangeToHardPts(index, numIns, &hardIndex, &hardNum);
		modIndicesAfter(hardIndex, hardNum, _id);
		modDone(_id);
	}
}

void Vect3DData::rangeToHardPts(int indexIn, int numIn,
	int *indexHard, int *numHard)
{
        int start, end;
 	/*
	 * Find 1st point before without nils.
	 * If 1st point in data has nils, start = -1.
	 */
	for (start = indexIn - 1;
		start >= 0
			&& (_xData[start] == _nil || _yData[start] == _nil
			|| _zData[start] == _nil);
		start--);

	/*
	 * Find 1st point after without nils.
	 * If last point in data has nils, end = _numPts.
	 */
	for (end   = indexIn + numIn;
		end   < _numPts
			&& (_xData[end  ] == _nil || _yData[end  ] == _nil
			|| _zData[end  ] == _nil);
		end++  );

	/*
	 * ++start & --end get to last points with nils.
	 */
	*indexHard = ++start;
	*numHard   = --end - start + 1;
}
