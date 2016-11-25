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
#include "vect/vect_data.hh"

#include <string.h>
#include <assert.h>

VectData::VectData(int numPts, float *x, float *y, long id)
	: BaseData(), _numPts(numPts), _id(id)
{
	assert(_numPts >= 0);

	if (_numPts)
	{
		_xData = new float[_numPts];
		_yData = new float[_numPts];

		for (int i = 0; i < _numPts; i++)
		{
			_xData[i] = x[i];
			_yData[i] = y[i];
		}
	}
	else
	{
		_xData = (float *) NULL;
		_yData = (float *) NULL;
	}
}

VectData::~VectData()
{
	if (_xData)
	{
		delete [] _xData;
		delete [] _yData;
	}
}

void VectData::insert(int index, int numIns, float *x, float *y)
{
	assert(index >= 0
		&& index <= _numPts
		&& numIns >= 0);

	if (numIns)
	{
		float *oldXData = _xData;
		float *oldYData = _yData;

		_numPts += numIns;
		_xData = new float[_numPts];
		_yData = new float[_numPts];

		memcpy(_xData, oldXData, (size_t) index * sizeof(float));
		memcpy(_yData, oldYData, (size_t) index * sizeof(float));

		for (int i = 0; i < numIns; i++)
		{
			_xData[i + index] = x[i];
			_yData[i + index] = y[i];
		}

		memcpy(_xData + index + numIns, oldXData + index,
			(size_t) (_numPts - index - numIns) * sizeof(float));
		memcpy(_yData + index + numIns, oldYData + index,
			(size_t) (_numPts - index - numIns) * sizeof(float));

		delete [] oldXData;
		delete [] oldYData;

		modIndicesAfter(index, numIns, _id);	// check new points
		modDone(_id);
	}
}

void VectData::remove(int index, int numRem)
{
	assert(index >= 0
		&& (index < _numPts || (index == 0 && numRem == 0))
		&& numRem >= 0
		&& numRem <= _numPts - index);

	if (numRem)
	{
		modIndicesBefore(index, numRem, _id);	// check old points

		float *oldXData = _xData;
		float *oldYData = _yData;

		_numPts -= numRem;
		_xData = new float[_numPts];
		_yData = new float[_numPts];

		memcpy(_xData, oldXData, (size_t) index * sizeof(float));
		memcpy(_yData, oldYData, (size_t) index * sizeof(float));

		memcpy(_xData + index, oldXData + index + numRem,
			(size_t) (_numPts - index) * sizeof(float));
		memcpy(_yData + index, oldYData + index + numRem,
			(size_t) (_numPts - index) * sizeof(float));

		delete [] oldXData;
		delete [] oldYData;

		modDone(_id);
	}
}

void VectData::replace(int index, int numRep, float *x, float *y)
{
	assert(index >= 0
		&& (index < _numPts || (index == 0 && numRep == 0))
		&& numRep >= 0
		&& numRep <= _numPts - index);

	if (numRep)
	{
		modIndicesBefore(index, numRep, _id);	// check old points

		for (int i = 0; i < numRep; i++)
		{
			_xData[i + index] = x[i];
			_yData[i + index] = y[i];
		}

		modIndicesAfter(index, numRep, _id);	// check new points
		modDone(_id);
	}
}

void VectData::replace(int index, int numRem, int numIns, float *x, float *y)
{
	if (numRem == numIns)
	{
		replace(index, numRem, x, y);
	}
	else if (numRem == 0)
	{
		insert(index, numIns, x, y);
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

		modIndicesBefore(index, numRem, _id);	// check old points

		float *oldXData = _xData;
		float *oldYData = _yData;

		_numPts += numIns - numRem;
		_xData = new float[_numPts];
		_yData = new float[_numPts];

		memcpy(_xData, oldXData, (size_t) index * sizeof(float));
		memcpy(_yData, oldYData, (size_t) index * sizeof(float));

		for (int i = 0; i < numIns; i++)
		{
			_xData[i + index] = x[i];
			_yData[i + index] = y[i];
		}

		memcpy(_xData + index + numIns, oldXData + index + numRem,
			(size_t) (_numPts - index - numIns) * sizeof(float));
		memcpy(_yData + index + numIns, oldYData + index + numRem,
			(size_t) (_numPts - index - numIns) * sizeof(float));

		delete [] oldXData;
		delete [] oldYData;

		modIndicesAfter(index, numIns, _id);	// check new points
		modDone(_id);
	}
}
