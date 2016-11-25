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
#include "vect/wire_data.hh"

#include <string.h>
#include <math.h>
#include <assert.h>

/* static variable */
float WireData::_degreesToRadiansFactor = 0.0;

WireData::WireData(int numPts, float *x, float *y, float *z,
	float degreesZ, float degreesY, long id) : BaseData(),
	_numPts(numPts), _degreesZ(degreesZ), _degreesY(degreesY), _id(id)
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

	/* Initialize static variable, atan(1) = 45 degrees */
	if (_degreesToRadiansFactor == 0.0)
		_degreesToRadiansFactor = (float) atan(1.0) / 45.0;

	setRotationFactors();
}

WireData::~WireData()
{
	if (_xData != NULL)
	{
		assert(_yData != NULL && _zData != NULL);

		delete [] _xData;
		delete [] _yData;
		delete [] _zData;
	}
	else
	{
		assert(_yData == NULL && _zData == NULL);
	}
}

float WireData::getX(int i, long id)
{
	assert(id == _id);

	return _x1 * _xData[i] + _x2 * _yData[i];
}

float WireData::getY(int i, long id)
{
	assert(id == _id);

	return _y1 * _xData[i] + _y2 * _yData[i] + _y3 * _zData[i];
}

void WireData::getDataPoint(int i, float *x, float *y, float *z, long id)
{
	assert(id == _id);

	*x = _xData[i];
	*y = _yData[i];
	*z = _zData[i];
}

void WireData::insert(int index, int numIns, float *x, float *y, float *z)
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

		modIndicesAfter(index, numIns, _id);	// check new points
		modDone(_id);
	}
}

void WireData::remove(int index, int numRem)
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

void WireData::replace(int index, int numRep, float *x, float *y, float *z)
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
			_zData[i + index] = z[i];
		}

		modIndicesAfter(index, numRep, _id);	// check new points
		modDone(_id);
	}
}

void WireData::replace(int index, int numRem, int numIns,
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

		modIndicesBefore(index, numRem, _id);	// check old points

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

		modIndicesAfter(index, numIns, _id);	// check new points
		modDone(_id);
	}
}

void WireData::setAngles(float degreesZ, float degreesY)
{
	modIndicesBefore(0, _numPts, _id);

	_degreesZ = degreesZ;
	_degreesY = degreesY;
	setRotationFactors();

	modIndicesAfter(0, _numPts, _id);
	modDone(_id);
}

void WireData::setRotationFactors()
{
	/*
	 * The rotation matrix is from "Mathematics of Classical
	 * and Quantum Physics", Byron & Fuller, Vol. 1,  p. 11.
	 * The screen x-axis is the 3D y-axis and
	 * the screen y-axis is the 3D z-axis.
	 */
	double radiansZ = (double) degreesToRadians(_degreesZ);
	double sinZ = sin(radiansZ);
	double cosZ = cos(radiansZ);

	double radiansY = (double) degreesToRadians(_degreesY);
	double sinY = sin(radiansY);
	double cosY = cos(radiansY);

	_x1 = (float) -sinZ;
	_x2 = (float)  cosZ;
	/* x3 is zero */
	_y1 = (float) (cosZ * sinY);
	_y2 = (float) (sinZ * sinY);
	_y3 = (float)  cosY;
}

float WireData::degreesToRadians(float degrees)
{
	for (; degrees >  180.0; degrees -= 180.0);
	for (; degrees < -180.0; degrees += 180.0);

	return _degreesToRadiansFactor * degrees;
}
