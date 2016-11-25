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
#include "pick/fk_poly_data.hh"

#include <stdio.h>
#include <string.h>
#include <assert.h>

FkPolyData::FkPolyData(class FkDataLinkedList *list,
	float headerValue1, float headerValue2,
	int numPts, float *wn, float *freq, CutPass cutPass, long id)
	: FkData(list, headerValue1, headerValue2, cutPass, id), _numPts(numPts)
{
	assert(_numPts >= 0);

	if (_numPts)
	{
		if (_numPts > 1
		 && wn  [0] == wn  [_numPts - 1]
		 && freq[0] == freq[_numPts - 1])
		{
			_numPts--;
		}

		_wn   = new float[_numPts];
		_freq = new float[_numPts];

		for (int i = 0; i < _numPts; i++)
		{
			_wn  [i] = wn  [i];
			_freq[i] = freq[i];
		}
	}
}

FkPolyData::FkPolyData(class FkDataLinkedList *list,
	float headerValue1, float headerValue2, FkPolyData *data, long id)
	: FkData(list, headerValue1, headerValue2, data->_cutPass, id)
{
	_numPts = data->_numPts;

	_wn   = new float[_numPts];
	_freq = new float[_numPts];

	for (int i = 0; i < _numPts; i++)
	{
		_wn  [i] = (data->_wn  )[i];
		_freq[i] = (data->_freq)[i];
	}
}

FkPolyData::~FkPolyData()
{
	if (_numPts)
	{
		delete [] _wn  ;
		delete [] _freq;
	}
}

void FkPolyData::insert(int index, float x, float y)
{
	assert((index >= 1 || (index == 0 && _numPts == 0))
		&& (index <= _numPts));

	float *oldWn   = _wn  ;
	float *oldFreq = _freq;

	_numPts++;
	_wn   = new float[_numPts];
	_freq = new float[_numPts];

	memcpy(_wn  , oldWn  , (size_t) index * sizeof(float));
	memcpy(_freq, oldFreq, (size_t) index * sizeof(float));

	_wn  [index] = x;
	_freq[index] = y;

	memcpy(_wn   + index + 1, oldWn   + index,
		(size_t) (_numPts - index - 1) * sizeof(float));
	memcpy(_freq + index + 1, oldFreq + index,
		(size_t) (_numPts - index - 1) * sizeof(float));

	delete [] oldWn  ;
	delete [] oldFreq;

	modIndicesAfter(index, 1, _id);    // check new points
	modDone(_id);
}

void FkPolyData::remove(int index)
{
	assert(index >= 0 && index <= _numPts && _numPts > 0);

	if (index == 0 || index == _numPts)
	{
		modIndicesBefore(0      , 1, _id);   // check old points
		modIndicesBefore(_numPts, 1, _id);   // check old points
		index = 0;
	}
	else
	{
		modIndicesBefore(index  , 1, _id);   // check old points
	}

	float *oldWn   = _wn  ;
	float *oldFreq = _freq;

	_numPts--;
	_wn   = new float[_numPts];
	_freq = new float[_numPts];

	memcpy(_wn  , oldWn  , (size_t) index * sizeof(float));
	memcpy(_freq, oldFreq, (size_t) index * sizeof(float));

	memcpy(_wn   + index, oldWn   + index + 1,
		(size_t) (_numPts - index) * sizeof(float));
	memcpy(_freq + index, oldFreq + index + 1,
		(size_t) (_numPts - index) * sizeof(float));

	delete [] oldWn  ;
	delete [] oldFreq;

	modDone(_id);
}

void FkPolyData::replace(int index, float x, float y)
{
	assert(index >= 0 && index <= _numPts && _numPts > 0);

	if (index == 0 || index == _numPts)
	{
		modIndicesBefore(0      , 1, _id);   // check old points
		modIndicesBefore(_numPts, 1, _id);   // check old points
		index = 0;
	}
	else
	{
		modIndicesBefore(index  , 1, _id);   // check old points
	}

	_wn  [index] = x;
	_freq[index] = y;

	if (index == 0)
	{
		modIndicesAfter (0      , 1, _id);   // check new points
		modIndicesAfter (_numPts, 1, _id);   // check new points
	}
	else
	{
		modIndicesAfter (index  , 1, _id);   // check new points
	}

	modDone(_id);
}

int FkPolyData::writeRecord(FILE *stream)
{
	if (EOF == fprintf(stream, "POLYGON "))
                return -1;

	int retval;
	switch(_cutPass)
	{
		case Cut :  retval = fprintf(stream, "CUT   " );  break;
		case Pass:  retval = fprintf(stream, "PASS   ");  break;
		default  :  assert(0);
	}
	if (EOF == retval)
                return -1;

	if (EOF == fprintf(stream, "%d", _numPts + 1))
                return -1;

	for (int i = 0; i <= _numPts; i++)
	{
		if ((i != 0) && (i % 2 == 0))
			if (EOF == fprintf(stream, "\n"))
				return -1;

		if (EOF == fprintf(stream, "   %f, %f",
			_wn[i % _numPts], _freq[i % _numPts]))
		{
			return -1;
		}
	}

	if (EOF == fprintf(stream, "\n"))
		return -1;

	return 0;
}
