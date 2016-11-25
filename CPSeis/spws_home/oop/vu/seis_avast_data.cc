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
#include "vu/seis_avast_data.hh"

#include <string.h>
#include <assert.h>

AvastData::AvastData( long maxIds)
{
  long i;

  assert(maxIds >0);
  _maxIds = maxIds;
  _curId = 0;
  _vData = new AvastVectStruct [_maxIds];
  for (i=0; i<_maxIds; i++) {
      _vData[i].x = NULL;
      _vData[i].y = NULL;
  }
}


void AvastData::insert(int numPts, float *x, float *y, long id)
{
        _vData[_curId].id     = id;
        _vData[_curId].numPts = numPts;

	assert(numPts >= 0);

	if (numPts)
	{
		_vData[_curId].x = new float[numPts];
		_vData[_curId].y = new float[numPts];

		for (int i = 0; i < numPts; i++)
		{
			_vData[_curId].x[i] = x[i];
			_vData[_curId].y[i] = y[i];
		}
	}
	else
	{
		_vData[_curId].x = (float *) NULL;
		_vData[_curId].y = (float *) NULL;
	}
        _curId++;
}

int AvastData::getNumPts(long id)
{
    long i;

    for (i=0; i<_maxIds; i++) {
        if (_vData[i].id == id) {
           return(_vData[i].numPts);
        }
    }
    return(0);
}

float AvastData::getX(int indx, long id)
{
  long i;

  for (i=0; i<_maxIds; i++) {
      if (_vData[i].id == id) {
         return(_vData[i].x[indx]);
      }
  }
  return(0);
}


float AvastData::getY(int indx, long id)
{
  long i;

  for (i=0; i<_maxIds; i++) {
      if (_vData[i].id == id) {
         return(_vData[i].y[indx]);
      }
  }
  return(0);
}


AvastData::~AvastData()
{
  long i;

  if (_vData) {
     for (i=0; i<_maxIds; i++) {
         if (_vData[i].x) {
            delete [] _vData[i].x;
            _vData[i].x = NULL;
         }
         if (_vData[i].y) {
            delete [] _vData[i].y;
             _vData[i].y = NULL;
         }
     }
     delete [] _vData;
     _vData = NULL;
  }
}
