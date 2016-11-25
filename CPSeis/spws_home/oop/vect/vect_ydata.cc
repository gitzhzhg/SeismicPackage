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

//----------------------- vect_ydata.cc --------------------//
//----------------------- vect_ydata.cc --------------------//
//----------------------- vect_ydata.cc --------------------//


//  This class is identical to the VectData class except that the
//      X coordinate is assumed to be the index plus one, and no X array
//      is used inside the class.
//  This class can be used to display vectors on SeisPlot when
//      the number of points in the vector is equal to the number
//      of traces in memory (for the current display), and each point
//      is to be placed consecutively onto each trace.  The X coordinate
//      is the trace number in memory (for the current display).
//  This class was made from VectData by removing all references to _xData.
//  This class was written by Tom Stoeckley on 8/17/94.
//  This class was modified by Tom Stoeckley on 10/29/94 to automatically
//      restrict the number of data points being replaced to only that
//      range which actually changes.  See the comments in one of the
//      replace methods for details.
//  This class was modified by Tom Stoeckley on 11/02/94 to allow special
//      markers and special Y-coordinates for special data Y-values.
//  This class was modified by Tom Stoeckley on 01/16/95 to use new
//      options in Vector and BaseData for special markers and special
//      Y-coordinates for special data Y-values.


#include <string.h>
#include "vect/vect_ydata.hh"
#include "vect/vector.hh"
#include "cprim.h"
#include <iostream.h>
#include <assert.h>

//------------- constructor and destructor -------------------//
//------------- constructor and destructor -------------------//
//------------- constructor and destructor -------------------//

VectYdata::VectYdata(int numPts, float *y, long id)
	: BaseData(),
                _numPts               (numPts),
                _id                   (id),
                _num_special_markers  (0),
                _normal_marker        (Vector::NoMarker)
{
	assert(_numPts >= 0);

	if (_numPts)
	{
		_yData = new float[_numPts];

		for (int i = 0; i < _numPts; i++)
		{
			_yData[i] = y[i];
		}
	}
	else
	{
		_yData = (float *) NULL;
	}
}

VectYdata::~VectYdata()
{
	if (_yData)
	{
		delete [] _yData;
	}
}


//------------ functions for setting special markers ----------//
//------------ functions for setting special markers ----------//
//------------ functions for setting special markers ----------//


void VectYdata::useNormalMarker(int normal_marker)
{
  _normal_marker = normal_marker;
}


   // The following method replaces the special_marker and
   //   special_ycoord if the special_yvalue already
   //   is in the array; also, the coord/offset flag is reset.
   //   Otherwise, a new entry is added to the arrays.


void VectYdata::useSpecialMarker(float special_yvalue,
                                 int   special_marker,
                                 float special_ycoord,
                                 int   offset_flag)
{
  for(int num = 0; num < _num_special_markers; num++)
      {
      if(special_yvalue == _special_yvalue[num])
          {
          _special_marker[num] = special_marker;
          _special_ycoord[num] = special_ycoord;
          _offset_flag   [num] = offset_flag;
          return;
          }
      }
  assert(_num_special_markers < _MAX_NUM_SPECIAL_MARKERS);
  _special_yvalue[_num_special_markers] = special_yvalue;
  _special_marker[_num_special_markers] = special_marker;
  _special_ycoord[_num_special_markers] = special_ycoord;
  _offset_flag   [_num_special_markers] = offset_flag;
  _num_special_markers++;
}


//------------ functions to return info to the vector ------------//
//------------ functions to return info to the vector ------------//
//------------ functions to return info to the vector ------------//

   // Warning:  getY should be called only by a vector,
   //    since it returns a modified value in some cases.
   //    For anyone else to get a value, one of the fetch
   //    functions should be used instead.


float VectYdata::getY(int i, long id)
{
  assert(id == _id);
  for(int num = 0; num < _num_special_markers; num++)
      {
      if(_yData[i] == _special_yvalue[num])
               return _special_ycoord[num];
      }
  return _yData[i];
}



int VectYdata::getMarkerType(int i, long id)
{
  assert(id == _id);
  for(int num = 0; num < _num_special_markers; num++)
      {
      if(_yData[i] == _special_yvalue[num])
        return        _special_marker[num];
      }
  return _normal_marker;
}



int VectYdata::getYOffsetType(int i, long id)
{
  assert(id == _id);
  for(int num = 0; num < _num_special_markers; num++)
      {
      if(_yData[i] == _special_yvalue[num])
               return _offset_flag   [num];
      }
  return FALSE;
}



//-------------------------- fetch ---------------------------//
//-------------------------- fetch ---------------------------//
//-------------------------- fetch ---------------------------//


void VectYdata::fetch(int index, int numFetch, float *y)
{
	assert(index >= 0
		&& index < _numPts
		&& numFetch >= 0
                && numFetch <= _numPts - index);

	if (numFetch)
	{
		memcpy(y, _yData + index, (size_t) numFetch * sizeof(float));
	}
}


float VectYdata::fetch(int index)
{
	assert(index >= 0
		&& index < _numPts);

        return _yData[index];
}



//-------------------------- insert ----------------------------//
//-------------------------- insert ----------------------------//
//-------------------------- insert ----------------------------//


void VectYdata::insert(int index, int numIns, float *y)
{
	assert(index >= 0
		&& index <= _numPts
		&& numIns >= 0);

	if (numIns)
	{
		float *oldYData = _yData;

		_numPts += numIns;
		_yData = new float[_numPts];

		memcpy(_yData, oldYData, (size_t) index * sizeof(float));

		for (int i = 0; i < numIns; i++)
		{
			_yData[i + index] = y[i];
		}

		memcpy(_yData + index + numIns, oldYData + index,
			(size_t) (_numPts - index - numIns) * sizeof(float));

		delete [] oldYData;

		modIndicesAfter(index, numIns, _id);	// check new points
		modDone(_id);
	}
}



//-------------------------- remove ----------------------------//
//-------------------------- remove ----------------------------//
//-------------------------- remove ----------------------------//


void VectYdata::remove(int index, int numRem)
{
	assert(index >= 0
		&& (index < _numPts || (index == 0 && numRem == 0))
		&& numRem >= 0
		&& numRem <= _numPts - index);

	if (numRem)
	{
		modIndicesBefore(index, numRem, _id);	// check old points

		float *oldYData = _yData;

		_numPts -= numRem;
		_yData = new float[_numPts];

		memcpy(_yData, oldYData, (size_t) index * sizeof(float));

		memcpy(_yData + index, oldYData + index + numRem,
			(size_t) (_numPts - index) * sizeof(float));

		delete [] oldYData;

		modDone(_id);
	}
}



//-------------------------- replace ----------------------------//
//-------------------------- replace ----------------------------//
//-------------------------- replace ----------------------------//


void VectYdata::replace(int index, int numRep, float *y)
{
	assert(index >= 0
		&& (index < _numPts || (index == 0 && numRep == 0))
		&& numRep >= 0
		&& numRep <= _numPts - index);

	if (numRep)
	{
   //////// the following lines added to reduce replace range if possible.
                int i, ia = -1, ib = -1;
		for (i = 0; i < numRep; i++)
		{
                        if(_yData[i + index] != y[i])
                              {
                              if(ia == -1) ia = i;
                              ib = i;
                              }
		}
/*
          //////// temporary debug printout:
          cout << "  index = "  << index  <<
                  "  numRep = " << numRep <<
                  "  ia = "     << ia     <<
                  "  ib = "     << ib     << endl;
*/
                if(ia == -1) return;
                index += ia;
                numRep = ib - ia + 1;
                y += ia;
   //////// the above lines added to reduce replace range if possible.

		modIndicesBefore(index, numRep, _id);	// check old points

		for (i = 0; i < numRep; i++)
		{
			_yData[i + index] = y[i];
		}

		modIndicesAfter(index, numRep, _id);	// check new points
		modDone(_id);
	}
}


void VectYdata::replace(int index, int numRem, int numIns, float *y)
{
	if (numRem == numIns)
	{
		replace(index, numRem, y);
	}
	else if (numRem == 0)
	{
		insert(index, numIns, y);
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

		float *oldYData = _yData;

		_numPts += numIns - numRem;
		_yData = new float[_numPts];

		memcpy(_yData, oldYData, (size_t) index * sizeof(float));

		for (int i = 0; i < numIns; i++)
		{
			_yData[i + index] = y[i];
		}

		memcpy(_yData + index + numIns, oldYData + index + numRem,
			(size_t) (_numPts - index - numIns) * sizeof(float));

		delete [] oldYData;

		modIndicesAfter(index, numIns, _id);	// check new points
		modDone(_id);
	}
}


//---------------------------- end -----------------------------//
//---------------------------- end -----------------------------//
//---------------------------- end -----------------------------//

