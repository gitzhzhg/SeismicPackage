#include "fg2d/fg2d_data.hh"
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
#include "geom/field_geometry.hh"

#include <assert.h>

Fg2DData::Fg2DData(FieldGeometry *fg, long id)
	:	BaseData(), _fg(fg), _id(id), _numPts(0),
		_oldMarkerColorIndex((int *) 0),
		_modifiedMarkerIndex((int *) 0),
		_numModifiedMarkers(0)
{
	/* just initializers */
}

Fg2DData::~Fg2DData()
{
	if (_oldMarkerColorIndex)
		delete _oldMarkerColorIndex;

	if (_modifiedMarkerIndex)
		delete _modifiedMarkerIndex;
}
void Fg2DData::markerColorFlush()
{
	if (_numModifiedMarkers > 0)
	{
		assert(_modifiedMarkerIndex);

		/*
		 * 1 is for ignore hold.
		 * Should not be holding
		 * since only change marker colors.
		 */
		modAttributesByIndices(_modifiedMarkerIndex,
			_numModifiedMarkers, 1, _id);

		_numModifiedMarkers = 0;
	}

	if (_modifiedMarkerIndex)
	{
		delete _modifiedMarkerIndex;
		_modifiedMarkerIndex = (int *) 0;
	}
}

void Fg2DData::storeMarkerColorsIndices()
{
	if (!_oldMarkerColorIndex)
	{
		assert(!_modifiedMarkerIndex && !_numModifiedMarkers);

		int numPts = getNumPts();

		_oldMarkerColorIndex = new int[numPts];

		for (int i = 0; i < numPts; i++)
			_oldMarkerColorIndex[i] = getAltMarkerColor(i, _id);
	}
}

void Fg2DData::updateMarkerColorsIndices()
{
	if (_oldMarkerColorIndex)
	{
		assert(!_modifiedMarkerIndex && !_numModifiedMarkers);

		int numPts = getNumPts();

		_modifiedMarkerIndex = new int[numPts];

		for (int i = 0; i < numPts; i++)
			if (getAltMarkerColor(i,_id) != _oldMarkerColorIndex[i])
				_modifiedMarkerIndex[_numModifiedMarkers++] = i;

		delete _oldMarkerColorIndex;
		_oldMarkerColorIndex = (int *) 0;
	}
}

void Fg2DData::forgetIt()
{
	if (_oldMarkerColorIndex)
	{
		assert(!_modifiedMarkerIndex && !_numModifiedMarkers);

		delete _oldMarkerColorIndex;
		_oldMarkerColorIndex = (int *) 0;
	}
}

int Fg2DData::getNumPts(long id)
{
	assert(id == _id);

	return _numPts;
}
