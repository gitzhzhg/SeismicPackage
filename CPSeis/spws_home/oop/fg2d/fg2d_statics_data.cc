#include "fg2d/fg2d_statics_data.hh"
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
#include "fgqc/statics_file.hh"
#include "vect/vector.hh"
#include "cprim.h"

#include <assert.h>

Fg2DStaticsData::Fg2DStaticsData(FieldGeometry *fg, long lineIndex,
	StaticsFile *sf,
	int activeColorIndex, int selectedColorIndex, int normalColorIndex,
	int activeDependent , int selectedDependent ,
	long id)
	: Fg2DData(fg, id),
	  _lineIndex(lineIndex),
	  _firstGroundPosition(_fg->firstGroundPosition(_lineIndex)),
	  _sf(sf),
	  _activeColorIndex  (activeColorIndex  ),
	  _selectedColorIndex(selectedColorIndex),
	  _normalColorIndex  (normalColorIndex  ),
	  _activeDependent   (activeDependent   ),
	  _selectedDependent (selectedDependent )
{
	/* just initializers */
}

Fg2DStaticsData::~Fg2DStaticsData()
{
	/* do nothing */
}

int Fg2DStaticsData::getNumPts(long id)
{
	assert(id == _id);

	return (int) _fg->numFlagsOnLine(_lineIndex);
}

float Fg2DStaticsData::getX(int i, long id)
{
	assert(i >= 0 && i < getNumPts() && id == _id);

	return (float) (i + (int) _firstGroundPosition);
}

float Fg2DStaticsData::getY(int i, long id)
{
	assert(id == _id);

	float retval = getStatic(i);

	return (retval == (float) ZNIL) ? (float) 0.0 : retval;
}

float Fg2DStaticsData::getStatic(int i)
{
	assert(i >= 0 && i < getNumPts());

	float retval;

	switch (_sf->getCoordinateSystem())
	{
		case StaticsFile::GRIDSYSTEM:
		case StaticsFile::GRIDSYSTEM2D:
			retval = _sf->interpolatePoint(
				(float) _fg->getXgrid(_lineIndex, (long) i),
				(float) _fg->getYgrid(_lineIndex, (long) i));
			break;
		case StaticsFile::SURVEYSYSTEM:
		case StaticsFile::SURVEYSYSTEM2D:
			retval = _sf->interpolatePoint(
				(float) _fg->getXloc (_lineIndex, (long) i),
				(float) _fg->getYloc (_lineIndex, (long) i));
			break;
		case StaticsFile::GROUNDPOSITION:
			retval = _sf->interpolatePoint(
				(float) (_firstGroundPosition + (long) i),
				(float) 0.0);
			break;
		case StaticsFile::GROUPSYSTEM:
			assert(!_fg->sourceGathersOutOfDate());
			if (_fg->numSourcesAtFlag(_lineIndex, (long) i))
			{
			retval = _sf->interpolatePoint(
				(float) _fg->sourceGroupNumber(
					_lineIndex, (long) i, (long) 0),
				(float) 0.0);
			}
			else
			{
				retval = (float) ZNIL;
			}
			break;
		case StaticsFile::UNRECOGNIZEDSYSTEM:
		default:
			assert(0);
	}

	return retval;
}

int Fg2DStaticsData::getMarkerType(int i, long id)
{
	assert(id == _id);

	return (getStatic(i) == (float) ZNIL) ? (int) Vector::NMarker
				      : (int) Vector::CrossMarker;
}

int Fg2DStaticsData::getAltMarkerColor(int i, long id)
{
	assert(i >= 0 && i < getNumPts() && id == _id);

	int retval;

	if (_activeDependent && 
		(_fg->getActiveFlagIndexOnLine(_lineIndex) == (long) i))
	{
		retval = _activeColorIndex;
	}
	else if (_selectedDependent &&
		(_fg->flagIsSelected(_lineIndex, (long) i)))
	{
		retval = _selectedColorIndex;
	}
	else
	{
		retval = _normalColorIndex ;
	}
		
	return retval;
}


int Fg2DStaticsData::getRange(float *xMin, float *xMax,
	float *yMin, float *yMax)
{
	int retval;

	int numPts = getNumPts();

	if (numPts)
	{
		*xMin = *xMax = getX(0);
		*yMin = *yMax = getY(0);

		float x, y;

		for (int i = 1; i < numPts; i++)
		{
			x = getX(i);
			y = getY(i);

			if (x < *xMin) *xMin = x;
			if (x > *xMax) *xMax = x;
			if (y < *yMin) *yMin = y;
			if (y > *yMax) *yMax = y;
		}

		retval = 1;
	}
	else
	{
		retval = 0;
	}

	return retval;
}

void Fg2DStaticsData::getMarkerPrecedence(int *activeDependent,
	int *selectedDependent)
{
	  *activeDependent =   _activeDependent;
	*selectedDependent = _selectedDependent;
}

void Fg2DStaticsData::setMarkerPrecedence(int activeDependent,
	int selectedDependent)
{
	  _activeDependent =   activeDependent;
	_selectedDependent = selectedDependent;
}

long Fg2DStaticsData::getLineIndex()
{
	return _lineIndex;
}

void Fg2DStaticsData::setActiveByIndex(int index)
{
	_fg->setActiveFlagIndexOnLine(_lineIndex, (long) index);
}

void Fg2DStaticsData::setSelectedByIndex(int *indices, int num,
	char c, int threshold)
{
	int doFreeze = (num > 1) &&
		(num * (int) _fg->numFlagsOnLine(_lineIndex) > threshold);

	if (doFreeze)
		_fg->freezeDependentUpdates();
	else
		_fg->preMultipleOperations ();

	for (int i = 0; i < num; i++)
		_fg->setFlagSelectValue(_lineIndex, (long) indices[i], c);

	if (doFreeze)
		_fg->resumeDependentUpdates();
	else
		_fg->postMultipleOperations();
}
void Fg2DStaticsData::outputByXlatByIndex(int index, long *line_index,
	long *flag_index, long *shot_index, FgMapToFlag::SkidType *skid_type)
{
	*line_index = _lineIndex;
	*flag_index = (long) index;
	*shot_index = -1;
	*skid_type = FgMapToFlag::NoSkid;
}
