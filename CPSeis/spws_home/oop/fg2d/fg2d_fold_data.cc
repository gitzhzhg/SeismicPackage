#include "fg2d/fg2d_fold_data.hh"
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

#include <stdlib.h>
#include <assert.h>

Fg2DFoldData::Fg2DFoldData(FieldGeometry *fg,
	int activeColorIndex, int selectedColorIndex, int normalColorIndex,
	int activeDependent , int selectedDependent ,
	long id)
	: Fg2DData(fg, id), 
	_activeColorIndex  (activeColorIndex  ),
	_selectedColorIndex(selectedColorIndex),
	_normalColorIndex  (normalColorIndex  ),
	_activeDependent   (activeDependent   ),
	_selectedDependent (selectedDependent ),
	_arraySize(0)
{
	/* just initializers */
}
	
Fg2DFoldData::~Fg2DFoldData()
{
	if (_numPts)
		free((void *) _foldPoint);
}

void Fg2DFoldData::addPoint(float gridBinCenter, long ixcmp, long fold)
{
	assert(!_modifiedMarkerIndex);

	if (_numPts == _arraySize)
	{
		_arraySize += (int) _ALLOCATION_INC;

		/*
		 * Use malloc 1st time.
		 */
		if ((int) _ALLOCATION_INC == _arraySize)
			assert(_foldPoint = (FoldPoint *) malloc (
				(size_t) _arraySize * sizeof(FoldPoint)));
		else
			assert(_foldPoint = (FoldPoint *) realloc(
				(void *) _foldPoint,
				(size_t) _arraySize * sizeof(FoldPoint)));
	}

	_foldPoint[_numPts].gridBinCenter = gridBinCenter;
	_foldPoint[_numPts].ixcmp         = ixcmp        ;
	_foldPoint[_numPts].fold          = fold         ;

	if (0 == _numPts)
	{
		_minGridBinCenter = _maxGridBinCenter = gridBinCenter;
		_maxFold = fold;
	}
	else
	{
		if (gridBinCenter < _minGridBinCenter)
			_minGridBinCenter = gridBinCenter;

		if (gridBinCenter > _maxGridBinCenter)
			_maxGridBinCenter = gridBinCenter;

		if (fold > _maxFold)
			_maxFold = fold;
	}

	_numPts++;
}

void Fg2DFoldData::adjustArray()
{
	if (_numPts < _arraySize)
		assert(_foldPoint = (FoldPoint *) realloc((void *) _foldPoint,
			(size_t) _numPts * sizeof(FoldPoint)));
}

float Fg2DFoldData::getX(int i, long id)
{
	assert(id == _id && i >= 0 && i < _numPts);

	return _foldPoint[i].gridBinCenter;
}

float Fg2DFoldData::getY(int i, long id)
{
	assert(id == _id && i >= 0 && i < _numPts);

	return (float) _foldPoint[i].fold;
}

int Fg2DFoldData::getAltMarkerColor(int i, long id)
{
	assert(id == _id && i >= 0 && i < _numPts);

	int retval;

	if (_activeDependent && _fg->getActiveCmpIndex() == _foldPoint[i].ixcmp)
	{
		retval = _activeColorIndex;
	}
	else if (_selectedDependent && _fg->cmpIsSelected(_foldPoint[i].ixcmp))
	{
		retval = _selectedColorIndex;
	}
	else
	{
		retval = _normalColorIndex ;
	}

	return retval;
}

int Fg2DFoldData::getRange(float *xMin, float *xMax, float *yMin, float *yMax)
{
	int retval;

	if (_numPts)
	{
		*xMin = _minGridBinCenter;
		*xMax = _maxGridBinCenter;
		*yMin = 0.0;
		*yMax = (float) _maxFold;

		retval = 1;
	}
	else
	{
		retval = 0;
	}

	return retval;
}

void Fg2DFoldData::getMarkerPrecedence(int *activeDependent,
	int *selectedDependent)
{
	  *activeDependent =   _activeDependent;
	*selectedDependent = _selectedDependent;
}

void Fg2DFoldData::setMarkerPrecedence(int  activeDependent,
	int  selectedDependent)
{
	  _activeDependent =   activeDependent;
	_selectedDependent = selectedDependent;
}

void Fg2DFoldData::setActiveByIndex(int index)
{
	_fg->setActiveCmpIndex(_foldPoint[index].ixcmp);
}

void Fg2DFoldData::setSelectedByIndex(int *indices, int num,
	char c, int threshold)
{
	int doFreeze = (num > 1) &&
		(num * (int) _fg->numCmpGathers() > threshold);

	if (doFreeze)
		_fg->freezeDependentUpdates();
	else
		_fg->preMultipleOperations ();

	for (int i = 0; i < num; i++)
		_fg->setCmpSelectValue(_foldPoint[indices[i]].ixcmp, c);

	if (doFreeze)
		_fg->resumeDependentUpdates();
	else
		_fg->postMultipleOperations();
}

void Fg2DFoldData::outputByXlatByIndex(int index, long *line_index,
	long *flag_index, long *shot_index, FgMapToFlag::SkidType *skid_type)
{
	if (_foldPoint[index].fold > 0)
	{
		/*
		 * Get 1st trace in cmp.
		 * Add 1 to go from trace index to trace number.
		 */
		long traceNum =
			_fg->originalTraceIndex(_foldPoint[index].ixcmp, 0) + 1;

		_fg->calculateHeaderWords(traceNum, 0);
		
		double header58 = _fg->getHeaderWordValue(58);
		double header59 = _fg->getHeaderWordValue(59);
		
		*line_index = _fg->findNearestLine(header58, header59);
		*flag_index = _fg->findNearestFlagOnLine(*line_index,
			header58, header59);
	}
	else
	{
		/*
		 * With non-zero fixed distance this may not be exactly
		 * right, but with no traces its the best I got.
		 */
		double xloc, yloc;
		_fg->getCmpLocBinCenter(_foldPoint[index].ixcmp, &xloc, &yloc);
		*line_index = _fg->findNearestLine(xloc, yloc);
		*flag_index = _fg->findNearestFlagOnLine(*line_index,
			xloc, yloc);
	}

	/*
	 * Returning line and flag for midpoint, not src or rcv.
	 */
	*shot_index = -1;
	*skid_type = FgMapToFlag::NoSkid;
}
