#include "fg2d/fg2d_gpc_data.hh"
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
#include "geom/fg_constants.hh"

#include <stdlib.h>
#include <assert.h>

Fg2DGpcData::Fg2DGpcData(FieldGeometry *fg, long lineIndex,
	int srcMarker, int rcvMarker,
	int activeColorIndex, int selectedColorIndex, int normalColorIndex,
	int activeDependent , int selectedDependent ,
	ActiveDriver activeDriver, SelectDriver selectDriver, long id)
	: 	Fg2DChartData(fg,
			activeColorIndex, selectedColorIndex, normalColorIndex,
			activeDependent , selectedDependent ,
			activeDriver    , selectDriver      , id),
		_lineIndex(lineIndex),
		_firstGroundPosition(fg->firstGroundPosition(_lineIndex)),
		_srcMarker(srcMarker), _rcvMarker(rcvMarker),
		_arraySize(0)
{
	/* just initializers */
}

Fg2DGpcData::~Fg2DGpcData()
{
	if (_arraySize)
		free((void *) _gpcPoint);
}

void Fg2DGpcData::addPoint(long flagIndex, long groupNumber, long traceNumber,
	MarkerType markerType)
{
	assert(!_modifiedMarkerIndex);
	assert(flagIndex >= 0 && groupNumber >= 1);

	switch (markerType)
	{
		case Source  :	assert(traceNumber == -1);	break;
		case Receiver:	assert(traceNumber >=  1);	break;
		default      :	assert(                0);
	}

	if (_numPts == _arraySize)
	{
		_arraySize += (int) _ALLOCATION_INC;

		/*
		 * Use malloc 1st time.
		 */
		if ((int) _ALLOCATION_INC == _arraySize)
			assert(_gpcPoint = (GpcPoint *) malloc (
				(size_t) _arraySize * sizeof(GpcPoint)));
		else
			assert(_gpcPoint = (GpcPoint *) realloc(
				(void *) _gpcPoint,
				(size_t) _arraySize * sizeof(GpcPoint)));
	}

	_gpcPoint[_numPts].flagIndex    = flagIndex   ;
	_gpcPoint[_numPts].groupNumber  = groupNumber ;
	_gpcPoint[_numPts].traceNumber  = traceNumber ;
	_gpcPoint[_numPts].markerType   = markerType  ;
	_gpcPoint[_numPts].traceType    = Unknown     ;

	switch (_gpcPoint[_numPts].markerType)
	{
		case Source:
			_gpcPoint[_numPts].srcFlagIndex =  flagIndex;
			break;
		case Receiver:
			_fg->calculateHeaderWords(
				_gpcPoint[_numPts].traceNumber, 0);

			if (_fg->getHeaderSourceLineIndex() == _lineIndex)
				_gpcPoint[_numPts].srcFlagIndex =
					_fg->getHeaderSourceFlagIndex();
			else
				_gpcPoint[_numPts].srcFlagIndex = -1;

			break;
		default:
			assert(0);
	}

	if (0 == _numPts)
	{
		_minFlagIndex   = _maxFlagIndex   = flagIndex  ;
		_minGroupNumber = _maxGroupNumber = groupNumber;
	}
	else
	{
		if (flagIndex   < _minFlagIndex  )
			_minFlagIndex   = flagIndex  ;

		if (flagIndex   > _maxFlagIndex  )
			_maxFlagIndex   = flagIndex  ;

		if (groupNumber < _minGroupNumber)
			_minGroupNumber = groupNumber;

		if (groupNumber > _maxGroupNumber)
			_maxGroupNumber = groupNumber;
	}

	_numPts++;
}

void Fg2DGpcData::adjustArray()
{
	if (_numPts < _arraySize)
	{
		assert(_gpcPoint = (GpcPoint *) realloc((void *) _gpcPoint,
			(size_t) _numPts * sizeof(GpcPoint)));

		_arraySize = _numPts;
	}

	if (_numPts > 0)
		qsort((void *) _gpcPoint, (size_t) _numPts, sizeof(GpcPoint),
			gpcCom);
}

float Fg2DGpcData::getX(int i, long id)
{
	assert(id == _id && i >= 0 && i < _numPts);

	return (float) (_gpcPoint[i].flagIndex + _firstGroundPosition);
}

float Fg2DGpcData::getY(int i, long id)
{
	assert(id == _id && i >= 0 && i < _numPts);

	return (float) _gpcPoint[i].groupNumber;
}

int Fg2DGpcData::getMarkerType(int i, long id)
{
	assert(id == _id && i >= 0 && i < _numPts);

	int retval;

	switch (_gpcPoint[i].markerType)
	{
		case Source:
			retval = _srcMarker;
			break;
		case Receiver:
			retval = _rcvMarker;
			break;
		default:
			assert(0);
	}

	return retval;
}

int Fg2DGpcData::isActive(int i)
{
	/*
	 * If line does not exist or has changed its index, do not
	 * panic; the gathers will go out of date anyway.
	 */

	int retval;

	switch (_activeDriver)
	{
		case ActiveFlag:
			if (_lineIndex < _fg->numLines())
				retval = (_fg->getActiveFlagIndexOnLine(
					_lineIndex) == _gpcPoint[i].flagIndex);
			else
				retval = 0;
			break;
		case ActiveFlagSrc:
			if ((_gpcPoint[i].markerType == Source  )
				&& (_lineIndex < _fg->numLines()))
			{
				retval = (_fg->getActiveFlagIndexOnLine(
					_lineIndex) == _gpcPoint[i].flagIndex);
			}
			else
			{
				retval = 0;
			}
			break;
		case ActiveFlagRcv:
			if ((_gpcPoint[i].markerType == Receiver)
				&& (_lineIndex < _fg->numLines()))
			{
				retval = (_fg->getActiveFlagIndexOnLine(
					_lineIndex) == _gpcPoint[i].flagIndex);
			}
			else
			{
				retval = 0;
			}
			break;
		case ActiveTrace:
			retval = (_fg->getActiveTraceNumber()
				== _gpcPoint[i].traceNumber);
			break;
		case ActiveGroup:
			retval = (_fg->getActiveGroupNumber()
				== _gpcPoint[i].groupNumber);
			break;
		case ActiveFlagSrcGroups:
			if (_lineIndex < _fg->numLines())
				retval = (_fg->getActiveFlagIndexOnLine(
					_lineIndex)==_gpcPoint[i].srcFlagIndex);
			else
				retval = 0;
			break;
		case ActiveCmp:
		default:
			assert(0);
	}

	return retval;
}

int Fg2DGpcData::isSelected(int i)
{
	int retval;

	switch (_selectDriver)
	{
		case SelectedFlag:
			if ((_lineIndex < _fg->numLines())
				&& (_gpcPoint[i].flagIndex
					< _fg->numFlagsOnLine(_lineIndex)))
			{
				retval = _fg->flagIsSelected(
					_lineIndex, _gpcPoint[i].flagIndex);
			}
			else
			{
				retval = 0;
			}
			break;
		case SelectedFlagSrc:
			if ((_gpcPoint[i].markerType == Source  )
				&& (_lineIndex < _fg->numLines())
				&& (_gpcPoint[i].flagIndex
					< _fg->numFlagsOnLine(_lineIndex)))
			{
				retval = _fg->flagIsSelected(
					_lineIndex, _gpcPoint[i].flagIndex);
			}
			else
			{
				retval = 0;
			}
			break;
		case SelectedFlagRcv:
			if ((_gpcPoint[i].markerType == Receiver)
				&& (_lineIndex < _fg->numLines())
				&& (_gpcPoint[i].flagIndex
					< _fg->numFlagsOnLine(_lineIndex)))
			{
				retval = _fg->flagIsSelected(
					_lineIndex, _gpcPoint[i].flagIndex);
			}
			else
			{
				retval = 0;
			}
			break;
		case DeadTraces:
			assert(Unknown != _gpcPoint[i].traceType);
			retval = (Dead == _gpcPoint[i].traceType);
			break;
		case RevPolTraces:
			assert(Unknown != _gpcPoint[i].traceType);
			retval = (RevPol == _gpcPoint[i].traceType);
			break;
		case MissingTraces:
			assert(Unknown != _gpcPoint[i].traceType);
			retval = (Missing == _gpcPoint[i].traceType);
			break;
		case SelectedCmp:
		default:
			assert(0);
	}

	return retval;
}

int Fg2DGpcData::getRange(float *xMin, float *xMax, float *yMin, float *yMax)
{
	int retval;

	if (_numPts)
	{
		*xMin = (float) (_minFlagIndex + _firstGroundPosition);
		*xMax = (float) (_maxFlagIndex + _firstGroundPosition);
		*yMin = (float)  _minGroupNumber;
		*yMax = (float)  _maxGroupNumber;

		retval = 1;
	}
	else
	{
		retval = 0;
	}

	return retval;
}

void Fg2DGpcData::determineTraceTypes()
{
	for (int i = 0; i < _numPts; i++)
	{
		switch (_gpcPoint[i].markerType)
		{
			case Source  :
				switch (_fg->getDeadSourceCode(_lineIndex,
					_gpcPoint[i].flagIndex))
				{
					case ZT_CODE_ZERO:
						_gpcPoint[i].traceType =Dead   ;
						break;
					case ZT_CODE_REV :
						_gpcPoint[i].traceType =RevPol ;
						break;
					case ZT_CODE_MISS:
						_gpcPoint[i].traceType =Missing;
						break;
					case ZT_CODE_LIVE:
						_gpcPoint[i].traceType =Normal ;
						break;
					default:
						assert(0);
				}

				break;
			case Receiver:
				_gpcPoint[i].traceType =
					getTraceType(_gpcPoint[i].traceNumber);
				break;
			default:
				assert(0);
		}
	}
}

void Fg2DGpcData::actSelDeadRevMissChanged(long flagIndex, int num)
{
	if (!_modifiedMarkerIndex)
	{
		assert(0 == _numModifiedMarkers);
		_modifiedMarkerIndex = new int[_numPts];
	}

	long maxFlagIndex = flagIndex + (long) (num - 1);

	for (int i = 0; i < _numPts; i++)
		if (_gpcPoint[i].flagIndex >= flagIndex
		 && _gpcPoint[i].flagIndex <= maxFlagIndex)
		{
			_modifiedMarkerIndex[_numModifiedMarkers++] = i;
		}
}

long Fg2DGpcData::getLineIndex()
{
	return _lineIndex;
}

void Fg2DGpcData::setActiveByIndex(int index)
{
	if (_gpcPoint[index].markerType == Source)
	{
		/*
		 * I could store isx2 in GpcPoint, but I will tolerate this
		 * inefficiency until it becomes a performance problem.
		 * Remember, things get set active one at a time.
		 */
		long numSources = _fg->numSourcesAtFlag(_lineIndex,
			_gpcPoint[index].flagIndex);

		long isx2;
		for (isx2 = 0; isx2 < numSources; isx2++)
			if (_gpcPoint[index].groupNumber ==
				_fg->sourceGroupNumber(_lineIndex,
					_gpcPoint[index].flagIndex, isx2))
			{
				break;
			}

		assert(isx2 < numSources);

		_fg->setActiveSourceIndices(_lineIndex,
			_gpcPoint[index].flagIndex, isx2);
	}
	else
	{
		_fg->setActiveReceiverIndices(_gpcPoint[index].traceNumber);
	}
}

void Fg2DGpcData::setSelectedByIndex(int *indices, int num,
	char c, int threshold)
{
	assert(num > 0);

	long *flagIndices = new long[num];

	int i;
	for (i = 0; i < num; i++)
		flagIndices[i] = _gpcPoint[indices[i]].flagIndex;

	qsort((void *) flagIndices, (size_t) num, sizeof(long), compar);

	int numFlags;
	for (i = numFlags = 1; i < num; i++)
		if (flagIndices[i] != flagIndices[numFlags - 1])
			flagIndices[numFlags++] = flagIndices[i];

	int doFreeze = (numFlags > 1) &&
		(numFlags * (int) _fg->numFlagsOnLine(_lineIndex) > threshold);

	if (doFreeze)
		_fg->freezeDependentUpdates();
	else
		_fg->preMultipleOperations ();

	for (i = 0; i < numFlags; i++)
		_fg->setFlagSelectValue(_lineIndex, flagIndices[i], c);

	delete [] flagIndices;

	if (doFreeze)
		_fg->resumeDependentUpdates();
	else
		_fg->postMultipleOperations();
}

void Fg2DGpcData::outputByXlatByIndex(int index, long *line_index,
	long *flag_index, long *shot_index, FgMapToFlag::SkidType *skid_type)
{
	*line_index = _lineIndex;
	*flag_index = _gpcPoint[index].flagIndex;

	if (_gpcPoint[index].markerType == Source)
	{
		/*
		 * I could store isx2 in GpcPoint, but I will tolerate this
		 * inefficiency until it becomes a performance problem.
		 */
		long numSources = _fg->numSourcesAtFlag(_lineIndex,
			_gpcPoint[index].flagIndex);

		long isx2;
		for (isx2 = 0; isx2 < numSources; isx2++)
			if (_gpcPoint[index].groupNumber ==
				_fg->sourceGroupNumber(_lineIndex,
					_gpcPoint[index].flagIndex, isx2))
			{
				break;
			}

		assert(isx2 < numSources);

		*shot_index = isx2;

		if (_fg->sourceIsSkidded(*line_index, *flag_index, *shot_index))
			*skid_type = FgMapToFlag::ShotSkid;
		else
			*skid_type = FgMapToFlag::NoSkid  ;
	}
	else
	{
		*shot_index = -1;

		if (_fg->receiverIsSkidded(*line_index, *flag_index))
			*skid_type = FgMapToFlag::RecSkid;
		else
			*skid_type = FgMapToFlag::NoSkid ;
	}
}

int Fg2DGpcData::getIndexByGroup(long group, long seqGndPos)
{
	int retval;

	if (_numPts)
	{
		GpcPoint *key = new GpcPoint;
		key->groupNumber = group;
		key->flagIndex   = seqGndPos - _firstGroundPosition;

		void *ptr = bsearch((void *) key, (void *) _gpcPoint,
			(size_t) _numPts, sizeof(GpcPoint), gpcCom);

		if (ptr)
			retval = (int) ((GpcPoint *) ptr - _gpcPoint);
		else
			retval = -1;

		delete key;
	}
	else
	{
		retval = -1;
	}

	return retval;
}

int Fg2DGpcData::compar(const void *element1, const void *element2)
{
	return (*((long *) element1)  < *((long *) element2)) ? -1 :
	      ((*((long *) element1) == *((long *) element2)) ?  0 : 1);
}

int Fg2DGpcData::gpcCom(const void *element1, const void *element2)
{
	int retval;

	GpcPoint *ptr1 = (GpcPoint *) element1;
	GpcPoint *ptr2 = (GpcPoint *) element2;

	if      (ptr1->flagIndex < ptr2->flagIndex)
	{
		retval = -1;
	}
	else if (ptr1->flagIndex > ptr2->flagIndex)
	{
		retval =  1;
	}
	else
	{
		if      (ptr1->groupNumber < ptr2->groupNumber)
			retval = -1;
		else if (ptr1->groupNumber > ptr2->groupNumber)
			retval =  1;
		else
			retval =  0;
	}

	return retval;
}
