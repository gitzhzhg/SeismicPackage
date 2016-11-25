#include "fg2d/fg2d_sc_data.hh"
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
#include <math.h>

Fg2DScData *Fg2DScData::_thisData;	/* static data element */

Fg2DScData::Fg2DScData(FieldGeometry *fg,
	int activeColorIndex, int selectedColorIndex, int normalColorIndex,
	int activeDependent , int selectedDependent ,
	ActiveDriver activeDriver, SelectDriver selectDriver, long id)
	:	Fg2DChartData(fg,
			activeColorIndex, selectedColorIndex, normalColorIndex,
			activeDependent , selectedDependent ,
			activeDriver    , selectDriver      , id),
		_arraySize(0)
{
	/* just initializers */
}

Fg2DScData::~Fg2DScData()
{
	if (_numPts)
		free((void *) _scPoint);
}

void Fg2DScData::addPoint(float gridBinCenter, long ixcmp,
	long groupNumber, long traceNumber)
{
	assert(!_modifiedMarkerIndex);
	assert(groupNumber >= 1);

	if (_numPts == _arraySize)
	{
		_arraySize += (int) _ALLOCATION_INC;

		/*
		 * Use malloc 1st time.
		 */
		if ((int) _ALLOCATION_INC == _arraySize)
			assert(_scPoint = (ScPoint *) malloc (
				(size_t) _arraySize * sizeof(ScPoint)));
		else
			assert(_scPoint = (ScPoint *) realloc(
				(void *) _scPoint,
				(size_t) _arraySize * sizeof(ScPoint)));
	}

	_scPoint[_numPts].gridBinCenter =
		(long) floor((double) gridBinCenter + 0.5);
	_scPoint[_numPts].ixcmp         = ixcmp        ;
	_scPoint[_numPts].groupNumber   = groupNumber  ;
	_scPoint[_numPts].traceNumber   = traceNumber  ;
	_scPoint[_numPts].traceType     = Unknown      ;

	_fg->calculateHeaderWords(_scPoint[_numPts].traceNumber, 0);
	_scPoint[_numPts].srcLineIndex = _fg->getHeaderSourceLineIndex  ();
	_scPoint[_numPts].srcFlagIndex = _fg->getHeaderSourceFlagIndex  ();
	_scPoint[_numPts].rcvLineIndex = _fg->getHeaderReceiverLineIndex();
	_scPoint[_numPts].rcvFlagIndex = _fg->getHeaderReceiverFlagIndex();

	if (0 == _numPts)
	{
		_minGridBinCenter = _maxGridBinCenter = gridBinCenter;
		_minGroupNumber   = _maxGroupNumber   = groupNumber  ;
	}
	else
	{
		if (gridBinCenter < _minGridBinCenter)
			_minGridBinCenter = gridBinCenter;

		if (gridBinCenter > _maxGridBinCenter)
			_maxGridBinCenter = gridBinCenter;

		if (groupNumber   < _minGroupNumber  )
			_minGroupNumber   = groupNumber  ;

		if (groupNumber   > _maxGroupNumber  )
			_maxGroupNumber   = groupNumber  ;
	}

	_numPts++;
}

void Fg2DScData::adjustArray()
{
	if (_numPts < _arraySize)
	{
		assert(_scPoint = (ScPoint *) realloc((void *) _scPoint,
			(size_t) _numPts * sizeof(ScPoint)));

		_arraySize = _numPts;
        }

	if (_numPts > 0)
		qsort((void *) _scPoint, (size_t) _numPts, sizeof(ScPoint),
			scCom);
}

float Fg2DScData::getX(int i, long id)
{
	assert(id == _id && i >= 0 && i < _numPts);

	return (float) _scPoint[i].gridBinCenter;
}

float Fg2DScData::getY(int i, long id)
{
	assert(id == _id && i >= 0 && i < _numPts);

	return (float) _scPoint[i].groupNumber;
}

int Fg2DScData::isActive(int i)
{
  int retval;

  long numLines = _fg->numLines();

  switch (_activeDriver)
  {
    case ActiveFlag:
      if ((_scPoint[i].srcLineIndex < numLines)
       && (_scPoint[i].rcvLineIndex < numLines))
      {
        long activeLineIndex = _fg->getActiveLineIndex();

        retval = (((activeLineIndex == _scPoint[i].srcLineIndex)
                && (_fg->getActiveFlagIndexOnLine(_scPoint[i].srcLineIndex)
                   == _scPoint[i].srcFlagIndex))
               || ((activeLineIndex == _scPoint[i].rcvLineIndex)
                && (_fg->getActiveFlagIndexOnLine(_scPoint[i].rcvLineIndex)
                   == _scPoint[i].rcvFlagIndex)));
      }
      else
      {
        retval = 0;
      }
      break;
    case ActiveFlagSrc:
    case ActiveFlagSrcGroups:
      if (_scPoint[i].srcLineIndex < numLines)
        retval = ((_fg->getActiveLineIndex()  == _scPoint[i].srcLineIndex)
                && (_fg->getActiveFlagIndexOnLine(_scPoint[i].srcLineIndex)
                   == _scPoint[i].srcFlagIndex));
      else
        retval = 0;
      break;
    case ActiveFlagRcv:
      if (_scPoint[i].rcvLineIndex < numLines)
        retval = ((_fg->getActiveLineIndex()  == _scPoint[i].rcvLineIndex)
                && (_fg->getActiveFlagIndexOnLine(_scPoint[i].rcvLineIndex)
                   == _scPoint[i].rcvFlagIndex));
      else
        retval = 0;
      break;
    case ActiveCmp:
      retval = (_fg->getActiveCmpIndex() == _scPoint[i].ixcmp);
      break;
    case ActiveTrace:
      retval = (_fg->getActiveTraceNumber() == _scPoint[i].traceNumber);
      break;
    case ActiveGroup:
      retval = (_fg->getActiveGroupNumber() == _scPoint[i].groupNumber);
      break;
    default:
      assert(0);
  }

  return retval;
}

int Fg2DScData::isSelected(int i)
{
  int retval;

  long numLines = _fg->numLines();

  switch (_selectDriver)
  {
    case SelectedFlag:
      if ((_scPoint[i].srcLineIndex < numLines)
       && (_scPoint[i].rcvLineIndex < numLines)
       && (_scPoint[i].srcFlagIndex <
             _fg->numFlagsOnLine(_scPoint[i].srcLineIndex))
       && (_scPoint[i].rcvFlagIndex <
             _fg->numFlagsOnLine(_scPoint[i].rcvLineIndex)))
      {
        retval = (_fg->flagIsSelected(_scPoint[i].srcLineIndex,
                    _scPoint[i].srcFlagIndex)
               || _fg->flagIsSelected(_scPoint[i].rcvLineIndex,
                    _scPoint[i].rcvFlagIndex));
      }
      else
      {
        retval = 0;
      }
      break;
    case SelectedFlagSrc:
      if ((_scPoint[i].srcLineIndex < numLines)
       && (_scPoint[i].srcFlagIndex <
             _fg->numFlagsOnLine(_scPoint[i].srcLineIndex)))
      {
        retval = _fg->flagIsSelected(_scPoint[i].srcLineIndex,
                    _scPoint[i].srcFlagIndex);
      }
      else
      {
        retval = 0;
      }
      break;
    case SelectedFlagRcv:
      if ((_scPoint[i].rcvLineIndex < numLines)
       && (_scPoint[i].rcvFlagIndex <
             _fg->numFlagsOnLine(_scPoint[i].rcvLineIndex)))
      {
        retval = _fg->flagIsSelected(_scPoint[i].rcvLineIndex,
                    _scPoint[i].rcvFlagIndex);
      }
      else
      {
        retval = 0;
      }
      break;
    case SelectedCmp:
      if (_scPoint[i].ixcmp < _fg->numCmpGathers())
      {
        retval = _fg->cmpIsSelected(_scPoint[i].ixcmp);
      }
      else
      {
        retval = 0;
      }
      break;
    case DeadTraces:
      assert(Unknown != _scPoint[i].traceType);
      retval = (Dead == _scPoint[i].traceType);
      break;
    case RevPolTraces:
      assert(Unknown != _scPoint[i].traceType);
      retval = (RevPol == _scPoint[i].traceType);
      break;
    case MissingTraces:
      assert(Unknown != _scPoint[i].traceType);
      retval = (Missing == _scPoint[i].traceType);
      break;
    default:
      assert(0);
  }

  return retval;
}

int Fg2DScData::getRange(float *xMin, float *xMax, float *yMin, float *yMax)
{
	int retval;

	if (_numPts)
	{
		*xMin = _minGridBinCenter;
		*xMax = _maxGridBinCenter;
		*yMin = (float) _minGroupNumber;
		*yMax = (float) _maxGroupNumber;

		retval = 1;
	}
	else
	{
		retval = 0;
	}

	return retval;
}

void Fg2DScData::determineTraceTypes()
{
	for (int i = 0; i < _numPts; i++)
		_scPoint[i].traceType = getTraceType(_scPoint[i].traceNumber);
}

void Fg2DScData::actSelDeadRevMissChanged(long /*flagIndex*/, int /*num*/)
{
	if (!_modifiedMarkerIndex)
	{
		assert(0 == _numModifiedMarkers);
		_modifiedMarkerIndex = new int[_numPts];
	}
}

void Fg2DScData::setActiveByIndex(int index)
{
	_fg->setActiveReceiverIndices(_scPoint[index].traceNumber);
}

void Fg2DScData::setSelectedByIndex(int *indices, int num,
	char c, int threshold)
{
	assert(num > 0);

	long *cmpIndices = new long[num];
	int i;
	for (i = 0; i < num; i++)
		cmpIndices[i] = _scPoint[indices[i]].ixcmp;

	qsort((void *) cmpIndices, (size_t) num, sizeof(long), comparCmps);

	int numCmps;
	for (i = numCmps = 1; i < num; i++)
		if (cmpIndices[i] != cmpIndices[numCmps - 1])
			cmpIndices[numCmps++] = cmpIndices[i];
	/*
	 * Allows static comparRcvs to use _scPoint.
	 * Will not work in multi-threaded application.
	 */
	_thisData = this;

	/*
	 * Use indices array for sort and duplicate elimination.
	 * Calling methods will have indices changed so do not use it.
	 */
	qsort((void *) indices, (size_t) num, sizeof(long), comparRcvs);

	int numFlags, longest, length;
	for (i = numFlags = 1,
		longest = (int) _fg->numFlagsOnLine(_scPoint[0].rcvLineIndex);
		i < num; i++)
	{
		if      (_scPoint[indices[i           ]].rcvLineIndex
		      != _scPoint[indices[numFlags - 1]].rcvLineIndex)
		{
			length = (int) _fg->numFlagsOnLine(
				_scPoint[indices[i]].rcvLineIndex);

			if (length > longest)
				longest = length;

			indices[numFlags++] = indices[i];
		}
		else if (_scPoint[indices[i           ]].rcvFlagIndex
		      != _scPoint[indices[numFlags - 1]].rcvFlagIndex)
		{
			indices[numFlags++] = indices[i];
		}
	}

	int gathers = (int) _fg->numCmpGathers();
	int doFreeze = ((numCmps  > 1) && (numCmps  * gathers > threshold))
		    || ((numFlags > 1) && (numFlags * longest > threshold));

	if (doFreeze)
		_fg->freezeDependentUpdates();
	else
		_fg->preMultipleOperations ();

	for (i = 0; i < numFlags; i++)
		_fg->setFlagSelectValue(_scPoint[indices[i]].rcvLineIndex,
					_scPoint[indices[i]].rcvFlagIndex, c);

	for (i = 0; i < numCmps; i++)
		_fg->setCmpSelectValue(cmpIndices[i], c);

	delete [] cmpIndices;

	if (doFreeze)
		_fg->resumeDependentUpdates();
	else
		_fg->postMultipleOperations();
}

int Fg2DScData::comparCmps(const void *element1, const void *element2)
{
	return (*((long *) element1)  < *((long *) element2)) ? -1 :
	      ((*((long *) element1) == *((long *) element2)) ?  0 : 1);
}

int Fg2DScData::comparRcvs(const void *element1, const void *element2)
{
	/*
	 * rcvLineIndex is primary key.
	 * rcvFlagIndex is secondary key.
	 */
	int retval;

	ScPoint *point1 = &(_thisData->_scPoint)[*((int *) element1)];
	ScPoint *point2 = &(_thisData->_scPoint)[*((int *) element2)];

	if      (point1->rcvLineIndex < point2->rcvLineIndex)
	{
		retval = -1;
	}
	else if (point1->rcvLineIndex > point2->rcvLineIndex)
	{
		retval =  1;
	}
	else
	{
		if      (point1->rcvFlagIndex < point2->rcvFlagIndex)
			retval = -1;
		else if (point1->rcvFlagIndex > point2->rcvFlagIndex)
			retval =  1;
		else
			retval =  0;
	}

	return retval;
}

void  Fg2DScData::outputByXlatByIndex(int index, long *line_index,
	long *flag_index, long *shot_index, FgMapToFlag::SkidType *skid_type)
{
	_fg->calculateHeaderWords(_scPoint[index].traceNumber, 0);

	double header58 = _fg->getHeaderWordValue(58);
	double header59 = _fg->getHeaderWordValue(59);

	*line_index = _fg->findNearestLine(header58, header59);
	*flag_index = _fg->findNearestFlagOnLine(*line_index,
		header58, header59);

	/*
	 * Returning line and flag for midpoint, not src or rcv.
	 */
	*shot_index = -1;
	*skid_type = FgMapToFlag::NoSkid;
}

int Fg2DScData::getIndexByGroup(long group, long seqGndPos)
{
	int retval;

	if (_numPts)
	{
		ScPoint *key = new ScPoint;
		key->groupNumber   = group;
		key->gridBinCenter = seqGndPos;

		void *ptr = bsearch((void *) key, (void *) _scPoint,
			(size_t) _numPts, sizeof(ScPoint), scCom);

		if (ptr)
			retval = (int) ((ScPoint *) ptr - _scPoint);
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

int Fg2DScData::scCom(const void *element1, const void *element2)
{
	int retval;

	ScPoint *ptr1 = (ScPoint *) element1;
	ScPoint *ptr2 = (ScPoint *) element2;

	if      (ptr1->gridBinCenter < ptr2->gridBinCenter)
	{
		retval = -1;
	}
	else if (ptr1->gridBinCenter > ptr2->gridBinCenter)
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
