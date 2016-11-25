#include <string.h>
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
#include "fgxp/fgxp_data.hh"
#include "fgxp/ll_fgxp_data.hh"
#include "geom/field_geometry.hh"

#include <stdlib.h>
#include <assert.h>

/*
 * Static variables.
 */
int *FgXpData::_markerColors ;
int *FgXpData::_markerIndices;
int  FgXpData::_arraySizes = 0;

FgXpData::FgXpData(FgXpDataLinkedList *list,
	long xIndex, long yIndex, long zIndex, long id)
	: _list(list), _xIndex(xIndex), _yIndex(yIndex), _zIndex(zIndex),
	  _savingLine(0), _depSelActChanged(0), _id(id),
	  _numSkiddedSrcs(0), _numSkiddedRcvs(0)
{
	if (-1 == _yIndex)
		_yIndex = _xIndex;

	if (-1 == _zIndex)
		_zIndex = _yIndex;

	setSkids();
}

FgXpData::~FgXpData()
{
	if (_numSkiddedSrcs)
	{
		free((void *) _skiddedSrcs  );
		free((void *) _skiddedSrcGrp);
	}

	if (_numSkiddedRcvs)
		free((void *) _skiddedRcvs);
}

void FgXpData::getRange(float *xMin, float *xMax, float *yMin, float *yMax,
	float *zMin, float *zMax)
{
	int numPts = getNumPts(_id);
	float value;

	for (int i = 0; i < numPts; i++)
	{
		value = getX(i, _id);

		if (value < *xMin)
			*xMin = value;

		if (value > *xMax)
			*xMax = value;

		value = getY(i, _id);

		if (value < *yMin)
			*yMin = value;

		if (value > *yMax)
			*yMax = value;

		if (zMin || zMax)
		{
			value = getZ(i, _id);

			if (zMin && value < *zMin)
				*zMin = value;

			if (zMax && value > *zMax)
				*zMax = value;
		}
	}

}

int FgXpData::getFirstPnt(float *x, float *y, float *z)
{
	int retval;

	if (getNumPts(_id))
	{
		*x = getX(0, _id);
		*y = getY(0, _id);

		if (z)
			*z = getZ(0, _id);

		retval = 1;
	}
	else
	{
		retval = 0;
	}

	return retval;
}

int FgXpData::getNumPts(long id)
{
	assert(id == _id);

	int retval = getNumFlags();

	retval += 2 * ( _numSkiddedSrcs + _numSkiddedRcvs);

	return retval;
}

int FgXpData::getNumFlags()
{
	int retval;

	int xNumPts = getXYZNumPts(_list->getXCardType(), _xIndex);
	int yNumPts = getXYZNumPts(_list->getYCardType(), _yIndex);
	int zNumPts = getXYZNumPts(_list->getZCardType(), _zIndex);

	retval = xNumPts < yNumPts ? xNumPts : yNumPts;
	retval = retval  < zNumPts ? retval  : zNumPts;

	return retval;
}

int FgXpData::getXYZNumPts(CardType cardType, long index)
{

	int retval;

	switch (cardType)
	{
		case LdCardType:
			retval = (int) _list->getFg()->numFlagsOnLine(index);
			break;
		default:
			assert(0);
	}

	return retval;
}

float FgXpData::getX(int i, long id)
{
	float retval;

	if (_numSkiddedSrcs || _numSkiddedRcvs)
	{
		retval = getSkiddedValue(_list->getXCardType(),
			_list->getXDataType(), _xIndex, i, id);
	}
	else
	{
		retval = getValue       (_list->getXCardType(),
			_list->getXDataType(), _xIndex, i, id);
	}

	return retval;
}

float FgXpData::getY(int i, long id)
{
	float retval;

	if (_numSkiddedSrcs || _numSkiddedRcvs)
	{
		retval = getSkiddedValue(_list->getYCardType(),
			_list->getYDataType(), _yIndex, i, id);
	}
	else
	{
		retval = getValue       (_list->getYCardType(),
			_list->getYDataType(), _yIndex, i, id);
	}

	return retval;
}

float FgXpData::getZ(int i, long id)
{
	/*
	 * Only thing skids have to do with z is they can make
	 * the data index not equal to the flag index.
	 */
	float retval;

	if (_numSkiddedSrcs || _numSkiddedRcvs)
	{
		retval = getSkiddedValue(_list->getZCardType(),
			_list->getZDataType(), _zIndex, i, id);
	}
	else
	{
		retval = getValue       (_list->getZCardType(),
			_list->getZDataType(), _zIndex, i, id);
	}

	return retval;
}

float FgXpData::getValue(CardType cardType, int dataType, long index, int i,
	long id)
{
   assert(id == _id);

   float retval;

   switch (cardType)
   {
      case LdCardType:
         switch (dataType)
         {
            case FG_SHOT:
               retval = _list->getFg()->getShotpoint(index, (long) i);
               break;
            case FG_DIST:
               retval = (float) _list->getFg()->getIncrDistance(
                  index, (long) i);
               break;
            case FG_XLOC:
               retval = (float) _list->getFg()->getXloc(index, (long) i);
               break;
            case FG_YLOC:
               retval = (float) _list->getFg()->getYloc(index, (long) i);
               break;
            case FG_ELEV:
               retval = _list->getFg()->getElevation(index, (long) i);
               break;
            case FG_XGRID:
               retval = (float) _list->getFg()->getXgrid(index, (long) i);
               break;
            case FG_YGRID:
               retval = (float) _list->getFg()->getYgrid(index, (long) i);
               break;
            case FG_HD:
               retval = _list->getFg()->getHoleDepth(index, (long) i);
               break;
            case FG_TUH:
               retval = _list->getFg()->getUpholeTime(index, (long) i);
               break;
            case FG_RSTAT:
               retval = _list->getFg()->getReceiverStatic(index, (long) i);
               break;
            case FG_SSTAT:
               retval = _list->getFg()->getSourceStatic(index, (long) i);
               break;
            case FG_XSKID:
               retval = _list->getFg()->getReceiverXskid(index, (long) i);
               break;
            case FG_YSKID:
               retval = _list->getFg()->getReceiverYskid(index, (long) i);
               break;
            case FG_ESKID:
               retval = _list->getFg()->getReceiverEskid(index, (long) i);
               break;
            case FG_CUM:
               retval = (float) _list->getFg()->getCumDistance(index, (long) i);
               break;
            case FG_AZIM:
               retval = (float) _list->getFg()->getAzimuth(index, (long) i);
               break;
            default:
               assert(0);
         }
         break;
      default:
         assert(0);
   }

   return retval;
}

int FgXpData::getMarkerType(int i, long id)
{
	assert(id == _id);

	int retval;

	int skiddedSrcs = bs(_skiddedSrcs, _numSkiddedSrcs, i);
	int skiddedRcvs = bs(_skiddedRcvs, _numSkiddedRcvs, i);

	/*
	 * markerType:  0 = neither rcv nor src
	 *              1 = rcv
	 *              2 = src
	 *              3 = both rcv and src
	 */
	int markerType  =
		  2 * (  numSourcesAtPoint(i, skiddedSrcs, skiddedRcvs) > 0)
		+     (numReceiversAtPoint(i, skiddedSrcs, skiddedRcvs) > 0);

	switch (_list->getFlagMode())
	{
		case ShowAll:
			switch(markerType)
			{
				case 0:   retval = _list->getNeitherMarker();
					  break;
				case 1:   retval = _list->getRcvMarker    ();
					  break;
				case 2:   retval = _list->getSrcMarker    ();
					  break;
				case 3:   retval = _list->getBothMarker   ();
					  break;
				default:  assert(0);
			}
			break;
		case ShowSrcsOnly:
			switch(markerType)
			{
				case 0:   retval = _list->getNoMarker ();
					  break;
				case 1:   retval = _list->getNoMarker ();
					  break;
				case 2:   retval = _list->getSrcMarker();
					  break;
				case 3:   retval = _list->getSrcMarker();
					  break;
				default:  assert(0);
			}
			break;
		case ShowRcvsOnly:
			switch(markerType)
			{
				case 0:   retval = _list->getNoMarker ();
					  break;
				case 1:   retval = _list->getRcvMarker();
					  break;
				case 2:   retval = _list->getNoMarker ();
					  break;
				case 3:   retval = _list->getRcvMarker();
					  break;
				default:  assert(0);
			}
			break;
		case HideComputed:
			if (numDependenciesAtPoint(translateDataIndexToFlag(i)))
			{
				retval = _list->getNoMarker();
				break;
			}

			switch(markerType)
			{
				case 0:   retval = _list->getNeitherMarker();
					  break;
				case 1:   retval = _list->getRcvMarker    ();
					  break;
				case 2:   retval = _list->getSrcMarker    ();
					  break;
				case 3:   retval = _list->getBothMarker   ();
					  break;
				default:  assert(0);
			}
			break;
		case HideUnassigned:
			switch(markerType)
			{
				case 0:   retval = _list->getNoMarker  ();
					  break;
				case 1:   retval = _list->getRcvMarker ();
					  break;
				case 2:   retval = _list->getSrcMarker ();
					  break;
				case 3:   retval = _list->getBothMarker();
					  break;
				default:  assert(0);
			}
			break;
		default:
			assert(0);
	}

	return retval;
}

int FgXpData::getAltMarkerColor(int i, long id)
{
	assert(id == _id);

	int retval;
	int flag = translateDataIndexToFlag(i);

	if      (_list->activeDependent   () && numActiveAtPoint      (flag))
		retval = _list->getActiveColor   ();
	else if (_list->selectDependent   () && numSelectedAtPoint    (flag))
		retval = _list->getSelectedColor ();
	else if (_list->dependentDependent() && numDependenciesAtPoint(flag))
		retval = _list->getDependentColor();
	else
		retval = _list->getNormalColor   ();

	return retval;
}

void FgXpData::replacePoint(int index, float x, float y, float /*z*/)
{
	FieldGeometry *fg = _list->getFg();
	fg->preMultipleOperations();

	/*
	 * Need to mess with skids?
	 */
	if (_numSkiddedSrcs || _numSkiddedRcvs)
	{
		int priorSkiddedSrcs = bs(_skiddedSrcs, _numSkiddedSrcs, index);
		int priorSkiddedRcvs = bs(_skiddedRcvs, _numSkiddedRcvs, index);

		if (priorSkiddedSrcs		/* Skidded source? */
			&& _skiddedSrcs[priorSkiddedSrcs - 1] == index - 1)
		{
			/* Do nothing for now. */
		}
		else if (priorSkiddedRcvs	/* Skidded receiver? */
			&& _skiddedRcvs[priorSkiddedRcvs - 1] == index - 1)
		{
			/* Do nothing for now. */
		}
		else
		{
			index -= 2 * (priorSkiddedSrcs + priorSkiddedRcvs);

			setValue(_list->getXCardType(), _list->getXDataType(),
				_xIndex, index, x);

			setValue(_list->getYCardType(), _list->getYDataType(),
				_yIndex, index, y);
		}
	}
	else
	{
		setValue(_list->getXCardType(), _list->getXDataType(), _xIndex,
			index, x);

		setValue(_list->getYCardType(), _list->getYDataType(), _yIndex,
			index, y);
	}

	fg->postMultipleOperations();
}

int FgXpData::translateDataIndexToFlag(int index,
   long **srcIndicesRet, int *numSrcsRet)
{
   if (srcIndicesRet)
      assert(numSrcsRet);

   if (numSrcsRet)
      assert(srcIndicesRet);

   int retval;
   FieldGeometry *fg = _list->getFg();

   /*
    * Are there skids?
    */
   if (_numSkiddedSrcs || _numSkiddedRcvs)
   {
      int priorSkiddedSrcs = bs(_skiddedSrcs, _numSkiddedSrcs, index);
      int priorSkiddedRcvs = bs(_skiddedRcvs, _numSkiddedRcvs, index);

      retval = index - 2 * (priorSkiddedSrcs + priorSkiddedRcvs);

      if      (priorSkiddedSrcs &&   /* Skidded source? */
         _skiddedSrcs[priorSkiddedSrcs - 1] == index - 1)
      {
         retval++;

         if (srcIndicesRet)
         {
            /*
             * 1st get src index of this src.
             */
            long src;
            long numSrcs = fg->numSourcesAtFlag(_xIndex, (long) retval);
            *srcIndicesRet = new long[numSrcs];

            for (src = 0; src < numSrcs; src++)
            {
               if (_skiddedSrcGrp[priorSkiddedSrcs - 1] == (int)
                  fg->sourceGroupNumber(_xIndex, (long) retval, src))
               {
                  break;
               }
            }

            assert(src < numSrcs);
            (*srcIndicesRet)[0] = src;
             *numSrcsRet = 1;

            double x, y, x2, y2;
            fg->getSkiddedSourceCoords(
               (long) _skiddedSrcGrp[priorSkiddedSrcs - 1], &x, &y);

            /*
             * Next see if any other srcs at this flag skidded to
             * exact same point.
             */
            for (src++;
               src < numSrcs && priorSkiddedSrcs < _numSkiddedSrcs &&
                  _skiddedSrcs[priorSkiddedSrcs - 1] + 2
                     == _skiddedSrcs[priorSkiddedSrcs];
               priorSkiddedSrcs++,  src++)
            {
               fg->getSkiddedSourceCoords(
                  (long) _skiddedSrcGrp[priorSkiddedSrcs], &x2, &y2);

               if (x == x2 && y == y2)
                  (*srcIndicesRet)[(*numSrcsRet)++] = src;
            }
         }
      }
      else if (priorSkiddedRcvs &&   /* Skidded receiver? */
         _skiddedRcvs[priorSkiddedRcvs - 1] == index - 1)
      {
         retval++;

         if (srcIndicesRet)
         {
            *srcIndicesRet = (long *) 0;
            *numSrcsRet    =          0;
         }
      }
      else   /* Not a skidded point. */
      {
         if (srcIndicesRet)
         {
            long numSrcs = fg->numSourcesAtFlag(_xIndex, (long) retval);

            if (numSrcs)
            {
              *srcIndicesRet = new long[numSrcs];
              *numSrcsRet    = 0;
  
              for (long src = 0; src < numSrcs; src++)
                 if (!fg->sourceIsSkidded(_xIndex, (long) retval, src))
                    (*srcIndicesRet)[(*numSrcsRet)++] = src;
  
              if (0 == *numSrcsRet)
              {
                 delete [] *srcIndicesRet;
                 *srcIndicesRet = (long *) 0;
              }
            }
            else
            {
              *srcIndicesRet = (long *) 0;
              *numSrcsRet    =          0;
            }
         }
      }
   }
   /*
    * No skids to worry about.
    */
   else
   {
      retval = index;

      if (srcIndicesRet)
      {
         long numSrcs = fg->numSourcesAtFlag(_xIndex, (long) retval);

         if (numSrcs)
         {
            *srcIndicesRet = new long[numSrcs];
            *numSrcsRet    = 0;

            for (long src = 0; src < numSrcs; src++)
               (*srcIndicesRet)[(*numSrcsRet)++] = src;
         }
         else
         {
            *srcIndicesRet = (long *) 0;
            *numSrcsRet    =          0;
         }
      }
   }

   return retval;
}

void FgXpData::setValue(CardType cardType, int dataType, long index, int i,
	float value)
{
   switch (cardType)
   {
      case LdCardType:
         if (_list->getFg()->allowSettingValue(dataType))
         {
            switch (dataType)
            {
               case FG_SHOT:
                  _list->getFg()->setShotpoint(index, (long) i, value);
                  break;
               case FG_DIST:
                  _list->getFg()->setIncrDistance(index, (long) i,
                     (double) value);
                  break;
               case FG_XLOC:
                  _list->getFg()->setXloc(index, (long) i, (double) value);
                  break;
               case FG_YLOC:
                  _list->getFg()->setYloc(index, (long) i, (double) value);
                  break;
               case FG_ELEV:
                  _list->getFg()->setElevation(index, (long) i, value);
                  break;
               case FG_XGRID:
                  _list->getFg()->setXgrid(index, (long) i, (double) value);
                  break;
               case FG_YGRID:
                   _list->getFg()->setYgrid(index, (long) i, (double) value);
                  break;
               case FG_HD:
                  _list->getFg()->setHoleDepth(index, (long) i, value);
                  break;
               case FG_TUH:
                  _list->getFg()->setUpholeTime(index, (long) i, value);
                  break;
               case FG_RSTAT:
                  _list->getFg()->setReceiverStatic(index, (long) i, value);
                  break;
               case FG_SSTAT:
                  _list->getFg()->setSourceStatic(index, (long) i, value);
                  break;
               case FG_XSKID:
                  _list->getFg()->setReceiverXskid(index, (long) i, value);
                  break;
               case FG_YSKID:
                  _list->getFg()->setReceiverYskid(index, (long) i, value);
                  break;
               case FG_ESKID:
                  _list->getFg()->setReceiverEskid(index, (long) i, value);
                  break;
               default:
                  assert(0);
            }
         }
         break;
      default:
         assert(0);
   }
}

void FgXpData::pointsRemoved(int index, int num)
{
	/*
	 * Extend one on each side of range if there are skids,
	 * because a skidded point from an adjacent flag can
	 * be moved if this edit changes the slope thru it.
	 */
	if (hasSkids())
	{
		if (index > 0)
		{
			index--;
			num++;
		}

		if (index + num < getNumFlags())
			num++;
	}

	int dataIndex, numDataIndices;
	flagToDataIndex(index, num, &dataIndex, &numDataIndices);

	modIndicesBefore(dataIndex, numDataIndices, _id);
}

void FgXpData::pointsInserted(int index, int num)
{
	/*
	 * Extend one on each side of range if there are skids,
	 * because a skidded point from an adjacent flag can
	 * be moved if this edit changes the slope thru it.
	 */
	if (hasSkids())
	{
		if (index > 0)
		{
			index--;
			num++;
		}

		if (index + num < getNumFlags())
			num++;
	}

	int dataIndex, numDataIndices;
	flagToDataIndex(index, num, &dataIndex, &numDataIndices);

	modIndicesAfter(dataIndex, numDataIndices, _id);
	modDone(_id);
}

void FgXpData::srcRcvChanged(int index, int num)
{
	int dataIndex, numDataIndices;
	flagToDataIndex(index, num, &dataIndex, &numDataIndices);

	modAttributesNeedingRepair(dataIndex, numDataIndices, _id);
}

void FgXpData::depSelActChanged(int index, int num)
{
	int dataIndex, numDataIndices;
	flagToDataIndex(index, num, &dataIndex, &numDataIndices);

	if (numDataIndices > 0)
	{
		if (_depSelActChanged)
		{
			if (dataIndex < _depSelActIndex)
			{
				_depSelActNum  += _depSelActIndex - dataIndex;
				_depSelActIndex = dataIndex;
			}

			if (dataIndex + numDataIndices
				> _depSelActIndex + _depSelActNum)
			{
				_depSelActNum = dataIndex + numDataIndices
					- _depSelActIndex;
			}
		}
		else
		{
			_depSelActChanged = 1             ;
			_depSelActIndex   = dataIndex     ;
			_depSelActNum     = numDataIndices;
		}
	}
}

void FgXpData::depSelActFlush()
{
	if (_depSelActChanged)
	{
		int numPts = getNumPts(_id);

		/*
		 * If crossplotting data from 2 (or 3) different lines,
		 * changed flag may be past end of shortest line.
		 */
		if (_depSelActIndex < numPts)
		{
			if (_depSelActIndex + _depSelActNum > numPts)
				_depSelActNum = numPts - _depSelActIndex;

			/* 1 is for ignore hold. */
			modAttributes(_depSelActIndex, _depSelActNum, 1, _id);
			_depSelActChanged = 0;
		}
	}
}

void FgXpData::forgetDepSelActChanged()
{
	_depSelActChanged = 0;
}

void FgXpData::saveLineNumber()
{
	assert(!_savingLine);

	FieldGeometry *fg = _list->getFg();

	_saveXLine = fg->getLineNumber(_xIndex);
	_saveYLine = fg->getLineNumber(_yIndex);
	_saveZLine = fg->getLineNumber(_zIndex);

	_savingLine = 1;
}

int FgXpData::restoreLineNumber()
{
	assert(_savingLine);

	FieldGeometry *fg = _list->getFg();

	_xIndex = fg->findMatchingLineNumber(_saveXLine);
	_yIndex = fg->findMatchingLineNumber(_saveYLine);
	_zIndex = fg->findMatchingLineNumber(_saveZLine);

	_savingLine = 0;

	return (-1 != _xIndex && -1 != _yIndex && -1 != _zIndex);
}

int FgXpData::numReceiversAtPoint(int i,
	int priorSkiddedSrcs, int priorSkiddedRcvs)
{
	int retval;
	FieldGeometry *fg = _list->getFg();

	/*
	 * Need to mess with skids?
	 */
	if (_numSkiddedSrcs || _numSkiddedRcvs)
	{
		if (priorSkiddedRcvs		/* Skidded receiver? */
			&& _skiddedRcvs[priorSkiddedRcvs - 1] == i - 1)
		{
			retval = 1;
		}
		else if (priorSkiddedSrcs	/* Skidded source? */
			&& _skiddedSrcs[priorSkiddedSrcs - 1] == i - 1)
		{
			retval = 0;
		}
		else
		{
			i -= 2 * (priorSkiddedSrcs + priorSkiddedRcvs);

			if ( fg->numReceiversAtFlag(_xIndex, (long) i)
			 && !fg->receiverIsSkidded (_xIndex, (long) i))
			{
				retval = 1;
			}
			else
			{
				retval = 0;
			}
		}
	}
	else
	{
		/*
		 * 0 != stuff makes sure return from flagHasReceiver is 0 or 1.
		 */
		retval = (0 != fg->flagHasReceiver(_xIndex, (long) i));

		if (_yIndex != _xIndex)
			retval += (0 != fg->flagHasReceiver(_yIndex, (long) i));

		if (_zIndex != _yIndex && _zIndex != _xIndex)
			retval += (0 != fg->flagHasReceiver(_zIndex, (long) i));
	}

	return retval;
}

int FgXpData::numSourcesAtPoint(int i,
	int priorSkiddedSrcs, int priorSkiddedRcvs)
{
	int retval;
	FieldGeometry *fg = _list->getFg();

	/*
	 * Need to mess with skids?
	 */
	if (_numSkiddedSrcs || _numSkiddedRcvs)
	{
		if (priorSkiddedSrcs		/* Skidded source? */
			&& _skiddedSrcs[priorSkiddedSrcs - 1] == i - 1)
		{
			retval = 1;
		}
		else if (priorSkiddedRcvs	/* Skidded receiver? */
			&& _skiddedRcvs[priorSkiddedRcvs - 1] == i - 1)
		{
			retval = 0;
		}
		else
		{
			i -= 2 * (priorSkiddedSrcs + priorSkiddedRcvs);

			int src;
			int numSrcs = (int) fg->numSourcesAtFlag(
				_xIndex, (long) i);

			for (retval = numSrcs, src = 0; src < numSrcs; src++)
				if (fg->sourceIsSkidded(_xIndex,
					(long) i, (long) src))
				{
					retval--;
				}
		}
	}
	else
	{
		retval = (int) fg->numSourcesAtFlag(_xIndex, (long) i);

		if (_yIndex != _xIndex)
			retval += (int) fg->numSourcesAtFlag(_yIndex, (long) i);

		if (_zIndex != _yIndex && _zIndex != _xIndex)
			retval += (int) fg->numSourcesAtFlag(_zIndex, (long) i);
	}

	return retval;
}

int FgXpData::numDependenciesAtPoint(int i)
{
	/*
	 * 0 != stuff makes sure return from flagValueIsDependent is 0 or 1.
	 */
	int retval = 0;

	if (_list->getXDataType() != FG_NONE)
		retval += (0 != _list->getFg()->flagValueIsDependent(
			_xIndex, (long) i, _list->getXDataType()));

	if (_list->getYDataType() != FG_NONE)
		retval += (0 != _list->getFg()->flagValueIsDependent(
			_yIndex, (long) i, _list->getYDataType()));

	if (_list->getZDataType() != FG_NONE)
		retval += (0 != _list->getFg()->flagValueIsDependent(
			_zIndex, (long) i, _list->getZDataType()));

	return retval;
}

int FgXpData::numSelectedAtPoint(int i)
{
	/*
	 * 0 != stuff makes sure return from flagIsSelected is 0 or 1.
	 */
	int retval = (0 != _list->getFg()->flagIsSelected(_xIndex, (long) i));

	if (_yIndex != _xIndex)
		retval += (0 != _list->getFg()->flagIsSelected(
			_yIndex, (long) i));

	if (_zIndex != _yIndex && _zIndex != _xIndex)
		retval += (0 != _list->getFg()->flagIsSelected(
			_zIndex, (long) i));

	return retval;
}

int FgXpData::numActiveAtPoint(int i)
{
	int retval = (_list->getFg()->getActiveFlagIndexOnLine(_xIndex) == i);

	if (_yIndex != _xIndex)
		retval += (_list->getFg()->
			getActiveFlagIndexOnLine(_yIndex) == i);

	if (_zIndex != _yIndex && _zIndex != _xIndex)
		retval += (_list->getFg()->
			getActiveFlagIndexOnLine(_zIndex) == i);

	return retval;
}

int FgXpData::numReceiversAtLine()
{
	FieldGeometry *fg = _list->getFg();
	int retval = (int) fg->numReceiversOnLine(_xIndex);

	if (_yIndex != _xIndex)
		retval += (int) fg->numReceiversOnLine(_yIndex);

	if (_zIndex != _yIndex && _zIndex != _xIndex)
		retval += (int) fg->numReceiversOnLine(_zIndex);

	return retval;
}

int FgXpData::numSourcesAtLine()
{
	FieldGeometry *fg = _list->getFg();
	int retval = (int) fg->numSourcesOnLine(_xIndex);

	if (_yIndex != _xIndex)
		retval += (int) fg->numSourcesOnLine(_yIndex);

	if (_zIndex != _yIndex && _zIndex != _xIndex)
		retval += (int) fg->numSourcesOnLine(_zIndex);

	return retval;
}

int FgXpData::numActiveAtLine()
{
	int retval = (_list->getFg()->getActiveLineIndex() == _xIndex);

	if (_yIndex != _xIndex)
		retval += (_list->getFg()->getActiveLineIndex() == _yIndex);

	if (_zIndex != _yIndex && _zIndex != _xIndex)
		retval += (_list->getFg()->getActiveLineIndex() == _zIndex);

	return retval;
}

int FgXpData::numSelectedAtLine()
{
	int retval = (_list->getFg()->lineIsSelected(_xIndex) != 0);

	if (_yIndex != _xIndex)
		retval += (_list->getFg()->lineIsSelected(_yIndex) != 0);

	if (_zIndex != _yIndex && _zIndex != _xIndex)
		retval += (_list->getFg()->lineIsSelected(_zIndex) != 0);

	return retval;
}

void FgXpData::selectPoint(int i)
{
	FieldGeometry *fg = _list->getFg();
	int flag = translateDataIndexToFlag(i);

	fg->setFlagSelectValue(_xIndex, (long) flag, 'Y');

	if (_yIndex != _xIndex)
		fg->setFlagSelectValue(_yIndex, (long) flag, 'Y');

	if (_zIndex != _yIndex && _zIndex != _xIndex)
		fg->setFlagSelectValue(_zIndex, (long) flag, 'Y');
}

void FgXpData::activatePoint(int i)
{
	FieldGeometry *fg = _list->getFg();
	int flag = translateDataIndexToFlag(i);

	fg->setActiveFlagIndexOnLine(_xIndex, (long) flag);

	if (_yIndex != _xIndex)
		fg->setActiveFlagIndexOnLine(_yIndex, (long) flag);

	if (_zIndex != _yIndex && _zIndex != _xIndex)
		fg->setActiveFlagIndexOnLine(_zIndex, (long) flag);
}

void FgXpData::storeMarkerColors()
{
	int num = getNumPts(_id);

	checkArraySizes(num);

	for (int i = 0; i < num; i++)
		_markerColors[i] = getAltMarkerColor(i, _id);
}

void FgXpData::updateMarkerColors()
{
	int num = getNumPts(_id);
	int numChanged, i;

	for (numChanged = i = 0; i < num; i++)
		if (_markerColors[i] != getAltMarkerColor(i, _id))
			_markerIndices[numChanged++] = i;

	if (numChanged)
	{
		/* 0 is to not ignore hold. */
		modAttributesByIndices(_markerIndices, numChanged, 0, _id);
	}
}

void FgXpData::checkArraySizes(int num)
{
	if (_arraySizes < num);
	{
		_arraySizes = num;

		if (_arraySizes)
		{
			assert(_markerColors  = (int *) realloc( _markerColors ,
				(size_t) _arraySizes * sizeof(int)));

			assert(_markerIndices = (int *) realloc( _markerIndices,
				(size_t) _arraySizes * sizeof(int)));
		}
		else
		{
			assert(_markerColors  = (int *) malloc(
				(size_t) _arraySizes * sizeof(int)));

			assert(_markerIndices = (int *) malloc(
				(size_t) _arraySizes * sizeof(int)));
		}
	}
}

int FgXpData::couldHaveSkids()
{
  return (_xIndex == _yIndex) &&
    ((_list->getXDataType() == FG_XLOC  && _list->getYDataType() == FG_YLOC )
  || (_list->getXDataType() == FG_XGRID && _list->getYDataType() == FG_YGRID));
}

void FgXpData::setSkids()
{
	if (couldHaveSkids())
	{
		FieldGeometry *fg = _list->getFg();
		int numFlags = (int) fg->numFlagsOnLine(_xIndex);

		if (_numSkiddedSrcs)
		{
			assert(_skiddedSrcs   = (int *)
				realloc((void *) _skiddedSrcs  ,
				(size_t) numFlags * sizeof(int)));

			assert(_skiddedSrcGrp = (int *)
				realloc((void *) _skiddedSrcGrp,
				(size_t) numFlags * sizeof(int)));
		}
		else
		{
			assert(_skiddedSrcs   = (int *)
				malloc((size_t) numFlags * sizeof(int)));

			assert(_skiddedSrcGrp = (int *)
				malloc((size_t) numFlags * sizeof(int)));
		}

		if (_numSkiddedRcvs)
			assert(_skiddedRcvs = (int *)
				realloc((void *) _skiddedRcvs,
				(size_t) numFlags * sizeof(int)));
		else
			assert(_skiddedRcvs = (int *)
				malloc((size_t) numFlags * sizeof(int)));

		_numSkiddedSrcs = _numSkiddedRcvs = 0;

		int flag, numSources, i;
		int src_alloc = numFlags;

		for (flag = 0; flag < numFlags; flag++)
		{
			numSources = (int) fg->numSourcesAtFlag(
				_xIndex, (long) flag);

			for (i = 0; i < numSources; i++)
				if (fg->sourceIsSkidded(_xIndex,
					(long) flag, (long) i))
				{
					if (_numSkiddedSrcs == src_alloc)
					{
					  src_alloc += numFlags;

					  assert(_skiddedSrcs   = (int *)
					    realloc((void *) _skiddedSrcs  ,
					    (size_t) src_alloc * sizeof(int)));

					  assert(_skiddedSrcGrp = (int *)
					    realloc((void *) _skiddedSrcGrp,
					    (size_t) src_alloc * sizeof(int)));
					}

					_skiddedSrcs  [_numSkiddedSrcs] = flag
						+ 2 * (_numSkiddedSrcs
						     + _numSkiddedRcvs);

					_skiddedSrcGrp[_numSkiddedSrcs] = (int)
						fg->sourceGroupNumber(_xIndex,
						(long) flag, (long) i);

					_numSkiddedSrcs++;
				}

			if (fg->numReceiversAtFlag(_xIndex, (long) flag)
			 && fg->receiverIsSkidded (_xIndex, (long) flag))
			{
				_skiddedRcvs[_numSkiddedRcvs] = flag +
					2 * (_numSkiddedSrcs + _numSkiddedRcvs);
				_numSkiddedRcvs++;
			}
		}

		if (_numSkiddedSrcs)
		{
			assert(_skiddedSrcs   = (int *)
				realloc((void *) _skiddedSrcs  ,
				(size_t) _numSkiddedSrcs * sizeof(int)));

			assert(_skiddedSrcGrp = (int *)
				realloc((void *) _skiddedSrcGrp,
				(size_t) _numSkiddedSrcs * sizeof(int)));
		}
		else
		{
			free((void *) _skiddedSrcs  );
			free((void *) _skiddedSrcGrp);
		}

		if (_numSkiddedRcvs)
			assert(_skiddedRcvs = (int *)
				realloc((void *) _skiddedRcvs,
				(size_t) _numSkiddedRcvs * sizeof(int)));
		else
			free((void *) _skiddedRcvs);
	}
}

float FgXpData::getSkiddedValue(CardType cardType, int dataType, long index,
	int i, long id)
{
	float retval;

	if (dataType == FG_XLOC || dataType == FG_XGRID) 
	{
		int priorSkiddedSrcs = bs(_skiddedSrcs, _numSkiddedSrcs, i);
		int priorSkiddedRcvs = bs(_skiddedRcvs, _numSkiddedRcvs, i);

		switch (2 * (0 != priorSkiddedRcvs) + (0 != priorSkiddedSrcs))
		{
			case 0:		/* No prior skids. */
				retval = getValue(cardType,
					dataType, index, i, id);

				if (dataType == FG_XLOC)
					_skiddedY = getValue(cardType, FG_YLOC ,
						index, i, id);
				else
					_skiddedY = getValue(cardType, FG_YGRID,
						index, i, id);

				if (FG_NONE != _list->getZDataType())
					_skiddedZ = getValue(
						_list->getZCardType(),
						_list->getZDataType(),
						_zIndex, i, id);
				break;
			case 1:		/* Prior source skids. */
				retval = getValueWithPriorSrcSkids(
					priorSkiddedSrcs, cardType,
					dataType, index, i, id);
				break;
			case 2:		/* Prior receiver skids. */
				retval = getValueWithPriorRcvSkids(
					priorSkiddedRcvs, cardType,
					dataType, index, i, id);
				break;
			case 3:		/* Prior source and receiver skids. */
				retval = getValueWithBothPriorSkids(
					priorSkiddedSrcs, priorSkiddedRcvs,
					cardType, dataType, index, i, id);
				break;
			default:
				assert(0);
		}
	}
	else if (dataType == FG_YLOC || dataType == FG_YGRID) 
	{
		/*
		 * Assumes getX was always called before getY.
		 */
		retval = _skiddedY;
	}
	else	/* Must be z. */
	{
		/*
		 * Assumes getX was always called before getZ.
		 */
		retval = _skiddedZ;
	}

	return retval;
}

float FgXpData::getValueWithPriorSrcSkids(int priorSkiddedSrcs,
	CardType cardType, int dataType, long index, int i, long id)
{
	float retval;

	/*
	 * Is this a skidded source point?
	 */
	if (_skiddedSrcs[priorSkiddedSrcs - 1] == i - 1)
	{
		assert(cardType == LdCardType && index == _xIndex);

		int flag = i - 2 * priorSkiddedSrcs + 1;
		FieldGeometry *fg = _list->getFg();
		double x, y;

		fg->getSkiddedSourceCoords(
			(long) _skiddedSrcGrp[priorSkiddedSrcs - 1],
			&x, &y);

		if (dataType == FG_XLOC)
		{
			retval    = (float) x;
			_skiddedY = (float) y;
		}
		else if (dataType == FG_XGRID)
		{
			retval    = (float) fg->getXgridCoord(x, y);
			_skiddedY = (float) fg->getYgridCoord(x, y);
		}
		else
		{
			assert(0);
		}

		if (FG_NONE != _list->getZDataType())
			_skiddedZ = getValue(_list->getZCardType(),
				_list->getZDataType(), _zIndex, flag, id);
	}
	else
	{
		retval = getUnskiddedValueWithPriorSkids(priorSkiddedSrcs,
			cardType, dataType, index, i, id);
	}

	return retval;
}

float FgXpData::getValueWithPriorRcvSkids(int priorSkiddedRcvs,
	CardType cardType, int dataType, long index, int i, long id)
{
	float retval;

	/*
	 * Is this a skidded receiver point?
	 */
	if (_skiddedRcvs[priorSkiddedRcvs - 1] == i - 1)
	{
		assert(cardType == LdCardType && index == _xIndex);

		int flag = i - 2 * priorSkiddedRcvs + 1;
		FieldGeometry *fg = _list->getFg();
		double x, y;

		fg->getSkiddedReceiverCoords(_xIndex, (long) flag, &x, &y);

		if (dataType == FG_XLOC)
		{
			retval    = (float) x;
			_skiddedY = (float) y;
		}
		else if (dataType == FG_XGRID)
		{
			retval    = (float) fg->getXgridCoord(x, y);
			_skiddedY = (float) fg->getYgridCoord(x, y);
		}
		else
		{
			assert(0);
		}

		if (FG_NONE != _list->getZDataType())
			_skiddedZ = getValue(_list->getZCardType(),
				_list->getZDataType(), _zIndex, flag, id);
	}
	else
	{
		retval = getUnskiddedValueWithPriorSkids(priorSkiddedRcvs,
			cardType, dataType, index, i, id);
	}

	return retval;
}

float FgXpData::getValueWithBothPriorSkids(
	int priorSkiddedSrcs, int priorSkiddedRcvs,
	CardType cardType, int dataType, long index, int i, long id)
{
	float retval;
	FieldGeometry *fg = _list->getFg();
	double x, y;

	/*
	 * Is this a skidded source point?
	 */
	if (_skiddedSrcs[priorSkiddedSrcs - 1] == i - 1)
	{
		assert(cardType == LdCardType && index == _xIndex);

		int flag = i - 2 * (priorSkiddedSrcs + priorSkiddedRcvs) + 1;

		fg->getSkiddedSourceCoords(
			(long) _skiddedSrcGrp[priorSkiddedSrcs - 1],
			&x, &y);

		if (dataType == FG_XLOC)
		{
			retval    = (float) x;
			_skiddedY = (float) y;
		}
		else if (dataType == FG_XGRID)
		{
			retval    = (float) fg->getXgridCoord(x, y);
			_skiddedY = (float) fg->getYgridCoord(x, y);
		}
		else
		{
			assert(0);
		}

		if (FG_NONE != _list->getZDataType())
			_skiddedZ = getValue(_list->getZCardType(),
				_list->getZDataType(), _zIndex, flag, id);
	}
	/*
	 * Is this a skidded receiver point?
	 */
	else if (_skiddedRcvs[priorSkiddedRcvs - 1] == i - 1)
	{
		assert(cardType == LdCardType && index == _xIndex);

		int flag = i - 2 * (priorSkiddedSrcs + priorSkiddedRcvs) + 1;

		fg->getSkiddedReceiverCoords(_xIndex, (long) flag, &x, &y);

		if (dataType == FG_XLOC)
		{
			retval    = (float) x;
			_skiddedY = (float) y;
		}
		else if (dataType == FG_XGRID)
		{
			retval    = (float) fg->getXgridCoord(x, y);
			_skiddedY = (float) fg->getYgridCoord(x, y);
		}
		else
		{
			assert(0);
		}

		if (FG_NONE != _list->getZDataType())
			_skiddedZ = getValue(_list->getZCardType(),
				_list->getZDataType(), _zIndex, flag, id);
	}
	else
	{
		retval = getUnskiddedValueWithPriorSkids(
			priorSkiddedSrcs + priorSkiddedRcvs,
			cardType, dataType, index, i, id);
	}

	return retval;
}

float FgXpData::getUnskiddedValueWithPriorSkids(int priorSkids,
	CardType cardType, int dataType, long index, int i, long id)
{
	float retval;

	int flag = i - 2 * priorSkids;

	retval = getValue(cardType, dataType, index, flag, id);

	if      (dataType == FG_XLOC)
		_skiddedY = getValue(cardType, FG_YLOC , index, flag, id);
	else if (dataType == FG_XGRID)
		_skiddedY = getValue(cardType, FG_YGRID, index, flag, id);
	else
		assert(0);

	if (FG_NONE != _list->getZDataType())
		_skiddedZ = getValue(_list->getZCardType(),
			_list->getZDataType(), _zIndex, flag, id);
	return retval;
}

/*
 * Binary search with a twist.
 * Assumes array is sorted with no values repeated.
 * Returns index of value in array.
 * If value is not found in array, returns index of where value
 * would be inserted.
 * If value < 1st  element, returns 0.
 * If value > last element, returns num.
 * If array is empty returns 0.
 */
int FgXpData::bs(int *array, int num, int value)
{
	assert(num >= 0);

	int retval;

	if (num)
	{
		int lower, upper, mid;

		for (lower = 0, upper = num - 1; lower <= upper;)
		{
			mid = (lower + upper) / 2;

			if      (array[mid] < value)
				lower = mid + 1;
			else if (array[mid] > value)
				upper = mid - 1;
			else
				break;
		}

		retval = (lower > mid) ? mid + 1 : mid;
	}
	else
	{
		retval = 0;
	}

	return retval;
}

void FgXpData::flagToDataIndex(int flagIndex, int numFlagIndices,
	int *dataIndex, int *numDataIndices)
{
	*dataIndex      = flagIndex     ;
	*numDataIndices = numFlagIndices;

	if (_numSkiddedSrcs || _numSkiddedRcvs)
	{
		int srcIndex = 0;
		int rcvIndex = 0;
		int recheckSrcs;

		for (int i = 0; i < numFlagIndices; )
		{
			for (; srcIndex < _numSkiddedSrcs
				&& _skiddedSrcs[srcIndex] <= flagIndex;)
			{
				if (_skiddedSrcs[srcIndex] < flagIndex)
				{
					assert(i == 0);
					*dataIndex      += 2;
				}
				else
				{
					*numDataIndices += 2;
				}

				srcIndex++;
				flagIndex += 2;
			}

			for (recheckSrcs = 0;
				rcvIndex < _numSkiddedRcvs
				&& _skiddedRcvs[rcvIndex] <= flagIndex;)
			{
				if (_skiddedRcvs[rcvIndex] < flagIndex)
				{
					assert(i == 0);
					recheckSrcs = 1;
					*dataIndex      += 2;
				}
				else
				{
					recheckSrcs = 0;
					*numDataIndices += 2;
				}

				rcvIndex++;
				flagIndex += 2;
			}

			if (!recheckSrcs)
			{
				flagIndex++;
				i++;
			}
		}
	}
}

void FgXpData::adjacentDataIndices(int index, int *index1, int *index2)
{
	/*
	 * Any skids?
	 */
	if (_numSkiddedSrcs || _numSkiddedRcvs)
	{
		int priorSkiddedSrcs = bs(_skiddedSrcs, _numSkiddedSrcs, index);
		int priorSkiddedRcvs = bs(_skiddedRcvs, _numSkiddedRcvs, index);

		if (priorSkiddedSrcs		/* Skidded source? */
			&& _skiddedSrcs[priorSkiddedSrcs - 1] == index - 1)
		{
			*index1 = index - 1;
			*index2 = index + 1;
		}
		else if (priorSkiddedRcvs	/* Skidded receiver? */
			&& _skiddedRcvs[priorSkiddedRcvs - 1] == index - 1)
		{
			*index1 = index - 1;
			*index2 = index + 1;
		}
		else
		{
			int flag = translateDataIndexToFlag(index);

			int dataIndex, numDataIndices;
			flagToDataIndex(flag, 1, &dataIndex, &numDataIndices);

			if (0 == dataIndex)
				*index1 = -1;
			else
				*index1 = dataIndex - 1;

			if (getNumPts() == dataIndex + numDataIndices)
				*index2 = -1;
			else
				*index2 = dataIndex + numDataIndices;
		}
	}
	else
	{
		if (0 == index)
			*index1 = -1;
		else
			*index1 = index - 1;

		if (getNumPts() - 1 == index)
			*index2 = -1;
		else
			*index2 = index + 1;
	}
}
