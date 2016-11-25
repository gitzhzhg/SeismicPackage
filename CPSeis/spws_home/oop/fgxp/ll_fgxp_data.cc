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
#include "fgxp/ll_fgxp_data.hh"

FgXpDataElement::FgXpDataElement(class FgXpDataLinkedList *list,
	long xIndex, long yIndex, long zIndex, long id)
{
	_fgXpData = new FgXpData(list, xIndex, yIndex, zIndex, id);
}

FgXpDataElement::~FgXpDataElement()
{
	delete _fgXpData;
}

FgXpDataLinkedList::FgXpDataLinkedList(class FieldGeometry *fg,
	int srcMarker, int rcvMarker, int bothMarker,
	int neitherMarker, int noMarker,
	CardType xCardType, int xDataType, CardType yCardType, int yDataType,
	CardType zCardType, int zDataType,
	int normalColor, int dependentColor, int selectedColor, int activeColor,
	int activeDependent, int selectDependent, int dependentDependent,
	FlagMode flagMode)
	: _fg(fg),
	  _srcMarker (srcMarker ), _rcvMarker    (rcvMarker    ),
	  _bothMarker(bothMarker), _neitherMarker(neitherMarker),
	  _noMarker  (noMarker  ),
	  _xCardType(xCardType), _xDataType(xDataType), _yCardType(yCardType),
	  _yDataType(yDataType), _zCardType(zCardType), _zDataType(zDataType),
	  _normalColor  (normalColor)  , _dependentColor(dependentColor),
	  _selectedColor(selectedColor), _activeColor   (activeColor   ),
	  _activeDependent   (activeDependent   ),
	  _selectDependent   (selectDependent   ),
	  _dependentDependent(dependentDependent),
	  _flagMode          (flagMode          )
{
	/* just initializers */
}

FgXpData *FgXpDataLinkedList::add(long xIndex, long yIndex, long zIndex,
	long id)
{
	FgXpDataElement *theElement = new FgXpDataElement(this,
		xIndex, yIndex, zIndex, id);

	BaseLinkedList::add((Element *) theElement);

	return theElement->_fgXpData;
}

void FgXpDataLinkedList::remove(class FgXpData *fgXpData)
{
	BaseLinkedList::remove((void *) fgXpData);
}

class FgXpData *FgXpDataLinkedList::find(class FgXpData *fgXpData, void **p)
{
	FgXpDataElement *ptr = (FgXpDataElement *)
		BaseLinkedList::find((void *) fgXpData, p);

	return (ptr ? ptr->_fgXpData : (class FgXpData *) NULL);
}

class FgXpData *FgXpDataLinkedList::top    (void **p)
{
	FgXpDataElement *ptr = (FgXpDataElement *) BaseLinkedList::top    (p);

	return (ptr ? ptr->_fgXpData : (class FgXpData *) NULL);
}

class FgXpData *FgXpDataLinkedList::bottom (void **p)
{
	FgXpDataElement *ptr = (FgXpDataElement *) BaseLinkedList::bottom (p);

	return (ptr ? ptr->_fgXpData : (class FgXpData *) NULL);
}

class FgXpData *FgXpDataLinkedList::next   (void **p)
{
	FgXpDataElement *ptr = (FgXpDataElement *) BaseLinkedList::next   (p);

	return (ptr ? ptr->_fgXpData : (class FgXpData *) NULL);
}

class FgXpData *FgXpDataLinkedList::prev   (void **p)
{
	FgXpDataElement *ptr = (FgXpDataElement *) BaseLinkedList::prev   (p);

	return (ptr ? ptr->_fgXpData : (class FgXpData *) NULL);
}

class FgXpData *FgXpDataLinkedList::current(void **p)
{
	FgXpDataElement *ptr = (FgXpDataElement *) BaseLinkedList::current(p);

	return (ptr ? ptr->_fgXpData : (class FgXpData *) NULL);
}

int FgXpDataLinkedList::needUpdate(int ident)
{
	int retval;

	if (_xDataType == ident || _yDataType == ident || _zDataType == ident)
	{
		retval = 1;
	}
	else if (  (_xDataType == FG_XGRID || _xDataType == FG_YGRID
		||  _yDataType == FG_XGRID || _yDataType == FG_YGRID
		||  _zDataType == FG_XGRID || _zDataType == FG_YGRID)
		&& (ident      == FG_XLOC  || ident      == FG_YLOC ))
	{
		retval = 1;
	}
	else
	{
		retval = 0;
	}

	return retval;
}

int FgXpDataLinkedList::affectedByGridTransform()
{
	return	_xDataType == FG_XGRID || _xDataType == FG_YGRID ||
		_yDataType == FG_XGRID || _yDataType == FG_YGRID ||
		_zDataType == FG_XGRID || _zDataType == FG_YGRID;
}

void FgXpDataLinkedList::setPrecedence(int activeDependent,
	int selectDependent, int dependentDependent)
{
	int oldActiveDependent    = _activeDependent   ;
	int oldSelectDependent    = _selectDependent   ;
	int oldDependentDependent = _dependentDependent;

	FgXpData *ptr;
	void *p;
	for (ptr = top(&p); ptr; ptr = next(&p))
	{
		_activeDependent    = oldActiveDependent   ;
		_selectDependent    = oldSelectDependent   ;
		_dependentDependent = oldDependentDependent;

		ptr->storeMarkerColors();

		_activeDependent    = activeDependent   ;
		_selectDependent    = selectDependent   ;
		_dependentDependent = dependentDependent;

		ptr->updateMarkerColors();
	}
}

void FgXpDataLinkedList::setFlagMode(FlagMode flagMode)
{
	_flagMode = flagMode;
}

FlagMode FgXpDataLinkedList::getFlagMode()
{
	return _flagMode;
}
