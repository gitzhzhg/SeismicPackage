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
#include "fgxp/ll_fgxp_vect.hh"
#include "fgxp/fgxp_data.hh"
#include "oprim/static_utils.hh"

FgXpVectLinkedList::FgXpVectLinkedList(DisplayMode displayMode,
	const char *normalLineColor  , const char *sourceLineColor   ,
	const char *receiverLineColor, const char *srcAndRcvLineColor,
	const char *selectedLineColor, const char *activeLineColor  ,
	const char *normalFlagColor  , const char *dependentFlagColor,
	const char *selectedFlagColor, const char *activeFlagColor   ,
	int normalFlagColorIndex  , int dependentFlagColorIndex,
	int selectedFlagColorIndex, int activeFlagColorIndex   ,
	int activeDependent  , int selectedDependent, int srcAndRcvDependent,
	int receiverDependent, int sourceDependent  ,
	unsigned int width, unsigned int markerSize,
	unsigned int markerLineWidth, const char *label, const char *font)
	: _displayMode(displayMode),
	  _normalLineColor  ((char *) NULL), _sourceLineColor   ((char *) NULL),
	  _receiverLineColor((char *) NULL), _srcAndRcvLineColor((char *) NULL),
	  _activeLineColor  ((char *) NULL), _selectedLineColor ((char *) NULL),
	  _normalFlagColor  ((char *) NULL), _dependentFlagColor((char *) NULL),
	  _selectedFlagColor((char *) NULL), _activeFlagColor   ((char *) NULL),
	  _normalFlagColorIndex   (normalFlagColorIndex   ),
	  _dependentFlagColorIndex(dependentFlagColorIndex),
	  _selectedFlagColorIndex (selectedFlagColorIndex ),
	  _activeFlagColorIndex   (activeFlagColorIndex   ),
	  _activeDependent   (activeDependent   ),
	  _selectedDependent (selectedDependent ),
	  _srcAndRcvDependent(srcAndRcvDependent),
	  _receiverDependent (receiverDependent ),
	  _sourceDependent   (sourceDependent   ),
	  _width(width), _markerSize(markerSize),
	  _markerLineWidth(markerLineWidth),
	  _label((char *) NULL), _font((char *) NULL)
{
	Vector::smartStrcpy(&_normalLineColor   , normalLineColor   );
	Vector::smartStrcpy(&_sourceLineColor   , sourceLineColor   );
	Vector::smartStrcpy(&_receiverLineColor , receiverLineColor );
	Vector::smartStrcpy(&_srcAndRcvLineColor, srcAndRcvLineColor);
	Vector::smartStrcpy(&_selectedLineColor , selectedLineColor );
	Vector::smartStrcpy(&_activeLineColor   , activeLineColor   );

	Vector::smartStrcpy(&_normalFlagColor   , normalFlagColor   );
	Vector::smartStrcpy(&_dependentFlagColor, dependentFlagColor);
	Vector::smartStrcpy(&_selectedFlagColor , selectedFlagColor );
	Vector::smartStrcpy(&_activeFlagColor   , activeFlagColor   );

	Vector::smartStrcpy(&_label, label);
	Vector::smartStrcpy(&_font , font );

	if (_displayMode == LinesAndAutoFlags)
		setAutoMarkers(True );
	else
		setAutoMarkers(False);
}

FgXpVectLinkedList::~FgXpVectLinkedList()
{
	if (_normalLineColor   )
		delete [] _normalLineColor   ;

	if (_sourceLineColor   )
		delete [] _sourceLineColor   ;

	if (_receiverLineColor )
		delete [] _receiverLineColor ;

	if (_srcAndRcvLineColor)
		delete [] _srcAndRcvLineColor;

	if (_selectedLineColor )
		delete [] _selectedLineColor ;

	if (_activeLineColor   )
		delete [] _activeLineColor   ;

	if (_normalFlagColor   )
		delete [] _normalFlagColor   ;

	if (_dependentFlagColor)
		delete [] _dependentFlagColor;

	if (_selectedFlagColor )
		delete [] _selectedFlagColor ;

	if (_activeFlagColor   )
		delete [] _activeFlagColor   ;

	if (_label)
		delete [] _label;

	if (_font )
		delete [] _font ;
}

Vector *FgXpVectLinkedList::add(BaseData *data, Bool waitIfHolding)
{
	const char *colorPtr = getColor((FgXpData *) data->actualData());

	Vector::VectorStyle  style ;
	Vector::VectorMarker marker;

	switch (_displayMode)
	{
		case Lines:
			style  = Vector::SolidLine;
			marker = Vector::NoMarker;
			break;
		case Flags:
			style  = Vector::NoLine;
			marker = Vector::DataSpecifiedMarker;
			break;
		case LinesAndFlags:
		case LinesAndAutoFlags:
			style  = Vector::SolidLine;
			marker = Vector::DataSpecifiedMarker;
			break;
		default:
			assert(False);
	}


	Vector *retval = SeisVectLinkedList::add(data, colorPtr, _width, False,
		style, marker, _markerSize, _markerLineWidth, _label, _font);

	retval->allowAltMarkerColors(True);
	retval->setAltMarkerColor(_normalFlagColorIndex   ,_normalFlagColor   );
	retval->setAltMarkerColor(_dependentFlagColorIndex,_dependentFlagColor);
	retval->setAltMarkerColor(_selectedFlagColorIndex ,_selectedFlagColor );
	retval->setAltMarkerColor(_activeFlagColorIndex   ,_activeFlagColor   );
	retval->makeVisible(waitIfHolding);

	return retval;
}

Bool FgXpVectLinkedList::setColor(Vector *vector, Bool ignoreHold)
{
	return vector->setColor(
		getColor((FgXpData *) vector->getData()->actualData()),
		ignoreHold);
}

const char *FgXpVectLinkedList::getColor(FgXpData *fgXpData)
{
	const char *retval;

	switch (  8 * (fgXpData->numActiveAtLine   () > 0 && _activeDependent)
		+ 4 * (fgXpData->numSelectedAtLine () > 0 && _selectedDependent)
		+ 2 * (fgXpData->numReceiversAtLine() > 0 &&
			(_receiverDependent || _srcAndRcvDependent))
		+     (fgXpData->numSourcesAtLine  () > 0 &&
			(_sourceDependent   || _srcAndRcvDependent)))
	{
		case  0:
			retval = _normalLineColor;
			break;
		case  1:
			if (_sourceDependent)
			{
				retval = _sourceLineColor  ;
			}
			else
			{
				assert(_srcAndRcvDependent);
				retval = _normalLineColor  ;
			}
			break;
		case  2:
			if (_receiverDependent)
			{
				retval = _receiverLineColor;
			}
			else
			{
				assert(_srcAndRcvDependent);
				retval = _normalLineColor  ;
			}
			break;
		case  3:
			if (_srcAndRcvDependent)
			{
				retval = _srcAndRcvLineColor;
			}
			else
			{
				/*
				 * Paper covers rock and receiver beats source.
				 */
				assert(_receiverDependent);
				retval = _receiverLineColor;
			}
			break;
		case  4:
		case  5:
		case  6:
		case  7:
			retval = _selectedLineColor;
			break;
		case  8:
		case  9:
		case 10:
		case 11:
		case 12:
		case 13:
		case 14:
		case 15:
			retval = _activeLineColor;
			break;
		default:
			assert(False);
	}

	return retval;
}

DisplayMode FgXpVectLinkedList::getDisplayMode()
{
	return _displayMode;
}

void FgXpVectLinkedList::setDisplayMode(DisplayMode displayMode)
{
	Vector *ptr;
	void *p;
	Bool holding = SU::isHoldingVectors();

	switch (4 * displayMode + _displayMode)
	{
		case  0:
		case  5:
		case 10:
		case 15:
			/* do nothing, displayMode == _display */
			break;
		case  1:
			/* Flags to Lines */
			if (!holding)
				SU::holdVectors();
			for (ptr = top(&p); ptr; ptr = next(&p))
			{
				ptr->setStyle (Vector::SolidLine);
				ptr->setMarker(Vector::NoMarker ,
					_markerSize, _markerLineWidth);
			}
			if (!holding)
				SU::flushVectors();
			_displayMode = displayMode;
			break;
		case  2:
			/* LinesAndFlags to Lines */
			if (!holding)
				SU::holdVectors();
			for (ptr = top(&p); ptr; ptr = next(&p))
				ptr->setMarker(Vector::NoMarker ,
					_markerSize, _markerLineWidth);
			if (!holding)
				SU::flushVectors();
			_displayMode = displayMode;
			break;
		case  3:
			/* LinesAndAutoFlags to Lines */
			/*
			 * When not room for markers, this will cause
			 * an unnecessary erase and redraw.
			 */
			if (!holding)
				SU::holdVectors();
			for (ptr = top(&p); ptr; ptr = next(&p))
				ptr->setMarker(Vector::NoMarker ,
					_markerSize, _markerLineWidth);
			setAutoMarkers(False);
			if (!holding)
				SU::flushVectors();
			_displayMode = displayMode;
			break;
		case  4:
			/* Lines to Flags */
			if (!holding)
				SU::holdVectors();
			for (ptr = top(&p); ptr; ptr = next(&p))
			{
				ptr->setStyle (Vector::NoLine);
				ptr->setMarker(Vector::DataSpecifiedMarker,
					_markerSize, _markerLineWidth);
			}
			if (!holding)
				SU::flushVectors();
			_displayMode = displayMode;
			break;
		case  6:
			/* LinesAndFlags to Flags */
			if (!holding)
				SU::holdVectors();
			for (ptr = top(&p); ptr; ptr = next(&p))
				ptr->setStyle(Vector::NoLine);
			if (!holding)
				SU::flushVectors();
			_displayMode = displayMode;
			break;
		case  7:
			/* LinesAndAutoFlags to Flags */
			if (!holding)
				SU::holdVectors();
			for (ptr = top(&p); ptr; ptr = next(&p))
				ptr->setStyle(Vector::NoLine);
			setAutoMarkers(False);
			if (!holding)
				SU::flushVectors();
			_displayMode = displayMode;
			break;
		case  8:
			/* Lines to LinesAndFlags */
			for (ptr = top(&p); ptr; ptr = next(&p))
				ptr->setMarker(Vector::DataSpecifiedMarker,
					_markerSize, _markerLineWidth);
			_displayMode = displayMode;
			break;
		case  9:
			/* Flags to LinesAndFlags */
			for (ptr = top(&p); ptr; ptr = next(&p))
				ptr->setStyle(Vector::SolidLine);
			_displayMode = displayMode;
			break;
		case 11:
			/* LinesAndAutoFlags to LinesAndFlags */
			setAutoMarkers(False);
			_displayMode = displayMode;
			break;
		case 12:
			/* Lines to LinesAndAutoFlags */
			/*
			 * When not room for markers, this will cause
			 * an unnecessary erase and redraw.
			 * Order, setAutoMarkers before setMarker, is important.
			 */
			setAutoMarkers(True);
			for (ptr = top(&p); ptr; ptr = next(&p))
				ptr->setMarker(Vector::DataSpecifiedMarker,
					_markerSize, _markerLineWidth);
			_displayMode = displayMode;
			break;
		case 13:
			/* Flags to LinesAndAutoFlags */
			/*
			 * Order, setAutoMarkers before setStyle, is important.
			 */
			setAutoMarkers(True);
			for (ptr = top(&p); ptr; ptr = next(&p))
				ptr->setStyle(Vector::SolidLine);
			_displayMode = displayMode;
			break;
		case 14:
			/* LinesAndFlags to LinesAndAutoFlags */
			setAutoMarkers(True);
			_displayMode = displayMode;
			break;
		default:
			assert(False);
	}
}

void FgXpVectLinkedList::getLineColors(const char **normalLineColor,
	const char **sourceLineColor   , const char **receiverLineColor,
	const char **srcAndRcvLineColor, const char **selectedLineColor,
	const char **activeLineColor   )
{
	*normalLineColor    = _normalLineColor   ;
	*sourceLineColor    = _sourceLineColor   ;
	*receiverLineColor  = _receiverLineColor ;
	*srcAndRcvLineColor = _srcAndRcvLineColor;
	*selectedLineColor  = _selectedLineColor ;
	*activeLineColor    = _activeLineColor   ;
}

void FgXpVectLinkedList::setLineColors( const char *normalLineColor  ,
	const char *sourceLineColor   , const char *receiverLineColor,
	const char *srcAndRcvLineColor, const char *selectedLineColor,
	const char *activeLineColor   )
{
	/*
	 * Do not bother checking what has changed.
	 * That is done is Vector.
	 */
	Vector::smartStrcpy(&_normalLineColor   , normalLineColor   );
	Vector::smartStrcpy(&_sourceLineColor   , sourceLineColor   );
	Vector::smartStrcpy(&_receiverLineColor , receiverLineColor );
	Vector::smartStrcpy(&_srcAndRcvLineColor, srcAndRcvLineColor);
	Vector::smartStrcpy(&_selectedLineColor , selectedLineColor );
	Vector::smartStrcpy(&_activeLineColor   , activeLineColor   );

	Vector *ptr;
	void *p;
	for (ptr = top(&p); ptr; ptr = next(&p))
		ptr->setColor(
			getColor((FgXpData *) ptr->getData()->actualData()));
}

void FgXpVectLinkedList::getFlagColors(const char **normalFlagColor,
	const char **dependentFlagColor, const char **selectedFlagColor,
	const char **activeFlagColor   )
{
	*normalFlagColor    = _normalFlagColor   ;
	*dependentFlagColor = _dependentFlagColor;
	*selectedFlagColor  = _selectedFlagColor ;
	*activeFlagColor    = _activeFlagColor   ;
}

void FgXpVectLinkedList::setFlagColors(const char *normalFlagColor,
	const char *dependentFlagColor, const char *selectedFlagColor,
	const char *activeFlagColor   )
{
	/*
	 * Do not bother checking what has changed.
	 * That is done is Vector.
	 */
	Vector::smartStrcpy(&_normalFlagColor   , normalFlagColor   );
	Vector::smartStrcpy(&_dependentFlagColor, dependentFlagColor);
	Vector::smartStrcpy(&_selectedFlagColor , selectedFlagColor );
	Vector::smartStrcpy(&_activeFlagColor   , activeFlagColor   );

	Vector *ptr;
	void *p;
	for (ptr = top(&p); ptr; ptr = next(&p))
	{
		ptr->setAltMarkerColor(_normalFlagColorIndex   ,
			_normalFlagColor   );
		ptr->setAltMarkerColor(_dependentFlagColorIndex,
			_dependentFlagColor);
		ptr->setAltMarkerColor(_selectedFlagColorIndex ,
			_selectedFlagColor );
		ptr->setAltMarkerColor(_activeFlagColorIndex   ,
			_activeFlagColor   );
	}
}

void FgXpVectLinkedList::getPrecedence(int *activeDependent,
	int *selectedDependent, int *srcAndRcvDependent,
	int *receiverDependent, int *sourceDependent   )
{
	*activeDependent    = _activeDependent   ;
	*selectedDependent  = _selectedDependent ;
	*srcAndRcvDependent = _srcAndRcvDependent;
	*receiverDependent  = _receiverDependent ;
	*sourceDependent    = _sourceDependent   ;
}

void FgXpVectLinkedList::setPrecedence(int activeDependent,
	int selectedDependent, int srcAndRcvDependent,
	int receiverDependent, int sourceDependent   )
{
	/*
	 * Do not bother checking what has changed.
	 * That is done is Vector.
	 */

	_activeDependent    = activeDependent   ;
	_selectedDependent  = selectedDependent ;
	_srcAndRcvDependent = srcAndRcvDependent;
	_receiverDependent  = receiverDependent ;
	_sourceDependent    = sourceDependent   ;

	Vector *ptr;
	void *p;
	for (ptr = top(&p); ptr; ptr = next(&p))
		ptr->setColor(
			getColor((FgXpData *) ptr->getData()->actualData()));
}
