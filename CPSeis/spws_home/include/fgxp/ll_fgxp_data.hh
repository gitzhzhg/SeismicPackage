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
#ifndef _LL_FGXP_DATA_HH
#define _LL_FGXP_DATA_HH

#include "oprim/element.hh"
#include "oprim/ll_base.hh"
#include "fgxp/fgxp_data.hh"
#include "geom/fg_constants.hh"

#include <iostream.h>
#include <locale.h>
#include <assert.h>

class FgXpDataElement : public Element
{
	friend class FgXpDataLinkedList;

	private:

		class FgXpData *_fgXpData;

		FgXpDataElement()
			{ assert(0); }
		FgXpDataElement(class FgXpDataLinkedList *list,
			long xIndex, long yIndex = -1, long zIndex = -1,
			long id = FgXpData::defaultId);
		~FgXpDataElement();
		int operator ==(void * const fgXpData) const
			{ return((class FgXpData *) fgXpData == _fgXpData); }
		void print() const
			{ cout << " " << _fgXpData; }
};

class FgXpDataLinkedList : public BaseLinkedList
{
	friend class FgXpData;

	public:

		/*
		 * No defaults for marker types so Vector.hh does not
		 * have to be included.
		 */
		FgXpDataLinkedList(class FieldGeometry *fg,
			int srcMarker    , int rcvMarker , int bothMarker,
			int neitherMarker, int noMarker  ,
			CardType xCardType =LdCardType, int xDataType =FG_XLOC, 
			CardType yCardType =LdCardType, int yDataType =FG_YLOC,
			CardType zCardType =LdCardType, int zDataType =FG_NONE,
                        int normalColor   = 1, int dependentColor = 2,
                        int selectedColor = 3, int activeColor    = 4,
			int activeDependent     = 1, int selectDependent = 1,
			int dependentDependent  = 1, FlagMode = ShowAll);
		~FgXpDataLinkedList()
			{ /* do nothing */ }
		class FgXpData *add(long xIndex, long yIndex = -1,
			long zIndex = -1, long id = FgXpData::defaultId);
		void remove(class FgXpData *fgXpData);
		class FgXpData *find(class FgXpData *fgXpData, void **p = NULL);
		class FgXpData *top    (void **p = NULL);
		class FgXpData *bottom (void **p = NULL);
		class FgXpData *next   (void **p = NULL);
		class FgXpData *prev   (void **p = NULL);
		class FgXpData *current(void **p = NULL);

		CardType getXCardType() const
			{ return _xCardType; }
		CardType getYCardType() const
			{ return _yCardType; }
		CardType getZCardType() const
			{ return _zCardType; }

		int getXDataType() const
			{ return _xDataType; }
		int getYDataType() const
			{ return _yDataType; }
		int getZDataType() const
			{ return _zDataType; }

		int needUpdate(int ident);
		int affectedByGridTransform();

		void setPrecedence(int activeDependent,
			int selectDependent, int dependentDependent);
		void getPrecedence(int *activeDependent,
			int *selectDependent, int *dependentDependent)
		{
			*activeDependent    = _activeDependent   ;
			*selectDependent    = _selectDependent   ;
			*dependentDependent = _dependentDependent;
		}

		void     setFlagMode(FlagMode flagMode);
		FlagMode getFlagMode(                 );

	private:

		class FieldGeometry *_fg;

		class FieldGeometry *getFg()
			{ return _fg; }

		/*
		 * CardTypes and DataTypes are const.  If you think you
		 * need to changed them, you really need to delete
		 * instances and new another one.
		 */
		const CardType _xCardType, _yCardType, _zCardType;

		/*
		 * Data types set with enums from fg_constants.hh
		 */
		const int _xDataType, _yDataType, _zDataType;

		int _srcMarker, _rcvMarker, _bothMarker, _neitherMarker,
			_noMarker;
		int _normalColor, _dependentColor, _selectedColor, _activeColor;

		FlagMode _flagMode;

		int getSrcMarker     () { return _srcMarker     ; }
		int getRcvMarker     () { return _rcvMarker     ; }
		int getBothMarker    () { return _bothMarker    ; }
		int getNeitherMarker () { return _neitherMarker ; }
		int getNoMarker      () { return _noMarker      ; }
		int getNormalColor   () { return _normalColor   ; }
		int getDependentColor() { return _dependentColor; }
		int getSelectedColor () { return _selectedColor ; }
		int getActiveColor   () { return _activeColor   ; }

		/*
		 * From marker color precedence.
		 * Used by FgXpData::getAltMarkerColor.
		 */
		int _activeDependent, _selectDependent, _dependentDependent;
		int activeDependent   () { return _activeDependent   ; }
		int selectDependent   () { return _selectDependent   ; }
		int dependentDependent() { return _dependentDependent; }
};

#endif
