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
#include <string.h>
#include "vect/ll_plot_info.hh"
#include "plot/plot_base.hh"
#include <assert.h>

PlotInfoElement::PlotInfoElement(PlotBase *plot) :
	_plot(plot),
	_drawn(False),
	_fontStruct((XFontStruct *) NULL),
	_loadFont(True),
	_checkLabel(True),
	_edited(False),
	_display((Display *) NULL),
	/*
	 * Initialize these to avoid purity UMR when variables are used
	 * as fun arguments.
	 */
	_xMin((short) 0), _yMin((short) 0), _xMax((short) 0), _yMax((short) 0),
	_labelLeftOffset(0), _labelRightOffset(0),
	_labelUpOffset  (0), _labelDownOffset (0),
	_labelDrawX((short) 0), _labelDrawY((short) 0)
{
	/* nothing else to do */
}

PlotInfoElement::~PlotInfoElement()
{
	if (_fontStruct)
	{
		assert(_display);
		XFreeFont(_display, _fontStruct);
	}
}

void PlotInfoLinkedList::getInfo(class PlotBase *plot, Bool **drawn,
	short **xMin, short **yMin, short **xMax, short **yMax,
	XFontStruct ***fontStruct, Bool **loadFont, Bool **checkLabel,
	int **labelLeftOffset, int **labelRightOffset,
	int **labelUpOffset, int **labelDownOffset, Bool **edited,
	short **labelDrawX, short **labelDrawY, Display ***display)
{
	// Returning the addresses of private data like this violates
	// the oop paradyne.  But what the heck, just this once.

	PlotInfoElement *ptr;
	assert(ptr = find(plot));

	*drawn              = &ptr->_drawn           ;
	*xMin               = &ptr->_xMin            ;
	*yMin               = &ptr->_yMin            ;
	*xMax               = &ptr->_xMax            ;
	*yMax               = &ptr->_yMax            ;
	*fontStruct         = &ptr->_fontStruct      ;
	*loadFont           = &ptr->_loadFont        ;
	*checkLabel         = &ptr->_checkLabel      ;
	*labelLeftOffset    = &ptr->_labelLeftOffset ;
	*labelRightOffset   = &ptr->_labelRightOffset;
	*labelUpOffset      = &ptr->_labelUpOffset   ;
	*labelDownOffset    = &ptr->_labelDownOffset ;

	if (edited)
		*edited     = &ptr->_edited          ;

	if (labelDrawX)
		*labelDrawX = &ptr->_labelDrawX      ;

	if (labelDrawY)
		*labelDrawY = &ptr->_labelDrawY      ;

	if (display)
		*display    = &ptr->_display         ;
}
