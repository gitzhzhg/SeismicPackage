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
#include "vect/ll_plot_base.hh"
#include "plot/plot_base.hh"
#include <assert.h>

PlotBaseElement::PlotBaseElement(PlotBase *plot)
	: _plot(plot), _useCount(0), _holding(0), _holdRepair(0),
	  _cm((class SLClientMessage *) NULL), _roomForMarkers(1)
{
	/* nothing else */
}

void PlotBaseLinkedList::updateRange(PlotBase *plot,
	int xMin, int xMax, int yMin, int yMax)
{
	PlotBaseElement *ptr;
	assert(ptr = (PlotBaseElement *) BaseLinkedList::find((void *) plot));

	updateRange((void *) ptr, xMin, xMax, yMin, yMax);
}

void PlotBaseLinkedList::updateRange(void *p,
	int xMin, int xMax, int yMin, int yMax)
{
	PlotBaseElement *ptr = (PlotBaseElement *) p;

	if (ptr->_holding)
	{
		if (xMin < ptr->_xMin)	ptr->_xMin = xMin;
		if (xMax > ptr->_xMax)	ptr->_xMax = xMax;
		if (yMin < ptr->_yMin)	ptr->_yMin = yMin;
		if (yMax > ptr->_yMax)	ptr->_yMax = yMax;
	}
	else
	{
		ptr->_xMin = xMin;
		ptr->_xMax = xMax;
		ptr->_yMin = yMin;
		ptr->_yMax = yMax;

		ptr->_holding = 1;
	}
}

int PlotBaseLinkedList::getRange(PlotBase *plot,
	int *xMin, int *xMax, int *yMin, int *yMax)
{
	PlotBaseElement *ptr;
	assert(ptr = (PlotBaseElement *) BaseLinkedList::find((void *) plot));

	return getRange((void *) ptr, xMin, xMax, yMin, yMax);
}

int PlotBaseLinkedList::getRange(void *p,
	int *xMin, int *xMax, int *yMin, int *yMax)
{
	PlotBaseElement *ptr = (PlotBaseElement *) p;

	int retval = ptr->_holding;

	if (ptr->_holding)
	{
		*xMin = ptr->_xMin;
		*xMax = ptr->_xMax;
		*yMin = ptr->_yMin;
		*yMax = ptr->_yMax;

		ptr->_holding = 0;
	}

	return retval;
}

void PlotBaseLinkedList::addUser(PlotBase *plot)
{
	PlotBaseElement *ptr =
		(PlotBaseElement *) BaseLinkedList::find((void *) plot);

	if (!ptr)
	{
		ptr = new PlotBaseElement(plot);
		BaseLinkedList::add(ptr);
	}

	ptr->_useCount++;
}

void PlotBaseLinkedList::removeUser(PlotBase *plot)
{
	PlotBaseElement *ptr;
	assert(ptr = (PlotBaseElement *) BaseLinkedList::find((void *) plot));

	if (--ptr->_useCount == 0)
	{
		if (ptr->_holding && ptr->_plot->getDrawable())
			(ptr->_plot)->repair(ptr->_xMin, ptr->_yMin,
				ptr->_xMax - ptr->_xMin + 1,
				ptr->_yMax - ptr->_yMin + 1);

		removeByPtr(ptr);
	}
}

void PlotBaseLinkedList::holdRepair(PlotBase *plot)
{
	PlotBaseElement *ptr;
	assert(ptr = (PlotBaseElement *) BaseLinkedList::find((void *) plot));

	ptr->_holdRepair = 1;
}

void PlotBaseLinkedList::releaseRepair(PlotBase *plot)
{
	PlotBaseElement *ptr;
	assert(ptr = (PlotBaseElement *) BaseLinkedList::find((void *) plot));

	ptr->_holdRepair = 0;
}

int PlotBaseLinkedList::repairHeld(PlotBase *plot)
{
	PlotBaseElement *ptr;
	assert(ptr = (PlotBaseElement *) BaseLinkedList::find((void *) plot));

	return ptr->_holdRepair;
}

class SLClientMessage **PlotBaseLinkedList::getCMPtrPtr(PlotBase *plot)
{
	PlotBaseElement *ptr;
	assert(ptr = (PlotBaseElement *) BaseLinkedList::find((void *) plot));

	return &ptr->_cm;
}

void PlotBaseLinkedList::setRoomForMarkers(PlotBase *plot, int roomForMarkers,
	void **p)
{
	PlotBaseElement *ptr;

	if (p && *p != NULL)
	{
		ptr = (PlotBaseElement *) *p;
		assert(ptr->_plot == plot);
	}
	else
	{
		assert(ptr = (PlotBaseElement *)
			BaseLinkedList::find((void *) plot, p));
	}

	ptr->_roomForMarkers = roomForMarkers;
}

int PlotBaseLinkedList::getRoomForMarkers(PlotBase *plot, void **p)
{
	PlotBaseElement *ptr;

	if (p && *p != NULL)
	{
		ptr = (PlotBaseElement *) *p;
		assert(ptr->_plot == plot);
	}
	else
	{
		assert(ptr = (PlotBaseElement *)
			BaseLinkedList::find((void *) plot, p));
	}

	return ptr->_roomForMarkers;
}
