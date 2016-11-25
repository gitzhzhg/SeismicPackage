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
#include "ipc/ll_sd.hh"

#ifndef XM_H
#include <Xm/Xm.h>
#define XM_H
#endif

#ifndef ASSERT_H
#include <assert.h>
#define ASSERT_H
#endif

#ifndef STDIO_H
#include <stdio.h>
#define STDIO_H
#endif

#ifndef STRING_H
#include <string.h>
#define STRING_H
#endif

#ifndef _WPROCDEF_H_
#include "wproc.h"
#define _WPROCDEF_H_
#endif

SlaveDisplayLinkedList::SlaveDisplayLinkedList(Widget widget, int colors,
	int planes, char *title, int left, int right, int top, int bottom)
		: _widget(widget), _colors(colors), _planes(planes),
		  _left(left), _right(right), _top(top), _bottom(bottom),
		  _xorCursor(False)
{
	assert(title);
	assert(_title = new char[strlen(title) + 1]);
	strcpy(_title, title);

	Widget w;
	for (w = _widget; !XtIsComposite(w); w = XtParent(w));
	assert(_errorDialog = new ErrorHandler(w));

	assert(_pixmapIDs = new PixmapIDLinkedList);

	_currentPixmapID  = (Pixmap) NULL;
	_currentOverlayID = (Pixmap) NULL;

	int accessCheck = wpAccess(SlaveDisplayBase::_slaveApp);

	switch (accessCheck)
	{
		case 0:
			/* OK */
			break;
		case 1:
			notifyError(_ERROR_SLAVE_DPY_NOT_FOUND);
			break;
		case 2:
			notifyError(_ERROR_SLAVE_DPY_NOT_EXEC);
			break;
		case 3:
			notifyError(_ERROR_VMS_SPWS_BIN_NOT_SET);
			break;
		default:
			assert(False);
	}
}

SlaveDisplayLinkedList::~SlaveDisplayLinkedList()
{
	delete [] _title;

	delete _errorDialog;

	delete _pixmapIDs;
}

void SlaveDisplayLinkedList::add(char *nodeName, Bool privateColormap)
{
	assert(checkError() != _ERROR_SLAVE_DPY_NOT_FOUND
		&& checkError() != _ERROR_SLAVE_DPY_NOT_EXEC);

	if (wpIsXServer(nodeName))
	{
		SlaveDisplayElement *theElement;
		assert(theElement = new SlaveDisplayElement(nodeName, this,
			privateColormap));

		theElement->setErrorHandler(this);

		BaseLinkedList::add((Element *) theElement);

		if (_xorCursor)
			theElement->_slaveDisplay->setXorCursor(True);

		for (Pixmap pixmapID = _pixmapIDs->topPixmapID();
			pixmapID;
			pixmapID = _pixmapIDs->nextPixmapID())
		{
			theElement->_slaveDisplay->addPixmap(pixmapID);
		}

		if (_currentPixmapID)
			theElement->_slaveDisplay-> displayPixmap(
				_currentPixmapID);

		if (_currentOverlayID)
			theElement->_slaveDisplay-> displayPixmap(
				_currentOverlayID, _currentPlaneMask);
	}
	else
	{
		// Note that _error is not reset on successful add.
		// Should use errorNotify instead of synchronous checking.
		notifyError(_ERROR_BAD_NODE_NAME);
	}
}

void SlaveDisplayLinkedList::addPixmap(Pixmap pixmap)
{
	SlaveDisplayElement *ptr;

	if (_pixmapIDs->find(pixmap))
		for (ptr = top(); ptr; ptr = next())
			ptr->_slaveDisplay->deletePixmap(pixmap);
	else
		_pixmapIDs->add(pixmap);

	for (ptr = top(); ptr; ptr = next())
		ptr->_slaveDisplay->addPixmap(pixmap);
}

void SlaveDisplayLinkedList::deletePixmap(Pixmap pixmap)
{
	if (_pixmapIDs->find(pixmap))
	{
		for (SlaveDisplayElement *ptr = top(); ptr; ptr = next())
			ptr->_slaveDisplay->deletePixmap(pixmap);

		_pixmapIDs->remove(pixmap);
	}
}

void SlaveDisplayLinkedList::displayPixmap(Pixmap pixmap,
	unsigned long planeMask)
{
	for (SlaveDisplayElement *ptr = top(); ptr; ptr = next())
		ptr->_slaveDisplay->displayPixmap(pixmap, planeMask);

	_pixmapIDs->addIfNotFound(pixmap);

	if (planeMask)
	{
		_currentOverlayID = pixmap   ;
		_currentPlaneMask = planeMask;
	}
	else
	{
		_currentPixmapID  = pixmap       ;
		_currentOverlayID = (Pixmap) NULL;
	}
}

void SlaveDisplayLinkedList::updateRGBs()
{
	for (SlaveDisplayElement *ptr = top(); ptr; ptr = next())
		ptr->_slaveDisplay->updateRGBs();
}

void SlaveDisplayLinkedList::numPixmaps(int num)
{
	for (SlaveDisplayElement *ptr = top(); ptr; ptr = next())
		ptr->_slaveDisplay->numPixmaps(num);
}

void SlaveDisplayLinkedList::errorHandler(HandlesErrors *errorPtr, int errorNum)
{
	// Sneaky trick to translate this ptr from HandlesErrors to this
	// ptr for SlaveDisplayElement.
	// Multiple inheritance leads to different this ptrs.
	SlaveDisplayElement *elementPtr =
		((SlaveDisplayElement *) errorPtr)->getThis();

	switch(errorNum)
	{
		case _ERROR_OUT_OF_COLORS:
			if (!elementPtr->_privateColormap)
			{
				add(elementPtr->_node, True);

				removeByPtr((Element *) elementPtr);

				break;	// break is in if
			}
		case _ERROR_NONE:
		case _ERROR_PARTNER_DIED:
		case _ERROR_OUT_OF_PIXMAP_MEMORY:
			char *errorBuff;
			assert(errorBuff = new char[strlen(elementPtr->_node) +
				strlen(errorString[errorNum]) + 2]);
			sprintf(errorBuff, "%s %s", elementPtr->_node,
				errorString[errorNum]);

			if (errorNum == _ERROR_NONE)
				_errorDialog->deliverInformation(errorBuff);
			else
				_errorDialog->deliverError(errorBuff);

			delete [] errorBuff;

			removeByPtr((Element *) elementPtr);

			notifyError(errorNum);

			break;
		default:
			assert(False);
	}
}

void SlaveDisplayLinkedList::setExitOnDelete()
{
	for (SlaveDisplayElement *ptr = top(); ptr; ptr = next())
		ptr->_slaveDisplay->setExitOnDelete();
}

void SlaveDisplayLinkedList::deleteAndExit()
{
	if (count())
	{
		setExitOnDelete();
		delete this;
	}
	else
	{
		delete this;
		exit(0);
	}
}

/*
 * Appended Border to input variables to avoid conflict with 
 * linked list top method.
 */
void SlaveDisplayLinkedList::setBorder(int leftBorder, int rightBorder,
	int topBorder, int bottomBorder)
{
	if (leftBorder != _left || rightBorder != _right
		|| topBorder != _top || bottomBorder != _bottom)
	{
		_left   = leftBorder  ;
		_right  = rightBorder ;
		_top    = topBorder   ;
		_bottom = bottomBorder;

		void *p;
		for (SlaveDisplayElement *ptr = top(&p); ptr; ptr = next(&p))
			ptr->_slaveDisplay->setBorder(_left, _right,
				_top, _bottom);
	}
}

void SlaveDisplayLinkedList::setXorCursor(Bool xorCursor)
{
	if (_xorCursor != xorCursor)
	{
		_xorCursor = xorCursor;

		void *p;
		for (SlaveDisplayElement *ptr = top(&p); ptr; ptr = next(&p))
			ptr->_slaveDisplay->setXorCursor(_xorCursor);
	}
}

void SlaveDisplayLinkedList::clearCursor()
{
	void *p;
	for (SlaveDisplayElement *ptr = top(&p); ptr; ptr = next(&p))
		ptr->_slaveDisplay->clearCursor();
}

void SlaveDisplayLinkedList::refreshCursor()
{
	void *p;
	for (SlaveDisplayElement *ptr = top(&p); ptr; ptr = next(&p))
		ptr->_slaveDisplay->refreshCursor();
}
