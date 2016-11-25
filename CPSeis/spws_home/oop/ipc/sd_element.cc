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
#include "ipc/sd_element.hh"

#ifndef XM_H
#include <Xm/Xm.h>
#define XM_H
#endif

#ifndef ASSERT_H
#include <assert.h>
#define ASSERT_H
#endif

#ifndef STRING_H
#include <string.h>
#define STRING_H
#endif

SlaveDisplayElement::SlaveDisplayElement(char *node,
	SlaveDisplayLinkedList *linkedList, Bool privateColormap)
{
	assert(node);
	assert(_node = new char[strlen(node) + 1]);
	strcpy(_node, node);

	_privateColormap = privateColormap;

	if (_privateColormap)
	{
		char *slaveApp;
		assert(slaveApp = new char[strlen(SlaveDisplayBase::_slaveApp)
			+ strlen(SlaveDisplayBase::_privateColormap) + 2]);

		strcpy(slaveApp, SlaveDisplayBase::_slaveApp);
		strcat(slaveApp, " ");
		strcat(slaveApp, SlaveDisplayBase::_privateColormap);

		assert(_slaveDisplay = new SlaveDisplayMaster(_node,
			linkedList->_widget, linkedList->_colors,
			linkedList->_planes, &_slaveDisplay,
			linkedList->_title, slaveApp,
			linkedList->_left, linkedList->_right ,
			linkedList->_top , linkedList->_bottom));

		delete [] slaveApp;
	}
	else
	{
		assert(_slaveDisplay = new SlaveDisplayMaster(_node,
			linkedList->_widget, linkedList->_colors,
			linkedList->_planes, &_slaveDisplay,
			linkedList->_title, SlaveDisplayBase::_slaveApp,
			linkedList->_left, linkedList->_right ,
			linkedList->_top , linkedList->_bottom));
	}

	_slaveDisplay->setErrorNotify(this);
}

SlaveDisplayElement::~SlaveDisplayElement()
{
	if (_slaveDisplay)
		delete _slaveDisplay;

	delete [] _node;
}

void SlaveDisplayElement::errorNotify(int errorNum)
{
	assert(!_slaveDisplay);

	switch(errorNum)
	{
		case _ERROR_NONE:
		case _ERROR_PARTNER_DIED:
		case _ERROR_OUT_OF_COLORS:
		case _ERROR_OUT_OF_PIXMAP_MEMORY:
			handleError(errorNum);
			break;
		default:
			assert(False);
	}
}
