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
#include "ipc/sd_master.hh"

#ifndef XM_H
#include <Xm/Xm.h>
#define XM_H
#endif

#ifndef STDIO_H
#include <stdio.h>
#define STDIO_H
#endif

#ifndef STRING_H
#include <string.h>
#define STRING_H
#endif

#ifndef ASSERT_H
#include <assert.h>
#define ASSERT_H
#endif

SlaveDisplayMaster::SlaveDisplayMaster()
	: SlaveDisplayBase((Widget) NULL, (SlaveDisplayBase **) NULL)
{
	assert(False);
}

SlaveDisplayMaster::SlaveDisplayMaster(char *displayString, Widget w,
	int numPixels, int numPlanes, SlaveDisplayMaster **zeroWhenDestroyed,
	char *title, const char *const slaveApp,
	int left, int right, int top, int bottom)
	: SlaveDisplayBase(w, (SlaveDisplayBase **) zeroWhenDestroyed)
{
	_exitOnDelete = False;

	char *buff = new char[strlen(slaveApp) + strlen(displayString) + 11];
	sprintf(buff, "%s -display %s", slaveApp, displayString);
	_ipc = new ClientMessageMaster(w, this, buff);
	delete [] buff;

	_ipc->setErrorHandler(this);

	_ipc->receive(receivedIntPtr);

	if (strlen(title) > 19)
	{
		char trunTitle[20];
		strncpy(trunTitle, title, 19);
		trunTitle[19] = '\0';
		_ipc->send(trunTitle);
	}
	else
	{
		_ipc->send(title);
	}

	long array[4];
	array[0] = _PIXEL_INFO;
	array[1] = (long) XtWindow(w);
	array[2] = numPixels;
	array[3] = numPlanes;
	_ipc->send(array, 4);

	setBorder(left, right, top, bottom);

	addEventHandlers();
}

SlaveDisplayMaster::~SlaveDisplayMaster()
{
	/* do nothing */
}

void SlaveDisplayMaster::quitMessage()
{
	delete this;
}

void SlaveDisplayMaster::resize(int width, int height)
{
	if (_width != width || _height != height)
	{
		_width  = width ;
		_height = height;

		long array[4];
		array[0] = _SIZE_CHANGE;
		array[1] = _width ;
		array[2] = _height;
		_ipc->send(array, 3);
	}
}

void SlaveDisplayMaster::addPixmap(Pixmap localPixmap)
{
	long array[4];
	array[0] = _ADD_PIXMAP;
	array[1] = (long) localPixmap;
	_ipc->send(array, 2);
}

void SlaveDisplayMaster::deletePixmap(Pixmap localPixmap)
{
	long array[4];
	array[0] = _DELETE_PIXMAP;
	array[1] = (long) localPixmap;
	_ipc->send(array, 2);
}

void SlaveDisplayMaster::displayPixmap(Pixmap localPixmap, unsigned long plane)
{
	long array[4];
	array[0] = _DISPLAY_PIXMAP;
	array[1] = (long) localPixmap;
	array[2] = (long) plane;
	_ipc->send(array, 3);
}

void SlaveDisplayMaster::updateRGBs()
{
	long array[4];
	array[0] = _UPDATE_RGBS;
	_ipc->send(array, 1);
}

void SlaveDisplayMaster::numPixmaps(int num)
{
	long array[4];
	array[0] = _NUM_PIXMAPS;
	array[1] = num;
	_ipc->send(array, 2);
}

void SlaveDisplayMaster::setBorder(int left, int right, int top, int bottom)
{
	long array[4];
	array[0] = _BORDER_INFO;
	array[1] = left * 10000 + right ;
	array[2] = top  * 10000 + bottom;
	_ipc->send(array, 3);
}

void SlaveDisplayMaster::errorHandler(HandlesErrors * /*ptr*/, int errorNum)
{
	switch (errorNum)
	{
		case _ERROR_PARTNER_DIED:
			_otherGuyDied = True;
			delete this;
			break;
		case _ERROR_OUT_OF_COLORS:
		case _ERROR_OUT_OF_PIXMAP_MEMORY:
			break;
		default:
			assert(False);
		break;
	}
}
