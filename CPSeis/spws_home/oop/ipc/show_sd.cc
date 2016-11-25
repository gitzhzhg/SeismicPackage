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
#include "ipc/show_sd.hh"

#ifndef XM_H
#include <Xm/Xm.h>
#define XM_H
#endif

#ifndef STDLIB_H
#include <stdlib.h>
#define STDLIB_H
#endif

#ifndef STDIO_H
#include <stdio.h>
#define STDIO_H
#endif

#ifndef WPROC_H
#include "wproc.h"
#define WPROC_H
#endif

typedef struct {ErrorHandler *eh; SlaveDisplayLinkedList *sdll;} errorAckStruct;

ShowSlaveDisplay::ShowSlaveDisplay(Widget widget, char **nodes, int numSlaves,
	char *title) : _widget(widget), _numSlaves(numSlaves)

{
	Widget w;
	for (w = _widget; !XtIsComposite(w); w = XtParent(w));
	assert(_errorDialog = new ErrorHandler(w));

	int i;
	for (i = 0; i < _MAX_PIXMAPS; i++)
		_pixmap[i] = (Pixmap) NULL;

	_currentPixmap = -1;

	makePixmap();

	if (checkError())	// Could I make pixmap?
	{
		_theMasterList = (SlaveDisplayLinkedList *) NULL;
	}
	else
	{
		// 1 extra color in case redisplay from party obscurred window.
		_numColors = colorsOnPixmap() + 1;

		assert(_theMasterList = new SlaveDisplayLinkedList(
			_widget, _numColors, 0, title));

		if (_theMasterList->checkError())  // Could I find slave_dpy?
		{
			handleError(this, _theMasterList->checkError());
			delete _theMasterList;
			_theMasterList = (SlaveDisplayLinkedList *) NULL;
		}
		else
		{
			for (i = 0; i < _numSlaves; i++)
			{
				_theMasterList->add(nodes[i]);

				if (_theMasterList->checkError())
				{
					handleError(this,
						_theMasterList->checkError());

					_numSlaves = i;

					break;
				}
			}

			if (!checkError())
			{
				_theMasterList->
					displayPixmap( _pixmap[_currentPixmap]);

				_theMasterList->setErrorNotify(this);
			}
		}
	}
}

ShowSlaveDisplay::~ShowSlaveDisplay()
{
	for (int i = 0; i < _MAX_PIXMAPS; i++)
		if (_pixmap[i])
			XFreePixmap(XtDisplay(_widget), _pixmap[i]);

	if (_numSlaves && !checkError() && _theMasterList) // Quit on master?
	{
		_theMasterList->setExitOnDelete();
		delete _theMasterList;
		delete _errorDialog;
	}
	else
	{
		char *errorBuff;
		char *node;
		errorAckStruct *errorAckData;
		assert(errorAckData = new errorAckStruct);

		if (_numSlaves && _theMasterList)	// Error on master?
		{
			node = "Master display";
			errorAckData->sdll = _theMasterList;
			errorAckData->eh   = _errorDialog;
		}
		else if(_theMasterList)			// Last slave done?
		{
			node = "Slave display";
			delete _theMasterList;
			errorAckData->sdll =
				(SlaveDisplayLinkedList *) NULL;
			errorAckData->eh = _errorDialog;
		}
		else					// Never got started.
		{
			node = "Master display";
			errorAckData->sdll =
				(SlaveDisplayLinkedList *) NULL;
			errorAckData->eh = _errorDialog;
		}

		assert(errorBuff = new char[strlen(node)
			+ strlen(errorString[checkError()]) + 2]);
		sprintf(errorBuff, "%s %s", node, errorString[checkError()]);

		_errorDialog->setAltErrorAck(errorAck, (void *) errorAckData);

		if (checkError() == _ERROR_NONE)
			_errorDialog->deliverInformation(errorBuff);
		else
			_errorDialog->deliverError(errorBuff);

		delete [] errorBuff;
	}
}

void ShowSlaveDisplay::makePixmap()
{
	Dimension width, height;
	int depth;

	XtVaGetValues(_widget,
		XmNwidth , &width ,
		XmNheight, &height,
		XmNdepth , &depth ,
		NULL);

	_currentPixmap++;
	if (_currentPixmap == _MAX_PIXMAPS)
		_currentPixmap = 0;

	if (_pixmap[_currentPixmap])
	{
		_theMasterList->deletePixmap(_pixmap[_currentPixmap]);
		XFreePixmap(XtDisplay(_widget), _pixmap[_currentPixmap]);
	}

	_pixmap[_currentPixmap] = wprocCreatePixmap(XtDisplay(_widget),
		XtWindow(_widget), (unsigned int) width, (unsigned int) height,
		(unsigned int) depth);

	if (_pixmap[_currentPixmap])
	{
		GC theGC = XCreateGC(XtDisplay(_widget), XtWindow(_widget),
			0, (XGCValues *) NULL);
		XSetSubwindowMode(XtDisplay(_widget), theGC, IncludeInferiors);

		wpCopyArea(XtDisplay(_widget), XtWindow(_widget),
			_pixmap[_currentPixmap], theGC, 0, 0,
			(unsigned int) width, (unsigned int) height, 0, 0);

		XFreeGC(XtDisplay(_widget), theGC);
	}
	else
	{
		handleError(this, _ERROR_OUT_OF_PIXMAP_MEMORY);
	}
}

int ShowSlaveDisplay::colorsOnPixmap()
{
	Window getRoot;
	int getX, getY;
	unsigned int getWidth, getHeight, getBwidth, getDepth;

	XGetGeometry(XtDisplay(_widget), _pixmap[_currentPixmap], &getRoot,
		&getX, &getY, &getWidth, &getHeight, &getBwidth, &getDepth);

	XImage *theImage = XGetImage(XtDisplay(_widget),
		_pixmap[_currentPixmap], 0, 0, getWidth, getHeight,
		AllPlanes, ZPixmap);

	char *ptr;
	int row, numColors;
	const int MAX_COLORS = 256;
	char colors[MAX_COLORS];
	int column, i;

	for (ptr = theImage->data, row = 0, numColors = 0;
		row < theImage->height;
		ptr += theImage->bytes_per_line, row++)
	{
		for (column = 0; column < theImage->width; column++)
		{
			for (i = 0; i < numColors; i++)
				if (colors[i] == ptr[column])
					break;

			if (i == numColors)
				colors[numColors++] = ptr[column];
		}
	}

	XDestroyImage(theImage);

	return(numColors);
}

void ShowSlaveDisplay::errorNotify(int)
{
	if (!--_numSlaves)
		delete this;
	else
		handleError(this, _ERROR_NONE);		// Reset
}

void ShowSlaveDisplay::redisplay()
{
	_theMasterList->clearCursor();
	makePixmap();
	_theMasterList->refreshCursor();

	if (checkError())
	{
		delete this;
	}
	else
	{
		if (_numColors >= colorsOnPixmap())
		{
			_theMasterList->displayPixmap(_pixmap[_currentPixmap]);
		}
		else
		{
			handleError(this, _ERROR_OUT_OF_COLORS);
			delete this;
		}
	}
}

void ShowSlaveDisplay::errorAck(void *client)
{
	SlaveDisplayLinkedList *sdll = ((errorAckStruct *) client)->sdll;

	delete ((errorAckStruct *) client)->eh;
	delete  (errorAckStruct *) client;

	if (sdll)
	{
		sdll->setExitOnDelete();
		delete sdll;
	}
	else
	{
		exit(0);
	}
}
