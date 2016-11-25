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
#include "ipc/sd_slave.hh"
#include "ipc/sd_scrwin.hh"

#ifndef XM_H
#include <Xm/Xm.h>
#define XM_H
#endif

#ifndef SHELL_H
#include <X11/Shell.h>
#define SHELL_H
#endif

#ifndef ASSERT_H
#include <assert.h>
#define ASSERT_H
#endif

#ifndef STDLIB_H
#include <stdlib.h>
#define STDLIB_H
#endif

#ifndef STDIO_H
#include <stdio.h>
#define STDIO_H
#endif

#ifndef STRING_H
#include <string.h>
#define STRING_H
#endif

typedef struct { ErrorHandler *eh; IpcQuit *quitter; } errorAckStruct;

SlaveDisplaySlave::SlaveDisplaySlave()
	: SlaveDisplayBase((Widget) NULL, (SlaveDisplayBase **) NULL)
{
	assert(False);
}


SlaveDisplaySlave::SlaveDisplaySlave(Widget w, int *argc, char **argv,
		class SLApp *application, SlaveDisplayScrWin *sdScrWin)
	: SlaveDisplayBase(w, (SlaveDisplayBase **) NULL),
		_application(application), _sdScrWin(sdScrWin)
{
	_exitOnDelete = True;

	assert(_ipc = new ClientMessageSlave(w, this, argc, argv));

	_ipc->setErrorHandler(this);

	_ipc->receive(receivedIntPtr );
	_ipc->receive(receivedCharPtr);

	_pixmaps = (PixmapLinkedList *) NULL;

	addEventHandlers();

	if (_sdScrWin == NULL)
		_borders = False;
	else
		_borders = True ;
}

SlaveDisplaySlave::~SlaveDisplaySlave()
{
	if (_pixmaps)
	{
		delete _pixmaps;
		_pixmaps = (PixmapLinkedList *) NULL;	// reproducible bugs
	}
}

void SlaveDisplaySlave::reDisplay(Widget widget, int xSrc, int ySrc,
	unsigned width, unsigned height, int xDst, int yDst)
{
	if (_pixmaps)
		_pixmaps->reDisplay(widget, xSrc, ySrc, width, height,
			xDst, yDst);
}

void SlaveDisplaySlave::addPixmapMessage(long *message, int num)
{
	assert(message[0] == _ADD_PIXMAP && num == 2);

	_pixmaps->add((Pixmap) message[1]);
}

void SlaveDisplaySlave::deletePixmapMessage(long *message, int num)
{
	assert(message[0] == _DELETE_PIXMAP && num == 2);
	assert(_pixmaps->getNumPixmapsComing() == 0);

	_pixmaps->remove((Pixmap) message[1]);
}

void SlaveDisplaySlave::displayPixmapMessage(long *message, int num)
{
	assert(message[0] == _DISPLAY_PIXMAP && num == 3);
	assert(_pixmaps->getNumPixmapsComing() == 0);

	if (!_pixmaps->find((Pixmap) message[1]))
		_pixmaps->add((Pixmap) message[1]);

	if (!_pixmaps->checkError())
		_pixmaps->display((Pixmap) message[1],
			(unsigned long) message[2]);
}

void SlaveDisplaySlave::pixelInfoMessage(long *message, int num)
{
	assert(message[0] == _PIXEL_INFO && num == 4);
	assert(!_pixmaps);

	sameSizeWindows((Window) message[1]);

	_pixmaps = new PixmapLinkedList(
		((ClientMessageSlave *) _ipc)->getMasterDisplay(),
		(Window) message[1], _localWidget, message[2], message[3],
		_application, _sdScrWin);
	if (_pixmaps->checkError())
		handleError(_pixmaps->checkError());
}

void SlaveDisplaySlave::sizeChangeMessage(long *message, int num)
{
	assert(message[0] == _SIZE_CHANGE && num == 3);

	_width  = message[1];
	_height = message[2];

	assert(_pixmaps);
	_pixmaps->windowSize((unsigned int) _width, (unsigned int) _height);

	XtVaSetValues(_localWidget,
		XmNwidth , (Dimension) _width ,
		XmNheight, (Dimension) _height,
		NULL);
}


void SlaveDisplaySlave::updateRGBsMessage()
{
	_pixmaps->updateRGBs();
}

void SlaveDisplaySlave::quitMessage()
{
	delete this;
}

void SlaveDisplaySlave::titleMessage(char *message)
{
	Widget shell;
	for (shell = _localWidget;
		!XtIsShell(shell);
		shell = XtParent(shell));

	XtVaSetValues(shell,
		XmNtitle, message,
		NULL);
}

void SlaveDisplaySlave::numPixmapsMessage(long *message, int num)
{
	assert(message[0] == _NUM_PIXMAPS && num == 2);

	_pixmaps->setNumPixmapsComing(message[1]);
}

void SlaveDisplaySlave::borderInfoMessage(long *message, int num)
{
	assert(message[0] == _BORDER_INFO && num == 3);

	if (_sdScrWin != NULL)
	{
		_leftBorder  = message[1] / 10000;
		_rightBorder = message[1] % 10000;
		_topBorder   = message[2] / 10000;
		_bottomBorder= message[2] % 10000;

		if (_borders)
		{
			_sdScrWin->setLeftBorder  (_leftBorder  );
			_sdScrWin->setRightBorder (_rightBorder );
			_sdScrWin->setTopBorder   (_topBorder   );
			_sdScrWin->setBottomBorder(_bottomBorder);
		}
	}
}

void SlaveDisplaySlave::resize(int /*width*/, int /*height*/)
{
//	Don't assert, if queue gets backed up there can be more than one
//	resizes before the event is handled.
//	assert(_width == width && _height == height);
}

void SlaveDisplaySlave::sameSizeWindows(Window remoteWindow)
{
	Window		remoteRoot  ,               localRoot;
	int		remoteX     , remoteY     , localX    , localY;
	unsigned int	remoteWidth , remoteHeight, localWidth, localHeight;
	unsigned int	remoteBwidth,               localBwidth;
	unsigned int	remoteDepth ,               localDepth;

	XGetGeometry(
		((ClientMessageSlave *) _ipc)->getMasterDisplay(), remoteWindow,
		&remoteRoot, &remoteX,
		&remoteY, &remoteWidth, &remoteHeight, &remoteBwidth,
		&remoteDepth);

	XGetGeometry(
		XtDisplay(_localWidget), XtWindow(_localWidget),
		&localRoot , &localX ,
		&localY , &localWidth , &localHeight , &localBwidth ,
		&localDepth );

	assert(remoteDepth == localDepth);

	if (remoteWidth != localWidth || remoteHeight != localHeight)
		XtVaSetValues(_localWidget,
			XmNwidth , (Dimension) remoteWidth ,
			XmNheight, (Dimension) remoteHeight,
			NULL);

	_width  = (int) remoteWidth ;
	_height = (int) remoteHeight;
}

void SlaveDisplaySlave::errorHandler(HandlesErrors * /*ptr*/, int errorNum)
{
	char *suffix = "";
	char *masterSuffix = "Master process ";
	Bool displayMessage = True;
	long array[5];

	switch (errorNum)
	{
		case _ERROR_PARTNER_DIED:
			_otherGuyDied = True;
			suffix = masterSuffix;
			break;
		case _ERROR_OUT_OF_COLORS:
			Colormap theColormap;
			XtVaGetValues(_localWidget,
				XmNcolormap, &theColormap,
				NULL);
			if (theColormap == DefaultColormap(
				XtDisplay(_localWidget),
				DefaultScreen(XtDisplay(_localWidget))))
			{
				displayMessage = False;
			}
			/* no break */
		case _ERROR_OUT_OF_PIXMAP_MEMORY:
			array[0] = _ERROR;
			array[1] = errorNum;
			_ipc->send(array, 2);
			break;
		default:
			assert(False);
	}

	if (displayMessage)
	{
		char *errorMessage;

		if (strlen(suffix))
		{
			assert(errorMessage = new char[strlen(suffix) +
				strlen(errorString[errorNum]) + 1]);

			sprintf(errorMessage, "%s%s", suffix,
				errorString[errorNum]);
		}
		else
		{
			errorMessage = errorString[errorNum];
		}

		errorAckStruct *errorAckData;
		assert(errorAckData = new errorAckStruct);

		errorAckData->quitter = (IpcQuit *) NULL;
		_quitterPtr = &errorAckData->quitter;

		assert(errorAckData->eh = new ErrorHandler(_localWidget));
		errorAckData->eh->setAltErrorAck(errorAck,
			(void *) errorAckData);
		errorAckData->eh->deliverError(errorMessage);

		if (strlen(suffix))
			delete [] errorMessage;

		_exitOnDelete = False;
	}
	else
	{
		_exitOnDelete = True ;
	}

	delete this;
}

void SlaveDisplaySlave::errorAck(void *client)
{
	delete ((errorAckStruct *) client)->eh;

	IpcQuit *quitter = ((errorAckStruct *) client)->quitter;

	delete (errorAckStruct *) client;

	if (quitter)
		quitter->setExitOnDelete();
	else
		exit(0);
}

void SlaveDisplaySlave::setBorders(Bool borders)
{
	if (_sdScrWin != NULL && borders != _borders)
	{
		_borders = borders;

		if (_borders)
		{
			_sdScrWin->setLeftBorder  (_leftBorder  );
			_sdScrWin->setRightBorder (_rightBorder );
			_sdScrWin->setTopBorder   (_topBorder   );
			_sdScrWin->setBottomBorder(_bottomBorder);
		}
		else
		{
			_sdScrWin->setLeftBorder  (0);
			_sdScrWin->setRightBorder (0);
			_sdScrWin->setTopBorder   (0);
			_sdScrWin->setBottomBorder(0);
		}
	}
}
