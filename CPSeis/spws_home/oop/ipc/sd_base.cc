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
#include "ipc/sd_base.hh"
#include "sl/paintset_collection.hh"
#include "sl/paintset.hh"

#ifndef XM_H
#include <Xm/Xm.h>
#define XM_H
#endif

#ifndef STDLIB_H
#include <stdlib.h>
#define STDLIB_H
#endif

#ifndef ASSERT_H
#include <assert.h>
#define ASSERT_H
#endif

#ifndef WPROC_H
#include "wproc.h"
#define WPROC_H
#endif

#ifndef myMIN
#define myMIN(x, y)	(((x) < (y)) ? (x) : (y))
#endif

#ifndef myMAX
#define myMAX(x, y)	(((x) > (y)) ? (x) : (y))
#endif

const char *const SlaveDisplayBase::_CURSOR_COLOR    = "red";
const char *const SlaveDisplayBase::_slaveApp        = "slave_dpy";
const char *const SlaveDisplayBase::_privateColormap = "-SPWS_PRIVATE_COLORMAP";

SlaveDisplayBase::SlaveDisplayBase(Widget w,
	SlaveDisplayBase **zeroWhenDestroyed) : _xorCursor(False),
	_exposuresPending(0), _cursorMovedWhileExposurePending(False)
{
	_localWidget = w;
	_zeroWhenDestroyed = zeroWhenDestroyed;

	_otherGuyQuit = False;
	_otherGuyDied = False;

	_quitterPtr = (IpcQuit **) NULL;

	Dimension width, height;
	XtVaGetValues(_localWidget,
		XmNwidth , &width ,
		XmNheight, &height,
		NULL);
	_width  = (int) width ;
	_height = (int) height;

	initCursor();
}

SlaveDisplayBase::~SlaveDisplayBase()
{
	XtRemoveEventHandler(_localWidget, PointerMotionMask | EnterWindowMask
		| LeaveWindowMask | ExposureMask,
		False, eventHandler, (XtPointer) this);

	XtRemoveEventHandler(_localWidget, StructureNotifyMask, False, resizeEH,
		(XtPointer) this);

	closeCursor();

	if (_zeroWhenDestroyed)
		*_zeroWhenDestroyed = (SlaveDisplayBase *) NULL;

	if (_otherGuyQuit || _otherGuyDied)
	{
		if (_otherGuyQuit)
		{
			long array[5];
			array[0] = _QUIT_OK;
			_ipc->send(array, 1);
		}

		delete _ipc;
		_ipc = (ClientMessageBase *) NULL;	// reproducible bugs

		if (_exitOnDelete)
			exit(0);

		notifyError(checkError());
	}
	else
	{
		long array[5];
		array[0] = _QUIT_REQUEST;
		_ipc->send(array, 1);

		if (_quitterPtr)
			assert(*_quitterPtr = new IpcQuit(_ipc, _QUIT_OK,
				_exitOnDelete, _quitterPtr));
		else
			assert(new IpcQuit(_ipc, _QUIT_OK,
				_exitOnDelete, (IpcQuit **) NULL));
	}
}

/*
 * Called at end of derived class constructors.
 * Adding these event handlers in the base class constructor causes problems.
 */
void SlaveDisplayBase::addEventHandlers()
{
	/*
	 * Add eventHandler here rather than in base constructor
	 * because of SUN SOLARIS quirk.  The XtOpenDisplay in
	 * ClientMessageBase causes the expose event handler to
	 * be called immediately, not waiting for MainLoop.  This
	 * calls the event handler before all the data structures are
	 * ready causing a crash.
	 */
	XtAddEventHandler(_localWidget, PointerMotionMask | EnterWindowMask
		| LeaveWindowMask | ExposureMask,
		False, eventHandler, (XtPointer) this);

	/*
	 * Add resizeEH here rather than in base constructor so it is
	 * not called by resize caused by adding ipc label width as child.
	 */
	XtAddEventHandler(_localWidget, StructureNotifyMask, False, resizeEH,
		(XtPointer) this);
}

void SlaveDisplayBase::receivedMessage(long *message, int num)
{
	switch (*message)
	{
		case _UPDATE_CURSOR:
			updateCursorMessage (message, num);
			break;
		case _ADD_PIXMAP:
			addPixmapMessage    (message, num);
			if (_pixmaps->checkError())
				handleError(_pixmaps->checkError());
			break;
		case _DELETE_PIXMAP:
			deletePixmapMessage (message, num);
			break;
		case _DISPLAY_PIXMAP:
			clearCursor();
			displayPixmapMessage(message, num);
			if (_pixmaps->checkError())
				handleError(_pixmaps->checkError());
			else
				refreshCursor();
			break;
		case _PIXEL_INFO:
			pixelInfoMessage    (message, num);
			break;
		case _SIZE_CHANGE:
			sizeChangeMessage   (message, num);
			break;
		case _UPDATE_RGBS:
			updateRGBsMessage   (            );
			break;
		case _QUIT_REQUEST:
			_otherGuyQuit = True;
			quitMessage         (            );
			break;
		case _ERROR:
			handleError         (message[1]  );
			break;
		case _NUM_PIXMAPS:
			numPixmapsMessage   (message, num);
			break;
		case _BORDER_INFO:
			borderInfoMessage   (message, num);
			break;
		case _EXPOSURE:
			exposureMessage     (message, num);
			break;
		default:
			assert(False);
	}
}

void SlaveDisplayBase::receivedMessage(char *message)
{
	titleMessage(message);
}

void SlaveDisplayBase::receivedIntPtr(long *message, int num, XtPointer client)
{
	SlaveDisplayBase *obj = (SlaveDisplayBase *) client;
	obj->receivedMessage(message, num);
}

void SlaveDisplayBase::receivedCharPtr(char *message, XtPointer client)
{
	SlaveDisplayBase *obj = (SlaveDisplayBase *) client;
	obj->receivedMessage(message);
}

void SlaveDisplayBase::initCursor()
{
	assert(_CURSOR_HALF_LENGTH > _CURSOR_HALF_WIDTH);

	XWindowAttributes attributes;
	assert(XGetWindowAttributes(XtDisplay(_localWidget),
		XtWindow(_localWidget), &attributes));

	assert(_cursorPixmap[0] = wprocCreatePixmap(XtDisplay(_localWidget),
		XtWindow(_localWidget), (unsigned int) _CURSOR_WORKING_PIXMAP,
		(unsigned int) _CURSOR_WORKING_PIXMAP,
		(unsigned int) attributes.depth));

	assert(_cursorPixmap[1] = wprocCreatePixmap(XtDisplay(_localWidget),
		XtWindow(_localWidget), (unsigned int) _CURSOR_PIXMAP_LENGTH ,
		(unsigned int) _CURSOR_PIXMAP_WIDTH  ,
		(unsigned int) attributes.depth));

	assert(_cursorPixmap[2] = wprocCreatePixmap(XtDisplay(_localWidget),
		XtWindow(_localWidget), (unsigned int) _CURSOR_PIXMAP_WIDTH  ,
		(unsigned int) _CURSOR_PIXMAP_LENGTH ,
		(unsigned int) attributes.depth));

	assert(_cursorPixmap[3] = wprocCreatePixmap(XtDisplay(_localWidget),
		XtWindow(_localWidget), (unsigned int) _CURSOR_PIXMAP_LENGTH ,
		(unsigned int) _CURSOR_PIXMAP_WIDTH  ,
		(unsigned int) attributes.depth));

	assert(_cursorPixmap[4] = wprocCreatePixmap(XtDisplay(_localWidget),
		XtWindow(_localWidget), (unsigned int) _CURSOR_PIXMAP_WIDTH  ,
		(unsigned int) _CURSOR_PIXMAP_LENGTH ,
		(unsigned int) attributes.depth));

	_cursorGC = XCreateGC(XtDisplay(_localWidget), XtWindow(_localWidget),
		0, (XGCValues *) NULL);

/*
	XColor closest, exact;
	if (XAllocNamedColor(XtDisplay(_localWidget), attributes.colormap,
		_CURSOR_COLOR, &closest, &exact))
	{
		XSetForeground(XtDisplay(_localWidget), _cursorGC,
			closest.pixel);
	}
	else
	{
		XSetForeground(XtDisplay(_localWidget), _cursorGC,
			PaintsetCollection::black(DefaultScreenOfDisplay(
			XtDisplay(_localWidget))));
	}
*/
	Paintset *paintset = PaintsetCollection::fetchExistingByColormap (
          _localWidget);
	Pixel pixel = paintset->getForegroundPixelFromName (_CURSOR_COLOR);
	XSetForeground(XtDisplay(_localWidget), _cursorGC, pixel);


	XSetSubwindowMode(XtDisplay(_localWidget), _cursorGC,
          IncludeInferiors);

	XSetLineAttributes(XtDisplay(_localWidget), _cursorGC,
		(unsigned) _CURSOR_PIXMAP_WIDTH, LineSolid, CapButt, JoinMiter);

	XFillRectangle(XtDisplay(_localWidget), _cursorPixmap[1], _cursorGC,
		0, 0, (unsigned int) _CURSOR_PIXMAP_LENGTH,
		(unsigned int) _CURSOR_PIXMAP_WIDTH );

	XFillRectangle(XtDisplay(_localWidget), _cursorPixmap[2], _cursorGC,
		0, 0, (unsigned int) _CURSOR_PIXMAP_WIDTH ,
		(unsigned int) _CURSOR_PIXMAP_LENGTH);

	_cursorX = _cursorY = -1;
}

void SlaveDisplayBase::updateCursorMessage(long *message, int num)
{
	assert(message[0] == _UPDATE_CURSOR && num == 3);

	if (_exposuresPending)
	{
		_cursorMovedWhileExposurePending = True;
		_saveCursorX = message[1];
		_saveCursorY = message[2];
	}
	else
	{
		updateCursor(message[1], message[2]);
	}
}

void SlaveDisplayBase::updateCursor(int x, int y)
{
	if (x == -1)
	{
		clearCursor();
		_cursorX = _cursorY = -1;
	}
	else if (_cursorX == -1)
	{
		_cursorX = x;
		_cursorY = y;
		refreshCursor();
	}
	else if (  abs(x - _cursorX) > _CURSOR_THRESHOLD
		|| abs(y - _cursorY) > _CURSOR_THRESHOLD)
	{
		clearCursor();
		_cursorX = x;
		_cursorY = y;
		refreshCursor();
	}
	else
	{
		repositionCursor(x, y);
	}
}

void SlaveDisplayBase::clearCursor()
{
	if (_cursorX == -1)
		return;

	if (_xorCursor)
	{
		drawXorCursor();
	}
	else
	{
		int xll, xlw, xrl, xrw, ytl, ytw, ybl, ybw;
		getLosses(_cursorX, _cursorY, 0, 0, _width, _height,
			&xll, &xlw, &xrl, &xrw, &ytl, &ytw, &ybl, &ybw);

		/*
		 * In these compound names, the 1st name (length or
		 * width) refers to the cursor, the 2nd name (Width or
		 * Height) refers to the pixmap.
		 */
		int lengthWidth  = _CURSOR_PIXMAP_LENGTH - xll - xrl;
		int lengthHeight = _CURSOR_PIXMAP_LENGTH - ytl - ybl;

		if (lengthWidth > 0 && lengthHeight > 0)
		{
			int xWindow = _cursorX - _CURSOR_HALF_LENGTH + xll;
			int yWindow = _cursorY - _CURSOR_HALF_LENGTH + ytl;
			int widthWidth  = _CURSOR_PIXMAP_WIDTH - xlw - xrw;
			int widthHeight = _CURSOR_PIXMAP_WIDTH - ytw - ybw;

			/*
			 * xCursor & yCursor are distances to cursor &
			 * recover strips.
			 */
			int xCursor = _CURSOR_HALF_LENGTH - _CURSOR_HALF_WIDTH
				+ xlw;
			int yCursor = _CURSOR_HALF_LENGTH - _CURSOR_HALF_WIDTH
				+ ytw;

			// Get
			XCopyArea(XtDisplay(_localWidget),
				XtWindow(_localWidget), _cursorPixmap[0],
				_cursorGC,
				xWindow, yWindow,
				(unsigned) lengthWidth, (unsigned) lengthHeight,
				xll, ytl);

			// Recover
			if (widthHeight > 0)
				XCopyArea(XtDisplay(_localWidget),
					_cursorPixmap[3], _cursorPixmap[0],
					_cursorGC,
					xll, ytw,
					(unsigned) lengthWidth,
					(unsigned) widthHeight,
					xll, yCursor);

			if (widthWidth > 0)
				XCopyArea(XtDisplay(_localWidget),
					_cursorPixmap[4], _cursorPixmap[0],
					_cursorGC,
					xlw, ytl,
					(unsigned) widthWidth  ,
					(unsigned) lengthHeight,
					xCursor, ytl);

			// Put
			XCopyArea(XtDisplay(_localWidget),
				_cursorPixmap[0], XtWindow(_localWidget),
				_cursorGC, xll, ytl,
				(unsigned) lengthWidth, (unsigned) lengthHeight,
				xWindow, yWindow);
		}
	}
}

void SlaveDisplayBase::refreshCursor()
{
	refreshCursor(0, 0, _width, _height);
}

void SlaveDisplayBase::refreshCursor(int x, int y, int width, int height)
{
	// Must only use refreshCursor if cursor is not displayed.
	// Otherwise cursor will be read into recover pixmaps.

	if (_cursorX == -1)
		return;

	if (_xorCursor)
	{
		if (width != _width || height != _height)
			drawXorCursor(x, y, width, height);
		else
			drawXorCursor();
	}
	else
	{
		int xll, xlw, xrl, xrw, ytl, ytw, ybl, ybw;
		getLosses(_cursorX, _cursorY, x, y, width, height,
			&xll, &xlw, &xrl, &xrw, &ytl, &ytw, &ybl, &ybw);

		/*
		 * In these compound names, the 1st name (length or
		 * width) refers to the cursor, the 2nd name (Width or
		 * Height) refers to the pixmap.
		 */
		int lengthWidth  = _CURSOR_PIXMAP_LENGTH - xll - xrl;
		int lengthHeight = _CURSOR_PIXMAP_LENGTH - ytl - ybl;

		if (lengthWidth > 0 && lengthHeight > 0)
		{
			int xWindow = _cursorX - _CURSOR_HALF_LENGTH + xll;
			int yWindow = _cursorY - _CURSOR_HALF_LENGTH + ytl;
			int widthWidth  = _CURSOR_PIXMAP_WIDTH - xlw - xrw;
			int widthHeight = _CURSOR_PIXMAP_WIDTH - ytw - ybw;

			/*
			 * xCursor & yCursor are distances to cursor &
			 * recover strips.
			 */
			int xCursor = _CURSOR_HALF_LENGTH - _CURSOR_HALF_WIDTH
				+ xlw;
			int yCursor = _CURSOR_HALF_LENGTH - _CURSOR_HALF_WIDTH
				+ ytw;

			// Get
			XCopyArea(XtDisplay(_localWidget),
				XtWindow(_localWidget), _cursorPixmap[0],
				_cursorGC,
				xWindow, yWindow,
				(unsigned) lengthWidth, (unsigned) lengthHeight,
				xll, ytl);

			// Save
			if (widthHeight > 0)
				XCopyArea(XtDisplay(_localWidget),
					_cursorPixmap[0], _cursorPixmap[3],
					_cursorGC,
					xll, yCursor,
					(unsigned) lengthWidth,
					(unsigned) widthHeight,
					xll, ytw);

			if (widthWidth > 0)
				XCopyArea(XtDisplay(_localWidget),
					_cursorPixmap[0], _cursorPixmap[4],
					_cursorGC,
					xCursor, ytl,
					(unsigned) widthWidth  ,
					(unsigned) lengthHeight,
					xlw, ytl);

			// Add cursor
			if (widthHeight > 0)
				XCopyArea(XtDisplay(_localWidget),
					_cursorPixmap[1], _cursorPixmap[0],
					_cursorGC,
					xll, ytw,
					(unsigned) lengthWidth,
					(unsigned) widthHeight,
					xll, yCursor);

			if (widthWidth > 0)
				XCopyArea(XtDisplay(_localWidget),
					_cursorPixmap[2], _cursorPixmap[0],
					_cursorGC,
					xlw, ytl,
					(unsigned) widthWidth  ,
					(unsigned) lengthHeight,
					xCursor, ytl);

			// Put
			XCopyArea(XtDisplay(_localWidget),
				_cursorPixmap[0], XtWindow(_localWidget),
				_cursorGC, xll, ytl,
				(unsigned) lengthWidth, (unsigned) lengthHeight,
				xWindow, yWindow);
		}
	}
}

void SlaveDisplayBase::repositionCursor(int x, int y)
{
	if (_xorCursor)
	{
		drawXorCursor();

		_cursorX = x;
		_cursorY = y;

		drawXorCursor();
	}
	else
	{
		int xll1, xlw1, xrl1, xrw1, ytl1, ytw1, ybl1, ybw1;
		getLosses(_cursorX, _cursorY, 0, 0, _width, _height,
			&xll1, &xlw1, &xrl1, &xrw1, &ytl1, &ytw1, &ybl1, &ybw1);

		int xll2, xlw2, xrl2, xrw2, ytl2, ytw2, ybl2, ybw2;
		getLosses(x, y, 0, 0, _width, _height,
			&xll2, &xlw2, &xrl2, &xrw2, &ytl2, &ytw2, &ybl2, &ybw2);

		int xll = myMAX(xll1, xll2);
		int xrl = myMAX(xrl1, xrl2);
		int ytl = myMAX(ytl1, ytl2);
		int ybl = myMAX(ybl1, ybl2);

		int xOffset = myMIN(_cursorX, x) - _CURSOR_HALF_LENGTH + xll;
		int yOffset = myMIN(_cursorY, y) - _CURSOR_HALF_LENGTH + ytl;
		int xRange  = myMAX(_cursorX, x)
			+ _CURSOR_HALF_LENGTH - xrl - xOffset + 1;
		int yRange  = myMAX(_cursorY, y)
			+ _CURSOR_HALF_LENGTH - ybl - yOffset + 1;

		if (xRange > 0 && yRange > 0)
		{
			int lengthWidth1  = _CURSOR_PIXMAP_LENGTH - xll1 - xrl1;
			int lengthHeight1 = _CURSOR_PIXMAP_LENGTH - ytl1 - ybl1;
			int widthWidth1   = _CURSOR_PIXMAP_WIDTH  - xlw1 - xrw1;
			int widthHeight1  = _CURSOR_PIXMAP_WIDTH  - ytw1 - ybw1;
			int lengthWidth2  = _CURSOR_PIXMAP_LENGTH - xll2 - xrl2;
			int lengthHeight2 = _CURSOR_PIXMAP_LENGTH - ytl2 - ybl2;
			int widthWidth2   = _CURSOR_PIXMAP_WIDTH  - xlw2 - xrw2;
			int widthHeight2  = _CURSOR_PIXMAP_WIDTH  - ytw2 - ybw2;

			int xHoriz1 = _cursorX - xOffset - _CURSOR_HALF_LENGTH
				+ xll1;
			int yHoriz1 = _cursorY - yOffset - _CURSOR_HALF_WIDTH
				+ ytw1;
			int xVert1  = _cursorX - xOffset - _CURSOR_HALF_WIDTH
				+ xlw1;
			int yVert1  = _cursorY - yOffset - _CURSOR_HALF_LENGTH
				+ ytl1;
			int xHoriz2 = x - xOffset - _CURSOR_HALF_LENGTH + xll2;
			int yHoriz2 = y - yOffset - _CURSOR_HALF_WIDTH  + ytw2;
			int xVert2  = x - xOffset - _CURSOR_HALF_WIDTH  + xlw2;
			int yVert2  = y - yOffset - _CURSOR_HALF_LENGTH + ytl2;

			// Get
			XCopyArea(XtDisplay(_localWidget),
				XtWindow(_localWidget), _cursorPixmap[0],
				_cursorGC, xOffset, yOffset,
				(unsigned int) xRange, (unsigned int) yRange,
				0, 0);

			// Recover
			if (lengthWidth1 > 0 && widthHeight1 > 0)
				XCopyArea(XtDisplay(_localWidget),
					_cursorPixmap[3], _cursorPixmap[0],
					_cursorGC, xll1, ytw1,
					(unsigned) lengthWidth1,
					(unsigned) widthHeight1,
					xHoriz1, yHoriz1);

			if (widthWidth1 > 0 && lengthHeight1 > 0)
				XCopyArea(XtDisplay(_localWidget),
					_cursorPixmap[4], _cursorPixmap[0],
					_cursorGC, xlw1, ytl1,
					(unsigned) widthWidth1,
					(unsigned) lengthHeight1,
					xVert1, yVert1);

			// Save
			if (lengthWidth2 > 0 && widthHeight2 > 0)
				XCopyArea(XtDisplay(_localWidget),
					_cursorPixmap[0], _cursorPixmap[3],
					_cursorGC,
					xHoriz2, yHoriz2,
					(unsigned) lengthWidth2,
					(unsigned) widthHeight2,
					xll2, ytw2);

			if (widthWidth2 > 0 && lengthHeight2 > 0)
				XCopyArea(XtDisplay(_localWidget),
					_cursorPixmap[0], _cursorPixmap[4],
					_cursorGC,
					xVert2, yVert2,
					(unsigned) widthWidth2,
					(unsigned) lengthHeight2,
					xlw2, ytl2);

			// Add cursor
			if (lengthWidth2 > 0 && widthHeight2 > 0)
				XCopyArea(XtDisplay(_localWidget),
					_cursorPixmap[1], _cursorPixmap[0],
					_cursorGC, xll2, ytw2,
					(unsigned) lengthWidth2,
					(unsigned) widthHeight2,
					xHoriz2, yHoriz2);

			if (widthWidth2 > 0 && lengthHeight2 > 0)
				XCopyArea(XtDisplay(_localWidget),
					_cursorPixmap[2], _cursorPixmap[0],
					_cursorGC, xlw2, ytl2,
					(unsigned) widthWidth2,
					(unsigned) lengthHeight2,
					xVert2, yVert2);

			// Put
			XCopyArea(XtDisplay(_localWidget),
				_cursorPixmap[0], XtWindow(_localWidget),
				_cursorGC, 0, 0,
				(unsigned int) xRange, (unsigned int) yRange,
				xOffset, yOffset);
		}

		_cursorX = x;
		_cursorY = y;
	}
}

void SlaveDisplayBase::closeCursor()
{
	clearCursor();

	for (int i = 0; i < _NUM_CURSOR_PIXMAPS; i++)
		XFreePixmap(XtDisplay(_localWidget), _cursorPixmap[i]);

	Paintset *paintset = PaintsetCollection::fetchExistingByColormap (
          _localWidget);
	paintset->freePixelFromName (_CURSOR_COLOR, 0);

	XFreeGC(XtDisplay(_localWidget), _cursorGC);
}

void SlaveDisplayBase::getLosses(int x, int y, int xOrg, int yOrg,
	int width, int height, int *xll, int *xlw, int *xrl, int *xrw,
	int *ytl, int *ytw, int *ybl, int *ybw)
{
	x -= xOrg;
	y -= yOrg;

	*xll = myMAX(0, _CURSOR_HALF_LENGTH - x);	//lost off left
	*xlw = myMAX(0, _CURSOR_HALF_WIDTH  - x);
	*xrl = myMAX(0, _CURSOR_HALF_LENGTH + x - (width  - 1));  //right
	*xrw = myMAX(0, _CURSOR_HALF_WIDTH  + x - (width  - 1));
	*ytl = myMAX(0, _CURSOR_HALF_LENGTH - y);	//lost off top
	*ytw = myMAX(0, _CURSOR_HALF_WIDTH  - y);
	*ybl = myMAX(0, _CURSOR_HALF_LENGTH + y - (height - 1));  //bottom
	*ybw = myMAX(0, _CURSOR_HALF_WIDTH  + y - (height - 1));
}

void SlaveDisplayBase::eventHandler(Widget, XtPointer client, XEvent *event,
	Boolean *)
{
	SlaveDisplayBase *obj = (SlaveDisplayBase *) client;

	switch (event->type)
	{
		case MotionNotify:
			obj->pointerMotion(event);
			break;
		case EnterNotify:
			obj->enterWindow(event);
			break;
		case LeaveNotify:
			obj->leaveWindow(event);
			break;
		case Expose:
			obj->exposure(event);
			break;
		default:
			assert(False);
			break;
	}
}

void SlaveDisplayBase::pointerMotion(XEvent *event)
{
	long array[5];
	array[0] = _UPDATE_CURSOR;
	array[1] = event->xmotion.x;
	array[2] = event->xmotion.y;
	_ipc->send(array, 3);
}

void SlaveDisplayBase::enterWindow(XEvent *event)
{
	long array[5];
	array[0] = _UPDATE_CURSOR;
	array[1] = event->xcrossing.x;
	array[2] = event->xcrossing.y;
	_ipc->send(array, 3);
}

void SlaveDisplayBase::leaveWindow(XEvent * /*event*/)
{
	long array[5];
	array[0] = _UPDATE_CURSOR;
	array[1] = -1;
	array[2] = -1;
	_ipc->send(array, 3);
}

void SlaveDisplayBase::exposure(XEvent *event)
{
	long array[4];
	array[0] = _EXPOSURE;
	array[1] = event->xexpose.x     * 10000 + event->xexpose.y     ;
	array[2] = event->xexpose.width * 10000 + event->xexpose.height;

	/*
	 * Send client message to myself to handle exposure.
	 * This will make sure (I hope) that all other exposure
	 * handling is done first.
	 * There can still be problems with exposures if there are
	 * multiple remotes that have overlapping cursor positions
	 * on the master.
	 */
	_ipc->send(array, 3, True);

	_exposuresPending++;
}

void SlaveDisplayBase::exposureMessage(long *message, int num)
{
	assert(message[0] == _EXPOSURE && num == 3);

	int x      = message[1] / 10000;
	int y      = message[1] % 10000;
	int width  = message[2] / 10000;
	int height = message[2] % 10000;

	refreshCursor(x, y, width, height);

	if (--_exposuresPending == 0 && _cursorMovedWhileExposurePending)
	{
		updateCursor(_saveCursorX, _saveCursorY);
		_cursorMovedWhileExposurePending = False;
	}
}

void SlaveDisplayBase::resizeEH(Widget, XtPointer client, XEvent *event,
	Boolean *)
{
	SlaveDisplayBase *obj;
	switch (event->type)
	{
		case ConfigureNotify:
			obj = (SlaveDisplayBase *) client;
			obj->resize(event->xconfigure.width,
				event->xconfigure.height);
			break;
		default:
			// do nothing
			break;
	}
}

void SlaveDisplayBase::setXorCursor(Bool xorCursor)
{
	if (xorCursor != _xorCursor)
	{
		clearCursor();

		_xorCursor = xorCursor;

		if (_xorCursor)
			XSetFunction(XtDisplay(_localWidget), _cursorGC,
				GXxor );
		else
			XSetFunction(XtDisplay(_localWidget), _cursorGC,
				GXcopy);

		refreshCursor();
	}
}

void SlaveDisplayBase::drawXorCursor()
{
	XDrawLine(XtDisplay(_localWidget), XtWindow(_localWidget), _cursorGC,
		_cursorX - _CURSOR_HALF_LENGTH, _cursorY,
		_cursorX + _CURSOR_HALF_LENGTH, _cursorY);

	XDrawLine(XtDisplay(_localWidget), XtWindow(_localWidget), _cursorGC,
		_cursorX, _cursorY - _CURSOR_HALF_LENGTH,
		_cursorX, _cursorY + _CURSOR_HALF_LENGTH);
}

void SlaveDisplayBase::drawXorCursor(int x, int y, int width, int height)
{
	XRectangle clipArea;

	clipArea.x      = (short) 0;
	clipArea.y      = (short) 0;
	clipArea.width  = (unsigned short) width ;
	clipArea.height = (unsigned short) height;

	XSetClipRectangles(XtDisplay(_localWidget), _cursorGC, x, y,
		&clipArea, 1, YXBanded);

	drawXorCursor();

	XSetClipMask(XtDisplay(_localWidget), _cursorGC, None);
}
