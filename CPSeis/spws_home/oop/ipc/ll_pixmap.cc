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
#include "ipc/ll_pixmap.hh"

#ifndef _LL_PIXMAPS_COMING_HH
#include "ipc/ll_pixmaps_coming.hh"
#define _LL_PIXMAPS_COMING_HH
#endif

#ifndef SLAPP_H
#include "sl/sl_app.hh"
#define SLAPP_H
#endif

#ifndef XM_H
#include <Xm/Xm.h>
#define XM_H
#endif

#ifndef STDIO_H
#include <stdio.h>
#define STDIO_H
#endif

#ifndef ASSERT_H
#include <assert.h>
#define ASSERT_H
#endif

PixmapLinkedList::PixmapLinkedList(Display *remoteDisplay, Window remoteWindow,
	Widget localWidget, int numPixels, int numPlanes, SLApp *application,
	class SlaveDisplayScrWin *sdScrWin)
	:	_remoteDisplay(remoteDisplay), _widget(localWidget),
		_pixmapsComing((PixmapsComingLinkedList *) NULL),
		_numPixmapsComing(0), _application(application),
		_sdScrWin(sdScrWin)
{
	getWindowSize();

	assert(_colorTranslator = new ColorTranslate(_remoteDisplay,
		remoteWindow, XtDisplay(_widget), XtWindow(_widget), numPixels,
		numPlanes));
	if (_colorTranslator->checkError())
		handleError(_colorTranslator->checkError());

	_pixmapGC = XCreateGC(XtDisplay(_widget), XtWindow(_widget), 0,
		(XGCValues *) NULL);

	_bitmapGC = (GC) False;	// At 1st bitmap to give depth of 1.

	_currentPixmap = (PixmapElement *) NULL;
	_currentBitmap = (PixmapElement *) NULL;
	assert(_currentBitmapOverlay = new PixmapElement *[(int) _depth]);
	for (int i = 0; i < (int) _depth; i++)
		_currentBitmapOverlay[i] = (PixmapElement *) NULL;

	XtAddEventHandler(_widget, ExposureMask       , False, exposureEH,
		(XtPointer) this);
	XtAddEventHandler(_widget, StructureNotifyMask, False, resizeEH  ,
		(XtPointer) this);

	if (_application)
		_application->setMode("No data");
}

PixmapLinkedList::~PixmapLinkedList()
{
	delete _colorTranslator;
	_colorTranslator = (ColorTranslate *) NULL;	// reproducible bugs

	XFreeGC(XtDisplay(_widget), _pixmapGC);

	if (_bitmapGC)
		XFreeGC(XtDisplay(_widget), _bitmapGC);

	delete [] _currentBitmapOverlay;

	XtRemoveEventHandler(_widget, ExposureMask       , False, exposureEH,
		(XtPointer) this);
	XtRemoveEventHandler(_widget, StructureNotifyMask, False, resizeEH  ,
		(XtPointer) this);
}

void PixmapLinkedList::add(Pixmap remotePixmap)
{
	static int totalPixmapsComing = 0;
	static char buff[80];

	if (_numPixmapsComing)
	{
		if (totalPixmapsComing == 0)	// 1st time for this series.
		{
			totalPixmapsComing = _numPixmapsComing;

			if (_application)
				_application->setMode("Loading data");
		}

		if (_application)
		{
			sprintf(buff, "Allocating pixmap %d of %d",
				totalPixmapsComing - _numPixmapsComing + 1,
				totalPixmapsComing);

			_application->setMessage(buff);
		}
	}
	else
	{
		if (_application)
		{
			_application->setMode   ("Loading data"     );
			_application->setMessage("Allocating pixmap");
		}
	}

	PixmapElement *theElement = new PixmapElement(remotePixmap, this);

	int error = theElement->checkError();
	if (error)
	{
		delete theElement;
		handleError(error);
	}
	else
	{
		BaseLinkedList::add((Element *) theElement);

		if (_numPixmapsComing)
		{
			_pixmapsComing->add(theElement);

			if (--_numPixmapsComing == 0)
			{
				void *p;
				int i = 0;
				for (theElement = _pixmapsComing->top(&p);
					theElement;
					theElement = _pixmapsComing->next(&p))
				{
					if (_application)
					{
						sprintf(buff,
						"Copying pixmap %d of %d",
						++i, totalPixmapsComing);

						_application->setMessage(buff);
					}

					theElement->copyPixmap();
				}

				delete _pixmapsComing;
				_pixmapsComing =
					(PixmapsComingLinkedList *) NULL;
				totalPixmapsComing = 0;

				if (_application)
				{
					_application->setMode   ("");
					_application->setMessage("");
				}
			}
		}
		else
		{
			if (_application)
				_application->setMessage("Copying pixmap");

			theElement->copyPixmap();

			if (_application)
			{
				_application->setMode   ("");
				_application->setMessage("");
			}
		}
	}
}

void PixmapLinkedList::remove(Pixmap remotePixmap)
{
	PixmapElement *ptr = find(remotePixmap);

	Bool removedCurrent = False;

	if (ptr == _currentPixmap || ptr == _currentBitmap)
	{
		removedCurrent = True;
	}
	else
	{
		for (int i = 0; i < (int) _depth; i++)
			if (ptr == _currentBitmapOverlay[i])
			{
				removedCurrent = True;
				break;
			}
	}

	if (removedCurrent)
	{
		_currentPixmap = (PixmapElement *) NULL;
		_currentBitmap = (PixmapElement *) NULL;

		for (int i = 0; i < (int) _depth; i++)
			_currentBitmapOverlay[i] = (PixmapElement *) NULL;

		if (_application)
			_application->setMode("No data");

		XClearWindow(XtDisplay(_widget), XtWindow(_widget));
	}

	BaseLinkedList::remove((void *) remotePixmap);
}

void PixmapLinkedList::display(Pixmap remotePixmap,
	unsigned long remotePlaneMask)
{
	PixmapElement *theElement;
	assert(theElement = find(remotePixmap));

	theElement->display(remotePlaneMask);
}

void PixmapLinkedList::reDisplay(Widget widget, int xSrc, int ySrc,
	unsigned width, unsigned height, int xDst, int yDst)
{
	/*
	 * reDisplay is used for SlaveDisplayScrWin annotate,
	 * so does not need to handle bitmap overlays.
	 */
	if (_currentPixmap)
		_currentPixmap->display(widget, xSrc, ySrc,
			width, height, xDst, yDst);
	else if (_currentBitmap)
		_currentBitmap->display(widget, xSrc, ySrc,
			width, height, xDst, yDst);
}

void PixmapLinkedList::setNumPixmapsComing(int numPixmapsComing)
{
	if (_numPixmapsComing == 0)
	{
		assert(_pixmapsComing == (PixmapsComingLinkedList *) NULL);
		_numPixmapsComing = numPixmapsComing;
		_pixmapsComing = new PixmapsComingLinkedList();
	}
	else
	{
		_numPixmapsComing += numPixmapsComing;
	}
}

void PixmapLinkedList::exposureEH(Widget, XtPointer client, XEvent *event,
	Boolean *)
{
	PixmapLinkedList *obj = (PixmapLinkedList *) client;

	obj->exposure(event);
}

void PixmapLinkedList::exposure(XEvent *event)
{
	assert(!(_currentPixmap && _currentBitmap));

	if (_currentPixmap)
	{
		_currentPixmap->display(event->xexpose.x, event->xexpose.y,
			(unsigned int) event->xexpose.width ,
			(unsigned int) event->xexpose.height);

		int i;
		unsigned long plane;
		for (i = 0, plane = 1; i < (int) _depth; i++, plane <<= 1)
			if (_currentBitmapOverlay[i])
				_currentBitmapOverlay[i]->display(
					event->xexpose.x, event->xexpose.y,
					(unsigned int) event->xexpose.width ,
					(unsigned int) event->xexpose.height,
					plane);
	}
	else if (_currentBitmap)
	{
		_currentBitmap->display(event->xexpose.x, event->xexpose.y,
			(unsigned int) event->xexpose.width ,
			(unsigned int) event->xexpose.height);

		for (int i = 0; i < (int) _depth; i++)
			assert(!_currentBitmapOverlay[i]);
	}
}

void PixmapLinkedList::resizeEH(Widget, XtPointer client, XEvent *event,
	Boolean *)
{
	PixmapLinkedList *obj;

	switch (event->type)
	{
		case ConfigureNotify:
			obj = (PixmapLinkedList *) client;
			obj->resize(event->xconfigure.width,
				event->xconfigure.height);
			break;
		default:
			// do nothing
			break;
	}
}

void PixmapLinkedList::resize(int /*width*/, int /*height*/)
{
//	Don't assert, if queue gets backed up there can be more than one
//	resizes before the event is handled.
//	assert( _width  == (unsigned int) width  &&
//		_height == (unsigned int) height); 
}

void PixmapLinkedList::getWindowSize()
{
	Window root;
	int x, y;
	unsigned int bwidth;

	XGetGeometry(XtDisplay(_widget), XtWindow(_widget), &root, &x, &y,
		&_width, &_height, &bwidth, &_depth);
}
