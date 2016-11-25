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
#include "ipc/pixmap_element.hh"
#include "ipc/sd_scrwin.hh"
#include "sl/shell_watch.hh"
#include "sl/paintset_collection.hh"

#ifndef ASSERT_H
#include <assert.h>
#define ASSERT_H
#endif

#ifndef XM_H
#include <Xm/Xm.h>
#define XM_H
#endif

#ifndef XPROTO_H
#include <X11/Xproto.h>
#define XPROTO_H
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

PixmapElement::PixmapElement(Pixmap remotePixmap, PixmapLinkedList *list)
{
	ShellWatch theWatch;

	_remotePixmap = remotePixmap;
	_linkedList   = list        ;

	getPixmapSize();

	_localPixmap = wprocCreatePixmap(XtDisplay(_linkedList->_widget),
		XtWindow(_linkedList->_widget), _width, _height, _depth);

	if (!_localPixmap)
	{
		handleError(_ERROR_OUT_OF_PIXMAP_MEMORY);
		return;
	}
}

PixmapElement::~PixmapElement()
{
	if (_localPixmap)
		XFreePixmap(XtDisplay(_linkedList->_widget), _localPixmap);
}

void PixmapElement::copyPixmap()
{
	ShellWatch theWatch;

	if (_depth == 1)		// bitmap
	{
		XImage *theImage = XGetImage(_linkedList->_remoteDisplay,
			_remotePixmap, 0, 0, _width, _height, 1, XYPixmap);
		assert(theImage->depth == 1);

		if (!_linkedList->_bitmapGC)
		{
			// _bitmapGC has depth of one.
			// Only used in XPutImage of bitmap.
			_linkedList->_bitmapGC = XCreateGC(
				XtDisplay(_linkedList->_widget), _localPixmap,
				0, (XGCValues *) NULL);
			// Not sure if XSetForeground, XSetBackground, &
			// XSetPlaneMask really needed.
			XSetForeground(XtDisplay(_linkedList->_widget),
				_linkedList->_bitmapGC, 1);
			XSetBackground(XtDisplay(_linkedList->_widget),
				_linkedList->_bitmapGC, 0);
			XSetPlaneMask(XtDisplay(_linkedList->_widget),
				_linkedList->_bitmapGC, 1);
		}

		XPutImage(XtDisplay(_linkedList->_widget), _localPixmap,
			_linkedList->_bitmapGC, theImage, 0, 0, 0, 0,
			_width, _height);

		_background = PaintsetCollection::white(
                        DefaultScreenOfDisplay(
			XtDisplay(_linkedList->_widget)));

		XDestroyImage(theImage);
	}
	else	// pixmap
	{
		XImage *theImage = XGetImage(_linkedList->_remoteDisplay,
			_remotePixmap, 0, 0, _width, _height, AllPlanes,
			ZPixmap);
		assert(theImage->depth == (int) _linkedList->_depth);

		if (theImage->depth == 8) translateImage(theImage);

		XSetPlaneMask(XtDisplay(_linkedList->_widget),
			_linkedList->_pixmapGC, AllPlanes);

		XPutImage(XtDisplay(_linkedList->_widget), _localPixmap,
			_linkedList->_pixmapGC, theImage, 0, 0, 0, 0,
			_width, _height);

		_background = (Pixel) theImage->data[0];

		XDestroyImage(theImage);
	}
}

/*
 * Display requests come here.
 */
void PixmapElement::display(unsigned long remotePlaneMask)
{
	unsigned long localPlaneMask;

	if (remotePlaneMask)	// Bitmap for overlay.
	{
		localPlaneMask = (unsigned long)
			_linkedList->_colorTranslator->translatePlane(
			(Pixel) remotePlaneMask);

		(_linkedList->_currentBitmapOverlay)
			[_linkedList->_colorTranslator->maskToInt(
			(Pixel) localPlaneMask)] = this;
	}
	else			// Pixmap or bitmap for main display.
	{
		localPlaneMask = 0;

		// Fresh display, no overlays.
		for (int i = 0; i <  (int) _linkedList->_depth; i++)
			(_linkedList->_currentBitmapOverlay)[i] =
				(PixmapElement *) NULL;
	}

	display(0, 0, _width, _height, localPlaneMask);

	if (_linkedList->_sdScrWin)
	{
		_linkedList->_sdScrWin->redrawAnnotation();

		if (remotePlaneMask == 0)
			_linkedList->_sdScrWin->setCornerFillColor(_background);
	}
}

/*
 * Exposures come here.
 */
void PixmapElement::display(int x, int y, unsigned int width,
		unsigned int height, unsigned long localPlaneMask)
{
	display(_linkedList->_widget, x, y, width, height,
		x, y, localPlaneMask);
}

/*
 * Annotate requests and the real work is done here.
 */
void PixmapElement::display(Widget widget, int xSrc, int ySrc,
	unsigned width, unsigned height, int xDst, int yDst,
	unsigned long localPlaneMask)
{
	width  = myMIN(width , myMIN(_width  - (unsigned int) xSrc,
		_linkedList->_width  - (unsigned int) xDst));
	height = myMIN(height, myMIN(_height - (unsigned int) ySrc,
		_linkedList->_height - (unsigned int) yDst));

	if (width <= 0 || height <= 0)
		return;

	if (localPlaneMask)	// Bitmap for overlay.
	{
		assert(_linkedList->_currentPixmap &&
			!_linkedList->_currentBitmap);

		XSetPlaneMask(XtDisplay(widget),
			_linkedList->_pixmapGC, localPlaneMask);
		XSetForeground(XtDisplay(widget),
			_linkedList->_pixmapGC, localPlaneMask);
		XSetBackground(XtDisplay(widget),
			_linkedList->_pixmapGC,              0);

		XCopyPlane(XtDisplay(widget), _localPixmap,
			XtWindow(widget), _linkedList->_pixmapGC,
			xSrc, ySrc, width, height, xDst, yDst, 1);
	}
	else if (_depth == 1)	// Bitmap for main display.
	{
		XSetPlaneMask(XtDisplay(widget),
			_linkedList->_pixmapGC, AllPlanes);
		XSetForeground(XtDisplay(widget), _linkedList->_pixmapGC,
			PaintsetCollection::black(DefaultScreenOfDisplay(
				XtDisplay(widget))));
		XSetBackground(XtDisplay(widget), _linkedList->_pixmapGC,
			PaintsetCollection::white(DefaultScreenOfDisplay(
				XtDisplay(widget))));

		XCopyPlane(XtDisplay(widget), _localPixmap,
			XtWindow(widget), _linkedList->_pixmapGC,
			xSrc, ySrc, width, height, xDst, yDst, 1);

		_linkedList->_currentBitmap = this;
		_linkedList->_currentPixmap = (PixmapElement *) NULL;
	}
	else	// Pixmap for main display.
	{
		XSetPlaneMask(XtDisplay(widget),
			_linkedList->_pixmapGC, AllPlanes);

		XCopyArea(XtDisplay(widget), _localPixmap,
			XtWindow(widget), _linkedList->_pixmapGC,
			xSrc, ySrc, width, height, xDst, yDst);

		_linkedList->_currentPixmap = this;
		_linkedList->_currentBitmap = (PixmapElement *) NULL;
	}
}

void PixmapElement::getPixmapSize()
{
	Window root;
	int x, y;
	unsigned int bwidth;

	XGetGeometry(_linkedList->_remoteDisplay, _remotePixmap, &root, &x, &y,
		&_width, &_height, &bwidth, &_depth);

	assert(_depth == _linkedList->_depth || _depth == 1);
}

void PixmapElement::translateImage(XImage *theImage)
{
	assert(theImage->width  == (int) _width );
	assert(theImage->height == (int) _height);
	assert(theImage->bits_per_pixel == (int) _linkedList->_depth);
	assert(_linkedList->_depth == 8);

	if (theImage->bytes_per_line == theImage->width)
	{
		_linkedList->_colorTranslator->translatePixels(
			(unsigned char *) theImage->data,
			(unsigned char *) theImage->data,
			theImage->width * theImage->height);
	}
	else
	{
		assert(4 - theImage->width % (theImage->bitmap_pad / 8)
			== theImage->bytes_per_line - theImage->width);

		unsigned char *ptr;
		int i;

		for (ptr = (unsigned char *) theImage->data, i = 0;
			i < theImage->height;
			ptr += theImage->bytes_per_line, i++)
		{
			_linkedList->_colorTranslator->translatePixels(
				ptr, ptr, theImage->width);
		}
	}
}
