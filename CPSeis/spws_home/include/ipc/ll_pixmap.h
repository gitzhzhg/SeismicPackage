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
#ifndef LL_PIXMAP_H
#define LL_PIXMAP_H

#ifndef LL_BASE_H
#include "oprim/ll_base.hh"
#define LL_BASE_H
#endif

#ifndef HANDLES_ERRORS_H
#include "oprim/handles_errors.hh"
#define HANDLES_ERRORS_H
#endif

#ifndef PIXMAP_ELEMENT_H
#include "ipc/pixmap_element.hh"
#define PIXMAP_ELEMENT_H
#endif

#ifndef COLOR_XLATE_H
#include "ipc/color_xlate.hh"
#define COLOR_XLATE_H
#endif

#ifndef XM_H
#include <Xm/Xm.h>
#define XM_H
#endif

#ifndef ASSERT_H
#include <assert.h>
#define ASSERT_H
#endif

class PixmapLinkedList : public BaseLinkedList, public HandlesErrors
{
	friend class PixmapElement;	// So pixmap elements can get display
					// & color stuff and set _bitmapGC.
	private:

		Display *_remoteDisplay;
		Widget _widget;
		class SlaveDisplayScrWin *_sdScrWin;

		unsigned int _width ;
		unsigned int _height;
		unsigned int _depth ;

		ColorTranslate *_colorTranslator;

		GC _pixmapGC;
		GC _bitmapGC;

		PixmapElement *_currentPixmap;
		PixmapElement *_currentBitmap;
		PixmapElement **_currentBitmapOverlay;	// One for each plane.

		class SLApp *_application;
		class PixmapsComingLinkedList *_pixmapsComing;
		int _numPixmapsComing;

		// Using a callback for exposures and resizes does not seem to
		// get removed properly.  Motif bug???
		static void exposureEH(Widget, XtPointer, XEvent *, Boolean *);
		void exposure(XEvent *);
		static void resizeEH  (Widget, XtPointer, XEvent *, Boolean *);
		void resize  (int, int);

		void getWindowSize();

	public:

		PixmapLinkedList()	{ assert(False); }	// dummy
		PixmapLinkedList(Display *, Window, Widget, int, int,
			class SLApp * = NULL,
			class SlaveDisplayScrWin * = NULL);
		~PixmapLinkedList();
		void add(Pixmap);
		void remove(Pixmap);
		PixmapElement *find(Pixmap value)
			{
				return((PixmapElement *)
					BaseLinkedList::find ((void *) value));
			}
		PixmapElement *top    ()
			{ return((PixmapElement *) BaseLinkedList::top    ()); }
		PixmapElement *bottom ()
			{ return((PixmapElement *) BaseLinkedList::bottom ()); }
		PixmapElement *next   ()
			{ return((PixmapElement *) BaseLinkedList::next   ()); }
		PixmapElement *prev   ()
			{ return((PixmapElement *) BaseLinkedList::prev   ()); }
		PixmapElement *current()
			{ return((PixmapElement *) BaseLinkedList::current()); }
		void display(Pixmap, unsigned long = 0);
		void reDisplay(Widget, int, int, unsigned, unsigned, int, int);
		void windowSize(unsigned int width, unsigned int height)
			{ _width = width; _height = height; }
		void updateRGBs()
			{ _colorTranslator->updateRGBs(); }
		void setNumPixmapsComing(int numPixmapsComing);
		int getNumPixmapsComing()
			{ return _numPixmapsComing; }
};

#endif
