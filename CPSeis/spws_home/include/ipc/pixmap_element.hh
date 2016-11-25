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
#ifndef PIXMAP_ELEMENT_H
#define PIXMAP_ELEMENT_H	// at beginning because pixmap_element.hh &
				// ll_pixmap.hh include each other

#ifndef ELEMENT_H
#include "oprim/element.hh"
#define ELEMENT_H
#endif

#ifndef HANDLES_ERRORS_H
#include "oprim/handles_errors.hh"
#define HANDLES_ERRORS_H
#endif

#ifndef LL_PIXMAP_H
#include "ipc/ll_pixmap.hh"
#define LL_PIXMAP_H
#endif

#ifndef XM_H
#include <Xm/Xm.h>
#define XM_H
#endif

#ifndef ASSERT_H
#include <assert.h>
#define ASSERT_H
#endif

#ifndef IOSTREAM_H
#include <iostream.h>
#define IOSTREAM_H
#endif

class PixmapLinkedList;

class PixmapElement : public Element, public HandlesErrors
{
	friend class PixmapLinkedList;

	private:

		PixmapLinkedList *_linkedList;	// for display & color info
		Pixmap _remotePixmap;		// other process, master
		Pixmap _localPixmap;		// this process, slave

		unsigned int _width ;
		unsigned int _height;
		unsigned int _depth ;

		Pixel _background;		// for SLScrollWin

		PixmapElement()		{ assert(False); }	//dummy
		PixmapElement(Pixmap, PixmapLinkedList *);
		~PixmapElement();
		void copyPixmap();
		int operator ==(void * const value) const
			{ return((Pixmap) value == _remotePixmap); }
		void print() const
			{ cout << " " << _remotePixmap; }
		void display(unsigned long = 0);
		void display(int, int, unsigned int, unsigned int,
			unsigned long = 0);
		void display(Widget, int, int, unsigned int, unsigned int,
			int, int, unsigned long = 0);
		void getPixmapSize();
		void translateImage(XImage *);
};

#endif
