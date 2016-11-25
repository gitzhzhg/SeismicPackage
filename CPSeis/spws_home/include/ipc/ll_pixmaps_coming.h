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
#ifndef _LL_PIXMAPS_COMING_HH
#define _LL_PIXMAPS_COMING_HH

#include "oprim/element.hh"
#include "oprim/ll_base.hh"
#include <assert.h>
#include <iostream.h>

class PixmapsComingElement : public Element
{
	friend class PixmapsComingLinkedList;

	private:

		class PixmapElement *_pixmapElement;

		PixmapsComingElement()
			{ assert(0); }
		PixmapsComingElement(class PixmapElement *pixmapElement)
			: _pixmapElement(pixmapElement)
			{ /* do nothing */ }
		~PixmapsComingElement()
			{ /* do nothing */ }
		int operator ==(void * const pixmapElement) const
		{
			return((class PixmapElement *) pixmapElement
				== _pixmapElement);
		}
		void print() const
			{ cout << " " << _pixmapElement; }
};

class PixmapsComingLinkedList : public BaseLinkedList
{
	public:

		PixmapsComingLinkedList()
			{ /* do nothing */ }
		~PixmapsComingLinkedList()
			{ /* do nothing */ }
		void add(class PixmapElement *pixmapElement)
		{
			PixmapsComingElement *theElement =
				new PixmapsComingElement(pixmapElement);
			BaseLinkedList::add((Element *) theElement);
		}
		void remove(class PixmapElement *pixmapElement)
			{ BaseLinkedList::remove((void *) pixmapElement); }

		class PixmapElement *find(class PixmapsElement *pixmapElement)
		{
			PixmapsComingElement *ptr = (PixmapsComingElement *)
				BaseLinkedList::find((void *) pixmapElement);
			return (ptr ? ptr->_pixmapElement
				: (class PixmapElement *) NULL);
		}
		class PixmapElement *top    (void **p = (void **) NULL)
		{
			PixmapsComingElement *ptr = (PixmapsComingElement *)
				BaseLinkedList::top    (p);
			return (ptr ? ptr->_pixmapElement
				: (class PixmapElement *) NULL);
		}
		class PixmapElement *bottom (void **p = (void **) NULL)
		{
			PixmapsComingElement *ptr = (PixmapsComingElement *)
				BaseLinkedList::bottom (p);
			return (ptr ? ptr->_pixmapElement
				: (class PixmapElement *) NULL);
		}
		class PixmapElement *next   (void **p = (void **) NULL)
		{
			PixmapsComingElement *ptr = (PixmapsComingElement *)
				BaseLinkedList::next   (p);
			return (ptr ? ptr->_pixmapElement
				: (class PixmapElement *) NULL);
		}
		class PixmapElement *prev   (void **p = (void **) NULL)
		{
			PixmapsComingElement *ptr = (PixmapsComingElement *)
				BaseLinkedList::prev   (p);
			return (ptr ? ptr->_pixmapElement
				: (class PixmapElement *) NULL);
		}
		class PixmapElement *current(void **p = (void **) NULL)
		{
			PixmapsComingElement *ptr = (PixmapsComingElement *)
				BaseLinkedList::current(p);
			return (ptr ? ptr->_pixmapElement
				: (class PixmapElement *) NULL);
		}
};

#endif
