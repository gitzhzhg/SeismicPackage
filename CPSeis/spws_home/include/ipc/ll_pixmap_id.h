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
#ifndef LL_PIXMAP_ID_H
#define LL_PIXMAP_ID_H

#ifndef LL_BASE_H
#include "oprim/ll_base.hh"
#define LL_BASE_H
#endif

#ifndef PIXMAP_ID_ELEMENT_H
#include "ipc/pixmap_id_element.hh"
#define PIXMAP_ID_ELEMENT_H
#endif

#ifndef XM_H
#include <Xm/Xm.h>
#define XM_H
#endif

class PixmapIDLinkedList : public BaseLinkedList
{
	public:

		PixmapIDLinkedList()   { /* do nothing */ }	// base class
		~PixmapIDLinkedList()  { /* do nothing */ }	// does the work
		void add(Pixmap pixmap)
			{
				PixmapIDElement *theElement =
					new PixmapIDElement(pixmap);
				BaseLinkedList::add((Element *) theElement);
			}
		void remove(Pixmap pixmap)
			{ BaseLinkedList::remove((void *) pixmap); }
		PixmapIDElement *find(Pixmap pixmap)
			{
				return((PixmapIDElement *)
					BaseLinkedList::find((void *) pixmap));
			}
		void addIfNotFound(Pixmap pixmap)
			{
				if (!find(pixmap))
					add(pixmap);
			}

		PixmapIDElement *top    ()
			{return((PixmapIDElement *) BaseLinkedList::top    ());}
		PixmapIDElement *bottom ()
			{return((PixmapIDElement *) BaseLinkedList::bottom ());}
		PixmapIDElement *next   ()
			{return((PixmapIDElement *) BaseLinkedList::next   ());}
		PixmapIDElement *prev   ()
			{return((PixmapIDElement *) BaseLinkedList::prev   ());}
		PixmapIDElement *current()
			{return((PixmapIDElement *) BaseLinkedList::current());}

		Pixmap topPixmapID    ()
			{
				PixmapIDElement *ptr = top    ();
				if (ptr)
					return(ptr->_pixmap);
				else
					return((Pixmap) NULL);
			}
		Pixmap bottomPixmapID ()
			{
				PixmapIDElement *ptr = bottom ();
				if (ptr)
					return(ptr->_pixmap);
				else
					return((Pixmap) NULL);
			}
		Pixmap nextPixmapID   ()
			{
				PixmapIDElement *ptr = next   ();
				if (ptr)
					return(ptr->_pixmap);
				else
					return((Pixmap) NULL);
			}
		Pixmap prevPixmapID   ()
			{
				PixmapIDElement *ptr = prev   ();
				if (ptr)
					return(ptr->_pixmap);
				else
					return((Pixmap) NULL);
			}
		Pixmap currentPixmapID()
			{
				PixmapIDElement *ptr = current();
				if (ptr)
					return(ptr->_pixmap);
				else
					return((Pixmap) NULL);
			}
};

#endif
