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
#ifndef _LL_TAG_HH
#define _LL_TAG_HH

#include "oprim/element.hh"
#include "oprim/ll_base.hh"
#include "vect/label_placement.hh"

#include <Xm/Xm.h>

#include <assert.h>
#include <iostream.h>

class TagElement : public Element
{
	friend class TagLinkedList;

	private:

		class Tag    *_tag ;
		class Vector *_vect;

		TagElement()
			{ assert(0); }
		TagElement(class Tag *tag)
			: _tag(tag), _vect((class Vector *) 0)
			{ /* just initializer */ }
		~TagElement()
			{ /* do nothing */ }
		int operator ==(void * const tag) const
			{ return((class Tag *) tag == _tag); }
		void print() const
			{ cout << " " << _tag; }
};

class TagLinkedList : public BaseLinkedList
{
	friend class Tag;

	public:

		TagLinkedList(class SeisPlot *sp,
			VectorLabelPlacement labelPlacement = DeadCenter,
			const char *font  = "fixed",
			const char *color = "black",
			Bool makeInvisibleWhenSelected = True,
			void (*overPrintFunc)(class Tag *, class Tag *, int) =
				(void (*)(class Tag *, class Tag *, int)) NULL,
				int offset = 0);
		~TagLinkedList();

		Tag *find(const Tag *tag, void **p = (void **) NULL)
		{
			TagElement *ptr = (TagElement *)
				BaseLinkedList::find((void *) tag, p);
			return (ptr ? ptr->_tag : (Tag *) NULL);
		}
		Tag *top    (void **p = (void **) NULL)
		{
			TagElement *ptr = (TagElement *)
				BaseLinkedList::top    (p);
			return (ptr ? ptr->_tag : (Tag *) NULL);
		}
		Tag *bottom (void **p = (void **) NULL)
		{
			TagElement *ptr = (TagElement *)
				BaseLinkedList::bottom (p);
			return (ptr ? ptr->_tag : (Tag *) NULL);
		}
		Tag *next   (void **p = (void **) NULL)
		{
			TagElement *ptr = (TagElement *)
				BaseLinkedList::next   (p);
			return (ptr ? ptr->_tag : (Tag *) NULL);
		}
		Tag *prev   (void **p = (void **) NULL)
		{
			TagElement *ptr = (TagElement *)
				BaseLinkedList::prev   (p);
			return (ptr ? ptr->_tag : (Tag *) NULL);
		}
		Tag *current(void **p = (void **) NULL)
		{
			TagElement *ptr = (TagElement *)
				BaseLinkedList::current(p);
			return (ptr ? ptr->_tag : (Tag *) NULL);
		}

		void setMakeInvisibleWhenSelected(Bool set)
			{ _makeInvisibleWhenSelected = set; }
		void setOverPrintFunc(void (*overPrintFunc)(Tag *, Tag *, int))
			{ _overPrintFunc = overPrintFunc; }
		void checkPositions();
		void hold()
			{ _holding = True; }
		void flush();
		void freeFont(XFontStruct *fontStruct);

		void addVectToTag(class Tag *tag, class Vector *vect);
		class Vector *getVectFromTag(class Tag    *t);
		class Tag    *getTagFromVect(class Vector *v);

	private:

		class PlotBase *_plot;
		class TagInform *_inform;
		VectorLabelPlacement _labelPlacement;
		int _offset;
		char *_font, *_color;
		Bool _makeInvisibleWhenSelected;
		void (*_overPrintFunc)(class Tag *, class Tag *, int);
		XFontStruct *_fontStruct;
		Bool _holding;

		typedef struct {
			Display *display;
			XFontStruct *fontStruct;
		} CMStruct;

		/*
		 * Accessed by Tag constructor and destructor.
		 */
		void add     (class Tag *tag);
		void addAtTop(class Tag *tag);
		void remove(class Tag *tag)
			{ BaseLinkedList::remove((void *) tag); }

		XFontStruct *getFontStruct();
		static void smartStrcpy(char **dst, const char *src);
		static void clientMessageFunc(void *cmStruct);
};

#endif	/* _LL_TAG_HH */
