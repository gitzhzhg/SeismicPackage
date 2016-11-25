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
#ifndef _LL_PICK_BASE_H
#define _LL_PICK_BASE_H

/*
 * This is a little sneaky.  PickBaseLinkedList is actually a linked
 * list of linked lists.  The higher level linked list (PickBaseLinkedList)
 * is really just a linked list of PlotBases which is used to sort the
 * PickBases by PlotBases.  The lower level linked list
 * (PickBaseSamePlotBaseLinkedList) is actually the linked list of
 * PickBases.  The linked list traversal methods (top, bottom, next, & prev)
 * just traverse PickBases for a single PlotBase.
 */

#include "oprim/element.hh"
#include "oprim/ll_base.hh"

#include <Xm/Xm.h>
#include <iostream.h>
#include <assert.h>

class PickBaseSamePlotBaseElement : public Element
{
	friend class PickBaseSamePlotBaseLinkedList;

	private:

		class PickBase *_pickBase;

		PickBaseSamePlotBaseElement()
			{ assert(0); }
		PickBaseSamePlotBaseElement(class PickBase *pickBase)
			{ _pickBase = pickBase; }
		~PickBaseSamePlotBaseElement()
			{ /* do nothing */ }
		int operator ==(void * const pickBase) const
			{ return((class PickBase *) pickBase == _pickBase); }
		void print() const
			{ cout << " " << _pickBase; }
};

class PickBaseSamePlotBaseLinkedList : public BaseLinkedList
{
	friend class PickBaseElement;
	friend class PickBaseLinkedList;

	private:

		PickBaseSamePlotBaseLinkedList()
			{ /* do nothing */ }
		~PickBaseSamePlotBaseLinkedList()
			{ /* do nothing */ }
		void add(class PickBase *pickBase);
		void remove(class PickBase *pickBase)
			{ BaseLinkedList::remove((void *) pickBase); }
		class PickBase *find(class PickBase *pickBase, void **p = NULL);
		class PickBase *top    (void **p = NULL);
		class PickBase *bottom (void **p = NULL);
		class PickBase *next   (void **p = NULL);
		class PickBase *prev   (void **p = NULL);
		class PickBase *current(void **p = NULL);
};

class PickBaseElement : public Element
{
	friend class PickBaseLinkedList;

	private:

		class PlotBase *_plot;
		PickBaseSamePlotBaseLinkedList *_pickBases;
		XtTranslations _oldTranslations;

		PickBaseElement()
			{ assert(0); }
		PickBaseElement(class PlotBase *plot,
			XtTranslations translations);
		~PickBaseElement();
		int operator ==(void * const plot) const
			{ return((class PlotBase *) plot == _plot); }
		void print() const
			{ cout << " " << _plot; }

};

typedef class PickBase *(PickBaseSamePlotBaseLinkedList::*posFunc)(void **p);

class PickBaseLinkedList : public BaseLinkedList
{
	public:

		PickBaseLinkedList(Widget widget);
		~PickBaseLinkedList()
			{ /* do nothing */ }
		void add(class PickBase *pickBase);
		void remove(class PickBase *pickBase);
		class PickBase *find(class PickBase *pickBase, void **p = NULL);
		class PickBase *top    (class PlotBase *plotBase,
			void **p = NULL);
		class PickBase *bottom (class PlotBase *plotBase,
			void **p = NULL);
		class PickBase *next   (class PlotBase *plotBase,
			void **p = NULL);
		class PickBase *prev   (class PlotBase *plotBase,
			void **p = NULL);
		class PickBase *current(class PlotBase *plotBase,
			void **p = NULL);
		int getPlots(class PlotBase ***plotBases);
		void changePlotBase(class PlotBase *from, class PlotBase *to);

	private:

		XtTranslations _translations;

		static void staticPressAction  (Widget w, XEvent *event,
			char **params, Cardinal *num_params);
		static void staticMotionAction (Widget w, XEvent *event,
			char **params, Cardinal *num_params);
		static void staticReleaseAction(Widget w, XEvent *event,
			char **params, Cardinal *num_params);
		static int parseInt(char *string);
		class PickBase *getPickBase(Window window);

		class PickBase *position(class PlotBase *plotBase, posFunc func,
			void **p = NULL);
};

class SimplePickBaseElement : public Element
{
	friend class SimplePickBaseLinkedList;

	private:

		class PickBase *_pickBase;

		SimplePickBaseElement()
			{ assert(0); }
		SimplePickBaseElement(class PickBase *pickBase)
			{ _pickBase = pickBase; }
		~SimplePickBaseElement()
			{ /* do nothing */ }
		int operator ==(void * const pickBase) const
			{ return((class PickBase *) pickBase == _pickBase); }
		void print() const
			{ cout << " " << _pickBase; }
};

class SimplePickBaseLinkedList : public BaseLinkedList
{
	public:

		SimplePickBaseLinkedList()
			{ /* do nothing */ }
		~SimplePickBaseLinkedList()
			{ /* do nothing */ }
		void add(class PickBase *pickBase);
		void remove(class PickBase *pickBase)
			{ BaseLinkedList::remove((void *) pickBase); }
		class PickBase *find(class PickBase *pickBase, void **p = NULL);
		class PickBase *top    (void **p = NULL);
		class PickBase *bottom (void **p = NULL);
		class PickBase *next   (void **p = NULL);
		class PickBase *prev   (void **p = NULL);
		class PickBase *current(void **p = NULL);
};

#endif
