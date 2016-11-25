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
#ifndef _SL_CLIENT_MESSAGE_HH
#define _SL_CLIENT_MESSAGE_HH

#include "sl/sl_delay.hh"
#include "oprim/element.hh"
#include "oprim/ll_base.hh"

#include <Xm/Xm.h>
#include <Xm/Label.h>

class SLClientMessageElement : public Element
{
	friend class SLClientMessageLinkedList;

	private:

		class SLClientMessage *_cm;

		SLClientMessageElement(class SLClientMessage *cm);
		virtual ~SLClientMessageElement();
		virtual int operator ==(void * const _cm) const;
		virtual void print() const;
};

class SLClientMessageLinkedList : public BaseLinkedList
{
	public:

		SLClientMessageLinkedList();
		virtual ~SLClientMessageLinkedList();

		void add(class SLClientMessage *cm);
		void remove(class SLClientMessage *cm);

		class SLClientMessage *find(class SLClientMessage *cm,
			void **p = (void **) NULL);

		class SLClientMessage *top    (void **p = (void **) NULL);
		class SLClientMessage *bottom (void **p = (void **) NULL);
		class SLClientMessage *next   (void **p = (void **) NULL);
		class SLClientMessage *prev   (void **p = (void **) NULL);
		class SLClientMessage *current(void **p = (void **) NULL);
};

class SLClientMessage : public SLDelay
{
	friend class shutUpPrivateDestructorWarning;

	public:

		SLClientMessage(const Widget w, const char *name,
			void (*func)(void *) = (void (*)(void *)) NULL,
			void *data = (void *) NULL,
			SLClientMessage **ptrToMe = (SLClientMessage **) NULL,
			int numEvents = 1);
		virtual WidgetClass topClass()
			{ return xmLabelWidgetClass; }
		void forgetIt();
		static void cleanup();

	private:

		void (*_func)(void *);
		void *_data;
		SLClientMessage **_ptrToMe;
		int _numEvents, _i_am_forgotten;
		static Widget _messageWidget;
		static SLClientMessageLinkedList _list;

		~SLClientMessage();	/* Can only be deleted by itself. */
		static void clientMessageEH(Widget, XtPointer, XEvent *event,
			Boolean *);
		static void       destroyCB(Widget, XtPointer, XtPointer);
		void checkIfDone();

		SLClientMessage() : SLDelay((char *) NULL)
			{ /* private, no access to default constructor */ }
		SLClientMessage(SLClientMessage &) : SLDelay((char *) NULL)
			{ /* private, no access to copy constructor */ }
		SLClientMessage& operator=(SLClientMessage &p)
			{ /* private, no access to = */ return p; }
};

#endif /* _SL_CLIENT_MESSAGE_HH */





