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
#include "sl/sl_client_message.hh"
#include "wproc.h"

#include <iostream.h>
#include <assert.h>

SLClientMessageElement::SLClientMessageElement(SLClientMessage *cm)
	: Element(), _cm(cm)
{
	/* just initializers */
}

SLClientMessageElement::~SLClientMessageElement()
{
	/* do nothing */
}

int SLClientMessageElement::operator ==(void * const cm) const
{
	return((SLClientMessage *) cm == _cm);
}

void SLClientMessageElement::print() const
{
	cout << " " << _cm;
}

SLClientMessageLinkedList::SLClientMessageLinkedList()
	: BaseLinkedList()
{
	/* do nothing */
}

SLClientMessageLinkedList::~SLClientMessageLinkedList()
{
	/* do nothing */
}

void SLClientMessageLinkedList::add(SLClientMessage *cm)
{
	SLClientMessageElement *theElement = new SLClientMessageElement(cm);
	BaseLinkedList::add(theElement);
}

void SLClientMessageLinkedList::remove(SLClientMessage *cm)
{
	BaseLinkedList::remove((void *) cm);
}

SLClientMessage *SLClientMessageLinkedList::find(SLClientMessage *cm, void **p)
{
	SLClientMessageElement *ptr = (SLClientMessageElement *)
		BaseLinkedList::find((void *) cm, p);
	return (ptr ? ptr->_cm : (SLClientMessage *) NULL);
}

SLClientMessage *SLClientMessageLinkedList::top    (void **p)
{
	SLClientMessageElement *ptr = (SLClientMessageElement *)
		BaseLinkedList::top    (p);
	return (ptr ? ptr->_cm : (SLClientMessage *) NULL);
}

SLClientMessage *SLClientMessageLinkedList::bottom (void **p)
{
	SLClientMessageElement *ptr = (SLClientMessageElement *)
		BaseLinkedList::bottom (p);
	return (ptr ? ptr->_cm : (SLClientMessage *) NULL);
}

SLClientMessage *SLClientMessageLinkedList::next   (void **p)
{
	SLClientMessageElement *ptr = (SLClientMessageElement *)
		BaseLinkedList::next   (p);
	return (ptr ? ptr->_cm : (SLClientMessage *) NULL);
}

SLClientMessage *SLClientMessageLinkedList::prev   (void **p)
{
	SLClientMessageElement *ptr = (SLClientMessageElement *)
		BaseLinkedList::prev   (p);
	return (ptr ? ptr->_cm : (SLClientMessage *) NULL);
}

SLClientMessage *SLClientMessageLinkedList::current(void **p)
{
	SLClientMessageElement *ptr = (SLClientMessageElement *)
		BaseLinkedList::current(p);
	return (ptr ? ptr->_cm : (SLClientMessage *) NULL);
}

/*
 * Static variables
 */
Widget SLClientMessage::_messageWidget = (Widget) 0;
SLClientMessageLinkedList SLClientMessage::_list;

SLClientMessage::SLClientMessage(const Widget w, const char *name,
	void (*func)(void *), void *data, SLClientMessage **ptrToMe,
	int numEvents)
	: SLDelay(name), _func(func), _data(data), _ptrToMe(ptrToMe),
	  _numEvents(numEvents), _i_am_forgotten(0)
{
	assert(_numEvents > 0);

	if (!_messageWidget)
	{
		Widget parent = get_shell_child(get_toplevel_shell(w));
		assert(XtIsComposite(parent));

		_messageWidget = XtVaCreateManagedWidget("spws_junk",
			xmLabelWidgetClass, parent,
			XmNmappedWhenManaged, False,
			XmNheight           , 1    ,
			XmNwidth            , 1    ,
			NULL);

		XtAddEventHandler(_messageWidget, NoEventMask, True,
			clientMessageEH, (XtPointer) 0);

		XtAddCallback(_messageWidget, XmNdestroyCallback, 
			destroyCB, (XtPointer) 0);

		assert(sizeof(long) == sizeof(SLClientMessage *));
	}

	XClientMessageEvent event;
	event.type      = ClientMessage;
	event.display   = XtDisplay(_messageWidget);
	event.window    = XtWindow (_messageWidget);
	event.format    = 32;
	event.data.l[0] = (long) this;

	XSendEvent(XtDisplay(_messageWidget), XtWindow (_messageWidget),
		True, NoEventMask, (XEvent *) &event);

	/*
	 * SLDelay stuff.
	 * Not using since using static widget.
	 *
	 * setTopWidget(_messageWidget);
	 * make        (_messageWidget);
	 */

	_list.add(this);
}

SLClientMessage::~SLClientMessage()
{
	callNotifyComplex(0);

	if (_func)
		(*_func)(_data);

	if (_ptrToMe)
		*_ptrToMe = (SLClientMessage *) NULL;

	_list.remove(this);
}

void SLClientMessage::clientMessageEH(Widget, XtPointer, XEvent *event,
	Boolean *)
{
	SLClientMessage *cm = (SLClientMessage *)
		((XClientMessageEvent *) event)->data.l[0];

	cm->checkIfDone();
}

void SLClientMessage::destroyCB(Widget, XtPointer, XtPointer)
{
	_messageWidget = (Widget) 0;
}

void SLClientMessage::checkIfDone()
{
	if ((--_numEvents == 0) || _i_am_forgotten)
	{
		delete this;
	}
	else
	{
		XClientMessageEvent event;
		event.type      = ClientMessage;
		event.display   = XtDisplay(_messageWidget);
		event.window    = XtWindow (_messageWidget);
		event.format    = 32;
		event.data.l[0] = (long) this;

		XSendEvent(XtDisplay(_messageWidget), XtWindow(_messageWidget),
			True, NoEventMask, (XEvent *) &event);
	}
}

void SLClientMessage::forgetIt()
{
	_func    = (void (*)(void *))   NULL;
	_ptrToMe = (SLClientMessage **) NULL;
	_i_am_forgotten = 1;

	/*
	 * Avoid leak.
	 */
	if (_data)
		delete _data;
}

void SLClientMessage::cleanup()
{
	SLClientMessage *ptr;
	// Initialized to eliminate compiler warning.
	SLClientMessage *next_ptr = (SLClientMessage *) 0;
	void *p;

	for (ptr = _list.top(&p); ptr; ptr = next_ptr)
	{
		// destructor will remove ptr
		next_ptr = _list.next(&p);

//		ptr->forgetIt();
		delete ptr;
	}
}
