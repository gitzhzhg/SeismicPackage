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
#include "ipc/cm_base.hh"

#ifndef STRING_H
#include <string.h>
#define STRING_H
#endif

#ifndef ASSERT_H
#include <assert.h>
#define ASSERT_H
#endif

#ifndef STDLIB_H
#include <stdlib.h>
#define STDLIB_H
#endif

#ifndef XM_H
#include <Xm/Xm.h>
#define XM_H
#endif

#ifndef XPROTO_H
#include <X11/Xproto.h>
#define XPROTO_H
#endif

#ifndef STDIO_H
#include <stdio.h>
#define STDIO_H
#endif

/* static data elements */

const char *const ClientMessageBase::_commandLineOption = "-SPWS_CM";

ClientMessageErrorLinkedList ClientMessageBase::_error(
	&ClientMessageBase::staticErrorHandler);

/* end of static data elements */

ClientMessageBase::ClientMessageBase(XtPointer client)
{
	_client = client;

	_eventQue = (XClientMessageEvent **) NULL;
	_numEventsQueued = 0;

	_newDisplay  = False;
	_newTopLevel = False;

#ifndef NDEBUG
	_receiveInt      = (void (*)(int    ,      XtPointer)) False;
	_receiveFloat    = (void (*)(float  ,      XtPointer)) False;
	_receiveIntPtr   = (void (*)(long  *, int, XtPointer)) False;
	_receiveFloatPtr = (void (*)(float *, int, XtPointer)) False;
	_receiveCharPtr  = (void (*)(char  *,      XtPointer)) False;

	_display   = (Display *) False;
	_outWindow = (Window   ) False;
	_inWidget  = (Widget   ) False;

	_clientMessageAtom[0] = (Atom) False;
#endif
}

ClientMessageBase::ClientMessageBase()
{
	assert(False);
}

ClientMessageBase::~ClientMessageBase()
{
	XtRemoveEventHandler(_inWidget, NoEventMask, True, clientMessageEH,
		(XtPointer) this);

	XSync(_display, False);

	_error.remove(_outWindow);

	if (_newTopLevel)
		XtDestroyWidget(XtParent(_inWidget));
	else
		XtDestroyWidget(_inWidget);

	if (_newDisplay)
		XtCloseDisplay(_display);

#ifndef NDEBUG		// In case memory is not overwritten, assert in send
	_display = (Display *) False;	// will catch this.
#endif
}

void ClientMessageBase::initClientMessage()
{
	static const char *const _clientMessageName[] =
	{
		"SPWS_IPC_SLAVE_INFO",
		"SPWS_IPC_INT",
		"SPWS_IPC_FLOAT",
		"SPWS_IPC_INT_PTR",
		"SPWS_IPC_FLOAT_PTR",
		"SPWS_IPC_CHAR_PTR"
	};
	int i;

	assert(_display && _inWidget && !_clientMessageAtom[0]);
	assert(sizeof(_clientMessageName) / sizeof(char *)
		== (size_t) _NUM_CM_TYPES);	// sizes the same

	for (i = 0; i < _NUM_CM_TYPES; i++)
		_clientMessageAtom[i] = XInternAtom(_display,
			_clientMessageName[i], False);

	XtAddEventHandler(_inWidget, NoEventMask, True, clientMessageEH,
		(XtPointer) this);
}

void ClientMessageBase::clientMessageEH(Widget, XtPointer client, XEvent *event,
	Boolean *)
{
	ClientMessageBase *obj = (ClientMessageBase *) client;

	obj->clientMessageReceived((XClientMessageEvent *) event);
}

void ClientMessageBase::clientMessageReceived(XClientMessageEvent *event)
{
	static void (ClientMessageBase::*function[])(XClientMessageEvent *)
		= {	&ClientMessageBase::slaveInfoReceived,
			&ClientMessageBase::intReceived      ,
			&ClientMessageBase::floatReceived    ,
			&ClientMessageBase::intPtrReceived   ,
			&ClientMessageBase::floatPtrReceived ,
			&ClientMessageBase::charPtrReceived };
	int i;

	assert(sizeof(function) / sizeof(function[0])
		== (size_t) _NUM_CM_TYPES);	// sizes the same
	assert((sizeof(long) | sizeof(int) | sizeof(float)
		| sizeof(Display *) | sizeof(Window)) == (size_t) 4);
	assert(_clientMessageAtom[0]);	// make sure initialized

	for (i = 0; i < _NUM_CM_TYPES; i++)
		if (_clientMessageAtom[i] == event->message_type)
		{
			(this->*function[i])(event);
			return;
		}

	assert(False);	// should never get here
}

void ClientMessageBase::slaveInfoReceived(XClientMessageEvent *event)
{
	assert(!_outWindow);

	_outWindow = (Window) event->data.l[0];
	_error.add(_outWindow, this);

	if (_numEventsQueued)
	{
		for (int i = 0; i < _numEventsQueued; i++)
		{
			_eventQue[i]->window = _outWindow;

			XSendEvent(_display, _outWindow, True, NoEventMask,
				(XEvent *) _eventQue[i]);

			delete _eventQue[i];
		}

		free(_eventQue);
	}
}

void ClientMessageBase::intReceived(XClientMessageEvent *event)
{
	assert(_receiveInt);

	_receiveInt((int) event->data.l[0], _client);
}

void ClientMessageBase::floatReceived(XClientMessageEvent *event)
{
	float value;

	assert(_receiveFloat);

	memcpy((void *) &value, (void *) event->data.l, sizeof(long));
	_receiveFloat(value, _client);
}

void ClientMessageBase::intPtrReceived(XClientMessageEvent *event)
{
	assert(_receiveIntPtr);
	assert((int) event->data.l[0] >= 1 && (int) event->data.l[0] <= 4);

	_receiveIntPtr((long *) (event->data.l + 1), (int) event->data.l[0],
		_client);
}

void ClientMessageBase::floatPtrReceived(XClientMessageEvent *event)
{
	assert(_receiveFloatPtr);
	assert((int) event->data.l[0] >= 1 && (int) event->data.l[0] <= 4);

	_receiveFloatPtr((float *) (event->data.l + 1), (int) event->data.l[0],
		_client);
}

void ClientMessageBase::charPtrReceived(XClientMessageEvent *event)
{
	assert(_receiveCharPtr);
	assert(strlen(event->data.b) <= (size_t) 19);

	_receiveCharPtr(event->data.b, _client);
}

void ClientMessageBase::sendClientMessage(XClientMessageEvent *event, Bool self)
{
	event->type = ClientMessage;
	assert(_clientMessageAtom[0]);

	if (self)
	{
		assert(XtWindow(_inWidget));

		event->display = XtDisplay(_inWidget);
		event->window  = XtWindow (_inWidget);
	}
	else
	{
		assert(_display);

		/*
		 * Not really needed, server will set
		 * properly since receiving process
		 * probably has different value for
		 * Display * of same X-server.
		 */
		event->display = _display;
		event->window  = _outWindow;
	}

	if (self)
	{
		XSendEvent(XtDisplay(_inWidget), XtWindow(_inWidget),
			True, NoEventMask, (XEvent *) event);
	}
	else if (_outWindow)
	{
		XSendEvent(_display, _outWindow, True, NoEventMask,
			(XEvent *) event);
	}
	else
	{
		// ANSI realloc should be malloc if _eventQue == NULL,
		// but sun does not work like this.
		if (_eventQue)
			assert(_eventQue = (XClientMessageEvent **) realloc(
				(void *) _eventQue,
				(size_t) (_numEventsQueued + 1) *
				sizeof(XClientMessageEvent *)));
		else
			assert(_eventQue = (XClientMessageEvent **) malloc (
//				(size_t) (_numEventsQueued + 1) *
				sizeof(XClientMessageEvent *)));

		 _eventQue[_numEventsQueued  ] = new XClientMessageEvent;
		*_eventQue[_numEventsQueued++] = *event;
	}
}

void ClientMessageBase::send(Window window)
{
	XClientMessageEvent event;

	event.message_type = _clientMessageAtom[_SLAVE_INFO_TYPE];
	event.format       = 32;
	event.data.l[0]    = (long) window;

	sendClientMessage(&event, False);
}

void ClientMessageBase::send(int value, Bool self)
{
	XClientMessageEvent event;

	event.message_type = _clientMessageAtom[_INT_TYPE];
	event.format       = 32;
	event.data.l[0]    = (long) value;

	sendClientMessage(&event, self);
}

void ClientMessageBase::send(float value, Bool self)
{
	XClientMessageEvent event;

	event.message_type = _clientMessageAtom[_FLOAT_TYPE];
	event.format       = 32;
	memcpy((void *) event.data.l, (void *) &value, sizeof(long));

	sendClientMessage(&event, self);
}

void ClientMessageBase::send(long *values, int num, Bool self)
{
	XClientMessageEvent event;

	assert(num >= 1 && num <= 4);

	event.message_type = _clientMessageAtom[_INT_PTR_TYPE];
	event.format       = 32;
	event.data.l[0]    = (long)num;
	memcpy((void *)(event.data.l + 1), (void *)values,
		(size_t) num * sizeof(long));

	sendClientMessage(&event, self);
}

void ClientMessageBase::send(float *values, int num, Bool self)
{
	XClientMessageEvent event;

	assert(num >= 1 && num <= 4);

	event.message_type = _clientMessageAtom[_FLOAT_PTR_TYPE];
	event.format       = 32;
	event.data.l[0]    = (long) num;
	memcpy((void *) (event.data.l + 1), (void *) values,
		(size_t) num * sizeof(long));

	sendClientMessage(&event, self);
}

void ClientMessageBase::send(char *string, Bool self)
{
	XClientMessageEvent event;
	assert(strlen(string) <= (size_t) 19);

	event.message_type = _clientMessageAtom[_CHAR_PTR_TYPE];
	event.format       = 8;
	strcpy(event.data.b, string);

	sendClientMessage(&event, self);
}

void ClientMessageBase::receive(void (*function)(int, XtPointer))
{
	_receiveInt = function;
}

void ClientMessageBase::receive(void (*function)(float, XtPointer))
{
	_receiveFloat = function;
}

void ClientMessageBase::receive(void (*function)(long *, int, XtPointer))
{
	_receiveIntPtr = function;
}

void ClientMessageBase::receive(void (*function)(float *, int, XtPointer))
{
	_receiveFloatPtr = function;
}

void ClientMessageBase::receive(void (*function)(char *, XtPointer))
{
	_receiveCharPtr = function;
}

int ClientMessageBase::staticErrorHandler(Display *display, XErrorEvent *event)
{

	class ClientMessageBase *obj = _error.find((Window) event->resourceid);

        //Bad window caught
	if (event->error_code == BadWindow
		&& event->request_code == X_SendEvent
		&& obj)
	{
		obj->handleError(_ERROR_PARTNER_DIED);
	}
	else
	{
          (*_error.getOldErrorHandler())(display, event);
	}

	return(0);
}

void ClientMessageBase::errorHandler(HandlesErrors * /*ptr*/, int errorNum)
{
	switch (errorNum)
	{
		case _ERROR_PARTNER_DIED:
			delete this;
			break;
		default:
			assert(False);
			break;
	}
}
