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
#ifndef CM_BASE_H
#define CM_BASE_H

#ifndef HANDLES_ERRORS_H
#include "oprim/handles_errors.hh"
#define HANDLES_ERRORS_H
#endif

#ifndef LL_CM_ERROR_H
#include "ipc/ll_cm_error.hh"
#define LL_CM_ERROR_H
#endif

#ifndef XM_H
#include <Xm/Xm.h>
#define XM_H
#endif

class ClientMessageBase : public HandlesErrors
{
	private:

		XtPointer _client;

		XClientMessageEvent **_eventQue;
		int _numEventsQueued;

		void (*_receiveInt     )(int    ,      XtPointer);
		void (*_receiveFloat   )(float  ,      XtPointer);
		void (*_receiveIntPtr  )(long  *, int, XtPointer);
		void (*_receiveFloatPtr)(float *, int, XtPointer);
		void (*_receiveCharPtr )(char  *,      XtPointer);

		enum {	_SLAVE_INFO_TYPE,
			_INT_TYPE       ,
			_FLOAT_TYPE     ,
			_INT_PTR_TYPE   ,
			_FLOAT_PTR_TYPE ,
			_CHAR_PTR_TYPE  ,
			_NUM_CM_TYPES   	/* _NUM_CM_TYPES must be last */
		};

		Atom _clientMessageAtom[_NUM_CM_TYPES];

		static void clientMessageEH(Widget, XtPointer, XEvent *,
			Boolean *);
		void clientMessageReceived(XClientMessageEvent *);
		void slaveInfoReceived    (XClientMessageEvent *);
		void intReceived          (XClientMessageEvent *);
		void floatReceived        (XClientMessageEvent *);
		void intPtrReceived       (XClientMessageEvent *);
		void floatPtrReceived     (XClientMessageEvent *);
		void charPtrReceived      (XClientMessageEvent *);

		void sendClientMessage(XClientMessageEvent *, Bool);

		static int staticErrorHandler(Display *, XErrorEvent *);
		void errorHandler(HandlesErrors *, int);

	protected:

		Display	*_display;	// send & rcv on same display
		Widget	_inWidget;
		Window	_outWindow;
		Bool _newDisplay;
		Bool _newTopLevel;

		static const char *const _commandLineOption;
		static ClientMessageErrorLinkedList _error;

		ClientMessageBase(XtPointer);
		ClientMessageBase();	// dummy

		void initClientMessage();	// init Atoms & add EH
		void send(Window);		// send slave info

	public:

		virtual ~ClientMessageBase();

		void send(int  , Bool self = False);
		void send(float, Bool self = False);
		void send(long  *, int, Bool self = False);
		void send(float *, int, Bool self = False);
		void send(char *, Bool self = False);
		void receive(void (*)(int  ,        XtPointer));
		void receive(void (*)(float,        XtPointer));
		void receive(void (*)(long  *, int, XtPointer));
		void receive(void (*)(float *, int, XtPointer));
		void receive(void (*)(char  *,      XtPointer));

		void changeClientData(XtPointer client)
			{ _client = client; }
		Widget getWidget()
			{ return _inWidget; }
};

#endif
