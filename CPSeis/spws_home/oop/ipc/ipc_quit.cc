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
#include "ipc/ipc_quit.hh"

#ifndef XM_H
#include <Xm/Xm.h>
#define XM_H
#endif

#ifndef STDLIB_H
#include <stdlib.h>
#define STDLIB_H
#endif

#ifndef ASSERT_H
#include <assert.h>
#define ASSERT_H
#endif

int IpcQuit::_count = 0;

IpcQuit::IpcQuit(ClientMessageBase *ipc, int waitFor, Bool exitOnDelete,
	IpcQuit **zeroWhenDestroyed)
{
	_ipc			= ipc;
	_waitFor		= waitFor;
	_exitOnDelete		= exitOnDelete;
	_zeroWhenDestroyed	= zeroWhenDestroyed;

	_ipc->changeClientData((XtPointer) this);

	_ipc->receive(receivedIntPtr );
	_ipc->receive(receivedCharPtr);

	_ipc->setErrorHandler(this);

	_count++;

	_timer = XtAppAddTimeOut(
		XtWidgetToApplicationContext(_ipc->getWidget()),
			(long) _TIMEOUT, timerCB, (XtPointer) this);
}

IpcQuit::~IpcQuit()
{
	delete _ipc;

	if (_zeroWhenDestroyed)
		*_zeroWhenDestroyed = (IpcQuit *) NULL;

	_count--;

	if (!_count && _exitOnDelete)
		exit(0);
}


void IpcQuit::receivedIntPtr (long *message, int num, XtPointer client)
{
	IpcQuit *obj = (IpcQuit *) client;
	obj->receivedMessage(message, num);
}

void IpcQuit::receivedCharPtr(char *, XtPointer)
{
	// do nothing
}

void IpcQuit::timerCB(XtPointer client, XtIntervalId *)
{
	IpcQuit *obj = (IpcQuit *) client;
	delete obj;
}

void IpcQuit::receivedMessage(long *message, int)
{
	if (message[0] == _waitFor)
	{
		XtRemoveTimeOut(_timer);
		delete this;
	}
}

void IpcQuit::errorHandler(HandlesErrors * /*ptr*/, int errorNum)
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
