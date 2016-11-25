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
#ifndef IPC_QUIT_H
#define IPC_QUIT_H

#ifndef CM_BASE_H
#include "ipc/cm_base.hh"
#define CM_BASE_H
#endif

#ifndef ERROR_HANDLER_H
#include "sl/error_handler.hh"
#define ERROR_HANDLER_H
#endif

#ifndef XM_H
#include <Xm/Xm.h>
#define XM_H
#endif

#ifndef ASSERT_H
#include <assert.h>
#define ASSERT_H
#endif

class IpcQuit : public HandlesErrors
{
	private:

		static int _count;

		ClientMessageBase *_ipc;
		int _waitFor;
		Bool _exitOnDelete;
		IpcQuit **_zeroWhenDestroyed;

		enum { _TIMEOUT = 5000 };	// 5 sec.
		XtIntervalId _timer;

		static void receivedIntPtr (long *, int, XtPointer);
		static void receivedCharPtr(char *,      XtPointer);

		static void timerCB(XtPointer , XtIntervalId *);

		void receivedMessage(long *, int);
		void errorHandler(HandlesErrors *, int);

	public:

		IpcQuit()	{ assert(False); }
		IpcQuit(ClientMessageBase *, int, Bool,
			IpcQuit ** = (IpcQuit **) NULL);
		~IpcQuit();
		void setExitOnDelete()
			{ _exitOnDelete = True; }
};

#endif
