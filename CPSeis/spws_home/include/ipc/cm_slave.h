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
#ifndef CM_SLAVE_H
#define CM_SLAVE_H

#ifndef CM_BASE_H
#include "ipc/cm_base.hh"
#define CM_BASE_H
#endif

#ifndef XM_H
#include <Xm/Xm.h>
#define XM_H
#endif

class ClientMessageSlave : public ClientMessageBase
{
	private:

		static Bool parseCommandLine(int *, char **, char **, Window *);
		static int strcmpCaseInsensitive(const char *s1,
			const char *s2);

	public:

		ClientMessageSlave(Widget, XtPointer, int *, char **);
		ClientMessageSlave();	// dummy
		~ClientMessageSlave();

		Display *getMasterDisplay()	{ return(_display  ); }
		Window   getMasterWindow ()	{ return(_outWindow); }
};

#endif
