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
#include "ipc/ll_cm_error.hh"

#ifndef XM_H
#include <Xm/Xm.h>
#define XM_H
#endif

#ifndef XPROTO_H
#include <X11/Xproto.h>
#define XPROTO_H
#endif

ClientMessageErrorLinkedList::ClientMessageErrorLinkedList(
	int (*errorHandler)(Display *, XErrorEvent *))
{
	_oldErrorHandler = XSetErrorHandler(errorHandler);
}

ClientMessageErrorLinkedList::~ClientMessageErrorLinkedList()
{
	XSetErrorHandler(_oldErrorHandler);
}

void ClientMessageErrorLinkedList::add(Window window, ClientMessageBase *cm)
{
	ClientMessageErrorElement *theElement =
		new ClientMessageErrorElement(window, cm);

	BaseLinkedList::add((Element *) theElement);
}

ClientMessageBase *ClientMessageErrorLinkedList::find(Window window)
{
	Element *element;

	if (element = BaseLinkedList::find((void *) window))
		return(((ClientMessageErrorElement *) element)->_cm);
	else
		return((ClientMessageBase *) NULL);
}
