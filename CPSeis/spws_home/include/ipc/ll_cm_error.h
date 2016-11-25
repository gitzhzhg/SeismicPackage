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
#ifndef LL_CM_ERROR_H
#define LL_CM_ERROR_H

#ifndef LL_BASE_H
#include "oprim/ll_base.hh"
#define LL_BASE_H
#endif

#ifndef CM_ERROR_ELEMENT_H
#include "ipc/cm_error_element.hh"
#define CM_ERROR_ELEMENT_H
#endif

#ifndef XM_H
#include <Xm/Xm.h>
#define XM_H
#endif

#ifndef ASSERT_H
#include <assert.h>
#define ASSERT_H
#endif

class ClientMessageErrorLinkedList : public BaseLinkedList
{
	private:

		int (*_oldErrorHandler)(Display *, XErrorEvent *);

	public:

		ClientMessageErrorLinkedList()
			{ assert(False); }
		ClientMessageErrorLinkedList(int (*)(Display *, XErrorEvent *));
		~ClientMessageErrorLinkedList();
		void add(Window, class ClientMessageBase *);
		void remove(Window window)
			{ BaseLinkedList::remove((void *) window); }
		class ClientMessageBase *find(Window);
		ClientMessageErrorElement *top    ()
			{ return((ClientMessageErrorElement *)
				BaseLinkedList::top    ()); }
		ClientMessageErrorElement *bottom ()
			{ return((ClientMessageErrorElement *)
			BaseLinkedList::bottom ()); }
		ClientMessageErrorElement *next   ()
			{ return((ClientMessageErrorElement *)
				BaseLinkedList::next   ()); }
		ClientMessageErrorElement *prev   ()
			{ return((ClientMessageErrorElement *)
				BaseLinkedList::prev   ()); }
		ClientMessageErrorElement *current()
			{ return((ClientMessageErrorElement *)
				BaseLinkedList::current()); }
		int (*getOldErrorHandler())(Display *, XErrorEvent *)
			{ return(_oldErrorHandler); }
};

#endif
