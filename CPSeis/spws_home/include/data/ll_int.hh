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
#ifndef LL_INT_H
#define LL_INT_H

#ifndef LL_BASE_H
#include "oprim/ll_base.hh"
#define LL_BASE_H
#endif

#ifndef INT_ELEMENT_H
#include "oprim/int_element.hh"
#define INT_ELEMENT_H
#endif

class IntLinkedList : public BaseLinkedList
{
	public:

		IntLinkedList()   { /* do nothing */ }	// base class
		~IntLinkedList()  { /* do nothing */ }	// does the work
		void add(int);
		void remove(int value)
			{ BaseLinkedList::remove((void *) value); }
		IntElement *find(int value)
			{
				return((IntElement *)
					BaseLinkedList::find((void *) value));
			}
		IntElement *top    (void **ptr = (void **) NULL)
			{ return((IntElement *) BaseLinkedList::top    (ptr)); }
		IntElement *bottom (void **ptr = (void **) NULL)
			{ return((IntElement *) BaseLinkedList::bottom (ptr)); }
		IntElement *next   (void **ptr = (void **) NULL)
			{ return((IntElement *) BaseLinkedList::next   (ptr)); }
		IntElement *prev   (void **ptr = (void **) NULL)
			{ return((IntElement *) BaseLinkedList::prev   (ptr)); }
		IntElement *current(void **ptr = (void **) NULL)
			{ return((IntElement *) BaseLinkedList::current(ptr)); }
};

#endif
