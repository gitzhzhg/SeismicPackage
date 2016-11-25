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
#ifndef LL_CHARPTR_H
#define LL_CHARPTR_H


#include "oprim/ll_base.hh"
#include "oprim/element.hh"
#include <assert.h>

class CharPtrElement : public Element
{
	friend class CharPtrLinkedList;

	private:
		char *_p;
		CharPtrElement(char *p) : _p(p) {}
		~CharPtrElement() {}
		int operator ==(void * const p) const
			{ return((char*) p == _p); }
                virtual void print() const {}
};




class CharPtrLinkedList : public BaseLinkedList
{
	public:
           CharPtrLinkedList()   { /* do nothing */ }	// base class
	   ~CharPtrLinkedList()  { /* do nothing */ }	// does the work
           void add(char *);
           void remove(char *p);
	   char *find(char *p);
           char *top    (void **p = (void **) NULL);
	   char *bottom (void **p = (void **) NULL);
	   char *next   (void **p = (void **) NULL);
	   char *prev   (void **p = (void **) NULL);
	   char *current(void **p = (void **) NULL);
};

#endif
