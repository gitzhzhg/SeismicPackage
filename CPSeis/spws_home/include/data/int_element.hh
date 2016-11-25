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
#ifndef INT_ELEMENT_H
#define INT_ELEMENT_H

#ifndef ELEMENT_H
#include "oprim/element.hh"
#define ELEMENT_H
#endif

#ifndef ASSERT_H
#include <assert.h>
#define ASSERT_H
#endif

#ifndef IOSTREAM_H
#include <iostream.h>
#define IOSTREAM_H
#endif

class IntElement : public Element
{
	friend class IntLinkedList;

	private:

		int _value;

		IntElement()	{ assert(0); }	// dummy
		IntElement(int value)	{ _value = value; }
		~IntElement()	// virtual in Element
			{ /* do nothing */ }
		int operator ==(void * const value) const
//			{ return((int) value == _value); } // virtual in Element
			{ return(value == (void*)_value);} // virtual in Element
		void print() const
			{ cout << " " << _value; }	// virtual in Element
};

#endif
