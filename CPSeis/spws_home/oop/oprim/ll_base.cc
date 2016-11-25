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
#include <string.h>
#include "oprim/ll_base.hh"

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

#ifndef LOCALE_H
#include <locale.h>
#define LOCALE_H
#endif

BaseLinkedList::BaseLinkedList()
{
	_top     = (Element *) 0;
	_bottom  = (Element *) 0;
	_current = (Element *) 0;

	_count = 0;
}

BaseLinkedList::~BaseLinkedList()
{
	recursiveDestroy(_top);
}

void BaseLinkedList::add(Element *theElement)
{
	// make sure element not already in a linked list
	assert(theElement->_next == (Element *) 1);

	theElement->_prev = _bottom;
	theElement->_next = (Element *) 0;

	if (_bottom)
		_bottom->_next = theElement;
	else
		_top           = theElement;

	_bottom = theElement;

	if (!_current)
		_current = _top;

	_count++;
}

void BaseLinkedList::addBeforeCurrent(Element *theElement, void **p)
{
	if (_count)
	{
		Element *ptr;

		if (p)
			ptr = (Element *) *p;
		else
			ptr = _current;

		assert(ptr);

		theElement->_next = ptr;
		theElement->_prev = ptr->_prev;

		if (_top == ptr)
			_top = theElement;
		else
			ptr->_prev->_next = theElement;

		ptr->_prev = theElement;

		_count++;
	}
	else
	{
		add(theElement);
	}
}

void BaseLinkedList::addAfterCurrent(Element *theElement, void **p)
{
	if (_count)
	{
		Element *ptr;

		if (p)
			ptr = (Element *) *p;
		else
			ptr = _current;

		assert(ptr);

		theElement->_next = ptr->_next;
		theElement->_prev = ptr;

		if (_bottom == ptr)
			_bottom = theElement;
		else
			ptr->_next->_prev = theElement;

		ptr->_next = theElement;

		_count++;
	}
	else
	{
		add(theElement);
	}
}

void BaseLinkedList::remove(void *value)
{
	Element *ptr;
	assert(ptr = find(value));

	removeByPtr(ptr);
}

void BaseLinkedList::removeByPtr(Element *ptr)
{
	if (ptr == _current)
		if (_current->_next)
			_current = _current->_next;
		else
			_current = _current->_prev;

	if (ptr->_next)
		ptr->_next->_prev = ptr->_prev;
	else
		_bottom = ptr->_prev;

	if (ptr->_prev)
		ptr->_prev->_next = ptr->_next;
	else
		_top = ptr->_next;

	delete ptr;

	_count--;
}

Element *BaseLinkedList::find(void *value, void **p)
{
	for (Element *ptr = _top; ptr; ptr = ptr->_next)
		if (*ptr == value)
		{
			if (p != NULL)
			{
				*p = (void *) ptr;
			}
			else
			{
				/*
				 * Should update _current, but that might
				 * break old code.  Use moveTo instead!
				 */
			}

			return(ptr);
		}

	/* p is unchanged if not found */
	return((Element *) 0);
}

Element *BaseLinkedList::moveTo(void *value, void **p)
{
	for (Element *ptr = _top; ptr; ptr = ptr->_next)
		if (*ptr == value)
		{
			if (p != NULL)
			{
				*p = (void *) ptr;
			}
			else
			{
				 _current = ptr;
			}

			return(ptr);
		}

	/* p is unchanged if not found */
	return((Element *) 0);
}

void BaseLinkedList::recursiveDestroy(Element *theElement)
{
	if (theElement)
	{
		recursiveDestroy(theElement->_next);

		delete theElement;
	}
}

void BaseLinkedList::printList()
{
	Element *ptr;

	cout << "forward:  ";
	for (ptr = _top   ; ptr; ptr = ptr->_next)
		ptr->print();
	cout << endl;

	cout << "backward:  ";
	for (ptr = _bottom; ptr; ptr = ptr->_prev)
		ptr->print();
	cout << endl;
}

Element *BaseLinkedList::next(void **ptr)
{
	if (ptr == NULL)
	{
		if (_current && _current->_next)
			return(_current = _current->_next);
		else
			return((class Element *) NULL);
	}
	else
	{
		if (*ptr && ((Element *) *ptr)->_next)
			return((Element *)
				(*ptr = (void *) ((Element *) *ptr)->_next));
		else
			return((Element *) (*ptr = (void *) NULL));
	}
}

Element *BaseLinkedList::prev(void **ptr)
{
	if (ptr == NULL)
	{
		if (_current && _current->_prev)
			return(_current = _current->_prev);
		else
			return((Element *) NULL);
	}
	else
	{
		if (*ptr && ((Element *) *ptr)->_prev)
			return((Element *)
				(*ptr = (void *) ((Element *) *ptr)->_prev));
		else
			return((Element *) (*ptr = (void *) NULL));
	}
}
