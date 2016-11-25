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
#ifndef _LL_BASE_DATA_HH
#define _LL_BASE_DATA_HH

#include "oprim/element.hh"
#include "oprim/ll_base.hh"
#include <assert.h>
#include <iostream.h>
#include <locale.h>

class BaseDataElement : public Element
{
	friend class BaseDataLinkedList;

	private:

		class BaseData *_baseData;

		BaseDataElement()
			{ assert(0); }
		BaseDataElement(class BaseData *baseData) : _baseData(baseData)
			{ /* do nothing */ }
		~BaseDataElement()
			{ /* do nothing */ }
		int operator ==(void * const baseData) const
			{ return((class BaseData*) baseData == _baseData); }
		void print() const
			{ cout << " " << _baseData; }
};

class BaseDataLinkedList : public BaseLinkedList
{
	public:

		BaseDataLinkedList()
			{ /* do nothing */ }
		~BaseDataLinkedList()
			{ /* do nothing */ }
		void add(class BaseData *baseData)
		{
			BaseDataElement *theElement =
				new BaseDataElement(baseData);
			BaseLinkedList::add(theElement);
		}
		void remove(class BaseData *baseData)
			{ BaseLinkedList::remove((void *) baseData); }

		class BaseData *find(class BaseData *baseData)
		{
			BaseDataElement *ptr = (BaseDataElement *)
				BaseLinkedList::find((void *) baseData);
			return (ptr ? ptr->_baseData : (class BaseData *) NULL);
		}
		class BaseData *top    (void **p = (void **) NULL)
		{
			BaseDataElement *ptr = (BaseDataElement *)
				BaseLinkedList::top    (p);
			return (ptr ? ptr->_baseData : (class BaseData *) NULL);
		}
		class BaseData *bottom (void **p = (void **) NULL)
		{
			BaseDataElement *ptr = (BaseDataElement *)
				BaseLinkedList::bottom (p);
			return (ptr ? ptr->_baseData : (class BaseData *) NULL);
		}
		class BaseData *next   (void **p = (void **) NULL)
		{
			BaseDataElement *ptr = (BaseDataElement *)
				BaseLinkedList::next   (p);
			return (ptr ? ptr->_baseData : (class BaseData *) NULL);
		}
		class BaseData *prev   (void **p = (void **) NULL)
		{
			BaseDataElement *ptr = (BaseDataElement *)
				BaseLinkedList::prev   (p);
			return (ptr ? ptr->_baseData : (class BaseData *) NULL);
		}
		class BaseData *current(void **p = (void **) NULL)
		{
			BaseDataElement *ptr = (BaseDataElement *)
				BaseLinkedList::current(p);
			return (ptr ? ptr->_baseData : (class BaseData *) NULL);
		}
};

#endif
