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
#ifndef _LL_DATA_USER_H
#define _LL_DATA_USER_H

#include "oprim/element.hh"
#include "oprim/ll_base.hh"
#include <assert.h>
#include <iostream.h>
#include <locale.h>

class DataUserElement : public Element
{
	friend class DataUserLinkedList;

	private:

		class DataUser *_dataUser;

		DataUserElement()
			{ assert(0); }
		DataUserElement(class DataUser *dataUser) : _dataUser(dataUser)
			{ /* do nothing */ }
		~DataUserElement()
			{ /* do nothing */ }
		int operator ==(void * const dataUser) const
			{ return((class DataUser*) dataUser == _dataUser); }
		void print() const
			{ cout << " " << _dataUser; }
};

class DataUserLinkedList : public BaseLinkedList
{
	public:

		DataUserLinkedList()
			{ /* do nothing */ }
		~DataUserLinkedList()
			{ /* do nothing */ }
		void add(class DataUser *dataUser)
		{
			DataUserElement *theElement =
				new DataUserElement(dataUser);
			BaseLinkedList::add((Element *) theElement);
		}
		void remove(class DataUser *dataUser)
			{ BaseLinkedList::remove((void *) dataUser); }

		class DataUser *find(class DataUser *dataUser)
		{
			DataUserElement *ptr = (DataUserElement *)
				BaseLinkedList::find((void *) dataUser);
			return (ptr ? ptr->_dataUser : (class DataUser *) NULL);
		}
		class DataUser *top    (void **p = (void **) NULL)
		{
			DataUserElement *ptr = (DataUserElement *)
				BaseLinkedList::top    (p);
			return (ptr ? ptr->_dataUser : (class DataUser *) NULL);
		}
		class DataUser *bottom (void **p = (void **) NULL)
		{
			DataUserElement *ptr = (DataUserElement *)
				BaseLinkedList::bottom (p);
			return (ptr ? ptr->_dataUser : (class DataUser *) NULL);
		}
		class DataUser *next   (void **p = (void **) NULL)
		{
			DataUserElement *ptr = (DataUserElement *)
				BaseLinkedList::next   (p);
			return (ptr ? ptr->_dataUser : (class DataUser *) NULL);
		}
		class DataUser *prev   (void **p = (void **) NULL)
		{
			DataUserElement *ptr = (DataUserElement *)
				BaseLinkedList::prev   (p);
			return (ptr ? ptr->_dataUser : (class DataUser *) NULL);
		}
		class DataUser *current(void **p = (void **) NULL)
		{
			DataUserElement *ptr = (DataUserElement *)
				BaseLinkedList::current(p);
			return (ptr ? ptr->_dataUser : (class DataUser *) NULL);
		}
};

#endif
