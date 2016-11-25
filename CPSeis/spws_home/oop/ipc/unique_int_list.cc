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
#include "ipc/unique_int_list.hh"

#include <string.h>
#include <iostream.h>
#include <stdio.h>

IntAndListElement::IntAndListElement (int value) :
  Element (),
  _value  (value),
  _list    (0)
{
}

IntAndListElement::~IntAndListElement ()
{
}

int IntAndListElement::operator == (void * const value) const
{
  int retval;

  if (!value) {
    retval = 1;
  }
  else {
    retval = _value == *((int *)value);
  }
  return retval;
}

void IntAndListElement::print () const
{
  cout << " " << _value;
}

void IntAndListElement::attachList (BaseLinkedList *list)
{
  _list = list;
}

BaseLinkedList *IntAndListElement::list ()
{
  return _list;
}



UniqueIntList::UniqueIntList () :
  BaseLinkedList ()
{
}

UniqueIntList::~UniqueIntList ()
{
}

void UniqueIntList::add (int value)
{
  if (!find(value)) {
    IntAndListElement *element = new IntAndListElement (value);
    BaseLinkedList::add ((Element *)element);
  }
}

void UniqueIntList::remove (int value)
{
  Element *element = BaseLinkedList::find ((void*)(&value));
  if (element) {
    BaseLinkedList::remove (element);
  }
}

int *UniqueIntList::find (int value)
{
  int *retval;

  Element *element = BaseLinkedList::find ((void*)(&value));
  if (!element) {
    retval = NULL;
  }
  else {
    retval = &(((IntAndListElement *)element)->_value);
  }
  return retval;
}

int *UniqueIntList::top (void **p)
{
  int *retval;

  Element *element = BaseLinkedList::top (p);
  if (!element) {
    retval = NULL;
  }
  else {
    retval = &(((IntAndListElement *)element)->_value);
  }
  return retval;
}

int *UniqueIntList::next (void **p)
{
  int *retval;

  Element *element = BaseLinkedList::next (p);
  if (!element) {
    retval = NULL;
  }
  else {
    retval = &(((IntAndListElement *)element)->_value);
  }
  return retval;
}

void UniqueIntList::attachList (BaseLinkedList *list)
{
  if (current()) {
    ((IntAndListElement *)current())->attachList (list);
  }
}

BaseLinkedList *UniqueIntList::list ()
{
  BaseLinkedList *retval;

  if (current()) {
    retval = ((IntAndListElement *)current())->list ();
  }
  else {
    retval = NULL;
  }
  return retval;
}
