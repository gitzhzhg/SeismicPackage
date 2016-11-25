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
#include "ipc/unique_string_list.hh"

#include <string.h>
#include <iostream.h>
#include <stdio.h>

StringAndListElement::StringAndListElement (char *string) :
  Element (),
  _string  (string),
  _list    (0)
{
}

StringAndListElement::~StringAndListElement ()
{
}

int StringAndListElement::operator == (void * const string) const
{
  int retval;

  if (!_string && !string) {
    retval = 1;
  }
  else if (!_string || !string) {
    retval = 0;
  }
  else {
    retval = !strcmp (_string, (const char *)string);
  }
  return retval;
}

void StringAndListElement::print () const
{
  cout << " " << _string;
}

void StringAndListElement::attachList (BaseLinkedList *list)
{
  _list = list;
}

BaseLinkedList *StringAndListElement::list ()
{
  return _list;
}



UniqueStringList::UniqueStringList () :
  BaseLinkedList ()
{
}

UniqueStringList::~UniqueStringList ()
{
}

void UniqueStringList::add (char *string)
{
  if (!find(string)) {
    StringAndListElement *element = new StringAndListElement (string);
    BaseLinkedList::add ((Element *)element);
  }
}

void UniqueStringList::remove (char *string)
{
  Element *element = BaseLinkedList::find ((void*)string);
  if (element) {
    BaseLinkedList::remove (element);
  }
}

char *UniqueStringList::find (char *string)
{
  char *retval;

  Element *element = BaseLinkedList::find ((void*)string);
  if (!element) {
    retval = NULL;
  }
  else {
    retval = ((StringAndListElement *)element)->_string;
  }
  return retval;
}

char *UniqueStringList::top (void **p)
{
  char *retval;

  Element *element = BaseLinkedList::top (p);
  if (!element) {
    retval = NULL;
  }
  else {
    retval = ((StringAndListElement *)element)->_string;
  }
  return retval;
}

char *UniqueStringList::next (void **p)
{
  char *retval;

  Element *element = BaseLinkedList::next (p);
  if (!element) {
    retval = NULL;
  }
  else {
    retval = ((StringAndListElement *)element)->_string;
  }
  return retval;
}

void UniqueStringList::attachList (BaseLinkedList *list)
{
  if (current()) {
    ((StringAndListElement *)current())->attachList (list);
  }
}

BaseLinkedList *UniqueStringList::list ()
{
  BaseLinkedList *retval;

  if (current()) {
    retval = ((StringAndListElement *)current())->list ();
  }
  else {
    retval = NULL;
  }
  return retval;
}
