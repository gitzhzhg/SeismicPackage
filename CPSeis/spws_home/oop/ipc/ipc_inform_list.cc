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
#include "ipc/ipc_inform_list.hh"
#include <string.h>
#include <iostream.h>
#include <locale.h>

IpcInformElement::IpcInformElement (IpcInform *inform) :
  Element (),
  _inform  (inform)
{
}

int IpcInformElement::operator == (void * const inform) const
{
  return (int)((IpcInform*)inform == _inform);
}


void IpcInformElement::print () const
{
  cout << " " << _inform << endl;
}

IpcInformList::IpcInformList () :
  BaseLinkedList()
{
}

void IpcInformList::add (IpcInform *inform)
{
  Element *element = new IpcInformElement (inform);
  BaseLinkedList::add (element);
}

void IpcInformList::remove (IpcInform *inform)
{
  BaseLinkedList::remove ((void*)inform);
}

IpcInform *IpcInformList::find (IpcInform *inform)
{
  Element *element = BaseLinkedList::find ((void*)inform);
  if (!element) return NULL;
  return ((IpcInformElement*)element)->_inform;
}

IpcInform *IpcInformList::top (void **p)
{
  Element *element = BaseLinkedList::top (p);
  if (!element) return NULL;
  return ((IpcInformElement*)element)->_inform;       \
}

IpcInform *IpcInformList::bottom (void **p)
{
  Element *element = BaseLinkedList::bottom (p);
  if (!element) return NULL;
  return ((IpcInformElement*)element)->_inform;       \
}

IpcInform *IpcInformList::next (void **p)
{
  Element *element = BaseLinkedList::next (p);
  if (!element) return NULL;
  return ((IpcInformElement*)element)->_inform;       \
}

IpcInform *IpcInformList::prev (void **p)
{
  Element *element = BaseLinkedList::prev (p);
  if (!element) return NULL;
  return ((IpcInformElement*)element)->_inform;       \
}

IpcInform *IpcInformList::current (void **p)
{
  Element *element = BaseLinkedList::current (p);
  if (!element) return NULL;
  return ((IpcInformElement*)element)->_inform;       \
}
