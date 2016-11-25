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
#include "ipc/ipc_client_list.hh"
#include "ipc/ipc_client.hh"

#include <iostream.h>
#include <stdio.h>

IpcClientElement::IpcClientElement (IpcClient *client) :
  Element (),
  _client  (client)
{
}

IpcClientElement::~IpcClientElement ()
{
}

// Overloading the == operator is the key to the linked list working
int IpcClientElement::operator == (void * const client) const
{
  return _client == client;
}

void IpcClientElement::print () const
{
  cout << " " << _client;
}



IpcClientList::IpcClientList () :
  BaseLinkedList ()
{
}

IpcClientList::~IpcClientList ()
{
}

void IpcClientList::add (IpcClient *client)
{
  Element *element = new IpcClientElement (client);
  BaseLinkedList::add (element);
}

void IpcClientList::remove (IpcClient *client)
{
  BaseLinkedList::remove (client);
}

IpcClient *IpcClientList::find (IpcClient *client)
{
  IpcClient *retval;

  Element *element = BaseLinkedList::find ((void*)client);
  if (!element) {
    retval = NULL;
  }
  else {
    retval = ((IpcClientElement *)element)->_client;
  }
  return retval;
}

IpcClient *IpcClientList::top (void **p)
{
  IpcClient *retval;

  Element *element = BaseLinkedList::top (p);
  if (!element) {
    retval = NULL;
  }
  else {
    retval = ((IpcClientElement *)element)->_client;
  }
  return retval;
}

IpcClient *IpcClientList::next (void **p)
{
  IpcClient *retval;

  Element *element = BaseLinkedList::next (p);
  if (!element) {
    retval = NULL;
  }
  else {
    retval = ((IpcClientElement *)element)->_client;
  }
  return retval;
}
