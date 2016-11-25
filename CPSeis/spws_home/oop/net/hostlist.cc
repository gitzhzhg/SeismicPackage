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
#include <stdio.h>
#include "net/hostlist.hh"

#define True  1
#define False 0

void HostList::add(char *hostname, int port, NetEnv *netenv)
{
   HostNameElement *theElement = new HostNameElement(hostname, port, netenv);
   BaseLinkedList::add((Element *) theElement);
}

HostNameElement *HostList::top(void **ptr ) 
{ 
   HostNameElement* q= (HostNameElement*)BaseLinkedList::top(ptr);
   return q;
}

HostNameElement *HostList::bottom(void **ptr )
{
   HostNameElement* q= (HostNameElement*)BaseLinkedList::bottom(ptr);
   return q;
}


HostNameElement *HostList::next(void **ptr)
{
   HostNameElement* q= (HostNameElement*)BaseLinkedList::next(ptr);
   return q;
}

HostNameElement *HostList::prev(void **ptr)
{
   HostNameElement* q= (HostNameElement*)BaseLinkedList::prev(ptr);
   return q;
}

HostNameElement *HostList::current(void **ptr)
{
   HostNameElement* q= (HostNameElement*)BaseLinkedList::current(ptr);
   return q;
}

HostNameElement *HostList::find(char *hostname, int port, void **ptr)
{
 unsigned char found= False; // boolean value
 HostNameElement *retval= NULL;

 for (HostNameElement *q= top(ptr); (q && !found); q= next(ptr) ) {
        if ((strcmp(hostname, q->_hostname) == 0) && 
             port==q->_port)  {
              retval= q;
              found= True;
        } 
 }
 return retval;
}
