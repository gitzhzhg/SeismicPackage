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
#include "sl/delay_list.hh"
#include <X11/Intrinsic.h>
#include <Xm/Xm.h>
#include "sl/sl_delay.hh"


void DelayList::add(SLDelay *contain)
{
   DelayElement *theElement = new DelayElement(contain);
   BaseLinkedList::add((Element *) theElement);
}

SLDelay *DelayList::top(void **ptr ) 
{ 
   DelayElement* q= (DelayElement*)BaseLinkedList::top(ptr);
   return (q ? q->_ele : NULL);
}

SLDelay *DelayList::bottom(void **ptr )
{
   DelayElement* q= (DelayElement*)BaseLinkedList::bottom(ptr);
   return (q ? q->_ele : NULL);
}


SLDelay *DelayList::next(void **ptr)
{
   DelayElement* q= (DelayElement*)BaseLinkedList::next(ptr);
   return (q ? q->_ele : NULL);
}

SLDelay *DelayList::prev(void **ptr)
{
   DelayElement* q= (DelayElement*)BaseLinkedList::prev(ptr);
   return (q ? q->_ele : NULL);
}

SLDelay *DelayList::current(void **ptr)
{
   DelayElement* q= (DelayElement*)BaseLinkedList::current(ptr);
   return (q ? q->_ele : NULL);
}
