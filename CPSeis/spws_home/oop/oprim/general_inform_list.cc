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
#include "oprim/general_inform_list.hh"
#include "oprim/general_inform.hh"


GeneralInform *GeneralInformList::top(void **ptr)
{
   GeneralInformElement* q= (GeneralInformElement*)BaseLinkedList::top(ptr);
   return (q ? q->_inform : (GeneralInform*) 0 );
}
GeneralInform *GeneralInformList::find(GeneralInform *ptr)
{
   GeneralInformElement* q= (GeneralInformElement*)BaseLinkedList::find(ptr);
   return (q ? q->_inform : (GeneralInform*) 0 );
}
GeneralInform *GeneralInformList::bottom(void **ptr)
{
   GeneralInformElement* q= (GeneralInformElement*)BaseLinkedList::bottom(ptr);
   return (q ? q->_inform : (GeneralInform*) 0 );
}
GeneralInform *GeneralInformList::next(void **ptr)
{
   GeneralInformElement* q= (GeneralInformElement*)BaseLinkedList::next(ptr);
   return (q ? q->_inform : (GeneralInform*) 0 );
}
GeneralInform *GeneralInformList::prev(void **ptr)
{
   GeneralInformElement* q= (GeneralInformElement*)BaseLinkedList::prev(ptr);
   return (q ? q->_inform : (GeneralInform*) 0 );
}
GeneralInform *GeneralInformList::current(void **ptr)
{
   GeneralInformElement* q= (GeneralInformElement*)BaseLinkedList::current(ptr);
   return (q ? q->_inform : (GeneralInform*) 0 );
}





void GeneralInformList::callPreAction(GeneralObject *g)
{
  void *x;
  for(GeneralInform *q= top(&x); (q); q= next(&x))  q->preAction(g);
}

void GeneralInformList::callPostAction(GeneralObject *g)
{
  void *x;
  for(GeneralInform *q= top(&x); (q); q= next(&x))  q->postAction(g);
}
