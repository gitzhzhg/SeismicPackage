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
#include "oprim/general_list.hh"

GeneralObject *GeneralList::top(void **ptr) 
{ 
   GeneralElement* q= (GeneralElement*)BaseLinkedList::top(ptr);
   return (q ? q->_gen : (GeneralObject*) 0 );
}

GeneralObject *GeneralList::bottom(void **ptr)
{
   GeneralElement* q= (GeneralElement*)BaseLinkedList::bottom(ptr);
   return (q ? q->_gen : (GeneralObject*) 0 );
}


GeneralObject *GeneralList::next(void **ptr)
{
   GeneralElement* q= (GeneralElement*)BaseLinkedList::next(ptr);
   return (q ? q->_gen : (GeneralObject*) 0 );
}

GeneralObject *GeneralList::prev(void **ptr)
{
   GeneralElement* q= (GeneralElement*)BaseLinkedList::prev(ptr);
   return (q ? q->_gen : (GeneralObject*) 0 );
}

GeneralObject *GeneralList::current(void **ptr)
{
   GeneralElement* q= (GeneralElement*)BaseLinkedList::current(ptr);
   return (q ? q->_gen : (GeneralObject*) 0 );
}

void *GeneralList::currentUserData(void **ptr)
{
   GeneralElement* q= (GeneralElement*)BaseLinkedList::current(ptr);
   return (q ? q->_user_data : (GeneralObject*) 0 );
}

GeneralObject *GeneralList::find(GeneralObject *obj, void **ptr)
{
  GeneralElement* q= (GeneralElement*)BaseLinkedList::find((void*)obj, ptr);
  return (q ? q->_gen : (GeneralObject*) 0 );
}
