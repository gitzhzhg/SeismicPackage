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
#include <locale.h>
#include <stdlib.h>
#include "oprim/ll_charptr.hh"

void CharPtrLinkedList::add(char *p)
{
   CharPtrElement *theElement = new CharPtrElement(p);
   BaseLinkedList::add((Element *) theElement);
}

void CharPtrLinkedList::remove(char *p)
{
   BaseLinkedList::remove((void*)p);
}

char *CharPtrLinkedList::top(void **p)
{
   CharPtrElement* q= (CharPtrElement*)BaseLinkedList::top(p);
   return (q ? q->_p : NULL);
}

char *CharPtrLinkedList::find(char *p)
{
  CharPtrElement* q= (CharPtrElement*)BaseLinkedList::find((void*)p);
  return (q ? q->_p : NULL);
}

char *CharPtrLinkedList::bottom(void **x)
{
   CharPtrElement* q= (CharPtrElement*)BaseLinkedList::bottom(x);
   return (q ? q->_p : NULL);
}

char *CharPtrLinkedList::next(void **x)
{
   CharPtrElement* q= (CharPtrElement*)BaseLinkedList::next(x);
   return (q ? q->_p : NULL);
}

char *CharPtrLinkedList::prev(void **x)
{
   CharPtrElement* q= (CharPtrElement*)BaseLinkedList::prev(x);
   return (q ? q->_p : NULL);
}

char *CharPtrLinkedList::current(void **x)
{
   CharPtrElement* q= (CharPtrElement*)BaseLinkedList::current(x);
   return (q ? q->_p : NULL);
}

