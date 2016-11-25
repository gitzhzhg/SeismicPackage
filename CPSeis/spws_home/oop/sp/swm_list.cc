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
#include "sp/swm_list.hh"

SeisWinMan *SWMList::top(void **ptr) 
{ 
   SWMElement* q= (SWMElement*)BaseLinkedList::top(ptr);
   return (q ? q->_swm : NULL);
}

SeisWinMan *SWMList::bottom(void **ptr)
{
   SWMElement* q= (SWMElement*)BaseLinkedList::bottom(ptr);
   return (q ? q->_swm : NULL);
}


SeisWinMan *SWMList::next(void **ptr)
{
   SWMElement* q= (SWMElement*)BaseLinkedList::next(ptr);
   return (q ? q->_swm : NULL);
}

SeisWinMan *SWMList::prev(void **ptr)
{
   SWMElement* q= (SWMElement*)BaseLinkedList::prev(ptr);
   return (q ? q->_swm : NULL);
}

SeisWinMan *SWMList::current(void **ptr)
{
   SWMElement* q= (SWMElement*)BaseLinkedList::current(ptr);
   return (q ? q->_swm : NULL);
}

SeisZoomer *SWMList::currentZoomer(void **ptr)
{
   SWMElement* q= (SWMElement*)BaseLinkedList::current(ptr);
   return (q ? q->_zoomer : NULL);
}

void SWMList::setCurrentZoomer(void **ptr, SeisZoomer *zoomer)
{
   SWMElement* q= (SWMElement*)BaseLinkedList::current(ptr);
   if (q) q->_zoomer= zoomer;
}

SeisWinMan *SWMList::find(SeisWinMan *swm, void **ptr)
{
  SWMElement* q= (SWMElement*)BaseLinkedList::find((void*)swm, ptr);
  return (q ? q->_swm : NULL);
}
