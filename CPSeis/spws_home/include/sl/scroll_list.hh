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
#ifndef SCROLLLIST_HH
#define SCROLLLIST_HH

#include "oprim/element.hh"
#include "oprim/ll_base.hh"


class SLScrollWin;

class ScrollElement : public Element
{
  friend class ScrollList;
  protected:
     SLScrollWin *_ele;

     ScrollElement(SLScrollWin *ele) : _ele(ele) {}
     ~ScrollElement() {}
     int operator ==(void * const ele) const {return((SLScrollWin*) ele == _ele);}
     virtual void print() const {}
};



class ScrollList : public BaseLinkedList
{
  public:
        void add(SLScrollWin *ele);
        void remove(SLScrollWin *ele) {BaseLinkedList::remove((void*) ele);};
        ScrollElement* find(SLScrollWin *ele)
        { return( (ScrollElement *)BaseLinkedList::find((void *)ele));}
        SLScrollWin *top(void **ptr = (void **) 0);
        SLScrollWin *bottom(void **ptr = (void **) 0);
        SLScrollWin *next(void **ptr = (void **) 0);
        SLScrollWin *prev(void **ptr = (void **) 0);
        SLScrollWin *current(void **ptr = (void **) 0);
};

#endif
