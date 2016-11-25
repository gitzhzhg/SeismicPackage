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
#ifndef CONTAINERLIST_h
#define CONTAINERLIST_h

#include <X11/Intrinsic.h>
#include <Xm/Xm.h>
#include "oprim/element.hh"
#include "oprim/ll_base.hh"


class SLShellContainer;

class ContainerElement : public Element
{
  friend class ContainerList;
  protected:
     SLShellContainer *_contain;
     Boolean          _put_sep_after;

     ContainerElement(SLShellContainer *contain) : 
                       _contain(contain), _put_sep_after(False) {}
     ~ContainerElement() {}
     int operator ==(void * const contain) const
                        { return((SLShellContainer*) contain == _contain); }
     virtual void print() const {}
};



class ContainerList : public BaseLinkedList
{
  public:
        void add(SLShellContainer *contain);
        void putSeparatorAtBottomElement();
        void putSeparatorAfter(SLShellContainer *contain);
        Boolean needsSeparator();
        void remove(SLShellContainer *contain) 
                { BaseLinkedList::remove((void*) contain); };
        ContainerElement* find(SLShellContainer *contain)
        { return( (ContainerElement *)BaseLinkedList::find((void *)contain));}
        SLShellContainer *top();
        SLShellContainer *bottom();
        SLShellContainer *next();
        SLShellContainer *prev();
        SLShellContainer *current();

};

#endif
