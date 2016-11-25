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
#ifndef ERRLIST_H
#define ERRLIST_H


#include "oprim/element.hh"
#include "oprim/ll_base.hh"
#include <X11/Intrinsic.h>
#include <Xm/Xm.h>



class EboxElement : public Element
{
  friend class ErrList;
  protected:
     Widget _shell;
     Widget _ebox;

     EboxElement(Widget shell, Widget ebox) : _shell(shell), _ebox(ebox) {}
     ~EboxElement() {}
     int operator ==(void * const w) const
                        { return((Widget) w == _shell); }
     virtual void print() const {}

  public:
     Widget eBox()   {return _ebox;}
     Widget eShell() {return _shell;}
};



class ErrList : public BaseLinkedList
{
  protected:
        static void shellDestroyCallback(Widget, XtPointer, XtPointer);
        void shellDestroy(Widget);
  public:
        void add(Widget ebox, Widget shell);
        void remove(Widget shell) 
                { BaseLinkedList::remove((void*) shell); };
        EboxElement* find(Widget shell)
              { return( (EboxElement *)BaseLinkedList::find((void *) shell)); }
};


#endif
