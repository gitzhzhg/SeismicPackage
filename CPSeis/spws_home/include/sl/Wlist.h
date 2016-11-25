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
#ifndef WLIST_H
#define WLIST_H


#include "oprim/element.hh"
#include "oprim/ll_base.hh"
#include <X11/Intrinsic.h>
#include <Xm/Xm.h>



class WidgetElement : public Element
{
  friend class Wlist;
  protected:
     Widget _w;

     WidgetElement(Widget w) : _w(w) {}
     ~WidgetElement() {}
     int operator ==(void * const w) const
                        { return((Widget) w == _w); }
     virtual void print() const {}
};



class Wlist : public BaseLinkedList
{
  protected:
        static void wDestroyCallback(Widget, XtPointer, XtPointer);
        void wDestroy(Widget);
  public:
        void add(Widget w);
        void remove(Widget w);
        WidgetElement* find(Widget w)
           { return( (WidgetElement *)BaseLinkedList::find((void *) w)); }
        Widget top();
        Widget bottom();
        Widget next();
        Widget prev();
        Widget current();

};


#endif
