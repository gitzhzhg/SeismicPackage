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
#include "sl/wlist.hh"
#include <X11/Intrinsic.h>
#include <Xm/Xm.h>


void Wlist::add(Widget w)
{
   WidgetElement *theElement = new WidgetElement(w);

   BaseLinkedList::add((Element *) theElement);

   XtAddCallback( w, XmNdestroyCallback, wDestroyCallback, this);
}

void Wlist::wDestroyCallback(Widget w, XtPointer udata, XtPointer)
{
   Wlist *obj = (Wlist*)udata;
   obj->wDestroy(w);
}

void Wlist::remove(Widget w) 
{ 
  XtRemoveCallback( w, XmNdestroyCallback, wDestroyCallback, this);
  BaseLinkedList::remove((void*) w); 
}

void Wlist::wDestroy(Widget w)
{
  remove(w);
}

Widget Wlist::top() 
{ 
   WidgetElement* q= (WidgetElement*)BaseLinkedList::top();
   return (q ? q->_w : NULL);
}

Widget Wlist::bottom()
{
   WidgetElement* q= (WidgetElement*)BaseLinkedList::bottom();
   return (q ? q->_w : NULL);
}


Widget Wlist::next()
{
   WidgetElement* q= (WidgetElement*)BaseLinkedList::next();
   return (q ? q->_w : NULL);
}

Widget Wlist::prev()
{
   WidgetElement* q= (WidgetElement*)BaseLinkedList::prev();
   return (q ? q->_w : NULL);
}

Widget Wlist::current()
{
   WidgetElement* q= (WidgetElement*)BaseLinkedList::current();
   return (q ? q->_w : NULL);
}
