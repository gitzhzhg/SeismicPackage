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
#ifndef SPLIST_H
#define SPLIST_H


#include "oprim/element.hh"
#include "oprim/ll_base.hh"
#include <X11/Intrinsic.h>
#include <Xm/Xm.h>

class SeisPlot;
class SeisPick;


class SPElement : public Element
{
  friend class SPList;
  protected:
     SeisPlot *_sp;
     void     *_user_data;

     SPElement(SeisPlot *sp, void *user_data =NULL) : 
                     _sp(sp), _user_data(user_data) {}
     ~SPElement() {}
     int operator ==(void * const sp) const { return((SeisPlot*) sp == _sp); }
     virtual void print() const {}
};


class SPList : public BaseLinkedList
{
  public:
   void add(SeisPlot *sp, void *user_data =NULL)
               { SPElement *theElement = new SPElement(sp);
                 BaseLinkedList::add((Element *) theElement); 
                 theElement->_user_data= user_data; }

   void remove(SeisPlot *sp) { BaseLinkedList::remove((void*)sp); };
   SeisPlot *top(void **ptr = (void **) 0);
   SeisPlot *find(SeisPlot *sp, void **ptr = (void **) 0);
   SeisPlot *bottom(void **ptr = (void **) 0);
   SeisPlot *next(void **ptr = (void **) 0);
   SeisPlot *prev(void **ptr = (void **) 0);
   SeisPlot *current(void **ptr = (void **) 0);
   void *currentUserData(void **ptr = (void **) 0);
};


#endif
