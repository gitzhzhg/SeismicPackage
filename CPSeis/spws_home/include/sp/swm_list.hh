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
#ifndef SWMLIST_H
#define SWMLIST_H


#include "oprim/element.hh"
#include "oprim/ll_base.hh"
#include <X11/Intrinsic.h>
#include <Xm/Xm.h>

class SeisWinMan;
class SeisZoomer;


class SWMElement : public Element
{
  friend class SWMList;
  protected:
     SeisWinMan *_swm;
     SeisZoomer *_zoomer;

     SWMElement(SeisWinMan *swm, SeisZoomer *zoomer =NULL) : 
                     _swm(swm), _zoomer(zoomer) {}
     ~SWMElement() {}
     int operator ==(void * const swm) const 
                         { return((SeisWinMan*) swm == _swm); }
     virtual void print() const {}
};


class SWMList : public BaseLinkedList
{
  public:
   void add(SeisWinMan *swm, SeisZoomer *zoomer =NULL)
               { SWMElement *theElement = new SWMElement(swm);
                 BaseLinkedList::add((Element *) theElement); 
                 theElement->_zoomer= zoomer; }

   void remove(SeisWinMan *swm) { BaseLinkedList::remove((void*)swm); };
   SeisWinMan *top(void **ptr = (void **) 0);
   SeisWinMan *find(SeisWinMan *swm, void **ptr = (void **) 0);
   SeisWinMan *bottom(void **ptr = (void **) 0);
   SeisWinMan *next(void **ptr = (void **) 0);
   SeisWinMan *prev(void **ptr = (void **) 0);
   SeisWinMan *current(void **ptr = (void **) 0);
   SeisZoomer *currentZoomer(void **ptr = (void **) 0);
   void setCurrentZoomer(void **ptr = (void **) 0, SeisZoomer *zoomer =NULL);
};


#endif
