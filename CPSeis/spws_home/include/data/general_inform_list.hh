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
#ifndef INFORM_LIST_HH
#define INFORM_LIST_HH

#include "oprim/element.hh"
#include "oprim/ll_base.hh"

#include "oprim/general_object.hh"

class GeneralInform;


class GeneralInformElement : public Element
{
  friend class GeneralInformList;
  protected:
     GeneralInform *_inform;
     GeneralInformElement(GeneralInform *inform) : _inform(inform) {}
     int operator ==(void * const inform) const 
                 { return((GeneralInform*) inform == _inform); }
     virtual void print() const {}
};



class GeneralInformList : public BaseLinkedList
{
  public:
   void add(GeneralInform *inform)
               { GeneralInformElement *theElement = 
                                 new GeneralInformElement(inform);
                 BaseLinkedList::add((Element *) theElement); }

   void remove(GeneralInform *inform) {BaseLinkedList::remove((void*)inform);};
   GeneralInform *top(void **ptr = (void **) 0);
   GeneralInform *find(GeneralInform *inform);
   GeneralInform *bottom(void **ptr = (void **) 0);
   GeneralInform *next(void **ptr = (void **) 0);
   GeneralInform *prev(void **ptr = (void **) 0);
   GeneralInform *current(void **ptr = (void **) 0);

   void callPreAction(GeneralObject*);
   void callPostAction(GeneralObject*);

};

#endif
