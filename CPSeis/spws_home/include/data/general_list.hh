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
#ifndef GENERALELEMENT_HH
#define GENERALELEMENT_HH


#include "oprim/element.hh"
#include "oprim/ll_base.hh"

class GeneralObject;


class GeneralElement : public Element
{
  friend class GeneralList;
  protected:
     GeneralObject *_gen;
     void     *_user_data;

     GeneralElement(GeneralObject *gen, void *user_data = (void*) 0 ) : 
                     _gen(gen), _user_data(user_data) {}
     ~GeneralElement() {}
     int operator ==(void * const gen) const { 
                             return((GeneralObject*) gen == _gen); }
     virtual void print() const {}
};


class GeneralList : public BaseLinkedList
{
  public:
   void add(GeneralObject *gen, void *user_data = (void*) 0 )
               { GeneralElement *theElement = new GeneralElement(gen);
                 BaseLinkedList::add((Element *) theElement); 
                 theElement->_user_data= user_data; }

   void remove(GeneralObject *gen) { BaseLinkedList::remove((void*)gen); };
   GeneralObject *top(void **ptr = (void **) 0);
   GeneralObject *find(GeneralObject *gen, void **ptr = (void **) 0);
   GeneralObject *bottom(void **ptr = (void **) 0);
   GeneralObject *next(void **ptr = (void **) 0);
   GeneralObject *prev(void **ptr = (void **) 0);
   GeneralObject *current(void **ptr = (void **) 0);
   void *currentUserData(void **ptr = (void **) 0);
};


#endif
