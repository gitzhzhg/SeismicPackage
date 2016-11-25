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

//----------------------- family_list.cc --------------------------//
//----------------------- family_list.cc --------------------------//
//----------------------- family_list.cc --------------------------//

//       implementation file for FamilyElement and FamilyList
//             derived from Element and BaseLinkedList
//                        subdirectory sl


#include "sl/family_list.hh"
#include <string.h>
#include <iostream.h>
#include <locale.h>


//-------------------- FamilyElement ------------------------//
//-------------------- FamilyElement ------------------------//
//-------------------- FamilyElement ------------------------//

FamilyElement::FamilyElement(void (*fun)(void *data), void *data)
          : Element(),
              _fun  (fun),
              _data (data)
{
}


int FamilyElement::operator ==(void * const element) const
{
  if( ((FamilyElement*)element)->_fun  != _fun ) return 0;
  if( ((FamilyElement*)element)->_data != _data) return 0;
  return 1;
}


void FamilyElement::print() const
{
//  cout << " " << _fun << _data << endl;
  cout << " " << _data << endl;
}



//------------------- FamilyList ----------------------------//
//------------------- FamilyList ----------------------------//
//------------------- FamilyList ----------------------------//

FamilyList::FamilyList()
       : BaseLinkedList()
{
}


void FamilyList::add(void (*fun)(void *data), void *data)
{
  Element *element = new FamilyElement(fun, data);
  BaseLinkedList::add(element); 
}


void FamilyList::remove(void (*fun)(void *data), void *data)
{
  FamilyElement *temp = new FamilyElement(fun, data);
  BaseLinkedList::remove((void*)temp);
  delete temp; 
}


int FamilyList::find(void (*fun)(void *data), void *data)
{
  FamilyElement *temp = new FamilyElement(fun, data);
  Element *element = BaseLinkedList::find((void*)temp);
  delete temp;
  if(!element) return 1;
  return 0;
}


#define SHORTHAND(top)                       \
int FamilyList::top(void)                    \
{                                            \
  Element *element = BaseLinkedList::top();  \
  if(!element) return 1;                     \
  return 0;                                  \
}

  SHORTHAND(top)
  SHORTHAND(bottom)
  SHORTHAND(next)
  SHORTHAND(prev)
  SHORTHAND(current)


//--------------------- additional methods -------------------//
//--------------------- additional methods -------------------//
//--------------------- additional methods -------------------//

void *FamilyList::getCurrentFun(void)
{
  Element *element = BaseLinkedList::current();
  if(!element) return NULL;
  return (void *)(((FamilyElement*)element)->_fun);
}


void *FamilyList::getCurrentData(void)
{
  Element *element = BaseLinkedList::current();
  if(!element) return NULL;
  return ((FamilyElement*)element)->_data;
}


int FamilyList::callCurrentFun(void)
{
  FamilyElement *element = (FamilyElement*) BaseLinkedList::current();
  if(!element || !element->_fun) return 1;
  element->_fun(element->_data);
  return 0;
}


void FamilyList::callFamily (void)
{
  for(int error = top(); !error; error = next())
      {
      callCurrentFun();
      }
}


//------------------------- end --------------------------//
//------------------------- end --------------------------//
//------------------------- end --------------------------//

