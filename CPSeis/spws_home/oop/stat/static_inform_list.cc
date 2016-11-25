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

//------------------ static_inform_list.cc -----------------------------//
//------------------ static_inform_list.cc -----------------------------//
//------------------ static_inform_list.cc -----------------------------//

//  implementation file for StaticInformElement and StaticInformList
//             derived from Element and BaseLinkedList
//                       subdirectory stat


#include "stat/static_inform_list.hh"
#include <string.h>
#include <iostream.h>
#include <locale.h>


//--------------------- StaticInformElement ------------------------//
//--------------------- StaticInformElement ------------------------//
//--------------------- StaticInformElement ------------------------//

StaticInformElement::StaticInformElement(StaticInform *inform)
              : Element(),
                  _inform  (inform)
{
}


int StaticInformElement::operator ==(void * const inform) const
{
  return (int)((StaticInform*)inform == _inform);
}


void StaticInformElement::print() const
{
  cout << " " << _inform << endl;
}



//-------------------- StaticInformList ----------------------//
//-------------------- StaticInformList ----------------------//
//-------------------- StaticInformList ----------------------//


StaticInformList::StaticInformList()
        : BaseLinkedList()
{
}


void StaticInformList::add(StaticInform *inform)
{
  Element *element = new StaticInformElement(inform);
  BaseLinkedList::add(element);
}


void StaticInformList::remove(StaticInform *inform)
{
  BaseLinkedList::remove((void*)inform);
}


StaticInform *StaticInformList::find(StaticInform *inform)
{
  Element *element = BaseLinkedList::find((void*)inform);
  if(!element) return NULL;
  return ((StaticInformElement*)element)->_inform;
}


#define SHORTHAND(top)                               \
StaticInform *StaticInformList::top(void **p)        \
{                                                    \
  Element *element = BaseLinkedList::top(p);         \
  if(!element) return NULL;                          \
  return ((StaticInformElement*)element)->_inform;   \
}

  SHORTHAND(top)
  SHORTHAND(bottom)
  SHORTHAND(next)
  SHORTHAND(prev)
  SHORTHAND(current)


//------------------------- end --------------------------//
//------------------------- end --------------------------//
//------------------------- end --------------------------//

