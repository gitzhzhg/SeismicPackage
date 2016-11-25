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

//------------------ vf_inform_list.cc -----------------------------//
//------------------ vf_inform_list.cc -----------------------------//
//------------------ vf_inform_list.cc -----------------------------//

//     implementation file for VfInformElement and VfInformList
//             derived from Element and BaseLinkedList
//                       subdirectory vf


#include <string.h>
#include "vf/vf_inform_list.hh"
#include <iostream.h>
#include <locale.h>


//--------------------- VfInformElement ------------------------//
//--------------------- VfInformElement ------------------------//
//--------------------- VfInformElement ------------------------//

VfInformElement::VfInformElement(VfInform *inform)
              : Element(),
                  _inform  (inform)
{
}


int VfInformElement::operator ==(void * const inform) const
{
  return (int)((VfInform*)inform == _inform);
}


void VfInformElement::print() const
{
  cout << " " << _inform << endl;
}



//-------------------- VfInformList ----------------------//
//-------------------- VfInformList ----------------------//
//-------------------- VfInformList ----------------------//


VfInformList::VfInformList()
        : BaseLinkedList()
{
}


void VfInformList::add(VfInform *inform)
{
  Element *element = new VfInformElement(inform);
  BaseLinkedList::add(element);
}


void VfInformList::remove(VfInform *inform)
{
  BaseLinkedList::remove((void*)inform);
}


VfInform *VfInformList::find(VfInform *inform)
{
  Element *element = BaseLinkedList::find((void*)inform);
  if(!element) return NULL;
  return ((VfInformElement*)element)->_inform;
}


#define SHORTHAND(top)                               \
VfInform *VfInformList::top(void **p)                \
{                                                    \
  Element *element = BaseLinkedList::top(p);         \
  if(!element) return NULL;                          \
  return ((VfInformElement*)element)->_inform;       \
}

  SHORTHAND(top)
  SHORTHAND(bottom)
  SHORTHAND(next)
  SHORTHAND(prev)
  SHORTHAND(current)


//------------------------- end --------------------------//
//------------------------- end --------------------------//
//------------------------- end --------------------------//

