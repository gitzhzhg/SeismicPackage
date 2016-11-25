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

//------------------ datafield_list.cc -----------------------------//
//------------------ datafield_list.cc -----------------------------//
//------------------ datafield_list.cc -----------------------------//

// implementation file for DatafieldElement and DatafieldList
//          derived from Element and BaseLinkedList
//                     subdirectory sl


#include "sl/datafield_list.hh"
#include <string.h>
#include <iostream.h>
#include <locale.h>


//--------------------- DatafieldElement ------------------------//
//--------------------- DatafieldElement ------------------------//
//--------------------- DatafieldElement ------------------------//

DatafieldElement::DatafieldElement(DFpBase *gui)
              : Element(),
                  _gui(gui)
{
}


int DatafieldElement::operator ==(void * const gui) const
{
  return (int)((DFpBase*)gui == _gui);
}


void DatafieldElement::print() const
{
  cout << " " << _gui << endl;
}



//-------------------- DatafieldList ----------------------//
//-------------------- DatafieldList ----------------------//
//-------------------- DatafieldList ----------------------//


DatafieldList::DatafieldList()
        : BaseLinkedList()
{
}


void DatafieldList::add(DFpBase *gui)
{
  Element *element = new DatafieldElement(gui);
  BaseLinkedList::add(element);
}


void DatafieldList::remove(DFpBase *gui)
{
  BaseLinkedList::remove((void*)gui);
}


DFpBase *DatafieldList::find(DFpBase *gui)
{
  Element *element = BaseLinkedList::find((void*)gui);
  if(!element) return NULL;
  return ((DatafieldElement*)element)->_gui;
}


#define SHORTHAND(top)                         \
DFpBase *DatafieldList::top()                  \
{                                              \
  Element *element = BaseLinkedList::top();    \
  if(!element) return NULL;                    \
  return ((DatafieldElement*)element)->_gui;   \
}

  SHORTHAND(top)
  SHORTHAND(bottom)
  SHORTHAND(next)
  SHORTHAND(prev)
  SHORTHAND(current)


//------------------------- end --------------------------//
//------------------------- end --------------------------//
//------------------------- end --------------------------//

