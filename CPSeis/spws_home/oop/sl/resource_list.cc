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

//----------------- resource_list.cc -----------------------------//
//----------------- resource_list.cc -----------------------------//
//----------------- resource_list.cc -----------------------------//

//    implementation file for ResourceElement and ResourceList
//            derived from Element and BaseLinkedList
//                     subdirectory sl

//        supports linked lists of BaseGR objects


#include "sl/resource_list.hh"
#include "sl/resource_classes.hh"
#include <iostream.h>


//----------------------- ResourceElement ------------------------//
//----------------------- ResourceElement ------------------------//
//----------------------- ResourceElement ------------------------//

ResourceElement::ResourceElement(BaseGR *resource)
         : Element(),
             _resource(resource)
{
}


int ResourceElement::operator ==(void * const resource) const
{
  return (int)((BaseGR*)resource == _resource);
}


void ResourceElement::print() const
{
  cout << " " << _resource << endl;
}


//---------------------- ResourceList ----------------------------//
//---------------------- ResourceList ----------------------------//
//---------------------- ResourceList ----------------------------//

ResourceList::ResourceList()
         : BaseLinkedList(),
            _allow_reload_default        (TRUE),
            _allow_reload_system_default (FALSE)
{
}


void ResourceList::add(BaseGR *resource)
{
  Element *element = new ResourceElement(resource);
  BaseLinkedList::add(element);
}


void ResourceList::remove(BaseGR *resource)
{
  BaseLinkedList::remove((void*)resource);
}


BaseGR *ResourceList::find(BaseGR *resource)
{
  Element *element = BaseLinkedList::find((void*)resource);
  if(!element) return NULL;
  return ((ResourceElement*)element)->_resource;
}


#define SHORTHAND(top)                            \
BaseGR *ResourceList::top()                       \
{                                                 \
  Element *element = BaseLinkedList::top();       \
  if(!element) return NULL;                       \
  return ((ResourceElement*)element)->_resource;  \
}

  SHORTHAND(top)
  SHORTHAND(bottom)
  SHORTHAND(next)
  SHORTHAND(prev)
  SHORTHAND(current)


void ResourceList::updatePacket(Boolean force)
{
  for ( BaseGR *resource = top(); resource;
                resource = next() )
          {
          resource->updateValue(force);
          }
}


//------------------------- end --------------------------//
//------------------------- end --------------------------//
//------------------------- end --------------------------//

