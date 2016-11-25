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
#include <X11/Intrinsic.h>
#include <Xm/Xm.h>
#include "sl/sl_shell_container.hh"
#include "sl/container_list.hh"


void ContainerList::add(SLShellContainer *contain)
{
   ContainerElement *theElement = new ContainerElement(contain);

   BaseLinkedList::add((Element *) theElement);

}

void ContainerList::putSeparatorAfter(SLShellContainer *contain)
{
   ContainerElement* q=  (ContainerElement *)
                              BaseLinkedList::find((void *)contain);
   if (q) q->_put_sep_after= True;
}

void ContainerList::putSeparatorAtBottomElement()
{
   ContainerElement* q= (ContainerElement*)BaseLinkedList::bottom();
   if (q) q->_put_sep_after= True;
}

Boolean ContainerList::needsSeparator()
{
 ContainerElement* q= (ContainerElement*)BaseLinkedList::current();
 return (q ? q->_put_sep_after : False);
}

SLShellContainer *ContainerList::top() 
{ 
   ContainerElement* q= (ContainerElement*)BaseLinkedList::top();
   return (q ? q->_contain : NULL);
}

SLShellContainer *ContainerList::bottom()
{
   ContainerElement* q= (ContainerElement*)BaseLinkedList::bottom();
   return (q ? q->_contain : NULL);
}


SLShellContainer *ContainerList::next()
{
   ContainerElement* q= (ContainerElement*)BaseLinkedList::next();
   return (q ? q->_contain : NULL);
}

SLShellContainer *ContainerList::prev()
{
   ContainerElement* q= (ContainerElement*)BaseLinkedList::prev();
   return (q ? q->_contain : NULL);
}

SLShellContainer *ContainerList::current()
{
   ContainerElement* q= (ContainerElement*)BaseLinkedList::current();
   return (q ? q->_contain : NULL);
}
