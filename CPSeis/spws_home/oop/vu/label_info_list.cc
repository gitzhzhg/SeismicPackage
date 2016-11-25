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
#include <stdlib.h>
#include <assert.h>
#include <locale.h>
#include "vu/label_info_list.hh"
#include "vu/pick_label.hh"
#include "plot/plot_base.hh"


LabelInfoList::~LabelInfoList()
{
  disablePicking();
}



PlotBase *LabelInfoList::top(void **ptr)
{
   LabelInfoElement* q= (LabelInfoElement*)BaseLinkedList::top(ptr);
   return (q ? q->_plot : NULL);
}

PlotBase *LabelInfoList::bottom(void **ptr)
{
   LabelInfoElement* q= (LabelInfoElement*)BaseLinkedList::bottom(ptr);
   return (q ? q->_plot : NULL);
}


PlotBase *LabelInfoList::next(void **ptr)
{
   LabelInfoElement* q= (LabelInfoElement*)BaseLinkedList::next(ptr);
   return (q ? q->_plot : NULL);
}

PlotBase *LabelInfoList::prev(void **ptr)
{
   LabelInfoElement* q= (LabelInfoElement*)BaseLinkedList::prev(ptr);
   return (q ? q->_plot : NULL);
}

PlotBase *LabelInfoList::current(void **ptr)
{
   LabelInfoElement* q= (LabelInfoElement*)BaseLinkedList::current(ptr);
   return (q ? q->_plot : NULL);
}

/*
 *PickBase *LabelInfoList::currentPicker(void **ptr)
 *{
 *    LabelInfoElement* q= (LabelInfoElement*)BaseLinkedList::current(ptr);
 *   return (q ? q->_picker : NULL);
 *}
 *
 *void LabelInfoList::setCurrentPicker(void **ptr, PickBase *pick)
 *{
 *   LabelInfoElement* q= (LabelInfoElement*)BaseLinkedList::current(ptr);
 *   assert(q);
 *   q->_picker= pick;
 *}
 */

SeisVectLinkedList *LabelInfoList::currentVectorList(void **ptr)
{
   LabelInfoElement* q= (LabelInfoElement*)BaseLinkedList::current(ptr);
   return (q ? q->_vlist : NULL);
}

PlotBase *LabelInfoList::find(PlotBase *plot, void **ptr)
{
  LabelInfoElement* q= 
           (LabelInfoElement*)BaseLinkedList::find((void*)plot, ptr);
  return (q ? q->_plot : NULL);
}


void LabelInfoList::enablePicking(SeisLabel *sl)
{
  void *dummy;
  LabelInfoElement *q;

  for(q= (LabelInfoElement*)BaseLinkedList::top(&dummy); (q); 
      q= (LabelInfoElement*)BaseLinkedList::next(&dummy) ) {
           if (q->_picker) delete q->_picker;
           if (q->_plot->isCurrentInWindow())
                   q->_picker= new PickLabel(q->_plot, sl);
  }
}



void LabelInfoList::disablePicking()
{
  void *dummy;
  LabelInfoElement *q;

  for(q= (LabelInfoElement*)BaseLinkedList::bottom(&dummy); (q); 
      q= (LabelInfoElement*)BaseLinkedList::prev(&dummy) ) {
            if (q->_picker) {
                 delete q->_picker;
                 q->_picker= NULL;
            }
  }
}

void LabelInfoList::remove(PlotBase *ele) 
{
  BaseLinkedList::remove((void*) ele);
}
