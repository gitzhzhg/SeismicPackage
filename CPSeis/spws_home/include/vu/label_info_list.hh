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
#ifndef LABEL_INFO_LIST_HH
#define LABEL_INFO_LIST_HH

#include "oprim/element.hh"
#include "oprim/ll_base.hh"


class PlotBase;
class PickBase;
class SeisLabel;
class SeisVectLinkedList;


class LabelInfoElement : public Element
{
  friend class LabelInfoList;
  protected:
      PlotBase           *_plot;
      PickBase           *_picker;
      SeisVectLinkedList *_vlist;

  public:
      LabelInfoElement( PlotBase           *plot,
                        SeisVectLinkedList *vlist) :
           _plot(plot), _picker(NULL), _vlist(vlist) {}
      ~LabelInfoElement() {}
      int operator ==(void * const p) const {return((PlotBase*) p == _plot);}
      virtual void print() const {}
};


class LabelInfoList : public BaseLinkedList
{
  public:
        LabelInfoList() {};
        ~LabelInfoList();
        void add( PlotBase           *plot,
                  SeisVectLinkedList *vlist)
             { LabelInfoElement *ele= new LabelInfoElement(plot,vlist);
               BaseLinkedList::add((Element *) ele); }

        void remove(PlotBase *ele);

        PlotBase *find(PlotBase *ele, void **ptr = (void **) 0);
        PlotBase *top(void **ptr = (void **) 0);
        PlotBase *bottom(void **ptr = (void **) 0);
        PlotBase *next(void **ptr = (void **) 0);
        PlotBase *prev(void **ptr = (void **) 0);
        PlotBase *current(void **ptr = (void **) 0);
//        PickBase *currentPicker(void **ptr);
//        void setCurrentPicker(void **ptr, PickBase *pick);
        void enablePicking(SeisLabel *sl);
        void disablePicking();
        SeisVectLinkedList *currentVectorList(void **ptr);
};

#endif
