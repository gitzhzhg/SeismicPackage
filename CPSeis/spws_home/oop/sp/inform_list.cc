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
#include "sp/inform_list.hh"
#include "sp/seis_inform.hh"


SeisInform *InformList::top(void **ptr)
{
   InformElement* q= (InformElement*)BaseLinkedList::top(ptr);
   return (q ? q->_inform : NULL);
}
SeisInform *InformList::find(SeisInform *ptr)
{
   InformElement* q= (InformElement*)BaseLinkedList::find(ptr);
   return (q ? q->_inform : NULL);
}
SeisInform *InformList::bottom(void **ptr)
{
   InformElement* q= (InformElement*)BaseLinkedList::bottom(ptr);
   return (q ? q->_inform : NULL);
}
SeisInform *InformList::next(void **ptr)
{
   InformElement* q= (InformElement*)BaseLinkedList::next(ptr);
   return (q ? q->_inform : NULL);
}
SeisInform *InformList::prev(void **ptr)
{
   InformElement* q= (InformElement*)BaseLinkedList::prev(ptr);
   return (q ? q->_inform : NULL);
}
SeisInform *InformList::current(void **ptr)
{
   InformElement* q= (InformElement*)BaseLinkedList::current(ptr);
   return (q ? q->_inform : NULL);
}






void InformList::callPreZoom(SeisPlot *sp, SeisZoomer *zoomer, 
                             SeisPlot::ZoomDir dir)
{
  void *x;
  for(SeisInform *q= top(&x); (q); q= next(&x))  q->preZoom(sp,zoomer,dir);
}


void InformList::callZoomInProgress(SeisPlot *sp, SeisPlot::ZoomDir dir)
{
  void *x;
  for(SeisInform *q= top(&x); (q); q= next(&x))q->zoomInProgress(sp,dir);
}

void InformList::callPostZoom(SeisPlot *sp, SeisPlot::ZoomDir dir)
{
  void *x;
  for(SeisInform *q= top(&x); (q); q= next(&x))  q->postZoom(sp,dir);
}

void InformList::callPostZoomSeparateWindow(SeisPlot *sp, SeisPlot *zoomsp)
{
  void *x;
  for(SeisInform *q= top(&x); (q); q= next(&x))  
          q->postZoomSeparateWindow(sp,zoomsp);
}


void InformList::callPreScan(SeisPlot  *sp, SeisPlot::ScanDir dir  )
{
  void *x;
  for(SeisInform *q= top(&x); (q); q= next(&x))  q->preScan(sp,dir);
}

void InformList::callPostScan(SeisPlot *sp, SeisPlot::ScanDir dir )
{
  void *x;
  for(SeisInform *q= top(&x); (q); q= next(&x))  q->postScan(sp,dir);
}


void InformList::callPreMovie(SeisPlot *sp,  SeisPlot::MovieDir dir)
{
  void *x;
  for(SeisInform *q= top(&x); (q); q= next(&x))  q->preMovie(sp,dir);
}

void InformList::callPostMovie(SeisPlot *sp, SeisPlot::MovieDir dir)
{
  void *x;
  for(SeisInform *q= top(&x); (q); q= next(&x))  q->postMovie(sp,dir);
}


void InformList::callPreDataChange(SeisPlot *sp )
{
  void *x;
  for(SeisInform *q= top(&x); (q); q= next(&x))  q->preDataChange(sp);
}


void InformList::callPlotTypeChange(SeisPlot *sp, 
                                    int       new_plot_mode,
                                    int       old_plot_mode )
{
  void *x;
  for(SeisInform *q= top(&x); (q); q= next(&x))  
              q->plotTypeChange(sp,new_plot_mode,old_plot_mode);
}



void InformList::callPrePlot(SeisPlot *sp )
{
  void *x;
  for(SeisInform *q= top(&x); (q); q= next(&x))  q->prePlot(sp);
}

void InformList::callNewPlot(SeisPlot *sp )
{
  void *x;
  for(SeisInform *q= top(&x); (q); q= next(&x))  q->newPlot(sp);
}

void InformList::callNoPlotDisplayed(SeisPlot *sp )
{
  void *x;
  for(SeisInform *q= top(&x); (q); q= next(&x))  q->noPlotDisplayed(sp);
}


void InformList::callAddingTie(SeisPlotTie *sp, SeisPlot *tieplot )
{
  void *x;
  for(SeisInform *q= top(&x); (q); q= next(&x))  
          q->addingTie(sp,tieplot);
}

void InformList::callRemovingTie(SeisPlotTie *sp, SeisPlot *tieplot)
{
  void *x;
  for(SeisInform *q= top(&x); (q); q= next(&x))  
          q->removingTie(sp,tieplot);
}

void InformList::callDragImage(SeisPlot *sp)
{
  void *x;
  for(SeisInform *q= top(&x); (q); q= next(&x))  q->dragImage(sp);
}


void InformList::callUnitChange(SeisPlot *sp, int units)
{
  void *x;
  for(SeisInform *q= top(&x); (q); q= next(&x))  q->unitChange(sp,units);
}


void InformList::callExpose(SeisPlot *sp, int x, int y, int width, int height)
{
  void *p;
  for(SeisInform *q= top(&p); (q); q= next(&p))  
          q->expose(sp,x,y,width,height);
}

void InformList::callDestroyed(SeisPlot *sp )
{
  void *x;
  SeisInform *p;
  for(p= top(&x); (p); p= top(&x))  {
       p->delSeisPlot(sp);
       p->destroyed(sp);
  }
}


void InformList::callColorChange(long num_allocated, long which_set)
{
  void *x;
  for(SeisInform *q= top(&x); (q); q= next(&x))  
               q->colorChange(num_allocated,which_set);
}

void InformList::callPostDataPreImage(SeisPlot *sp)
{
  void *x;
  for(SeisInform *q= top(&x); (q); q= next(&x)) q->postDataPreImage(sp);

}
void InformList::callVisibleAreaChange(SeisPlot *sp, 
                                       int       x,
                                       int       y, 
                                       int       width, 
                                       int       height)
{
  void *p;
  for(SeisInform *q= top(&p); (q); q= next(&p))  
          q->visableAreaChange(sp,x,y,width,height);
}

void InformList::callStartingDragScroll(SeisPlot *sp)
{
  void *p;
  for(SeisInform *q= top(&p); (q); q= next(&p)) q->startingDragScroll(sp);

}
void InformList::callEndingDragScroll(SeisPlot *sp)
{
  void *p;
  for(SeisInform *q= top(&p); (q); q= next(&p)) q->endingDragScroll(sp);
}

void InformList::callMouseOutputUpdate(SeisPlot *sp, float x, float y)
{
  void *p;
  for(SeisInform *q= top(&p); (q); q= next(&p))  
          q->mouseOutputUpdate(sp,x,y);
}

void InformList::callPreWriteHardCopy(SeisPlot *sp)
{
  void *p;
  for(SeisInform *q= top(&p); (q); q= next(&p))  q->preWriteHardCopy(sp);
}

void InformList::callWriteToHardCopy(SeisPlot     *sp, 
                                     HardCopyPlot *hc,
                                     float         marker_scale)
{
  void *p;
  for(SeisInform *q= top(&p); (q); q= next(&p))  
          q->writeToHardCopy(sp,hc,marker_scale);
}

void InformList::callPostWriteHardCopy(SeisPlot *sp)
{
  void *p;
  for(SeisInform *q= top(&p); (q); q= next(&p))  q->postWriteHardCopy(sp);
}

void InformList::callNotCurrentInWindow(SeisPlot *sp)
{
  void *p;
  for(SeisInform *q= top(&p); (q); q= next(&p))  q->notCurrentInWindow(sp);
}

void InformList::callNowCurrentInWindow(SeisPlot *sp)
{
  void *p;
  for(SeisInform *q= top(&p); (q); q= next(&p))  q->nowCurrentInWindow(sp);
}

void InformList::callNewSeisPlotCreatedInWindow(SeisPlot *newsp)
{
  void *x;
  for(SeisInform *q= top(&x); (q); q= next(&x))  
                 q->newSeisPlotCreatedInWindow(newsp);
}

void InformList::calBackingStoreChange(SeisPlot *sp, Boolean backing_on)
{
  void *x;
  for(SeisInform *q= top(&x); (q); q= next(&x)) 
             q->backingStoreChange(sp,backing_on);
}
