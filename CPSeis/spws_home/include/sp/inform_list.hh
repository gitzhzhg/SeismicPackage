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
#ifndef INFORM_LIST_HH
#define INFORM_LIST_HH

#include "oprim/element.hh"
#include "oprim/ll_base.hh"

#include "sp/seis_plot.hh"

class SeisInform;
class SeisPlotUnder;
class SeisZoomer;
class HardCopyPlot;


class InformElement : public Element
{
  friend class InformList;
  protected:
     SeisInform *_inform;
     InformElement(SeisInform *inform) : _inform(inform) {}
     int operator ==(void * const inform) const 
                 { return((SeisInform*) inform == _inform); }
     virtual void print() const {}
};



class InformList : public BaseLinkedList
{
  public:
   void add(SeisInform *inform)
               { InformElement *theElement = new InformElement(inform);
                 BaseLinkedList::add((Element *) theElement); }

   void remove(SeisInform *inform) { BaseLinkedList::remove((void*)inform); };
   SeisInform *top(void **ptr = (void **) 0);
   SeisInform *find(SeisInform *inform);
   SeisInform *bottom(void **ptr = (void **) 0);
   SeisInform *next(void **ptr = (void **) 0);
   SeisInform *prev(void **ptr = (void **) 0);
   SeisInform *current(void **ptr = (void **) 0);



   void callPreZoom(SeisPlot *sp, SeisZoomer *zoomer, SeisPlot::ZoomDir dir);
   void callZoomInProgress(SeisPlot *sp, SeisPlot::ZoomDir dir);
   void callPostZoom(SeisPlot *sp, SeisPlot::ZoomDir dir);
   void callPostZoomSeparateWindow(SeisPlot *sp, SeisPlot *zoomsp);

   void callPreScan(SeisPlot  *sp, SeisPlot::ScanDir dir  );
   void callPostScan(SeisPlot *sp, SeisPlot::ScanDir dir );

   void callPreMovie(SeisPlot *sp,  SeisPlot::MovieDir dir);
   void callPostMovie(SeisPlot *sp, SeisPlot::MovieDir dir);

   void callPlotTypeChange(SeisPlot *sp, 
                           int       new_plot_mode,
                           int       old_plot_mode );
   void callPreDataChange(SeisPlot *sp );
   void callPrePlot(SeisPlot *sp );
   void callNewPlot(SeisPlot *sp );
   void callNoPlotDisplayed(SeisPlot *sp );

   void callAddingTie(SeisPlotTie *sp, SeisPlot *tieplot );
   void callRemovingTie(SeisPlotTie *sp, SeisPlot *tieplot);

   void callDragImage(SeisPlot *sp);

   void callUnitChange(SeisPlot *sp, int units);

   void callExpose(SeisPlot *sp, int x, int y, int width, int height);
   void callDestroyed(SeisPlot *sp );

   void callColorChange(long num_allocated, long which_set);

   void callMouseOutputUpdate(SeisPlot*, float x, float y);

   void callStartingDragScroll(SeisPlot *sp);
   void callEndingDragScroll(SeisPlot *sp);

   void callPostDataPreImage(SeisPlot *sp);
   void callVisibleAreaChange(SeisPlot *, int x, int y, int width, int height);

   void callPreWriteHardCopy(SeisPlot *);
   void callWriteToHardCopy(SeisPlot *, 
                            HardCopyPlot *,  
                            float        marker_scale);
   void callPostWriteHardCopy(SeisPlot *);

  void callNotCurrentInWindow(SeisPlot *sp);
  void callNowCurrentInWindow(SeisPlot *sp);
  void callNewSeisPlotCreatedInWindow(SeisPlot *newsp);
  void calBackingStoreChange(SeisPlot*, Boolean backing_on);

};

#endif
