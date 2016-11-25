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
#include "cube/cube.hh"

class CubeInform;
class SeisPlot;


class CubeInformElement : public Element
{
  friend class CubeInformList;
  protected:
     CubeInform *_inform;
     CubeInformElement(CubeInform *inform) : _inform(inform) {}
     int operator ==(void * const inform) const 
                 { return((CubeInform*) inform == _inform); }
     virtual void print() const {}
};



class CubeInformList : public BaseLinkedList
{
  public:
   CubeInformList ();

   virtual ~CubeInformList ();

   void add(CubeInform *inform)
               { CubeInformElement *theElement = new CubeInformElement(inform);
                 BaseLinkedList::add((Element *) theElement); }

   void remove(CubeInform *inform) { BaseLinkedList::remove((void*)inform); };
   CubeInform *top(void **ptr = (void **) 0);
   CubeInform *find(CubeInform *inform);
   CubeInform *bottom(void **ptr = (void **) 0);
   CubeInform *next(void **ptr = (void **) 0);
   CubeInform *prev(void **ptr = (void **) 0);
   CubeInform *current(void **ptr = (void **) 0);

   void callNewInLinePlot(Cube *cube,    int slice);
   void callNewCrossLinePlot(Cube *cube, int slice);
   void callNewTimeSlicePlot(Cube *cube, int slice);
   void callPostPlot(Cube *cube, int il_slice, int xl_slice, int ts_slice);
   void callNoPlotDisplayed(Cube *cube);
   void callCubeMovie(Cube            *cube,
                      Cube::WhichPlot  face,
                      Cube::MovieDir   mdir,
                      int              slice);
   void callCubeIsCurrent(Cube *cube);
   void callCubeIsNolongerCurrent(Cube* cube,Cube* newcube);
   void callNewCubeCreated(Cube *);
   void callDestroyed(Cube *cube);
   void callMaxAmplitudeChanged(double amp);
   void callNewFilename(Cube *);
   void callPreZoom(Cube *cube, Cube::WhichPlot which );
   void callPostZoomSeparateWindow(Cube *cube, Cube::WhichPlot, SeisPlot *);

};

#endif
