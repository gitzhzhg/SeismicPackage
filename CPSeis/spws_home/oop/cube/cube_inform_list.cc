#include "cube/cube_inform_list.hh"
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
#include "cube/cube_inform.hh"

CubeInformList::CubeInformList () :
  BaseLinkedList ()
{
  int i = 0;
}

CubeInformList::~CubeInformList ()
{
  int i = 0;
}

CubeInform *CubeInformList::top(void **ptr)
{
   CubeInformElement* q= (CubeInformElement*)BaseLinkedList::top(ptr);
   return (q ? q->_inform : NULL);
}
CubeInform *CubeInformList::find(CubeInform *ptr)
{
   CubeInformElement* q= (CubeInformElement*)BaseLinkedList::find(ptr);
   return (q ? q->_inform : NULL);
}
CubeInform *CubeInformList::bottom(void **ptr)
{
   CubeInformElement* q= (CubeInformElement*)BaseLinkedList::bottom(ptr);
   return (q ? q->_inform : NULL);
}
CubeInform *CubeInformList::next(void **ptr)
{
   CubeInformElement* q= (CubeInformElement*)BaseLinkedList::next(ptr);
   return (q ? q->_inform : NULL);
}
CubeInform *CubeInformList::prev(void **ptr)
{
   CubeInformElement* q= (CubeInformElement*)BaseLinkedList::prev(ptr);
   return (q ? q->_inform : NULL);
}
CubeInform *CubeInformList::current(void **ptr)
{
   CubeInformElement* q= (CubeInformElement*)BaseLinkedList::current(ptr);
   return (q ? q->_inform : NULL);
}



void CubeInformList::callNewInLinePlot(Cube *cube, int slice)
{
  void *x;
  for(CubeInform *q= top(&x); (q); q= next(&x)) q->newInLinePlot(cube,slice);
}

void CubeInformList::callNewCrossLinePlot(Cube *cube, int slice)
{
  void *x;
  for(CubeInform *q= top(&x); (q); q= next(&x)) 
                       q->newCrossLinePlot(cube,slice);
}

void CubeInformList::callNewTimeSlicePlot(Cube *cube, int slice)
{
  void *x;
  for(CubeInform *q= top(&x); (q); q= next(&x)) 
                       q->newTimeSlicePlot(cube,slice);
}
void CubeInformList::callCubeIsCurrent(Cube *cube)
{
  void *x;
  for(CubeInform *q= top(&x); (q); q= next(&x)) q->cubeIsCurrent(cube);
}

void CubeInformList::callCubeIsNolongerCurrent(Cube* cube,Cube* newcube)
{
  void *x;
  for(CubeInform *q= top(&x); (q); q= next(&x)) 
                     q->cubeIsNolongerCurrent(cube,newcube);
}

void CubeInformList::callNewCubeCreated(Cube *cube)
{
  void *x;
  for(CubeInform *q= top(&x); (q); q= next(&x)) q->newCubeCreated(cube);
}

void CubeInformList::callPostPlot(Cube *cube, 
                                  int il_slice, 
                                  int xl_slice, 
                                  int ts_slice)
{
  void *x;
  for(CubeInform *q= top(&x); (q); q= next(&x))  
                 q->postPlot(cube, il_slice, xl_slice, ts_slice);
}


void CubeInformList::callNoPlotDisplayed(Cube *cube)
{
  void *x;
  for(CubeInform *q= top(&x); (q); q= next(&x)) q->noPlotDisplayed(cube);
}

void CubeInformList::callMaxAmplitudeChanged(double amp)
{
  void *x;
  for(CubeInform *q= top(&x); (q); q=next(&x)) 
    {
    q->maxAmplitudeChanged(amp);
    }
}

void CubeInformList::callNewFilename(Cube *cube)
{
  void *x;
  for(CubeInform *q= top(&x); (q); q=next(&x)) q->newFilename(cube);
}

void CubeInformList::callPreZoom(Cube *cube, Cube::WhichPlot which )
{
  void *x;
  for(CubeInform *q= top(&x); (q); q=next(&x)) q->preZoom(cube,which);
}

void CubeInformList::callPostZoomSeparateWindow(Cube           *cube,
                                                Cube::WhichPlot which, 
                                                SeisPlot       *sp)
{
  void *x;
  for(CubeInform *q= top(&x); (q); q=next(&x)) 
                      q->postZoomSeparateWindow(cube,which,sp);
}

void CubeInformList::callCubeMovie(Cube            *cube,
                                   Cube::WhichPlot  face,
                                   Cube::MovieDir   mdir,
                                   int              slice)
{
  void *x;
  for(CubeInform *q= top(&x); (q); q= next(&x))  
                 q->cubeMovie(cube, face, mdir, slice);
}

void CubeInformList::callDestroyed(Cube *cube)
{
  void *x;
  CubeInform *q, *p;
  for(p= q= top(&x); (p); p=q)  {
       q= next(&x);
       p->delCube(cube);
       p->destroyed(cube);
  }
}
