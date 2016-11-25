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
#include "sp/multi_zoom.hh"
#include "sp/multi_zoom_inform.hh"
#include "sp/seis_plot.hh"
#include "sp/seis_zoomer.hh"
#include "sp/seis_winman.hh"


MultiZoom::MultiZoom(SeisPlot *sp) : _allbut_sp(NULL)
{
 _inform= new MultiZoomInform(this);
 if (sp) addSeisPlot(sp);
}

MultiZoom::~MultiZoom()
{
  delete _inform;
}


void MultiZoom::addSeisPlot(SeisPlot *sp)
{
 if (sp) {
    SeisWinMan *swm= sp->getSeisWinMan();
    if (!find(swm)) add(swm);
 }
}

void MultiZoom::delSeisPlot(SeisPlot * /*sp*/)
{

}



void MultiZoom::zoomUp()
{
  void *x;
  SeisPlot *sp;
  for (SeisWinMan *swm= top(&x); (swm); swm= next(&x)) {
          sp=  swm->currentSP();
          _inform->addSeisPlot(sp);
          sp->zoomUp();
  }
}

void MultiZoom::zoomDown()
{
  void *x;
  SeisPlot *sp;
  for (SeisWinMan *swm= top(&x); (swm); swm= next(&x)) {
          sp=  swm->currentSP();
          _inform->addSeisPlot(sp);
          sp->zoomDown();
  }
}

void MultiZoom::originalSize()
{
  void *x;
  SeisPlot *sp;
  for (SeisWinMan *swm= top(&x); (swm); swm= next(&x)) {
          sp=  swm->currentSP();
          _inform->addSeisPlot(sp);
          sp->originalSize();
  }
}

void MultiZoom::zoomUpSeparateWin()
{
  void *x;
  SeisPlot *sp;
  for (SeisWinMan *swm= top(&x); (swm); swm= next(&x)) {
          sp=  swm->currentSP();
          _inform->addSeisPlot(sp);
          sp->zoomUpSeparateWin();
  }
}

void MultiZoom::abortOthers(SeisPlot *sp)
{
  void *x;
  SeisZoomer *sz;
  if (!_allbut_sp) _allbut_sp= sp;
  for (SeisWinMan *swm= top(&x); (swm); swm= next(&x)) {
          if (_allbut_sp != swm->currentSP()) {
               sz= currentZoomer(&x);
               setCurrentZoomer(&x,NULL);
               if (sz) sz->zoomAbort();
          } // end if
  }
  if (sp == _allbut_sp) _allbut_sp= NULL;

}

void MultiZoom::setZoomer(SeisPlot *sp, SeisZoomer *zoomer)
{
    void *x;
    if (find(sp->getSeisWinMan(),&x))
          setCurrentZoomer(&x,zoomer);
}
