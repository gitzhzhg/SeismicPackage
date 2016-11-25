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

//------------------- header_dump_inform.cc ---------------------//
//------------------- header_dump_inform.cc ---------------------//
//------------------- header_dump_inform.cc ---------------------//

//          implementation file for the HeaderDumpInform class
//                  derived from the SeisInform class
//                         subdirectory pick


#include "vu/header_dump_inform.hh"
#include "vu/header_dump_pop.hh"
#include "vu/separate_inform.hh"
#include "sp/seis_plot.hh"
#include <iostream.h>
#include <assert.h>


//--------------------- constructor and destructor ---------------//
//--------------------- constructor and destructor ---------------//
//--------------------- constructor and destructor ---------------//

HeaderDumpInform::HeaderDumpInform(SeisPlot *sp,
                                   HeaderDumpPop *headpop)
                  : SeisInform(sp),
                    _headpop     (headpop),
                    _sep_inform  (NULL)
{
  assert(sp && headpop);
  _sep_inform = new SeparateInform(sp, sepDestroyedTrap, this,
                                       NULL            , NULL);
}


HeaderDumpInform::~HeaderDumpInform()
{
  delete _sep_inform;
}



//--------------- trap called from SeparateInform --------------//
//--------------- trap called from SeparateInform --------------//
//--------------- trap called from SeparateInform --------------//


void HeaderDumpInform::sepDestroyedTrap(void *data, SeisPlot *sp,
                                               SeisPlot *)
{
  assert(data);
  // cout << "HeaderDumpInform::sepDestroyedTrap" << endl;
  HeaderDumpInform *headinform = (HeaderDumpInform*)data;
  headinform->_headpop->stopPicking();
  headinform->_headpop->newDisplay(sp);
}



//------------ new display (called from SeisPlot) ----------------//
//------------ new display (called from SeisPlot) ----------------//
//------------ new display (called from SeisPlot) ----------------//

void HeaderDumpInform::postZoomSeparateWindow
                                 (SeisPlot *, SeisPlot *zoomsp)
{
  // cout << "HeaderDumpInform::postZoomSeparateWindow" << endl;
  _headpop->stopPicking();
  _headpop->newDisplay(zoomsp);
  _sep_inform->addSeisPlot(zoomsp);
}


void HeaderDumpInform::postZoom  (SeisPlot *sp, SeisPlot::ZoomDir)
{
  _headpop->newDisplay(sp);
}


void HeaderDumpInform::postScan  (SeisPlot *sp, SeisPlot::ScanDir)
{
  _headpop->newDisplay(sp);
}


void HeaderDumpInform::postMovie (SeisPlot *sp, SeisPlot::MovieDir)
{
  _headpop->newDisplay(sp);
}


void HeaderDumpInform::newPlot   (SeisPlot *sp)
{
  // cout << "HeaderDumpInform::newPlot" << endl;
  _headpop->stopPicking();
  _headpop->newDisplay(sp);
}


void HeaderDumpInform::dragImage (SeisPlot *sp)
{
  _headpop->newDisplay(sp);
}



//------------ end display (called from SeisPlot) ----------------//
//------------ end display (called from SeisPlot) ----------------//
//------------ end display (called from SeisPlot) ----------------//

void HeaderDumpInform::preZoom   (SeisPlot *sp, SeisZoomer *,
                                  SeisPlot::ZoomDir)
{
  _headpop->endDisplay(sp);
}


void HeaderDumpInform::preScan   (SeisPlot *sp, SeisPlot::ScanDir)
{
  _headpop->endDisplay(sp);
}


void HeaderDumpInform::preMovie  (SeisPlot *sp, SeisPlot::MovieDir)
{
  _headpop->endDisplay(sp);
}


void HeaderDumpInform::preDataChange (SeisPlot *sp)
{
  // cout << "HeaderDumpInform::preDataChange" << endl;
  _headpop->stopPicking();
  _headpop->endDisplay(sp);
}


void HeaderDumpInform::prePlot (SeisPlot *sp)
{
  // cout << "HeaderDumpInform::prePlot" << endl;
  _headpop->stopPicking();
  _headpop->endDisplay(sp);
}


void HeaderDumpInform::noPlotDisplayed (SeisPlot *sp)
{
  // cout << "HeaderDumpInform::noPlotDisplayed" << endl;
  _headpop->stopPicking();
  _headpop->endDisplay(sp);
}


void HeaderDumpInform::destroyed (SeisPlot *sp)
{
  // cout << "HeaderDumpInform::destroyed" << endl;
  _headpop->stopPicking();
  _headpop->endDisplay(sp);
  //SeisInform::destroyed(sp);  <-- this is called automaticly now
}

void HeaderDumpInform::notCurrentInWindow (SeisPlot *sp)
{
   addSeisPlot(sp->currentSPInWindow());
  _headpop->restartDisplay(sp->currentSPInWindow());
}

//--------------------------- end -------------------------------//
//--------------------------- end -------------------------------//
//--------------------------- end -------------------------------//
