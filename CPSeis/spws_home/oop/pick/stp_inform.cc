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

//------------------------ stp_inform.cc --------------------------//
//------------------------ stp_inform.cc --------------------------//
//------------------------ stp_inform.cc --------------------------//

//             implementation file for the StpInform class
//                  derived from the SeisInform class
//                         subdirectory pick

                     // used with StpPopupBase

#include "pick/stp_inform.hh"
#include "pick/stp_popup_base.hh"
#include "sp/seis_plot.hh"
#include <iostream.h>
#include <assert.h>


// possible values of why passed to newDisplay:
//       _NEW_DISPLAY    _LEFT    _RIGHT

// possible values of why passed to endDisplay:
//       _END_DISPLAY   _NEW_DISPLAY_COMING


//static Boolean printit = True;
static Boolean printit = False;

//--------------------- constructor and destructor ---------------//
//--------------------- constructor and destructor ---------------//
//--------------------- constructor and destructor ---------------//

StpInform::StpInform(SeisPlot *sp, StpPopupBase *pop)
                  : SeisInform(sp),
/*
                      _sp          (sp),     // removed 9/2/97
*/
                      _pop         (pop)
{
  assert(sp && pop);
}


StpInform::~StpInform()
{
}



/*
//--------------- trap called from SeparateInform --------------//
//--------------- trap called from SeparateInform --------------//
//--------------- trap called from SeparateInform --------------//


void StpInform::sepDestroyedTrap(void *data, SeisPlot *sp,
                                             SeisPlot *)
{
  assert(data);
  if(printit) cout << "StpInform::sepDestroyedTrap" << endl;
  StpInform *inform = (StpInform*)data;
  inform->_pop->newDisplay(sp, StpPopupBase::_NEW_DISPLAY);
}
*/



//------------ new display (called from SeisPlot) ----------------//
//------------ new display (called from SeisPlot) ----------------//
//------------ new display (called from SeisPlot) ----------------//

void StpInform::postZoomSeparateWindow
                                 (SeisPlot *sp, SeisPlot *)
{
  if(printit) cout << "StpInform::postZoomSeparateWindow" << endl;
  _pop->newDisplay(sp, StpPopupBase::_NEW_DISPLAY);
}


void StpInform::postZoom(SeisPlot *sp, SeisPlot::ZoomDir dir)
{
  if(printit) cout << "StpInform::postZoom dir = " << dir << endl;
  _pop->newDisplay(sp, StpPopupBase::_NEW_DISPLAY);
}


void StpInform::postScan(SeisPlot *sp, SeisPlot::ScanDir dir)
{
  if(printit) cout << "StpInform::postScan dir = " << dir << endl;
  StpPopupBase::Why why;
  switch(dir)
    {
    case SeisPlot::Right: why = StpPopupBase::_RIGHT      ; break;
    case SeisPlot::Left : why = StpPopupBase::_LEFT       ; break;
    default             : why = StpPopupBase::_NEW_DISPLAY; break;
    }
  _pop->newDisplay(sp, why);
}


void StpInform::postMovie (SeisPlot *sp, SeisPlot::MovieDir dir)
{
  if(printit) cout << "StpInform::postMovie dir = " << dir << endl;
  StpPopupBase::Why why;
  switch(dir)
    {
    case SeisPlot::StepForward : why = StpPopupBase::_RIGHT      ; break;
    case SeisPlot::StepBackward: why = StpPopupBase::_LEFT       ; break;
    default                    : why = StpPopupBase::_NEW_DISPLAY; break;
    }
  _pop->newDisplay(sp, why);
}


void StpInform::newPlot(SeisPlot *sp)
{
  if(printit) cout << "StpInform::newPlot" << endl;
  _pop->newDisplay(sp, StpPopupBase::_NEW_DISPLAY);
}


void StpInform::dragImage (SeisPlot *sp)
{
  if(printit) cout << "StpInform::dragImage" << endl;
  _pop->newDisplay(sp, StpPopupBase::_NEW_DISPLAY);
}



//------------ end display (called from SeisPlot) ----------------//
//------------ end display (called from SeisPlot) ----------------//
//------------ end display (called from SeisPlot) ----------------//

void StpInform::preZoom   (SeisPlot *sp, SeisZoomer *,
                                        SeisPlot::ZoomDir dir)
{
  if(printit) cout << "StpInform::preZoom dir = " << dir << endl;
  _pop->endDisplay(sp, StpPopupBase::_END_DISPLAY);
}


void StpInform::preScan   (SeisPlot *sp, SeisPlot::ScanDir dir)
{
  if(printit) cout << "StpInform::preScan dir = " << dir << endl;
  _pop->endDisplay(sp, StpPopupBase::_NEW_DISPLAY_COMING);
}


void StpInform::preMovie  (SeisPlot *sp, SeisPlot::MovieDir dir)
{
  if(printit) cout << "StpInform::preMovie dir = " << dir << endl;
  _pop->endDisplay(sp, StpPopupBase::_NEW_DISPLAY_COMING);
}


void StpInform::preDataChange   (SeisPlot *sp)
{
  if(printit) cout << "StpInform::preDataChange" << endl;
  _pop->endDisplay(sp, StpPopupBase::_NEW_DISPLAY_COMING);
}


void StpInform::prePlot   (SeisPlot *sp)
{
  if(printit) cout << "StpInform::prePlot" << endl;
  _pop->endDisplay(sp, StpPopupBase::_NEW_DISPLAY_COMING);
}


void StpInform::noPlotDisplayed (SeisPlot *sp)
{
  if(printit) cout << "StpInform::noPlotDisplayed" << endl;
  _pop->endDisplay(sp, StpPopupBase::_END_DISPLAY);
}


void StpInform::destroyed (SeisPlot *sp)
{
  if(printit) cout << "StpInform::destroyed" << endl;
  _pop->endDisplay(sp, StpPopupBase::_END_DISPLAY);
  SeisInform::destroyed(sp);      // apparently do-nothing now (9/2/97)
  if(find(sp))                        // new 9/2/97
      {                               // new 9/2/97
      _pop->removeSeisPlot(sp);       // new 9/2/97
      //remove(sp);                     // new 9/2/97
      delSeisPlot(sp);                     // new 9/2/97
      }                               // new 9/2/97
}



//----------------- new stuff to allow seisplot to change ------------//
//----------------- new stuff to allow seisplot to change ------------//
//----------------- new stuff to allow seisplot to change ------------//

            // new 9/2/97

void StpInform::notCurrentInWindow (SeisPlot *sp)
{
  if(printit) cout << "StpInform::notCurrentInWindow" << endl;
  _pop->endDisplay(sp, StpPopupBase::_END_DISPLAY);
  SeisPlot *sp2 = sp->currentSPInWindow();
  if(!find(sp2))
      {
      _pop->addSeisPlot(sp2);
      addSeisPlot(sp2);
      }
/*
  _pop->newCurrentSeisPlot(sp2);
*/
  if(sp2->isPlotDisplayed())
      {
      _pop->newDisplay(sp2, StpPopupBase::_NEW_DISPLAY);
      }
}



//--------------------------- end -------------------------------//
//--------------------------- end -------------------------------//
//--------------------------- end -------------------------------//
