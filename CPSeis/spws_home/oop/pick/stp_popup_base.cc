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

//---------------------- stp_popup_base.cc -----------------------//
//---------------------- stp_popup_base.cc -----------------------//
//---------------------- stp_popup_base.cc -----------------------//

//           implementation file for the StpPopupBase class
//                 derived from the SLDialog class
//                       subdirectory pick

          // The prefix Stp... means "seismic trace picking"

//    This is a base class for popups which control the picking of
//    seismic traces displayed by SeisPlot.  This class uses the
//    following friend classes strictly internally:
//                StpInform
//                StpPick

//    For each class derived from this class, you must also derive
//    a class from each of the following base classes:
//                SLFilePairPlus
//                SeisVectLinkedList

//    Classes derived from this class are to complete the popup by
//    adding an object derived from SLFilePairPlus to the SLSmartForm
//    object returned by getWorkArea(), and by also adding any required
//    SLDelay children.  These should be done in the constructor.

//    Classes derived from this class must notify this class what
//    the SLFilePairPlus object is by calling setPairBase from
//    the constructor.

//    Classes derived from this class must create and return a
//    SeisVectLinkedList object when the virtual function
//    createVectors is called, and delete it when the virtual
//    function deleteVectors is called.

//    There are several virtual functions which may necessarily or
//    optionally be overridden in a derived class.

//    The function stopPicking() must be called from the destructor
//    of the derived class.  It does not matter whether picking
//    is actually in progress at the time.

// possible values of why passed to updateVectors:
//     _UPDATE_REQUESTED  _STARTING  _NEW_DISPLAY  _LEFT  _RIGHT

// possible values of why passed to updateFile:
//     _UPDATE_REQUESTED  _STOPPING  _END_DISPLAY  _NEW_DISPLAY_COMING


#include "pick/stp_popup_base.hh"
#include "pick/stp_inform.hh"
#include "pick/stp_pick.hh"
#include "vect/vector.hh"
#include "vect/ll_seis_vect.hh"
#include "sp/seis_plot.hh"
#include "sp/seis_winman.hh"
#include "sl/sl_file_pair_plus.hh"
#include "sl/sl_smart_form.hh"
#include "sl/slp_push.hh"
#include "oprim/static_utils.hh"
#include "cprim.h"
#include <stream.h>
#include <iostream.h>
#include <assert.h>


//---------------- vector hold and flush functions -------------//
//---------------- vector hold and flush functions -------------//
//---------------- vector hold and flush functions -------------//

           // these functions call special non-member
           // functions associated with static data
           // in the vector and/or vector-linked-list
           // classes

void StpPopupBase::holdAllVectors()
{
  if(!SU::isHoldingVectors())  SU::holdVectors();
}


void StpPopupBase::flushAllVectors()
{
  if( SU::isHoldingVectors()) SU::flushVectors();
}



//--------------------- static traps -------------------------//
//--------------------- static traps -------------------------//
//--------------------- static traps -------------------------//

void StpPopupBase::okTrap(void *data, long /*ident*/)
{
  StpPopupBase *pop = (StpPopupBase*)data;
  Boolean doit = pop->startPicking();
  if(doit) pop->unmanage();
}


void StpPopupBase::applyTrap(void *data, long /*ident*/)
{
  StpPopupBase *pop = (StpPopupBase*)data;
  pop->startPicking();
}


void StpPopupBase::updateTrap(void *data, long /*ident*/)
{
  StpPopupBase *pop = (StpPopupBase*)data;
  SeisPlot     *sp  = pop->getSeisPlot();
  assert(pop->_picking);
/*
  pop->saveToFile   (pop->_sp, _UPDATE_REQUESTED);
  holdAllVectors();
  pop->updateVectors(pop->_sp, _UPDATE_REQUESTED);  // virtual function
*/
  assert(sp);
  pop->saveToFile   (sp, _UPDATE_REQUESTED);
  holdAllVectors();
  pop->updateVectors(sp, _UPDATE_REQUESTED);      // virtual function
  flushAllVectors();
}


void StpPopupBase::endTrap(void *data, long /*ident*/)
{
  StpPopupBase *pop = (StpPopupBase*)data;
  pop->stopPicking();
}



//--------------------- static update functions ----------------//
//--------------------- static update functions ----------------//
//--------------------- static update functions ----------------//

long StpPopupBase::okSenseUpfun(void *data)
{
  StpPopupBase *pop = (StpPopupBase*)data;
  return !pop->_picking;
}


long StpPopupBase::updateSenseUpfun(void *data)
{
  StpPopupBase *pop = (StpPopupBase*)data;
  return (pop->_picking && pop->_changed);
}


long StpPopupBase::endSenseUpfun(void *data)
{
  StpPopupBase *pop = (StpPopupBase*)data;
  return pop->_picking;
}




//--------------------- constructor ------------------------//
//--------------------- constructor ------------------------//
//--------------------- constructor ------------------------//


StpPopupBase::StpPopupBase(SLDelay *slparent, char *name, HelpCtx hctx,
                           SeisPlot          *sp,
                           const Boolean      want_update_button,
                           const Boolean      want_keyhelp_button,
                           const char * const picking_mode,
                           const char * const help_token,
                           const char * const help_fallback)
            : SLDialog(slparent, name, hctx),
/*
                 _sp             (sp),         // removed 9/4/97
*/
                 _winman         (sp->getSeisWinMan()),   // new 9/4/97
                 _inform         (NULL),
                 _pick           (NULL),
                 _vect           (NULL),        // new 9/2/97
                 _picking_mode   (newstr((char*)picking_mode )),
                 _help_token     (newstr((char*)help_token   )),
                 _help_fallback  (newstr((char*)help_fallback)),
                 _rub_color_name (newstr("black")),
                 _cursor         (XC_plus),
                 _rub_line_width (3),
                 _picking        (FALSE),
                 _changed        (FALSE),
                 _ref            (NULL)
{
  assert(_winman && _picking_mode && _help_token && _help_fallback);

  SLpPush *push_ok     = addBottomOK();
  SLpPush *push_apply  = addBottomApply();
                         addBottomRemove();
  if(want_update_button)
      {
      SLpPush *push_update = addBottomPush("Update");
      push_update->setAtrap     (updateTrap       , this);
      push_update->setupSenseFun(updateSenseUpfun , this);
      }
  SLpPush *push_end    = addBottomPush("End Picking");
  if(want_keyhelp_button)addBottomKeyhelp();
                         addBottomHelp();

  push_ok    ->setAtrap     (okTrap         , this);
  push_apply ->setAtrap     (applyTrap      , this);
  push_end   ->setAtrap     (endTrap        , this);
  push_ok    ->setupSenseFun(okSenseUpfun   , this);
  push_apply ->setupSenseFun(okSenseUpfun   , this);
  push_end   ->setupSenseFun(endSenseUpfun  , this);
 
  update();
}



//------------------------- destructor -------------------------//
//------------------------- destructor -------------------------//
//------------------------- destructor -------------------------//

              // stopPicking() must be called from
              //   destructor of derived class

StpPopupBase::~StpPopupBase()
{
  free(_picking_mode);
  free(_help_token);
  free(_help_fallback);
  free(_rub_color_name);
}



//----------------- set changed to true ------------------//
//----------------- set changed to true ------------------//
//----------------- set changed to true ------------------//

  // called from StpPick after calling pickingActionCompleted

void StpPopupBase::setChangedToTrue()
{
  _changed = TRUE;
}


  
//------------------ set picking variables ------------------//
//------------------ set picking variables ------------------//
//------------------ set picking variables ------------------//

   // optionally called from constructor of derived class
   // optionally called from createVectors in derived class

void StpPopupBase::setPickingColorName(const char * const rub_color_name)
{
  assert(rub_color_name && _rub_color_name);
  free(_rub_color_name);
  _rub_color_name = newstr(rub_color_name);
}


void StpPopupBase::setPickingCursor(Cursor cursor)
{
  _cursor = cursor;
}


void StpPopupBase::setPickingLineWidth(int rub_line_width)
{
  _rub_line_width = rub_line_width;
}


   // optionally called at any time:

void StpPopupBase::setHelpFallback(const char * const help_fallback)
{
  assert(help_fallback && _help_fallback);
  free(_help_fallback);
  _help_fallback = newstr((char*)help_fallback);
  if(_pick) _pick->setHelpFallback(_help_fallback);
}


void StpPopupBase::changeHelpToken(const char * const help_token)
{
  assert(help_token && _help_token);
  free(_help_token);
  _help_token = newstr((char*)help_token);
  if(_pick) _pick->changeHelpToken(_help_token);
}



//-------------------- start picking --------------------------//
//-------------------- start picking --------------------------//
//-------------------- start picking --------------------------//

           // called when ok or apply button is pushed

Boolean StpPopupBase::startPicking()
{
  assert(!_picking && !_pick && !_vect && !_inform);
  SLFilePairPlus *pair = getFilePairPlus();
  assert(pair);
  long status = pair->openFile();
  if(!pair->fileIsLoaded()) return FALSE;

  SeisPlot *sp = getSeisPlot();         // new 9/4/97
/*
  SeisVectLinkedList *vectors = createVectors(_sp);
                                          // virtual function  changed 9/2/97
*/
  _vect = createVectors(sp);              // virtual function

  if(!pair->fileIsReadOnly())
       {
       _pick = new StpPick(sp, this, _vect,
/*
       _pick = new StpPick(_sp, this, vectors,
*/
                         _picking_mode, _help_token, _help_fallback,
                         _rub_color_name, _cursor, _rub_line_width);
       }
  _inform  = new StpInform (sp, this);
  _picking = TRUE;
  holdAllVectors();
  updateVectors(sp, _STARTING);          // virtual function
  flushAllVectors();
  activityNotify(StartingActivity);
  return TRUE;
}



//---------------------- stop picking --------------------------//
//---------------------- stop picking --------------------------//
//---------------------- stop picking --------------------------//

       // must be called from destructor of derived class.
       // called from end-picking button.
       // called at any time by anyone.

void StpPopupBase::stopPicking()
{
  if(!_picking)
      {
      unmanage();     // put here 2/16/95
      return;
      }
  assert(_picking && _vect && _inform);
  SeisPlot *sp = getSeisPlot();         // new 9/4/97
  saveToFile(sp, _STOPPING);

  delete _inform;
  if(_pick) delete _pick;
  _inform = NULL;
  _pick   = NULL;
  deleteVectors();                     // virtual function
  _vect = NULL;                     // new 9/2/97

  SLFilePairPlus *pair = getFilePairPlus();
  assert(pair);
  pair->closeFile();
  _picking = FALSE;
  if(!isManaged())
      {
      activityNotify(PopDownEndingActivity);
      }
  unmanage();     // put here 2/16/95
}



//------------------ new and end display ---------------------//
//------------------ new and end display ---------------------//
//------------------ new and end display ---------------------//


   // possible value of why passed from StpInform:  _NEW_DISPLAY
   // possible value of why passed from StpInform:  _LEFT
   // possible value of why passed from StpInform:  _RIGHT

void StpPopupBase::newDisplay(SeisPlot *sp, Why why)
{
  assert(sp && _picking);
  holdAllVectors();
  updateVectors(sp, why);            // virtual function
  flushAllVectors();
  update();
}


   // possible value of why passed from StpInform:  _END_DISPLAY
   // possible value of why passed from StpInform:  _NEW_DISPLAY_COMING

void StpPopupBase::endDisplay(SeisPlot *sp, Why why)
{
  assert(sp && _picking);
  saveToFile(sp, why);
  if(why == _NEW_DISPLAY_COMING) holdAllVectors();
  update();
}



//---------------------- add and remove seis plot ----------------//
//---------------------- add and remove seis plot ----------------//
//---------------------- add and remove seis plot ----------------//

            // new 9/2/97

void StpPopupBase::addSeisPlot(SeisPlot *sp)
{
  assert(sp);
  if(_vect) _vect->addPlot(sp);
}



/*
void StpPopupBase::newCurrentSeisPlot(SeisPlot *sp)
{
  assert(sp);
  _sp = sp;
}
*/



void StpPopupBase::removeSeisPlot(SeisPlot *sp)
{
  assert(sp);
  if(_vect) _vect->removePlot(sp);
}



//----------------------- save to file ----------------------//
//----------------------- save to file ----------------------//
//----------------------- save to file ----------------------//

  // possible value of why passed from updateTrap:  _UPDATE_REQUESTED
  // possible value of why passed from stopPicking: _STOPPING
  // possible value of why passed from endDisplay:  _END_DISPLAY
  // possible value of why passed from endDisplay:  _NEW_DISPLAY_COMING

void StpPopupBase::saveToFile(SeisPlot *sp, Why why)   // private
{
  if(!_picking) assert(!_changed);
  if(!_changed) return;
  SLFilePairPlus *pair = getFilePairPlus();
  assert(pair);
  assert(pair->fileIsLoaded());
  assert(!pair->fileIsReadOnly());
  updateFile(sp, why);                   // virtual function
  _changed = FALSE;
}



//------------- manage and unmanage notify methods ---------//
//------------- manage and unmanage notify methods ---------//
//------------- manage and unmanage notify methods ---------//


Boolean StpPopupBase::preManageNotify()
{
  if(!_picking)
      {
      becomingActive();
      SLFilePairPlus *pair = getFilePairPlus();
      assert(pair);
      SeisPlot *sp = getSeisPlot();
      pair->maybeUpdate(sp->filename());
      activityNotify(PopUpNoActivity);
      }
  return TRUE;
}



void StpPopupBase::postUnmanageNotify()
{
  if(!_picking)
      {
      becomingInactive();
      activityNotify(PopDownNoActivity);
      }
}



//-------------------- access to variables ------------------------//
//-------------------- access to variables ------------------------//
//-------------------- access to variables ------------------------//

       // public.

Boolean StpPopupBase::haveDisplay()  const
{
/*
  return (_sp->memoryTraces() > 0);
*/
  SeisPlot *sp = getSeisPlot();
  return (sp->isPlotDisplayed());   // new     2/18/98
//return (sp->memoryTraces() > 0);  // removed 2/18/98
}



SeisPlot *StpPopupBase::getSeisPlot()  const
{
  return _winman->currentSP();
}



//-------------------------- end -------------------------------//
//-------------------------- end -------------------------------//
//-------------------------- end -------------------------------//
