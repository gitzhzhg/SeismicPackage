
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
//--------------------- fg_informer.cc ---------------------//
//--------------------- fg_informer.cc ---------------------//
//--------------------- fg_informer.cc ---------------------//

//         implementation file for the FgInformer class
//                 not derived from any class
//                      subdirectory geom


#include "geom/fg_informer.hh"
#include "geom/fg_inform_list.hh"
#include "geom/fg_inform.hh"
#include "cprim.h"
#include <iostream.h>
#include <math.h>
#include <stdlib.h>
#include <string.h>
#include <assert.h>

//#define DEBUG  TRUE
#define DEBUG  FALSE


//------------------------- rules ------------------------------//
//------------------------- rules ------------------------------//
//------------------------- rules ------------------------------//

// (1) FieldGeometry does not ask FgInform any questions
//        (all FgInform functions are type void).
//
// (2) FgInformer does not ask FieldGeometry any questions
//        (FgInformer does not need the FieldGeometry header file).
//
// (3) FgInformer does not tell FieldGeometry anything
//        (FgInformer does not need the FieldGeometry header file).
//
// (4) FgInformer remembers all the information it needs for
//        dispatching messages, based on messages it gets from
//        FieldGeometry.
//
// (5) FieldGeometry remembers everything it needs for its internal
//        operation, to reply to questions from outside; it does
//        not rely on FgInformer for anything.



//---------------------- constructor -----------------------//
//---------------------- constructor -----------------------//
//---------------------- constructor -----------------------//

     // to be called from the FieldGeometry constructor
     // BEFORE any internal FieldGeometry objects are created.

     // otherwise there might be an attempt to call this
     // object before it has been created.

FgInformer::FgInformer(FieldGeometry *fg)
           :
                 _started          (FALSE),
                 _multiple         (0),
                 _frozen           (FALSE),
                 _slow             (FALSE),
                 _looping          (FALSE),
                 _quitting         (FALSE),
                 _fg               (fg),
                 _inform_list      (NULL)
{
  assert(_fg);
  _inform_list = new FgInformList();
}



//------------------------ destructor --------------------------//
//------------------------ destructor --------------------------//
//------------------------ destructor --------------------------//

     // to be called from the FieldGeometry destructor
     // AFTER all internal FieldGeometry objects have been
     // deleted.

     // otherwise there might be an attempt to call this
     // object after it has been deleted.

FgInformer::~FgInformer()
{
  dataNowGone();
  delete _inform_list;
}



//---------------- add and remove inform object ----------------//
//---------------- add and remove inform object ----------------//
//---------------- add and remove inform object ----------------//

            // called (indirectly) from inform object
            //       constructor/destructor.


void FgInformer::addInformObject(FgInform *inform)
{
  _inform_list->add(inform);
}


void FgInformer::removeInformObject(FgInform *inform)
{
  _inform_list->remove(inform);
}



//---------------------- loop macro -----------------------------//
//---------------------- loop macro -----------------------------//
//---------------------- loop macro -----------------------------//

// first  argument is the FgInform function (incl args) to call.
// second argument is top  (for forward loop) or bottom (for backward loop).
// third  argument is next (for forward loop) or prev   (for backward loop).
// fourth argument is TRUE if OK to loop if already looping.

#define LOOP(XXXX, top, next, safe)                          \
      {                                                      \
      int keep = _looping;                                   \
      if(!safe) assert(!_looping);                           \
      _looping = TRUE;                                       \
      void *p;                                               \
      for(FgInform *inform = _inform_list->top(&p);          \
                    inform;                                  \
                    inform = _inform_list->next(&p))         \
           {                                                 \
           if(!inform->messagesAreDisabled()) inform->XXXX;  \
           }                                                 \
      _looping = keep;                                       \
      }



//------------------------ macros --------------------------------//
//------------------------ macros --------------------------------//
//------------------------ macros --------------------------------//

   //  "MAYBE_" means execute only if updates are not frozen.
   //  loops are not executed after _quitting is set to TRUE.


#define    MAYBE_LOOP(XXXX)                           \
                 if(!_frozen && !_quitting) LOOP(XXXX, top   , next, FALSE)
#define  FORWARD_LOOP(XXXX)  if(!_quitting) LOOP(XXXX, top   , next, FALSE)
#define BACKWARD_LOOP(XXXX)  if(!_quitting) LOOP(XXXX, bottom, prev, FALSE)
#define     SAFE_LOOP(XXXX)  if(!_quitting) LOOP(XXXX, top   , next, TRUE)

#define       PRE                         startingChanges();
#define       POST                        finishedChanges();

#define MAYBE_PRE         if(!_frozen)    startingChanges();
#define MAYBE_POST        if(!_frozen)    finishedChanges();



//------------------- private functions ---------------------------//
//------------------- private functions ---------------------------//
//------------------- private functions ---------------------------//


void FgInformer::startingChanges()
{
  if(DEBUG) cout << "startingChanges  multiple=" << _multiple << endl;
  _multiple++;
  if(_started) return;
  _started = TRUE;
  FORWARD_LOOP(startingChanges(_fg))
}


void FgInformer::finishedChanges()
{
  if(DEBUG) cout << "finishedChanges  multiple=" << _multiple << endl;
  _multiple--;
  if(_multiple > 0) return;
  if(!_started) return;
  _started = FALSE;
  if(_slow)
      {
      BACKWARD_LOOP(postSlowOperations(_fg))
      _slow = FALSE;
      }
  BACKWARD_LOOP(finishedChanges(_fg))
}


void FgInformer::dataNowGone()
{
  assert(_quitting);
  LOOP(dataNowGone(), top, next, FALSE)
}



//--------------------- data going away ------------------------//
//--------------------- data going away ------------------------//
//--------------------- data going away ------------------------//

     // to be called from the FieldGeometry destructor
     // BEFORE any internal FieldGeometry objects are deleted.

     // the users are first notified that the data object
     // is going away, so that any cleanup (such as saving
     // data) can be performed by the users.

     // then a flag is set so that the users will not get
     // any more messages as the FieldGeometry data is
     // disappearing.

void FgInformer::dataGoingAway()
{
  BACKWARD_LOOP(dataGoingAway(_fg));
  _quitting = TRUE;
}



//------------------ miscellaneous notifications ---------------------//
//------------------ miscellaneous notifications ---------------------//
//------------------ miscellaneous notifications ---------------------//

   // these are not generally bracketed by PRE and POST.
   // the following can be called by a data user from within a loop:
   //   ringBell, showMessage, sendMessage
   // the following should be called only by FgWatchInform:
   //   returningToEventLoop

void FgInformer::preMultipleOperations()
{
  if(DEBUG) cout << "preMultipleOperations  multiple=" << _multiple << endl;
  _multiple++;
}


void FgInformer::postMultipleOperations()
{
  if(DEBUG) cout << "postMultipleOperations  multiple=" << _multiple << endl;
  POST
}


void FgInformer::ringBell()
{
  SAFE_LOOP(ringBell(_fg))
}


void FgInformer::showMessage(char *msg)
{
  SAFE_LOOP(showMessage(_fg, msg))
}


void FgInformer::sendMessage(char *msg, long i1, long i2, long i3,
                                                 long i4, long i5)
{
  SAFE_LOOP(sendMessage(_fg, msg, i1, i2, i3, i4, i5))
}


void FgInformer::returningToEventLoop()
{
  SAFE_LOOP(returningToEventLoop(_fg))
}



//--------------- always inform specific change ----------------------//
//--------------- always inform specific change ----------------------//
//--------------- always inform specific change ----------------------//

    // these always call the informees.
    // these are always bracketed by PRE and POST.
    // the following can be called by a data user from within a loop:
    //   preSlowOperations, postSlowOperations, turnOffDataNeedsSavingFlag


void FgInformer::preSlowOperations()
{
  if(DEBUG) cout << "preSlowOperations  multiple=" << _multiple << endl;
  if(_looping)
      {
      if(_slow) return;
      SAFE_LOOP(preSlowOperations(_fg))
      }
  else
      {
      PRE
      if(_slow) return;
      FORWARD_LOOP(preSlowOperations(_fg))
      }
  _slow = TRUE;
}


void FgInformer::postSlowOperations()
{
  if(DEBUG) cout << "postSlowOperations  multiple=" << _multiple << endl;
  if(_looping) return;
  POST
}



void FgInformer::dataNeedsSavingFlagTurnedOn()
{
  PRE
  FORWARD_LOOP(dataNeedsSavingFlagTurnedOn(_fg))
  POST
}

void FgInformer::dataNeedsSavingFlagTurnedOff()
{
  PRE                        // added 9/25/95
  SAFE_LOOP(dataNeedsSavingFlagTurnedOff(_fg))
  POST                       // added 9/25/95
}



void FgInformer::freezingDependentUpdates()
{
  PRE
  FORWARD_LOOP(freezingDependentUpdates(_fg))
  _frozen = TRUE;
  POST
}

void FgInformer::dependentValuesOutOfDate()
{
  PRE
  FORWARD_LOOP(dependentValuesOutOfDate(_fg))
  POST
}


void FgInformer::preResumeDependentUpdates()
{
  PRE
  FORWARD_LOOP(preResumeDependentUpdates(_fg))
  FORWARD_LOOP(showMessage(_fg, "updating dependent values..."))
}

void FgInformer::postResumeDependentUpdates()
{
  BACKWARD_LOOP(postResumeDependentUpdates(_fg))
  BACKWARD_LOOP(showMessage(_fg, "dependent values are up-to-date"))
  _frozen = FALSE;
  POST
}



void FgInformer::preChangeDataLock()
{
  PRE
  FORWARD_LOOP(preChangeDataLock(_fg))
}

void FgInformer::postChangeDataLock()
{
  FORWARD_LOOP(postChangeDataLock(_fg))
  POST
}



void FgInformer::preNewGridTransform()
{
  PRE
  FORWARD_LOOP(preNewGridTransform(_fg))
}

void FgInformer::postNewGridTransform()
{
  FORWARD_LOOP(postNewGridTransform(_fg))
  POST
}



void FgInformer::preNewTestingGridTransform()
{
  PRE
  FORWARD_LOOP(preNewTestingGridTransform(_fg))
}

void FgInformer::postNewTestingGridTransform()
{
  FORWARD_LOOP(postNewTestingGridTransform(_fg))
  POST
}



void FgInformer::sourceGathersOutOfDate()
{
  PRE
  FORWARD_LOOP(sourceGathersOutOfDate(_fg))
  POST
}

void FgInformer::preUpdateSourceGathers()
{
  PRE
  FORWARD_LOOP(preUpdateSourceGathers(_fg))
}

void FgInformer::postUpdateSourceGathers()
{
  FORWARD_LOOP(postUpdateSourceGathers(_fg))
  POST
}



void FgInformer::receiverGathersOutOfDate()
{
  PRE
  FORWARD_LOOP(receiverGathersOutOfDate(_fg))
  POST
}

void FgInformer::preUpdateReceiverGathers()
{
  PRE
  FORWARD_LOOP(preUpdateReceiverGathers(_fg))
}

void FgInformer::postUpdateReceiverGathers()
{
  FORWARD_LOOP(postUpdateReceiverGathers(_fg))
  POST
}



void FgInformer::midpointGathersOutOfDate()
{
  PRE
  FORWARD_LOOP(midpointGathersOutOfDate(_fg))
  POST
}

void FgInformer::preUpdateMidpointGathers()
{
  PRE
  FORWARD_LOOP(preUpdateMidpointGathers(_fg))
}

void FgInformer::postUpdateMidpointGathers()
{
  FORWARD_LOOP(postUpdateMidpointGathers(_fg))
  POST
}



void FgInformer::liveFoldOutOfDate()
{
  PRE
  FORWARD_LOOP(liveFoldOutOfDate(_fg))
  POST
}

void FgInformer::preUpdateLiveFold()
{
  PRE
  FORWARD_LOOP(preUpdateLiveFold(_fg))
}

void FgInformer::postUpdateLiveFold()
{
  FORWARD_LOOP(postUpdateLiveFold(_fg))
  POST
}



//--------------- sometimes inform specific change ---------------------//
//--------------- sometimes inform specific change ---------------------//
//--------------- sometimes inform specific change ---------------------//

     // these do not call the informees when updates are frozen.
     // these are always bracketed by MAYBE_PRE and MAYBE_POST.

#define MPRE(AAAA,XXXX)   \
void FgInformer::AAAA     \
{                         \
  MAYBE_PRE               \
  MAYBE_LOOP(XXXX)        \
}

#define MPOST(AAAA,XXXX)   \
void FgInformer::AAAA      \
{                          \
  MAYBE_LOOP(XXXX)         \
  MAYBE_POST               \
}


MPRE  ( preNewChaining(),  preNewChaining(_fg))
MPOST (postNewChaining(), postNewChaining(_fg))

MPRE  ( preSortByLineNumber(),  preSortByLineNumber(_fg))
MPOST (postSortByLineNumber(), postSortByLineNumber(_fg))

MPRE  ( preSortReceiverPatterns(),  preSortReceiverPatterns(_fg))
MPOST (postSortReceiverPatterns(), postSortReceiverPatterns(_fg))

MPRE  ( preReverseLineDirection(long ixl),  preReverseLineDirection(_fg, ixl))
MPOST (postReverseLineDirection(long ixl), postReverseLineDirection(_fg, ixl))

MPRE  ( preNewActiveLine(),  preNewActiveLine(_fg))
MPOST (postNewActiveLine(), postNewActiveLine(_fg))

MPRE  ( preNewActiveFlag(long ixl),  preNewActiveFlag(_fg, ixl))
MPOST (postNewActiveFlag(long ixl), postNewActiveFlag(_fg, ixl))

MPRE  ( preNewActiveRpCard(),  preNewActiveRpCard(_fg))
MPOST (postNewActiveRpCard(), postNewActiveRpCard(_fg))

MPRE  ( preNewActivePpCard(),  preNewActivePpCard(_fg))
MPOST (postNewActivePpCard(), postNewActivePpCard(_fg))

MPRE  ( preNewActiveZt1Card(),  preNewActiveZt1Card(_fg))
MPOST (postNewActiveZt1Card(), postNewActiveZt1Card(_fg))

MPRE  ( preNewActiveZt2Card(),  preNewActiveZt2Card(_fg))
MPOST (postNewActiveZt2Card(), postNewActiveZt2Card(_fg))

MPRE  ( preNewActiveZt3Card(),  preNewActiveZt3Card(_fg))
MPOST (postNewActiveZt3Card(), postNewActiveZt3Card(_fg))

MPRE  ( preNewActiveZt4Card(),  preNewActiveZt4Card(_fg))
MPOST (postNewActiveZt4Card(), postNewActiveZt4Card(_fg))

MPRE  ( preNewActiveCmp(),  preNewActiveCmp(_fg))
MPOST (postNewActiveCmp(), postNewActiveCmp(_fg))

MPRE  ( preNewActiveTrace(),  preNewActiveTrace(_fg))
MPOST (postNewActiveTrace(), postNewActiveTrace(_fg))

MPRE  ( preNewActiveGroup(),  preNewActiveGroup(_fg))
MPOST (postNewActiveGroup(), postNewActiveGroup(_fg))

MPRE  ( preCmpSelectionsChanged(),  preCmpSelectionsChanged(_fg))
MPOST (postCmpSelectionsChanged(), postCmpSelectionsChanged(_fg))

MPRE  ( preLineSelectionsChanged(long index, long nrem, long nins),
        preLineSelectionsChanged(_fg, index, nrem, nins))
MPOST (postLineSelectionsChanged(long index, long nrem, long nins),
       postLineSelectionsChanged(_fg, index, nrem, nins))



void FgInformer::preFlagValuesChanged
                 (long ixl, int ident, long index, long nrem, long nins)
{
  MAYBE_PRE
  MAYBE_LOOP(preFlagValuesChanged(_fg, ixl, ident, index, nrem, nins))
}


void FgInformer::postFlagValuesChanged
                 (long ixl, int ident, long index, long nrem, long nins)
{
  MAYBE_LOOP(postFlagValuesChanged(_fg, ixl, ident, index, nrem, nins))
  MAYBE_POST
}



void FgInformer::preRemoveInsertFlags
                             (long ixl, long index, long nrem, long nins)
{
  MAYBE_PRE
  MAYBE_LOOP(preRemoveInsertFlags(_fg, ixl, index, nrem, nins))
}


void FgInformer::postRemoveInsertFlags
                             (long ixl, long index, long nrem, long nins)
{
  MAYBE_LOOP(postRemoveInsertFlags(_fg, ixl, index, nrem, nins))
  MAYBE_POST
}



void FgInformer::preLineNumbersChanged (long index, long nrem, long nins)
{
  MAYBE_PRE
  MAYBE_LOOP(preLineNumbersChanged(_fg, index, nrem, nins))
}


void FgInformer::postLineNumbersChanged (long index, long nrem, long nins)
{
  MAYBE_LOOP(postLineNumbersChanged(_fg, index, nrem, nins))
  MAYBE_POST
}



void FgInformer::preRemoveInsertLines (long index, long nrem, long nins)
{
  MAYBE_PRE
  MAYBE_LOOP(preRemoveInsertLines(_fg, index, nrem, nins))
}


void FgInformer::postRemoveInsertLines (long index, long nrem, long nins)
{
  MAYBE_LOOP(postRemoveInsertLines(_fg, index, nrem, nins))
  MAYBE_POST
}



void FgInformer::preRemoveInsertPpCards (long index, long nrem, long nins)
{
  MAYBE_PRE
  MAYBE_LOOP(preRemoveInsertPpCards(_fg, index, nrem, nins))
}


void FgInformer::postRemoveInsertPpCards (long index, long nrem, long nins)
{
  MAYBE_LOOP(postRemoveInsertPpCards(_fg, index, nrem, nins))
  MAYBE_POST
}



void FgInformer::prePpValuesChanged
                          (int ident, long index, long nrem, long nins)
{
  MAYBE_PRE
  MAYBE_LOOP(prePpValuesChanged(_fg, ident, index, nrem, nins))
}


void FgInformer::postPpValuesChanged
                          (int ident, long index, long nrem, long nins)
{
  MAYBE_LOOP(postPpValuesChanged(_fg, ident, index, nrem, nins))
  MAYBE_POST
}




void FgInformer::preRemoveInsertRpCards (long index, long nrem, long nins)
{
  MAYBE_PRE
  MAYBE_LOOP(preRemoveInsertRpCards(_fg, index, nrem, nins))
}


void FgInformer::postRemoveInsertRpCards (long index, long nrem, long nins)
{
  MAYBE_LOOP(postRemoveInsertRpCards(_fg, index, nrem, nins))
  MAYBE_POST
}



void FgInformer::preRpValuesChanged
                          (int ident, long index, long nrem, long nins)
{
  MAYBE_PRE
  MAYBE_LOOP(preRpValuesChanged(_fg, ident, index, nrem, nins))
}


void FgInformer::postRpValuesChanged
                          (int ident, long index, long nrem, long nins)
{
  MAYBE_LOOP(postRpValuesChanged(_fg, ident, index, nrem, nins))
  MAYBE_POST
}




void FgInformer::preRemoveInsertZt1Cards (long index, long nrem, long nins)
{
  MAYBE_PRE
  MAYBE_LOOP(preRemoveInsertZt1Cards(_fg, index, nrem, nins))
}


void FgInformer::postRemoveInsertZt1Cards (long index, long nrem, long nins)
{
  MAYBE_LOOP(postRemoveInsertZt1Cards(_fg, index, nrem, nins))
  MAYBE_POST
}



void FgInformer::preZt1ValuesChanged
                          (int ident, long index, long nrem, long nins)
{
  MAYBE_PRE
  MAYBE_LOOP(preZt1ValuesChanged(_fg, ident, index, nrem, nins))
}


void FgInformer::postZt1ValuesChanged
                          (int ident, long index, long nrem, long nins)
{
  MAYBE_LOOP(postZt1ValuesChanged(_fg, ident, index, nrem, nins))
  MAYBE_POST
}




void FgInformer::preRemoveInsertZt2Cards (long index, long nrem, long nins)
{
  MAYBE_PRE
  MAYBE_LOOP(preRemoveInsertZt2Cards(_fg, index, nrem, nins))
}


void FgInformer::postRemoveInsertZt2Cards (long index, long nrem, long nins)
{
  MAYBE_LOOP(postRemoveInsertZt2Cards(_fg, index, nrem, nins))
  MAYBE_POST
}



void FgInformer::preZt2ValuesChanged
                          (int ident, long index, long nrem, long nins)
{
  MAYBE_PRE
  MAYBE_LOOP(preZt2ValuesChanged(_fg, ident, index, nrem, nins))
}


void FgInformer::postZt2ValuesChanged
                          (int ident, long index, long nrem, long nins)
{
  MAYBE_LOOP(postZt2ValuesChanged(_fg, ident, index, nrem, nins))
  MAYBE_POST
}




void FgInformer::preRemoveInsertZt3Cards (long index, long nrem, long nins)
{
  MAYBE_PRE
  MAYBE_LOOP(preRemoveInsertZt3Cards(_fg, index, nrem, nins))
}


void FgInformer::postRemoveInsertZt3Cards (long index, long nrem, long nins)
{
  MAYBE_LOOP(postRemoveInsertZt3Cards(_fg, index, nrem, nins))
  MAYBE_POST
}



void FgInformer::preZt3ValuesChanged
                          (int ident, long index, long nrem, long nins)
{
  MAYBE_PRE
  MAYBE_LOOP(preZt3ValuesChanged(_fg, ident, index, nrem, nins))
}


void FgInformer::postZt3ValuesChanged
                          (int ident, long index, long nrem, long nins)
{
  MAYBE_LOOP(postZt3ValuesChanged(_fg, ident, index, nrem, nins))
  MAYBE_POST
}




void FgInformer::preRemoveInsertZt4Cards (long index, long nrem, long nins)
{
  MAYBE_PRE
  MAYBE_LOOP(preRemoveInsertZt4Cards(_fg, index, nrem, nins))
}


void FgInformer::postRemoveInsertZt4Cards (long index, long nrem, long nins)
{
  MAYBE_LOOP(postRemoveInsertZt4Cards(_fg, index, nrem, nins))
  MAYBE_POST
}



void FgInformer::preZt4ValuesChanged
                          (int ident, long index, long nrem, long nins)
{
  MAYBE_PRE
  MAYBE_LOOP(preZt4ValuesChanged(_fg, ident, index, nrem, nins))
}


void FgInformer::postZt4ValuesChanged
                          (int ident, long index, long nrem, long nins)
{
  MAYBE_LOOP(postZt4ValuesChanged(_fg, ident, index, nrem, nins))
  MAYBE_POST
}



void FgInformer::preTredValuesChanged()
{
  MAYBE_PRE
  MAYBE_LOOP(preTredValuesChanged(_fg))
}


void FgInformer::postTredValuesChanged()
{
  MAYBE_LOOP(postTredValuesChanged(_fg))
  MAYBE_POST
}




//-------------------------- end -------------------------------//
//-------------------------- end -------------------------------//
//-------------------------- end -------------------------------//

