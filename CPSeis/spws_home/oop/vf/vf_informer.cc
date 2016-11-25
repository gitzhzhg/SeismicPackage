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

//--------------------- vf_informer.cc ---------------------//
//--------------------- vf_informer.cc ---------------------//
//--------------------- vf_informer.cc ---------------------//

//        implementation file for the VfInformer class
//                 not derived from any class
//                      subdirectory vf


#include "vf/vf_informer.hh"
#include "vf/vf_inform_list.hh"
#include "vf/vf_inform.hh"
#include "named_constants.h"
#include <math.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <assert.h>
#include <time.h>
#include <iostream.h>

//#define DEBUG  TRUE
#define DEBUG  FALSE


//------------------------- rules ------------------------------//
//------------------------- rules ------------------------------//
//------------------------- rules ------------------------------//

// (1) VfManager and VfDataset do not ask VfInform any questions.
//        (all VfInform functions are type void)
//
// (2) VfInformer does not ask VfManager or VfDataset any questions.
//        (VfInformer does not need the VfManager or VfDataset header files)
//
// (3) VfInformer does not tell VfManager or VfDataset anything.
//        (VfInformer does not need the VfManager or VfDataset header files)
//
// (4) VfInformer remembers all the information it needs for dispatching
//     messages, based on messages it gets from VfManager and VfDataset.
//
// (5) VfManager and VfDataset remember everything they need for their
//     internal operation, to reply to questions from outside; they do not
//     rely on VfInformer for anything.



//---------------------- constructor -----------------------//
//---------------------- constructor -----------------------//
//---------------------- constructor -----------------------//

     // to be called from the VfManager constructor
     //   BEFORE any internal data is created.
     // otherwise there might be an attempt to call this
     //   object before it has been created.

VfInformer::VfInformer()
           :
                 _multiple         (0),
                 _slow             (0),
                 _looping          (FALSE),
                 _quitting         (FALSE),
                 _list             (NULL)
{
  _list = new VfInformList();
}



//------------------------ destructor --------------------------//
//------------------------ destructor --------------------------//
//------------------------ destructor --------------------------//

     // to be called from the VfManager destructor
     //   AFTER all internal data has been deleted.
     // otherwise there might be an attempt to call this
     //   object after it has been deleted.

VfInformer::~VfInformer()
{
  delete _list;
}



//---------------------- show working message ------------------------//
//---------------------- show working message ------------------------//
//---------------------- show working message ------------------------//

     // public.

static const clock_t time_increment  = (clock_t)(0.2 * CLOCKS_PER_SEC);
static       clock_t remember        = 0;

void VfInformer::showWorkingMessage
                          (const char *clause, long kount, long number)
{
  if(clause && kount == 10 * (kount / 10))
      {
      clock_t time = clock();
      if(time - remember > time_increment)
          {
          char show[80];
          sprintf(show, "%s %d of %d", clause, kount+1, number);
          showMessage(show);
          remember = time;
          }
      }
}



//---------------- add and remove inform object ----------------//
//---------------- add and remove inform object ----------------//
//---------------- add and remove inform object ----------------//


void VfInformer::addInformObject(VfInform *inform)
{
  _list->add(inform);
}


void VfInformer::removeInformObject(VfInform *inform)
{
  _list->remove(inform);
}



//---------------------- loop macro -----------------------------//
//---------------------- loop macro -----------------------------//
//---------------------- loop macro -----------------------------//

// first  argument is the VfInform function (incl args) to call.
// second argument is top  (for forward loop) or bottom (for backward loop).
// third  argument is next (for forward loop) or prev   (for backward loop).
// fourth argument is TRUE if OK to loop if already looping.

// loops are not executed after _quitting is set to TRUE.


#define LOOP(XXXX, top, next, safe)                          \
   if(!_quitting)                                            \
      {                                                      \
      int keep = _looping;                                   \
      if(!safe) assert(!_looping);                           \
      _looping = TRUE;                                       \
      void *p;                                               \
      for(VfInform *inform = _list->top(&p);                 \
                    inform;                                  \
                    inform = _list->next(&p))                \
           {                                                 \
           if(inform->messagesAreEnabled()) inform->XXXX;    \
           }                                                 \
      _looping = keep;                                       \
      }



//------------------------ macros --------------------------------//
//------------------------ macros --------------------------------//
//------------------------ macros --------------------------------//


#define BEFORE       beforeChanges();
#define AFTER        afterChanges();


#define  FORWARD_LOOP(XXXX)  LOOP(XXXX, top   , next, FALSE)
#define BACKWARD_LOOP(XXXX)  LOOP(XXXX, bottom, prev, FALSE)
#define     SAFE_LOOP(XXXX)  LOOP(XXXX, top   , next, TRUE)


#define PRE(AAAA,XXXX)        \
void VfInformer::AAAA         \
{                             \
  BEFORE                      \
  FORWARD_LOOP(XXXX)          \
}

#define POST(AAAA,XXXX)       \
void VfInformer::AAAA         \
{                             \
  FORWARD_LOOP(XXXX)          \
  AFTER                       \
}



//------------------ before and after changes ---------------------//
//------------------ before and after changes ---------------------//
//------------------ before and after changes ---------------------//

   // data users are notified only when the first nested beforeChanges
   // and the last nested afterChanges calls are made.

void VfInformer::beforeChanges()
{
  if(DEBUG) cout << "beforeChanges  multiple=" << _multiple << endl;
  _multiple++;
  if(_multiple > 1) return;
  beginSlowOperations();
  FORWARD_LOOP(beforeChanges())
}


void VfInformer::afterChanges()
{
  if(DEBUG) cout << "afterChanges  multiple=" << _multiple << endl;
  _multiple--;
  if(_multiple > 0) return;
  BACKWARD_LOOP(afterChanges())
  endSlowOperations();
}



//------------------ begin and end slow operations ------------------//
//------------------ begin and end slow operations ------------------//
//------------------ begin and end slow operations ------------------//

   // data users are notified only when the first nested beginSlowOperations
   // and the last nested endSlowOperations calls are made.

void VfInformer::beginSlowOperations()
{
  if(DEBUG) cout << "beginSlowOperations  slow=" << _slow << endl;
  _slow++;
  if(_slow > 1) return;
  SAFE_LOOP(beginSlowOperations())
}


void VfInformer::endSlowOperations()
{
  if(DEBUG) cout << "endSlowOperations  slow=" << _slow << endl;
  _slow--;
  if(_slow > 0) return;
  SAFE_LOOP(endSlowOperations())
}



//------------------ general use functions ---------------------------//
//------------------ general use functions ---------------------------//
//------------------ general use functions ---------------------------//


void VfInformer::ringBell()
{
  SAFE_LOOP(ringBell())
}


void VfInformer::showMessage(const char *msg)
{
  SAFE_LOOP(showMessage(msg))
}


void VfInformer::sendMessage(const char *code, const char *msg,
                             long i1, long i2, long i3, long i4, long i5)
{
  SAFE_LOOP(sendMessage(code, msg, i1, i2, i3, i4, i5))
}


void VfInformer::findActiveVelocityFunction(VfDataset *dataset)
{
  SAFE_LOOP(findActiveVelocityFunction(dataset))
}



//----------------- returning to event loop ----------------------//
//----------------- returning to event loop ----------------------//
//----------------- returning to event loop ----------------------//

     // should be called only by the VfguiWatch class.

void VfInformer::returningToEventLoop()
{
  SAFE_LOOP(returningToEventLoop())
}



//--------------------- data going away ------------------------//
//--------------------- data going away ------------------------//
//--------------------- data going away ------------------------//

     // to be called from the VfManager destructor before
     // any internal data is destroyed.

     // the users are first notified that the data object
     // is going away, so that any cleanup (such as saving
     // data) can be performed by the users.

     // then the base class of each user is notified that the
     // data object is gone, so that the base class can set
     // its VfDataset pointer to NULL.

     // finally a flag is set so that the users will not get
     // any more messages as the VfDataset objects (and the
     // VfManager object) are disappearing.

void VfInformer::dataGoingAway()
{
  BACKWARD_LOOP(dataGoingAway());
  BACKWARD_LOOP(dataNowGone());
  _quitting = TRUE;
}



//------------------------- new history card ----------------------//
//------------------------- new history card ----------------------//
//------------------------- new history card ----------------------//

      // to be called only by VfDataset.

void VfInformer::newHistoryCard(VfDataset *dataset)
{
  FORWARD_LOOP(newHistoryCard(dataset))
}



//-------------------- data change notifications ---------------------//
//-------------------- data change notifications ---------------------//
//-------------------- data change notifications ---------------------//

      // to be called only by VfManager or VfUtilities or VfHelper.

PRE  ( preChangeBinTolerances(),  preChangeBinTolerances())
POST (postChangeBinTolerances(), postChangeBinTolerances())


PRE  ( preNewActiveDataset(),  preNewActiveDataset())
POST (postNewActiveDataset(), postNewActiveDataset())


PRE  ( preNewReferenceDataset(),  preNewReferenceDataset())
POST (postNewReferenceDataset(), postNewReferenceDataset())


PRE  ( preSelectDataset(VfDataset *dataset),  preSelectDataset(dataset))
POST (postSelectDataset(VfDataset *dataset), postSelectDataset(dataset))


PRE  ( preUnselectDataset(VfDataset *dataset),  preUnselectDataset(dataset))
POST (postUnselectDataset(VfDataset *dataset), postUnselectDataset(dataset))


PRE  ( preTotalChanges(VfDataset *dataset),  preTotalChanges(dataset))
POST (postTotalChanges(VfDataset *dataset), postTotalChanges(dataset))


PRE  ( preRemoveInsertDatasets(long index, long nrem, long nins),
       preRemoveInsertDatasets(index, nrem, nins))
POST (postRemoveInsertDatasets(long index, long nrem, long nins),
      postRemoveInsertDatasets(index, nrem, nins))


PRE  ( preChangeDataLock(VfDataset *dataset),  preChangeDataLock(dataset))
POST (postChangeDataLock(VfDataset *dataset), postChangeDataLock(dataset))


void VfInformer::dataNeedsSavingFlagTurnedOn(VfDataset *dataset)
{
  BEFORE
  FORWARD_LOOP(dataNeedsSavingFlagTurnedOn(dataset))
  AFTER
}

void VfInformer::dataNeedsSavingFlagTurnedOff(VfDataset *dataset)
{
  BEFORE
  SAFE_LOOP(dataNeedsSavingFlagTurnedOff(dataset))
  AFTER
}


PRE  ( preChangeHeaderWords(VfDataset *dataset),
       preChangeHeaderWords(dataset))
POST (postChangeHeaderWords(VfDataset *dataset),
      postChangeHeaderWords(dataset))


PRE  ( preChangeMoveoutOrder(VfDataset *dataset),
       preChangeMoveoutOrder(dataset))
POST (postChangeMoveoutOrder(VfDataset *dataset),
      postChangeMoveoutOrder(dataset))


PRE  ( preChangeUnits(VfDataset *dataset),
       preChangeUnits(dataset))
POST (postChangeUnits(VfDataset *dataset),
      postChangeUnits(dataset))


PRE  ( preNewActiveVelocityFunction(VfDataset *dataset),
       preNewActiveVelocityFunction(dataset))
POST (postNewActiveVelocityFunction(VfDataset *dataset),
      postNewActiveVelocityFunction(dataset))


PRE  ( preNewReferenceVelocityFunction(VfDataset *dataset),
       preNewReferenceVelocityFunction(dataset))
POST (postNewReferenceVelocityFunction(VfDataset *dataset),
      postNewReferenceVelocityFunction(dataset))


PRE  ( preNewNeighbors(VfDataset *dataset),  preNewNeighbors(dataset))
POST (postNewNeighbors(VfDataset *dataset), postNewNeighbors(dataset))


PRE  ( preRemoveInsertVelocityFunctions
                  (VfDataset *dataset, long ifun, long nrem, long nins),
       preRemoveInsertVelocityFunctions(dataset, ifun, nrem, nins))
POST (postRemoveInsertVelocityFunctions
                  (VfDataset *dataset, long ifun, long nrem, long nins),
      postRemoveInsertVelocityFunctions(dataset, ifun, nrem, nins))


PRE  ( preChangeSelections(VfDataset *dataset, long ifun, long nchng),
       preChangeSelections(dataset, ifun, nchng))
POST (postChangeSelections(VfDataset *dataset, long ifun, long nchng),
      postChangeSelections(dataset, ifun, nchng))


PRE  ( preChangeCoords(VfDataset *dataset, long ifun, long nchng),
       preChangeCoords(dataset, ifun, nchng))
POST (postChangeCoords(VfDataset *dataset, long ifun, long nchng),
      postChangeCoords(dataset, ifun, nchng))


PRE  ( preNewDefaultTypes(VfDataset *dataset, long ifun, long nchng),
       preNewDefaultTypes(dataset, ifun, nchng))
POST (postNewDefaultTypes(VfDataset *dataset, long ifun, long nchng),
      postNewDefaultTypes(dataset, ifun, nchng))


PRE  ( preModifyStrings(VfDataset *dataset, long ifun, long nchng),
       preModifyStrings(dataset, ifun, nchng))
POST (postModifyStrings(VfDataset *dataset, long ifun, long nchng),
      postModifyStrings(dataset, ifun, nchng))


PRE  ( preNewActivePicks(VfDataset *dataset, long ifun, long nchng),
       preNewActivePicks(dataset, ifun, nchng))
POST (postNewActivePicks(VfDataset *dataset, long ifun, long nchng),
      postNewActivePicks(dataset, ifun, nchng))


PRE  ( preChangePickSelections(VfDataset *dataset, long ifun),
       preChangePickSelections(dataset, ifun))
POST (postChangePickSelections(VfDataset *dataset, long ifun),
      postChangePickSelections(dataset, ifun))


PRE  ( preModifyPicks
         (VfDataset *dataset, long ifun, int type, long ipick, long nrem),
       preModifyPicks(dataset, ifun, type, ipick, nrem))
POST (postModifyPicks
         (VfDataset *dataset, long ifun, int type, long ipick, long nrem,
                                                               long nins),
      postModifyPicks(dataset, ifun, type, ipick, nrem, nins))



//-------------------- data user messages ---------------------------//
//-------------------- data user messages ---------------------------//
//-------------------- data user messages ---------------------------//


PRE  ( preNewActiveHorizon(),  preNewActiveHorizon())
POST (postNewActiveHorizon(), postNewActiveHorizon())


PRE  ( preRemoveInsertHorizons(long ihorizon, long nrem, long nins),
       preRemoveInsertHorizons(ihorizon, nrem, nins))
POST (postRemoveInsertHorizons(long ihorizon, long nrem, long nins),
      postRemoveInsertHorizons(ihorizon, nrem, nins))


PRE  ( preNewSelectedHorizons(),  preNewSelectedHorizons())
POST (postNewSelectedHorizons(), postNewSelectedHorizons())


PRE  ( preNewActiveHorizonPick(long ihorizon),
       preNewActiveHorizonPick(ihorizon))
POST (postNewActiveHorizonPick(long ihorizon),
      postNewActiveHorizonPick(ihorizon))


PRE  ( preNewHorizonColor(long ihorizon),  preNewHorizonColor(ihorizon))
POST (postNewHorizonColor(long ihorizon), postNewHorizonColor(ihorizon))


PRE  ( preNewHorizonTransform(),  preNewHorizonTransform())
POST (postNewHorizonTransform(), postNewHorizonTransform())




//-------------------------- end -------------------------------//
//-------------------------- end -------------------------------//
//-------------------------- end -------------------------------//

