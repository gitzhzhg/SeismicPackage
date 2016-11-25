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

//--------------------- static_informer.cc ---------------------//
//--------------------- static_informer.cc ---------------------//
//--------------------- static_informer.cc ---------------------//

//        implementation file for the StaticInformer class
//                 not derived from any class
//                      subdirectory stat


#include "stat/static_informer.hh"
#include "stat/static_inform_list.hh"
#include "stat/static_inform.hh"
#include "named_constants.h"
#include <string.h>
#include <iostream.h>
#include <stream.h>
#include <math.h>
#include <stdlib.h>
#include <stdio.h>
#include <assert.h>
#include <time.h>

//#define DEBUG  TRUE
#define DEBUG  FALSE


//------------------------- rules ------------------------------//
//------------------------- rules ------------------------------//
//------------------------- rules ------------------------------//

// (1) StaticDataset and StaticManager do not ask StaticInform
//     any questions.
//        (all StaticInform functions are type void)
//
// (2) StaticInformer does not ask StaticDataset or StaticManager
//     any questions.
//        (StaticInformer does not need the StaticDataset and
//         StaticManager header files)
//
// (3) StaticInformer does not tell StaticDataset or StaticManager
//     anything.
//        (StaticInformer does not need the StaticDataset and
//         StaticManager header files)
//
// (4) StaticInformer remembers all the information it needs for
//     dispatching messages, based on messages it gets from StaticDataset
//     and StaticManager.
//
// (5) StaticDataset and StaticManager remember everything they
//     need for their internal operation, to reply to questions from
//     outside; they do not rely on StaticInformer for anything.



//---------------------- constructor -----------------------//
//---------------------- constructor -----------------------//
//---------------------- constructor -----------------------//

     // to be called from the StaticManager constructor
     //   BEFORE any internal StaticDataset objects are created.
     // otherwise there might be an attempt to call this
     //   object before it has been created.

StaticInformer::StaticInformer()
           :
                 _multiple         (0),
                 _slow             (0),
                 _looping          (FALSE),
                 _quitting         (FALSE),
                 _list             (NULL)
{
  _list = new StaticInformList();
}



//------------------------ destructor --------------------------//
//------------------------ destructor --------------------------//
//------------------------ destructor --------------------------//

     // to be called from the StaticManager destructor
     //   AFTER all internal StaticDataset objects have been deleted.
     // otherwise there might be an attempt to call this
     //   object after it has been deleted.

StaticInformer::~StaticInformer()
{
  delete _list;
}



//---------------------- show working message ------------------------//
//---------------------- show working message ------------------------//
//---------------------- show working message ------------------------//

     // public.

static const clock_t time_increment  = (clock_t)(0.2 * CLOCKS_PER_SEC);
static       clock_t remember        = 0;

void StaticInformer::showWorkingMessage
                          (const char *clause, int kount, int number)
{
  if(kount == 10 * (kount / 10))
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


void StaticInformer::addInformObject(StaticInform *inform)
{
  _list->add(inform);
}


void StaticInformer::removeInformObject(StaticInform *inform)
{
  _list->remove(inform);
}



//---------------------- loop macro -----------------------------//
//---------------------- loop macro -----------------------------//
//---------------------- loop macro -----------------------------//

// first  argument is the StaticInform function (incl args) to call.
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
      for(StaticInform *inform = _list->top(&p);             \
                        inform;                              \
                        inform = _list->next(&p))            \
           {                                                 \
           if(!inform->messagesAreDisabled()) inform->XXXX;  \
           }                                                 \
      _looping = keep;                                       \
      }



//------------------------ macros --------------------------------//
//------------------------ macros --------------------------------//
//------------------------ macros --------------------------------//


#define BEFORE       beforeChanges();
#define AFTER        afterChanges();


#define   FORWARD_LOOP(XXXX)  LOOP(XXXX, top   , next, FALSE)
#define  BACKWARD_LOOP(XXXX)  LOOP(XXXX, bottom, prev, FALSE)
#define      SAFE_LOOP(XXXX)  LOOP(XXXX, top   , next, TRUE)


#define PRE(AAAA,XXXX)        \
void StaticInformer::AAAA     \
{                             \
  BEFORE                      \
  FORWARD_LOOP(XXXX)          \
}

#define POST(AAAA,XXXX)       \
void StaticInformer::AAAA     \
{                             \
  FORWARD_LOOP(XXXX)          \
  AFTER                       \
}



//------------------ before and after changes ---------------------//
//------------------ before and after changes ---------------------//
//------------------ before and after changes ---------------------//

   // data users are notified only when the first beforeChanges
   // and the last afterChanges calls are made.

void StaticInformer::beforeChanges()
{
  if(DEBUG) cout << "beforeChanges  multiple=" << _multiple << endl;
  _multiple++;
  if(_multiple > 1) return;
  beginSlowOperations();
  FORWARD_LOOP(beforeChanges())
}


void StaticInformer::afterChanges()
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

void StaticInformer::beginSlowOperations()
{
  if(DEBUG) cout << "beginSlowOperations  slow=" << _slow << endl;
  _slow++;
  if(_slow > 1) return;
  SAFE_LOOP(beginSlowOperations())
}


void StaticInformer::endSlowOperations()
{
  if(DEBUG) cout << "endSlowOperations  slow=" << _slow << endl;
  _slow--;
  if(_slow > 0) return;
  SAFE_LOOP(endSlowOperations())
}



//------------------ general use functions ---------------------------//
//------------------ general use functions ---------------------------//
//------------------ general use functions ---------------------------//


void StaticInformer::ringBell()
{
  SAFE_LOOP(ringBell())
}


void StaticInformer::showMessage(const char *msg)
{
  SAFE_LOOP(showMessage(msg))
}


void StaticInformer::sendMessage(const char *code, const char *msg,
                                 int i1, int i2, int i3, int i4, int i5)
{
  SAFE_LOOP(sendMessage(code, msg, i1, i2, i3, i4, i5))
}


void StaticInformer::findActiveGroundPosition(StaticDataset *dataset)
{
  SAFE_LOOP(findActiveGroundPosition(dataset))
}



//----------------- returning to event loop ----------------------//
//----------------- returning to event loop ----------------------//
//----------------- returning to event loop ----------------------//

     // should be called only by the StatguiWatch class.

void StaticInformer::returningToEventLoop()
{
  SAFE_LOOP(returningToEventLoop())
}



//--------------------- data going away ------------------------//
//--------------------- data going away ------------------------//
//--------------------- data going away ------------------------//

     // to be called from the StaticManager destructor
     // BEFORE any internal StaticDataset objects are deleted.

     // the users are first notified that the data objects
     // are going away, so that any cleanup (such as saving
     // data) can be performed by the users.

     // then the base class of each user is notified that the
     // data objects are gone, so that the base class can set
     // its StaticManager pointer to NULL.

     // finally a flag is set so that the users will not get
     // any more messages as the StaticDataset objects (and the
     // StaticManager object) are disappearing.

void StaticInformer::dataGoingAway()
{
  BACKWARD_LOOP(dataGoingAway());
  BACKWARD_LOOP(dataNowGone());
  _quitting = TRUE;
}



//-------------------- data change notifications ---------------------//
//-------------------- data change notifications ---------------------//
//-------------------- data change notifications ---------------------//


PRE  ( preNewActiveDataset(),  preNewActiveDataset())
POST (postNewActiveDataset(), postNewActiveDataset())


PRE  ( preNewReferenceDataset(),  preNewReferenceDataset())
POST (postNewReferenceDataset(), postNewReferenceDataset())


void StaticInformer::dataNeedsSavingFlagTurnedOn(StaticDataset *dataset)
{
  BEFORE
  FORWARD_LOOP(dataNeedsSavingFlagTurnedOn(dataset))
  AFTER
}

void StaticInformer::dataNeedsSavingFlagTurnedOff(StaticDataset *dataset)
{
  BEFORE
  SAFE_LOOP(dataNeedsSavingFlagTurnedOff(dataset))
  AFTER
}


PRE  ( preRemoveInsertDatasets(int index, int nrem, int nins),
       preRemoveInsertDatasets(index, nrem, nins))
POST (postRemoveInsertDatasets(int index, int nrem, int nins),
      postRemoveInsertDatasets(index, nrem, nins))


PRE  ( preSelectDataset(StaticDataset *dataset),  preSelectDataset(dataset))
POST (postSelectDataset(StaticDataset *dataset), postSelectDataset(dataset))


PRE  ( preUnselectDataset(StaticDataset *dataset),  preUnselectDataset(dataset))
POST (postUnselectDataset(StaticDataset *dataset), postUnselectDataset(dataset))


PRE  ( preChangeDataLock(StaticDataset *dataset),  preChangeDataLock(dataset))
POST (postChangeDataLock(StaticDataset *dataset), postChangeDataLock(dataset))


PRE  ( preTotalChanges(StaticDataset *dataset),  preTotalChanges(dataset))
POST (postTotalChanges(StaticDataset *dataset), postTotalChanges(dataset))


PRE  ( preNewActiveGroundPosition(StaticDataset *dataset),
       preNewActiveGroundPosition(dataset))
POST (postNewActiveGroundPosition(StaticDataset *dataset),
      postNewActiveGroundPosition(dataset))


PRE  ( preChangeSelections(StaticDataset *dataset,
                           int ix, int iy, int nxchng, int nychng),
       preChangeSelections(dataset, ix, iy, nxchng, nychng))
POST (postChangeSelections(StaticDataset *dataset,
                           int ix, int iy, int nxchng, int nychng),
      postChangeSelections(dataset, ix, iy, nxchng, nychng))


PRE  ( preChangeStaticValues(StaticDataset *dataset,
                             int ix, int iy, int nxchng, int nychng),
       preChangeStaticValues(dataset, ix, iy, nxchng, nychng))
POST (postChangeStaticValues(StaticDataset *dataset,
                             int ix, int iy, int nxchng, int nychng),
      postChangeStaticValues(dataset, ix, iy, nxchng, nychng))


PRE  ( preChangeStattype(StaticDataset *dataset),  preChangeStattype(dataset))
POST (postChangeStattype(StaticDataset *dataset), postChangeStattype(dataset))


PRE  ( preChangeHeaderWords(StaticDataset *dataset),
       preChangeHeaderWords(dataset))
POST (postChangeHeaderWords(StaticDataset *dataset),
      postChangeHeaderWords(dataset))


PRE  ( preTransformGroundPositions(StaticDataset *dataset),
       preTransformGroundPositions(dataset))
POST (postTransformGroundPositions(StaticDataset *dataset),
      postTransformGroundPositions(dataset))


//-------------------------- end -------------------------------//
//-------------------------- end -------------------------------//
//-------------------------- end -------------------------------//

