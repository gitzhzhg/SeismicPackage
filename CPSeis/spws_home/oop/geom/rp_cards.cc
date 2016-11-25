
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
//---------------------- rp_cards.cc -----------------------//
//---------------------- rp_cards.cc -----------------------//
//---------------------- rp_cards.cc -----------------------//

//            implementation file for the RpCards class
//                 derived from the SmartArray class
//                        subdirectory geom


#include "geom/rp_cards.hh"
#include "geom/rp_card.hh"
#include "geom/fg_informer.hh"
#include "geom/fg_connect.hh"
#include "geom/fg_constants.hh"
#include "geom/acc_nchan.hh"
#include "oprim/acc_search.hh"
#include "oprim/fast_sort.hh"
#include "cprim.h"
#include <iostream.h>
#include <stdlib.h>
#include <string.h>
#include <math.h>
#include <assert.h>


#define STEP      2000


//--------------------- static functions ------------------------//
//--------------------- static functions ------------------------//
//--------------------- static functions ------------------------//

#define RPCARD  ((RpCards*)data)->unsafeRpCard(index)


static float get_pattern_number(void *data, long index)
{
  return (float)RPCARD->getRpPatternNumber();
}



//---------------------- constructor -----------------------//
//---------------------- constructor -----------------------//
//---------------------- constructor -----------------------//


RpCards::RpCards(FgInformer *informer, FgConnect *connect)
           :  SmartArray(STEP),
                _frozen              (FALSE),
                _informer            (informer),
                _connect             (connect)
{
  assert(_informer);
  assert(_connect);
  _sort   = new FastSort  (this, get_pattern_number);
  _nchan  = new AccNchan  (this, _connect);
  _search = new AccSearch (this, get_pattern_number);
}



//------------------------ destructor --------------------------//
//------------------------ destructor --------------------------//
//------------------------ destructor --------------------------//

RpCards::~RpCards()
{
  deleteAllRpCards();
  delete _sort;
  delete _nchan;
  delete _search;
}



//------------- before and after new active index -----------------//
//------------- before and after new active index -----------------//
//------------- before and after new active index -----------------//

        //  private virtual functions overriding SmartArray.
        //  Called from SmartArray before and after changing
        //    the active index.
        //  The function in SmartArray is empty.


void RpCards::beforeNewActiveIndex()
{
  _informer->preNewActiveRpCard();
}


void RpCards::afterNewActiveIndex()
{
  _informer->postNewActiveRpCard();
}



//------------------- before remove insert --------------------------//
//------------------- before remove insert --------------------------//
//------------------- before remove insert --------------------------//

        //  private virtual function overriding SmartArray.
        //  Called from SmartArray before removing and/or inserting
        //    one or more array elements.
        //  The function in SmartArray is empty.


void RpCards::beforeRemoveInsert(long index, long nrem, long nins)
{
  _informer->preRemoveInsertRpCards (index, nrem, nins);
  valuesWillChange        (RP_PAT  , index, nrem, nins);
  valuesWillChange        (RP_FLAG , index, nrem, nins);
  valuesWillChange        (RP_SHOT , index, nrem, nins);
  valuesWillChange        (RP_LINE , index, nrem, nins);
  valuesWillChange        (RP_NX   , index, nrem, nins);
  valuesWillChange        (RP_XINC , index, nrem, nins);
  valuesWillChange        (RP_NY   , index, nrem, nins);
  valuesWillChange        (RP_YINC , index, nrem, nins);
  valuesWillChange        (RP_XSKID, index, nrem, nins);
  valuesWillChange        (RP_YSKID, index, nrem, nins);
  valuesWillChange        (RP_ESKID, index, nrem, nins);
  _nchan   ->preChange              (index, nrem, nins);
  _search  ->preChange              (index, nrem, nins);
}



//------------------- after remove insert --------------------------//
//------------------- after remove insert --------------------------//
//------------------- after remove insert --------------------------//

        //  private virtual function overriding SmartArray.
        //  Called from SmartArray after removing and/or inserting
        //    one or more array elements.
        //  The function in SmartArray is empty.


void RpCards::afterRemoveInsert(long index, long nrem, long nins)
{
  valuesHaveChanged        (RP_PAT  , index, nrem, nins);
  valuesHaveChanged        (RP_FLAG , index, nrem, nins);
  valuesHaveChanged        (RP_SHOT , index, nrem, nins);
  valuesHaveChanged        (RP_LINE , index, nrem, nins);
  valuesHaveChanged        (RP_NX   , index, nrem, nins);
  valuesHaveChanged        (RP_XINC , index, nrem, nins);
  valuesHaveChanged        (RP_NY   , index, nrem, nins);
  valuesHaveChanged        (RP_YINC , index, nrem, nins);
  valuesHaveChanged        (RP_XSKID, index, nrem, nins);
  valuesHaveChanged        (RP_YSKID, index, nrem, nins);
  valuesHaveChanged        (RP_ESKID, index, nrem, nins);
  _nchan   ->post1Change              ();
  _search  ->post1Change              ();
  _nchan   ->post2Change              ();
  _search  ->post2Change              ();
  _informer->postRemoveInsertRpCards (index, nrem, nins);
  _connect ->receiverPatternsHaveChanged();
}



//-------------------- before values changed ---------------------//
//-------------------- before values changed ---------------------//
//-------------------- before values changed ---------------------//

      //  private convenience function.
      //  Called by a public function before that function changes
      //    a value.

void RpCards::beforeValuesChanged(int ident, long index, long nchange)
{
  valuesWillChange (ident, index, nchange, nchange);
  if(ident == RP_PAT || ident == RP_FLAG || ident == RP_NX ||
     ident == RP_NY)
      {
      _nchan->preChange               (index, nchange, nchange);
      }
  if(ident == RP_PAT)
      {
      _search->preChange               (index, nchange, nchange);
      }
/***********
  switch(ident)   // alternative code
    {
    case RP_PAT    : _nchan ->preChange(index, nchange, nchange);
                     _search->preChange(index, nchange, nchange);
                     break;
    case RP_FLAG   :
    case RP_NX     :
    case RP_NY     : _nchan ->preChange(index, nchange, nchange);
                     break;
    default        : break;
    }
***********/
}



//-------------------- after values changed ---------------------//
//-------------------- after values changed ---------------------//
//-------------------- after values changed ---------------------//

      //  private convenience function.
      //  Called by a public function after that function changes
      //    a value.

void RpCards::afterValuesChanged(int ident, long index, long nchange)
{
  if(ident == RP_PAT || ident == RP_FLAG || ident == RP_NX ||
     ident == RP_NY)
      {
      _nchan->post1Change();
      }
  if(ident == RP_PAT)
      {
      _search->post1Change();
      }
  valuesHaveChanged (ident, index, nchange, nchange);
  if(ident == RP_PAT || ident == RP_FLAG || ident == RP_NX ||
     ident == RP_NY)
      {
      _nchan->post2Change();
      }
  if(ident == RP_PAT)
      {
      _search->post2Change();
      }
  _connect ->receiverPatternsHaveChanged();
}



//--------------- values will change (or have changed) -------------//
//--------------- values will change (or have changed) -------------//
//--------------- values will change (or have changed) -------------//

    // virtual functions overriding SmartArray.
    // called by AccBase objects and by functions in this class.

void RpCards::valuesWillChange
                  (int ident, long index, long nrem, long nins)
{
  _informer->preRpValuesChanged (ident, index, nrem, nins);
}

void RpCards::valuesHaveChanged
                  (int ident, long index, long nrem, long nins)
{
  _informer->postRpValuesChanged (ident, index, nrem, nins);
}




//------------ public access to these RP cards --------------//
//------------ public access to these RP cards --------------//
//------------ public access to these RP cards --------------//
//------------ public access to these RP cards --------------//
//------------ public access to these RP cards --------------//
//------------ public access to these RP cards --------------//
//------------ public access to these RP cards --------------//
//------------ public access to these RP cards --------------//
//------------ public access to these RP cards --------------//
//------------ public access to these RP cards --------------//
//------------ public access to these RP cards --------------//
//------------ public access to these RP cards --------------//
//------------ public access to these RP cards --------------//

        // index = index of desired RP card.


void RpCards::freezeDependentUpdates()
{
  if(_frozen) return;
  _frozen = TRUE;
  _nchan         ->freezeUpdates();
  _search        ->freezeUpdates();
}


void RpCards::resumeDependentUpdates()
{
  if(!_frozen) return;
  _frozen = FALSE;
  _nchan         ->resumeUpdates();
  _search        ->resumeUpdates();
}


void RpCards::performDependentUpdates()
{
  if(!_frozen) return;
  _frozen = FALSE;
  _nchan         ->performUpdates();
  _search        ->performUpdates();
}



//--------------- do create and delete object --------------------//
//--------------- do create and delete object --------------------//
//--------------- do create and delete object --------------------//

          // private virtual functions overriding SmartArray

void *RpCards::doCreateObject()
{
  return new RpCard();
}


void RpCards::doDeleteObject(void *object)
{
  delete (RpCard*)object;
}




//----------------------- insert or remove cards --------------------//
//----------------------- insert or remove cards --------------------//
//----------------------- insert or remove cards --------------------//

   // the non-void functions return index where action occurred.
   // the non-void functions return -1 if failed.
 

long RpCards::appendNewRpCard()
{
  return appendNullElement();
}


long RpCards::placeNewRpCard(long pattern)      // search
{
  long index = _search->findInsertionLocation((double)pattern);
  index = insertNewRpCard(index);
  if(index >= 0) setRpPatternNumber(index, pattern);
  return index;
}



long RpCards::insertNewRpCard(long index)
{
  return insertNullElement(index);
}


long RpCards::insertNewRpCardFromBuffer(long index)
{
  return insertElementFromBuffer(index);
}



long RpCards::deleteRpCard(long index)
{
  return removeElement(index);
}


long RpCards::deleteRpCardToBuffer(long index)
{
  return removeElementToBuffer(index);
}


void RpCards::deleteAllRpCards()
{
  removeAllElements();
}



//--------------------- find receiver pattern ----------------------//
//--------------------- find receiver pattern ----------------------//
//--------------------- find receiver pattern ----------------------//

         // these return an index, or -1 if not found.
         // returns index of first card with matching receiver pattern.

long RpCards::findReceiverPattern (long pattern) const
{
  return _search->findMatchingValue((float)pattern);
}


long RpCards::findEndOfReceiverPattern (long pattern) const
{
  long ixrp = findReceiverPattern(pattern);
  return getEndOfReceiverPattern(ixrp);
}


long RpCards::findRpCardWithDesiredChannel (long pattern, long channel) const
{
  long ixrp = findReceiverPattern(pattern);
  return getRpCardWithDesiredChannel(ixrp, channel);
}


   // ixrp MUST be the FIRST card in the pattern:

long RpCards::getEndOfReceiverPattern (long ixrp) const
{
  long n = numElements();
  if(ixrp < 0 || ixrp >= n) return -1;
  long pattern = rpCard(ixrp)->getRpPatternNumber();
  if(ixrp > 0)
      {
      long pattern2 = rpCard(ixrp-1)->getRpPatternNumber();
      assert(pattern2 != pattern);
      }
  while(ixrp < n-1 && rpCard(ixrp+1)->getRpPatternNumber() == pattern)
      {
      ixrp++;
      }
  return ixrp;
}


   // ixrp MUST be the FIRST card in the pattern:

long RpCards::getRpCardWithDesiredChannel (long ixrp, long channel) const
{
  if(channel <= 0) return -1;
  long n = numElements();
  if(ixrp < 0 || ixrp >= n) return -1;
  long pattern = rpCard(ixrp)->getRpPatternNumber();
  if(ixrp > 0)
      {
      long pattern2 = rpCard(ixrp-1)->getRpPatternNumber();
      assert(pattern2 != pattern);
      }
  while(ixrp < n && rpCard(ixrp)->getRpPatternNumber() == pattern)
      {
      long cumchannel = rpCard(ixrp)->getRpCumChannels();
      if(cumchannel >= channel) return ixrp;
      ixrp++;
      }
  return -1;
}



   // given first and last card of a pattern,
   // returns the card on which the specified channel
   // is described.

long RpCards::getRpCardWithDesiredChannel
                    (long ixrp_first, long ixrp_last, long channel) const
{
  for(long ixrp = ixrp_first; ixrp <= ixrp_last; ixrp++)
      {
      long cumchannel = rpCard(ixrp)->getRpCumChannels();
      if(cumchannel >= channel) return ixrp;
      }
  return -1;
}




   // given first and last card of a pattern,
   // returns first and last card corresponding to "irregular"
   // cards (i.e. those with SKIP or DUP flags), or -1 if there
   // are no "irregular" cards:

void RpCards::getRpIrregularRange(long ixrp_first, long ixrp_last,
                   long *ixrp_first_irreg, long *ixrp_last_irreg)  const
{
  *ixrp_first_irreg = -1;
  *ixrp_last_irreg  = -1;
  for(long ixrp = ixrp_first; ixrp <= ixrp_last; ixrp++)
      {
      int rpflag = rpCard(ixrp)->getRpFlag();
      if(rpflag != RP_FLAG_SKIP && rpflag != RP_FLAG_DUP) continue;
      if(*ixrp_first_irreg == -1) *ixrp_first_irreg = ixrp;
                                  *ixrp_last_irreg  = ixrp;
      }

}



//------------------- num channels in pattern ---------------------//
//------------------- num channels in pattern ---------------------//
//------------------- num channels in pattern ---------------------//

     // public.
     // given desired receiver pattern, returns the number of
     // channels (from all cards) of the receiver pattern.
     // returns 0 if receiver pattern is not found.

long RpCards::numChannelsInPattern(long pattern)  const
{
  long ixrp = findReceiverPattern(pattern);
  if(ixrp >= 0) return getRpNumChannels(ixrp);
  return 0;
}



//-------------------- receiver patterns are sorted ---------------//
//-------------------- receiver patterns are sorted ---------------//
//-------------------- receiver patterns are sorted ---------------//

int RpCards::receiverPatternsAreSorted()  const
{
  if(numElements() >= 2) return _search->isAscending();
  return _search->isSorted();
}



//------------------- sort receiver patterns ------------------------//
//------------------- sort receiver patterns ------------------------//
//------------------- sort receiver patterns ------------------------//

void RpCards::sortReceiverPatterns()
{
  long n = numElements();
  if(n <= 1) return;
  _informer->preSortReceiverPatterns();
  beforeRemoveInsert(0, n, n);
/*
  _sort->sort(RP_PAT, 1);                // 1 = desired direction
  _sort->sort(get_pattern_number, 1);        // 1 = desired direction
*/
  _sort->sort(1);                    // 1 = desired direction
  afterRemoveInsert(0, n, n);
  _informer->postSortReceiverPatterns();
}




//----------- pass-thru functions to individual RP cards --------------//
//----------- pass-thru functions to individual RP cards --------------//
//----------- pass-thru functions to individual RP cards --------------//
//----------- pass-thru functions to individual RP cards --------------//
//----------- pass-thru functions to individual RP cards --------------//
//----------- pass-thru functions to individual RP cards --------------//
//----------- pass-thru functions to individual RP cards --------------//
//----------- pass-thru functions to individual RP cards --------------//
//----------- pass-thru functions to individual RP cards --------------//
//----------- pass-thru functions to individual RP cards --------------//

     // index  = index of desired RP card.



//---------------------- get card values ---------------------------//
//---------------------- get card values ---------------------------//
//---------------------- get card values ---------------------------//

class RpCard *RpCards::getRpCardPointer(long ixrp)  const
{
  return rpCard(ixrp);
}



#define GETV(getXxxx, long2)                         \
long2 RpCards::getXxxx(long index) const             \
{                                                    \
  assert(index >= 0 && index < numElements());       \
  RpCard *card = rpCard(index);                      \
  return card->getXxxx();                            \
}


GETV(getRpPatternNumber, long )
GETV(getRpNumChannels  , long )
GETV(getRpCumChannels  , long )
GETV(getRpFlag         , int  )
GETV(getRpShotpoint    , float)
GETV(getRpLineNumber   , long )
GETV(getRpNumX         , long )
GETV(getRpXinc         , long )
GETV(getRpNumY         , long )
GETV(getRpYinc         , long )
GETV(getRpXskid        , float)
GETV(getRpYskid        , float)
GETV(getRpEskid        , float)
/*
GETV(getRpLineIndex    , long )
GETV(getRpFlagIndex    , long )
*/


void RpCards::getRpChannelsOnCard(long index,
                         long *first_channel, long *last_channel)  const
{
  assert(index >= 0 && index < numElements());
  RpCard *card = rpCard(index);
  card->getRpChannelsOnCard(first_channel, last_channel);
}

void RpCards::getRpIncrements(long index, long channel,
                              long *xinc,  long *yinc,
                              long *incrx, long *incry)  const
{
  assert(index >= 0 && index < numElements());
  RpCard *card = rpCard(index);
  card->getRpIncrements(channel, xinc, yinc, incrx, incry);
}



//------------------------ set card values -------------------------//
//------------------------ set card values -------------------------//
//------------------------ set card values -------------------------//


// the following function sets all consecutive matching pattern
//   numbers, beginning at the specified index, to the new value.

void RpCards::setRpPatternNumber(long index, long value)
{                                                    
  long n = numElements();
  assert(index >= 0 && index < n);
  RpCard *card = rpCard(index);
  long old_pattern = card->getRpPatternNumber();
  long ixrp;
  long test_pattern = old_pattern;
  long nchange = 0;
  for(ixrp = index; ixrp < n && test_pattern == old_pattern; ixrp++)
      {
      nchange++;
      if(ixrp+1 < n)
          {
          RpCard *card = rpCard(index);
          test_pattern = card->getRpPatternNumber();
          }
      }
  beforeValuesChanged(RP_PAT, index, nchange);
  for(ixrp = index; ixrp < index + nchange; ixrp++)
      {
      RpCard *card = rpCard(index);
      card->setRpPatternNumber(value);
      }
  afterValuesChanged(RP_PAT, index, nchange);
}



#define SETV(setXxxx, long2, ident)               \
void RpCards::setXxxx(long index, long2 value)    \
{                                                 \
  assert(index >= 0 && index < numElements());    \
  RpCard *card = rpCard(index);                   \
  beforeValuesChanged(ident, index, 1);           \
  card->setXxxx(value);                           \
  afterValuesChanged(ident, index, 1);            \
}


SETV(setRpFlag       , int  , RP_FLAG )
SETV(setRpShotpoint  , float, RP_SHOT )
SETV(setRpLineNumber , long , RP_LINE )
SETV(setRpNumX       , long , RP_NX   )
SETV(setRpXinc       , long , RP_XINC )
SETV(setRpNumY       , long , RP_NY   )
SETV(setRpYinc       , long , RP_YINC )
SETV(setRpXskid      , float, RP_XSKID)
SETV(setRpYskid      , float, RP_YSKID)
SETV(setRpEskid      , float, RP_ESKID)



//-------------------------- end -------------------------------//
//-------------------------- end -------------------------------//
//-------------------------- end -------------------------------//

