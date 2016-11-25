
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
//---------------------- zt2_cards.cc -----------------------//
//---------------------- zt2_cards.cc -----------------------//
//---------------------- zt2_cards.cc -----------------------//

//            implementation file for the Zt2Cards class
//                 derived from the SmartArray class
//                        subdirectory geom


#include "geom/zt2_cards.hh"
#include "geom/zt2_card.hh"
#include "geom/fg_informer.hh"
#include "geom/fg_connect.hh"
#include "geom/fg_constants.hh"
#include "cprim.h"
#include <iostream.h>
#include <stdlib.h>
#include <string.h>
#include <math.h>
#include <assert.h>


#define STEP      500


//---------------------- constructor -----------------------//
//---------------------- constructor -----------------------//
//---------------------- constructor -----------------------//


Zt2Cards::Zt2Cards(FgInformer *informer, FgConnect *connect)
           :  SmartArray(STEP),
                _frozen              (FALSE),
                _informer            (informer),
                _connect             (connect)
{
  assert(_informer);
  assert(_connect);
}



//------------------------ destructor --------------------------//
//------------------------ destructor --------------------------//
//------------------------ destructor --------------------------//

Zt2Cards::~Zt2Cards()
{
  deleteAllZt2Cards();
}



//------------- before and after new active index -----------------//
//------------- before and after new active index -----------------//
//------------- before and after new active index -----------------//

        //  private virtual functions overriding SmartArray.
        //  Called from SmartArray before and after changing
        //    the active index.
        //  The function in SmartArray is empty.


void Zt2Cards::beforeNewActiveIndex()
{
  _informer->preNewActiveZt2Card();
}


void Zt2Cards::afterNewActiveIndex()
{
  _informer->postNewActiveZt2Card();
}



//------------------- before remove insert --------------------------//
//------------------- before remove insert --------------------------//
//------------------- before remove insert --------------------------//

        //  private virtual function overriding SmartArray.
        //  Called from SmartArray before removing and/or inserting
        //    one or more array elements.
        //  The function in SmartArray is empty.


void Zt2Cards::beforeRemoveInsert(long index, long nrem, long nins)
{
  _informer->preRemoveInsertZt2Cards (index, nrem, nins);
  valuesWillChange        (ZT2_CODE , index, nrem, nins);
  valuesWillChange        (ZT2_SHOT1, index, nrem, nins);
  valuesWillChange        (ZT2_SHOT2, index, nrem, nins);
  valuesWillChange        (ZT2_LINE , index, nrem, nins);
}



//------------------- after remove insert --------------------------//
//------------------- after remove insert --------------------------//
//------------------- after remove insert --------------------------//

        //  private virtual function overriding SmartArray.
        //  Called from SmartArray after removing and/or inserting
        //    one or more array elements.
        //  The function in SmartArray is empty.


void Zt2Cards::afterRemoveInsert(long index, long nrem, long nins)
{
  valuesHaveChanged       (ZT2_CODE , index, nrem, nins);
  valuesHaveChanged       (ZT2_SHOT1, index, nrem, nins);
  valuesHaveChanged       (ZT2_SHOT2, index, nrem, nins);
  valuesHaveChanged       (ZT2_LINE , index, nrem, nins);
  _informer->postRemoveInsertZt2Cards (index, nrem, nins);
  _connect ->zt2CardsHaveChanged();
}



//-------------------- before values changed ---------------------//
//-------------------- before values changed ---------------------//
//-------------------- before values changed ---------------------//

      //  private convenience function.
      //  Called by a public function before that function changes
      //    a value.

void Zt2Cards::beforeValuesChanged(int ident, long index, long nchange)
{
  valuesWillChange (ident, index, nchange, nchange);
}



//-------------------- after values changed ---------------------//
//-------------------- after values changed ---------------------//
//-------------------- after values changed ---------------------//

      //  private convenience function.
      //  Called by a public function after that function changes
      //    a value.

void Zt2Cards::afterValuesChanged(int ident, long index, long nchange)
{
  valuesHaveChanged (ident, index, nchange, nchange);
  _connect ->zt2CardsHaveChanged();
}



//--------------- values will change (or have changed) -------------//
//--------------- values will change (or have changed) -------------//
//--------------- values will change (or have changed) -------------//

    // virtual functions overriding SmartArray.
    // called by Acc... objects etc.

void Zt2Cards::valuesWillChange
                  (int ident, long index, long nrem, long nins)
{
  _informer->preZt2ValuesChanged (ident, index, nrem, nins);
}

void Zt2Cards::valuesHaveChanged
                  (int ident, long index, long nrem, long nins)
{
  _informer->postZt2ValuesChanged (ident, index, nrem, nins);
}



//--------------- do create and delete object --------------------//
//--------------- do create and delete object --------------------//
//--------------- do create and delete object --------------------//

          // private virtual functions overriding SmartArray

void *Zt2Cards::doCreateObject()
{
  return new Zt2Card();
}


void Zt2Cards::doDeleteObject(void *object)
{
  delete (Zt2Card*)object;
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


//----------------------- set values ------------------------//
//----------------------- set values ------------------------//
//----------------------- set values ------------------------//


void Zt2Cards::freezeDependentUpdates()
{
  if(_frozen) return;
  _frozen = TRUE;
}


void Zt2Cards::resumeDependentUpdates()
{
  if(!_frozen) return;
  _frozen = FALSE;
}


void Zt2Cards::performDependentUpdates()
{
  if(!_frozen) return;
  _frozen = FALSE;
}



//----------------------- insert or remove cards --------------------//
//----------------------- insert or remove cards --------------------//
//----------------------- insert or remove cards --------------------//

   // the non-void functions return index where action occurred.
   // the non-void functions return -1 if failed.
 

long Zt2Cards::appendNewZt2Card()
{
  return appendNullElement();
}


long Zt2Cards::insertNewZt2Card(long index)
{
  return insertNullElement(index);
}


long Zt2Cards::insertNewZt2CardFromBuffer(long index)
{
  return insertElementFromBuffer(index);
}



long Zt2Cards::deleteZt2Card(long index)
{
  return removeElement(index);
}


long Zt2Cards::deleteZt2CardToBuffer(long index)
{
  return removeElementToBuffer(index);
}


void Zt2Cards::deleteAllZt2Cards()
{
  removeAllElements();
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


#define GETV(getXxxx, long2)                         \
long2 Zt2Cards::getXxxx(long index) const            \
{                                                    \
  assert(index >= 0 && index < numElements());       \
  Zt2Card *card = zt2Card(index);                    \
  return card->getXxxx();                            \
}


GETV(getZt2Code                 , int  )
GETV(getZt2FromReceiverShotpoint, float)
GETV(getZt2ToReceiverShotpoint  , float)
GETV(getZt2ReceiverLineNumber   , long )



//------------------------ set card values -------------------------//
//------------------------ set card values -------------------------//
//------------------------ set card values -------------------------//


#define SETV(setXxxx, long2, ident)               \
void Zt2Cards::setXxxx(long index, long2 value)   \
{                                                 \
  assert(index >= 0 && index < numElements());    \
  Zt2Card *card = zt2Card(index);                 \
  beforeValuesChanged(ident, index, 1);           \
  card->setXxxx(value);                           \
  afterValuesChanged(ident, index, 1);            \
}


SETV(setZt2Code                 , int  , ZT2_CODE )
SETV(setZt2FromReceiverShotpoint, float, ZT2_SHOT1)
SETV(setZt2ToReceiverShotpoint  , float, ZT2_SHOT2)
SETV(setZt2ReceiverLineNumber   , long , ZT2_LINE )



//-------------------------- end -------------------------------//
//-------------------------- end -------------------------------//
//-------------------------- end -------------------------------//

