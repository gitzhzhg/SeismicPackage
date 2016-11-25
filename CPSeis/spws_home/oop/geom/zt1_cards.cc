
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
//---------------------- zt1_cards.cc -----------------------//
//---------------------- zt1_cards.cc -----------------------//
//---------------------- zt1_cards.cc -----------------------//

//            implementation file for the Zt1Cards class
//                 derived from the SmartArray class
//                        subdirectory geom


#include "geom/zt1_cards.hh"
#include "geom/zt1_card.hh"
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


Zt1Cards::Zt1Cards(FgInformer *informer, FgConnect *connect)
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

Zt1Cards::~Zt1Cards()
{
  deleteAllZt1Cards();
}



//------------- before and after new active index -----------------//
//------------- before and after new active index -----------------//
//------------- before and after new active index -----------------//

        //  private virtual functions overriding SmartArray.
        //  Called from SmartArray before and after changing
        //    the active index.
        //  The function in SmartArray is empty.


void Zt1Cards::beforeNewActiveIndex()
{
  _informer->preNewActiveZt1Card();
}


void Zt1Cards::afterNewActiveIndex()
{
  _informer->postNewActiveZt1Card();
}



//------------------- before remove insert --------------------------//
//------------------- before remove insert --------------------------//
//------------------- before remove insert --------------------------//

        //  private virtual function overriding SmartArray.
        //  Called from SmartArray before removing and/or inserting
        //    one or more array elements.
        //  The function in SmartArray is empty.


void Zt1Cards::beforeRemoveInsert(long index, long nrem, long nins)
{
  _informer->preRemoveInsertZt1Cards (index, nrem, nins);
  valuesWillChange        (ZT1_CODE , index, nrem, nins);
  valuesWillChange        (ZT1_SHOT1, index, nrem, nins);
  valuesWillChange        (ZT1_SHOT2, index, nrem, nins);
  valuesWillChange        (ZT1_LINE , index, nrem, nins);
}



//------------------- after remove insert --------------------------//
//------------------- after remove insert --------------------------//
//------------------- after remove insert --------------------------//

        //  private virtual function overriding SmartArray.
        //  Called from SmartArray after removing and/or inserting
        //    one or more array elements.
        //  The function in SmartArray is empty.


void Zt1Cards::afterRemoveInsert(long index, long nrem, long nins)
{
  valuesHaveChanged       (ZT1_CODE , index, nrem, nins);
  valuesHaveChanged       (ZT1_SHOT1, index, nrem, nins);
  valuesHaveChanged       (ZT1_SHOT2, index, nrem, nins);
  valuesHaveChanged       (ZT1_LINE , index, nrem, nins);
  _informer->postRemoveInsertZt1Cards (index, nrem, nins);
  _connect ->zt1CardsHaveChanged();
}



//-------------------- before values changed ---------------------//
//-------------------- before values changed ---------------------//
//-------------------- before values changed ---------------------//

      //  private convenience function.
      //  Called by a public function before that function changes
      //    a value.

void Zt1Cards::beforeValuesChanged(int ident, long index, long nchange)
{
  valuesWillChange (ident, index, nchange, nchange);
}



//-------------------- after values changed ---------------------//
//-------------------- after values changed ---------------------//
//-------------------- after values changed ---------------------//

      //  private convenience function.
      //  Called by a public function after that function changes
      //    a value.

void Zt1Cards::afterValuesChanged(int ident, long index, long nchange)
{
  valuesHaveChanged (ident, index, nchange, nchange);
  _connect ->zt1CardsHaveChanged();
}



//--------------- values will change (or have changed) -------------//
//--------------- values will change (or have changed) -------------//
//--------------- values will change (or have changed) -------------//

    // virtual functions overriding SmartArray.
    // called by Acc... objects etc.

void Zt1Cards::valuesWillChange
                  (int ident, long index, long nrem, long nins)
{
  _informer->preZt1ValuesChanged (ident, index, nrem, nins);
}

void Zt1Cards::valuesHaveChanged
                  (int ident, long index, long nrem, long nins)
{
  _informer->postZt1ValuesChanged (ident, index, nrem, nins);
}



//--------------- do create and delete object --------------------//
//--------------- do create and delete object --------------------//
//--------------- do create and delete object --------------------//

          // private virtual functions overriding SmartArray

void *Zt1Cards::doCreateObject()
{
  return new Zt1Card();
}


void Zt1Cards::doDeleteObject(void *object)
{
  delete (Zt1Card*)object;
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


void Zt1Cards::freezeDependentUpdates()
{
  if(_frozen) return;
  _frozen = TRUE;
}


void Zt1Cards::resumeDependentUpdates()
{
  if(!_frozen) return;
  _frozen = FALSE;
}


void Zt1Cards::performDependentUpdates()
{
  if(!_frozen) return;
  _frozen = FALSE;
}



//----------------------- insert or remove cards --------------------//
//----------------------- insert or remove cards --------------------//
//----------------------- insert or remove cards --------------------//

   // the non-void functions return index where action occurred.
   // the non-void functions return -1 if failed.
 

long Zt1Cards::appendNewZt1Card()
{
  return appendNullElement();
}


long Zt1Cards::insertNewZt1Card(long index)
{
  return insertNullElement(index);
}


long Zt1Cards::insertNewZt1CardFromBuffer(long index)
{
  return insertElementFromBuffer(index);
}



long Zt1Cards::deleteZt1Card(long index)
{
  return removeElement(index);
}


long Zt1Cards::deleteZt1CardToBuffer(long index)
{
  return removeElementToBuffer(index);
}


void Zt1Cards::deleteAllZt1Cards()
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
long2 Zt1Cards::getXxxx(long index) const            \
{                                                    \
  assert(index >= 0 && index < numElements());       \
  Zt1Card *card = zt1Card(index);                    \
  return card->getXxxx();                            \
}


GETV(getZt1Code               , int  )
GETV(getZt1FromSourceShotpoint, float)
GETV(getZt1ToSourceShotpoint  , float)
GETV(getZt1SourceLineNumber   , long )



//------------------------ set card values -------------------------//
//------------------------ set card values -------------------------//
//------------------------ set card values -------------------------//


#define SETV(setXxxx, long2, ident)               \
void Zt1Cards::setXxxx(long index, long2 value)   \
{                                                 \
  assert(index >= 0 && index < numElements());    \
  Zt1Card *card = zt1Card(index);                 \
  beforeValuesChanged(ident, index, 1);           \
  card->setXxxx(value);                           \
  afterValuesChanged(ident, index, 1);            \
}


SETV(setZt1Code               , int  , ZT1_CODE )
SETV(setZt1FromSourceShotpoint, float, ZT1_SHOT1)
SETV(setZt1ToSourceShotpoint  , float, ZT1_SHOT2)
SETV(setZt1SourceLineNumber   , long , ZT1_LINE )



//-------------------------- end -------------------------------//
//-------------------------- end -------------------------------//
//-------------------------- end -------------------------------//

