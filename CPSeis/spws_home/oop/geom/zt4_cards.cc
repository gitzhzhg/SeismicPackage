
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
//---------------------- zt4_cards.cc -----------------------//
//---------------------- zt4_cards.cc -----------------------//
//---------------------- zt4_cards.cc -----------------------//

//            implementation file for the Zt4Cards class
//                 derived from the SmartArray class
//                        subdirectory geom


#include "geom/zt4_cards.hh"
#include "geom/zt4_card.hh"
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


Zt4Cards::Zt4Cards(FgInformer *informer, FgConnect *connect)
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

Zt4Cards::~Zt4Cards()
{
  deleteAllZt4Cards();
}



//------------- before and after new active index -----------------//
//------------- before and after new active index -----------------//
//------------- before and after new active index -----------------//

        //  private virtual functions overriding SmartArray.
        //  Called from SmartArray before and after changing
        //    the active index.
        //  The function in SmartArray is empty.


void Zt4Cards::beforeNewActiveIndex()
{
  _informer->preNewActiveZt4Card();
}


void Zt4Cards::afterNewActiveIndex()
{
  _informer->postNewActiveZt4Card();
}



//------------------- before remove insert --------------------------//
//------------------- before remove insert --------------------------//
//------------------- before remove insert --------------------------//

        //  private virtual function overriding SmartArray.
        //  Called from SmartArray before removing and/or inserting
        //    one or more array elements.
        //  The function in SmartArray is empty.


void Zt4Cards::beforeRemoveInsert(long index, long nrem, long nins)
{
  _informer->preRemoveInsertZt4Cards (index, nrem, nins);
  valuesWillChange        (ZT4_CODE  , index, nrem, nins);
  valuesWillChange        (ZT4_SSHOT1, index, nrem, nins);
  valuesWillChange        (ZT4_SSHOT2, index, nrem, nins);
  valuesWillChange        (ZT4_SLINE , index, nrem, nins);
  valuesWillChange        (ZT4_RSHOT1, index, nrem, nins);
  valuesWillChange        (ZT4_RSHOT2, index, nrem, nins);
  valuesWillChange        (ZT4_RLINE , index, nrem, nins);
}



//------------------- after remove insert --------------------------//
//------------------- after remove insert --------------------------//
//------------------- after remove insert --------------------------//

        //  private virtual function overriding SmartArray.
        //  Called from SmartArray after removing and/or inserting
        //    one or more array elements.
        //  The function in SmartArray is empty.


void Zt4Cards::afterRemoveInsert(long index, long nrem, long nins)
{
  valuesHaveChanged       (ZT4_CODE  , index, nrem, nins);
  valuesHaveChanged       (ZT4_SSHOT1, index, nrem, nins);
  valuesHaveChanged       (ZT4_SSHOT2, index, nrem, nins);
  valuesHaveChanged       (ZT4_SLINE , index, nrem, nins);
  valuesHaveChanged       (ZT4_RSHOT1, index, nrem, nins);
  valuesHaveChanged       (ZT4_RSHOT2, index, nrem, nins);
  valuesHaveChanged       (ZT4_RLINE , index, nrem, nins);
  _informer->postRemoveInsertZt4Cards (index, nrem, nins);
  _connect ->zt4CardsHaveChanged();
}



//-------------------- before values changed ---------------------//
//-------------------- before values changed ---------------------//
//-------------------- before values changed ---------------------//

      //  private convenience function.
      //  Called by a public function before that function changes
      //    a value.

void Zt4Cards::beforeValuesChanged(int ident, long index, long nchange)
{
  valuesWillChange (ident, index, nchange, nchange);
}



//-------------------- after values changed ---------------------//
//-------------------- after values changed ---------------------//
//-------------------- after values changed ---------------------//

      //  private convenience function.
      //  Called by a public function after that function changes
      //    a value.

void Zt4Cards::afterValuesChanged(int ident, long index, long nchange)
{
  valuesHaveChanged (ident, index, nchange, nchange);
  _connect ->zt4CardsHaveChanged();
}



//--------------- values will change (or have changed) -------------//
//--------------- values will change (or have changed) -------------//
//--------------- values will change (or have changed) -------------//

    // virtual functions overriding SmartArray.
    // called by Acc... objects etc.

void Zt4Cards::valuesWillChange
                  (int ident, long index, long nrem, long nins)
{
  _informer->preZt4ValuesChanged (ident, index, nrem, nins);
}

void Zt4Cards::valuesHaveChanged
                  (int ident, long index, long nrem, long nins)
{
  _informer->postZt4ValuesChanged (ident, index, nrem, nins);
}



//--------------- do create and delete object --------------------//
//--------------- do create and delete object --------------------//
//--------------- do create and delete object --------------------//

          // private virtual functions overriding SmartArray

void *Zt4Cards::doCreateObject()
{
  return new Zt4Card();
}


void Zt4Cards::doDeleteObject(void *object)
{
  delete (Zt4Card*)object;
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


void Zt4Cards::freezeDependentUpdates()
{
  if(_frozen) return;
  _frozen = TRUE;
}


void Zt4Cards::resumeDependentUpdates()
{
  if(!_frozen) return;
  _frozen = FALSE;
}


void Zt4Cards::performDependentUpdates()
{
  if(!_frozen) return;
  _frozen = FALSE;
}



//----------------------- insert or remove cards --------------------//
//----------------------- insert or remove cards --------------------//
//----------------------- insert or remove cards --------------------//

   // the non-void functions return index where action occurred.
   // the non-void functions return -1 if failed.
 

long Zt4Cards::appendNewZt4Card()
{
  return appendNullElement();
}


long Zt4Cards::insertNewZt4Card(long index)
{
  return insertNullElement(index);
}


long Zt4Cards::insertNewZt4CardFromBuffer(long index)
{
  return insertElementFromBuffer(index);
}



long Zt4Cards::deleteZt4Card(long index)
{
  return removeElement(index);
}


long Zt4Cards::deleteZt4CardToBuffer(long index)
{
  return removeElementToBuffer(index);
}


void Zt4Cards::deleteAllZt4Cards()
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
long2 Zt4Cards::getXxxx(long index) const            \
{                                                    \
  assert(index >= 0 && index < numElements());       \
  Zt4Card *card = zt4Card(index);                    \
  return card->getXxxx();                            \
}


GETV(getZt4Code                 , int  )
GETV(getZt4FromSourceShotpoint  , float)
GETV(getZt4ToSourceShotpoint    , float)
GETV(getZt4SourceLineNumber     , long )
GETV(getZt4FromReceiverShotpoint, float)
GETV(getZt4ToReceiverShotpoint  , float)
GETV(getZt4ReceiverLineNumber   , long )



//------------------------ set card values -------------------------//
//------------------------ set card values -------------------------//
//------------------------ set card values -------------------------//


#define SETV(setXxxx, long2, ident)               \
void Zt4Cards::setXxxx(long index, long2 value)   \
{                                                 \
  assert(index >= 0 && index < numElements());    \
  Zt4Card *card = zt4Card(index);                 \
  beforeValuesChanged(ident, index, 1);           \
  card->setXxxx(value);                           \
  afterValuesChanged(ident, index, 1);            \
}


SETV(setZt4Code                 , int  , ZT4_CODE  )
SETV(setZt4FromSourceShotpoint  , float, ZT4_SSHOT1)
SETV(setZt4ToSourceShotpoint    , float, ZT4_SSHOT2)
SETV(setZt4SourceLineNumber     , long , ZT4_SLINE )
SETV(setZt4FromReceiverShotpoint, float, ZT4_RSHOT1)
SETV(setZt4ToReceiverShotpoint  , float, ZT4_RSHOT2)
SETV(setZt4ReceiverLineNumber   , long , ZT4_RLINE )



//-------------------------- end -------------------------------//
//-------------------------- end -------------------------------//
//-------------------------- end -------------------------------//

