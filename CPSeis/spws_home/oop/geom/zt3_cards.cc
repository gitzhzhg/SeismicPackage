
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
//---------------------- zt3_cards.cc -----------------------//
//---------------------- zt3_cards.cc -----------------------//
//---------------------- zt3_cards.cc -----------------------//

//            implementation file for the Zt3Cards class
//                 derived from the SmartArray class
//                        subdirectory geom


#include "geom/zt3_cards.hh"
#include "geom/zt3_card.hh"
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


Zt3Cards::Zt3Cards(FgInformer *informer, FgConnect *connect)
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

Zt3Cards::~Zt3Cards()
{
  deleteAllZt3Cards();
}



//------------- before and after new active index -----------------//
//------------- before and after new active index -----------------//
//------------- before and after new active index -----------------//

        //  private virtual functions overriding SmartArray.
        //  Called from SmartArray before and after changing
        //    the active index.
        //  The function in SmartArray is empty.


void Zt3Cards::beforeNewActiveIndex()
{
  _informer->preNewActiveZt3Card();
}


void Zt3Cards::afterNewActiveIndex()
{
  _informer->postNewActiveZt3Card();
}



//------------------- before remove insert --------------------------//
//------------------- before remove insert --------------------------//
//------------------- before remove insert --------------------------//

        //  private virtual function overriding SmartArray.
        //  Called from SmartArray before removing and/or inserting
        //    one or more array elements.
        //  The function in SmartArray is empty.


void Zt3Cards::beforeRemoveInsert(long index, long nrem, long nins)
{
  _informer->preRemoveInsertZt3Cards (index, nrem, nins);
  valuesWillChange       (ZT3_CODE  , index, nrem, nins);
  valuesWillChange       (ZT3_GROUP1, index, nrem, nins);
  valuesWillChange       (ZT3_GROUP2, index, nrem, nins);
  valuesWillChange       (ZT3_TRACE1, index, nrem, nins);
  valuesWillChange       (ZT3_TRACE2, index, nrem, nins);
}



//------------------- after remove insert --------------------------//
//------------------- after remove insert --------------------------//
//------------------- after remove insert --------------------------//

        //  private virtual function overriding SmartArray.
        //  Called from SmartArray after removing and/or inserting
        //    one or more array elements.
        //  The function in SmartArray is empty.


void Zt3Cards::afterRemoveInsert(long index, long nrem, long nins)
{
  valuesHaveChanged      (ZT3_CODE  , index, nrem, nins);
  valuesHaveChanged      (ZT3_GROUP1, index, nrem, nins);
  valuesHaveChanged      (ZT3_GROUP2, index, nrem, nins);
  valuesHaveChanged      (ZT3_TRACE1, index, nrem, nins);
  valuesHaveChanged      (ZT3_TRACE2, index, nrem, nins);
  _informer->postRemoveInsertZt3Cards (index, nrem, nins);
  _connect ->zt3CardsHaveChanged();
}



//-------------------- before values changed ---------------------//
//-------------------- before values changed ---------------------//
//-------------------- before values changed ---------------------//

      //  private convenience function.
      //  Called by a public function before that function changes
      //    a value.

void Zt3Cards::beforeValuesChanged(int ident, long index, long nchange)
{
  valuesWillChange (ident, index, nchange, nchange);
}



//-------------------- after values changed ---------------------//
//-------------------- after values changed ---------------------//
//-------------------- after values changed ---------------------//

      //  private convenience function.
      //  Called by a public function after that function changes
      //    a value.

void Zt3Cards::afterValuesChanged(int ident, long index, long nchange)
{
  valuesHaveChanged (ident, index, nchange, nchange);
  _connect ->zt3CardsHaveChanged();
}



//--------------- values will change (or have changed) -------------//
//--------------- values will change (or have changed) -------------//
//--------------- values will change (or have changed) -------------//

    // virtual functions overriding SmartArray.
    // called by Acc... objects etc.

void Zt3Cards::valuesWillChange
                  (int ident, long index, long nrem, long nins)
{
  _informer->preZt3ValuesChanged (ident, index, nrem, nins);
}

void Zt3Cards::valuesHaveChanged
                  (int ident, long index, long nrem, long nins)
{
  _informer->postZt3ValuesChanged (ident, index, nrem, nins);
}



//--------------- do create and delete object --------------------//
//--------------- do create and delete object --------------------//
//--------------- do create and delete object --------------------//

          // private virtual functions overriding SmartArray

void *Zt3Cards::doCreateObject()
{
  return new Zt3Card();
}


void Zt3Cards::doDeleteObject(void *object)
{
  delete (Zt3Card*)object;
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


void Zt3Cards::freezeDependentUpdates()
{
  if(_frozen) return;
  _frozen = TRUE;
}


void Zt3Cards::resumeDependentUpdates()
{
  if(!_frozen) return;
  _frozen = FALSE;
}


void Zt3Cards::performDependentUpdates()
{
  if(!_frozen) return;
  _frozen = FALSE;
}



//----------------------- insert or remove cards --------------------//
//----------------------- insert or remove cards --------------------//
//----------------------- insert or remove cards --------------------//

   // the non-void functions return index where action occurred.
   // the non-void functions return -1 if failed.
 

long Zt3Cards::appendNewZt3Card()
{
  return appendNullElement();
}


long Zt3Cards::insertNewZt3Card(long index)
{
  return insertNullElement(index);
}


long Zt3Cards::insertNewZt3CardFromBuffer(long index)
{
  return insertElementFromBuffer(index);
}



long Zt3Cards::deleteZt3Card(long index)
{
  return removeElement(index);
}


long Zt3Cards::deleteZt3CardToBuffer(long index)
{
  return removeElementToBuffer(index);
}


void Zt3Cards::deleteAllZt3Cards()
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
long2 Zt3Cards::getXxxx(long index) const            \
{                                                    \
  assert(index >= 0 && index < numElements());       \
  Zt3Card *card = zt3Card(index);                    \
  return card->getXxxx();                            \
}


GETV(getZt3Code           , int )
GETV(getZt3FromGroupNumber, long)
GETV(getZt3ToGroupNumber  , long)
GETV(getZt3FromTraceNumber, long)
GETV(getZt3ToTraceNumber  , long)



//------------------------ set card values -------------------------//
//------------------------ set card values -------------------------//
//------------------------ set card values -------------------------//


#define SETV(setXxxx, long2, ident)               \
void Zt3Cards::setXxxx(long index, long2 value)   \
{                                                 \
  assert(index >= 0 && index < numElements());    \
  Zt3Card *card = zt3Card(index);                 \
  beforeValuesChanged(ident, index, 1);           \
  card->setXxxx(value);                           \
  afterValuesChanged(ident, index, 1);            \
}


SETV(setZt3Code           , int , ZT3_CODE )
SETV(setZt3FromGroupNumber, long, ZT3_GROUP1)
SETV(setZt3ToGroupNumber  , long, ZT3_GROUP2)
SETV(setZt3FromTraceNumber, long, ZT3_TRACE1)
SETV(setZt3ToTraceNumber  , long, ZT3_TRACE2)



//-------------------------- end -------------------------------//
//-------------------------- end -------------------------------//
//-------------------------- end -------------------------------//

