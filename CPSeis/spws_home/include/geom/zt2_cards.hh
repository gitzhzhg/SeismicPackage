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

//------------------------ zt2_cards.hh ---------------------//
//------------------------ zt2_cards.hh ---------------------//
//------------------------ zt2_cards.hh ---------------------//

//             header file for the Zt2Cards class
//             derived from the SmartArray class
//                    subdirectory geom

  // This class contains all ZT2 cards (zero receivers).

  // This class maintains an array of pointers to Zt2Card objects.

  // This object owns the ZT2 cards.  This means that this class
  // creates and deletes the ZT2 cards when they are inserted and
  // removed.  They are not to be created or deleted outside of
  // this class.
 

#ifndef _ZT2_CARDS_HH_
#define _ZT2_CARDS_HH_

#include "oprim/smart_array.hh"


class Zt2Cards  :  public SmartArray
{

//----------------------- data ------------------------------//
//----------------------- data ------------------------------//
//----------------------- data ------------------------------//

private:

  int   _frozen;           // whether dependent updates are frozen.

  class FgInformer    *_informer;  // informer class.
  class FgConnect     *_connect;   // connections class.

//--------------------- functions ---------------------------//
//--------------------- functions ---------------------------//
//--------------------- functions ---------------------------//

public:   // constructor and destructor

  Zt2Cards (FgInformer *informer, FgConnect *connect);
  virtual ~Zt2Cards();

  class Zt2Card *zt2Card (long ixzt2)  const
                          { return (Zt2Card*)arrayElement(ixzt2); }

public:    // virtual functions overriding SmartArray.

  virtual void valuesWillChange  (int ident, long index, long nrem, long nins);
  virtual void valuesHaveChanged (int ident, long index, long nrem, long nins);

private:    // virtual functions overriding SmartArray.

  virtual void  beforeRemoveInsert   (long index, long nrem, long nins);
  virtual void  afterRemoveInsert    (long index, long nrem, long nins);
  virtual void  beforeNewActiveIndex ();
  virtual void  afterNewActiveIndex  ();
  virtual void *doCreateObject       ();
  virtual void  doDeleteObject       (void *object);

private:

  void beforeValuesChanged (int ident, long index, long nchange);
  void afterValuesChanged  (int ident, long index, long nchange);

//---------------- public access to these ZT2 cards -------------//
//---------------- public access to these ZT2 cards -------------//
//---------------- public access to these ZT2 cards -------------//

        // ixzt2 = index of desired ZT2 card.

public:     // get values

  long  numZt2Cards           ()       const { return numElements(); }
  long  getActiveZt2CardIndex ()       const { return getActiveIndex(); }

public:     // set values

  void   setActiveZt2CardIndex   (long index) { setActiveIndex(index); }
  void   freezeDependentUpdates   ();
  void   resumeDependentUpdates   ();
  void   performDependentUpdates  ();

public:  // insert or remove cards.
         // the non-void functions return index (ixzt2) where action occurred.
         // the non-void functions return -1 if failed.

  long   appendNewZt2Card           ();
  long   insertNewZt2Card           (long ixzt2);
  long   insertNewZt2CardFromBuffer (long ixzt2);
  long   deleteZt2Card              (long ixzt2);
  long   deleteZt2CardToBuffer      (long ixzt2);
  void   deleteAllZt2Cards          ();

public:    // allocate and free space for ZT2 cards (optional usage).

  void  allocateSpaceForZt2Cards (long nadd) { allocateSpace(nadd); }
  void  freeSpaceForZt2Cards     ()          { freeSpace    (); }


//----------- pass-thru functions to individual ZT2 cards -------------//
//----------- pass-thru functions to individual ZT2 cards -------------//
//----------- pass-thru functions to individual ZT2 cards -------------//

     // ixzt2 = index of desired ZT2 card.

public:      // get card values

  int    getZt2Code                  (long ixzt2)  const;
  float  getZt2FromReceiverShotpoint (long ixzt2)  const;
  float  getZt2ToReceiverShotpoint   (long ixzt2)  const;
  long   getZt2ReceiverLineNumber    (long ixzt2)  const;

public:      // set card values

  void   setZt2Code                  (long ixzt2, int   value);
  void   setZt2FromReceiverShotpoint (long ixzt2, float value);
  void   setZt2ToReceiverShotpoint   (long ixzt2, float value);
  void   setZt2ReceiverLineNumber    (long ixzt2, long  value);


//---------------------- end of functions -----------------------//
//---------------------- end of functions -----------------------//
//---------------------- end of functions -----------------------//

} ;

#endif

//---------------------------- end --------------------------------//
//---------------------------- end --------------------------------//
//---------------------------- end --------------------------------//
