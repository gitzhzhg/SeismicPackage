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

//------------------------ zt1_cards.hh ---------------------//
//------------------------ zt1_cards.hh ---------------------//
//------------------------ zt1_cards.hh ---------------------//

//             header file for the Zt1Cards class
//             derived from the SmartArray class
//                    subdirectory geom

  // This class contains all ZT1 cards (zero sources).

  // This class maintains an array of pointers to Zt1Card objects.

  // This object owns the ZT1 cards.  This means that this class
  // creates and deletes the ZT1 cards when they are inserted and
  // removed.  They are not to be created or deleted outside of
  // this class.
 

#ifndef _ZT1_CARDS_HH_
#define _ZT1_CARDS_HH_

#include "oprim/smart_array.hh"


class Zt1Cards  :  public SmartArray
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

  Zt1Cards (FgInformer *informer, FgConnect *connect);
  virtual ~Zt1Cards();

  class Zt1Card *zt1Card (long ixzt1)  const
                          { return (Zt1Card*)arrayElement(ixzt1); }

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

//---------------- public access to these ZT1 cards -------------//
//---------------- public access to these ZT1 cards -------------//
//---------------- public access to these ZT1 cards -------------//

        // ixzt1 = index of desired ZT1 card.

public:     // get values

  long  numZt1Cards           ()       const { return numElements(); }
  long  getActiveZt1CardIndex ()       const { return getActiveIndex(); }

public:     // set values

  void   setActiveZt1CardIndex   (long index) { setActiveIndex(index); }
  void   freezeDependentUpdates   ();
  void   resumeDependentUpdates   ();
  void   performDependentUpdates  ();

public:  // insert or remove cards.
         // the non-void functions return index (ixzt1) where action occurred.
         // the non-void functions return -1 if failed.

  long   appendNewZt1Card           ();
  long   insertNewZt1Card           (long ixzt1);
  long   insertNewZt1CardFromBuffer (long ixzt1);
  long   deleteZt1Card              (long ixzt1);
  long   deleteZt1CardToBuffer      (long ixzt1);
  void   deleteAllZt1Cards          ();

public:    // allocate and free space for ZT1 cards (optional usage).

  void  allocateSpaceForZt1Cards (long nadd) { allocateSpace(nadd); }
  void  freeSpaceForZt1Cards     ()          { freeSpace    (); }


//----------- pass-thru functions to individual ZT1 cards -------------//
//----------- pass-thru functions to individual ZT1 cards -------------//
//----------- pass-thru functions to individual ZT1 cards -------------//

     // ixzt1 = index of desired ZT1 card.

public:      // get card values

  int    getZt1Code                (long ixzt1)  const;
  float  getZt1FromSourceShotpoint (long ixzt1)  const;
  float  getZt1ToSourceShotpoint   (long ixzt1)  const;
  long   getZt1SourceLineNumber    (long ixzt1)  const;

public:      // set card values

  void   setZt1Code                (long ixzt1, int   value);
  void   setZt1FromSourceShotpoint (long ixzt1, float value);
  void   setZt1ToSourceShotpoint   (long ixzt1, float value);
  void   setZt1SourceLineNumber    (long ixzt1, long  value);


//---------------------- end of functions -----------------------//
//---------------------- end of functions -----------------------//
//---------------------- end of functions -----------------------//

} ;

#endif

//---------------------------- end --------------------------------//
//---------------------------- end --------------------------------//
//---------------------------- end --------------------------------//
