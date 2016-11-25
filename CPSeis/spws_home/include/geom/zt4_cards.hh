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

//------------------------ zt4_cards.hh ---------------------//
//------------------------ zt4_cards.hh ---------------------//
//------------------------ zt4_cards.hh ---------------------//

//             header file for the Zt4Cards class
//             derived from the SmartArray class
//                    subdirectory geom

  // This class contains all ZT4 cards (zero receivers).

  // This class maintains an array of pointers to Zt4Card objects.

  // This object owns the ZT4 cards.  This means that this class
  // creates and deletes the ZT4 cards when they are inserted and
  // removed.  They are not to be created or deleted outside of
  // this class.
 

#ifndef _ZT4_CARDS_HH_
#define _ZT4_CARDS_HH_

#include "oprim/smart_array.hh"


class Zt4Cards  :  public SmartArray
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

  Zt4Cards (FgInformer *informer, FgConnect *connect);
  virtual ~Zt4Cards();

  class Zt4Card *zt4Card (long ixzt4)  const
                          { return (Zt4Card*)arrayElement(ixzt4); }

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

//---------------- public access to these ZT4 cards -------------//
//---------------- public access to these ZT4 cards -------------//
//---------------- public access to these ZT4 cards -------------//

        // ixzt4 = index of desired ZT4 card.

public:     // get values

  long  numZt4Cards           ()       const { return numElements(); }
  long  getActiveZt4CardIndex ()       const { return getActiveIndex(); }

public:     // set values

  void   setActiveZt4CardIndex   (long index) { setActiveIndex(index); }
  void   freezeDependentUpdates   ();
  void   resumeDependentUpdates   ();
  void   performDependentUpdates  ();

public:  // insert or remove cards.
         // the non-void functions return index (ixzt4) where action occurred.
         // the non-void functions return -1 if failed.

  long   appendNewZt4Card           ();
  long   insertNewZt4Card           (long ixzt4);
  long   insertNewZt4CardFromBuffer (long ixzt4);
  long   deleteZt4Card              (long ixzt4);
  long   deleteZt4CardToBuffer      (long ixzt4);
  void   deleteAllZt4Cards          ();

public:    // allocate and free space for ZT4 cards (optional usage).

  void  allocateSpaceForZt4Cards (long nadd) { allocateSpace(nadd); }
  void  freeSpaceForZt4Cards     ()          { freeSpace    (); }


//----------- pass-thru functions to individual ZT4 cards -------------//
//----------- pass-thru functions to individual ZT4 cards -------------//
//----------- pass-thru functions to individual ZT4 cards -------------//

     // ixzt4 = index of desired ZT4 card.

public:      // get card values

  int    getZt4Code                  (long ixzt4)  const;
  float  getZt4FromSourceShotpoint   (long ixzt4)  const;
  float  getZt4ToSourceShotpoint     (long ixzt4)  const;
  long   getZt4SourceLineNumber      (long ixzt4)  const;
  float  getZt4FromReceiverShotpoint (long ixzt4)  const;
  float  getZt4ToReceiverShotpoint   (long ixzt4)  const;
  long   getZt4ReceiverLineNumber    (long ixzt4)  const;

public:      // set card values

  void   setZt4Code                  (long ixzt4, int   value);
  void   setZt4FromSourceShotpoint   (long ixzt4, float value);
  void   setZt4ToSourceShotpoint     (long ixzt4, float value);
  void   setZt4SourceLineNumber      (long ixzt4, long  value);
  void   setZt4FromReceiverShotpoint (long ixzt4, float value);
  void   setZt4ToReceiverShotpoint   (long ixzt4, float value);
  void   setZt4ReceiverLineNumber    (long ixzt4, long  value);


//---------------------- end of functions -----------------------//
//---------------------- end of functions -----------------------//
//---------------------- end of functions -----------------------//

} ;

#endif

//---------------------------- end --------------------------------//
//---------------------------- end --------------------------------//
//---------------------------- end --------------------------------//
