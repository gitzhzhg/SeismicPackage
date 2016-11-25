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

//------------------------ zt3_cards.hh ---------------------//
//------------------------ zt3_cards.hh ---------------------//
//------------------------ zt3_cards.hh ---------------------//

//             header file for the Zt3Cards class
//             derived from the SmartArray class
//                    subdirectory geom

  // This class contains all ZT3 cards (zero receivers).

  // This class maintains an array of pointers to Zt3Card objects.

  // This object owns the ZT3 cards.  This means that this class
  // creates and deletes the ZT3 cards when they are inserted and
  // removed.  They are not to be created or deleted outside of
  // this class.
 

#ifndef _ZT3_CARDS_HH_
#define _ZT3_CARDS_HH_

#include "oprim/smart_array.hh"


class Zt3Cards  :  public SmartArray
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

  Zt3Cards (FgInformer *informer, FgConnect *connect);
  virtual ~Zt3Cards();

  class Zt3Card *zt3Card (long ixzt3)  const
                          { return (Zt3Card*)arrayElement(ixzt3); }

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

//---------------- public access to these ZT3 cards -------------//
//---------------- public access to these ZT3 cards -------------//
//---------------- public access to these ZT3 cards -------------//

        // ixzt3 = index of desired ZT3 card.

public:     // get values

  long  numZt3Cards           ()       const { return numElements(); }
  long  getActiveZt3CardIndex ()       const { return getActiveIndex(); }

public:     // set values

  void   setActiveZt3CardIndex   (long index) { setActiveIndex(index); }
  void   freezeDependentUpdates   ();
  void   resumeDependentUpdates   ();
  void   performDependentUpdates  ();

public:  // insert or remove cards.
         // the non-void functions return index (ixzt3) where action occurred.
         // the non-void functions return -1 if failed.

  long   appendNewZt3Card           ();
  long   insertNewZt3Card           (long ixzt3);
  long   insertNewZt3CardFromBuffer (long ixzt3);
  long   deleteZt3Card              (long ixzt3);
  long   deleteZt3CardToBuffer      (long ixzt3);
  void   deleteAllZt3Cards          ();

public:    // allocate and free space for ZT3 cards (optional usage).

  void  allocateSpaceForZt3Cards (long nadd) { allocateSpace(nadd); }
  void  freeSpaceForZt3Cards     ()          { freeSpace    (); }


//----------- pass-thru functions to individual ZT3 cards -------------//
//----------- pass-thru functions to individual ZT3 cards -------------//
//----------- pass-thru functions to individual ZT3 cards -------------//

     // ixzt3 = index of desired ZT3 card.

public:      // get card values

  int    getZt3Code                (long ixzt3)  const;
  long   getZt3FromGroupNumber     (long ixzt3)  const;
  long   getZt3ToGroupNumber       (long ixzt3)  const;
  long   getZt3FromTraceNumber     (long ixzt3)  const;
  long   getZt3ToTraceNumber       (long ixzt3)  const;

public:      // set card values

  void   setZt3Code                (long ixzt3, int   value);
  void   setZt3FromGroupNumber     (long ixzt3, long  value);
  void   setZt3ToGroupNumber       (long ixzt3, long  value);
  void   setZt3FromTraceNumber     (long ixzt3, long  value);
  void   setZt3ToTraceNumber       (long ixzt3, long  value);


//---------------------- end of functions -----------------------//
//---------------------- end of functions -----------------------//
//---------------------- end of functions -----------------------//

} ;

#endif

//---------------------------- end --------------------------------//
//---------------------------- end --------------------------------//
//---------------------------- end --------------------------------//
