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

//------------------------ rp_cards.hh ---------------------//
//------------------------ rp_cards.hh ---------------------//
//------------------------ rp_cards.hh ---------------------//

//             header file for the RpCards class
//             derived from the SmartArray class
//                    subdirectory geom

  // This class contains all receiver pattern cards (RP cards).

  // This class maintains an array of pointers to RpCard objects.

  // This object owns the RP cards.  This means that this class
  // creates and deletes the RP cards when they are inserted and
  // removed.  They are not to be created or deleted outside of
  // this class.
 

#ifndef _RP_CARDS_HH_
#define _RP_CARDS_HH_

#include "oprim/smart_array.hh"


class RpCards  :  public SmartArray
{

//----------------------- data ------------------------------//
//----------------------- data ------------------------------//
//----------------------- data ------------------------------//

private:

  int   _frozen;           // whether dependent updates are frozen.

  class FgInformer    *_informer;  // informer class.
  class FgConnect     *_connect;   // connections class.
  class FastSort      *_sort;      // sort helper class.
  class AccSearch     *_search;    // search helper class.
  class AccNchan      *_nchan;     // number of channels helper class.

//--------------------- functions ---------------------------//
//--------------------- functions ---------------------------//
//--------------------- functions ---------------------------//

public:   // constructor and destructor

  RpCards (FgInformer *informer, FgConnect *connect);
  virtual ~RpCards();

/*
  class RpCard *unsafeRpCard (long ixrp)  const
                          { return ((RpCard**)_array)[ixrp]; }
*/

  class RpCard *unsafeRpCard (long ixrp)  const
                          { return (RpCard*)unsafeArrayElement(ixrp); }

  class RpCard *rpCard (long ixrp)  const
                          { return (RpCard*)arrayElement(ixrp); }

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

//---------------- public access to these RP cards -------------//
//---------------- public access to these RP cards -------------//
//---------------- public access to these RP cards -------------//

        // ixrp = index of desired RP card.

public:     // get values

  long  numRpCards           ()             const { return numElements(); }
  long  numChannelsInPattern (long pattern) const;
  long  getActiveRpCardIndex ()             const { return getActiveIndex(); }
  int   receiverPatternsAreSorted ()        const;

public:     // set values

  void   setActiveRpCardIndex     (long index) { setActiveIndex(index); }
  void   sortReceiverPatterns     ();
  void   freezeDependentUpdates   ();
  void   resumeDependentUpdates   ();
  void   performDependentUpdates  ();

public:  // insert or remove cards.
         // the non-void functions return index (ixrp) where action occurred.
         // the non-void functions return -1 if failed.

  long   appendNewRpCard           ();
  long   insertNewRpCard           (long ixrp);
  long   placeNewRpCard            (long pattern);
  long   insertNewRpCardFromBuffer (long ixrp);
  long   deleteRpCard              (long ixrp);
  long   deleteRpCardToBuffer      (long ixrp);
  void   deleteAllRpCards          ();

public:    // allocate and free space for RP cards (optional usage).

  void  allocateSpaceForRpCards (long nadd) { allocateSpace(nadd); }
  void  freeSpaceForRpCards     ()          { freeSpace    (); }

public:    // search among RP cards.
           // these return an index (ixrp), or -1 if not found.
           // the ixrp argument should be the first pp card of the
           //   desired pattern (as returned by findReceiverPattern).

  long   findReceiverPattern          (long pattern)  const;
  long   findEndOfReceiverPattern     (long pattern)  const;
  long   findRpCardWithDesiredChannel (long pattern, long channel)  const;

  long   getEndOfReceiverPattern      (long ixrp)  const;
  long   getRpCardWithDesiredChannel  (long ixrp, long channel)  const;
  long   getRpCardWithDesiredChannel  (long ixrp_first,
                                    long ixrp_last, long channel)  const;

  void   getRpIrregularRange(long ixrp_first, long ixrp_last,
                   long *ixrp_first_irreg, long *ixrp_last_irreg)  const;

//----------- pass-thru functions to individual RP cards -------------//
//----------- pass-thru functions to individual RP cards -------------//
//----------- pass-thru functions to individual RP cards -------------//

     // ixrp = index of desired RP card.

public:      // get card values

  class RpCard *getRpCardPointer (long ixrp)     const;

  long   getRpPatternNumber (long ixrp)  const;
  long   getRpNumChannels   (long ixrp)  const;
  long   getRpCumChannels   (long ixrp)  const;
  int    getRpFlag          (long ixrp)  const;
  float  getRpShotpoint     (long ixrp)  const;
  long   getRpLineNumber    (long ixrp)  const;
  long   getRpNumX          (long ixrp)  const;
  long   getRpXinc          (long ixrp)  const;
  long   getRpNumY          (long ixrp)  const;
  long   getRpYinc          (long ixrp)  const;
  float  getRpXskid         (long ixrp)  const;
  float  getRpYskid         (long ixrp)  const;
  float  getRpEskid         (long ixrp)  const;

  void   getRpChannelsOnCard(long ixrp,
                  long *first_channel, long *last_channel)  const;

  void   getRpIncrements    (long ixrp, long channel,
                             long *xinc,  long *yinc,
                             long *incrx, long *incry)  const;

public:      // set card values

  void   setRpPatternNumber (long ixrp, long  value);
  void   setRpFlag          (long ixrp, int   value);
  void   setRpShotpoint     (long ixrp, float value);
  void   setRpLineNumber    (long ixrp, long  value);
  void   setRpNumX          (long ixrp, long  value);
  void   setRpXinc          (long ixrp, long  value);
  void   setRpNumY          (long ixrp, long  value);
  void   setRpYinc          (long ixrp, long  value);
  void   setRpXskid         (long ixrp, float value);
  void   setRpYskid         (long ixrp, float value);
  void   setRpEskid         (long ixrp, float value);

//---------------------- end of functions -----------------------//
//---------------------- end of functions -----------------------//
//---------------------- end of functions -----------------------//

} ;

#endif

//---------------------------- end --------------------------------//
//---------------------------- end --------------------------------//
//---------------------------- end --------------------------------//
