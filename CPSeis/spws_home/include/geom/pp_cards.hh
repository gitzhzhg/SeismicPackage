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

//------------------------ pp_cards.hh ---------------------//
//------------------------ pp_cards.hh ---------------------//
//------------------------ pp_cards.hh ---------------------//

//             header file for the PpCards class
//             derived from the SmartArray class
//                    subdirectory geom

  // This class contains all profile pattern cards (PP cards).

  // This class maintains an array of pointers to PpCard objects.

  // This object owns the PP cards.  This means that this class
  // creates and deletes the PP cards when they are inserted and
  // removed.  They are not to be created or deleted outside of
  // this class.
 

#ifndef _PP_CARDS_HH_
#define _PP_CARDS_HH_

#include "oprim/smart_array.hh"


class PpCards  :  public SmartArray
{

//----------------------- data ------------------------------//
//----------------------- data ------------------------------//
//----------------------- data ------------------------------//

private:

  int   _frozen;           // whether dependent updates are frozen.
  int   _need;             // whether dependent updates are needed.
  long  _active_trace;     // zero if none are active.
  long  _active_group;     // zero if none are active.

  class FgInformer    *_informer;       // informer class.
  class FgConnect     *_connect;        // connecting class.
  class AccBase       *_acc_file;       // array column control.
  class AccBase       *_acc_sshot;      // array column control.
  class AccBase       *_acc_sline;      // array column control.
  class AccBase       *_acc_rshot;      // array column control.
  class AccBase       *_acc_rline;      // array column control.
  class AccBase       *_acc_pattern;    // array column control.
  class AccBase       *_acc_xskid;      // array column control.
  class AccBase       *_acc_yskid;      // array column control.
  class AccBase       *_acc_hold;       // array column control.
  class AccBase       *_acc_elev;       // array column control.
  class AccBase       *_acc_hd;         // array column control.
  class AccBase       *_acc_tuh;        // array column control.
  class AccBase       *_acc_smove;      // array column control.
  class AccBase       *_acc_rmove;      // array column control.
  class AccBase       *_acc_ngroups;    // array column control.
  class AccNtraces    *_acc_ntraces;    // array column control.
  class AccBase       *_sum_thru_gr;    // summation helper class.
  class AccBase       *_sum_thru_tr;    // summation helper class.
  class AccSearch     *_search_thru_gr; // search helper class.
  class AccSearch     *_search_thru_tr; // search helper class.

//--------------------- functions ---------------------------//
//--------------------- functions ---------------------------//
//--------------------- functions ---------------------------//

public:   // constructor and destructor

  PpCards (FgInformer *informer, FgConnect *connect);
  virtual ~PpCards();

/*
  class PpCard *unsafePpCard (long ixpp)  const
                          { return ((PpCard**)_array)[ixpp]; }
*/

  class PpCard *unsafePpCard (long ixpp)  const
                          { return (PpCard*)unsafeArrayElement(ixpp); }

  class PpCard *ppCard (long ixpp)  const
                          { return (PpCard*)arrayElement(ixpp); }

  void receiverPatternsHaveChanged();         // called from FgConnect.

public:    // virtual functions overriding SmartArray.

  virtual int  valueIsDependent  (int ident, long ixpp)  const;
  virtual void setDependencyTrue (int ident, long ixpp);
  virtual void setDependencyFalse(int ident, long ixpp);
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

  void beforeValueChanged (int ident, long index);
  void afterValueChanged  (int ident, long index);

//---------------- public access to these PP cards -------------//
//---------------- public access to these PP cards -------------//
//---------------- public access to these PP cards -------------//

        // ixpp = index of desired PP card.

public:     // get values

  long   numPpCards           ()  const  { return numElements(); }
  long   numGroups            ()  const;
  long   numTraces            ()  const;
  long   groupNumber          (long trace)  const;
  long   channelNumber        (long trace)  const;
  long   getActivePpCardIndex ()  const  { return getActiveIndex(); }
  long   getActiveTraceNumber ()  const  { return _active_trace; }
  long   getActiveGroupNumber ()  const  { return _active_group; }

public:     // set values

  void   setActivePpCardIndex    (long index) { setActiveIndex(index); }
  void   setActiveTraceNumber    (long trace);
  void   setActiveGroupNumber    (long group);
  void   freezeDependentUpdates  ();
  void   resumeDependentUpdates  ();
  void   performDependentUpdates ();

public:  // insert or remove cards.
         // the non-void functions return index (ixpp) where action occurred.
         // the non-void functions return -1 if failed.

  long   appendNewPpCard           ();         
  long   insertNewPpCard           (long ixpp);
  long   insertNewPpCardFromBuffer (long ixpp); 
  long   deletePpCard              (long ixpp);
  long   deletePpCardToBuffer      (long ixpp);
  void   deleteAllPpCards          ();           

public:    // allocate and free space for PP cards (optional usage).

  void  allocateSpaceForPpCards (long nadd) { allocateSpace(nadd); }
  void  freeSpaceForPpCards     ()          { freeSpace    (); }

public:  // search among PP cards.
         // the first two return an index (ixpp), or -1 if not found.
         // the last two return a number, or 0 if not found.

  long  findPpCardWithDesiredGroup  (long group) const;
  long  findPpCardWithDesiredTrace  (long trace) const;
  long  findNumChannelsInGroup      (long group) const;
  long  findTraceNumber             (long group, long channel) const;


//----------- pass-thru functions to individual PP cards -------------//
//----------- pass-thru functions to individual PP cards -------------//
//----------- pass-thru functions to individual PP cards -------------//

     // ixpp = index of desired PP card.

public:      // get card values

  int    ppValueIsDependent   (long ixpp, int ident)  const;
/*
  float  getPpValue           (long ixpp, int ident)  const;
*/
  double getPpValue           (long ixpp, int ident)  const;

  long   getFirstFileNumber   (long ixpp)  const;
  long   getThruFileNumber    (long ixpp)  const;
  float  getSourceShotpoint   (long ixpp)  const;
  float  getReceiverShotpoint (long ixpp)  const;
  long   getSourceLine        (long ixpp)  const;
  long   getReceiverLine      (long ixpp)  const;
  long   getPatternNumber     (long ixpp)  const;
  float  getSourceXskid       (long ixpp)  const;
  float  getSourceYskid       (long ixpp)  const;
  long   getSkidHold          (long ixpp)  const;
  float  getNewElevation      (long ixpp)  const;
  float  getNewHoleDepth      (long ixpp)  const;
  float  getNewUpholeTime     (long ixpp)  const;
  long   getSourceMove        (long ixpp)  const;
  long   getReceiverMove      (long ixpp)  const;
  long   getNumGroupsOnCard   (long ixpp)  const;
  long   getNumTracesOnCard   (long ixpp)  const;
  long   getNumChannelsOnCard (long ixpp)  const;
  long   getFirstGroupNumber  (long ixpp)  const;
  long   getFirstTraceNumber  (long ixpp)  const;
  long   getThruGroupNumber   (long ixpp)  const;
  long   getThruTraceNumber   (long ixpp)  const;

  void   getSourceSkids       (long ixpp,
                      long group, float *inline_skid,
                                  float *crossline_skid)  const;
  void   getGroupAndChannel   (long ixpp,
                      long trace, long *group, long *channel)  const;

public:      // set card values

/*
  void   setPpValue           (long ixpp, int ident, float value);
  void   setDependentPpValue  (long ixpp, int ident, float value);
*/
  void   setPpValue           (long ixpp, int ident, double value);
  void   setDependentPpValue  (long ixpp, int ident, double value);

  void   setFirstFileNumber   (long ixpp, long  value);
  void   setSourceShotpoint   (long ixpp, float value);
  void   setReceiverShotpoint (long ixpp, float value);
  void   setSourceLine        (long ixpp, long  value);
  void   setReceiverLine      (long ixpp, long  value);
  void   setPatternNumber     (long ixpp, long  value);
  void   setSourceXskid       (long ixpp, float value);
  void   setSourceYskid       (long ixpp, float value);
  void   setSkidHold          (long ixpp, long  value);
  void   setNewElevation      (long ixpp, float value);
  void   setNewHoleDepth      (long ixpp, float value);
  void   setNewUpholeTime     (long ixpp, float value);
  void   setSourceMove        (long ixpp, long  value);
  void   setReceiverMove      (long ixpp, long  value);
  void   setNumGroupsOnCard   (long ixpp, long  value);
  void   setNumTracesOnCard   (long ixpp, long  value);

//---------------------- end of functions -----------------------//
//---------------------- end of functions -----------------------//
//---------------------- end of functions -----------------------//

} ;

#endif

//---------------------------- end --------------------------------//
//---------------------------- end --------------------------------//
//---------------------------- end --------------------------------//
