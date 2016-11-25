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

//---------------------- fg_informer.hh ---------------------//
//---------------------- fg_informer.hh ---------------------//
//---------------------- fg_informer.hh ---------------------//

//             header file for the FgInformer class
//                  not derived from any class
//                       subdirectory geom 

   // This class calls a linked list of FgInform objects
   // when requested to do so by FieldGeometry.  This class
   // is had by FieldGeometry.


#ifndef _FG_INFORMER_HH_
#define _FG_INFORMER_HH_


class FgInformer
{

//----------------------- data ------------------------------//
//----------------------- data ------------------------------//
//----------------------- data ------------------------------//

private:

  int _started;    // whether data changes have started.
  int _multiple;   // whether multiple operations are in progress.
  int _frozen;     // whether updates are frozen.
  int _slow;       // whether slow operations are in progress.
  int _looping;    // whether a loop thru data users is in progress.
  int _quitting;   // whether FieldGeometry is quitting.

  class FieldGeometry   *_fg;
  class FgInformList    *_inform_list;


//-------------- constructor and destructor -----------------//
//-------------- constructor and destructor -----------------//
//-------------- constructor and destructor -----------------//

public:

  FgInformer (FieldGeometry *fg);
  virtual ~FgInformer();


//------------- add and remove inform object ----------------//
//------------- add and remove inform object ----------------//
//------------- add and remove inform object ----------------//

public:   // called (indirectly) by an FgInform object.

  void     addInformObject (class FgInform *inform);
  void  removeInformObject (class FgInform *inform);


//------------------- private functions -------------------------//
//------------------- private functions -------------------------//
//------------------- private functions -------------------------//

private:   // these call FgInform functions with same name.
           // these are called from some of the public functions below.

  void  startingChanges     ();
  void  finishedChanges     ();
  void  dataNowGone         ();   // needed only by FgInform base class


//------------------ miscellaneous notifications ----------------//
//------------------ miscellaneous notifications ----------------//
//------------------ miscellaneous notifications ----------------//

public: // called from FieldGeometry.
        // these call FgInform functions with same name,
        //   unless otherwise indicated.
        // these are not generally bracketed by starting/finishedChanges.

  void  dataGoingAway();           // last message ever sent

  void   preMultipleOperations();  // does not call FgInform functions
  void  postMultipleOperations();  // does not call FgInform functions

  void  ringBell   ();
  void  showMessage(char *msg);
  void  sendMessage(char *msg, long i1 = 0, long i2 = 0,
                               long i3 = 0, long i4 = 0, long i5 = 0);

  void  returningToEventLoop();

  void  dataNeedsSavingFlagTurnedOff();


//-------------- always inform specific change ------------------//
//-------------- always inform specific change ------------------//
//-------------- always inform specific change ------------------//

public: // called from FieldGeometry.
        // these call FgInform functions with same name.
        // these always call FgInform functions.
        // these are always bracketed by starting/finishedChanges.

  void   preSlowOperations(); // non-redundantly calls FgInform functions
  void  postSlowOperations(); // non-redundantly calls FgInform functions

  void  dataNeedsSavingFlagTurnedOn();

  void  freezingDependentUpdates();
  void  dependentValuesOutOfDate();
  void   preResumeDependentUpdates();
  void  postResumeDependentUpdates();

  void   preChangeDataLock();
  void  postChangeDataLock();

  void   preNewGridTransform();
  void  postNewGridTransform();
  void   preNewTestingGridTransform();
  void  postNewTestingGridTransform();

  void  sourceGathersOutOfDate  ();
  void   preUpdateSourceGathers ();
  void  postUpdateSourceGathers ();

  void  receiverGathersOutOfDate  ();
  void   preUpdateReceiverGathers ();
  void  postUpdateReceiverGathers ();

  void  midpointGathersOutOfDate  ();
  void   preUpdateMidpointGathers ();
  void  postUpdateMidpointGathers ();

  void  liveFoldOutOfDate         ();
  void   preUpdateLiveFold        ();
  void  postUpdateLiveFold        ();


//------------- sometimes inform specific range -----------------//
//------------- sometimes inform specific range -----------------//
//------------- sometimes inform specific range -----------------//

public: // called from FieldGeometry.
        // these call FgInform functions with same name.
        // these do not call FgInform functions when updates are frozen.
        // these are always bracketed by starting/finishedChanges.
        // ixl = index of seismic line.

  void   preNewChaining();
  void  postNewChaining();

  void   preSortByLineNumber();
  void  postSortByLineNumber();

  void   preSortReceiverPatterns ();
  void  postSortReceiverPatterns ();

  void   preReverseLineDirection(long ixl);
  void  postReverseLineDirection(long ixl);

  void   preNewActiveLine();
  void  postNewActiveLine();

  void   preNewActiveFlag(long ixl);
  void  postNewActiveFlag(long ixl);

  void   preNewActiveRpCard();
  void  postNewActiveRpCard();

  void   preNewActivePpCard();
  void  postNewActivePpCard();

  void   preNewActiveZt1Card();
  void  postNewActiveZt1Card();

  void   preNewActiveZt2Card();
  void  postNewActiveZt2Card();

  void   preNewActiveZt3Card();
  void  postNewActiveZt3Card();

  void   preNewActiveZt4Card();
  void  postNewActiveZt4Card();

  void  preNewActiveCmp     ();
  void postNewActiveCmp     ();

  void  preNewActiveTrace   ();
  void postNewActiveTrace   ();

  void  preNewActiveGroup   ();
  void postNewActiveGroup   ();

  void  preCmpSelectionsChanged ();
  void postCmpSelectionsChanged ();

  void  preLineSelectionsChanged (long index, long nrem, long nins);
  void postLineSelectionsChanged (long index, long nrem, long nins);

  void   preFlagValuesChanged
                (long ixl, int ident, long index, long nrem, long nins);
  void  postFlagValuesChanged
                (long ixl, int ident, long index, long nrem, long nins);

  void   preRemoveInsertFlags (long ixl, long index, long nrem, long nins);
  void  postRemoveInsertFlags (long ixl, long index, long nrem, long nins);

  void   preLineNumbersChanged (long index, long nrem, long nins);
  void  postLineNumbersChanged (long index, long nrem, long nins);

  void   preRemoveInsertLines (long index, long nrem, long nins);
  void  postRemoveInsertLines (long index, long nrem, long nins);

  void   preRemoveInsertPpCards (long index, long nrem, long nins);
  void  postRemoveInsertPpCards (long index, long nrem, long nins);

  void   prePpValuesChanged (int ident, long index, long nrem, long nins);
  void  postPpValuesChanged (int ident, long index, long nrem, long nins);

  void   preRemoveInsertRpCards (long index, long nrem, long nins);
  void  postRemoveInsertRpCards (long index, long nrem, long nins);

  void   preRpValuesChanged (int ident, long index, long nrem, long nins);
  void  postRpValuesChanged (int ident, long index, long nrem, long nins);

  void   preRemoveInsertZt1Cards (long index, long nrem, long nins);
  void  postRemoveInsertZt1Cards (long index, long nrem, long nins);

  void   preZt1ValuesChanged (int ident, long index, long nrem, long nins);
  void  postZt1ValuesChanged (int ident, long index, long nrem, long nins);

  void   preRemoveInsertZt2Cards (long index, long nrem, long nins);
  void  postRemoveInsertZt2Cards (long index, long nrem, long nins);

  void   preZt2ValuesChanged (int ident, long index, long nrem, long nins);
  void  postZt2ValuesChanged (int ident, long index, long nrem, long nins);

  void   preRemoveInsertZt3Cards (long index, long nrem, long nins);
  void  postRemoveInsertZt3Cards (long index, long nrem, long nins);

  void   preZt3ValuesChanged (int ident, long index, long nrem, long nins);
  void  postZt3ValuesChanged (int ident, long index, long nrem, long nins);

  void   preRemoveInsertZt4Cards (long index, long nrem, long nins);
  void  postRemoveInsertZt4Cards (long index, long nrem, long nins);

  void   preZt4ValuesChanged (int ident, long index, long nrem, long nins);
  void  postZt4ValuesChanged (int ident, long index, long nrem, long nins);

/*
 * Called from FgTeData
 * ehs
 */
  void   preTredValuesChanged ();
  void  postTredValuesChanged ();


//---------------------- end of functions -----------------------//
//---------------------- end of functions -----------------------//
//---------------------- end of functions -----------------------//

} ;

#endif

//---------------------------- end --------------------------------//
//---------------------------- end --------------------------------//
//---------------------------- end --------------------------------//
