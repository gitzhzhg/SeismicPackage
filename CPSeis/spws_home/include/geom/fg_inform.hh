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

//------------------------ fg_inform.hh ----------------------------//
//------------------------ fg_inform.hh ----------------------------//
//------------------------ fg_inform.hh ----------------------------//

//            header file for the FgInform class
//                 not derived from any class
//                    subdirectory geom


//  FgInform is a base class for classes which use data in the
//  FieldGeometry class.  The FieldGeometry object is the DATA,
//  and the FgInform object is either the DATA USER itself, or
//  is had by a DATA USER.  The FieldGeometry object arranges
//  for a linked list of FgInform objects to be called whenever
//  anything changes within the FieldGeometry object.

//  ASSUMPTIONS:  It is assumed that there is only one FieldGeometry
//  object with which a given FgInform object will interact.  It is
//  also assumed that this FieldGeometry object already exists when
//  the FgInform object is created.

//  The FgInform object adds itself to the linked list by calling
//  the FieldGeometry object from its base constructor, and removes
//  itself from the linked list by calling the FieldGeometry object
//  from its base destructor.

//  The FieldGeometry object uses an FgInformer object to do the
//  dirty work of maintaining and calling the linked list.

//  The data user must understand that when it gets the message
//  that either FG_XLOC or FG_YLOC values have changed (constants
//  in fg_constants.hh), that it must be assumed that both of the
//  corresponding FG_XGRID and FG_YGRID values have also changed.

//  Normally, this class should be derived from.  However, this class
//  can also be used, without derivation, to test the proper receipt
//  of all messages which can come from FieldGeometry.  If you make
//  an instance of this class, and set the printit argument (in the
//  constructor) to TRUE, each virtual function will print a message
//  on the command line each time it is called.

//  WARNING:  It is not allowed to call functions on FieldGeometry
//  which CHANGE THE DATA, from an overriding virtual function which
//  receives messages from FieldGeometry thru the FgInform mechanism.
//  This would cause changes to be made while other changes are in
//  the process of being made, with possibly disasterous repercussions.
//  FieldGeometry will assert if an illegal call is made to FieldGeometry
//  from within an overriding FgInform virtual function.


#ifndef _FG_INFORM_HH_
#define _FG_INFORM_HH_


class FgInform
{

  friend class FgInformer;

//------------------------ data -------------------------------//
//------------------------ data -------------------------------//
//------------------------ data -------------------------------//

protected:

  class FieldGeometry *_fg;

private:

  const int _printit;   // whether to print message in
                        //    non-overridden virtual functions.
  int       _disabled;  // whether to temporarily disable
                        //    sending messages to this object.

//-------------- constructor and destructor ---------------------//
//-------------- constructor and destructor ---------------------//
//-------------- constructor and destructor ---------------------//

  // for testing, call the constructor directly with printit = TRUE.
  // otherwise, call the constructor from a derived class without printit.

public:

  FgInform(FieldGeometry *fg, int printit = 0);  // FALSE
  virtual ~FgInform();

  FieldGeometry *getFieldGeometry()  const  { return _fg; }

//----------------- private functions -----------------------//
//----------------- private functions -----------------------//
//----------------- private functions -----------------------//

private: // called only by FgInformer (indirectly from FieldGeometry).

  void  dataNowGone();   // needed only by FgInform base class

//------------------ disable/enable messages --------------------//
//------------------ disable/enable messages --------------------//
//------------------ disable/enable messages --------------------//

public: // inhibits sending messages to this data user.
        // intended to allow users who initiate a change to
        //   keep from being notified of the change.
        // can also be used to disable messages for any other reason.
        // data users must keep in mind that a specific requested
        //   change may initiate other dependent changes.

  // The first function should be called (by the derived class or
  //   by someone who controls the derived class) before initiating
  //   changes in FieldGeometry for which you do not want to be
  //   notified.
  // The second function should be called when it is time to
  //   re-enable messages to this data user.
  // The third function is called by FieldGeometry (actually by
  //   FgInformer) to determine whether to send a message.

  void disableMessages     ()         { _disabled = 1; }    // TRUE
  void  enableMessages     ()         { _disabled = 0; }    // FALSE
  int  messagesAreDisabled ()  const  { return _disabled; }

//------------- miscellaneous notifications ---------------------//
//------------- miscellaneous notifications ---------------------//
//------------- miscellaneous notifications ---------------------//

protected: // called only by FgInformer (indirectly from FieldGeometry).
           // to be overridden by derived class.

  virtual void dataGoingAway        (FieldGeometry *fg);

  virtual void startingChanges      (FieldGeometry *fg);  
  virtual void finishedChanges      (FieldGeometry *fg);  

  virtual void ringBell             (FieldGeometry *fg);  
  virtual void showMessage          (FieldGeometry *fg, char *msg);  
  virtual void sendMessage          (FieldGeometry *fg, char *msg,
                       long i1, long i2, long i3, long i4, long i5);

  virtual void returningToEventLoop (FieldGeometry *fg);  


//------------ always inform specific change -----------------//
//------------ always inform specific change -----------------//
//------------ always inform specific change -----------------//

protected: // called only by FgInformer (indirectly from FieldGeometry).
           // to be overridden by derived class.
           // always flanked by calls to starting/finishedChanges().

  virtual void  preSlowOperations         (FieldGeometry *fg);
  virtual void postSlowOperations         (FieldGeometry *fg);

  virtual void dataNeedsSavingFlagTurnedOn    (FieldGeometry *fg);
  virtual void dataNeedsSavingFlagTurnedOff   (FieldGeometry *fg);

  virtual void freezingDependentUpdates   (FieldGeometry *fg);
  virtual void dependentValuesOutOfDate   (FieldGeometry *fg);
  virtual void  preResumeDependentUpdates (FieldGeometry *fg);
  virtual void postResumeDependentUpdates (FieldGeometry *fg);

  virtual void  preChangeDataLock         (FieldGeometry *fg);
  virtual void postChangeDataLock         (FieldGeometry *fg);

  virtual void  preNewGridTransform          (FieldGeometry *fg);  
  virtual void postNewGridTransform          (FieldGeometry *fg);  
  virtual void  preNewTestingGridTransform   (FieldGeometry *fg);  
  virtual void postNewTestingGridTransform   (FieldGeometry *fg);  

  virtual void sourceGathersOutOfDate        (FieldGeometry *fg);
  virtual void  preUpdateSourceGathers       (FieldGeometry *fg);  
  virtual void postUpdateSourceGathers       (FieldGeometry *fg);  

  virtual void receiverGathersOutOfDate      (FieldGeometry *fg);
  virtual void  preUpdateReceiverGathers     (FieldGeometry *fg);  
  virtual void postUpdateReceiverGathers     (FieldGeometry *fg);  

  virtual void midpointGathersOutOfDate      (FieldGeometry *fg);
  virtual void  preUpdateMidpointGathers     (FieldGeometry *fg);  
  virtual void postUpdateMidpointGathers     (FieldGeometry *fg);  

  virtual void liveFoldOutOfDate             (FieldGeometry *fg);
  virtual void  preUpdateLiveFold            (FieldGeometry *fg);  
  virtual void postUpdateLiveFold            (FieldGeometry *fg);  

//------------ sometimes inform specific change -----------------//
//------------ sometimes inform specific change -----------------//
//------------ sometimes inform specific change -----------------//

protected: // called only by FgInformer (indirectly from FieldGeometry).
           // to be overridden by derived class.
           // always flanked by calls to starting/finishedChanges().
           // not called while dependent updates are frozen.
           // ixl = index of seismic line.

  virtual void  preNewChaining       (FieldGeometry *fg);  
  virtual void postNewChaining       (FieldGeometry *fg);  

  virtual void  preSortByLineNumber  (FieldGeometry *fg);  
  virtual void postSortByLineNumber  (FieldGeometry *fg);  

  virtual void  preReverseLineDirection (FieldGeometry *fg, long ixl);  
  virtual void postReverseLineDirection (FieldGeometry *fg, long ixl);  

  virtual void  preNewActiveLine     (FieldGeometry *fg);  
  virtual void postNewActiveLine     (FieldGeometry *fg);  

  virtual void  preNewActiveFlag     (FieldGeometry *fg, long ixl);  
  virtual void postNewActiveFlag     (FieldGeometry *fg, long ixl);  

  virtual void  preNewActiveRpCard   (FieldGeometry *fg);  
  virtual void postNewActiveRpCard   (FieldGeometry *fg);  

  virtual void  preNewActivePpCard   (FieldGeometry *fg);  
  virtual void postNewActivePpCard   (FieldGeometry *fg);  

  virtual void  preNewActiveZt1Card  (FieldGeometry *fg);  
  virtual void postNewActiveZt1Card  (FieldGeometry *fg);  

  virtual void  preNewActiveZt2Card  (FieldGeometry *fg);  
  virtual void postNewActiveZt2Card  (FieldGeometry *fg);  

  virtual void  preNewActiveZt3Card  (FieldGeometry *fg);  
  virtual void postNewActiveZt3Card  (FieldGeometry *fg);  

  virtual void  preNewActiveZt4Card  (FieldGeometry *fg);  
  virtual void postNewActiveZt4Card  (FieldGeometry *fg);  

  virtual void  preNewActiveCmp      (FieldGeometry *fg);  
  virtual void postNewActiveCmp      (FieldGeometry *fg);  

  virtual void  preNewActiveTrace    (FieldGeometry *fg);  
  virtual void postNewActiveTrace    (FieldGeometry *fg);  

  virtual void  preNewActiveGroup    (FieldGeometry *fg);  
  virtual void postNewActiveGroup    (FieldGeometry *fg);  

  virtual void  preCmpSelectionsChanged (FieldGeometry *fg);
  virtual void postCmpSelectionsChanged (FieldGeometry *fg); 

  virtual void  preLineSelectionsChanged (FieldGeometry *fg,
                                       long index, long nrem, long nins);
  virtual void postLineSelectionsChanged (FieldGeometry *fg,
                                       long index, long nrem, long nins); 

  virtual void  preFlagValuesChanged (FieldGeometry *fg,
                  long ixl, int ident, long index, long nrem, long nins);
  virtual void postFlagValuesChanged (FieldGeometry *fg,
                  long ixl, int ident, long index, long nrem, long nins);

  virtual void  preRemoveInsertFlags (FieldGeometry *fg,
                             long ixl, long index, long nrem, long nins);  
  virtual void postRemoveInsertFlags (FieldGeometry *fg,
                             long ixl, long index, long nrem, long nins);  

  virtual void  preLineNumbersChanged    (FieldGeometry *fg,
                                       long index, long nrem, long nins);  
  virtual void postLineNumbersChanged    (FieldGeometry *fg,
                                       long index, long nrem, long nins);  

  virtual void  preRemoveInsertLines (FieldGeometry *fg,
                                       long index, long nrem, long nins);  
  virtual void postRemoveInsertLines (FieldGeometry *fg,
                                       long index, long nrem, long nins);  

  virtual void  preRemoveInsertPpCards (FieldGeometry *fg,
                                 long index, long nrem, long nins);
  virtual void postRemoveInsertPpCards (FieldGeometry *fg,
                                 long index, long nrem, long nins);

  virtual void  prePpValuesChanged (FieldGeometry *fg,
                      int ident, long index, long nrem, long nins);
  virtual void postPpValuesChanged (FieldGeometry *fg,
                      int ident, long index, long nrem, long nins);

  virtual void  preRemoveInsertRpCards (FieldGeometry *fg,
                                 long index, long nrem, long nins);
  virtual void postRemoveInsertRpCards (FieldGeometry *fg,
                                 long index, long nrem, long nins);

  virtual void  preRpValuesChanged (FieldGeometry *fg,
                      int ident, long index, long nrem, long nins);
  virtual void postRpValuesChanged (FieldGeometry *fg,
                      int ident, long index, long nrem, long nins);

  virtual void  preRemoveInsertZt1Cards (FieldGeometry *fg,
                                 long index, long nrem, long nins);
  virtual void postRemoveInsertZt1Cards (FieldGeometry *fg,
                                 long index, long nrem, long nins);

  virtual void  preZt1ValuesChanged (FieldGeometry *fg,
                      int ident, long index, long nrem, long nins);
  virtual void postZt1ValuesChanged (FieldGeometry *fg,
                      int ident, long index, long nrem, long nins);

  virtual void  preRemoveInsertZt2Cards (FieldGeometry *fg,
                                 long index, long nrem, long nins);
  virtual void postRemoveInsertZt2Cards (FieldGeometry *fg,
                                 long index, long nrem, long nins);

  virtual void  preZt2ValuesChanged (FieldGeometry *fg,
                      int ident, long index, long nrem, long nins);
  virtual void postZt2ValuesChanged (FieldGeometry *fg,
                      int ident, long index, long nrem, long nins);

  virtual void  preRemoveInsertZt3Cards (FieldGeometry *fg,
                                 long index, long nrem, long nins);
  virtual void postRemoveInsertZt3Cards (FieldGeometry *fg,
                                 long index, long nrem, long nins);

  virtual void  preZt3ValuesChanged (FieldGeometry *fg,
                      int ident, long index, long nrem, long nins);
  virtual void postZt3ValuesChanged (FieldGeometry *fg,
                      int ident, long index, long nrem, long nins);

  virtual void  preRemoveInsertZt4Cards (FieldGeometry *fg,
                                 long index, long nrem, long nins);
  virtual void postRemoveInsertZt4Cards (FieldGeometry *fg,
                                 long index, long nrem, long nins);

  virtual void  preZt4ValuesChanged (FieldGeometry *fg,
                      int ident, long index, long nrem, long nins);
  virtual void postZt4ValuesChanged (FieldGeometry *fg,
                      int ident, long index, long nrem, long nins);

  virtual void  preTredValuesChanged (FieldGeometry *fg);
  virtual void postTredValuesChanged (FieldGeometry *fg);

  virtual void  preSortReceiverPatterns (FieldGeometry *fg);
  virtual void postSortReceiverPatterns (FieldGeometry *fg);

//---------------------- end of functions -----------------------//
//---------------------- end of functions -----------------------//
//---------------------- end of functions -----------------------//

} ;

#endif

//---------------------------- end --------------------------------//
//---------------------------- end --------------------------------//
//---------------------------- end --------------------------------//
