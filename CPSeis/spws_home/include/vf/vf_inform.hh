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

//------------------------ vf_inform.hh ----------------------------//
//------------------------ vf_inform.hh ----------------------------//
//------------------------ vf_inform.hh ----------------------------//

//             header file for the VfInform class
//                 not derived from any class
//                     subdirectory vf


//  VfInform is a base class for classes which use data in the VfDataset
//  classes owned by the VfManager class.  The VfDataset objects (and the
//  VfManager object) comprise the DATA, and the VfInform object is either
//  the DATA USER itself, or is had by a DATA USER.  The VfManager and
//  VfDataset objects arrange for a linked list of VfInform objects to be
//  called whenever anything changes within the VfManager object or any of
//  its VfDataset objects.

//  The VfInform object adds itself to the linked list by calling
//  the VfManager object from its base constructor, and removes
//  itself from the linked list by calling the VfManager object
//  from its base destructor.

//  The VfManager object and its VfDataset objects uses a VfInformer
//  object to do the dirty work of maintaining and calling the linked list.

//  ASSUMPTIONS:  It is assumed that there is only one VfManager
//  object with which a given VfInform object will interact.  It is
//  also assumed that this VfManager object, and all of its VfDataset
//  objects, already exist when the VfInform object is created.

//  ASSUMPTIONS:  It is assumed that a given VfInform object can
//  interact with the VfManager object, or with any VfDataset object
//  managed by the VfManager object, although it may choose to
//  interact with only one or a few of the VfDataset objects (for
//  example, with only the "active" object, or the "reference" object,
//  or "selected" objects, or "editable" objects, or objects of its
//  own choosing).

//  WARNING:  It is not allowed to call functions on VfManager or any
//  VfDataset object which CHANGE THE DATA, from an overriding virtual
//  function which receives messages thru the VfInform mechanism.  This
//  would cause changes to be made while other changes are in the process
//  of being made and announced, with possibly disasterous repercussions.
//  Any VfDataset object might assert if an illegal call is made to
//  VfDataset from within an overriding VfInform virtual function.


#ifndef _VF_INFORM_HH_
#define _VF_INFORM_HH_


class VfDataset;


class VfInform
{
  friend class VfInformer;   // to allow calling most of these functions.

//---------------------------- data -------------------------------//
//---------------------------- data -------------------------------//
//---------------------------- data -------------------------------//

private:

  class VfManager  *_manager;
  const int         _printit;   // whether to print on command line.
  int               _disabled;  // whether to temporarily disable
                                //    sending messages to this object.


//--------------- constructor and destructor and manager ------------------//
//--------------- constructor and destructor and manager ------------------//
//--------------- constructor and destructor and manager ------------------//

    // If you set printit to TRUE, each non-overridden virtual function
    // will print a message on the command line each time it is called.

public:

           VfInform (VfManager *manager, int printit = 0);
  virtual ~VfInform ();

  VfManager *manager()  const  { return _manager; }


//------------------ private helper functions ----------------------//
//------------------ private helper functions ----------------------//
//------------------ private helper functions ----------------------//

private:   // helper functions for printing on command line.
           // to be called only from non-overridden virtual functions.

  void dispose ();
  void front   (const char *string, class VfDataset *dataset = 0);
  void style0  (const char *string, VfDataset *dataset = 0);
  void style1  (const char *string, VfDataset *dataset,
                   const char *msg);
  void style1  (const char *string, VfDataset *dataset,
                   long ihorizon);
  void style2  (const char *string, VfDataset *dataset,
                   long ifun, long nchng);
  void style3  (const char *string, VfDataset *dataset,
                   long index, long nrem, long nins);
  void style4  (const char *string, VfDataset *dataset,
                   long ifun, int type, long ipick, long nrem);
  void style5  (const char *string, VfDataset *dataset,
                   long ifun, int type, long ipick, long nrem, long nins);
  void style7  (const char *string, VfDataset *dataset,
                   const char *code, const char *msg,
                   long i1, long i2, long i3, long i4, long i5);


//------------------------- data now gone ---------------------------//
//------------------------- data now gone ---------------------------//
//------------------------- data now gone ---------------------------//

private:   // to be called (thru VfInformer) only by VfManager
           //   when the data is about to disappear.
           // called after calling the virtual function dataGoingAway().
           // needed only by VfInform base class.
           // this function exists to make sure the derived VfInform
           //   class cannot try to access VfManager after it is
           //   no longer viable.

  void  dataNowGone()  { _manager = 0; }        // NULL


//---------------------- disable/enable messages --------------------//
//---------------------- disable/enable messages --------------------//
//---------------------- disable/enable messages --------------------//

public: // inhibits sending messages to this data user.
        // intended to allow users who initiate a change to
        //   keep from being notified of that change.
        // can also be used to disable messages for any other reason.
        // data users must keep in mind that a specific requested
        //   change may initiate other dependent changes.

  // The first function should be called (by the derived class or
  //   by someone who controls the derived class) before initiating
  //   changes in VfManager or VfDataset, or before sending any messages
  //   directly, for which you do not want to be notified.
  // The second function should be called when it is time to
  //   re-enable messages to this data user.
  // The fourth function is called by VfInformer to determine
  //   whether to send a message.

  void disableMessages     ()         { _disabled = 1; }    // TRUE
  void  enableMessages     ()         { _disabled = 0; }    // FALSE
  int  messagesAreDisabled ()  const  { return  _disabled; }
  int  messagesAreEnabled  ()  const  { return !_disabled; }


//----------------- general use functions ---------------------------//
//----------------- general use functions ---------------------------//
//----------------- general use functions ---------------------------//

  // to be called only by VfInformer.
  // to be called (thru VfInformer) by VfManager, VfDataset, or anybody else.
  // to be overridden by derived class.

private:

  virtual void beforeChanges();  
  virtual void afterChanges();  

  virtual void beginSlowOperations();
  virtual void endSlowOperations();

  virtual void ringBell     ();  
  virtual void showMessage  (const char *msg);  
  virtual void sendMessage  (const char *code, const char *msg,
                             long i1, long i2, long i3, long i4, long i5);

  virtual void findActiveVelocityFunction (VfDataset *dataset);


//--------------------- limited use functions -----------------------//
//--------------------- limited use functions -----------------------//
//--------------------- limited use functions -----------------------//

  // to be called only by VfInformer.
  // returningToEventLoop is to be called (thru VfInformer) only by VfguiWatch.
  // dataGoingAway is to be called (thru VfInformer) only by the VfManager
  //   destructor.
  // newHistoryCard is to be called (thru VfInformer) only by VfDataset.
  // to be overridden by derived class.

private:

  virtual void returningToEventLoop ();  
  virtual void dataGoingAway        ();         // last message ever sent.
  virtual void newHistoryCard       (VfDataset *dataset);


//--------------------- data change notifications -------------------//
//--------------------- data change notifications -------------------//
//--------------------- data change notifications -------------------//

    // to be called only by VfInformer.
    // to be called (thru VfInformer) only by VfManager or VfDataset.
    // to be overridden by derived class.
    // these are always flanked by calls to before/afterChanges().
    // pre/postNewNeighbors is called whenever anything changes which
    //   might change the index of the velocity functions which are
    //   neighbors of the active velocity function (prev/next in x/y dir).

private:

  virtual void   preChangeBinTolerances      ();
  virtual void  postChangeBinTolerances      ();

  virtual void   preNewActiveDataset         ();
  virtual void  postNewActiveDataset         ();

  virtual void   preNewReferenceDataset      ();
  virtual void  postNewReferenceDataset      ();

  virtual void   preSelectDataset            (VfDataset *dataset);
  virtual void  postSelectDataset            (VfDataset *dataset);

  virtual void   preUnselectDataset          (VfDataset *dataset);
  virtual void  postUnselectDataset          (VfDataset *dataset);

  virtual void  preTotalChanges              (VfDataset *dataset);
  virtual void postTotalChanges              (VfDataset *dataset);

  virtual void   preRemoveInsertDatasets (long index, long nrem, long nins);
  virtual void  postRemoveInsertDatasets (long index, long nrem, long nins);

  virtual void  preChangeDataLock            (VfDataset *dataset);
  virtual void postChangeDataLock            (VfDataset *dataset);

  virtual void dataNeedsSavingFlagTurnedOn   (VfDataset *dataset);
  virtual void dataNeedsSavingFlagTurnedOff  (VfDataset *dataset);

     // pre/postTotalChanges means the following:
     //   Anything below might change w/o explicit inform being called.
     //   Number of velocity functions is probably changing.
     //   A new velocity file might be read in.

  virtual void  preChangeHeaderWords            (VfDataset *dataset);
  virtual void postChangeHeaderWords            (VfDataset *dataset);

  virtual void  preChangeMoveoutOrder           (VfDataset *dataset);
  virtual void postChangeMoveoutOrder           (VfDataset *dataset);

  virtual void  preChangeUnits                  (VfDataset *dataset);
  virtual void postChangeUnits                  (VfDataset *dataset);

  virtual void  preNewActiveVelocityFunction    (VfDataset *dataset);
  virtual void postNewActiveVelocityFunction    (VfDataset *dataset);

  virtual void  preNewReferenceVelocityFunction (VfDataset *dataset);
  virtual void postNewReferenceVelocityFunction (VfDataset *dataset);

  virtual void  preNewNeighbors                 (VfDataset *dataset);
  virtual void postNewNeighbors                 (VfDataset *dataset);

  virtual void  preRemoveInsertVelocityFunctions
                       (VfDataset *dataset, long ifun, long nrem, long nins);
  virtual void postRemoveInsertVelocityFunctions
                       (VfDataset *dataset, long ifun, long nrem, long nins);

     // pre/postRemoveInsertVelocityFunctions means the following:
     //   Anything below (for the removed/inserted functions) might change,
     //   or velocity function selections (at or after ifun) might change,
     //   w/o explicit inform being called.

  virtual void  preChangeSelections (VfDataset *dataset, long ifun, long nchng);
  virtual void postChangeSelections (VfDataset *dataset, long ifun, long nchng);

  virtual void  preChangeCoords     (VfDataset *dataset, long ifun, long nchng);
  virtual void postChangeCoords     (VfDataset *dataset, long ifun, long nchng);

  virtual void  preNewDefaultTypes  (VfDataset *dataset, long ifun, long nchng);
  virtual void postNewDefaultTypes  (VfDataset *dataset, long ifun, long nchng);

  virtual void  preModifyStrings    (VfDataset *dataset, long ifun, long nchng);
  virtual void postModifyStrings    (VfDataset *dataset, long ifun, long nchng);

  virtual void  preNewActivePicks   (VfDataset *dataset, long ifun, long nchng);
  virtual void postNewActivePicks   (VfDataset *dataset, long ifun, long nchng);

  virtual void  preChangePickSelections    (VfDataset *dataset, long ifun);
  virtual void postChangePickSelections    (VfDataset *dataset, long ifun);

  virtual void  preModifyPicks
       (VfDataset *dataset, long ifun, int type, long ipick, long nrem);
  virtual void postModifyPicks 
       (VfDataset *dataset, long ifun, int type, long ipick, long nrem,
                                                             long nins);

   // Warning: The number of picks removed and inserted (nrem and nins above)
   // refer to the two arrays which define the velocity type being modified.
   // All other arrays should be considered to change from the specified
   // index (ipick) to the end of the array.  If picks are modified (but
   // not removed or inserted), nrem and nins will be equal.

   // Note: pre/postModifyPicks is called for only a single velocity
   // function.  If several functions are modified simultaneously,
   // either this function will be called more than once, or (more likely)
   // pre/postTotalChanges will be called.


//-------------------- data user messages ---------------------------//
//-------------------- data user messages ---------------------------//
//-------------------- data user messages ---------------------------//

    // to be called only by VfInformer.
    // to be called (thru VfInformer) by anybody.
    // these functions are to be used to communicate between data users.
    // these functions are not used by VfManager or VfDataset.
    // to be overridden by derived class.

private:

  virtual void   preNewActiveHorizon         ();
  virtual void  postNewActiveHorizon         ();

  virtual void   preRemoveInsertHorizons (long ihorizon, long nrem, long nins);
  virtual void  postRemoveInsertHorizons (long ihorizon, long nrem, long nins);

  virtual void   preNewSelectedHorizons      ();
  virtual void  postNewSelectedHorizons      ();

  virtual void   preNewActiveHorizonPick     (long ihorizon);
  virtual void  postNewActiveHorizonPick     (long ihorizon);

  virtual void   preNewHorizonColor          (long ihorizon);
  virtual void  postNewHorizonColor          (long ihorizon);

  virtual void   preNewHorizonTransform      ();
  virtual void  postNewHorizonTransform      ();


//---------------------- end of functions -----------------------//
//---------------------- end of functions -----------------------//
//---------------------- end of functions -----------------------//

} ;

#endif

//---------------------------- end --------------------------------//
//---------------------------- end --------------------------------//
//---------------------------- end --------------------------------//
