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

//---------------------- vf_informer.hh -------------------------//
//---------------------- vf_informer.hh -------------------------//
//---------------------- vf_informer.hh -------------------------//

//             header file for the VfInformer class
//                  not derived from any class
//                       subdirectory vf 

   // This class calls a linked list of VfInform objects when requested.
   // This class does not know anything about the people who call it.
   // This class does not provide any information to the people who call it.

   // Most functions in this class should be called only by a data
   //   object, or by other specific objects.
   // The functions in the "general use" and "data user messages" categories
   //   can be called by anyone.


#ifndef _VF_INFORMER_HH_
#define _VF_INFORMER_HH_


class VfDataset;    // undefined data object.


class VfInformer
{
  friend class VfInform;    // to allow calling add/removeInformObject.
  friend class VfManager;   // to allow calling dataGoingAway and
                            //                  data change notifications.
  friend class VfHelper;    // to allow calling data change notifications.
  friend class VfguiWatch;  // to allow calling returningToEventLoop.

//--------------------------- data ----------------------------------//
//--------------------------- data ----------------------------------//
//--------------------------- data ----------------------------------//

private:

  int _multiple;        // counter for multiple operations in progress.
  int _slow;            // whether slow operations are in progress.
  int _looping;         // whether a loop thru data users is in progress.
  int _quitting;        // whether the data object is quitting.

  class VfInformList *_list;   // linked list of VfInform objects.


//------------------ constructor and destructor ---------------------//
//------------------ constructor and destructor ---------------------//
//------------------ constructor and destructor ---------------------//

public:       // to be called only by the VfManager object.

           VfInformer ();
  virtual ~VfInformer ();


//----------------- add and remove inform object --------------------//
//----------------- add and remove inform object --------------------//
//----------------- add and remove inform object --------------------//

private:   // to be called only by the base class (friend) VfInform
           //   constructor or destructor.

  void     addInformObject (class VfInform *inform);
  void  removeInformObject (class VfInform *inform);


//------------------ general use functions --------------------------//
//------------------ general use functions --------------------------//
//------------------ general use functions --------------------------//

  // The "beforeChanges" and "afterChanges" functions should be called
  // before and after making multiple calls which change the data.
  // This makes it possible for the VfInformer class to know that other
  // changes are coming.  The VfInformer class will postpone notifying
  // the data users (VfInform objects) that the operations are finished
  // until the "afterChanges" function is called (or the last "afterChanges"
  // function if the "before/afterChanges" functions are nested).

  // The "beginSlowOperations" and "endSlowOperations" functions should be
  // called before and after time-consuming operations take place.
  // Data users will be informed of these calls.  These calls
  // might be used by data users to display a waiting message.  One data
  // user (VfguiWatch) will display the watch cursor when it gets these
  // calls.  It is usually not necessary to call these functions because they
  // are called automatically when changes are made to the data.  It WILL be
  // necessary to call these functions if time-consuming operations will
  // commence when no changes are made to this data object.  If these
  // functions is called in a nested manner, data users will be notified only
  // when the bracketing functions are called.

  // The "showMessage" function should be called to send any miscellaneous
  // message (character string) to data users.  These calls can be used
  // by data users to display the message.

  // The "showWorkingMessage" function should be called to send any repeated
  // message (character string) (regarding time-consuming work in progress)
  // to data users.  These calls can be used by data users to display the
  // message.  The message is constructed from clause, kount, and number
  // to appear as follows: "clause kount+1 of number".  Such a message
  // will be sent (via the showMessage function) only if kount is a multiple
  // of 10, and then only if at least 0.2 seconds has elapsed since the
  // previous message was sent by maybeShowMessage.  If the clause argument
  // is NULL, the function will do nothing.

  // The "sendMessage" function should be called to send any miscellaneous
  // message (code string, character string, and 5 integers) to data users.
  // In this way, data users which might know about each other, but do not
  // have an easy way to communicate, can communicate through the
  // VfInform mechanism by recognizing each other from the contents of
  // the code string.

  // The "findActiveVelocityFunction" function simply causes data users to
  // be notified that someone wants the active velocity function to be
  // made visible, so that data users can take any action that they would
  // normally take when the active velocity function actually changes
  // (such as scrolling to it in a table).

public:  // to be called by anybody.
         // these call VfInform functions with same name and argument list.
         // beforeChanges and afterChanges are called by all of the
         //    pre/post functions in this class.
         // you need to call beforeChanges and afterChanges only if you
         //    want to group pairs of pre/post calls together.
         // beforeChanges and afterChanges are not needed if
         //    pre/post calls are bracketed by a pre/post pair.

  void  beforeChanges ();
  void  afterChanges  ();
               // these non-redundantly call VfInform functions.
               // these are called from the pre/post functions below.

  void  beginSlowOperations ();
  void  endSlowOperations   ();
               // these non-redundantly call VfInform functions.
               // these are called by before/afterChanges.

  void  ringBell           ();
  void  showMessage        (const char *msg);
  void  showWorkingMessage (const char *clause, long kount, long number);
  void  sendMessage        (const char *code, const char *msg,
                            long i1 = 0, long i2 = 0,
                            long i3 = 0, long i4 = 0, long i5 = 0);

  void  findActiveVelocityFunction (VfDataset *dataset);


//--------------------- limited use functions -----------------------//
//--------------------- limited use functions -----------------------//
//--------------------- limited use functions -----------------------//

  // The "returningToEventLoop" function is to be called ONLY by
  // VfguiWatch, who gets this magic knowledge by overriding
  // the "update" virtual function in SLDelay.  (Any SLDelay object
  // could have done this, but only one of these is needed in the
  // application, and VfguiWatch didn't have enough to do.)

private:

  void  returningToEventLoop();
          // to be called only by the VfguiWatch (friend) class.
          // this calls VfInform virtual functions returningToEventLoop().

  void  dataGoingAway();          // last message ever sent.
          // to be called only by the VfManager (friend) destructor.
          // this calls VfInform virtual functions dataGoingAway().
          // this then calls VfInform base class functions dataNowGone().

  void  newHistoryCard (VfDataset *dataset);
          // to be called only by the VfHelper (friend) class.
          // this calls VfInform virtual functions newHistoryCard(dataset).

//--------------------- data change notifications -------------------//
//--------------------- data change notifications -------------------//
//--------------------- data change notifications -------------------//

     // to be called only by VfManager or VfHelper (friends).
     // these call VfInform functions with same name and argument list.
     // these are always flanked by calls to before/afterChanges.
     // pre/postNewNeighbors is called whenever anything changes which
     //   might change the index of the velocity functions which are
     //   neighbors of the active velocity function (prev/next in x/y dir).

private:

  void  preChangeBinTolerances      ();
  void postChangeBinTolerances      ();

  void  preNewActiveDataset         ();
  void postNewActiveDataset         ();

  void  preNewReferenceDataset      ();
  void postNewReferenceDataset      ();

  void  preSelectDataset            (VfDataset *dataset);
  void postSelectDataset            (VfDataset *dataset);

  void  preUnselectDataset          (VfDataset *dataset);
  void postUnselectDataset          (VfDataset *dataset);

  void  preTotalChanges             (VfDataset *dataset);
  void postTotalChanges             (VfDataset *dataset);

  void  preRemoveInsertDatasets (long index, long nrem, long nins);
  void postRemoveInsertDatasets (long index, long nrem, long nins);

  void  preChangeDataLock           (VfDataset *dataset);
  void postChangeDataLock           (VfDataset *dataset);

  void dataNeedsSavingFlagTurnedOn  (VfDataset *dataset);
  void dataNeedsSavingFlagTurnedOff (VfDataset *dataset);

     // pre/postTotalChanges means the following:
     //   Anything below might change w/o explicit inform being called.
     //   Number of velocity functions is probably changing.
     //   A new velocity file might be read in.

  void  preChangeHeaderWords            (VfDataset *dataset);
  void postChangeHeaderWords            (VfDataset *dataset);

  void  preChangeMoveoutOrder           (VfDataset *dataset);
  void postChangeMoveoutOrder           (VfDataset *dataset);

  void  preChangeUnits                  (VfDataset *dataset);
  void postChangeUnits                  (VfDataset *dataset);

  void  preNewActiveVelocityFunction    (VfDataset *dataset);
  void postNewActiveVelocityFunction    (VfDataset *dataset);

  void  preNewReferenceVelocityFunction (VfDataset *dataset);
  void postNewReferenceVelocityFunction (VfDataset *dataset);

  void  preNewNeighbors                 (VfDataset *dataset);
  void postNewNeighbors                 (VfDataset *dataset);

  void  preRemoveInsertVelocityFunctions
                       (VfDataset *dataset, long ifun, long nrem, long nins);
  void postRemoveInsertVelocityFunctions
                       (VfDataset *dataset, long ifun, long nrem, long nins);

     // pre/postRemoveInsertVelocityFunctions means the following:
     //   Anything below (for the removed/inserted functions) might change,
     //   or velocity function selections (at or after ifun) might change,
     //   w/o explicit inform being called.

  void  preChangeSelections  (VfDataset *dataset, long ifun, long nchng);
  void postChangeSelections  (VfDataset *dataset, long ifun, long nchng);

  void  preChangeCoords      (VfDataset *dataset, long ifun, long nchng);
  void postChangeCoords      (VfDataset *dataset, long ifun, long nchng);

  void  preNewDefaultTypes   (VfDataset *dataset, long ifun, long nchng);
  void postNewDefaultTypes   (VfDataset *dataset, long ifun, long nchng);

  void  preModifyStrings     (VfDataset *dataset, long ifun, long nchng);
  void postModifyStrings     (VfDataset *dataset, long ifun, long nchng);

  void  preNewActivePicks    (VfDataset *dataset, long ifun, long nchng);
  void postNewActivePicks    (VfDataset *dataset, long ifun, long nchng);

  void  preChangePickSelections    (VfDataset *dataset, long ifun);
  void postChangePickSelections    (VfDataset *dataset, long ifun);

  void  preModifyPicks
         (VfDataset *dataset, long ifun, int type, long ipick, long nrem);
  void postModifyPicks
         (VfDataset *dataset, long ifun, int type, long ipick, long nrem,
                                                               long nins);

   // Warning: The number of picks removed and inserted (nrem and nins above)
   // refer to the two arrays which define the velocity type being modified.
   // All other arrays should be considered to change from the specified
   // index (ipick) to the end of the array.  If picks are modified (but
   // not removed or inserted), nrem and nins will be equal.

   // Note: pre/postModifyPicks is called for only a single velocity
   // function only.  If several functions are modified simultaneously,
   // either this function will be called more than once, or (more likely)
   // pre/postTotalChanges will be called.

//-------------------- data user messages ---------------------------//
//-------------------- data user messages ---------------------------//
//-------------------- data user messages ---------------------------//

    // to be called by anybody.
    // these call VfInform functions with same name and argument list.
    // these functions are to be used to communicate between data users.
    // these functions are not used by VfManager or VfDataset.

public:

  void   preNewActiveHorizon         ();
  void  postNewActiveHorizon         ();

  void   preRemoveInsertHorizons (long index, long nrem, long nins);
  void  postRemoveInsertHorizons (long index, long nrem, long nins);

  void   preNewSelectedHorizons      ();
  void  postNewSelectedHorizons      ();

  void   preNewActiveHorizonPick     (long ihorizon);
  void  postNewActiveHorizonPick     (long ihorizon);

  void   preNewHorizonColor          (long ihorizon);
  void  postNewHorizonColor          (long ihorizon);

  void   preNewHorizonTransform      ();
  void  postNewHorizonTransform      ();

//-------------------------- end of functions ---------------------------//
//-------------------------- end of functions ---------------------------//
//-------------------------- end of functions ---------------------------//

} ;

#endif

//---------------------------- end ------------------------------------//
//---------------------------- end ------------------------------------//
//---------------------------- end ------------------------------------//
