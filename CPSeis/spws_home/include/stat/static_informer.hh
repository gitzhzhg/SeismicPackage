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

//---------------------- static_informer.hh -------------------------//
//---------------------- static_informer.hh -------------------------//
//---------------------- static_informer.hh -------------------------//

//             header file for the StaticInformer class
//                  not derived from any class
//                       subdirectory stat 

   // This class calls a linked list of StaticInform objects when requested.
   // This class does not know anything about the people who call it.
   // This class does not provide any information to the people who call it.

   // Most functions in this class should be called only by a data
   //   object, or by other specific objects.
   // The functions in the "general use" and "data user messages" categories
   //   can be called by anyone.


#ifndef _STAT_INFORMER_HH_
#define _STAT_INFORMER_HH_


class StaticDataset;    // undefined data object.


class StaticInformer
{
  friend class StaticInform;    // to allow calling add/removeInformObject.
  friend class StaticManager;   // to allow calling dataGoingAway and
                                //                  data change notifications.
  friend class StaticDataset;   // to allow calling data change notifications.
  friend class StaticHelper;    // to allow calling data change notifications.
  friend class StatguiWatch;    // to allow calling returningToEventLoop.

//--------------------------- data ----------------------------------//
//--------------------------- data ----------------------------------//
//--------------------------- data ----------------------------------//

private:

  int _multiple;        // counter for multiple operations in progress.
  int _slow;            // whether slow operations are in progress.
  int _looping;         // whether a loop thru data users is in progress.
  int _quitting;        // whether the data object is quitting.

  class StaticInformList *_list;   // linked list of StaticInform objects.


//------------------ constructor and destructor ---------------------//
//------------------ constructor and destructor ---------------------//
//------------------ constructor and destructor ---------------------//

public:       // to be called only by the StaticManager object.

           StaticInformer ();
  virtual ~StaticInformer ();


//----------------- add and remove inform object --------------------//
//----------------- add and remove inform object --------------------//
//----------------- add and remove inform object --------------------//

private:   // to be called only by the base class (friend) StaticInform
           //   constructor or destructor.

  void     addInformObject (class StaticInform *inform);
  void  removeInformObject (class StaticInform *inform);


//------------------ general use functions --------------------------//
//------------------ general use functions --------------------------//
//------------------ general use functions --------------------------//

  // The "beforeChanges" and "afterChanges" functions should be called
  // before and after making multiple calls which change the data.
  // This makes it possible for the StaticInformer class to know that other
  // changes are coming.  The StaticInformer class will postpone notifying
  // the data users (StaticInform objects) that the operations are finished
  // until the "afterChanges" function is called (or the last "afterChanges"
  // function if the "before/afterChanges" functions are nested).

  // The "beginSlowOperations" and "endSlowOperations" functions should be
  // called before and after time-consuming operations take place.
  // Data users will be informed of these calls.  These calls
  // might be used by data users to display a waiting message.  One data
  // user (StatguiWatch) will display the watch cursor when it gets these
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
  // previous message was sent by maybeShowMessage.

  // The "sendMessage" function should be called to send any miscellaneous
  // message (code string, character string, and 5 integers) to data users.
  // In this way, data users which might know about each other, but do not
  // have an easy way to communicate, can communicate through the
  // StaticInform mechanism by recognizing each other from the contents of
  // the code string.

  // The "findActiveGroundPosition" function simply causes data users to
  // be notified that someone wants the active ground position to be
  // made visible, so that data users can take any action that they would
  // normally take when the active ground position actually changes
  // (such as scrolling to it in a table).

public:  // to be called by anybody.
         // these call StaticInform functions with same name and argument list.
         // beforeChanges and afterChanges are called by all of the
         //    pre/post functions in this class.
         // you need to call beforeChanges and afterChanges only if you
         //    want to group pairs of pre/post calls together.
         // beforeChanges and afterChanges are not needed if
         //    pre/post calls are bracketed by a pre/post pair.

  void  beforeChanges ();
  void  afterChanges  ();
               // these non-redundantly call StaticInform functions.
               // these are called from the pre/post functions below.

  void  beginSlowOperations ();
  void  endSlowOperations   ();
               // these non-redundantly call StaticInform functions.
               // these are called by before/afterChanges.

  void  ringBell           ();
  void  showMessage        (const char *msg);
  void  showWorkingMessage (const char *clause, int kount, int number);
  void  sendMessage        (const char *code, const char *msg,
                            int i1 = 0, int i2 = 0,
                            int i3 = 0, int i4 = 0, int i5 = 0);

  void  findActiveGroundPosition (StaticDataset *dataset);

//--------------------- limited use functions -----------------------//
//--------------------- limited use functions -----------------------//
//--------------------- limited use functions -----------------------//

  // The "returningToEventLoop" function is to be called ONLY by
  // StatguiWatch, who gets this magic knowledge by overriding
  // the "update" virtual function in SLDelay.  (Any SLDelay object
  // could have done this, but only one of these is needed in the
  // application, and StatguiWatch didn't have enough to do.)

private:

  void  returningToEventLoop();
          // to be called only by the StatguiWatch (friend) class.
          // this calls StaticInform virtual functions returningToEventLoop().

  void  dataGoingAway();          // last message ever sent.
          // to be called only by the StaticManager (friend) destructor.
          // this calls StaticInform virtual functions dataGoingAway().
          // this then calls StaticInform base class functions dataNowGone().


//--------------------- data change notifications -------------------//
//--------------------- data change notifications -------------------//
//--------------------- data change notifications -------------------//

     // to be called only by StaticManager or StaticDataset (friends).
     // these call StaticInform functions with same name and argument list.
     // these are always flanked by calls to before/afterChanges.

private:

  void   preRemoveInsertDatasets (int index, int nrem, int nins);
  void  postRemoveInsertDatasets (int index, int nrem, int nins);

  void   preNewActiveDataset           ();
  void  postNewActiveDataset           ();

  void   preNewReferenceDataset        ();
  void  postNewReferenceDataset        ();

  void  dataNeedsSavingFlagTurnedOn    (StaticDataset *dataset);
  void  dataNeedsSavingFlagTurnedOff   (StaticDataset *dataset);

  void   preSelectDataset              (StaticDataset *dataset);
  void  postSelectDataset              (StaticDataset *dataset);

  void   preUnselectDataset            (StaticDataset *dataset);
  void  postUnselectDataset            (StaticDataset *dataset);

  void   preChangeDataLock             (StaticDataset *dataset);
  void  postChangeDataLock             (StaticDataset *dataset);

  void  preTotalChanges                (StaticDataset *dataset);
  void postTotalChanges                (StaticDataset *dataset);

     // pre/postTotalChanges means the following:
     //   Anything below might change w/o explicit inform being called.
     //   Number of ground positions is probably changing.
     //   A new static file might be read in.

  void  preNewActiveGroundPosition     (StaticDataset *dataset);
  void postNewActiveGroundPosition     (StaticDataset *dataset);

  void  preChangeSelections   (StaticDataset *dataset,
                               int ix, int iy, int nxchng, int nychng);
  void postChangeSelections   (StaticDataset *dataset,
                               int ix, int iy, int nxchng, int nychng);

  void  preChangeStaticValues (StaticDataset *dataset,
                               int ix, int iy, int nxchng, int nychng);
  void postChangeStaticValues (StaticDataset *dataset,
                               int ix, int iy, int nxchng, int nychng);

  void  preChangeStattype            (StaticDataset *dataset);
  void postChangeStattype            (StaticDataset *dataset);

  void  preChangeHeaderWords         (StaticDataset *dataset);
  void postChangeHeaderWords         (StaticDataset *dataset);

  void  preTransformGroundPositions  (StaticDataset *dataset);
  void postTransformGroundPositions  (StaticDataset *dataset);


//-------------------- data user messages ---------------------------//
//-------------------- data user messages ---------------------------//
//-------------------- data user messages ---------------------------//

    // to be called by anybody.
    // these call StaticInform functions with same name and argument list.
    // these functions are to be used to communicate between data users.
    // these functions are not used by StaticManager or StaticDataset.

public:


//-------------------------- end of functions ---------------------------//
//-------------------------- end of functions ---------------------------//
//-------------------------- end of functions ---------------------------//

} ;

#endif

//---------------------------- end ------------------------------------//
//---------------------------- end ------------------------------------//
//---------------------------- end ------------------------------------//
