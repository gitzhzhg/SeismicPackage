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

//------------------------ static_inform.hh ----------------------------//
//------------------------ static_inform.hh ----------------------------//
//------------------------ static_inform.hh ----------------------------//

//            header file for the StaticInform class
//                 not derived from any class
//                     subdirectory stat


//  StaticInform is a base class for classes which use data in the
//  StaticDataset classes owned by the StaticManager class.
//  The StaticDataset objects (and the StaticManager object)
//  comprise the DATA, and the StaticInform object is either the
//  DATA USER itself, or is had by a DATA USER.  The StaticManager
//  and StaticDataset objects arrange for a linked list of StaticInform
//  objects to be called whenever anything changes within the StaticManager
//  object or any of its StaticDataset objects.

//  The StaticInform object adds itself to the linked list by calling
//  the StaticManager object from its base constructor, and removes
//  itself from the linked list by calling the StaticManager object
//  from its base destructor.

//  The StaticManager object and its managed StaticDataset objects use a
//  StaticInformer object to do the dirty work of maintaining and
//  calling the linked list.

//  ASSUMPTIONS:  It is assumed that there is only one StaticManager
//  object with which a given StaticInform object will interact.  It is
//  also assumed that this StaticManager object, and the StaticDataset
//  objects it manages, already exist when the StaticInform object is
//  created.

//  ASSUMPTIONS:  It is assumed that a given StaticInform object can
//  interact with the StaticManager object, or with any StaticDataset object
//  managed by the StaticManager object, although it may choose to
//  interact with only one or a few of the StaticDataset objects (for
//  example, with only the "active" object, or the "reference" object,
//  or "selected" objects, or objects of its own choosing).

//  WARNING:  It is not allowed to call functions on StaticManager
//  or any StaticDataset object which CHANGE THE DATA, from an overriding
//  virtual function which receives messages thru the StaticInform mechanism.
//  This would cause changes to be made while other changes are in
//  the process of being made, with possibly disasterous repercussions.
//  Any StaticDataset will assert if an illegal call is made to StaticDataset
//  from within an overriding StaticInform virtual function.


#ifndef _STATIC_INFORM_HH_
#define _STATIC_INFORM_HH_


class StaticDataset;


class StaticInform
{

  friend class StaticInformer;   // to allow calling most of these functions.

//---------------------------- data -------------------------------//
//---------------------------- data -------------------------------//
//---------------------------- data -------------------------------//

private:

  class StaticManager *_manager;
  const int            _printit;   // whether to print on command line.
  int                  _disabled;  // whether to temporarily disable
                                   //    sending messages to this object.


//--------------- constructor and destructor and manager ------------------//
//--------------- constructor and destructor and manager ------------------//
//--------------- constructor and destructor and manager ------------------//

    // If you set printit to TRUE, each non-overridden virtual function
    // will print a message on the command line each time it is called.

public:

           StaticInform (StaticManager *manager, int printit = 0);
  virtual ~StaticInform ();

  StaticManager *manager()  const  { return _manager; }


//------------------ private helper functions ----------------------//
//------------------ private helper functions ----------------------//
//------------------ private helper functions ----------------------//

private:   // helper functions for printing on command line.
           // to be called only from non-overridden virtual functions.

  void dispose ();
  void front   (const char *string, class StaticDataset *dataset = 0);
  void style0  (const char *string, StaticDataset *dataset = 0);
  void style1  (const char *string, StaticDataset *dataset,
                   const char *msg);
  void style3  (const char *string, StaticDataset *dataset,
                   int index, int nrem, int nins);
  void style4  (const char *string, StaticDataset *dataset,
                   int ix, int iy, int nxchng, int nychng);
  void style7  (const char *string, StaticDataset *dataset,
                   const char *code, const char *msg,
                   int i1, int i2, int i3, int i4, int i5);


//------------------------- data now gone ---------------------------//
//------------------------- data now gone ---------------------------//
//------------------------- data now gone ---------------------------//

private:    // to be called (thru StaticInformer) only by StaticManager
            //   when the data is about to disappear.
            // called after calling the virtual function dataGoingAway().
            // needed only by StaticInform base class.
            // this function exists to make sure the derived StaticInform
            //   class cannot try to access StaticManager after it is
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
  //   changes in StaticManager or StaticDataset for which you
  //   do not want to be notified.
  // The second function should be called when it is time to
  //   re-enable messages to this data user.
  // The third function is called by StaticInformer to determine
  //   whether to send a message.

  void disableMessages     ()         { _disabled = 1; }    // TRUE
  void  enableMessages     ()         { _disabled = 0; }    // FALSE
  int  messagesAreDisabled ()  const  { return  _disabled; }
  int  messagesAreEnabled  ()  const  { return !_disabled; }


//------------------- general use functions ---------------------------//
//------------------- general use functions ---------------------------//
//------------------- general use functions ---------------------------//

  // to be called only by StaticInformer.
  // to be called (thru StaticInformer) by StaticManager, StaticDataset,
  //                                            or anybody else.
  // to be overridden by derived class.

private:

  virtual void beforeChanges();  
  virtual void afterChanges();  

  virtual void beginSlowOperations();
  virtual void endSlowOperations();

  virtual void ringBell     ();  
  virtual void showMessage  (const char *msg);  
  virtual void sendMessage  (const char *code, const char *msg,
                             int i1, int i2, int i3, int i4, int i5);

  virtual void findActiveGroundPosition (StaticDataset *dataset);


//--------------------- limited use functions -----------------------//
//--------------------- limited use functions -----------------------//
//--------------------- limited use functions -----------------------//

  // to be called only by StatInformer.
  // returningToEventLoop is to be called (thru StaticInformer) only
  //   by StatguiWatch.
  // dataGoingAway is to be called (thru StaticInformer) only by the
  //   StaticManager destructor.
  // to be overridden by derived class.

private:

  virtual void returningToEventLoop ();  
  virtual void dataGoingAway        ();         // last message ever sent.


//-------------------- data change notifications ---------------------//
//-------------------- data change notifications ---------------------//
//-------------------- data change notifications ---------------------//

    // to be called only by StaticInformer.
    // to be called (thru StaticInformer) only by StaticManager
    //   or StaticDataset.
    // to be overridden by derived class.
    // these are always flanked by calls to before/afterChanges().

private:

  virtual void   preRemoveInsertDatasets (int index, int nrem, int nins);
  virtual void  postRemoveInsertDatasets (int index, int nrem, int nins);

  virtual void  preNewActiveDataset          ();
  virtual void postNewActiveDataset          ();

  virtual void  preNewReferenceDataset       ();
  virtual void postNewReferenceDataset       ();

  virtual void dataNeedsSavingFlagTurnedOn   (StaticDataset *dataset);
  virtual void dataNeedsSavingFlagTurnedOff  (StaticDataset *dataset);

  virtual void  preSelectDataset             (StaticDataset *dataset);
  virtual void postSelectDataset             (StaticDataset *dataset);

  virtual void  preUnselectDataset           (StaticDataset *dataset);
  virtual void postUnselectDataset           (StaticDataset *dataset);

  virtual void  preChangeDataLock            (StaticDataset *dataset);
  virtual void postChangeDataLock            (StaticDataset *dataset);

  virtual void  preTotalChanges              (StaticDataset *dataset);
  virtual void postTotalChanges              (StaticDataset *dataset);

     // pre/postTotalChanges means the following:
     //   Anything below might change w/o explicit inform being called.
     //   Number of ground positions is probably changing.
     //   A new static file might be read in.

  virtual void  preNewActiveGroundPosition (StaticDataset *dataset);
  virtual void postNewActiveGroundPosition (StaticDataset *dataset);

  virtual void  preChangeSelections (StaticDataset *dataset,
                              int ix, int iy, int nxchng, int nychng);
  virtual void postChangeSelections (StaticDataset *dataset,
                              int ix, int iy, int nxchng, int nychng);

  virtual void  preChangeStaticValues (StaticDataset *dataset,
                              int ix, int iy, int nxchng, int nychng);
  virtual void postChangeStaticValues (StaticDataset *dataset,
                              int ix, int iy, int nxchng, int nychng);

  virtual void  preChangeStattype           (StaticDataset *dataset);
  virtual void postChangeStattype           (StaticDataset *dataset);

  virtual void  preChangeHeaderWords        (StaticDataset *dataset);
  virtual void postChangeHeaderWords        (StaticDataset *dataset);

  virtual void  preTransformGroundPositions (StaticDataset *dataset);
  virtual void postTransformGroundPositions (StaticDataset *dataset);


//-------------------- data user messages ---------------------------//
//-------------------- data user messages ---------------------------//
//-------------------- data user messages ---------------------------//

    // to be called only by StaticInformer.
    // to be called (thru StaticInformer) by anybody.
    // these functions are to be used to communicate between data users.
    // these functions are not used by StaticManager or StaticDataset.
    // to be overridden by derived class.

private:


//---------------------- end of functions -----------------------//
//---------------------- end of functions -----------------------//
//---------------------- end of functions -----------------------//

} ;

#endif

//---------------------------- end --------------------------------//
//---------------------------- end --------------------------------//
//---------------------------- end --------------------------------//
