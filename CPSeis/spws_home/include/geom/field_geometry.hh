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

//---------------------- field_geometry.hh ---------------------//
//---------------------- field_geometry.hh ---------------------//
//---------------------- field_geometry.hh ---------------------//

//            header file for the FieldGeometry class
//                  not derived from any class
//                       subdirectory geom 

// This class contains seismic field geometry data.

// ixl      = index of seismic line   in an entire seismic survey.
// ixf      = index of field flag     in a seismic line.
// ixrp     = index of RP card        in an array of receiver pattern cards.
// ixpp     = index of PP card        in an array of profile pattern cards.
// ixzt1    = index of ZT1 card       in an array of zero-trace cards.
// ixzt2    = index of ZT2 card       in an array of zero-trace cards.
// ixzt3    = index of ZT3 card       in an array of zero-trace cards.
// ixzt4    = index of ZT4 card       in an array of zero-trace cards.
// ixs2     = index of source         in an array of sources shot at a flag.
// ixr2     = index of trace          in an array of traces recorded at a flag.
// ixorig   = index of trace          among all traces in acquisition order.
// ixsorted = index of trace          among all traces in CMP-sorted order.
// ixcmp    = index of CMP gather     in an array of all CMP gathers.
// ixfold   = index of trace in CMP   in an array of traces in a CMP gather.
// group    = original group number   (starting with 1) in acquisition order.
// channel  = channel number          (starting with 1) in original group.
// trace    = sequential trace number (starting with 1) in acquisition order.
// ihead    = header word number      (starting with 1).

         //  0  <=  ixl       <   numLines().
         //  0  <=  ixf       <   numFlagsOnLine(ixl).
         //  0  <=  ixrp      <   numRpCards().
         //  0  <=  ixpp      <   numPpCards().
         //  0  <=  ixzt1     <   numZt1Cards().
         //  0  <=  ixzt2     <   numZt2Cards().
         //  0  <=  ixzt3     <   numZt3Cards().
         //  0  <=  ixzt4     <   numZt4Cards().
         //  0  <=  ixs2      <   numSourcesAtFlag  (ixl, ixf).
         //  0  <=  ixr2      <   numReceiversAtFlag(ixl, ixf).
         //  0  <=  ixorig    <   numTraces().
         //  0  <=  ixsorted  <   numTraces().
         //  0  <=  ixcmp     <   numCmpGathers().
         //  0  <=  ixfold    <   foldOfStack(ixcmp).
         //  1  <=  group     <=  numGroups().
         //  1  <=  channel   <=  getNumChannelsOnCard(ixpp).
         //  1  <=  trace     <=  numTraces().
         //  1  <=  ihead     <=  CPS_MAX_HEADERS == 64.


#ifndef _FIELD_GEOMETRY_HH_
#define _FIELD_GEOMETRY_HH_


class FieldGeometry
{

  typedef int AbortFun (void *abort_data);

//----------------------- data ------------------------------//
//----------------------- data ------------------------------//
//----------------------- data ------------------------------//

private:

  int _data_needs_saving; // whether data has changed (since last saved).
  int _frozen;            // whether updates of dependent values are frozen.
  int _out_of_date;       // whether dependent values are out-of-date.
  int _major;             // whether major operations are in progress.
  int _data_changing;     // whether data is in the process of changing.
  int _lock;              // to what extent data changes are locked.
  int _updatingMidpointGathers;	/* ehs */

  class FgInformer          *_informer;
  class FgUserAbort         *_ua;
  class FgConnect           *_connect;
  class GridTransform       *_transform;
  class GridTransform       *_testing;
  class SeisSurvey          *_survey;
  class PpCards             *_pp_cards;
  class RpCards             *_rp_cards;
  class ZtCards             *_zt_cards;
  class FgGroups            *_groups;
  class FgTraces            *_traces;
  class Midpoints           *_midpoints;
  class FgTraceValues       *_tv;
  class FgHeaders           *_headers;


//------------------ private functions ----------------------//
//------------------ private functions ----------------------//
//------------------ private functions ----------------------//

private:

/*
 * Need access to these methods for TRED
 */
public:	/* ehs */

  int  isLocked             (int lowest_lock);
  void notifyDataWillChange (int chng);
  void notifyDataHasChanged (int chng);


//------------------ constructor and destructor ------------------//
//------------------ constructor and destructor ------------------//
//------------------ constructor and destructor ------------------//

public:

  FieldGeometry ();
  virtual ~FieldGeometry();


//--------------------- user abort functions ----------------------//
//--------------------- user abort functions ----------------------//
//--------------------- user abort functions ----------------------//

public:   // register a function which will return TRUE if a time-
          //   consuming operation is to be aborted.
          // call with NULL's to disable this function.

  void registerAbortFun (AbortFun *abort_fun, void *abort_data);

public:   // these functions have no effect (or return FALSE) if the
          //   above function wasn't called or was called with NULL's.

  void startAbortOption();   // display abort button.
  void stopAbortOption();    // remove abort button.
  int  aborted();            // returns TRUE if user pressed abort button.


//------------- and or remove FgInform object --------------------//
//------------- and or remove FgInform object --------------------//
//------------- and or remove FgInform object --------------------//

public:  // called from FgInform constructor/destructor.

  void  addInformObject    (class FgInform *inform);
  void  removeInformObject (class FgInform *inform);


//------------------ miscellaneous functions -----------------------//
//------------------ miscellaneous functions -----------------------//
//------------------ miscellaneous functions -----------------------//

public:  // freezing and resuming updates.
         // functions returning int will return TRUE or FALSE.

  void   freezeDependentUpdates   ();
  void   resumeDependentUpdates   ();
  int    dependentUpdatesFrozen   ()  const  { return _frozen; }
  int    dependentValuesOutOfDate ()  const  { return _out_of_date; }

public:  // data-needs-saving flag.
         // is set to TRUE by this class when data is changed.
         // should be set to FALSE after data is read from file.
         // should be set to FALSE after data is saved to file.
         // function returning int will return TRUE or FALSE.

  void turnOffDataNeedsSavingFlag ();
  int  dataNeedsSaving            ()  const  { return _data_needs_saving; }

public:  // data locking control.
         // "allow..." functions will return TRUE or FALSE.
         // in some cases, some data on the specified cards can be
         //   changed even if FALSE is returned.

  void setDataLock      (int lock);
  void maximizeDataLock ();
  int  getDataLock                  ()  const  { return _lock; }
  int  allowDeletingData            ()  const;
  int  allowModifyingLdCards        ()  const;
  int  allowModifyingPpCards        ()  const;
  int  allowModifyingRpCards        ()  const;
  int  allowModifyingZt1Cards       ()  const;
  int  allowModifyingZt2Cards       ()  const;
  int  allowModifyingZt3Cards       ()  const;
  int  allowModifyingZt4Cards       ()  const;
  int  allowModifyingGridTransform  ()  const;


//--------------- interface to FgInform data users -----------------//
//--------------- interface to FgInform data users -----------------//
//--------------- interface to FgInform data users -----------------//

  // The "pre..." and "post..." functions should always be called
  // in pairs.  The function pairs can be nested.

  // The "...MultipleOperations" functions should be called before
  // and after making multiple calls which change the data.  This
  // makes it possible for this class to know that other changes
  // are coming before the event loop is returned to.  This class will
  // postpone notifying the data users that the operations are finished
  // until the "post..." function is called (or the last "post..."
  // function if nested).

  // The "...SlowOperations" functions should be called before and
  // after time-consuming operations are in progress.  Data users
  // will be informed of these calls.  These calls might be used
  // by FgInform objects to display a waiting message.

  // This FieldGeometry class will call the "...MultipleOperations"
  // and/or "...SlowOperations" functions itself if it has the
  // knowledge or judgment to do so.

  // The "showMessage" function should be called to send any
  // miscellaneous message (character string) to data users.
  // These calls can be used by FgInform objects to display
  // the message.

  // The "sendMessage" function should be called to send any
  // miscellaneous message (character string plus 5 integers)
  // to data users.  In this way, data users which might know
  // about each other, but do not have an easy way to communicate,
  // can communicate through the FgInform mechanism by recognizing
  // each other from the contents of the character string.

  // The "returningToEventLoop" function is to be called only by
  // FgWatchInform, who gets this magic knowledge by overriding
  // the "update" virtual function in SLDelay.  (Any SLDelay object
  // could have done this, but only one of these is needed in the
  // application, and FgWatchInform didn't have enough to do.)

  // The "...MajorChanges" functions should be called before and after
  // making a large number of changes, such as inputting data from a
  // file.  These functions differ from the "...MultipleOperations"
  // functions in the following ways:
  //   (1) the "...MultipleOperations" functions are called.
  //   (2) the "...SlowOperations" functions are called.
  //   (3) the "freezeDependentUpdates" and "resumeDependentUpdates"
  //         functions are called.
  // These calls substantially improve the efficiency of making a
  // large number of changes to the data by:
  //   (a) postponing intermediate updates of dependent values to the
  //         end, rather than doing (and re-doing) updates after each
  //         change.
  //   (b) deleting most intermediate calls to data users until the end.
  // A consequence is that data users will not learn the myriad details
  // of the changes which have occurred, and will need to redisplay
  // everything from scratch when they get the final message.

public:

  void preMultipleOperations   ();
  void postMultipleOperations  ();
  void preSlowOperations       ();
  void postSlowOperations      ();
  void ringBell                ();
  void showMessage             (char *msg);
  void sendMessage             (char *msg, long i1, long i2, long i3,
                                                    long i4, long i5);
  void returningToEventLoop    ();
  void preMajorChanges         ();
  void postMajorChanges        ();


//------------------------- grid transform -----------------------//
//------------------------- grid transform -----------------------//
//------------------------- grid transform -----------------------//

   // This is the active GridTransform object which is currently
   // applied to the data and used to create CMP gathers.

public:    // get values

  double getXorigin       ()  const;
  double getYorigin       ()  const;
  double getRotationAngle ()  const;
  double getXgridWidth    ()  const;
  double getYgridWidth    ()  const;
  int    isRightHanded    ()  const;
  int    isLeftHanded     ()  const;

  double getCosineAngle   ()  const;
  double getSineAngle     ()  const;
  double getDx11          ()  const;
  double getDx12          ()  const;
  double getDx21          ()  const;
  double getDx22          ()  const;
  double getDn11          ()  const;
  double getDn12          ()  const;
  double getDn21          ()  const;
  double getDn22          ()  const;
  double getDeterminant   ()  const;

public:    // get transformed coordinates.

  double getXlocCoord  (double xgrid, double ygrid)  const;
  double getYlocCoord  (double xgrid, double ygrid)  const;
  double getXgridCoord (double xloc , double yloc )  const;
  double getYgridCoord (double xloc , double yloc )  const;

public:  // set values.
         // other GridTransform functions must be called on the
         //   testing GridTransform object, and then passed to
         //   this GridTransform with setGridTransformValues.
         // incrementGridCoords also sets the testing transform.

  void setGridTransformValues ();
  void incrementGridCoords    (double xstep, double ystep);


//-------------------- testing transform -----------------------//
//-------------------- testing transform -----------------------//
//-------------------- testing transform -----------------------//

   // This is a GridTransform object used for testing purposes.
   // It is NOT the one which is currently applied to the data
   // or used to create CMP gathers.

public:    // get values

  double getTestingXorigin       ()  const;
  double getTestingYorigin       ()  const;
  double getTestingRotationAngle ()  const;
  double getTestingXgridWidth    ()  const;
  double getTestingYgridWidth    ()  const;
  int    isTestingRightHanded    ()  const;
  int    isTestingLeftHanded     ()  const;

  double getTestingCosineAngle   ()  const;
  double getTestingSineAngle     ()  const;
  double getTestingDx11          ()  const;
  double getTestingDx12          ()  const;
  double getTestingDx21          ()  const;
  double getTestingDx22          ()  const;
  double getTestingDn11          ()  const;
  double getTestingDn12          ()  const;
  double getTestingDn21          ()  const;
  double getTestingDn22          ()  const;
  double getTestingDeterminant   ()  const;

  void   getTestingGridTransformValues (GridTransform *transform)  const;

  class FgTraceValues *getFgTraceValues()   { return _tv;       }  /* ehs */
  class FgInformer    *getFgInformer   ()   { return _informer; }  /* ehs */
  class ZtCards       *getZtCards      ()   { return _zt_cards; }  /* ehs */
  class FgTraces      *getFgTraces     ()   { return _traces;   }  /* ehs */

  int   updatingMidpointGathers();	/* ehs */
  int   getDeadCode(int itrace);	/* ehs */

public:    // get transformed coordinates.

  double getTestingXlocCoord  (double xgrid, double ygrid)  const;
  double getTestingYlocCoord  (double xgrid, double ygrid)  const;
  double getTestingXgridCoord (double xloc , double yloc )  const;
  double getTestingYgridCoord (double xloc , double yloc )  const;

public:  // set values.
         // resetTestingGridTransformValues resets all values from
         // those in the active GridTransform object.

  void resetTestingGridTransformValues ();

public:    // set values.

  void setTestingXorigin       (double xorigin);
  void setTestingYorigin       (double yorigin);
  void setTestingRotationAngle (double angle);
  void setTestingXgridWidth    (double xwidth);
  void setTestingYgridWidth    (double ywidth);
  void setTestingRightHanded   ();
  void setTestingLeftHanded    ();

  void setTestingRightHandedTransform (double xorigin, double yorigin,
                          double angle, double xwidth, double ywidth);
  void setTestingLeftHandedTransform  (double xorigin, double yorigin,
                          double angle, double xwidth, double ywidth);

  void setTestingDx11 (double dx11);
  void setTestingDx21 (double dx21);
  void setTestingDx12 (double dx12);
  void setTestingDx22 (double dx22);
  void setTestingDn11 (double dn11);
  void setTestingDn21 (double dn21);
  void setTestingDn12 (double dn12);
  void setTestingDn22 (double dn22);

  void setTestingForwardRotationMatrix
                (double dx11, double dx12, double dx21, double dx22);
  void setTestingReverseRotationMatrix
                (double dn11, double dn12, double dn21, double dn22);

  void setTestingGridTransformValues (const GridTransform *transform);

public:     // useful functions to define coordinate system.

  void defineTestingOrigin         (double xgrid, double ygrid,
                                    double xloc , double yloc );
  void defineTestingRotationAngle  (double xloc1, double yloc1,
                                    double xloc2, double yloc2);
  void defineTestingOriginAndAngle (double xgrid, double ygrid,
                                    double xloc1, double yloc1,
                                    double xloc2, double yloc2);
  void refineTestingBinCenter      (double xloc , double yloc );
  void refineTestingRotationAngle  (double xloc , double yloc );


//-------------- miscellaneous seismic survey functions -------------//
//-------------- miscellaneous seismic survey functions -------------//
//-------------- miscellaneous seismic survey functions -------------//

public:       // get limits in survey.

  long   numLines                       ()  const;
  long   totNumFlags                    ()  const;
  long   totNumSources                  ()  const;
  long   totNumReceivers                ()  const;

  int   deadSourceCodesAreSet           ()  const;
  int   deadReceiverCodesAreSet         ()  const;
  long  totNumDeadSources               ()  const;
  long  totNumDeadReceivers             ()  const;
  long  totNumReversedSources           ()  const;
  long  totNumReversedReceivers         ()  const;
  long  totNumMissingSources            ()  const;
  long  totNumMissingReceivers          ()  const;
  long  totNumLiveSources               ()  const;
  long  totNumLiveReceivers             ()  const;

  double minimumXlocInSurvey            ()  const;
  double maximumXlocInSurvey            ()  const;
  double minimumYlocInSurvey            ()  const;
  double maximumYlocInSurvey            ()  const;
  double minimumXgridInSurvey           ()  const;
  double maximumXgridInSurvey           ()  const;
  double minimumYgridInSurvey           ()  const;
  double maximumYgridInSurvey           ()  const;

  long   getFirstLineNumber             ()  const;
  long   getLastLineNumber              ()  const;
  long   getSmallestLineNumber          ()  const;
  long   getLargestLineNumber           ()  const;
  long   getMinLineIncrement            ()  const;
  long   getMaxLineIncrement            ()  const;

public:       // chaining parameter.

  int    allowChangeChaining            ()              const;
  int    getChaining                    ()              const;
  void   setChaining                    (int chaining);

public:       // sorting seismic lines.

  int    linesAreDuplicatedOrNotSorted  ()              const;
  void   sortByLineNumber               ();

public:   // search among seismic lines.
          // these return a seismic line index (ixl), or -1 if not found.

  long  findNearestLineNumber       (long line_number, int dir = 0) const;
  long  findMatchingLineNumber      (long line_number)              const;
  long  findNearestLine             (double xloc, double yloc)      const;
  long  findNearestLineWithSource   (double xloc, double yloc)      const;
  long  findNearestLineWithReceiver (double xloc, double yloc)      const;


//------------------- get and set seismic line values -----------------//
//------------------- get and set seismic line values -----------------//
//------------------- get and set seismic line values -----------------//

                // ixl = index of desired line.

public:     // get limits on seismic line.

  long   numFlagsOnLine                 (long ixl)  const;
  long   numSourcesOnLine               (long ixl)  const;
  long   numReceiversOnLine             (long ixl)  const;

  double minimumXlocOnLine              (long ixl)  const;
  double maximumXlocOnLine              (long ixl)  const;
  double minimumYlocOnLine              (long ixl)  const;
  double maximumYlocOnLine              (long ixl)  const;
  double minimumXgridOnLine             (long ixl)  const;
  double maximumXgridOnLine             (long ixl)  const;
  double minimumYgridOnLine             (long ixl)  const;
  double maximumYgridOnLine             (long ixl)  const;

  float  getFirstShotpointOnLine        (long ixl)  const;
  float  getLastShotpointOnLine         (long ixl)  const;
  float  getSmallestShotpointOnLine     (long ixl)  const;
  float  getLargestShotpointOnLine      (long ixl)  const;
  float  getMinShotpointIncrOnLine      (long ixl)  const;
  float  getMaxShotpointIncrOnLine      (long ixl)  const;

public:     // get and set seismic line values.

  long   getLineNumber                      (long ixl)  const;
  long   firstCumulativeGroundPosition      (long ixl)  const;
  long   firstMatchableGroundPosition       (long ixl)  const;
  long   firstGroundPosition                (long ixl)  const;
  int    shotpointsAreDuplicatedOrNotSorted (long ixl)  const;
  double distanceToLine        (long ixl, double xloc, double yloc)  const;
  double distanceSquaredToLine (long ixl, double xloc, double yloc)  const;
  void   setLineNumber         (long ixl, long line_number);

public:    // reverse direction of seismic line.

  int    allowReverseLineDirections ()         const;
  void   reverseLineDirection       (long ixl);

public:    // search along seismic line.
           // these return a field flag index (ixf), or -1 if not found.

  long findNearestShotpointOnLine  (long ixl, float shotpoint,int dir=0)const;
  long findMatchingShotpointOnLine (long ixl, float shotpoint)          const;
  long findNearestFlagOnLine       (long ixl, double xloc, double yloc) const;
  long findNearestSourceOnLine     (long ixl, double xloc, double yloc) const;
  long findNearestReceiverOnLine   (long ixl, double xloc, double yloc) const;

public:    // find location of specified ground position.

       // these return a field flag index (ixf), or -1 if not found.
       // the first function also returns ixl, or -1 if not found.
       // there may be   one    matching matchable_gp on each line.
       // there may be only one matching cumulative_gp in survey.
       // the last function assumes gp == cumulative_gp when fixdist <= 0.
       // the last function assumes gp == matchable_gp  when fixdist >  0.

  void   findCumulativeGroundPosition (long cumulative_gp,
                                               long *ixl, long *ixf)  const;
  long   findCumulativeGroundPosition (long ixl, long cumulative_gp)  const;
  long   findMatchableGroundPosition  (long ixl, long matchable_gp)   const;
  long   findGroundPosition           (long ixl, long gp)             const;


//-------------------- get and set field flag values -----------------//
//-------------------- get and set field flag values -----------------//
//-------------------- get and set field flag values -----------------//

   // ixl  = index of desired line.
   // ixf  = index of desired flag on desired line.
   // ixs2 = index of desired source shot at desired flag on desired line.
   // ixr2 = index of desired trace recorded at desired flag on desired line.

public:    // get sources and receivers at a flag.

  long   numSourcesAtFlag      (long ixl, long ixf)             const;
  long   numReceiversAtFlag    (long ixl, long ixf)             const;
  int    flagHasSource         (long ixl, long ixf)             const;
  int    flagHasReceiver       (long ixl, long ixf)             const;
  long   sourceGroupNumber     (long ixl, long ixf, long ixs2)  const;
  long   receiverTraceNumber   (long ixl, long ixf, long ixr2)  const;

public:    // miscellaneous field flag functions.

  class FieldFlag *getFlagPointer (long ixl, long ixf)           const;
  long             getLineIndex   (class FieldFlag *field_flag)  const;
  long             getFlagIndex   (class FieldFlag *field_flag)  const;

  double distanceToFlag
                  (long ixl, long ixf, double xloc, double yloc) const;
  double distanceSquaredToFlag
                  (long ixl, long ixf, double xloc, double yloc) const;

  int    allowSettingIncrDistance       ()           const;
  int    allowSettingXloc               ()           const;
  int    allowSettingXgrid              ()           const;
  int    allowSettingYgrid              ()           const;
  int    allowSettingValue              (int ident)  const;

public:    // get field flag values.

  int    flagValueIsDependent  (long ixl, long ixf, int ident)  const;
  double getFlagValue          (long ixl, long ixf, int ident)  const;

  float  getShotpoint                (long ixl, long ixf)  const;
  double getIncrDistance             (long ixl, long ixf)  const;
  double getXloc                     (long ixl, long ixf)  const;
  double getYloc                     (long ixl, long ixf)  const;
  double getXgrid                    (long ixl, long ixf)  const;
  double getYgrid                    (long ixl, long ixf)  const;
  float  getElevation                (long ixl, long ixf)  const;
  float  getHoleDepth                (long ixl, long ixf)  const;
  float  getUpholeTime               (long ixl, long ixf)  const;
  float  getReceiverStatic           (long ixl, long ixf)  const;
  float  getSourceStatic             (long ixl, long ixf)  const;
  float  getReceiverXskid            (long ixl, long ixf)  const;
  float  getReceiverYskid            (long ixl, long ixf)  const;
  float  getReceiverEskid            (long ixl, long ixf)  const;
  double getCumDistance              (long ixl, long ixf)  const;
  double getAzimuth                  (long ixl, long ixf)  const;
  int    getDeadSourceCode           (long ixl, long ixf)  const;
  int    getDeadReceiverCode         (long ixl, long ixf)  const;
  int    sourceMaybeDead             (long ixl, long ixf)  const;
  int    receiverMaybeDead           (long ixl, long ixf)  const;
  long   getCumulativeGroundPosition (long ixl, long ixf)  const;
  long   getMatchableGroundPosition  (long ixl, long ixf)  const;
  long   getGroundPosition           (long ixl, long ixf)  const;

public:    // set field flag values.

  void   setFlagValue          (long ixl, long ixf, int ident, double value);
  void   setDependentFlagValue (long ixl, long ixf, int ident, double value);

  void   setLocation     (long ixl, long ixf, double xloc, double yloc);
  void   setGridLocation (long ixl, long ixf, double xgrid, double ygrid);

  void   setShotpoint             (long ixl, long ixf, float  value);
  void   setIncrDistance          (long ixl, long ixf, double value);
  void   setXloc                  (long ixl, long ixf, double value);
  void   setYloc                  (long ixl, long ixf, double value);
  void   setXgrid                 (long ixl, long ixf, double value);
  void   setYgrid                 (long ixl, long ixf, double value);
  void   setElevation             (long ixl, long ixf, float  value);
  void   setHoleDepth             (long ixl, long ixf, float  value);
  void   setUpholeTime            (long ixl, long ixf, float  value);
  void   setReceiverStatic        (long ixl, long ixf, float  value);
  void   setSourceStatic          (long ixl, long ixf, float  value);
  void   setReceiverXskid         (long ixl, long ixf, float  value);
  void   setReceiverYskid         (long ixl, long ixf, float  value);
  void   setReceiverEskid         (long ixl, long ixf, float  value);


//-------------------- get skidded coordinates ---------------------//
//-------------------- get skidded coordinates ---------------------//
//-------------------- get skidded coordinates ---------------------//

   // ixl  = index of desired line.
   // ixf  = index of desired flag on desired line.
   // ixs2 = index of desired source shot at desired flag on desired line.
   // ixr2 = index of desired trace recorded at desired flag on desired line.

   // skidded coords          = flag coords + argument skids.
   // skidded coords plus     = flag coords + argument skids
   //                                       + receiver skids on LD card.

   // skidded  source  coords = flag coords +  source  skids on PP card.
   // skidded receiver coords = flag coords + receiver skids on LD card.
   // skidded  trace   coords = flag coords + receiver skids on PP card
   //                                        + channel skids on RP card.

   // source   gathers must have been created when using ixs2.
   // receiver gathers must have been created when using ixr2.

public:

  void   getSkiddedCoords         (long ixl, long ixf,
                           float inline_skid, float crossline_skid,
                           double *x, double *y)  const;
  void   getSkiddedCoordsPlus     (long ixl, long ixf,
                           float inline_skid, float crossline_skid,
                           double *x, double *y)  const;

  void   getSourceSkids(long ixpp, long group,
                        float *inline_skid, float *crossline_skid)  const;
  void   getSourceSkids(long group,
                        float *inline_skid, float *crossline_skid)  const;
  void   getSourceSkids(long ixl, long ixf, long ixs2,
                        float *inline_skid, float *crossline_skid)  const;

  int    receiverIsSkidded (long ixl, long ixf)             const;
  int    sourceIsSkidded   (long ixl, long ixf, long ixs2)  const;
  int    traceIsSkidded    (long ixl, long ixf, long ixr2)  const;

  void   getSkiddedReceiverCoords (long ixl, long ixf,
                                     double *x, double *y)  const;

  void   getSkiddedSourceCoords   (long group,
                                     double *x, double *y)  const;
  void   getSkiddedSourceCoords   (long ixl, long ixf, long ixs2,
                                     double *x, double *y)  const;

  void   getSkiddedTraceCoords    (long group, long channel,
                                     double *x, double *y)  const;
  void   getSkiddedTraceCoords    (long ixl, long ixf, long ixr2,
                                     double *x, double *y)  const;


//-------------- miscellaneous PP card functions ---------------//
//-------------- miscellaneous PP card functions ---------------//
//-------------- miscellaneous PP card functions ---------------//

 // group   = original group number   (starting with 1) in acquisition order.
 // trace   = sequential trace number (starting with 1) in acquisition order.
 // channel = channel number          (starting with 1) in original group.

public:     // miscellaneous.

  long   numPpCards            ()            const;
  long   numGroups             ()            const;
  long   numTraces             ()            const;
  long   groupNumber           (long trace)  const;
  long   channelNumber         (long trace)  const;

public:  // search among PP cards.
         // the first two return a PP card index (ixpp), or -1 if not found.
         // the last two return a number, or 0 if not found.

  long  findPpCardWithDesiredGroup  (long group) const;
  long  findPpCardWithDesiredTrace  (long trace) const;
  long  findNumChannelsInGroup      (long group) const;
  long  findTraceNumber             (long group, long channel) const;
  long  numUnplacedTraces           (long group) const;


//----------------- get and set PP card values -----------------------//
//----------------- get and set PP card values -----------------------//
//----------------- get and set PP card values -----------------------//

public:      // get PP card values

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

public:      // set PP card values

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


//-------------- miscellaneous RP card functions ---------------//
//-------------- miscellaneous RP card functions ---------------//
//-------------- miscellaneous RP card functions ---------------//

public:  // miscellaneous.
         // the last two functions return an index (ixrp), or -1 if not found.
         // the ixrp argument should be the first RP card of the
         //   desired pattern (as returned by findReceiverPattern).

  int   receiverPatternsAreSorted ()    const;
  void  sortReceiverPatterns      ();

  long  getEndOfReceiverPattern      (long ixrp)  const;
  long  getRpCardWithDesiredChannel  (long ixrp, long channel)  const;
  long  numChannelsInPattern         (long pattern)  const;  // search.

public:  // search among RP cards.
         // these return an RP card index (ixrp), or -1 if not found.

  long  findReceiverPattern          (long pattern)  const;
  long  findEndOfReceiverPattern     (long pattern)  const;
  long  findRpCardWithDesiredChannel (long pattern, long channel)  const;


//------------------- get and set RP card values ---------------------//
//------------------- get and set RP card values ---------------------//
//------------------- get and set RP card values ---------------------//

public:      // get RP card values

  long   numRpCards         ()           const;
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

  void   getRpIncrements    (long ixrp, long channel,
                             long *xinc,  long *yinc,
                             long *incrx, long *incry)  const;

public:      // set RP card values

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


//----------------------- selected items ----------------------//
//----------------------- selected items ----------------------//
//----------------------- selected items ----------------------//

    // also can use ident FG_SEL in getFlagValue and setFlagValue.

public:

  long   numSelectedLines         ()                       const;
  long   numSelectedFlagsOnLine   (long ixl)               const;
  long   numSelectedCmpGathers    ()                       const;

  int    flagIsSelected           (long ixl, long ixf)     const;
  int    lineIsSelected           (long ixl)               const;
  int    cmpIsSelected            (long ixcmp)             const;
  int    cmpFattestBinette        (long ixcmp)             const;

  char   getLineSelectValue       (long ixl)               const;
  char   getFlagSelectValue       (long ixl, long ixf)     const;
  char   getCmpSelectValue        (long ixcmp)             const;

  void   setLineSelectValue       (long ixl,           char value);
  void   setFlagSelectValue       (long ixl, long ixf, char value);
  void   setCmpSelectValue        (long ixcmp,         char value);

  void   incrementLineSelectValue (long ixl);
  void   incrementFlagSelectValue (long ixl, long ixf);
  void   incrementCmpSelectValue  (long ixcmp);

  void   clearLineSelections      ();
  void   clearFlagSelections      (long ixl);
  void   clearCmpSelections       ();


//------------------------ active items -----------------------//
//------------------------ active items -----------------------//
//------------------------ active items -----------------------//

public:

  long   getActiveLineIndex        ()          const;
  long   getActiveFlagIndexOnLine  (long ixl)  const;
  long   getActiveRpCardIndex      ()          const;
  long   getActivePpCardIndex      ()          const;
  long   getActiveZt1CardIndex     ()          const;
  long   getActiveZt2CardIndex     ()          const;
  long   getActiveZt3CardIndex     ()          const;
  long   getActiveZt4CardIndex     ()          const;
  long   getActiveCmpIndex         ()          const;
  long   getActiveTraceNumber      ()          const;
  long   getActiveGroupNumber      ()          const;

  long   getActiveLineNumber       ()          const;
  float  getActiveShotpointOnLine  (long ixl)  const;

  void   setActiveLineIndex        (long ixl);
  void   setActiveFlagIndexOnLine  (long ixl, long ixf);
  void   setActiveRpCardIndex      (long ixrp);
  void   setActivePpCardIndex      (long ixpp);
  void   setActiveZt1CardIndex     (long ixzt1);
  void   setActiveZt2CardIndex     (long ixzt2);
  void   setActiveZt3CardIndex     (long ixzt3);
  void   setActiveZt4CardIndex     (long ixzt4);
  void   setActiveCmpIndex         (long ixcmp);
  void   setActiveTraceNumber      (long trace);
  void   setActiveGroupNumber      (long group);

  void   setActiveLineNumber       (long line_number);           // search
  void   setActiveShotpointOnLine  (long ixl, float shotpoint);  // search

  void   setActiveIndices          (long ixl, long ixf);
  void   setActiveSourceIndices    (long ixl, long ixf, long ixs2 = 0);
  void   setActiveReceiverIndices  (long ixl, long ixf, long ixr2 = 0);
  void   setActiveSourceIndices    (long trace);
  void   setActiveReceiverIndices  (long trace);
  void   setActiveGroupNumberPlus  (long group);


//------------------ insert or remove cards --------------------//
//------------------ insert or remove cards --------------------//
//------------------ insert or remove cards --------------------//

public:  // the non-void functions return index where action occurred.
         // the non-void functions return -1 if failed.

  long   appendNewFlagToLine           (long ixl);
  long   appendNewLine                 ();
  long   appendNewRpCard               ();
  long   appendNewPpCard               ();
  long   appendNewZt1Card              ();
  long   appendNewZt2Card              ();
  long   appendNewZt3Card              ();
  long   appendNewZt4Card              ();

  long   insertNewFlagOnLine           (long ixl, long ixf);
  long   insertNewLine                 (long ixl);
  long   insertNewRpCard               (long ixrp);
  long   insertNewPpCard               (long ixpp);
  long   insertNewZt1Card              (long ixzt1);
  long   insertNewZt2Card              (long ixzt2);
  long   insertNewZt3Card              (long ixzt3);
  long   insertNewZt4Card              (long ixzt4);

  long   placeNewFlagOnLine   (long ixl, float shotpoint);  // search
  long   placeNewLine                  (long line_number);  // search
  long   placeNewRpCard                (long pattern);      // search

  long   insertNewFlagOnLineFromBuffer (long ixl, long ixf);
  long   insertNewLineFromBuffer       (long ixl);
  long   insertNewRpCardFromBuffer     (long ixrp);
  long   insertNewPpCardFromBuffer     (long ixpp);
  long   insertNewZt1CardFromBuffer    (long ixzt1);
  long   insertNewZt2CardFromBuffer    (long ixzt2);
  long   insertNewZt3CardFromBuffer    (long ixzt3);
  long   insertNewZt4CardFromBuffer    (long ixzt4);

  long   deleteFlagFromLine            (long ixl, long ixf);
  long   deleteLine                    (long ixl);
  long   deleteRpCard                  (long ixrp);
  long   deletePpCard                  (long ixpp);
  long   deleteZt1Card                 (long ixzt1);
  long   deleteZt2Card                 (long ixzt2);
  long   deleteZt3Card                 (long ixzt3);
  long   deleteZt4Card                 (long ixzt4);

  long   deleteFlagFromLineToBuffer    (long ixl, long ixf);
  long   deleteLineToBuffer            (long ixl);
  long   deleteRpCardToBuffer          (long ixrp);
  long   deletePpCardToBuffer          (long ixpp);
  long   deleteZt1CardToBuffer         (long ixzt1);
  long   deleteZt2CardToBuffer         (long ixzt2);
  long   deleteZt3CardToBuffer         (long ixzt3);
  long   deleteZt4CardToBuffer         (long ixzt4);

  void   deleteAllFlagsFromLine        (long ixl);
  void   deleteAllLines                ();
  void   deleteAllRpCards              ();
  void   deleteAllPpCards              ();
  void   deleteAllZt1Cards             ();
  void   deleteAllZt2Cards             ();
  void   deleteAllZt3Cards             ();
  void   deleteAllZt4Cards             ();


//----------------- allocate and free space for cards --------------//
//----------------- allocate and free space for cards --------------//
//----------------- allocate and free space for cards --------------//

public:    //  optional usage.

  void  allocateSpaceForLine     (long ixl, long nadd);
  void  allocateSpaceForLines              (long nadd);
  void  allocateSpaceForRpCards            (long nadd);
  void  allocateSpaceForPpCards            (long nadd);
  void  allocateSpaceForZt1Cards           (long nadd);
  void  allocateSpaceForZt2Cards           (long nadd);
  void  allocateSpaceForZt3Cards           (long nadd);
  void  allocateSpaceForZt4Cards           (long nadd);

  void  freeSpaceForLine         (long ixl);
  void  freeSpaceForLines        ();
  void  freeSpaceForRpCards      ();
  void  freeSpaceForPpCards      ();
  void  freeSpaceForZt1Cards     ();
  void  freeSpaceForZt2Cards     ();
  void  freeSpaceForZt3Cards     ();
  void  freeSpaceForZt4Cards     ();


//------------------ get and set ZT card values --------------------//
//------------------ get and set ZT card values --------------------//
//------------------ get and set ZT card values --------------------//

                // ixzt1 = index of ZT1 card.
                // ixzt2 = index of ZT2 card.
                // ixzt3 = index of ZT3 card.
                // ixzt4 = index of ZT4 card.

public:      // number of ZT cards.

  long   numZt1Cards               ()            const;
  long   numZt2Cards               ()            const;
  long   numZt3Cards               ()            const;
  long   numZt4Cards               ()            const;

public:      // ZT1 card values

  int    getZt1Code                (long ixzt1)  const;
  float  getZt1FromSourceShotpoint (long ixzt1)  const;
  float  getZt1ToSourceShotpoint   (long ixzt1)  const;
  long   getZt1SourceLineNumber    (long ixzt1)  const;

  void   setZt1Code                (long ixzt1, int   value);
  void   setZt1FromSourceShotpoint (long ixzt1, float value);
  void   setZt1ToSourceShotpoint   (long ixzt1, float value);
  void   setZt1SourceLineNumber    (long ixzt1, long  value);

public:      // ZT2 card values

  int    getZt2Code                  (long ixzt2)  const;
  float  getZt2FromReceiverShotpoint (long ixzt2)  const;
  float  getZt2ToReceiverShotpoint   (long ixzt2)  const;
  long   getZt2ReceiverLineNumber    (long ixzt2)  const;

  void   setZt2Code                  (long ixzt2, int   value);
  void   setZt2FromReceiverShotpoint (long ixzt2, float value);
  void   setZt2ToReceiverShotpoint   (long ixzt2, float value);
  void   setZt2ReceiverLineNumber    (long ixzt2, long  value);

public:      // ZT3 card values

  int    getZt3Code                (long ixzt3)  const;
  long   getZt3FromGroupNumber     (long ixzt3)  const;
  long   getZt3ToGroupNumber       (long ixzt3)  const;
  long   getZt3FromTraceNumber     (long ixzt3)  const;
  long   getZt3ToTraceNumber       (long ixzt3)  const;
  int    groupPartlyDead           (long group)  const;

  void   setZt3Code                (long ixzt3, int   value);
  void   setZt3FromGroupNumber     (long ixzt3, long  value);
  void   setZt3ToGroupNumber       (long ixzt3, long  value);
  void   setZt3FromTraceNumber     (long ixzt3, long  value);
  void   setZt3ToTraceNumber       (long ixzt3, long  value);

public:      // ZT4 card values

  int    getZt4Code                  (long ixzt4)  const;
  float  getZt4FromSourceShotpoint   (long ixzt4)  const;
  float  getZt4ToSourceShotpoint     (long ixzt4)  const;
  long   getZt4SourceLineNumber      (long ixzt4)  const;
  float  getZt4FromReceiverShotpoint (long ixzt4)  const;
  float  getZt4ToReceiverShotpoint   (long ixzt4)  const;
  long   getZt4ReceiverLineNumber    (long ixzt4)  const;

  void   setZt4Code                  (long ixzt4, int   value);
  void   setZt4FromSourceShotpoint   (long ixzt4, float value);
  void   setZt4ToSourceShotpoint     (long ixzt4, float value);
  void   setZt4SourceLineNumber      (long ixzt4, long  value);
  void   setZt4FromReceiverShotpoint (long ixzt4, float value);
  void   setZt4ToReceiverShotpoint   (long ixzt4, float value);
  void   setZt4ReceiverLineNumber    (long ixzt4, long  value);


//------------------- convenience functions ----------------------//
//------------------- convenience functions ----------------------//
//------------------- convenience functions ----------------------//

public:

  float  defaultSourceDatumStatic   (long ixl, long ixf)  const;
  float  defaultReceiverDatumStatic (long ixl, long ixf)  const;

public:   // these return zero if requested item not found.

  long   numSourcesAtActiveFlag             ()           const;
  long   numReceiversAtActiveFlag           ()           const;
  long   sourceGroupNumberAtActiveFlag      (long ixs2)  const;
  long   receiverTraceNumberAtActiveFlag    (long ixr2)  const;

public:

  float  getSourceElevation   (long group)  const;
  float  getSourceHoleDepth   (long group)  const;
  float  getSourceUpholeTime  (long group)  const;
  float  getSourceDatumStatic (long group)  const;


//-------------------- trace gathers ---------------------------------//
//-------------------- trace gathers ---------------------------------//
//-------------------- trace gathers ---------------------------------//

// ixl      = index of seismic line  in an entire seismic survey.
// ixrp     = index of RP card       in an array of receiver pattern cards.
// ixorig   = index of trace         among all traces in acquisition order.
// ixsorted = index of trace         among all traces in CMP-sorted order.
// ixcmp    = index of CMP gather    in an array of all CMP gathers.
// ixfold   = index of trace in CMP  in an array of traces in a CMP gather.

public:    // set values
           // the returned int is 0 if CMP gathers have been created,
           //   but equal to the potential number of CMP gathers if
           //   there will be too many (in which case they were not
           //   created).

  void updateSourceGathers();
  void updateReceiverGathers();
  long updateMidpointGathers();
  void updateLiveFold();

public:    // get values

  int    sourceGathersOutOfDate     ()  const;   // TRUE/FALSE
  int    receiverGathersOutOfDate   ()  const;   // TRUE/FALSE
  int    midpointGathersOutOfDate   ()  const;   // TRUE/FALSE
  int    liveFoldOutOfDate          ()  const;   // TRUE/FALSE

  int    sourceGathersIncomplete    ()  const;   // TRUE/FALSE
  int    receiverGathersIncomplete  ()  const;   // TRUE/FALSE
  int    midpointGathersIncomplete  ()  const;   // TRUE/FALSE

  long   numCmpGathers              ()             const;
  long   numXbins                   ()             const;
  long   numYbins                   ()             const;
  long   numUnplacedSources         ()             const;
  long   numUnplacedTraces          ()             const;
  long   foldOfStack                (long ixcmp)   const;
  long   liveFoldOfStack            (long ixcmp)   const;
  long   headerWord3                (long ixcmp)   const;

  int    firstBinHasNilCoords       ()             const;  // TRUE/FALSE
  long   getMinimumFold             ()             const;
  long   getMaximumFold             ()             const;
  double getMinimumXlocBinCenter    ()             const;
  double getMinimumYlocBinCenter    ()             const;
  double getMaximumXlocBinCenter    ()             const;
  double getMaximumYlocBinCenter    ()             const;
  float  getMinimumOffset           ()             const;
  float  getMaximumOffset           ()             const;
  double getMinimumXgridBinCenter   ()             const;
  double getMinimumYgridBinCenter   ()             const;
  double getMaximumXgridBinCenter   ()             const;
  double getMaximumYgridBinCenter   ()             const;

  long   originalTraceIndex       (long ixcmp, long ixfold)  const;
  long   originalTraceIndex       (long ixsorted)            const;

  long   findNearestCmp           (double xloc , double yloc )  const;
  long   getMatchingCmp           (double xloc , double yloc )  const;
  long   findNearestCmpUsingGrid  (double xgrid, double ygrid)  const;
  long   getMatchingCmpUsingGrid  (double xgrid, double ygrid)  const;

  void   getCmpLocBinCenter  (long ixcmp, double *xloc , double *yloc ) const;
  void   getCmpGridBinCenter (long ixcmp, double *xgrid, double *ygrid) const;

  void   getCmpTraceLoc
                (long ixcmp, long ixfold, double *xloc , double *yloc ) const;
  void   getCmpTraceGrid
                (long ixcmp, long ixfold, double *xgrid, double *ygrid) const;
  float  getCmpTraceOffset  (long ixcmp, long ixfold) const;
  int    getCmpTraceBinette (long ixcmp, long ixfold) const;

  long   getSourceLineIndex     (long group)                const;  // ixl
  long   getSourceFlagIndex     (long group)                const;  // ixf
  long   getReceiverLineIndex   (long group, long channel)  const;  // ixl
  long   getReceiverFlagIndex   (long group, long channel)  const;  // ixf


//----------------------- trace headers -------------------------//
//----------------------- trace headers -------------------------//
//----------------------- trace headers -------------------------//

   // trace = sequential trace number (starting with 1) in acquisition order.
   // ihead = header word number      (starting with 1).

public:   // set and get FGD variables.

  void   setVe        (float value);
  void   setRef       (float value);
  void   setFixdist   (float value);
  void   setNdpt      (long  value);
  float  getVe        ()             const;
  float  getRef       ()             const;
  float  getFixdist   ()             const;
  long   getNdpt      ()             const;

public:  // calculate header words for one trace.
         // returns error flag (TRUE if error, FALSE if no error).
         // more = TRUE  to calculate all 64 header words.
         // more = FALSE to calculate only some of the header words.

  void   startHeadersFromScratch () {}  //////// no longer needed.
  int    calculateHeaderWords    (long trace, int more);

public:  // get information calculated above.
         // all but the last must follow call to calculateHeaderWords.
         // ihead = header word number (1-64).
         // getHeaderErrorFlag returns the same thing as calculateHeaderWords.
         // getHeaderTraceNumber returns the <trace> argument.
         // an unknown index will be returned as -1.
         // an unknown header word will be returned as DNIL.

  int         getHeaderErrorFlag         ()           const;
  long        getHeaderTraceNumber       ()           const;  // trace
  long        getHeaderSourceLineIndex   ()           const;  // ixl
  long        getHeaderSourceFlagIndex   ()           const;  // ixf
  long        getHeaderReceiverLineIndex ()           const;  // ixl
  long        getHeaderReceiverFlagIndex ()           const;  // ixf
  long        getHeaderRpCardIndex       ()           const;  // ixrp
  long        getHeaderPpCardIndex       ()           const;  // ixpp
  long        getHeaderCmpIndex          ()           const;  // ixcmp
  double      getHeaderWordValue         (int ihead)  const;
  const char *getHeaderWordDescription   (int ihead)  const;


//---------------------- end of functions -----------------------//
//---------------------- end of functions -----------------------//
//---------------------- end of functions -----------------------//

} ;

#endif

//---------------------------- end --------------------------------//
//---------------------------- end --------------------------------//
//---------------------------- end --------------------------------//
