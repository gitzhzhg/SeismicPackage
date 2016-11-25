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

//---------------------- seis_survey.hh ---------------------//
//---------------------- seis_survey.hh ---------------------//
//---------------------- seis_survey.hh ---------------------//

//               header file for the SeisSurvey class
//                derived from the SmartArray class
//                       subdirectory geom 

           // This class is an array of seismic lines.


#ifndef _SEIS_SURVEY_HH_
#define _SEIS_SURVEY_HH_

#include "oprim/smart_array.hh"


class SeisSurvey  :  public SmartArray
{

//----------------------- data ------------------------------//
//----------------------- data ------------------------------//
//----------------------- data ------------------------------//

private:

  int  _chaining;             // chaining parameter.
  int  _frozen;               // whether updates are frozen.
  long _tot_num_sources;      // total number of all  sources  in survey.
  long _tot_num_receivers;    // total number of all receivers in survey.
  long _tot_num_flags;        // total number of all   flags   in survey.

  int    _dead_source_codes_set;    // whether dead source codes are set.
  int    _dead_receiver_codes_set;  // whether dead receiver codes are set.
  long   _num_dead_sources;
  long   _num_dead_receivers;
  long   _num_reversed_sources;
  long   _num_reversed_receivers;
  long   _num_missing_sources;
  long   _num_missing_receivers;
  long   _num_live_sources;
  long   _num_live_receivers;

  class FgInformer    *_informer;       // pointer to informer object.
  class FgConnect     *_connect;        // pointer to connections object.
  class AccBase       *_acc_index;      // pointer to array column control.
  class AccBase       *_acc_cum_flag;   // pointer to array column control.
  class AccBase       *_acc_match_flag; // pointer to array column control.
  class AccSelect     *_acc_select;     // pointer to array column control.
  class AccSearch     *_search;         // pointer to search object.
  class FastSort      *_sort;           // pointer to fast sort object.


//------------------------ functions ------------------------//
//------------------------ functions ------------------------//
//------------------------ functions ------------------------//

public:    // constructor and destructor

  SeisSurvey (FgInformer *informer, FgConnect *connect);
  virtual ~SeisSurvey();

/*
  class SeisLine *unsafeSeisLine (long ixl)  const
                     { return ((SeisLine**)_array)[ixl]; }
*/

  class SeisLine *unsafeSeisLine (long ixl)  const
                           { return (SeisLine*)unsafeArrayElement(ixl); }

  class SeisLine *seisLine(long ixl)  const
                           { return (SeisLine*)arrayElement(ixl); }

public:   // virtual functions overriding SmartArray.

  virtual void valuesWillChange  (int ident, long index, long nrem, long nins);
  virtual void valuesHaveChanged (int ident, long index, long nrem, long nins);

private:    // virtual functions overriding SmartArray.

  virtual void  beforeRemoveInsert    (long index, long nrem, long nins);
  virtual void  afterRemoveInsert     (long index, long nrem, long nins);
  virtual void  beforeNewActiveIndex  ();
  virtual void  afterNewActiveIndex   ();
  virtual void *doCreateObject        ();
  virtual void  doDeleteObject        (void *object);
  virtual void  objectWillBeRemoved   (long ixl);
  virtual void  objectHasBeenInserted (long ixl);

private:     

  class SeisLine *seisLineInBuffer()  const
                           { return (SeisLine*)bufferElement(); }
                 ///       { return (SeisLine*)fetchElementFromBuffer(); }

  void resetRequiredLineIndices (long starting_index);
  void findHelper        (long ixl, double xloc, double yloc,
               long *nearest_index, double *nearest_distance2) const;
  void updateTotalsWhenRemovingFlag  (long ixl, long ixf);
  void updateTotalsWhenInsertingFlag (long ixl, long ixf);


//----------------- public access to this survey --------------------//
//----------------- public access to this survey --------------------//
//----------------- public access to this survey --------------------//

                     // ixl = index of desired line.

public:       // get values

  int    getChaining                   ()  const  { return _chaining; }
  int    allowSettingIncrDistance      ()  const;
  int    allowSettingXloc              ()  const;
  int    allowChangeChaining           ()  const;
  int    allowReverseLineDirections    ()  const;
  int    linesAreDuplicatedOrNotSorted ()  const;
  double minimumXlocInSurvey           ()  const;
  double maximumXlocInSurvey           ()  const;
  double minimumYlocInSurvey           ()  const;
  double maximumYlocInSurvey           ()  const;

  long numLines              ()  const  { return numElements(); }
  long numSelectedLines      ()  const;
  long totNumSources         ()  const  { return _tot_num_sources; }
  long totNumReceivers       ()  const  { return _tot_num_receivers; }
  long totNumFlags           ()  const  { return _tot_num_flags; }
  long getActiveLineIndex    ()  const  { return getActiveIndex(); }
  long getActiveLineNumber   ()  const;
  long getSmallestLineNumber ()  const;
  long getLargestLineNumber  ()  const;
  long getFirstLineNumber    ()  const;
  long getLastLineNumber     ()  const;
  long getMinLineIncrement   ()  const;
  long getMaxLineIncrement   ()  const;

  int   deadSourceCodesAreSet   () const { return _dead_source_codes_set; }
  int   deadReceiverCodesAreSet () const { return _dead_receiver_codes_set; }
  long  totNumDeadSources       () const { return _num_dead_sources; }
  long  totNumDeadReceivers     () const { return _num_dead_receivers; }
  long  totNumReversedSources   () const { return _num_reversed_sources; }
  long  totNumReversedReceivers () const { return _num_reversed_receivers; }
  long  totNumMissingSources    () const { return _num_missing_sources; }
  long  totNumMissingReceivers  () const { return _num_missing_receivers; }
  long  totNumLiveSources       () const { return _num_live_sources; }
  long  totNumLiveReceivers     () const { return _num_live_receivers; }

public:       // set values

  void   setChaining            (int chaining);
  void   setActiveLineIndex     (long index)  { setActiveIndex(index); }
  void   setActiveLineNumber    (long active_line_number);     // search
  void   sortByLineNumber       ();
  void   freezeDependentUpdates ();   // all lines
  void   resumeDependentUpdates ();   // all lines

  void   setDeadSourceCodes     ();
  void   setDeadReceiverCodes   ();
  void clearDeadSourceCodes     ();
  void clearDeadReceiverCodes   ();

public:  // insert or remove lines.
         // the non-void functions return index (ixl) where action occurred.
         // the non-void functions return -1 if failed.

  long       appendNewLine           ();
  long       placeNewLine            (long line_number);
  long       insertNewLine           (long ixl);
  long       insertNewLineFromBuffer (long ixl);
  long       deleteLine              (long ixl);
  long       deleteLineToBuffer      (long ixl);
  void       deleteAllLines          ();

public:    // allocate and free space for seismic lines (optional usage).

  void  allocateSpaceForLines (long nadd) { allocateSpace(nadd); }
  void  freeSpaceForLines     ()          { freeSpace    ();     }

public:    // search among seismic lines.
           // these return an index (ixl), or -1 if not found.

  long  findNearestLineNumber       (long line_number, int dir = 0) const;
  long  findMatchingLineNumber      (long line_number)              const;
  long  findNearestLine             (double xloc, double yloc)      const;
  long  findNearestLineWithSource   (double xloc, double yloc)      const;
  long  findNearestLineWithReceiver (double xloc, double yloc)      const;


//------------- pass-thru functions to individual lines ---------------//
//------------- pass-thru functions to individual lines ---------------//
//------------- pass-thru functions to individual lines ---------------//

                // ixl = index of desired line.
                // ixf = index of desired flag on desired line.

public:     // get seismic line values

  long   getLineNumber                      (long ixl)  const;
  int    lineIsSelected                     (long ixl)  const;
  char   getLineSelectValue                 (long ixl)  const;
  long   numSourcesOnLine                   (long ixl)  const;
  long   numReceiversOnLine                 (long ixl)  const;
  long   numFlagsOnLine                     (long ixl)  const;
  long   numSelectedFlagsOnLine             (long ixl)  const;
  long   firstCumulativeGroundPosition      (long ixl)  const;
  long   firstMatchableGroundPosition       (long ixl)  const;
  long   getActiveFlagIndexOnLine           (long ixl)  const;
  float  getActiveShotpointOnLine           (long ixl)  const;
  float  getFirstShotpointOnLine            (long ixl)  const;
  float  getLastShotpointOnLine             (long ixl)  const;
  float  getSmallestShotpointOnLine         (long ixl)  const;
  float  getLargestShotpointOnLine          (long ixl)  const;
  float  getMinShotpointIncrOnLine          (long ixl)  const;
  float  getMaxShotpointIncrOnLine          (long ixl)  const;
  int    shotpointsAreDuplicatedOrNotSorted (long ixl)  const;
  double minimumXlocOnLine                  (long ixl)  const;
  double maximumXlocOnLine                  (long ixl)  const;
  double minimumYlocOnLine                  (long ixl)  const;
  double maximumYlocOnLine                  (long ixl)  const;
  double distanceToLine        (long ixl, double xloc, double yloc)  const;
  double distanceSquaredToLine (long ixl, double xloc, double yloc)  const;

public:     // set seismic line values

  void   setLineNumber                (long ixl, long line_number);
  void   setActiveFlagIndexOnLine     (long ixl, long index);
  void   setActiveShotpointOnLine     (long ixl, float shotpoint);  // search
  void   reverseLineDirection         (long ixl);
  void   setLineSelectValue           (long ixl, char value);
  void   incrementLineSelectValue     (long ixl);
  void   clearLineSelections          ();

public:  // insert or remove flags.
         // the non-void functions return index (ixf) where action occurred.
         // the non-void functions return -1 if failed.

  long   appendNewFlagToLine           (long ixl);
  long   placeNewFlagOnLine            (long ixl, float shotpoint);
  long   insertNewFlagOnLine           (long ixl, long ixf);
  long   insertNewFlagOnLineFromBuffer (long ixl, long ixf);
  long   deleteFlagFromLine            (long ixl, long ixf);
  long   deleteFlagFromLineToBuffer    (long ixl, long ixf);
  void   deleteAllFlagsFromLine        (long ixl);

public:    // allocate and free space for a seismic line (optional usage).

  void  allocateSpaceForLine (long ixl, long nadd);
  void  freeSpaceForLine     (long ixl);

public:    // search along seismic line.
           // these return an index (ixf), or -1 if not found.

  long findNearestShotpointOnLine  (long ixl, float shotpoint,int dir=0)const;
  long findMatchingShotpointOnLine (long ixl, float shotpoint)          const;
  long findNearestFlagOnLine       (long ixl, double xloc, double yloc) const;
  long findNearestSourceOnLine     (long ixl, double xloc, double yloc) const;
  long findNearestReceiverOnLine   (long ixl, double xloc, double yloc) const;
  long closestNearbyFlag (long ixl, long ixf, double xloc, double yloc) const;


//----------------- pass-thru to SourcesReceivers ------------------//
//----------------- pass-thru to SourcesReceivers ------------------//
//----------------- pass-thru to SourcesReceivers ------------------//

   // ixl  = index of desired line.
   // ixf  = index of desired flag on desired line.
   // ixs2 = index of desired source at desired flag on desired line.
   // ixr2 = index of desired receiver at desired flag on desired line.

public:          // get source-receiver values

  long      numSourcesAtFlag      (long ixl, long ixf)  const;
  long      numReceiversAtFlag    (long ixl, long ixf)  const;
  int       flagHasSource         (long ixl, long ixf)  const;
  int       flagHasReceiver       (long ixl, long ixf)  const;

  long      sourceGroupNumber     (long ixl, long ixf, long ixs2)  const;
  long      receiverTraceNumber   (long ixl, long ixf, long ixr2)  const;

public:          // set source-receiver values

  void   addSourceToFlag    (long ixl, long ixf, long sgroup);
  void   addReceiverToFlag  (long ixl, long ixf, long rtrace);
  void   removeSourcesFromFlag                 (long ixl, long ixf);
  void   removeSourcesFromAllFlagsOnLine       (long ixl);
  void   removeSourcesFromAllFlagsOnAllLines   ();
  void   removeReceiversFromFlag               (long ixl, long ixf);
  void   removeReceiversFromAllFlagsOnLine     (long ixl);
  void   removeReceiversFromAllFlagsOnAllLines ();
  void   trimSourceAllocationsOnAllFlagsOnAllLines  ();
  void   trimReceiverAllocationsOnAllFlagsOnAllLines();


//----------- pass-thru functions to individual flags ----------------//
//----------- pass-thru functions to individual flags ----------------//
//----------- pass-thru functions to individual flags ----------------//

     // ixl  = index of desired line.
     // ixf  = index of desired flag on desired line.

public:    // get flag values

  class FieldFlag *getFlagPointer (long ixl, long ixf)           const;
  long             getLineIndex   (class FieldFlag *field_flag)  const;
  long             getFlagIndex   (class FieldFlag *field_flag)  const;

  int    flagIsSelected    (long ixl, long ixf)             const;
  double distanceToFlag    (long ixl, long ixf, double xloc, double yloc) const;
  double distanceSquaredToFlag
                           (long ixl, long ixf, double xloc, double yloc) const;

  int    flagValueIsDependent    (long ixl, long ixf, int ident)  const;
  double getFlagValue            (long ixl, long ixf, int ident)  const;

  float  getShotpoint                (long ixl, long ixf)  const;
  double getIncrDistance             (long ixl, long ixf)  const;
  double getXloc                     (long ixl, long ixf)  const;
  double getYloc                     (long ixl, long ixf)  const;
  float  getElevation                (long ixl, long ixf)  const;
  float  getHoleDepth                (long ixl, long ixf)  const;
  float  getUpholeTime               (long ixl, long ixf)  const;
  float  getReceiverStatic           (long ixl, long ixf)  const;
  float  getSourceStatic             (long ixl, long ixf)  const;
  float  getReceiverXskid            (long ixl, long ixf)  const;
  float  getReceiverYskid            (long ixl, long ixf)  const;
  float  getReceiverEskid            (long ixl, long ixf)  const;
  char   getFlagSelectValue          (long ixl, long ixf)  const;
  double getCumDistance              (long ixl, long ixf)  const;
  double getAzimuth                  (long ixl, long ixf)  const;
  int    getDeadSourceCode           (long ixl, long ixf)  const;
  int    getDeadReceiverCode         (long ixl, long ixf)  const;
  int    sourceMaybeDead             (long ixl, long ixf)  const;
  int    receiverMaybeDead           (long ixl, long ixf)  const;
  long   getCumulativeGroundPosition (long ixl, long ixf)  const;
  long   getMatchableGroundPosition  (long ixl, long ixf)  const;

  void   findCumulativeGroundPosition (long cumulative_gp,
                                               long *ixl, long *ixf)  const;
  long   findCumulativeGroundPosition (long ixl, long cumulative_gp)  const;
  long   findMatchableGroundPosition  (long ixl, long matchable_gp)   const;

  float defaultSourceDatumStatic
                (long ixl, long ixf, float ref, float ve)  const;
  float defaultReceiverDatumStatic
                (long ixl, long ixf, float ref, float ve)  const;

  int    receiverIsSkidded        (long ixl, long ixf)  const;
  void   getSkiddedCoords         (long ixl, long ixf,
                                    float inline_skid, float crossline_skid,
                                    double *x, double *y)  const;
  void   getSkiddedCoordsPlus     (long ixl, long ixf,
                                    float inline_skid, float crossline_skid,
                                    double *x, double *y)  const;
  void   getSkiddedReceiverCoords (long ixl, long ixf,
                                    double *x, double *y)  const;

public:    // set flag values

  void   setFlagValue          (long ixl, long ixf, int ident, double value);
  void   setDependentFlagValue (long ixl, long ixf, int ident, double value);

  void   setShotpoint             (long ixl, long ixf, float  value);
  void   setIncrDistance          (long ixl, long ixf, double value);
  void   setXloc                  (long ixl, long ixf, double value);
  void   setYloc                  (long ixl, long ixf, double value);
  void   setElevation             (long ixl, long ixf, float  value);
  void   setHoleDepth             (long ixl, long ixf, float  value);
  void   setUpholeTime            (long ixl, long ixf, float  value);
  void   setReceiverStatic        (long ixl, long ixf, float  value);
  void   setSourceStatic          (long ixl, long ixf, float  value);
  void   setReceiverXskid         (long ixl, long ixf, float  value);
  void   setReceiverYskid         (long ixl, long ixf, float  value);
  void   setReceiverEskid         (long ixl, long ixf, float  value);
  void   setFlagSelectValue       (long ixl, long ixf, char   value);
  void   incrementFlagSelectValue (long ixl, long ixf);
  void   clearFlagSelections      (long ixl);


//---------------------- end of functions -----------------------//
//---------------------- end of functions -----------------------//
//---------------------- end of functions -----------------------//

} ;

#endif

//---------------------------- end --------------------------------//
//---------------------------- end --------------------------------//
//---------------------------- end --------------------------------//
