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

//------------------------ seis_line.hh ---------------------//
//------------------------ seis_line.hh ---------------------//
//------------------------ seis_line.hh ---------------------//

//             header file for the SeisLine class
//             derived from the SmartArray class
//                    subdirectory geom

  // This class contains all data associated with a single
  // seismic line used in 2D or 3D seismic data acquisition.

  // This class maintains an array of pointers to FieldFlag objects.

  // This object owns the field flags.  This means that this class
  // creates and deletes the field flags when they are inserted and
  // removed.  They are not to be created or deleted outside of
  // this class.
 
  // It is intended that each SeisLine will have an array of field flags.
  // It is not anticipated that anyone else will have an array of field flags.


#ifndef _SEIS_LINE_HH_
#define _SEIS_LINE_HH_

#include "oprim/smart_array.hh"


class SeisLine  :  public SmartArray
{

//----------------------- data ------------------------------//
//----------------------- data ------------------------------//
//----------------------- data ------------------------------//

private:

  long   _line_number;       // number (identification) of seismic line.
  long   _ixl;               // index of this seismic line in survey.
  char   _select;            // selection value.
  long _last_cumulative_gp;  // cumulative ground position of last flag
                             //    on line, counting from start of survey.
  long _first_matchable_gp;  // matching ground position of first flag
                             //    on line, such that matching ground
                             //    positions on different lines correspond
                             //    to the same shotpoint (if shotpoint
                             //    intervals are same for all lines).
  long   _num_sources;       // number of sources on seismic line.
  long   _num_receivers;     // number of receivers on seismic line.
  int    _frozen;            // whether dependent updates are frozen.
  int    _need;              // whether dependent updates are needed.

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

  class FgInformer    *_informer;      // informer class.
  class FgConnect     *_connect;       // connections class.
  class AccBase       *_acc_index;     // array column control.
  class AccBase       *_acc_coords;    // array column control.
  class AccBase       *_acc_shot;      // array column control.
  class AccBase       *_acc_dist;      // array column control.
  class AccBase       *_acc_xloc;      // array column control.
  class AccBase       *_acc_yloc;      // array column control.
  class AccBase       *_acc_elev;      // array column control.
  class AccBase       *_acc_hd;        // array column control.
  class AccBase       *_acc_tuh;       // array column control.
  class AccBase       *_acc_rstat;     // array column control.
  class AccBase       *_acc_sstat;     // array column control.
  class AccBase       *_acc_xskid;     // array column control.
  class AccBase       *_acc_yskid;     // array column control.
  class AccBase       *_acc_eskid;     // array column control.
  class AccSelect     *_acc_select;    // array column control.
  class AccBase       *_acc_cum;       // array column control.
  class AccSearch     *_search;        // search helper class.
  class AccChain      *_chain;         // chain helper class.
  class FastSort      *_sort;          // sort helper class.

//--------------------- functions ---------------------------//
//--------------------- functions ---------------------------//
//--------------------- functions ---------------------------//

public:   // constructor and destructor

  SeisLine (class FgInformer *informer, FgConnect *connect, int chaining);
  virtual ~SeisLine();

/*
  class FieldFlag *unsafeFieldFlag (long ixf)  const
                     { return ((FieldFlag**)_array)[ixf]; }
*/

  class FieldFlag *unsafeFieldFlag (long ixf)  const
                     { return (FieldFlag*)unsafeArrayElement(ixf); }

  class FieldFlag *fieldFlag (long ixf)  const
                     { return (FieldFlag*)arrayElement(ixf); }

public:    // virtual functions overriding SmartArray.

  virtual int  valueIsDependent  (int ident, long ixf)  const;
  virtual void setDependencyTrue (int ident, long ixf);
  virtual void setDependencyFalse(int ident, long ixf);
  virtual void valuesWillChange  (int ident, long index, long nrem, long nins);
  virtual void valuesHaveChanged (int ident, long index, long nrem, long nins);

private:    // virtual functions overriding SmartArray.

  virtual void  beforeRemoveInsert    (long index, long nrem, long nins);
  virtual void  afterRemoveInsert     (long index, long nrem, long nins);
  virtual void  beforeNewActiveIndex  ();
  virtual void  afterNewActiveIndex   ();
  virtual void *doCreateObject        ();
  virtual void  doDeleteObject        (void *object);
  virtual void  objectWillBeRemoved   (long ixf);
  virtual void  objectHasBeenInserted (long ixf);

private:

  void beforeValueChanged (int ident, long index);
  void afterValueChanged  (int ident, long index);
  void findHelper         (long ixf, double xloc, double yloc,
               long *nearest_index, double *nearest_distance2) const;

public:   // called only by SeisSurvey.

  void   setChaining         (int chaining);
  void   setIndexOfThisLine  (long  value)                { _ixl = value; }
  void   setLineSelectValue  (char  value)             { _select = value; }
  void   setLastCumulativeGP (long  value) { _last_cumulative_gp = value; }
  void   setFirstMatchableGP (long  value) { _first_matchable_gp = value; }
  long   updateLastCumulativeGP (long prev_num_gps);


//---------------- public access to this line ------------------//
//---------------- public access to this line ------------------//
//---------------- public access to this line ------------------//

        // ixf = index of desired flag on this line.
        // ixl = index of this line in the survey.

public:     // get values

  long   getLineNumber                 ()  const  { return _line_number; }
  long   getLineIndex                  ()  const  { return _ixl; }
  char   getLineSelectValue            ()  const  { return _select; }
  long   numSourcesOnLine              ()  const  { return _num_sources; }
  long   numReceiversOnLine            ()  const  { return _num_receivers; }
  long   numFlagsOnLine                ()  const  { return numElements(); }
  long   numSelectedFlagsOnLine        ()  const;
  long   firstCumulativeGroundPosition ()  const;
  long   firstMatchableGroundPosition  ()  const;
  long   lastCumulativeGroundPosition  ()  const;
  long   lastMatchableGroundPosition   ()  const;
  long   getActiveFlagIndexOnLine      ()  const  { return getActiveIndex(); }
  float  getActiveShotpointOnLine      ()  const;
  float  getFirstShotpointOnLine       ()  const;
  float  getLastShotpointOnLine        ()  const;
  float  getSmallestShotpointOnLine    ()  const;
  float  getLargestShotpointOnLine     ()  const;
  float  getMinShotpointIncrOnLine     ()  const;
  float  getMaxShotpointIncrOnLine     ()  const;
  int    shotpointsAreDuplicatedOrNotSorted ()  const;
  double minimumXlocOnLine             ()  const;
  double maximumXlocOnLine             ()  const;
  double minimumYlocOnLine             ()  const;
  double maximumYlocOnLine             ()  const;
  double distanceToLine                (double xloc, double yloc)  const;
  double distanceSquaredToLine         (double xloc, double yloc)  const;

  int    deadSourceCodesAreSet     () const { return _dead_source_codes_set; }
  int    deadReceiverCodesAreSet   () const { return _dead_receiver_codes_set; }
  long   numDeadSourcesOnLine      () const { return _num_dead_sources; }
  long   numDeadReceiversOnLine    () const { return _num_dead_receivers; }
  long   numReversedSourcesOnLine  () const { return _num_reversed_sources; }
  long   numReversedReceiversOnLine() const { return _num_reversed_receivers; }
  long   numMissingSourcesOnLine   () const { return _num_missing_sources; }
  long   numMissingReceiversOnLine () const { return _num_missing_receivers; }
  long   numLiveSourcesOnLine      () const { return _num_live_sources; }
  long   numLiveReceiversOnLine    () const { return _num_live_receivers; }

public:     // set values

  void   setLineNumber                 (long line_number);
  void   setActiveFlagIndexOnLine      (long index) { setActiveIndex(index); }
  void   setActiveShotpointOnLine      (float shotpoint);       // search
  void   reverseLineDirection          ();
  void   freezeDependentUpdatesOnLine  ();
  void   resumeDependentUpdatesOnLine  ();
  void   performDependentUpdatesOnLine ();

  void   setDeadSourceCodes            ();
  void   setDeadReceiverCodes          ();
  void clearDeadSourceCodes            ();
  void clearDeadReceiverCodes          ();

public:  // insert or remove flags.
         // the non-void functions return index (ixf) where action occurred.
         // the non-void functions return -1 if failed.

  long   appendNewFlagToLine           ();         
  long   placeNewFlagOnLine            (float shotpoint);         
  long   insertNewFlagOnLine           (long ixf);
  long   insertNewFlagOnLineFromBuffer (long ixf); 
  long   deleteFlagFromLine            (long ixf);
  long   deleteFlagFromLineToBuffer    (long ixf);
  void   deleteAllFlagsFromLine        ();           

public:    // allocate and free space for a seismic line (optional usage).

  void  allocateSpaceForLine (long nadd) { allocateSpace(nadd); }
  void  freeSpaceForLine     ()          { freeSpace    (); }

public:    // search along seismic line.
           // these return an index (ixf), or -1 if not found.

  long  findNearestShotpointOnLine  (float shotpoint, int dir = 0) const;
  long  findMatchingShotpointOnLine (float shotpoint)              const;
  long  findNearestFlagOnLine       (double xloc, double yloc)     const;
  long  findNearestSourceOnLine     (double xloc, double yloc)     const;
  long  findNearestReceiverOnLine   (double xloc, double yloc)     const;
  long  closestNearbyFlag (long ixf, double xloc, double yloc)     const;

  void  findFlagsBracketingTrueMidpoint(double cmp_xloc, double cmp_yloc,
             long *ixfa, long *ixfb, float *wa, float *wb);

  void  findFlagsBracketingCenterMidpoint(float cmp_matchable_gp,
             long *ixfa, long *ixfb, float *wa, float *wb, float *past);


//----------------- pass-thru to SourcesReceivers ------------------//
//----------------- pass-thru to SourcesReceivers ------------------//
//----------------- pass-thru to SourcesReceivers ------------------//

   // ixf  = index of desired flag on this line.
   // ixs2 = index of desired source at desired flag on this line.
   // ixr2 = index of desired receiver at desired flag on this line.

public:          // get source-receiver values

  long      numSourcesAtFlag      (long ixf)  const;
  long      numReceiversAtFlag    (long ixf)  const;
  int       flagHasSource         (long ixf)  const;
  int       flagHasReceiver       (long ixf)  const;

  long      sourceGroupNumber     (long ixf, long ixs2)  const;
  long      receiverTraceNumber   (long ixf, long ixr2)  const;

public:          // set source-receiver values

  void   addSourceToFlag                   (long ixf, long sgroup);
  void   addReceiverToFlag                 (long ixf, long rtrace);

  void   removeSourcesFromFlag             (long ixf);
  void   removeSourcesFromAllFlagsOnLine   ();
  void   removeReceiversFromFlag           (long ixf);
  void   removeReceiversFromAllFlagsOnLine ();
  void   trimSourceAllocationsOnAllFlagsOnLine       ();
  void   trimReceiverAllocationsOnAllFlagsOnLine     ();


//----------- pass-thru functions to individual flags ----------------//
//----------- pass-thru functions to individual flags ----------------//
//----------- pass-thru functions to individual flags ----------------//

     // ixf  = index of desired flag on this line.

public:    // get flag values

  class FieldFlag *getFlagPointer (long ixf)                     const;
  static long      getLineIndex   (class FieldFlag *field_flag);
  static long      getFlagIndex   (class FieldFlag *field_flag);

  int     flagIsSelected    (long ixf)  const;
  double  distanceToFlag    (long ixf, double xloc, double yloc)  const;
  double  distanceSquaredToFlag
                            (long ixf, double xloc, double yloc)  const;

  int     flagValueIsDependent  (long ixf, int ident)  const;
  double  getFlagValue          (long ixf, int ident)  const;

  float   getShotpoint                (long ixf)  const;
  double  getIncrDistance             (long ixf)  const;
  double  getXloc                     (long ixf)  const;
  double  getYloc                     (long ixf)  const;
  float   getElevation                (long ixf)  const;
  float   getHoleDepth                (long ixf)  const;
  float   getUpholeTime               (long ixf)  const;
  float   getReceiverStatic           (long ixf)  const;
  float   getSourceStatic             (long ixf)  const;
  float   getReceiverXskid            (long ixf)  const;
  float   getReceiverYskid            (long ixf)  const;
  float   getReceiverEskid            (long ixf)  const;
  char    getFlagSelectValue          (long ixf)  const;
  double  getCumDistance              (long ixf)  const;
  double  getAzimuth                  (long ixf)  const;
  int     getDeadSourceCode           (long ixf)  const;
  int     getDeadReceiverCode         (long ixf)  const;
  int     sourceMaybeDead             (long ixf)  const;
  int     receiverMaybeDead           (long ixf)  const;
  long    getCumulativeGroundPosition (long ixf)  const;
  long    getMatchableGroundPosition  (long ixf)  const;

  long    findCumulativeGroundPosition (long cumulative_gp)  const;
  long    findMatchableGroundPosition  (long matchable_gp)   const;

  float defaultSourceDatumStatic   (long ixf, float ref, float ve)  const;
  float defaultReceiverDatumStatic (long ixf, float ref, float ve)  const;

  int     receiverIsSkidded        (long ixf)  const;
  long    getSkiddedCoords         (long ixf,
                                     float inline_skid, float crossline_skid,
                                     double *x, double *y)  const;
  long    getSkiddedCoordsPlus     (long ixf,
                                     float inline_skid, float crossline_skid,
                                     double *x, double *y)  const;
  long    getSkiddedReceiverCoords (long ixf,
                                     double *x, double *y)  const;

public:    // set flag values

  void   setFlagValue          (long ixf, int ident, double value);
  void   setDependentFlagValue (long ixf, int ident, double value);

  void   setShotpoint             (long ixf, float  value);
  void   setIncrDistance          (long ixf, double value);
  void   setXloc                  (long ixf, double value);
  void   setYloc                  (long ixf, double value);
  void   setElevation             (long ixf, float  value);
  void   setHoleDepth             (long ixf, float  value);
  void   setUpholeTime            (long ixf, float  value);
  void   setReceiverStatic        (long ixf, float  value);
  void   setSourceStatic          (long ixf, float  value);
  void   setReceiverXskid         (long ixf, float  value);
  void   setReceiverYskid         (long ixf, float  value);
  void   setReceiverEskid         (long ixf, float  value);
  void   setDeadSourceCode        (long ixf, int    value);
  void   setDeadReceiverCode      (long ixf, int    value);
  void   setSourceMaybeDead       (long ixf, int    value);
  void   setReceiverMaybeDead     (long ixf, int    value);
  void   setFlagSelectValue       (long ixf, char   value);
  void   incrementFlagSelectValue (long ixf);
  void   clearFlagSelections      ();


//---------------------- end of functions -----------------------//
//---------------------- end of functions -----------------------//
//---------------------- end of functions -----------------------//

} ;

#endif

//---------------------------- end --------------------------------//
//---------------------------- end --------------------------------//
//---------------------------- end --------------------------------//
