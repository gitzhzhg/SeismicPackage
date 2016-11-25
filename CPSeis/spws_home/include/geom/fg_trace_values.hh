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

//------------------------ fg_trace_values.hh -----------------------//
//------------------------ fg_trace_values.hh -----------------------//
//------------------------ fg_trace_values.hh -----------------------//

//           header file for the FgTraceValues class
//                 not derived from any class
//                    subdirectory geom


     //  This class derives values for seismic traces from
     //  the FieldGeometry class.
     //  See the implementation file for documentation.


#ifndef _FG_TRACE_VALUES_HH_
#define _FG_TRACE_VALUES_HH_

class SeisLine;
class FieldFlag;
class RpCard;
class PpCard;

class FgTraceValues
{

//------------------------ data -------------------------------//
//------------------------ data -------------------------------//
//------------------------ data -------------------------------//

private:

  class FgInformer    *_informer;
  class FgUserAbort   *_ua;
  class SeisSurvey    *_survey;
  class RpCards       *_rp_cards;
  class PpCards       *_pp_cards;
  class ZtCards       *_zt_cards;
  class FgGroups      *_groups;
  class FgTraces      *_traces;
  class Midpoints     *_midpoints;
  class FgTeData      *_tred;	/* ehs */

private:      // status of gathers.

  long _unplaced_sources; // number of unplaced sources (missing source flag).
  long _unplaced_traces;  // number of unplaced traces  (missing receiver flag).

  int _dependent_values_out_of_date;
  int _sources_out_of_date;
  int _receivers_out_of_date;
  int _dead_codes_out_of_date;
  int _coords_out_of_date;
  int _transform_out_of_date;

  int _speedup_added_to_field_flags;
  int _speedup_added_to_rp_cards;
  int _speedup_added_to_traces;

private:   //   FGD variables.

  float  _ve;       // elevation velocity.
  float  _ref;      // reference elevation.
  float  _fixdist;  // fixed inline distance increment.
  long   _ndpt;     // number of time values in trace.

private:  // controlling variables.

  long    _itrace;    // requested trace number (sequential from 1).
  int     _all_clear; // TRUE (if all values are guaranteed clear) or FALSE.
  int     _error;     // TRUE (if some values are bad) or FALSE.
  long    _ixpp;      // PP card index             (or -1).
  PpCard *_pp_card;   // PP card pointer           (or NULL).
  long    _ixg;       // group index on PP card    (or -1).
  long    _group;     // unique source file number (or inil).
  long    _channel;   // channel number            (or inil).
  long    _trace;     // sequential trace number   (or inil).

private:  // pp card information.

  long   _ngroups;       // number of groups on PP card.
  long   _ntraces;       // number of traces on PP card.
  long   _first_group;   // first group number on PP card.
  long   _first_trace;   // first trace number on PP card.
  long   _nchan;         // number of channels in group.
  long   _ixl_pp_source; // index of line with first source   on PP card.
  long   _ixf_pp_source; // index of flag with first source   on PP card.
  long   _ixl_pp_rec;    // index of line with first receiver on PP card.
  long   _ixf_pp_rec;    // index of flag with first receiver on PP card.

  long   _ixrp_first;       // index of first RP card in pattern.
  long   _ixrp_last;        // index of last  RP card in pattern.
  long   _ixrp_first_irreg; // index of first RP card with SKIP or DUP.
  long   _ixrp_last_irreg;  // index of last  RP card with SKIP or DUP.
  long   _ixrp_chan1;       // index of RP card containing channel 1.
  long   _rp_line_chan1;    // line number  for channel 1 on RP card.
  long   _rp_mgp_chan1;     // matchable GP for channel 1 on RP card.

private:  // group (source) information.

  double _source_xloc;
  double _source_yloc;
  SeisLine  *_sline;           // goes with _ixl_source.
  FieldFlag *_sflag;           // goes with _ixf_source.
  long   _ixl_source;          // line index of source.
  long   _ixf_source;          // flag index of source.
  long   _ixf_source_closest;  // index of closest flag to skidded source.
  long   _ixl_chan1;           // line index of first channel in group.
  long   _ixf_chan1;           // flag index of first channel in group.

  long   _field_file;          // original field file number.
  long   _source_line;
  float  _source_shot;
  float  _source_static;
  double _source_dist;         // inline distance.
  long   _source_cgp;          // sequential (cumulative) ground position.
  long   _source_mgp;          // sequential (matchable) ground position.
  long   _source_mgp_closest;
  float  _source_elev;
  float  _source_hd;
  float  _source_tuh;
  long   _chan1_line;
  long   _chan1_mgp;

private:  // channel (receiver) information.

  RpCard    *_rp_card;         // goes with _ixrp.
  long       _ixrp;            // RP card index.

  double     _rec_xloc;
  double     _rec_yloc;
  float      _rec_elev;
  SeisLine  *_rline;           // goes with _ixl_rec.
  FieldFlag *_rflag;           // goes with _ixf_rec.
  long       _ixl_rec;         // line index of receiver.
  long       _ixf_rec;         // flag index of receiver.
  long       _ixf_rec_closest; // index of closest flag to skidded receiver.

  int    _last_trace_flag; // 2(last trace), 1(last in group), 0(otherwise).
  long   _rec_line;
  float  _rec_shot;
  float  _rec_static;
  float  _hd_at_rec;
  float  _tuh_at_rec;
  double _rec_dist;            // inline distance.
  long   _rec_cgp;             // sequential (cumulative) ground position.
  long   _rec_mgp;             // sequential (matchable) ground position.
  long   _rec_mgp_closest;

private:  // midpoint and dead-trace information.

  float  _offset;
  double _cmp_xloc;
  double _cmp_yloc;
  float  _cmp_line;              // float since average of source & rec.
  float  _cmp_mgp;               // float since average of source & rec.
  float  _cmp_nearest_elev;
  float  _cmp_nearest_shot;
  double _cmp_nearest_dist;      // inline distance.
  float  _cmp_center_elev;
  float  _cmp_center_shot;
  int    _dead_trace_code;


//------------------ functions ----------------------------------//
//------------------ functions ----------------------------------//
//------------------ functions ----------------------------------//

public:      // constructor and destructor.

  FgTraceValues(FgInformer *informer, FgUserAbort *ua,
                SeisSurvey *survey,
                RpCards  *rp_cards,  PpCards  *pp_cards,
                ZtCards  *zt_cards, FgGroups *groups,
                FgTraces *traces, Midpoints *midpoints);
  virtual ~FgTraceValues();

public:      // set and get FGD variables.

  void   setVe        (float ve);
  void   setRef       (float ref);
  void   setFixdist   (float fixdist);
  void   setNdpt      (long  ndpt);
  float  getVe        ()             const  { return _ve; }
  float  getRef       ()             const  { return _ref; }
  float  getFixdist   ()             const  { return _fixdist; }
  long   getNdpt      ()             const  { return _ndpt; }
  void   setTred      (class FgTeData *tred);	/* ehs */
  int    getDeadCode  (int itrace);		/* ehs */

public:     // set and get gather information.

  void  dependentValuesOutOfDate();
  void  dependentUpdatesResumed();

  void  sourceGathersChanging();
  void  receiverGathersChanging();
  void  zt1CodesChanging();
  void  zt2CodesChanging();
  void  zt3CodesChanging();
  void  zt4CodesChanging();
  void  tredCodesChanging();	/* ehs */
  void  coordsChanging();
  void  transformChanging();

  void  updateSourceGathers();
  void  updateReceiverGathers();
  long  updateMidpointGathers();  // returns #CMP gathers if not created.
  void  updateLiveFold();

  int   sourceGathersIncomplete   () const { return (_unplaced_sources > 0); }
  int   receiverGathersIncomplete () const { return (_unplaced_traces  > 0); }
  int   midpointGathersIncomplete () const { return (_unplaced_traces  > 0); }

  long  numUnplacedSources        () const { return _unplaced_sources; }
  long  numUnplacedTraces         () const { return _unplaced_traces; }

  int   sourceGathersOutOfDate    () const { return _sources_out_of_date; }
  int   receiverGathersOutOfDate  () const { return _receivers_out_of_date; }

public:      // calculate all or some values for one trace.
             // returns _error (same as getErrorFlag).

  void  startFromScratch     ();
  int   calculateTraceValues (long itrace, int more);
  int   calculateTraceValues (long group, long channel, int more);

public:      // miscellaneous.

  long  getSourceLineIndex   (long group);
  long  getSourceFlagIndex   (long group);
  long  getReceiverLineIndex (long group, long channel);
  long  getReceiverFlagIndex (long group, long channel);

//--------------------- get information ---------------------------//
//--------------------- get information ---------------------------//
//--------------------- get information ---------------------------//

       // this information is available after calling
       // calculateTraceValues.

       // nil is returned for any values which cannot be
       // calculated for any reason.

public:   // get pp card information.
          // these return 0 if not calculated.

  // getErrorFlag returns TRUE if any values could not be calculated
  // because of invalid input arguments, or invalid or incomplete
  // information in FieldGeometry.  note that if more == FALSE,
  // some information will not be calculated, but getErrorFlag
  // will still return FALSE if everything that is calculated
  // is successful.

  // getRequestedTraceNumber returns >= 0 if calculateTraceValues
  // was called, and returns -1 if calculatePpCardInfo... was called.

  int    getErrorFlag            ()  const  { return _error; }
  long   getPpCardIndex          ()  const  { return _ixpp; }
  long   getRequestedTraceNumber ()  const  { return _itrace; }

  long   getNumGroupsOnPpCard    ()  const  { return _ngroups; }
  long   getNumChannels          ()  const  { return _nchan; }

public:   // get group (source) information.
          // these return nil if not calculated.
          // getSourceLineIndex returns -1 if not calculated.
          // getSourceFlagIndex returns -1 if not calculated.

  long   getGroupNumber          ()  const  { return _group; }

  double getSourceXloc           () const { return _source_xloc; }
  double getSourceYloc           () const { return _source_yloc; }
  long   getSourceLineIndex      () const { return _ixl_source; }
  long   getSourceFlagIndex      () const { return _ixf_source; }

  long   getFieldFileNumber      () const { return _field_file; }
  long   getSourceLineNumber     () const { return _source_line; }
  float  getSourceShotpoint      () const { return _source_shot; }
  float  getSourceStatic         () const { return _source_static; }
  double getSourceInlineDistance () const { return _source_dist; }
  long   getSourceCumulativeGP   () const { return _source_cgp; }
  long   getSourceMatchableGP    () const { return _source_mgp; }
  float  getSourceElevation      () const { return _source_elev; }
  float  getSourceHoleDepth      () const { return _source_hd; }
  float  getSourceUpholeTime     () const { return _source_tuh; }

  long   getSourceGroundPosition () const;
  double getSourceInlineFixdist  () const;
  float  getTotalSourceStatic    () const;

public:   // get channel (receiver) information.
          // these return nil if not calculated.
          // getReceiverLineIndex returns -1 if not calculated.
          // getReceiverFlagIndex returns -1 if not calculated.

  long   getChannelNumber          () const { return _channel; }
  long   getTraceNumber            () const { return _trace; }
  long   getRpCardIndex            () const { return _ixrp; }
  long   getFirstRpCardIndex       () const { return _ixrp_first; }

  double getReceiverXloc           () const { return _rec_xloc; }
  double getReceiverYloc           () const { return _rec_yloc; }
  float  getReceiverElevation      () const { return _rec_elev; }
  long   getReceiverLineIndex      () const { return _ixl_rec; }
  long   getReceiverFlagIndex      () const { return _ixf_rec; }

  int    getLastTraceFlag          () const { return _last_trace_flag; }
  long   getReceiverLineNumber     () const { return _rec_line; }
  float  getReceiverShotpoint      () const { return _rec_shot; }
  float  getReceiverStatic         () const { return _rec_static; }
  float  getHoleDepthAtReceiver    () const { return _hd_at_rec; }
  float  getUpholeTimeAtReceiver   () const { return _tuh_at_rec; }
  double getReceiverInlineDistance () const { return _rec_dist; }
  long   getReceiverCumulativeGP   () const { return _rec_cgp; }
  long   getReceiverMatchableGP    () const { return _rec_mgp; }

  long   getReceiverGroundPosition () const;
  double getReceiverInlineFixdist  ()                     const;
  float  getTotalReceiverStatic    ()                     const;

public:   // get midpoint and dead-trace information.
          // these return nil if not calculated.

  float  getOffset                   () const { return _offset; }
  double getCmpXloc                  () const { return _cmp_xloc; }
  double getCmpYloc                  () const { return _cmp_yloc; }
  float  getCmpLineNumber            () const { return _cmp_line; }
  float  getCmpMatchableGP           () const { return _cmp_mgp; }
  float  getCmpNearestElevation      () const { return _cmp_nearest_elev; }
  float  getCmpNearestShotpoint      () const { return _cmp_nearest_shot; }
  double getCmpNearestInlineDistance () const { return _cmp_nearest_dist; }
  float  getCmpCenterElevation       () const { return _cmp_center_elev; }
  float  getCmpCenterShotpoint       () const { return _cmp_center_shot; }
  int    getDeadTraceCode            () const { return _dead_trace_code; }

  double getEffectiveCmpXloc         () const;
  double getEffectiveCmpYloc         () const;
  float  getCmpElevation             () const;
  float  getCmpShotpoint             () const;
  double getCmpCenterInlineDistance  () const;
  float  getTotalTraceStatic         () const;

////// maybe rename CmpNearestInlineDistance to CmpInlineDistance?
////// maybe rename CmpCenterInlineDistance  to CmpInlineFixdist?

//-------------------- private functions --------------------------//
//-------------------- private functions --------------------------//
//-------------------- private functions --------------------------//

private:

  void workingMessage(char *word1, long i, long n, char *word2, long unplaced);
  void finalMessage  (char *word1, long unplaced, char *word2);

  void createSourcesHelper();
  void createReceiversOrMidpointsHelper(int create_midpoints);

  void addSpeedupToFieldFlags();
  void addSpeedupToRpCards();

  long getRpLineIndex          (RpCard *rp_card)            const;
  long getRpFlagIndex          (RpCard *rp_card, long ixl)  const;

  long adjustForIrregularities (long xinc,
                long actual_line, long actual_ixf, long first_ixf);

  void clearAllTraceValues  ();
  void obtainDeadTraceCode  (int use_zt3_cards);
  void updateDeadTraceCodes ();

  void fetchControlInformation                ();

  void clearPpCardInformation                 ();
  int  fetchPpCardInformationForSources       ();
  void  savePpCardInformationForSources       (int error);
  int  fetchPpCardInformationForReceivers     ();
  void  savePpCardInformationForReceivers     (int error);

  void clearGroupInformation                  ();
  int  fetchGroupInformationForSources        ();
  void  saveGroupInformationForSources        (int error);
  int  fetchGroupInformationForReceivers      ();
  void  saveGroupInformationForReceivers      (int error);
  int  fetchGroupInformationForMidpoints      ();
  void  saveGroupInformationForMidpoints      (int error);

  void clearTraceInformation                  ();
  int  fetchTraceInformationForReceivers      ();
  int  fetchTraceInformationForMidpoints      ();

  void clearAdditionalInformation             ();
  void fetchAdditionalInformationForSources   ();
  void fetchAdditionalInformationForReceivers ();
  void fetchAdditionalInformationForMidpoints ();

//---------------------- end of functions -----------------------//
//---------------------- end of functions -----------------------//
//---------------------- end of functions -----------------------//

} ;

#endif

//---------------------------- end --------------------------------//
//---------------------------- end --------------------------------//
//---------------------------- end --------------------------------//
