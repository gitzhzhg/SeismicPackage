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

//---------------------- fg_gathers.hh ---------------------//
//---------------------- fg_gathers.hh ---------------------//
//---------------------- fg_gathers.hh ---------------------//

//              header file for the FgGathers class
//                  not derived from any class
//                       subdirectory geom 

  // This class updates source and receiver gathers in the
  // FieldGeometry class.  The sources and receivers are placed
  // onto the correct field flags.


#ifndef _FG_GATHERS_HH_
#define _FG_GATHERS_HH_


class FgGathers
{

//----------------------- data ------------------------------//
//----------------------- data ------------------------------//
//----------------------- data ------------------------------//

private:

  class FieldGeometry    *_fg;
  class FgTraceValues    *_tv;
  class SeisSurvey       *_survey;
  class RpCards          *_rp_cards;
  class PpCards          *_pp_cards;
  class ZtCards          *_zt_cards;
  class Midpoints        *_midpoints;

  int _s_out_of_date; // whether source   gathers are out-of-date.
  int _r_out_of_date; // whether receiver gathers are out-of-date.
  int _m_out_of_date; // whether midpoint gathers are out-of-date.

  int _s_incomplete;  // whether source   gathers are incomplete.
  int _r_incomplete;  // whether receiver gathers are incomplete.
  int _m_incomplete;  // whether midpoint gathers are incomplete.

  long _unplaced_sources; // number of unplaced sources (no flag found).
  long _unplaced_traces;  // number of unplaced traces  (no flag found).

  int  _speedup;      // whether speedup info is available.

//----------------------- functions -------------------------//
//----------------------- functions -------------------------//
//----------------------- functions -------------------------//

public:     // constructor and destructor

  FgGathers (FieldGeometry *fg, FgTraceValues *tv,
                    SeisSurvey *survey, RpCards *rp_cards,
                    PpCards  *pp_cards, ZtCards *zt_cards,
                    class GridTransform *transform);
  virtual ~FgGathers();

public:    // pass-thru to Midpoints.

  long  numCmpGathers            ()                       const;
  long  numSelectedCmpGathers    ()                       const;
  long  getActiveCmpIndex        ()                       const;
  long  foldOfStack              (long ixcmp)             const;
  long  liveFoldOfStack          (long ixcmp)             const;
  char  getCmpSelectValue        (long ixcmp)             const;
  int   cmpIsSelected            (long ixcmp)             const;

  long   getMinimumFold           ()  const;
  long   getMaximumFold           ()  const;
  double getMinimumXlocBinCenter  ()  const;
  double getMinimumYlocBinCenter  ()  const;
  double getMaximumXlocBinCenter  ()  const;
  double getMaximumYlocBinCenter  ()  const;
  float  getMinimumOffset         ()  const;
  float  getMaximumOffset         ()  const;
  double getMinimumXgridBinCenter ()  const;
  double getMinimumYgridBinCenter ()  const;
  double getMaximumXgridBinCenter ()  const;
  double getMaximumYgridBinCenter ()  const;

  long  originalTraceIndex       (long ixcmp, long ixfold)  const;
  long  originalTraceIndex       (long ixsorted)            const;

  long  findNearestCmp          (double xloc , double yloc )  const;
  long  findNearestCmpUsingGrid (double xgrid, double ygrid)  const;

  void  getCmpLocBinCenter  (long ixcmp, double *xloc , double *yloc ) const;
  void  getCmpGridBinCenter (long ixcmp, double *xgrid, double *ygrid) const;

  void  getCmpTraceLoc
            (long ixcmp, long ixfold, double *xloc , double *yloc ) const;
  void  getCmpTraceGrid
            (long ixcmp, long ixfold, double *xgrid, double *ygrid) const;
  float getCmpTraceOffset (long ixcmp, long ixfold) const;

  void   setActiveCmpIndex        (long ixcmp);
  void   setCmpSelectValue        (long ixcmp, char value);
  void   incrementCmpSelectValue  (long ixcmp);
  void   clearCmpSelections       ();

public:    // set values

  void  updateSourceGathers            ();
  void  updateReceiverGathers          ();  // also updates sources if nec.
  void  updateMidpointGathers          ();  // also updates sou/rec if nec.

  void  sourceGathersGoingOutOfDate    ();  // rec and CMP gthrs go out too.
  void  receiverGathersGoingOutOfDate  ();  // CMP gthrs go out too.
  void  midpointGathersGoingOutOfDate  ();  // when transform changes.

public:    // get values

  long  getRpLineIndex       (long ixrp)            const;
  long  getRpFlagIndex       (long ixrp, long ixl)  const;

  long  getSourceLineIndex   (long group)                const;
  long  getSourceFlagIndex   (long group)                const;
  long  getReceiverLineIndex (long group, long channel)  const;
  long  getReceiverFlagIndex (long group, long channel)  const;

  int   sourceGathersOutOfDate    ()  const  { return _s_out_of_date; }
  int   receiverGathersOutOfDate  ()  const  { return _r_out_of_date; }
  int   midpointGathersOutOfDate  ()  const  { return _m_out_of_date; }

  int   sourceGathersIncomplete   ()  const  { return _s_incomplete; }
  int   receiverGathersIncomplete ()  const  { return _r_incomplete; }
  int   midpointGathersIncomplete ()  const  { return _m_incomplete; }

  long  numUnplacedSources        ()  const  { return _unplaced_sources; }
  long  numUnplacedTraces         ()  const  { return _unplaced_traces; }

private:

  void  calculateSpeedupInfo ();
  void  updateLiveTraceFlags ();
  void  doTheWork            (int which);

//---------------------- end of functions -----------------------//
//---------------------- end of functions -----------------------//
//---------------------- end of functions -----------------------//

} ;

#endif

//---------------------------- end --------------------------------//
//---------------------------- end --------------------------------//
//---------------------------- end --------------------------------//
