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

//---------------------- midpoints.hh ---------------------//
//---------------------- midpoints.hh ---------------------//
//---------------------- midpoints.hh ---------------------//

//              header file for the Midpoints class
//                  not derived from any class
//                      subdirectory geom

    // This class contains the X and Y midpoint coordinates for each
    // trace in the survey.


    // ixorig   = original trace index before CMP sort.
    // ixsorted =          trace index after  CMP sort.
    // ixcmp    = index of CMP gather              (after CMP sort).
    // ixfold   = index of trace within CMP gather (after CMP sort).

    // ixorig   ranges from 0 thru numTraces()        - 1.
    // ixsorted ranges from 0 thru numTraces()        - 1.
    // ixcmp    ranges from 0 thru numCmpGathers()    - 1.
    // ixfold   ranges from 0 thru foldOfStack(ixcmp) - 1.


#ifndef _MIDPOINTS_HH_
#define _MIDPOINTS_HH_


class Midpoints
{

//----------------------- data ------------------------------//
//----------------------- data ------------------------------//
//----------------------- data ------------------------------//

private:

  class FgInformer          *_informer;  // not owned by this class.
  class FgUserAbort         *_ua;        // not owned by this class.
  const class GridTransform *_transform; // not owned by this class.
  const class FgTraces      *_traces;    // not owned by this class.
  class SimpleSelect        *_select;    // owned by this class.

  long   _ntraces;   // number of traces in the survey.
  long  *_trums;     // array of original trace indices in CMP sorted order.
  char  *_binettes;  // array of original trace binettes (sub-bin 1 to 9).
  long  *_unique;    // temporary array of CMP bin numbers for each trace.

  long   _ncmps;     // number of midpoint locations.
  long   _nxbins;    // number of CMP bins in the X direction.
  long   _nybins;    // number of CMP bins in the Y direction.
  long  *_first;     // list of index into _trums array for first
                     //   trace for each midpoint.
  long  *_nfold;     // list of total fold for each midpoint.
  long  *_nlive;     // list of live fold for each midpoint.
  long  *_hwd3;      // list of CPS header word 3 for each midpoint.
  char  *_fattest;   // list of fattest binette for each midpoint.
  char  *_sel;       // array of selection flags for each midpoint.
  long   _active;    // active CMP index (or -1 if no midpoints).

  int    _first_bin_has_nil_coords;

  long   _foldmin;   // minimum fold of stack.
  long   _foldmax;   // maximum fold of stack.
  double _xlocmin;   // minimum X loc bin center coord.
  double _xlocmax;   // maximum X loc bin center coord.
  double _ylocmin;   // minimum Y loc bin center coord.
  double _ylocmax;   // maximum Y loc bin center coord.
  double _xgridmin;  // minimum X grid bin center coord.
  double _xgridmax;  // maximum X grid bin center coord.
  double _ygridmin;  // minimum Y grid bin center coord.
  double _ygridmax;  // maximum Y grid bin center coord.
  long   _ixgridmin; // minimum X grid bin center coord (internal use).
  long   _ixgridmax; // maximum X grid bin center coord (internal use).
  long   _iygridmin; // minimum Y grid bin center coord (internal use).
  long   _iygridmax; // maximum Y grid bin center coord (internal use).

//------------------------- functions -----------------------//
//------------------------- functions -----------------------//
//------------------------- functions -----------------------//

public:      // constructor and destructor.

  Midpoints (FgInformer *informer, FgUserAbort *ua,
               const GridTransform *transform, const FgTraces *traces);
  virtual ~Midpoints();

public:  // get values.
         // most of these return 0 or nil if there are no traces, or if
         //   midpoints are needed or a CMP sort is needed.

  long   numCmpGathers         ()            const  { return _ncmps; }
  long   numXbins              ()            const  { return _nxbins; }
  long   numYbins              ()            const  { return _nybins; }
  long   numSelectedCmpGathers ()            const;
  long   getActiveCmpIndex     ()            const  { return _active; }
  long   foldOfStack           (long ixcmp)  const;
  long   liveFoldOfStack       (long ixcmp)  const;
  long   headerWord3           (long ixcmp)  const;
  char   getCmpSelectValue     (long ixcmp)  const;
  int    cmpIsSelected         (long ixcmp)  const;
  int    cmpFattestBinette     (long ixcmp)  const;

  int   midpointGathersOutOfDate  () const { return (_trums == 0); }
  int   liveFoldOutOfDate         () const { return (_nlive == 0); }

  int    firstBinHasNilCoords () const  { return _first_bin_has_nil_coords; }
  long   getMinimumFold           ()  const  { return _foldmin; }
  long   getMaximumFold           ()  const  { return _foldmax; }
  double getMinimumXlocBinCenter  ()  const  { return _xlocmin; }
  double getMaximumXlocBinCenter  ()  const  { return _xlocmax; }
  double getMinimumYlocBinCenter  ()  const  { return _ylocmin; }
  double getMaximumYlocBinCenter  ()  const  { return _ylocmax; }
  double getMinimumXgridBinCenter ()  const  { return _xgridmin; }
  double getMaximumXgridBinCenter ()  const  { return _xgridmax; }
  double getMinimumYgridBinCenter ()  const  { return _ygridmin; }
  double getMaximumYgridBinCenter ()  const  { return _ygridmax; }

  long   originalTraceIndex (long ixcmp, long ixfold)  const;
             // given trace index (ixfold) within a given CMP gather,
             // returns original trace index (in acquisition order).

  long   originalTraceIndex (long ixsorted)  const;
             // given trace index after CMP sort, returns
             // original trace index (in acquisition order).

  long   findNearestCmp          (double xloc , double yloc )  const;
  long   getMatchingCmp          (double xloc , double yloc )  const;
  long   findNearestCmpUsingGrid (double xgrid, double ygrid)  const;
  long   getMatchingCmpUsingGrid (double xgrid, double ygrid)  const;

  void   getCmpLocBinCenter  (long ixcmp, double *xloc , double *yloc ) const;
  void   getCmpGridBinCenter (long ixcmp, double *xgrid, double *ygrid) const;

  void   getCmpTraceLoc
                (long ixcmp, long ixfold, double *xloc , double *yloc ) const;
  void   getCmpTraceGrid
                (long ixcmp, long ixfold, double *xgrid, double *ygrid) const;
  float  getCmpTraceOffset  (long ixcmp, long ixfold) const;
  int    getCmpTraceBinette (long ixcmp, long ixfold) const;

public:  // get values.
         // call to learn whether something is out of date.
         // midpoints are needed if midpointsChanging has been called.
         // CMP sort  is  needed if transformChanging has been called.
         // deadcodes are needed if deadCodesChanging has been called.
         // if midpoints are needed, must call initializeMidpoints,
         //   setMidpoint for each trace, and cmpSort.
         // if just CMP sort is needed, must call cmpSortNeeded.

  int    midpointsNeeded()  const  { return (_ntraces == 0); }
  int    cmpSortNeeded  ()  const  { return (_ncmps   == 0); }
  int    deadCodesNeeded()  const  { return (!_nlive); }

public:  // set values.
         // call midpointsChanging when data changes which affects midpoints.
         //   this causes a new set of midpoints to be needed.
         // call transformChanging when grid transform changes.
         //   this causes a new CMP sort to be needed.

  void   midpointsChanging();
  void   deadCodesChanging();
  void   transformChanging();

public:      // set values.

  long   cmpSort     ();   // sets _trums[] and _ncmps.
                           // allocates and sets _each[].
                           // needs grid transform.
                           // returns 0 if successful.
                           // returns potential #gathers if not created.
                           // returns -1 if user abort.

  void   updateLiveFold();

  void   setActiveCmpIndex        (long ixcmp);
  void   setCmpSelectValue        (long ixcmp, char value);
  void   incrementCmpSelectValue  (long ixcmp);
  void   clearCmpSelections       ();
  void   incrementGridCoords      (long ixstep, long iystep);

private:

  void       calculateGridCoords ();
  void       beforeSorting       ();
  void       afterSorting        ();
  void       useMysortMethod     ();
  void       useQsortMethod      ();
  void       useBinningMethod    ();
  void       useCountingMethod   ();
  static int mysortFun           (void *data, long lo, long up);
  static int qsortFun            (const void *element1, const void *element2);
  static int offsetSortFun       (const void *element1, const void *element2);

//---------------------- end of functions -----------------------//
//---------------------- end of functions -----------------------//
//---------------------- end of functions -----------------------//

} ;

#endif

//---------------------------- end --------------------------------//
//---------------------------- end --------------------------------//
//---------------------------- end --------------------------------//
