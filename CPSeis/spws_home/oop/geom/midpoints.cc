
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
//--------------------- midpoints.cc ---------------------//
//--------------------- midpoints.cc ---------------------//
//--------------------- midpoints.cc ---------------------//

//         implementation file for the Midpoints class
//                  not derived from any class
//                      subdirectory geom



#include "geom/midpoints.hh"
#include "geom/fg_traces.hh"
#include "geom/fg_constants.hh"
#include "geom/fg_informer.hh"
#include "geom/fg_user_abort.hh"
#include "oprim/grid_transform.hh"
#include "oprim/simple_select.hh"
#include "oprim/integer_list.hh"
#include "cprim.h"
#include "named_constants.h"
#include <iostream.h>
#include <math.h>
#include <stdlib.h>
#include <string.h>
#include <assert.h>
#include <limits.h>



//--------------------- stuff relating to sorting ---------------//
//--------------------- stuff relating to sorting ---------------//
//--------------------- stuff relating to sorting ---------------//

enum { MYSORT_METHOD, QSORT_METHOD, BINNING_METHOD, COUNTING_METHOD };

/*
enum { METHOD = MYSORT_METHOD };
enum { METHOD = QSORT_METHOD };
enum { METHOD = BINNING_METHOD };
*/
enum { METHOD = COUNTING_METHOD };


////// using generic_sort takes about 3-5 times as long as using qsort.

////// qsort has the disadvantage that one cannot pass user data
////// to the user-written sorting function.  therefore, the
////// static variable THIS_data is used to temporarily store
////// the pointer to this object.

///////static Midpoints *THIS_data;    // this.

////// for speedup (while using either generic_sort or qsort), the
////// following pointers are temporarily saved here while sorting:

static long *TEMP_unique;       // this->_unique.
static long *TEMP_trums;        // this->_trums.
static long *TEMP_offsets;      // this->_traces->_offsets.


//---------------------- constructor -----------------------//
//---------------------- constructor -----------------------//
//---------------------- constructor -----------------------//


Midpoints::Midpoints(FgInformer *informer, FgUserAbort *ua,
             const GridTransform *transform, const FgTraces *traces)
           :
          _informer                  (informer),
          _ua                        (ua),
          _transform                 (transform),
          _traces                    (traces),
          _select                    (NULL),
          _ntraces                   (0),
          _trums                     (NULL),
          _binettes                  (NULL),
          _unique                    (NULL),
          _ncmps                     (0),
          _nxbins                    (0),
          _nybins                    (0),
          _first                     (NULL),
          _nfold                     (NULL),
          _nlive                     (NULL),
          _hwd3                      (NULL),
          _fattest                   (NULL),
          _sel                       (NULL),
          _active                    (-1),
          _first_bin_has_nil_coords  (FALSE),
          _foldmin                   (0),
          _foldmax                   (0),
          _xlocmin                   (DNIL),
          _xlocmax                   (DNIL),
          _ylocmin                   (DNIL),
          _ylocmax                   (DNIL),
          _xgridmin                  (DNIL),
          _xgridmax                  (DNIL),
          _ygridmin                  (DNIL),
          _ygridmax                  (DNIL),
          _ixgridmin                 (INIL),
          _ixgridmax                 (INIL),
          _iygridmin                 (INIL),
          _iygridmax                 (INIL)
{
  assert(_informer && _ua && _transform && _traces);
  _select = new SimpleSelect();
}



//------------------------ destructor --------------------------//
//------------------------ destructor --------------------------//
//------------------------ destructor --------------------------//

Midpoints::~Midpoints()
{
  midpointsChanging();
  delete _select;
}



//----------------------- get values -----------------------------//
//----------------------- get values -----------------------------//
//----------------------- get values -----------------------------//

   // public.
   // these need _ntraces, _ncmps, and _first[].
   // some of these also needs _trums[] or _binettes[] or _xloc or _yloc.

   // these should be called only after cmpSort has been called,
   // and before any subsequent changes to the grid transformation
   // or to the midpoints of any traces.


long Midpoints::numSelectedCmpGathers()  const
{
  return _select->numSelected();
}



long Midpoints::foldOfStack(long ixcmp)  const
{
  if(ixcmp < 0 || ixcmp >= _ncmps) return 0;
  assert(_nfold);
  return _nfold[ixcmp];
}


long Midpoints::liveFoldOfStack(long ixcmp)  const
{
  if(ixcmp < 0 || ixcmp >= _ncmps) return 0;
  if(!_nlive) return INIL;
  return _nlive[ixcmp];
}


long Midpoints::headerWord3(long ixcmp)  const
{
  if(ixcmp < 0 || ixcmp >= _ncmps) return 0;
  if(!_hwd3) return INIL;
  return _hwd3[ixcmp];
}



char Midpoints::getCmpSelectValue(long ixcmp)  const
{
  if(ixcmp < 0 || ixcmp >= _ncmps) return 0;
  assert(_sel);
  return _select->getSelectValue(ixcmp);
}



int Midpoints::cmpIsSelected(long ixcmp)  const
{
  if(ixcmp < 0 || ixcmp >= _ncmps) return FALSE;
  assert(_sel);
  return _select->isSelected(ixcmp);
}



int Midpoints::cmpFattestBinette(long ixcmp)  const
{
  if(ixcmp < 0 || ixcmp >= _ncmps) return 0;
  if(!_fattest) return 0;
  return _fattest[ixcmp];
}



long Midpoints::originalTraceIndex(long ixcmp, long ixfold)  const
{
  long nfold = foldOfStack(ixcmp);
  if(ixfold < 0 || ixfold >= nfold) return -1;
  long ixsorted = _first[ixcmp] + ixfold - 1;
  assert(ixsorted >= 0 && ixsorted < _ntraces);
  assert(_trums);
  return _trums[ixsorted];
}



long Midpoints::originalTraceIndex(long ixsorted)  const
{
  if(_ncmps == 0) return -1;
  assert(ixsorted >= 0 && ixsorted < _ntraces);
  assert(_trums);
  return _trums[ixsorted];
}



/************************************
      ////// working on this stuff...
      ////// working on this stuff...
      ////// working on this stuff...
      ////// will need to keep additional arrays...
      ////// will need to keep additional arrays...
      ////// will need to keep additional arrays...

long Midpoints::sortedTraceIndex(long ixorig)  const
{
  long ixcmp = traceCmpIndex(ixorig);
  if(ixcmp == -1) return -1;

  long ixfirst = _first[ixcmp];
  
  long ixsorted = _first[ixcmp] + ixfold;
  return ixsorted;
}


long Midpoints::traceIndexInCmp(long ixorig)  const
{
  long ixcmp = traceCmpIndex(ixorig);
  if(ixcmp == -1) return -1;

  return ixfold;
}


long Midpoints::traceCmpIndex(long ixorig)  const
{
  if(_ncmps == 0 || !_unique) return -1;
  assert(ixorig >= 0 && ixorig < _ntraces);
  long ixcmp = _unique[ixorig];
  return ixcmp;
}
************************************/



long Midpoints::findNearestCmp (double xloc, double yloc)  const
{
  if(_ncmps == 0 || xloc == DNIL || yloc == DNIL) return -1;
  long   ixcmp_nearest = 0;
  double dist          = 0.0;
  int    starting      = TRUE;
  for(long ixcmp = 0; ixcmp < _ncmps; ixcmp++)
      {
      double xloc2, yloc2;
      getCmpLocBinCenter(ixcmp, &xloc2, &yloc2);
      if(xloc2 == DNIL || yloc2 == DNIL) continue;
      double xdist = xloc2 - xloc;
      double ydist = yloc2 - yloc;
      double dist2 = xdist * xdist + ydist * ydist;
      if(starting || dist2 < dist)
          {
          ixcmp_nearest = ixcmp;
          dist          = dist2;
          starting      = FALSE;
          }
      }
  return ixcmp_nearest;
}



long Midpoints::getMatchingCmp (double xloc, double yloc)  const
{
  double xgrid, ygrid;
  _transform->getGridCoords(xloc, yloc, &xgrid, &ygrid);
  return getMatchingCmpUsingGrid(xgrid, ygrid);
}



long Midpoints::findNearestCmpUsingGrid (double xgrid, double ygrid)  const
{
  double xloc, yloc;
  _transform->getDistanceCoords(xgrid, ygrid, &xloc, &yloc);
  return findNearestCmp(xloc, yloc);
}



long Midpoints::getMatchingCmpUsingGrid (double xgrid, double ygrid)  const
{
  if(_nxbins == 0 || _nybins == 0) return -1;
  long ixgrid = NearestInteger(xgrid);
  long iygrid = NearestInteger(ygrid);
  if(ixgrid < _ixgridmin || ixgrid > _ixgridmax) return -1;
  if(iygrid < _iygridmin || iygrid > _iygridmax) return -1;
  long ixcmp = (ixgrid - _ixgridmin) + _nxbins * (iygrid - _iygridmin);
  if(_first_bin_has_nil_coords) ixcmp++;
  assert(ixcmp >= 0 && ixcmp < _ncmps);
  return ixcmp;
}



void Midpoints::getCmpLocBinCenter (long ixcmp,
                                double *xloc, double *yloc)  const
{
  double xgrid, ygrid;
  getCmpGridBinCenter(ixcmp, &xgrid, &ygrid);
  _transform->getDistanceCoords(xgrid, ygrid, xloc, yloc);
}



void Midpoints::getCmpGridBinCenter (long ixcmp,
                              double *xgrid, double *ygrid)  const
{
  assert(ixcmp >= 0 && ixcmp < _ncmps);
  if(_first_bin_has_nil_coords)
      {
      if(ixcmp == 0)
          {
          *xgrid = DNIL;
          *ygrid = DNIL;
          return;
          }
      ixcmp--;
      }
  assert(_nxbins > 0);
  long iygrid = ixcmp / _nxbins;
  long ixgrid = ixcmp - _nxbins * iygrid;
  *xgrid = _xgridmin + (double)NearestInteger(ixgrid);
  *ygrid = _ygridmin + (double)NearestInteger(iygrid);
}



void Midpoints::getCmpTraceLoc (long ixcmp, long ixfold,
                              double *xloc, double *yloc)  const
{
  long ixorig = originalTraceIndex(ixcmp, ixfold);
  if(ixorig == -1)
      {
      *xloc = DNIL;
      *yloc = DNIL;
      return;
      }
  long ixloc = _traces->_xmids[ixorig];
  long iyloc = _traces->_ymids[ixorig];
  if(ixloc == INIL) *xloc = DNIL; else *xloc = (double)ixloc;
  if(iyloc == INIL) *yloc = DNIL; else *yloc = (double)iyloc;
}



void Midpoints::getCmpTraceGrid (long ixcmp, long ixfold,
                              double *xgrid, double *ygrid)  const
{
  double xloc, yloc;
  getCmpTraceLoc(ixcmp, ixfold, &xloc, &yloc);
  _transform->getGridCoords(xloc, yloc, xgrid, ygrid);
}



float Midpoints::getCmpTraceOffset (long ixcmp, long ixfold)  const
{
  long ixorig = originalTraceIndex(ixcmp, ixfold);
  if(ixorig == -1) return FNIL;
  long ioffset = _traces->_offsets[ixorig];
  if(ioffset == INIL) return FNIL;
  return (float)ioffset;
}


int Midpoints::getCmpTraceBinette (long ixcmp, long ixfold)  const
{
  if(!_binettes) return 0;
  long ixorig = originalTraceIndex(ixcmp, ixfold);
  if(ixorig == -1) return 0;
  return _binettes[ixorig];
}



//----------------------- set values -----------------------------//
//----------------------- set values -----------------------------//
//----------------------- set values -----------------------------//

       // public.

   // midpointsChanging should be called whenever any coordinates
   //   change due to changes in FieldGeometry.
   // this causes future attempts to get CMP gather data to
   //   return 0 (indicating that CMP gathers are out of date).

void Midpoints::midpointsChanging()
{
  transformChanging();
}


void Midpoints::deadCodesChanging()
{
  if(!_nlive) return;
  delete [] _nlive;
  _nlive = NULL;
  _informer->liveFoldOutOfDate();
}



void Midpoints::transformChanging()
{
  deadCodesChanging();
  if(!_trums) return;

  if(_active != -1)
      {
      _informer->preNewActiveCmp();
      _active = -1;
      _informer->postNewActiveCmp();
      }

  _informer->preCmpSelectionsChanged();
  _select->registerSelectArray(NULL);      // argument _sel will be NULL.
  _select->clearSelections(0);             // argument _ncmps will be 0.
  _informer->postCmpSelectionsChanged();

  if(_trums   ) delete [] _trums   ;
  if(_binettes) delete [] _binettes;
  if(_first   ) delete [] _first   ;
  if(_nfold   ) delete [] _nfold   ;
  if(_hwd3    ) delete [] _hwd3    ;
  if(_fattest ) delete [] _fattest ;
  if(_sel     ) delete [] _sel     ;
  _binettes  = NULL;
  _trums     = NULL;
  _first     = NULL;
  _nfold     = NULL;
  _hwd3      = NULL;
  _fattest   = NULL;
  _sel       = NULL;
  _ntraces   = 0;
  _ncmps     = 0;
  _nxbins    = 0;
  _nybins    = 0;
  _first_bin_has_nil_coords = FALSE;
  _foldmin   = 0;
  _foldmax   = 0;
  _xlocmin   = DNIL;
  _ylocmin   = DNIL;
  _xlocmax   = DNIL;
  _ylocmax   = DNIL;
  _xgridmin  = DNIL;
  _ygridmin  = DNIL;
  _xgridmax  = DNIL;
  _ygridmax  = DNIL;
  _ixgridmin = INIL;
  _iygridmin = INIL;
  _ixgridmax = INIL;
  _iygridmax = INIL;
  _informer->midpointGathersOutOfDate();
}



//----------------------- cmp sort ----------------------------//
//----------------------- cmp sort ----------------------------//
//----------------------- cmp sort ----------------------------//

  // public.

  // should be called after initializeMidpoints and setMidpoints
  // have been called.  this uses the current grid transformation
  // to sort to CMPs.  this can be called a number of times, after
  // the grid transformation has changed, without having to call
  // initializeMidpoints and setMidpoints again, so long that no
  // coordinates have changed in FieldGeometry.

  // temporarily allocates, uses, and deletes _unique[].
  // needs _ntraces and _xlocs[] and _ylocs[] and _offsets[].
  // allocates (or reallocates) and initializes and sorts _trums[].
  // allocates (or reallocates) _first[].
  // sets _ncmps and _first[].

  // if successful, returns 0.
  // if too many CMP bins would be created, returns the number
  //   of CMP bins which would have been created, but does not
  //   do anything more (except to set the bin limits).  this
  //   usually means that the fixdist parameter or the grid
  //   transformation has not been set properly.
  // if user abort, returns -1.

 /// NOTE: _ncmps must be temporarily set to zero while startAbortOption
 /// is being called from cmpSort because startAbortOption might cause the
 /// get values functions to be called, and cmpIsSelected will assert because
 /// _ncmps > 0 and _sel == NULL.  (beforeSorting sets _ncmps > 0, but _sel
 /// is allocated by afterSorting.)
 ///
 /// The startAbortOption function causes the event buffer to be flushed.
 /// If a window needs exposing at that time, its event handler will be
 /// called to cause the window to be redrawn.  Such an exposure will be
 /// required for example if a window is raised after beginning to create
 /// gathers, but before startAbortOption is called.
 ///
 /// If a windowbox table displaying CMP gathers requires exposing, the
 /// get values functions (including cmpIsSelected) will be called, causing
 /// the assert mentioned above unless _ncmps == 0.

long Midpoints::cmpSort()
{
  _ntraces = _traces->_ntraces;
  if(_ntraces == 0) return 0;
  beforeSorting();
  if(_ncmps > 1.5 * _ntraces)
      {
      long ngrid = _ncmps;
      _ncmps     = 0;
      _ntraces   = 0;
      return ngrid;
      }

  _informer->preUpdateMidpointGathers();
  _informer->preNewActiveCmp();
  _informer->preCmpSelectionsChanged();

  long temporary = _ncmps;
  _ncmps = 0;
  _ua->startAbortOption();
  _ncmps = temporary;

  assert(!_unique);
  _unique = new long [_ntraces];
  calculateGridCoords();

  if(!_ua->aborted())
      {
      switch(METHOD)
          {
          case MYSORT_METHOD  : useMysortMethod  (); break;
          case QSORT_METHOD   : useQsortMethod   (); break;
          case BINNING_METHOD : useBinningMethod (); break;
          case COUNTING_METHOD: useCountingMethod(); break;
          default             : assert(FALSE);
          }
      }

  if(!_ua->aborted())
      {
      afterSorting();
      }

  delete [] _unique;
  _unique = NULL;

  _ua->stopAbortOption();
  _informer->postCmpSelectionsChanged();
  _informer->postNewActiveCmp();
  _informer->postUpdateMidpointGathers();

  if(_ua->aborted())
      {
      midpointsChanging();
      return -1;
      }

  updateLiveFold();
  return 0;
}



//------------------- use mysort method ------------------------//
//------------------- use mysort method ------------------------//
//------------------- use mysort method ------------------------//

       // private.

void Midpoints::useMysortMethod()
{
  _informer->showMessage("sorting to CMP gathers (using MYSORT method)...");
  TEMP_unique  = _unique;             // temporary static variable.
  TEMP_trums   = _trums;              // temporary static variable.
  TEMP_offsets = _traces->_offsets;   // temporary static variable.
  generic_sort(_ntraces, mysortFun, this);
}



//------------------- use qsort method ------------------------//
//------------------- use qsort method ------------------------//
//------------------- use qsort method ------------------------//

       // private.

void Midpoints::useQsortMethod()
{
  _informer->showMessage("sorting to CMP gathers (using QSORT method)...");
  TEMP_unique  = _unique;             // temporary static variable.
  TEMP_trums   = _trums;              // temporary static variable.
  TEMP_offsets = _traces->_offsets;   // temporary static variable.
//THIS_data    = this;                // temporary static variable.
  qsort((void*)_trums, (unsigned int)_ntraces, sizeof(long), qsortFun);
}



//------------------- use binning method ------------------------//
//------------------- use binning method ------------------------//
//------------------- use binning method ------------------------//

       // private.
       // needs _ncmps and _ntraces.
       // needs _unique[] to be allocated and set.
       // needs _trums[] to be allocated.
       // sets  _trums[].

void Midpoints::useBinningMethod()
{
/*
  IntegerList *mmm[_ncmps];   // does not work with some compilers.
*/
  IntegerList **mmm = new IntegerList * [_ncmps];
  long ixcmp, ixorig;
  for(ixcmp = 0; ixcmp < _ncmps; ixcmp++)
      {
      mmm[ixcmp] = NULL;
      }

  _informer->showMessage("sorting to CMP gathers (using BINNING method)...");

  long first_step = 1 + _ntraces / _ncmps;
  long min_step = first_step;
  long max_step = 10 * first_step;
  IntegerList::setAllocationSteps(first_step, min_step, max_step);

  for(ixorig = 0; ixorig < _ntraces; ixorig++)
      {
      ixcmp = _unique[ixorig];
      mmm[ixcmp] = IntegerList::addElement(mmm[ixcmp], ixorig);
      }

  _informer->showMessage("sorting each CMP gather by offset...");

  TEMP_offsets = _traces->_offsets;   // temporary static variable.
  long ixcmp_start = 0;
  if(_first_bin_has_nil_coords) ixcmp_start = 1;
  for(ixcmp = ixcmp_start; ixcmp < _ncmps; ixcmp++)
      {
      long nfold = IntegerList::numElements(mmm[ixcmp]);
      if(nfold <= 1) continue;
      long *list = IntegerList::getListPointer(mmm[ixcmp]);
      qsort((void*)list, (unsigned int)nfold, sizeof(long), offsetSortFun);
      }

  _informer->showMessage("storing sorted order of traces...");

  ixorig = 0;
  for(ixcmp = 0; ixcmp < _ncmps; ixcmp++)
      {
      long nfold = IntegerList::numElements(mmm[ixcmp]);
      for(long ixfold = 0; ixfold < nfold; ixfold++)
          {
          _trums[ixorig++] = IntegerList::getElement(mmm[ixcmp], ixfold);
          }
      }
  assert(ixorig = _ntraces - 1);

  for(ixcmp = 0; ixcmp < _ncmps; ixcmp++)
      {
      IntegerList::clearList(mmm[ixcmp]);
      }
  IntegerList::setAllocationSteps();

  delete [] mmm;
}



//------------------- use counting method ------------------------//
//------------------- use counting method ------------------------//
//------------------- use counting method ------------------------//

       // private.
       // needs _ncmps and _ntraces.
       // needs _unique[] to be allocated and set.
       // needs _trums[] to be allocated.
       // sets  _trums[].

void Midpoints::useCountingMethod()
{
  _informer->showMessage("sorting to CMP gathers (using COUNTING method)...");
  _informer->showMessage("sorting to CMP gathers (first step)...");

  long *mmm = new long [_ncmps];
  long ixcmp, ixorig;
  for(ixcmp = 0; ixcmp < _ncmps; ixcmp++)
      {
      mmm[ixcmp] = 0;                         // initialize fold to zero.
      }

  _informer->showMessage("sorting to CMP gathers (second step)...");
  for(ixorig = 0; ixorig < _ntraces; ixorig++)
      {
      ixcmp = _unique[ixorig];
      assert(ixcmp >= 0 && ixcmp < _ncmps);
      mmm[ixcmp]++;                           // add to fold.
      _trums[ixorig] = -1;
      }

  _informer->showMessage("sorting to CMP gathers (third step)...");
  long sum = 0;
  for(ixcmp = 0; ixcmp < _ncmps; ixcmp++)
      {
      long fold = mmm[ixcmp];
      mmm[ixcmp] = sum;                  // now index of first trace in cmp.
      sum += fold;
      }

  _informer->showMessage("sorting to CMP gathers (fourth step)...");
  for(ixorig = 0; ixorig < _ntraces; ixorig++)
      {
      ixcmp = _unique[ixorig];
      long ixsorted = mmm[ixcmp];
      _trums[ixsorted] = ixorig;
      mmm[ixcmp]++;            // will become index of first trace in NEXT cmp.
      }

  _informer->showMessage("verifying sorted traces...");
  for(long ixsorted = 0; ixsorted < _ntraces; ixsorted++)
      {
      assert(_trums[ixsorted] != -1);
      }

  _informer->showMessage("sorting each CMP gather by offset...");

  TEMP_offsets = _traces->_offsets;   // temporary static variable.
  long ixcmp_start = 0;
  if(_first_bin_has_nil_coords) ixcmp_start = 1;
  for(ixcmp = ixcmp_start; ixcmp < _ncmps; ixcmp++)
      {
      long ixsorted;
      if(ixcmp == 0) ixsorted = 0;
      else           ixsorted = mmm[ixcmp - 1];
      long nfold = mmm[ixcmp] - ixsorted;
      if(nfold <= 1) continue;
      long *list = &_trums[ixsorted];
/*
          printf("cmp=%d  fold=%d  first=%d :", ixcmp, nfold, ixsorted);
          for(long i = 0; i < nfold; i++)
                { printf("  %d", list[i]); }
          printf("\n");
*/
      qsort((void*)list, (unsigned int)nfold, sizeof(long), offsetSortFun);
      }

  delete [] mmm;
}






//------------------- set values ---------------------------//
//------------------- set values ---------------------------//
//------------------- set values ---------------------------//

       // public.

void Midpoints::setActiveCmpIndex(long ixcmp)
{
  if(ixcmp == _active) return;
  assert(ixcmp >= 0 && ixcmp < _ncmps);
  _informer->preNewActiveCmp();
  _active = ixcmp;
  _informer->postNewActiveCmp();
}



void Midpoints::setCmpSelectValue(long ixcmp, char value)
{
  if(_ncmps == 0) return;
  assert(ixcmp >= 0 && ixcmp < _ncmps);
  assert(_sel);
  _informer->preCmpSelectionsChanged();
  _select->setSelectValue(ixcmp, value);
  _select->updateSelections(_ncmps);
  _informer->postCmpSelectionsChanged();
}



void Midpoints::incrementCmpSelectValue(long ixcmp)
{
  if(_ncmps == 0) return;
  assert(ixcmp >= 0 && ixcmp < _ncmps);
  assert(_sel);
  _informer->preCmpSelectionsChanged();
  _select->incrementSelectValue(ixcmp);
  _select->updateSelections(_ncmps);
  _informer->postCmpSelectionsChanged();
}



void Midpoints::clearCmpSelections()
{
  if(_ncmps == 0) return;
  _informer->preCmpSelectionsChanged();
  _select->clearSelections(_ncmps);
  _informer->postCmpSelectionsChanged();
}



//--------------- increment grid coords -------------------------//
//--------------- increment grid coords -------------------------//
//--------------- increment grid coords -------------------------//

   // public.
   // this must be called anytime _transform->incrementGridCoords()
   //   is called, with the same (but integerized) argument values.
   // this function resets the minimum and maximum grid limits only.
   // this function will not put CMP gathers out-of_date.

void Midpoints::incrementGridCoords(long ixstep, long iystep)
{
  if( _xgridmin != DNIL)  _xgridmin += ixstep;
  if( _xgridmax != DNIL)  _xgridmax += ixstep;
  if( _ygridmin != DNIL)  _ygridmin += iystep;
  if( _ygridmax != DNIL)  _ygridmax += iystep;
  if(_ixgridmin != INIL) _ixgridmin += ixstep;
  if(_ixgridmax != INIL) _ixgridmax += ixstep;
  if(_iygridmin != INIL) _iygridmin += iystep;
  if(_iygridmax != INIL) _iygridmax += iystep;
}



//--------------------- before sorting ------------------------//
//--------------------- before sorting ------------------------//
//--------------------- before sorting ------------------------//

     // private.
     // finds minimum/maximum X/Y limits from all traces.
     // returns the total number of bins implied by these limits.
     // needs the following variables:
     //    _ntraces
     //    _traces->_xmids[]
     //    _traces->_ymids[]
     // sets the following variables:
     //    _first_bin_has_nil_coords
     //     _xgridmin  _xgridmax  _ygridmin  _ygridmax
     //    _ixgridmin _ixgridmax _iygridmin _iygridmax
     //      _xlocmin   _xlocmax   _ylocmin   _ylocmax
     //    _nxbins _nybins _ncmps

void Midpoints::beforeSorting()
{
  _informer->showMessage("finding CMP bin center coordinate limits...");
  _xgridmin = DNIL;
  _xgridmax = DNIL;
  _ygridmin = DNIL;
  _ygridmax = DNIL;
  _ixgridmin = INIL;
  _ixgridmax = INIL;
  _iygridmin = INIL;
  _iygridmax = INIL;
  _first_bin_has_nil_coords = FALSE;
  _nxbins = 0;
  _nybins = 0;
  _ncmps  = 0;
  _xlocmin = DNIL;
  _xlocmax = DNIL;
  _ylocmin = DNIL;
  _ylocmax = DNIL;

///////////// get grid coordinates:

  long starting = TRUE;
  for(long ixorig = 0; ixorig < _ntraces; ixorig++)
      {
      long x, y;
      long xloc = _traces->_xmids[ixorig];
      long yloc = _traces->_ymids[ixorig];
      _transform->getGridCoordsUsingIntegers(xloc, yloc, &x, &y);
      if(x == INIL || y == INIL)
          {
          _first_bin_has_nil_coords = TRUE;
          }
      else if(starting)
          {
          _ixgridmin = x;
          _ixgridmax = x;
          _iygridmin = y;
          _iygridmax = y;
          starting = FALSE;
          }
      else
          {
          _ixgridmin = MinimumValue(x, _ixgridmin);
          _ixgridmax = MaximumValue(x, _ixgridmax);
          _iygridmin = MinimumValue(y, _iygridmin);
          _iygridmax = MaximumValue(y, _iygridmax);
          }
      }
  if(starting)
      {
      assert(_first_bin_has_nil_coords);
      _ncmps = 1;
      return;
      }
  _xgridmin = (double)_ixgridmin;
  _xgridmax = (double)_ixgridmax;
  _ygridmin = (double)_iygridmin;
  _ygridmax = (double)_iygridmax;
  _nxbins = _ixgridmax - _ixgridmin + 1;
  _nybins = _iygridmax - _iygridmin + 1;
  _ncmps = _nxbins * _nybins;
  if(_first_bin_has_nil_coords) _ncmps++;

///////////// get distance coordinates:

/*
          //// the following removed 9/24/96:
  starting         = TRUE;
  long ixcmp_start = 0;
  if(_first_bin_has_nil_coords) ixcmp_start = 1;
  for(long ixcmp = ixcmp_start; ixcmp < _ncmps; ixcmp++)
      {
      double xloc, yloc;
      getCmpLocBinCenter(ixcmp, &xloc, &yloc);
      if(starting)
          {
          _xlocmin = xloc;
          _xlocmax = xloc;
          _ylocmin = yloc;
          _ylocmax = yloc;
          starting = FALSE;
          }
      else
          {
          _xlocmin = MinimumValue(xloc, _xlocmin);
          _xlocmax = MaximumValue(xloc, _xlocmax);
          _ylocmin = MinimumValue(yloc, _ylocmin);
          _ylocmax = MaximumValue(yloc, _ylocmax);
          }
      }
*/
          //// the following inserted 9/24/96:
  double xloc1, yloc1;
  double xloc2, yloc2;
  double xloc3, yloc3;
  double xloc4, yloc4;
  _transform->getDistanceCoords(_xgridmin, _ygridmin, &xloc1, &yloc1);
  _transform->getDistanceCoords(_xgridmax, _ygridmin, &xloc2, &yloc2);
  _transform->getDistanceCoords(_xgridmin, _ygridmax, &xloc3, &yloc3);
  _transform->getDistanceCoords(_xgridmax, _ygridmax, &xloc4, &yloc4);
  _xlocmin = xloc1;
  if(xloc2 < _xlocmin) _xlocmin = xloc2;
  if(xloc3 < _xlocmin) _xlocmin = xloc3;
  if(xloc4 < _xlocmin) _xlocmin = xloc4;
  _xlocmax = xloc1;
  if(xloc2 > _xlocmax) _xlocmax = xloc2;
  if(xloc3 > _xlocmax) _xlocmax = xloc3;
  if(xloc4 > _xlocmax) _xlocmax = xloc4;
  _ylocmin = yloc1;
  if(yloc2 < _ylocmin) _ylocmin = yloc2;
  if(yloc3 < _ylocmin) _ylocmin = yloc3;
  if(yloc4 < _ylocmin) _ylocmin = yloc4;
  _ylocmax = yloc1;
  if(yloc2 > _ylocmax) _ylocmax = yloc2;
  if(yloc3 > _ylocmax) _ylocmax = yloc3;
  if(yloc4 > _ylocmax) _ylocmax = yloc4;
}



//------------------ calculate grid coords --------------------//
//------------------ calculate grid coords --------------------//
//------------------ calculate grid coords --------------------//

     // private.
     // needs _unique[ntraces] to be allocated.
     // needs _ntraces.
     // needs _traces->_xmids[ntraces].
     // needs _traces->_ymids[ntraces].
     // allocates    _trums[ntraces].
     // initializes  _trums[ntraces] to the original trace index.
     // sets        _unique[ntraces] to the CMP index.
     // allocates _binettes[ntraces].
     // sets      _binettes[ntraces] to the binette number for each trace.

void Midpoints::calculateGridCoords()
{
  _informer->showMessage("calculating grid coordinates...");
  assert(_ntraces > 0);
  assert(_ncmps > 0);
  assert(_unique);
  if(_trums) delete [] _trums;
  _trums = new long [_ntraces];
  if(_binettes) delete [] _binettes;
  _binettes = new char [_ntraces];
  for(long ixorig = 0; ixorig < _ntraces; ixorig++)
      {
      _trums[ixorig] = ixorig;
      long x, y;
      long xloc = _traces->_xmids[ixorig];
      long yloc = _traces->_ymids[ixorig];
      _transform->getGridCoordsUsingIntegers(xloc, yloc, &x, &y,
                                                 &_binettes[ixorig]);
      long ixcmp;
      if(x == INIL || y == INIL)
          {
          assert(_first_bin_has_nil_coords);
          ixcmp = 0;
          }
      else
          {
          ixcmp = (x - _ixgridmin) + _nxbins * (y - _iygridmin);
          if(_first_bin_has_nil_coords) ixcmp++;
          }
      assert(ixcmp >= 0 && ixcmp < _ncmps);
      _unique[ixorig] = ixcmp;
      }
}



//--------------------- after sorting ----------------------//
//--------------------- after sorting ----------------------//
//--------------------- after sorting ----------------------//

     // private.
     // needs _ntraces, _trums[], and _unique[].
     // allocates (or reallocates) _first[].
     // sets _ncmps and _first[].

void Midpoints::afterSorting()
{
////////// allocate and initialize arrays:

  if(_first  ) delete [] _first  ;
  if(_nfold  ) delete [] _nfold  ;
  if(_hwd3   ) delete [] _hwd3   ;
  if(_fattest) delete [] _fattest;
  if(_sel    ) delete [] _sel    ;
  _first   = new long [_ncmps];
  _nfold   = new long [_ncmps];
  _hwd3    = new long [_ncmps];
  _fattest = new char [_ncmps];
  _sel     = new char [_ncmps];
  _select->registerSelectArray(_sel);
  _select->clearSelections(_ncmps);
  _active = 0;

////////// find first trace of each gather:

  _informer->showMessage("finding first trace and fold of each gather...");
  long ixcmp;
  for(ixcmp = 0; ixcmp < _ncmps; ixcmp++)
      {
      _first  [ixcmp] =    0; // will remain  0  if gather has no traces.
      _nfold  [ixcmp] =    0; // will remain  0  if gather has no traces.
      _hwd3   [ixcmp] = INIL; // will remain nil if gather has no traces,
                              //    or this gather has nil coordinates.
      _fattest[ixcmp] =    0; // will remain  0  if gather has no traces,
                              //    or this gather has nil coordinates.
      }
  for(long ixsorted = 0; ixsorted < _ntraces; ixsorted++)
      {
      long ixorig = _trums[ixsorted];
      long ixcmp = _unique[ixorig];
      assert(ixcmp >= 0 && ixcmp < _ncmps);
      if(_first[ixcmp] == 0) _first[ixcmp] = ixsorted+1;
      _nfold[ixcmp]++;
      }

////////// get minimum and maximum fold and header word 3:

  _informer->showMessage("finding fold limits and header word 3...");
  _foldmin         = 0;
  _foldmax         = 0;
  int  starting    = TRUE;
  long kount       = 0;
  long ixcmp_start = 0;
  if(_first_bin_has_nil_coords) ixcmp_start = 1;
  for(ixcmp = ixcmp_start; ixcmp < _ncmps; ixcmp++)
      {
      long nfold = _nfold[ixcmp];
      if(starting)
          {
          _foldmin = nfold;
          _foldmax = nfold;
          starting = FALSE;
          }
      else
          {
          _foldmin = MinimumValue(nfold, _foldmin);
          _foldmax = MaximumValue(nfold, _foldmax);
          }
      if(nfold > 0) _hwd3[ixcmp] = ++kount;
      }

////////// get fattest binette:

  _informer->showMessage("finding most populated sub-bins...");

  for(ixcmp = ixcmp_start; ixcmp < _ncmps; ixcmp++)
      {
      long nfold = _nfold[ixcmp];
      if(nfold > 0)
          {
          int i, k[9];
          for(i = 0; i < 9; i++)
              {
              k[i] = 0;
              }
          for(long ixfold = 0; ixfold < nfold; ixfold++)
              {
              int binette = getCmpTraceBinette(ixcmp, ixfold);
              if(binette >= 1 && binette <= 9) k[binette - 1]++;
              }
          int maxfat = 0;
          for(i = 0; i < 9; i++)
              {
              if(k[i] > maxfat)
                  {
                  _fattest[ixcmp] = i + 1;
                  maxfat = k[i];
                  }
              }
          }
      }
}



//------------------- update live fold --------------------//
//------------------- update live fold --------------------//
//------------------- update live fold --------------------//

    // public.
    // get live fold for each gather.

void Midpoints::updateLiveFold()
{
  if(_ncmps == 0 || !_first || !_trums || !_nfold) return;
  _informer->preUpdateLiveFold();
  _informer->showMessage("finding live fold of each gather...");

  if(_nlive) delete [] _nlive;
  _nlive = new long [_ncmps];

  for(long ixcmp = 0; ixcmp < _ncmps; ixcmp++)
      {
      long nfold = _nfold[ixcmp];
      long nlive = 0;
      for(long ixfold = 0; ixfold < nfold; ixfold++)
          {
          long ixsorted = _first[ixcmp] + ixfold - 1;
          long ixorig = _trums[ixsorted];
          int dead = _traces->_dead[ixorig];
          if(dead == ZT_CODE_LIVE ||
             dead == ZT_CODE_REV  ||
             dead == ZT_CODE_NONE) nlive++;
          }
      _nlive[ixcmp] = nlive;
      }

  _informer->postUpdateLiveFold();
}



//------------------------ mysort fun --------------------------//
//------------------------ mysort fun --------------------------//
//------------------------ mysort fun --------------------------//

  // private static function used for sorting.
  // needs _trums[] and _unique[].
  // might interchange two elements of _trums[].
  // the un-commented code should be faster.

  // lo and up = indices from 1 thru _ntraces.
  // returns TRUE  if the values are switched.
  // returns FALSE if the values are not switched.

int Midpoints::mysortFun(void*, long lo, long up)
{
  long ixorig1 = TEMP_trums[lo-1];
  long ixorig2 = TEMP_trums[up-1];
  long compare = TEMP_unique[ixorig2] - TEMP_unique[ixorig1];
  if(compare == 0)
      {
      compare = TEMP_offsets[ixorig2] -
                TEMP_offsets[ixorig1];
      }
  if(compare < 0)
      {
      TEMP_trums[lo-1] = ixorig2;
      TEMP_trums[up-1] = ixorig1;
      return TRUE;
      }
  return FALSE;
}

/******
int Midpoints::mysortFun(void *data, long lo, long up)
{
  Midpoints *THIS = (Midpoints*)data;
  long ixorig1 = THIS->_trums[lo-1];
  long ixorig2 = THIS->_trums[up-1];
  long compare = THIS->_unique[ixorig2] - THIS->_unique[ixorig1];
  if(compare == 0)
      {
      compare = THIS->_traces->_offsets[ixorig2] -
                THIS->_traces->_offsets[ixorig1];
      }
  if(compare < 0)
      {
      THIS->_trums[lo-1] = ixorig2;
      THIS->_trums[up-1] = ixorig1;
      return TRUE;
      }
  return FALSE;
}
******/



//------------------------ qsort fun --------------------------//
//------------------------ qsort fun --------------------------//
//------------------------ qsort fun --------------------------//

  // private static function used for sorting with qsort.
  // needs _trums[] and _unique[] and _offsets[].
  // the un-commented code should be faster.

  // element1 = pointer to _trums[i1]  where i1 >= 0 && i1 < _ntraces.
  // element2 = pointer to _trums[i2]  where i2 >= 0 && i2 < _ntraces.
  // returns >= -1 if CMP bin  for _trums[i2] comes after  that for _trums[i1].
  // returns <= +1 if CMP bin  for _trums[i2] comes before that for _trums[i1].
  // returns ==  0 if CMP bins for _trums[i2] and _trums[i1] are the same.

int Midpoints::qsortFun(const void *element1, const void *element2)
{
  long ixorig1 = *(long*)element1;
  long ixorig2 = *(long*)element2;
  long compare = TEMP_unique[ixorig1] -
                 TEMP_unique[ixorig2];
  if(compare == 0)
      {
      compare = TEMP_offsets[ixorig1] -
                TEMP_offsets[ixorig2];
      }
  if(compare == 0)
      {
      compare = ixorig1 - ixorig2;
      }
  return (int)compare;
}

/******
int Midpoints::qsortFun(const void *element1, const void *element2)
{
  long ixorig1 = *(long*)element1;
  long ixorig2 = *(long*)element2;
  long compare = THIS_data->_unique[ixorig1] -
                 THIS_data->_unique[ixorig2];
  if(compare == 0)
      {
      compare = THIS_data->_traces->_offsets[ixorig1] -
                THIS_data->_traces->_offsets[ixorig2];
      }
  if(compare == 0)
      {
      compare = ixorig1 - ixorig2;
      }
  return (int)compare;
}
******/



//------------------------ offset sort fun --------------------------//
//------------------------ offset sort fun --------------------------//
//------------------------ offset sort fun --------------------------//

  // private static function used for sorting offsets with qsort.
  // needs _trums[] and _unique[] and _offsets[].

  // element1 = pointer to list[i1]  where i1 >= 0 && i1 < nfold.
  // element2 = pointer to list[i2]  where i2 >= 0 && i2 < nfold.
  // returns >= -1 if offset  for list[i2] comes after  that for list[i1].
  // returns <= +1 if offset  for list[i2] comes before that for list[i1].
  // returns ==  0 if offsets for list[i2] and list[i1] are the same.

int Midpoints::offsetSortFun(const void *element1, const void *element2)
{
  long ixorig1 = *(long*)element1;
  long ixorig2 = *(long*)element2;
  long compare = TEMP_offsets[ixorig1] -
                 TEMP_offsets[ixorig2];
  if(compare == 0)
      {
      compare = ixorig1 - ixorig2;
      }
  return (int)compare;
}



//-------------------------- end -------------------------------//
//-------------------------- end -------------------------------//
//-------------------------- end -------------------------------//

