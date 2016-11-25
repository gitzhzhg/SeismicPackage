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
//------------------------- CpseisBase.cc ---------------------------//
//------------------------- CpseisBase.cc ---------------------------//
//------------------------- CpseisBase.cc ---------------------------//

// This is the base class for a C++ wrapper around CPSeis processing modules.
// This wrapper converts a Fortran-90 module to a C++ class.
// This wrapper differs from the corresponding Fortran-90 module
// as follows:
//  (1) Required initializations are performed by calling cps_startup.
//  (2) The Fortran-90 module is created (if necessary) in the update
//       routine instead of the constructor so the parameter cache is
//       not needed by the constructor.
//  (3) Setup and wrapup summaries are printed.
//  (4) Some header words are set or adjusted.
//  (5) The use of the oneset or twosets execution routines is hidden
//       inside the class, relieving the processing system of needing
//       to know about them.  This is accomplished by hiding the 2D
//       trace and header arrays inside this class, and allowing the
//       processing system to pass just one trace at a time.  This
//       also increases flexibility (and might reduce memory use) since
//       the processing system probably stores trace gathers differently.
//  (6) The Fortran-90 module is protected from requests for traces if
//       it is not prepared to deal with the requests.
//  (7) Handles setup-only processes transparently.

#include "CpseisBase.hh"
#include "PCW.hh"
#include "named_constants.h"
#include <math.h>
#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <assert.h>

//-------------- fortran spelling adjustments and prototypes -------------//
//-------------- fortran spelling adjustments and prototypes -------------//
//-------------- fortran spelling adjustments and prototypes -------------//

#if NEED_UNDERSCORE
#define cps_startup      cps_startup_
#elif NEED_CAPITALS
#define cps_startup      CPS_STARTUP
#endif

extern "C"
    {
    void cps_startup();
    }

//---------------------------- constructor ----------------------------//
//---------------------------- constructor ----------------------------//
//---------------------------- constructor ----------------------------//

CpseisBase::CpseisBase (const char    *name,
                        ModuleCreate  *create,
                        ModuleDestroy *destroy,
                        ModuleUpdate  *update,
                        ModuleWrapup  *wrapup,
                        ModuleOneset  *oneset,
                        ModuleTwosets *twosets)
         :
            _name           (name),
            _label          (name),
            _create         (create),
            _destroy        (destroy),
            _update         (update),
            _wrapup         (wrapup),
            _oneset         (oneset),
            _twosets        (twosets),

            _created        (FALSE),
            _need_label     (FALSE),
            _need_request   (FALSE),
            _setup_only     (FALSE),

            _nwih1          (0),
            _ndpt1          (0),
            _numtr1         (0),
            _lenhd1         (0),
            _lentr1         (0),
            _maxtr1         (0),
            _hd1            (NULL),
            _tr1            (NULL),

            _nwih2          (0),
            _ndpt2          (0),
            _numtr2         (0),
            _lenhd2         (0),
            _lentr2         (0),
            _maxtr2         (0),
            _hd2            (NULL),
            _tr2            (NULL),

            _ntraces1       (0),
            _ntraces2       (0),
            _ngathers1      (0),
            _ngathers2      (0)
{
  assert(_name);
  assert(_create);
  assert(_destroy);
  assert(_update);
  assert(_wrapup);
  assert(_oneset);
  assert(_twosets);

  assert(sizeof(INTEGER) == sizeof(int   ));    // to simplify coding.
  assert(sizeof(REAL   ) == sizeof(float ));    // to simplify coding.
  assert(sizeof(DOUBLE ) == sizeof(double));    // to simplify coding.
}

//----------------------- destructor ----------------------------//
//----------------------- destructor ----------------------------//
//----------------------- destructor ----------------------------//

CpseisBase::~CpseisBase()
{
  wrapup();
  _destroy(&_fpoint);
}

//------------------------- wrapup ------------------------------//
//------------------------- wrapup ------------------------------//
//------------------------- wrapup ------------------------------//

void CpseisBase::wrapup()
{
  if(!_hd1) return;

  printf("\n");
  printf("+++++++++++++++++++++ wrapup for module %s +++++++++++++++++++++\n", _label);
  printf("number of gathers and traces input  = %d   %d   mean abs val = %f\n", _ngathers1, _ntraces1, _mav1 / MAX(_ntraces1, 1));
  printf("number of gathers and traces output = %d   %d   mean abs val = %f\n", _ngathers2, _ntraces2, _mav2 / MAX(_ntraces2, 1));

  freeBuffers();
  _wrapup(&_fpoint);

  printf("+++++++++++++++++++ end wrapup for module %s +++++++++++++++++++\n", _label);
  printf("\n");
}

//------------------------- free buffers -------------------------//
//------------------------- free buffers -------------------------//
//------------------------- free buffers -------------------------//

void CpseisBase::freeBuffers()
{
  if(_hd1 == NULL)
      {
      }
  else if(_hd1 == _hd2)
      {
      delete [] _hd1; _hd1 = NULL; _hd2 = NULL;
      delete [] _tr1; _tr1 = NULL; _tr2 = NULL;
      }
  else
      {
      delete [] _hd1; _hd1 = NULL;
      delete [] _tr1; _tr1 = NULL;
      delete [] _hd2; _hd2 = NULL;
      delete [] _tr2; _tr2 = NULL;
      }
}

//------------------------------- update --------------------------//
//------------------------------- update --------------------------//
//------------------------------- update --------------------------//

// Before calling update:
// (1) Call PCW.frontendUpdateNoprint() or PCW.backendUpdate().
//     Noprint puts -6 into the parameter cache.  This allows
//     the CPS process to get unit number +6 from the parameter
//     cache even though the parameter cache will not print any
//     messages.
// (2) Then put parameters into the parameter cache.

// After calling update:
// (1) Get parameters from the parameter cache.
// (2) Then call PCW.restore().
// (3) Then if this is a backend update, do the following, so that one
//     instance will be available for all processes during the run phase.
//     Must be done here while everything is still synchronous.
//        if(PCW.exists() == false)
//          {
//          PCW.backendUpdate();
//          PCW.backendExecute();
//          }
//     It is OK if someone else does a PCW.backendUpdate() and
//     PCW.restore() combination after this point because that
//     will not affect the state of the parameter cache at
//     this point, since the state here will be restored.

void CpseisBase::update()
{
  int am_in_setup = !PCW::doNotProcessTraces();

  if(am_in_setup)
      {
      printf("\n");
      printf("+++++++++++++++++++++ setup for module %s +++++++++++++++++++++\n", _label);
      PCW::printGlobalCards();
      }

  cps_startup();  // requires parameter cache to be initialized.

  int two = FALSE;

  PCW::getGlobal   ("nwih"   , &_nwih1);
  PCW::getGlobal   ("ndpt"   , &_ndpt1);
  PCW::getGlobal   ("numtr"  , &_numtr1);

  if(_created) _update(&_fpoint);
  else         _create(&_fpoint);

  _created = 1;

  PCW::getControlL ("twosets"      , &two);
  PCW::getControlL ("need_label"   , &_need_label);
  PCW::getControlL ("need_request" , &_need_request);
  PCW::getControlL ("setup_only"   , &_setup_only);
  PCW::getGlobal   ("nwih"         , &_nwih2);
  PCW::getGlobal   ("ndpt"         , &_ndpt2);
  PCW::getGlobal   ("numtr"        , &_numtr2);

  if(am_in_setup)
      {
      PCW::printProcessCards();
      PCW::printGlobalCards();
      printf("+++++++++++++++++++ end setup for module %s +++++++++++++++++++\n", _label);
      printf("\n");
      }

  freeBuffers();

  if(PCW::doNotProcessTraces()) return;  // not in setup, or error occurred in setup.

  if(two)
      {
      _lenhd1 = _nwih1;
      _lentr1 = _ndpt1;
      _maxtr1 = _numtr1;

      _lenhd2 = _nwih2;
      _lentr2 = _ndpt2;
      _maxtr2 = _numtr2;

      _hd1 = new double [_lenhd1 * _maxtr1];
      _tr1 = new float  [_lentr1 * _maxtr1];
      _hd2 = new double [_lenhd2 * _maxtr2];
      _tr2 = new float  [_lentr2 * _maxtr2];
      }
  else
      {
      _lenhd1 = (_nwih2  > _nwih1  ? _nwih2  : _nwih1);
      _lentr1 = (_ndpt2  > _ndpt1  ? _ndpt2  : _ndpt1);
      _maxtr1 = (_numtr2 > _numtr1 ? _numtr2 : _numtr1);

      _lenhd2 = _lenhd1;
      _lentr2 = _lentr1;
      _maxtr2 = _maxtr1;

      _hd1 = new double [_lenhd1 * _maxtr1];
      _tr1 = new float  [_lentr1 * _maxtr1];
      _hd2 = _hd1;
      _tr2 = _tr1;
      }

  _ntraces1  = 0;
  _ntraces2  = 0;
  _ngathers1 = 0;
  _ngathers2 = 0;
  _mav1      = 0.0;
  _mav2      = 0.0;
}

//---------------- get mean and largest absolute values ----------------//
//---------------- get mean and largest absolute values ----------------//
//---------------- get mean and largest absolute values ----------------//

static float static_compute_mean(float *tr, int ndpt)
{
  float mean = 0.0;
  float skip = 0.0;
  for(int isample = 0; isample < ndpt; isample++)
  {
      if     (tr[isample] > 0.0) mean +=  tr[isample];
      else if(tr[isample] < 0.0) mean += -tr[isample];
      else                        skip++;
  }
  if(skip == ndpt) return 0.0;
  return mean / (ndpt - skip);
}

static float static_compute_lav(float *tr, int ndpt)
{
  float lav = 0.0;
  for(int isample = 0; isample < ndpt; isample++)
  {
      if     (tr[isample] >  lav) lav =  tr[isample];
      else if(tr[isample] < -lav) lav = -tr[isample];
  }
  return lav;
}

//---------------------- static adjust headers ------------------------//
//---------------------- static adjust headers ------------------------//
//---------------------- static adjust headers ------------------------//

    // This function sets or adjusts the following header words:
    //    sets       hd[CPS_SEQUENCE]
    //    resets     hd[CPS_CURRENT_GROUP]   if it is zero.
    //    resets     hd[CPS_CURRENT_CHANNEL] if it is zero.
    //    constrains hd[CPS_TOP_MUTE]
    //    constrains hd[CPS_BOTTOM_MUTE]
    //    sets       hd[CPS_LAV]
    //    sets       hd[CPS_MIDPOINT_ELEV]
    //    resets     hd[CPS_MIDPOINT_XLOC]   if it is zero.
    //    resets     hd[CPS_MIDPOINT_YLOC]   if it is zero.
    //    resets     hd[CPS_OFFSET]          if it is zero.

    // If the CURRENT_GROUP and CURRENT_CHANNEL are zero, they are reset
    // based on whether the traces are gathered.  If traces are not gathered,
    // the CURRENT_GROUP will be the same as the SEQUENCE, and the current
    // channel will be 1.  This is different behavior from the way things
    // operate in the traditional CPS processing system, where CURRENT_GROUP
    // and CURRENT_CHANNEL are independent of whether the traces are gathered.
    // This means that the GATHER module will not work if it gathers on the
    // CURRENT_GROUP header word, which is the default.  Therefore, the
    // processing system should set CURRENT_GROUP and CURRENT_CHANNEL
    // based on the sort order of the data, if possible.

// ntraces is the previous number of traces received before this gather is started.
// ngathers is the previous number of gathers received before this gather is started.
// itr is the C-style trace index for this gather.

static void static_adjust_headers(int itr, double *hd, float *tr, int ndpt, int ntraces, int ngathers)
{
  hd[CPS_MIDPOINT_ELEV] = 0.5 * (hd[CPS_SOURCE_ELEV] + hd[CPS_RECEIVER_ELEV]);

  if(ABS(hd[CPS_MIDPOINT_XLOC]) < 0.5) hd[CPS_MIDPOINT_XLOC] = 0.5 * (hd[CPS_SOURCE_XLOC] + hd[CPS_RECEIVER_XLOC]);
  if(ABS(hd[CPS_MIDPOINT_YLOC]) < 0.5) hd[CPS_MIDPOINT_YLOC] = 0.5 * (hd[CPS_SOURCE_YLOC] + hd[CPS_RECEIVER_YLOC]);

  if (ABS(hd[CPS_OFFSET]) < 0.5)
      {
      double xx     = hd[CPS_SOURCE_XLOC] - hd[CPS_RECEIVER_XLOC];
      double yy     = hd[CPS_SOURCE_YLOC] - hd[CPS_RECEIVER_YLOC];
      double offset = sqrt(xx*xx + yy*yy);
      if (offset > 0.5) hd[CPS_OFFSET] = offset;
      }

  if (hd[CPS_TOP_MUTE]    <                 1) hd[CPS_TOP_MUTE]    =    1;  // constrain to valid index.
  if (hd[CPS_TOP_MUTE]    >              ndpt) hd[CPS_TOP_MUTE]    = ndpt;  // constrain to valid index.
  if (hd[CPS_BOTTOM_MUTE] <= hd[CPS_TOP_MUTE]) hd[CPS_BOTTOM_MUTE] = ndpt;  // assume not set if not > top mute.
  if (hd[CPS_BOTTOM_MUTE] >              ndpt) hd[CPS_BOTTOM_MUTE] = ndpt;  // constrain to valid index.

  if (ABS(hd[CPS_CURRENT_GROUP]) < 0.5)
      {
      hd[CPS_CURRENT_GROUP]   = ngathers + 1;
      hd[CPS_CURRENT_CHANNEL] = itr + 1;
      }
  else if (ABS(hd[CPS_CURRENT_CHANNEL]) < 0.5)
      {
      hd[CPS_CURRENT_CHANNEL] = itr + 1;
      }

  hd[CPS_SEQUENCE] = ntraces + itr + 1;
  hd[CPS_LAV]      = static_compute_lav(tr, ndpt);
}

//--------------------- put and get trace ---------------------------//
//--------------------- put and get trace ---------------------------//
//--------------------- put and get trace ---------------------------//

void CpseisBase::putTrace (int itr, double *hd, float *tr)
{
        // output             input
        //   |                  |
  memcpy (&_hd1[itr * _lenhd1], hd, _nwih1 * sizeof(double));
  memcpy (&_tr1[itr * _lentr1], tr, _ndpt1 * sizeof(float));

  static_adjust_headers(itr, &_hd1[itr * _lenhd1], tr, _ndpt1, _ntraces1, _ngathers1);
}

void CpseisBase::getTrace (int itr, double *hd, float *tr)
{
    //  output input
    //    |      |
  memcpy (hd, &_hd2[itr * _lenhd2], _nwih2 * sizeof(double));
  memcpy (tr, &_tr2[itr * _lentr2], _ndpt2 * sizeof(float));
}

//-------------------------- execute ---------------------------------//
//-------------------------- execute ---------------------------------//
//-------------------------- execute ---------------------------------//

int CpseisBase::execute(int ntr)
{
  if(_setup_only) return ntr;
  if(ntr == NEED_TRACES && !_need_label) return ntr;

  if(ntr > 0)
      {
      _ntraces1 += ntr;
      _ngathers1++;
      for(int i = 0; i < ntr; i++) _mav1 += static_compute_mean(&_tr1[_lentr1 * i], _ndpt1);
      }

  if(_hd1 == _hd2)
      {
      _oneset(&_fpoint, &ntr, _hd1, _tr1, &_lenhd1, &_lentr1, &_maxtr1);
      }
  else
      {
      _twosets(&_fpoint, &ntr, _hd1, _tr1, &_lenhd1, &_lentr1, &_maxtr1,
                               _hd2, _tr2, &_lenhd2, &_lentr2, &_maxtr2);
      }

  if(ntr > 0)
      {
      _ntraces2 += ntr;
      _ngathers2++;
      for(int i = 0; i < ntr; i++) _mav2 += static_compute_mean(&_tr2[_lentr2 * i], _ndpt2);
      }

  return ntr;
}

//---------------------------------- end ----------------------------------//
//---------------------------------- end ----------------------------------//
//---------------------------------- end ----------------------------------//
