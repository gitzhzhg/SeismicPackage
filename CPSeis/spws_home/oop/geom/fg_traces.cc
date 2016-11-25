
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
//--------------------- fg_traces.cc ---------------------//
//--------------------- fg_traces.cc ---------------------//
//--------------------- fg_traces.cc ---------------------//

//         implementation file for the FgTraces class
//                  not derived from any class
//                      subdirectory geom


#include "geom/fg_traces.hh"
#include "geom/fg_constants.hh"
#include "geom/fg_informer.hh"
#include "cprim.h"
#include "named_constants.h"
#include <assert.h>


//---------------------- constructor -----------------------//
//---------------------- constructor -----------------------//
//---------------------- constructor -----------------------//


FgTraces::FgTraces(FgInformer *informer)
           :
          _informer     (informer),
          _ntraces      (0),
          _grps         (NULL),
          _rflags       (NULL),
          _xmids        (NULL),
          _ymids        (NULL),
          _offsets      (NULL),
          _dead         (NULL),
          _offmin       (FNIL),
          _offmax       (FNIL)
{
  assert(_informer);
}



//------------------------ destructor --------------------------//
//------------------------ destructor --------------------------//
//------------------------ destructor --------------------------//

FgTraces::~FgTraces()
{
  clearTraces();
}



//-------------------- clear traces ------------------------------//
//-------------------- clear traces ------------------------------//
//-------------------- clear traces ------------------------------//

       // public.
       // must be called if anything changes in FieldGeometry
       //   which might make the traces out-of-date.
       // does nothing if traces are already cleared.

void FgTraces::clearTraces()
{
  if(_ntraces == 0) return;
  assert(_grps && _rflags);
  delete [] _grps;
  delete [] _rflags;
  _grps    = NULL;
  _rflags  = NULL;
  clearCoords();
  clearDeadTraceCodes();
  _ntraces = 0;
}


void FgTraces::clearCoords()
{
  if(!_xmids) return;
  assert(_ntraces > 0 && _xmids && _ymids && _offsets);
  delete [] _xmids;
  delete [] _ymids;
  delete [] _offsets;
  _xmids   = NULL;
  _ymids   = NULL;
  _offsets = NULL;
  _offmin  = FNIL;
  _offmax  = FNIL;
}



void FgTraces::clearDeadTraceCodes()
{
  if(!_dead) return;
  assert(_ntraces > 0 && _dead);
  delete [] _dead;
  _dead = NULL;
}



//--------------------- create traces ----------------------------//
//--------------------- create traces ----------------------------//
//--------------------- create traces ----------------------------//

      // public.
      // allocates _grps and _rflags arrays and fills them with NULL.
      // leaves the other arrays unallocated.
      // asserts if traces are already created.

void FgTraces::createTraces(long ntraces)
{
  assert(ntraces > 0 && _ntraces == 0);
  assert(!_grps && !_rflags && !_xmids && !_ymids && !_offsets && !_dead);
  _ntraces = ntraces;
  _grps    = new FgGroup   * [_ntraces];
  _rflags  = new FieldFlag * [_ntraces];
/***********
     ///// for efficiency, this section is omitted.
     ///// the calling program has the responsibility
     ///// to set all of these values.
  _informer->showMessage("initializing seismic traces...");
  for(long i = 0; i < _ntraces; i++)
      {
      _grps  [i] = NULL;
      _rflags[i] = NULL;
      }
***********/
}


void FgTraces::createCoords()
{
  assert(_ntraces > 0);
  assert(_grps && _rflags && !_xmids && !_ymids && !_offsets);
  _xmids   = new long [_ntraces];
  _ymids   = new long [_ntraces];
  _offsets = new long [_ntraces];
/***********
     ///// for efficiency, this section is omitted.
     ///// the calling program has the responsibility
     ///// to set all of these values.
  _informer->showMessage("initializing trace midpoint coordinates...");
  for(long i = 0; i < _ntraces; i++)
      {
      _xmids  [i] = INIL;
      _ymids  [i] = INIL;
      _offsets[i] = INIL;
      }
***********/
}



void FgTraces::createDeadTraceCodes()
{
  assert(_ntraces > 0);
  assert(_grps && _rflags && !_dead);
  _dead = new char [_ntraces];
/***********
     ///// for efficiency, this section is omitted.
     ///// the calling program has the responsibility
     ///// to set all of these values.
  _informer->showMessage("initializing dead trace codes...");
  for(long i = 0; i < _ntraces; i++)
      {
      _dead[i] = ZT_CODE_NONE;
      }
***********/
}



//------------------- find offset range ----------------------//
//------------------- find offset range ----------------------//
//------------------- find offset range ----------------------//

   // public.
   // should be called after after all offsets have been set.

void FgTraces::findOffsetRange()
{
  _informer->showMessage("finding offset range...");
  assert(_offsets);
  _offmin = FNIL;
  _offmax = FNIL;
  for(long i = 0; i < _ntraces; i++)
      {
      if(_offsets[i] != INIL)
          {
          float offset = (float)_offsets[i];
          if(_offmin == FNIL)
              {
              _offmin = offset;
              _offmax = offset;
              }
          else if(offset < _offmin)
              {
              _offmin = offset;
              }
          else if(offset > _offmax)
              {
              _offmax = offset;
              }
          }
      }
}



//-------------------------- end -------------------------------//
//-------------------------- end -------------------------------//
//-------------------------- end -------------------------------//

