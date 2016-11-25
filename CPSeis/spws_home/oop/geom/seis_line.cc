
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
//---------------------- seis_line.cc -----------------------//
//---------------------- seis_line.cc -----------------------//
//---------------------- seis_line.cc -----------------------//

//            implementation file for the SeisLine class
//                 derived from the SmartArray class
//                        subdirectory geom


#include "geom/seis_line.hh"
#include "geom/field_flag.hh"
#include "geom/fg_informer.hh"
#include "geom/fg_connect.hh"
#include "geom/fg_constants.hh"
#include "geom/acc_chain.hh"
#include "oprim/acc_index.hh"
#include "oprim/acc_down.hh"
#include "oprim/acc_interp.hh"
#include "oprim/acc_select.hh"
#include "oprim/acc_search.hh"
#include "oprim/acc_cum_dist.hh"
#include "oprim/fast_sort.hh"
#include "cprim.h"
#include "named_constants.h"
#include <iostream.h>
#include <stdlib.h>
#include <string.h>
#include <math.h>
#include <assert.h>


static long   STEP              = 500;
static double TOLERANCE         = 0.01;
static double DZERO             = 0.0;



//----------------------- static functions -----------------------//
//----------------------- static functions -----------------------//
//----------------------- static functions -----------------------//

#define FFLAG    ((SeisLine*)data)->unsafeFieldFlag(index)
#define FPREV    ((SeisLine*)data)->unsafeFieldFlag(index-1)
#define FNEXT    ((SeisLine*)data)->unsafeFieldFlag(index+1)
#define NUMFLAGS ((SeisLine*)data)->numElements()


static void set_index(void *data, long index)
{
  FFLAG->setIndexOfThisFlag(index);
}


static float get_shotpoint(void *data, long index)
{
  return FFLAG->getShotpoint();
}


static void update_cum_dist(void *data, long index)
{
  if(index == 0)
      {
      if(NUMFLAGS == 1) FFLAG->updateCumDist(NULL, NULL);
      else              FFLAG->updateCumDist(NULL, FNEXT);
      }
  else
      {
      FFLAG->updateCumDist(FPREV, NULL);
      }
}


static char get_select(void *data, long index)
{
  return FFLAG->getFlagSelectValue();
}


static void set_select(void *data, long index, char select)
{
  FFLAG->setFlagSelectValue(select);
}



#define AAF(get_value, set_value, getValue, setValue)        \
static double get_value(void *data, long index)              \
{                                                            \
  return (double)FFLAG->getValue();                          \
}                                                            \
                                                             \
static void set_value(void *data, long index, double value)  \
{                                                            \
  FFLAG->setValue((float)value);                             \
}


#define AAD(get_value, set_value, getValue, setValue)        \
static double get_value(void *data, long index)              \
{                                                            \
  return FFLAG->getValue();                                  \
}                                                            \
                                                             \
static void set_value(void *data, long index, double value)  \
{                                                            \
  FFLAG->setValue(value);                                    \
}


AAF(get_shot , set_shot , getShotpoint     , set2Shotpoint     )
AAD(get_dist , set_dist , getIncrDistance  , set2IncrDistance  )
AAD(get_xloc , set_xloc , getXloc          , set2Xloc          )
AAD(get_yloc , set_yloc , getYloc          , set2Yloc          )
AAF(get_elev , set_elev , getElevation     , set2Elevation     )
AAF(get_hd   , set_hd   , getHoleDepth     , set2HoleDepth     )
AAF(get_tuh  , set_tuh  , getUpholeTime    , set2UpholeTime    )
AAF(get_rstat, set_rstat, getReceiverStatic, set2ReceiverStatic)
AAF(get_sstat, set_sstat, getSourceStatic  , set2SourceStatic  )



//---------------------- constructor -----------------------//
//---------------------- constructor -----------------------//
//---------------------- constructor -----------------------//


SeisLine::SeisLine(FgInformer *informer, FgConnect *connect, int chaining)
           :  SmartArray(STEP),
                _line_number            (0),
                _ixl                    (-1),
                _select                 (' '),
                _last_cumulative_gp     (0),
                _first_matchable_gp     (0),
                _num_sources            (0),
                _num_receivers          (0),
                _frozen                 (FALSE),
                _need                   (FALSE),
                _dead_source_codes_set  (FALSE),
                _dead_receiver_codes_set(FALSE),
                _num_dead_sources       (0),
                _num_dead_receivers     (0),
                _num_reversed_sources   (0),
                _num_reversed_receivers (0),
                _num_missing_sources    (0),
                _num_missing_receivers  (0),
                _num_live_sources       (0),
                _num_live_receivers     (0),
                _informer               (informer),
                _connect                (connect)
{
  assert(_informer && _connect);
  _acc_index  = new AccIndex  (this,           set_index);
  _acc_coords = new AccBase   (this, FG_COORDS);
  _acc_shot   = new AccInterp (this, FG_SHOT , get_shot , set_shot , TRUE );
  _acc_dist   = new AccDown   (this, FG_DIST , get_dist , set_dist        );
  _acc_xloc   = new AccInterp (this, FG_XLOC , get_xloc , set_xloc , TRUE );
  _acc_yloc   = new AccInterp (this, FG_YLOC , get_yloc , set_yloc , TRUE );
  _acc_elev   = new AccInterp (this, FG_ELEV , get_elev , set_elev , FALSE);
  _acc_hd     = new AccInterp (this, FG_HD   , get_hd   , set_hd   , FALSE);
  _acc_tuh    = new AccInterp (this, FG_TUH  , get_tuh  , set_tuh  , FALSE);
  _acc_rstat  = new AccInterp (this, FG_RSTAT, get_rstat, set_rstat, FALSE);
  _acc_sstat  = new AccInterp (this, FG_SSTAT, get_sstat, set_sstat, FALSE);
  _acc_xskid  = new AccBase   (this, FG_XSKID);
  _acc_yskid  = new AccBase   (this, FG_YSKID);
  _acc_eskid  = new AccBase   (this, FG_ESKID);
  _acc_select = new AccSelect (this, FG_SEL  , get_select, set_select);
  _acc_cum    = new AccCumDist(this, FG_CUM  , update_cum_dist);
  _search     = new AccSearch (this,           get_shotpoint, TOLERANCE);
  _chain      = new AccChain  (this, chaining);
  _sort       = new FastSort  (this,           get_shotpoint);
}



//------------------------ destructor --------------------------//
//------------------------ destructor --------------------------//
//------------------------ destructor --------------------------//

SeisLine::~SeisLine()
{
  deleteAllFlagsFromLine();
  delete _acc_index;
  delete _acc_coords;
  delete _acc_shot;
  delete _acc_dist;
  delete _acc_xloc;
  delete _acc_yloc;
  delete _acc_elev;
  delete _acc_hd;
  delete _acc_tuh;
  delete _acc_rstat;
  delete _acc_sstat;
  delete _acc_xskid;
  delete _acc_yskid;
  delete _acc_eskid;
  delete _acc_select;
  delete _acc_cum;
  delete _search;
  delete _chain;
  delete _sort;
}


//------------------------ set values ----------------------------//
//------------------------ set values ----------------------------//
//------------------------ set values ----------------------------//

        // these need to be called only by SeisSurvey.

void SeisLine::setChaining(int chaining)
{
  long n = numFlagsOnLine();
  int old_chaining = _chain->getChaining();
  if(chaining == old_chaining) return;
  if(old_chaining == SLOPE_CHAINING || chaining == SLOPE_CHAINING)
      {
      _acc_cum->preChange        (0, n, n);
      valuesWillChange(FG_COORDS, 0, n, n);
      valuesWillChange(FG_XLOC  , 0, n, n);
      }
  int new_indep_ident = _chain->setChaining(chaining);
  if     (new_indep_ident == FG_DIST) _acc_dist->adjustDependencyFlags();
  else if(new_indep_ident == FG_XLOC) _acc_xloc->adjustDependencyFlags();
  if(old_chaining == SLOPE_CHAINING || chaining == SLOPE_CHAINING)
      {
      _acc_cum->postChange        ();
      valuesHaveChanged(FG_COORDS, 0, n, n);
      valuesHaveChanged(FG_XLOC  , 0, n, n);
      }
}



//-------------- before and after new active index -----------------//
//-------------- before and after new active index -----------------//
//-------------- before and after new active index -----------------//

        //  private virtual functions overriding SmartArray.
        //  Called from SmartArray before and after changing
        //    the active index.
        //  The function in SmartArray is empty.


void SeisLine::beforeNewActiveIndex()
{
  if(_ixl >= 0) _informer ->preNewActiveFlag(_ixl);
}



void SeisLine::afterNewActiveIndex()
{
  if(_ixl >= 0) _informer ->postNewActiveFlag(_ixl);
}



//------------------- before remove insert --------------------------//
//------------------- before remove insert --------------------------//
//------------------- before remove insert --------------------------//

        //  private virtual function overriding SmartArray.
        //  Called from SmartArray before removing and/or inserting
        //    one or more array elements.
        //  The function in SmartArray is empty.
        //  This function makes sure the interpolation and search
        //    and chain information is updated, and then adjusts
        //    the active index if necessary.
        //  The interpolations must be done before the search and
        //    chain updates.


void SeisLine::beforeRemoveInsert(long index, long nrem, long nins)
{
  if(_need) return;
  if(_ixl >= 0) _informer  ->preRemoveInsertFlags(_ixl, index, nrem, nins);
  _acc_index ->preChange(index, nrem, nins);
  _acc_shot  ->preChange(index, nrem, nins);
  _acc_yloc  ->preChange(index, nrem, nins);
  _acc_elev  ->preChange(index, nrem, nins);
  if(!_chain ->changesValues(FG_DIST))
      {
      _acc_dist  ->preChange(index, nrem, nins);
      }
  if(!_chain ->changesValues(FG_XLOC))
      {
      _acc_xloc  ->preChange(index, nrem, nins);
      _acc_coords->regRange (_acc_xloc);
      _acc_coords->preChange(_acc_yloc);
      }
  _acc_hd    ->preChange(index, nrem, nins);
  _acc_tuh   ->preChange(index, nrem, nins);
  _acc_rstat ->preChange(index, nrem, nins);
  _acc_sstat ->preChange(index, nrem, nins);
  _acc_xskid ->preChange(index, nrem, nins);
  _acc_yskid ->preChange(index, nrem, nins);
  _acc_eskid ->preChange(index, nrem, nins);
  _acc_select->preChange(index, nrem, nins);
  _search    ->preChange(index, nrem, nins);
  if(_chain  ->needsValues(FG_DIST)) _chain->regRange(_acc_dist);
  if(_chain  ->needsValues(FG_XLOC)) _chain->regRange(_acc_xloc);
  if(_chain  ->needsValues(FG_YLOC)) _chain->regRange(_acc_yloc);
  if(_chain  ->needsValues(FG_ELEV)) _chain->regRange(_acc_elev);
  _chain    ->preChange2(index, nrem, nins);
  _chain     ->preChange(index, nrem, nins);
  _acc_cum   ->preChange(index, nrem, nins);
}




//------------------- after remove insert --------------------------//
//------------------- after remove insert --------------------------//
//------------------- after remove insert --------------------------//

        //  private virtual function overriding SmartArray.
        //  Called from SmartArray after removing and/or inserting
        //    one or more array elements.
        //  The function in SmartArray is empty.
        //  This function makes sure the interpolation and search
        //    and chain information is updated, and then adjusts
        //    the active index if necessary.
        //  The interpolations must be done before the search and
        //    chain updates.


void SeisLine::afterRemoveInsert(long index, long nrem, long nins)
{
  if(_need) return;
  _acc_index ->post1Change();
  _acc_shot  ->post1Change();
  _acc_yloc  ->post1Change();
  _acc_elev  ->post1Change();
  if(!_chain ->changesValues(FG_DIST))
      {
      _acc_dist->post1Change();
      }
  if(!_chain ->changesValues(FG_XLOC))
      {
      _acc_xloc  ->post1Change();
      _acc_coords->post1Change();
      }
  _acc_hd    ->post1Change();
  _acc_tuh   ->post1Change();
  _acc_rstat ->post1Change();
  _acc_sstat ->post1Change();
  _acc_xskid ->post1Change();
  _acc_yskid ->post1Change();
  _acc_eskid ->post1Change();
  _acc_select->post1Change();
  _search    ->post1Change();
  _chain     ->post1Change();
  _acc_cum   ->post1Change();

  _acc_index ->post2Change();
  _acc_shot  ->post2Change();
  _acc_yloc  ->post2Change();
  _acc_elev  ->post2Change();
  if(!_chain ->changesValues(FG_DIST))
      {
      _acc_dist->post2Change();
      }
  if(!_chain ->changesValues(FG_XLOC))
      {
      _acc_xloc  ->post2Change();
      _acc_coords->post2Change();
      }
  _acc_hd    ->post2Change();
  _acc_tuh   ->post2Change();
  _acc_rstat ->post2Change();
  _acc_sstat ->post2Change();
  _acc_xskid ->post2Change();
  _acc_yskid ->post2Change();
  _acc_eskid ->post2Change();
  _acc_select->post2Change();
  _search    ->post2Change();
  _chain     ->post2Change();
  _chain     ->postChange2(index, nrem, nins);
  _acc_cum   ->post2Change();
  if(_ixl >= 0) _informer  ->postRemoveInsertFlags(_ixl, index, nrem, nins);
  if(_frozen) _need = TRUE;
}



//-------------------- before value changed ---------------------//
//-------------------- before value changed ---------------------//
//-------------------- before value changed ---------------------//

      //  private convenience function.
      //  Called by a public function before that function changes
      //    a value.
      //  This function makes sure the interpolation and search
      //    and chain information is updated.
      //  The interpolations must be done before the search and
      //    chain updates.

void SeisLine::beforeValueChanged(int ident, long index)
{
  if(_need) return;
  _informer    ->preMultipleOperations();
  int must_update_chain = (_chain->needsValues  (ident) ||
                           _chain->changesValues(ident));
  switch(ident)
    {
    case FG_SHOT : _acc_shot  ->preChange(index, 1, 1);
                   _search    ->preChange(index, 1, 1); break;
    case FG_DIST : _acc_dist  ->preChange(index, 1, 1);
                   if(_chain->needsValues(FG_DIST))_chain->regRange(_acc_dist);
                   break;
    case FG_XLOC : _acc_xloc  ->preChange(index, 1, 1);
                   _acc_coords->preChange(_acc_xloc);
                   if(_chain->needsValues(FG_XLOC))_chain->regRange(_acc_xloc);
                   break;
    case FG_YLOC : _acc_yloc  ->preChange(index, 1, 1);
                   if(!_chain->changesValues(FG_XLOC))
                                             _acc_coords->preChange(_acc_yloc);
                   if(_chain->needsValues(FG_YLOC))_chain->regRange(_acc_yloc);
                   break;
    case FG_ELEV : _acc_elev  ->preChange(index, 1, 1);
                   if(_chain->needsValues(FG_ELEV))_chain->regRange(_acc_elev);
                   break;
    case FG_HD   : _acc_hd    ->preChange(index, 1, 1); break;
    case FG_TUH  : _acc_tuh   ->preChange(index, 1, 1); break;
    case FG_RSTAT: _acc_rstat ->preChange(index, 1, 1); break;
    case FG_SSTAT: _acc_sstat ->preChange(index, 1, 1); break;
    case FG_XSKID: _acc_xskid ->preChange(index, 1, 1); break;
    case FG_YSKID: _acc_yskid ->preChange(index, 1, 1); break;
    case FG_ESKID: _acc_eskid ->preChange(index, 1, 1); break;
    case FG_SEL  : _acc_select->preChange(index, 1, 1); break;
    case FG_CUM  : assert(FALSE);
    case FG_AZIM : assert(FALSE);
    }
  if(must_update_chain) _chain->preChange(index, 1, 1);
  if(must_update_chain) _chain->preChange2(index, 1, 1);
  if(must_update_chain || ident == FG_XLOC || ident == FG_YLOC)
     _acc_cum->preChange(index, 1, 1);
}



//-------------------- after value changed ---------------------//
//-------------------- after value changed ---------------------//
//-------------------- after value changed ---------------------//

      //  private convenience function.
      //  Called by a public function after that function changes
      //    a value.
      //  This function makes sure the interpolation and search
      //    and chain information is updated.
      //  The interpolations must be done before the search and
      //    chain updates.

void SeisLine::afterValueChanged(int ident, long index)
{
  if(_need) return;
  int must_update_chain = (_chain->needsValues  (ident) ||
                           _chain->changesValues(ident));
  switch(ident)
    {
    case FG_SHOT : _acc_shot  ->post1Change();
                   _search    ->post1Change(); break;
    case FG_DIST : _acc_dist  ->post1Change(); break;
    case FG_XLOC : _acc_xloc  ->post1Change();
                   _acc_coords->post1Change(); break;
    case FG_YLOC : _acc_yloc  ->post1Change();
                   if(!_chain->changesValues(FG_XLOC))
                                      _acc_coords->post1Change();
                                              break;
    case FG_ELEV : _acc_elev  ->post1Change(); break;
    case FG_HD   : _acc_hd    ->post1Change(); break;
    case FG_TUH  : _acc_tuh   ->post1Change(); break;
    case FG_RSTAT: _acc_rstat ->post1Change(); break;
    case FG_SSTAT: _acc_sstat ->post1Change(); break;
    case FG_XSKID: _acc_xskid ->post1Change(); break;
    case FG_YSKID: _acc_yskid ->post1Change(); break;
    case FG_ESKID: _acc_eskid ->post1Change(); break;
    case FG_SEL  : _acc_select->post1Change(); break;
    case FG_CUM  : assert(FALSE);
    case FG_AZIM : assert(FALSE);
    }
  if(must_update_chain) _chain->post1Change();
  if(must_update_chain || ident == FG_XLOC || ident == FG_YLOC)
                      _acc_cum->post1Change();

  switch(ident)
    {
    case FG_SHOT : _acc_shot  ->post2Change();
                   _search    ->post2Change(); break;
    case FG_DIST : _acc_dist  ->post2Change(); break;
    case FG_XLOC : _acc_xloc  ->post2Change();
                   _acc_coords->post2Change(); break;
    case FG_YLOC : _acc_yloc  ->post2Change();
                   if(!_chain->changesValues(FG_XLOC))
                                      _acc_coords->post2Change();
                                              break;
    case FG_ELEV : _acc_elev  ->post2Change(); break;
    case FG_HD   : _acc_hd    ->post2Change(); break;
    case FG_TUH  : _acc_tuh   ->post2Change(); break;
    case FG_RSTAT: _acc_rstat ->post2Change(); break;
    case FG_SSTAT: _acc_sstat ->post2Change(); break;
    case FG_XSKID: _acc_xskid ->post2Change(); break;
    case FG_YSKID: _acc_yskid ->post2Change(); break;
    case FG_ESKID: _acc_eskid ->post2Change(); break;
    case FG_SEL  : _acc_select->post2Change(); break;
    case FG_CUM  : assert(FALSE);
    case FG_AZIM : assert(FALSE);
    }
  if(must_update_chain) _chain->post2Change();
  if(must_update_chain) _chain->postChange2(index, 1, 1);
  if(must_update_chain || ident == FG_XLOC || ident == FG_YLOC)
                      _acc_cum->post2Change();
  _informer    ->postMultipleOperations();
}



//------------------- overriding virtual functions -----------------//
//------------------- overriding virtual functions -----------------//
//------------------- overriding virtual functions -----------------//

    // public virtual functions overriding SmartArray.
    // called by AccInterp and AccDown.
    // these do not protect against index out of range.
    // these are public only because I don't want to make
    //    AccInterp and AccDown friends of SmartArray.
    // these are pass-thru to field flags.
    // the functions in the field flags are not virtual.

    // Can also be called by other people, but other functions are
    // provided for this, and should be used instead:
    //     flagValueIsDependent  (long ixf, int ident)
    //     getFlagValue          (long ixf, int ident)
    //     setFlagValue          (long ixf, int ident, double value)
    //     setDependentFlagValue (long ixf, int ident, double value)
    // (note different order of arguments)


int SeisLine::valueIsDependent(int ident, long ixf)  const
{
  return unsafeFieldFlag(ixf)->flagValueIsDependent(ident);
}


void SeisLine::setDependencyTrue(int ident, long ixf)
{
  unsafeFieldFlag(ixf)->setFlagDependencyTrue(ident);
}


void SeisLine::setDependencyFalse(int ident, long ixf)
{
  unsafeFieldFlag(ixf)->setFlagDependencyFalse(ident);
}




//--------------- values will change (or have changed) -------------//
//--------------- values will change (or have changed) -------------//
//--------------- values will change (or have changed) -------------//

    // virtual functions overriding SmartArray.
    // called by AccInterp and AccChain.

void SeisLine::valuesWillChange
                  (int ident, long index, long nrem, long nins)
{
  if(_ixl >= 0)
       _informer->preFlagValuesChanged (_ixl, ident, index, nrem, nins);
}

void SeisLine::valuesHaveChanged
                  (int ident, long index, long nrem, long nins)
{
  if(_ixl >= 0)
       _informer->postFlagValuesChanged (_ixl, ident, index, nrem, nins);
}



//------------ public access to this line -------------------//
//------------ public access to this line -------------------//
//------------ public access to this line -------------------//
//------------ public access to this line -------------------//
//------------ public access to this line -------------------//
//------------ public access to this line -------------------//
//------------ public access to this line -------------------//
//------------ public access to this line -------------------//
//------------ public access to this line -------------------//
//------------ public access to this line -------------------//

        // ixf = index of desired flag on this line.



//-------------------- get values ------------------------------//
//-------------------- get values ------------------------------//
//-------------------- get values ------------------------------//

long  SeisLine::numSelectedFlagsOnLine()  const
{
  return _acc_select->numSelected();
}


long  SeisLine::firstCumulativeGroundPosition()  const
{
  long n = numFlagsOnLine();
  if(n == 0) return INIL;
  return _last_cumulative_gp - n + 1;
}


long  SeisLine::firstMatchableGroundPosition()  const
{
  long n = numFlagsOnLine();
  if(n == 0) return INIL;
  return _first_matchable_gp;
}


long  SeisLine::lastCumulativeGroundPosition()  const
{
  long n = numFlagsOnLine();
  if(n == 0) return INIL;
  return _last_cumulative_gp;
}


long  SeisLine::lastMatchableGroundPosition()  const
{
  long n = numFlagsOnLine();
  if(n == 0) return INIL;
  return _first_matchable_gp + n - 1;
}


long  SeisLine::updateLastCumulativeGP(long prev_num_gps)
{
  _last_cumulative_gp = prev_num_gps + numFlagsOnLine();
  return _last_cumulative_gp;
}


float SeisLine::getActiveShotpointOnLine()  const
{
  long active = getActiveIndex();
  if(active >= 0) return fieldFlag(active)->getShotpoint();
  return FNIL;
}


float SeisLine::getFirstShotpointOnLine()  const
{
  long n = numFlagsOnLine();
  if(n >= 1) return fieldFlag(0)->getShotpoint();
  return FNIL;
}


float SeisLine::getLastShotpointOnLine()  const
{
  long n = numFlagsOnLine();
  if(n >= 1) return fieldFlag(n - 1)->getShotpoint();
  return FNIL;
}


float SeisLine::getSmallestShotpointOnLine()  const
{
  double value = _search->getMinimumValue();
  if(value == DNIL) return FNIL;
  return (float)value;
}


float SeisLine::getLargestShotpointOnLine()  const
{
  double value = _search->getMaximumValue();
  if(value == DNIL) return FNIL;
  return (float)value;
}


float SeisLine::getMinShotpointIncrOnLine()  const
{
  double step = _search->getMinimumStep();
  if(step == DNIL) return FNIL;
  return (float)step;
}


float SeisLine::getMaxShotpointIncrOnLine()  const
{
  double step = _search->getMaximumStep();
  if(step == DNIL) return FNIL;
  return (float)step;
}



int SeisLine::shotpointsAreDuplicatedOrNotSorted()  const
{
  return !_search->isStrictlySorted();
}



double SeisLine::minimumXlocOnLine()  const
{
  double xmin = DZERO;
  long n = numFlagsOnLine();
  for(long ixf = 0; ixf < n; ixf++)
      {
      double x = getXloc(ixf);
      if(ixf == 0 || x < xmin) xmin = x;
      }
  return xmin;
}



double SeisLine::maximumXlocOnLine()  const
{
  double xmax = DZERO;
  long n = numFlagsOnLine();
  for(long ixf = 0; ixf < n; ixf++)
      {
      double x = getXloc(ixf);
      if(ixf == 0 || x > xmax) xmax = x;
      }
  return xmax;
}



double SeisLine::minimumYlocOnLine()  const
{
  double ymin = DZERO;
  long n = numFlagsOnLine();
  for(long ixf = 0; ixf < n; ixf++)
      {
      double y = getYloc(ixf);
      if(ixf == 0 || y < ymin) ymin = y;
      }
  return ymin;
}



double SeisLine::maximumYlocOnLine()  const
{
  double ymax = DZERO;
  long n = numFlagsOnLine();
  for(long ixf = 0; ixf < n; ixf++)
      {
      double y = getYloc(ixf);
      if(ixf == 0 || y > ymax) ymax = y;
      }
  return ymax;
}



double SeisLine::distanceToLine(double xloc, double yloc)  const
{
  double distance2 = distanceSquaredToLine(xloc, yloc);
  if(distance2 == DNIL) return distance2;
  return sqrt(distance2);
}



double SeisLine::distanceSquaredToLine(double xloc, double yloc)  const
{
    ////
    //// Currently, this simply returns the distance (squared) to
    //// the nearest flag.  It should return the shortest
    //// perpendicular distance (squared) to the line.
    ////
  long index = findNearestFlagOnLine(xloc, yloc);
  if(index == -1) return DNIL;
  return distanceSquaredToFlag(index, xloc, yloc);
}



//----------------------- set values ------------------------------//
//----------------------- set values ------------------------------//
//----------------------- set values ------------------------------//


void SeisLine::setLineNumber(long line_number)
{
  if(_ixl >= 0) _informer->preLineNumbersChanged(_ixl, 1, 1);
  if(line_number == INIL) _line_number = 0;
  else                      _line_number = line_number;
  if(_ixl >= 0) _informer->postLineNumbersChanged(_ixl, 1, 1);
}



void SeisLine::setActiveShotpointOnLine(float shotpoint)    // search
{
  long n = numFlagsOnLine();
  if(n == 0) return;
  float active_shotpoint = getActiveShotpointOnLine();
  int direction;
  if     (shotpoint > active_shotpoint) direction =  1;
  else if(shotpoint < active_shotpoint) direction = -1;
  else                                  return;
  long index = findNearestShotpointOnLine(shotpoint, direction);
  setActiveFlagIndexOnLine(index);
}



void SeisLine::reverseLineDirection()
{
  assert(!_frozen);
  long n = numFlagsOnLine();
  if(n <= 1) return;
  if(_ixl >= 0) _informer    ->preReverseLineDirection(_ixl);
  if(_ixl >= 0) _informer    ->preNewActiveFlag(_ixl);
  _acc_index   ->preChange(0, n, n);
  _acc_select  ->preChange(0, n, n);
  _acc_cum     ->preChange(0, n, n);
  _search      ->preChange(0, n, n);

  _sort->switchDirection(0);        // 0 = desired direction.

  _chain       ->notifyReverseDirection();
  if(!_chain->changesValues(FG_DIST))
                             _acc_dist->adjustDependencyFlags();
  _acc_index   ->post1Change();
  _acc_select  ->post1Change();
  _acc_cum     ->post1Change();
  _search      ->post1Change();
  _acc_index   ->post2Change();
  _acc_select  ->post2Change();
  _acc_cum     ->post2Change();
  _search      ->post2Change();
  if(_ixl >= 0) _informer    ->postReverseLineDirection(_ixl);
  if(_ixl >= 0) _informer    ->postNewActiveFlag(_ixl);
}



void SeisLine::freezeDependentUpdatesOnLine()
{
  if(_frozen) return;
  _frozen = TRUE;
  _acc_index ->freezeUpdates();
  _acc_coords->freezeUpdates();
  _acc_shot  ->freezeUpdates();
  _acc_dist  ->freezeUpdates();
  _acc_xloc  ->freezeUpdates();
  _acc_yloc  ->freezeUpdates();
  _acc_elev  ->freezeUpdates();
  _acc_hd    ->freezeUpdates();
  _acc_tuh   ->freezeUpdates();
  _acc_rstat ->freezeUpdates();
  _acc_sstat ->freezeUpdates();
  _acc_xskid ->freezeUpdates();
  _acc_yskid ->freezeUpdates();
  _acc_eskid ->freezeUpdates();
  _acc_select->freezeUpdates();
  _search    ->freezeUpdates();
  _chain     ->freezeUpdates();
  _acc_cum   ->freezeUpdates();
}


void SeisLine::resumeDependentUpdatesOnLine()
{
  if(!_frozen) return;
  _frozen = FALSE;
  _need   = FALSE;
  _acc_index ->resumeUpdates();
  _acc_coords->resumeUpdates();
  _acc_shot  ->resumeUpdates();
  _acc_dist  ->resumeUpdates();
  _acc_xloc  ->resumeUpdates();
  _acc_yloc  ->resumeUpdates();
  _acc_elev  ->resumeUpdates();
  _acc_hd    ->resumeUpdates();
  _acc_tuh   ->resumeUpdates();
  _acc_rstat ->resumeUpdates();
  _acc_sstat ->resumeUpdates();
  _acc_xskid ->resumeUpdates();
  _acc_yskid ->resumeUpdates();
  _acc_eskid ->resumeUpdates();
  _acc_select->resumeUpdates();
  _search    ->resumeUpdates();
  _chain     ->resumeUpdates();
  _acc_cum   ->resumeUpdates();
}


void SeisLine::performDependentUpdatesOnLine()
{
  if(!_frozen) return;
  _frozen = FALSE;
  _need   = FALSE;
  _acc_index ->performUpdates();
  _acc_coords->performUpdates();
  _acc_shot  ->performUpdates();
  _acc_dist  ->performUpdates();
  _acc_xloc  ->performUpdates();
  _acc_yloc  ->performUpdates();
  _acc_elev  ->performUpdates();
  _acc_hd    ->performUpdates();
  _acc_tuh   ->performUpdates();
  _acc_rstat ->performUpdates();
  _acc_sstat ->performUpdates();
  _acc_xskid ->performUpdates();
  _acc_yskid ->performUpdates();
  _acc_eskid ->performUpdates();
  _acc_select->performUpdates();
  _search    ->performUpdates();
  _chain     ->performUpdates();
  _acc_cum   ->performUpdates();
}



/***********************
//--------------- set dead source and receiver codes ---------------//
//--------------- set dead source and receiver codes ---------------//
//--------------- set dead source and receiver codes ---------------//

   // public.
   // quicker logic than the routines coming later.
   // either one of these functions must be called whenever anything
   //   changes in this object or in the ZT cards which might render
   //   any of these codes to be out-of-date.
   // or the functions to clear the codes can be called.

void SeisLine::setDeadSourceCodes()
{
  if(_frozen)
      {
      clearDeadSourceCodes();
      return;
      }
  _num_dead_sources       = 0;
  _num_reversed_sources   = 0;
  _num_missing_sources    = 0;
  _num_live_sources       = 0;
  long nflags = numFlagsOnLine();
  for(long ixf = 0; ixf < nflags; ixf++)
      {
      FieldFlag *field_flag = fieldFlag(ixf);
      field_flag->setDeadSourceCode  (ZT_CODE_LIVE);
      }
  ZtCards *zt_cards = _connect->getZtCards();
  assert(zt_cards);
  long nzt1 = zt_cards->numZt1Cards();
  for(long ixzt1 = 0; ixzt1 < nzt1; ixzt1++)
      {
      int        code = zt_cards->getZt1Code(ixzt1);
      float from_shot = zt_cards->getZt1FromShot(ixzt1);
      float   to_shot = zt_cards->getZt1ToShot(ixzt1);
      long       line = zt_cards->getZt1Line(ixzt1);


***************************/





//--------------- set dead source and receiver codes ---------------//
//--------------- set dead source and receiver codes ---------------//
//--------------- set dead source and receiver codes ---------------//

   // public.
   // either one of these functions must be called whenever anything
   //   changes in this object or in the ZT cards which might render
   //   any of these codes to be out-of-date.
   // or the functions to clear the codes can be called.

void SeisLine::setDeadSourceCodes()
{
  if(_frozen)
      {
      clearDeadSourceCodes();
      return;
      }
  if(_dead_source_codes_set) return;
  _num_dead_sources       = 0;
  _num_reversed_sources   = 0;
  _num_missing_sources    = 0;
  _num_live_sources       = 0;
  long nflags = numFlagsOnLine();
  for(long ixf = 0; ixf < nflags; ixf++)
      {
      FieldFlag *field_flag = fieldFlag(ixf);
      float shotpoint = field_flag->getShotpoint();
      int sdead = _connect->getDeadSourceCode  (_line_number, shotpoint);
      field_flag->setDeadSourceCode  (sdead);
      switch(sdead)
          {
          case ZT_CODE_ZERO: _num_dead_sources++;     break;
          case ZT_CODE_REV : _num_reversed_sources++; break;
          case ZT_CODE_MISS: _num_missing_sources++;  break;
          case ZT_CODE_LIVE: _num_live_sources++;     break;
          case ZT_CODE_NONE:                          break;
          default: assert(FALSE);
          }
      int maybe = _connect->sourceMaybeDead (_line_number, shotpoint);
      field_flag->setSourceMaybeDead (maybe);
      }
  _dead_source_codes_set = TRUE;
}



void SeisLine::setDeadReceiverCodes()
{
  if(_frozen)
      {
      clearDeadReceiverCodes();
      return;
      }
  if(_dead_receiver_codes_set) return;
  _num_dead_receivers     = 0;
  _num_reversed_receivers = 0;
  _num_missing_receivers  = 0;
  _num_live_receivers     = 0;
  long nflags = numFlagsOnLine();
  for(long ixf = 0; ixf < nflags; ixf++)
      {
      FieldFlag *field_flag = fieldFlag(ixf);
      float shotpoint = field_flag->getShotpoint();
      int rdead = _connect->getDeadReceiverCode(_line_number, shotpoint);
      field_flag->setDeadReceiverCode(rdead);
      switch(rdead)
          {
          case ZT_CODE_ZERO: _num_dead_receivers++;     break;
          case ZT_CODE_REV : _num_reversed_receivers++; break;
          case ZT_CODE_MISS: _num_missing_receivers++;  break;
          case ZT_CODE_LIVE: _num_live_receivers++;     break;
          case ZT_CODE_NONE:                            break;
          default: assert(FALSE);
          }
      int maybe = _connect->receiverMaybeDead (_line_number, shotpoint);
      field_flag->setReceiverMaybeDead (maybe);
      }
  _dead_receiver_codes_set = TRUE;
}



//------------- clear dead source and receiver codes ------------//
//------------- clear dead source and receiver codes ------------//
//------------- clear dead source and receiver codes ------------//

   // public.
   // either one of these functions must be called whenever anything
   //   changes in this object or in the ZT cards which might render
   //   any of these codes to be out-of-date.
   // or the functions to set the codes can be called.

void SeisLine::clearDeadSourceCodes()
{
  if(!_dead_source_codes_set) return;
  long nflags = numFlagsOnLine();
  for(long ixf = 0; ixf < nflags; ixf++)
      {
      FieldFlag *field_flag = fieldFlag(ixf);
      field_flag->setDeadSourceCode  (ZT_CODE_NONE);
      field_flag->setSourceMaybeDead (FALSE);
      }
  _dead_source_codes_set  = FALSE;
  _num_dead_sources       = 0;
  _num_reversed_sources   = 0;
  _num_missing_sources    = 0;
  _num_live_sources       = 0;
}



void SeisLine::clearDeadReceiverCodes()
{
  if(!_dead_receiver_codes_set) return;
  long nflags = numFlagsOnLine();
  for(long ixf = 0; ixf < nflags; ixf++)
      {
      FieldFlag *field_flag = fieldFlag(ixf);
      field_flag->setDeadReceiverCode  (ZT_CODE_NONE);
      field_flag->setReceiverMaybeDead (FALSE);
      }
  _dead_receiver_codes_set = FALSE;
  _num_dead_receivers      = 0;
  _num_reversed_receivers  = 0;
  _num_missing_receivers   = 0;
  _num_live_receivers      = 0;
}



//------------------ get dead source or receiver code -----------//
//------------------ get dead source or receiver code -----------//
//------------------ get dead source or receiver code -----------//

    // public.
    // gets from FieldFlag.
    // if dead codes are not set, gets from ZtCards.

int SeisLine::getDeadSourceCode(long ixf) const
{
  if(_dead_source_codes_set)
           return fieldFlag(ixf)->getDeadSourceCode();
  float shotpoint = fieldFlag(ixf)->getShotpoint();
  return _connect->getDeadSourceCode(_line_number, shotpoint);
}


int SeisLine::getDeadReceiverCode(long ixf) const
{
  if(_dead_receiver_codes_set)
           return fieldFlag(ixf)->getDeadReceiverCode();
  float shotpoint = fieldFlag(ixf)->getShotpoint();
  return _connect->getDeadReceiverCode(_line_number, shotpoint);
}


int SeisLine::sourceMaybeDead(long ixf) const
{
  if(_dead_source_codes_set)
           return fieldFlag(ixf)->sourceMaybeDead();
  float shotpoint = fieldFlag(ixf)->getShotpoint();
  return _connect->sourceMaybeDead(_line_number, shotpoint);
}


int SeisLine::receiverMaybeDead(long ixf) const
{
  if(_dead_receiver_codes_set)
           return fieldFlag(ixf)->receiverMaybeDead();
  float shotpoint = fieldFlag(ixf)->getShotpoint();
  return _connect->receiverMaybeDead(_line_number, shotpoint);
}



//--------------- do create and delete object --------------------//
//--------------- do create and delete object --------------------//
//--------------- do create and delete object --------------------//

          // private virtual functions overriding SmartArray

void *SeisLine::doCreateObject()
{
  return new FieldFlag(this);
}


void SeisLine::doDeleteObject(void *object)
{
  delete (FieldFlag*)object;
}


void SeisLine::objectWillBeRemoved(long ixf)
{
  FieldFlag *flag = fieldFlag(ixf);
      _num_sources   -= flag->numSourcesAtFlag();
  ////_num_receivers -= flag->numReceiversAtFlag();
  if(flag->flagHasReceiver()) _num_receivers--;
  flag->removeSourcesFromFlag();       // since gathers will go out-of-date.
  flag->removeReceiversFromFlag();     // since gathers will go out-of-date.
}


void SeisLine::objectHasBeenInserted(long ixf)
{
  FieldFlag *flag = fieldFlag(ixf);
      _num_sources   += flag->numSourcesAtFlag();
  ////_num_receivers += flag->numReceiversAtFlag();
      if(flag->flagHasReceiver()) _num_receivers++;
}



//----------------------- insert or remove flags --------------------//
//----------------------- insert or remove flags --------------------//
//----------------------- insert or remove flags --------------------//

   // the non-void functions return index (ixf) where action occurred.
   // the non-void functions return -1 if failed.
 

long SeisLine::appendNewFlagToLine()
{
  return appendNullElement();
}


long SeisLine::placeNewFlagOnLine(float shotpoint)      // search
{
  long index = _search->findInsertionLocation((double)shotpoint);
  index = insertNewFlagOnLine(index);
  if(index >= 0) setShotpoint(index, shotpoint);
  return index;
}


long SeisLine::insertNewFlagOnLine(long ixf)
{
  return insertNullElement(ixf);
}


long SeisLine::insertNewFlagOnLineFromBuffer(long ixf)
{
  return insertElementFromBuffer(ixf);
}



long SeisLine::deleteFlagFromLine(long ixf)
{
  return removeElement(ixf);
}


long SeisLine::deleteFlagFromLineToBuffer(long ixf)
{
  return removeElementToBuffer(ixf);
}


void SeisLine::deleteAllFlagsFromLine()
{
  removeAllElements();
}



//------------------- find helper ------------------------//
//------------------- find helper ------------------------//
//------------------- find helper ------------------------//

                    // private

void SeisLine::findHelper(long ixf, double xloc, double yloc,
               long *nearest_index, double *nearest_distance2) const
{
  double distance2 = distanceSquaredToFlag(ixf, xloc, yloc);
  if(distance2 == DNIL) return;
  if(*nearest_index == -1 || distance2 < *nearest_distance2)
      {
      *nearest_index = ixf;
      *nearest_distance2 = distance2;
      }
}



//------------- search along seismic line -----------------------//
//------------- search along seismic line -----------------------//
//------------- search along seismic line -----------------------//

       // these return an index (ixf), or -1 if not found.

long SeisLine::findNearestShotpointOnLine(float shotpoint, int dir) const
{
  return _search->findNearestValue(shotpoint, dir);
}



long SeisLine::findMatchingShotpointOnLine(float shotpoint) const
{
  return _search->findMatchingValue(shotpoint);
}



long SeisLine::findNearestFlagOnLine(double xloc, double yloc) const
{
  long nearest_index = -1;
  double nearest_distance2;
  long n = numFlagsOnLine();
  for(long ixf = 0; ixf < n; ixf++)
      {
      findHelper(ixf, xloc, yloc, &nearest_index, &nearest_distance2);
      }
  return nearest_index;
}




long SeisLine::findNearestSourceOnLine(double xloc, double yloc) const
{
  long nearest_index = -1;
  double nearest_distance2;
  long n = numFlagsOnLine();
  for(long ixf = 0; ixf < n; ixf++)
      {
      if(flagHasSource(ixf))
         {
         findHelper(ixf, xloc, yloc, &nearest_index, &nearest_distance2);
         }
      }
  return nearest_index;
}




long SeisLine::findNearestReceiverOnLine(double xloc, double yloc) const
{
  long nearest_index = -1;
  double nearest_distance2;
  long n = numFlagsOnLine();
  for(long ixf = 0; ixf < n; ixf++)
      {
      if(flagHasReceiver(ixf))
         {
         findHelper(ixf, xloc, yloc, &nearest_index, &nearest_distance2);
         }
      }
  return nearest_index;
}



     // finds closest nearby flag to the indicated flag:
     // returns ixf if it is already the closest flag to (xloc,yloc).

long SeisLine::closestNearbyFlag(long ixf, double xloc, double yloc) const
{
  double dist2       = distanceSquaredToFlag(ixf, xloc, yloc);
  if(dist2 < 0.01) return ixf;
  long   ixf_plus    = ixf;
  long   ixf_minus   = ixf;
  double dist2_plus  = dist2;
  double dist2_minus = dist2;
  long   nflags      = numFlagsOnLine();
  assert(nflags >= 1);

  while(ixf_plus < nflags-1)
      {
      double test = distanceSquaredToFlag(ixf_plus+1, xloc, yloc);
      if(test >= dist2_plus) break;
      dist2_plus = test;
      ixf_plus++;
      }

  while(ixf_minus > 0)
      {
      double test = distanceSquaredToFlag(ixf_minus-1, xloc, yloc);
      if(test >= dist2_minus) break;
      dist2_minus = test;
      ixf_minus--;
      }

  if     (dist2_plus <= dist2_minus) ixf = ixf_plus;
  else if(dist2_plus >= dist2_minus) ixf = ixf_minus;
  return ixf;
}





//------------------ pass-thru to SourcesReceivers ------------------//
//------------------ pass-thru to SourcesReceivers ------------------//
//------------------ pass-thru to SourcesReceivers ------------------//
//------------------ pass-thru to SourcesReceivers ------------------//
//------------------ pass-thru to SourcesReceivers ------------------//
//------------------ pass-thru to SourcesReceivers ------------------//
//------------------ pass-thru to SourcesReceivers ------------------//
//------------------ pass-thru to SourcesReceivers ------------------//
//------------------ pass-thru to SourcesReceivers ------------------//
//------------------ pass-thru to SourcesReceivers ------------------//
//------------------ pass-thru to SourcesReceivers ------------------//
//------------------ pass-thru to SourcesReceivers ------------------//


   // ixf  = index of desired flag on this line.
   // ixs2 = index of desired source at desired flag on this line.
   // ixr2 = index of desired receiver at desired flag on this line.


//----------------- get source-receiver values ---------------------//
//----------------- get source-receiver values ---------------------//
//----------------- get source-receiver values ---------------------//

long SeisLine::numSourcesAtFlag      (long ixf)  const
{
  return fieldFlag(ixf)->numSourcesAtFlag();
}

long SeisLine::numReceiversAtFlag    (long ixf)  const
{
  return fieldFlag(ixf)->numReceiversAtFlag();
}


int SeisLine::flagHasSource    (long ixf)  const
{
  return fieldFlag(ixf)->flagHasSource();
}

int SeisLine::flagHasReceiver  (long ixf)  const
{
  return fieldFlag(ixf)->flagHasReceiver();
}


long SeisLine::sourceGroupNumber     (long ixf, long ixs2)    const
{
  return fieldFlag(ixf)->sourceGroupNumber(ixs2);
}

long SeisLine::receiverTraceNumber   (long ixf, long ixr2)  const
{
  return fieldFlag(ixf)->receiverTraceNumber(ixr2);
}

/*
long SeisLine::receiverGroupNumber   (long ixf, long ixr2)  const
{
  return fieldFlag(ixf)->receiverGroupNumber(ixr2);
}

long SeisLine::receiverChannelNumber (long ixf, long ixr2)  const
{
  return fieldFlag(ixf)->receiverChannelNumber(ixr2);
}
*/



//----------------- set source-receiver values ---------------------//
//----------------- set source-receiver values ---------------------//
//----------------- set source-receiver values ---------------------//

void SeisLine::addSourceToFlag        (long ixf, long sgroup)
{
  long ns1 = fieldFlag(ixf)->numSourcesAtFlag();
  fieldFlag(ixf)->addSourceToFlag(sgroup);
  long ns2 = fieldFlag(ixf)->numSourcesAtFlag();
  _num_sources += ns2 - ns1;
}

void SeisLine::addReceiverToFlag      (long ixf, long rtrace)
{
  long nr1 = fieldFlag(ixf)->numReceiversAtFlag();
  fieldFlag(ixf)->addReceiverToFlag(rtrace);
  long nr2 = fieldFlag(ixf)->numReceiversAtFlag();
  if(nr1 > 1) nr1 = 1;
  if(nr2 > 1) nr2 = 1;
  _num_receivers += nr2 - nr1;
}


void SeisLine::removeSourcesFromFlag (long ixf)
{
  long ns1 = fieldFlag(ixf)->numSourcesAtFlag();
  fieldFlag(ixf)->removeSourcesFromFlag();
  long ns2 = fieldFlag(ixf)->numSourcesAtFlag();
  _num_sources += ns2 - ns1;
}


void SeisLine::removeSourcesFromAllFlagsOnLine()
{
  long nflags = numFlagsOnLine();
  for(long ixf = 0; ixf < nflags; ixf++)
      {
      fieldFlag(ixf)->removeSourcesFromFlag();
      }
  _num_sources = 0;
}



void SeisLine::removeReceiversFromFlag (long ixf)
{
  long nr1 = fieldFlag(ixf)->numReceiversAtFlag();
  fieldFlag(ixf)->removeReceiversFromFlag();
  long nr2 = fieldFlag(ixf)->numReceiversAtFlag();
  if(nr1 > 1) nr1 = 1;
  if(nr2 > 1) nr2 = 1;
  _num_receivers += nr2 - nr1;
}


void SeisLine::removeReceiversFromAllFlagsOnLine()
{
  long nflags = numFlagsOnLine();
  for(long ixf = 0; ixf < nflags; ixf++)
      {
      fieldFlag(ixf)->removeReceiversFromFlag();
      }
  _num_receivers = 0;
}


void SeisLine::trimSourceAllocationsOnAllFlagsOnLine()
{
  long nflags = numFlagsOnLine();
  for(long ixf = 0; ixf < nflags; ixf++)
      {
      fieldFlag(ixf)->trimSourceAllocation();
      }
}


void SeisLine::trimReceiverAllocationsOnAllFlagsOnLine()
{
  long nflags = numFlagsOnLine();
  for(long ixf = 0; ixf < nflags; ixf++)
      {
      fieldFlag(ixf)->trimReceiverAllocation();
      }
}



//----------- pass-thru functions to individual flags --------------//
//----------- pass-thru functions to individual flags --------------//
//----------- pass-thru functions to individual flags --------------//
//----------- pass-thru functions to individual flags --------------//
//----------- pass-thru functions to individual flags --------------//
//----------- pass-thru functions to individual flags --------------//
//----------- pass-thru functions to individual flags --------------//
//----------- pass-thru functions to individual flags --------------//
//----------- pass-thru functions to individual flags --------------//
//----------- pass-thru functions to individual flags --------------//

     // ixf  = index of desired flag on this line.



//---------------------- get flag values ---------------------------//
//---------------------- get flag values ---------------------------//
//---------------------- get flag values ---------------------------//

class FieldFlag *SeisLine::getFlagPointer(long ixf)  const
{
  return fieldFlag(ixf);
}


long SeisLine::getLineIndex(FieldFlag *field_flag)    // static function.
{
  SeisLine *seis_line = field_flag->getLinePointer();
  return seis_line->_ixl;
}


long SeisLine::getFlagIndex(FieldFlag *field_flag)    // static function.
{
  return field_flag->getFlagIndex();
}



int SeisLine::flagIsSelected(long ixf)  const
{
  return _acc_select->isSelected(ixf);
}


double SeisLine::distanceToFlag(long ixf, double xloc, double yloc)  const
{
  return fieldFlag(ixf)->distanceToFlag(xloc, yloc);
}


double SeisLine::distanceSquaredToFlag
                               (long ixf, double xloc, double yloc)  const
{
  return fieldFlag(ixf)->distanceSquaredToFlag(xloc, yloc);
}


int SeisLine::flagValueIsDependent(long ixf, int ident)  const
{
  return fieldFlag(ixf)->flagValueIsDependent(ident);
}


double SeisLine::getFlagValue(long ixf, int ident)  const
{
  return fieldFlag(ixf)->getFlagValue(ident);
}



#define GETV(getIncrDistance, double)                  \
double SeisLine::getIncrDistance(long ixf) const       \
{                                                      \
  return fieldFlag(ixf)->getIncrDistance();            \
}


GETV(getShotpoint       , float )
GETV(getIncrDistance    , double)
GETV(getXloc            , double)
GETV(getYloc            , double)
GETV(getElevation       , float )
GETV(getHoleDepth       , float )
GETV(getUpholeTime      , float )
GETV(getReceiverStatic  , float )
GETV(getSourceStatic    , float )
GETV(getReceiverXskid   , float )
GETV(getReceiverYskid   , float )
GETV(getReceiverEskid   , float )
GETV(getCumDistance     , double)
GETV(getAzimuth         , double)


char SeisLine::getFlagSelectValue(long ixf)  const
{
  return _acc_select->getSelectValue(ixf);
}


long SeisLine::getCumulativeGroundPosition(long ixf)  const
{
  long n = numFlagsOnLine();
  if(n == 0) return INIL;
  long first = firstCumulativeGroundPosition();
  if(first == INIL) return INIL;
  return (first + ixf);
}


long SeisLine::getMatchableGroundPosition(long ixf)  const
{
  long n = numFlagsOnLine();
  if(n == 0) return INIL;
  long first = firstMatchableGroundPosition();
  if(first == INIL) return INIL;
  return (first + ixf);
}


long SeisLine::findCumulativeGroundPosition(long cumulative_gp)  const
{
  long n = numFlagsOnLine();
  if(n == 0) return -1;
  long first = firstCumulativeGroundPosition();
  if(first == INIL) return -1;
  long ixf = cumulative_gp - first;
  if(ixf < 0 || ixf >= n) return -1;
  return ixf;
}


long SeisLine::findMatchableGroundPosition(long matchable_gp)  const
{
  long n = numFlagsOnLine();
  if(n == 0) return -1;
  long first = firstMatchableGroundPosition();
  if(first == INIL) return -1;
  long ixf = matchable_gp - first;
  if(ixf < 0 || ixf >= n) return -1;
  return ixf;
}



float SeisLine::defaultSourceDatumStatic
                    (long ixf, float ref, float ve)  const
{
  return fieldFlag(ixf)->defaultSourceDatumStatic(ref, ve);
}


float SeisLine::defaultReceiverDatumStatic
                  (long ixf, float ref, float ve)  const
{
  return fieldFlag(ixf)->defaultReceiverDatumStatic(ref, ve);
}



//-------------------- get skidded coords ---------------------//
//-------------------- get skidded coords ---------------------//
//-------------------- get skidded coords ---------------------//

int SeisLine::receiverIsSkidded(long ixf)  const
{
  return fieldFlag(ixf)->receiverIsSkidded();
}


// also returns index of closest flag:

long SeisLine::getSkiddedCoords(long ixf,
                               float inline_skid, float crossline_skid,
                               double *x, double *y)  const
{
  if(inline_skid == 0.0 && crossline_skid == 0.0)
     {
     FieldFlag *ff = fieldFlag(ixf);
     *x = ff->getXloc();
     *y = ff->getYloc();
     return ixf;
     }

  long ixf_closest = ixf;
  long last = numFlagsOnLine() - 1;

  while(inline_skid > 0.0 && ixf < last)
     {
     FieldFlag *ff = fieldFlag(ixf);
     FieldFlag *gg = fieldFlag(ixf + 1);
     double cum1 = ff->getCumDistance();
     double cum2 = gg->getCumDistance();
     float increment = (float)(cum2 - cum1);
     if(inline_skid > 0.5 * increment && ixf < last) ixf_closest = ixf + 1;
     if(inline_skid >       increment && ixf < last)
         {
         ixf++;
         inline_skid -= increment;
         }
     else break;
     }

  while(inline_skid < 0.0 && ixf > 0)
     {
     FieldFlag *ff = fieldFlag(ixf);
     FieldFlag *gg = fieldFlag(ixf - 1);
     double cum2 = ff->getCumDistance();
     double cum1 = gg->getCumDistance();
     float increment = (float)(cum2 - cum1);
     if(-inline_skid > 0.5 * increment && ixf > 0) ixf_closest = ixf - 1;
     if(-inline_skid >       increment && ixf > 0)
         {
         ixf--;
         inline_skid += increment;
         }
     else break;
     }

  FieldFlag *ff = fieldFlag(ixf);
  double xloc = ff->getXloc();
  double yloc = ff->getYloc();
  if(inline_skid > 0.0 && ixf < last) ff = fieldFlag(ixf + 1);
  float sina = ff->getSineOfAzimuth();
  float cosa = ff->getCosineOfAzimuth();
  *x = xloc + inline_skid * cosa - crossline_skid * sina;
  *y = yloc + inline_skid * sina + crossline_skid * cosa;
  return ixf_closest;
}



long SeisLine::getSkiddedCoordsPlus(long ixf,
                               float inline_skid, float crossline_skid,
                               double *x, double *y)  const
{
  FieldFlag *ff = fieldFlag(ixf);
     inline_skid += ff->getReceiverXskid();
  crossline_skid += ff->getReceiverYskid();
  return getSkiddedCoords(ixf, inline_skid, crossline_skid, x, y);
}



long SeisLine::getSkiddedReceiverCoords(long ixf,
                               double *x, double *y)  const
{
  FieldFlag *ff = fieldFlag(ixf);
  float    inline_skid = ff->getReceiverXskid();
  float crossline_skid = ff->getReceiverYskid();
  return getSkiddedCoords(ixf, inline_skid, crossline_skid, x, y);
}



//------------------------ set flag values -------------------------//
//------------------------ set flag values -------------------------//
//------------------------ set flag values -------------------------//


void SeisLine::setFlagValue(long ixf, int ident, double value)
{
  beforeValueChanged(ident, ixf);
  fieldFlag(ixf)->setFlagValue(ident, value);
  afterValueChanged(ident, ixf);
}


void SeisLine::setDependentFlagValue(long ixf, int ident, double value)
{
  beforeValueChanged(ident, ixf);
  fieldFlag(ixf)->setDependentFlagValue(ident, value);
  afterValueChanged(ident, ixf);
}


#define SETV(setIncrDistance, double, ident)                \
void SeisLine::setIncrDistance(long ixf, double value)      \
{                                                           \
  beforeValueChanged(ident, ixf);                           \
  fieldFlag(ixf)->setIncrDistance(value);                   \
  afterValueChanged(ident, ixf);                            \
}


SETV(setShotpoint     , float , FG_SHOT )
SETV(setIncrDistance  , double, FG_DIST )
SETV(setXloc          , double, FG_XLOC )
SETV(setYloc          , double, FG_YLOC )
SETV(setElevation     , float , FG_ELEV )
SETV(setHoleDepth     , float , FG_HD   )
SETV(setUpholeTime    , float , FG_TUH  )
SETV(setReceiverStatic, float , FG_RSTAT)
SETV(setSourceStatic  , float , FG_SSTAT)
SETV(setReceiverXskid , float , FG_XSKID)
SETV(setReceiverYskid , float , FG_YSKID)
SETV(setReceiverEskid , float , FG_ESKID)


void SeisLine::setDeadSourceCode(long ixf, int value)
{
  fieldFlag(ixf)->setDeadSourceCode(value);
}


void SeisLine::setDeadReceiverCode(long ixf, int value)
{
  fieldFlag(ixf)->setDeadReceiverCode(value);
}


void SeisLine::setSourceMaybeDead(long ixf, int value)
{
  fieldFlag(ixf)->setSourceMaybeDead(value);
}


void SeisLine::setReceiverMaybeDead(long ixf, int value)
{
  fieldFlag(ixf)->setReceiverMaybeDead(value);
}


void SeisLine::setFlagSelectValue(long ixf, char value)
{
  beforeValueChanged(FG_SEL, ixf);
  _acc_select->setSelectValue(ixf, value);
  afterValueChanged(FG_SEL, ixf);
}


void SeisLine::incrementFlagSelectValue(long ixf)
{
  beforeValueChanged(FG_SEL, ixf);
  _acc_select->incrementSelectValue(ixf);
  afterValueChanged(FG_SEL, ixf);
}


void SeisLine::clearFlagSelections()
{
  long n = numFlagsOnLine();
  _acc_select->preChange(0, n, n);
  _acc_select->clearSelections();
  _acc_select->postChange();
}



//--------------- find flags bracketing true midpoint ----------------//
//--------------- find flags bracketing true midpoint ----------------//
//--------------- find flags bracketing true midpoint ----------------//

          // public.

void SeisLine::findFlagsBracketingTrueMidpoint
                     (double cmp_xloc, double cmp_yloc,
                      long *ixfa, long *ixfb, float *wa, float *wb)
{
///// check for only one flag:

  long nflags = numFlagsOnLine();
  if(nflags == 1)
      {
      *ixfa = 0;
      *ixfb = 0;
      *wa   = 0.5;
      *wb   = 0.5;
      return;
      }

///// find nearest flag to this midpoint:

  double x = cmp_xloc;
  double y = cmp_yloc;
  *ixfa         = findNearestFlagOnLine(       x, y);
  double dist2a = distanceSquaredToFlag(*ixfa, x, y);
  if(dist2a < 0.01)
      {
      *ixfb = *ixfa;
      *wa   = 0.5;
      *wb   = 0.5;
      return;
      }

///// find second nearest flag to this midpoint:

  long  ixfa_prev = MaximumValue(*ixfa - 1, 0);
  long  ixfa_next = MinimumValue(*ixfa + 1, nflags - 1);
  double dist2b;
  double dist2_prev = distanceSquaredToFlag(ixfa_prev, x, y);
  double dist2_next = distanceSquaredToFlag(ixfa_next, x, y);
  if     (*ixfa == 0)
            {
            *ixfb  = ixfa_next;
            dist2b = dist2_next;
            }
  else if(*ixfa == nflags-1)
            {
            *ixfb  = ixfa_prev;
            dist2b = dist2_prev;
            }
  else if(dist2_prev <= dist2_next)
            {
            *ixfb  = ixfa_prev;
            dist2b = dist2_prev;
            }
  else
            {
            *ixfb  = ixfa_next;
            dist2b = dist2_next;
            }

///// get corresponding weights:

  double xdiff = getXloc(*ixfb) - getXloc(*ixfa);
  double ydiff = getYloc(*ixfb) - getYloc(*ixfa);
  double dab2  = xdiff * xdiff + ydiff * ydiff;
  double dab   = sqrt(dab2);
  if(dab == 0.0)
      {
      *ixfb = *ixfa;
      *wa   = 0.5;
      *wb   = 0.5;
      return;
      }
  double dac = (dist2a - dist2b + dab2) / (2.0 * dab);
  if(dac <= 0.0 || dac >= dab)
      {
      *ixfb = *ixfa;
      *wa   = 0.5;
      *wb   = 0.5;
      return;
      }
  *wa = 1.0 / dac;
  *wb = 1.0 / (dab - dac);
  float sum = *wa + *wb;
  *wa /= sum;
  *wb /= sum;
}



//--------------- find flags bracketing center midpoint ---------------//
//--------------- find flags bracketing center midpoint ---------------//
//--------------- find flags bracketing center midpoint ---------------//

      // public.
      // sets past = distance (in gp units) past end of line (or zero).

void SeisLine::findFlagsBracketingCenterMidpoint
                 (float cmp_matchable_gp,
                  long *ixfa, long *ixfb, float *wa, float *wb, float *past)
{
///// check for only one flag:

  long nflags = numFlagsOnLine();
  if(nflags == 1)
      {
      *ixfa = 0;
      *ixfb = 0;
      *wa   = 0.5;
      *wb   = 0.5;
      *past = 0.0;
      return;
      }

////////// matchable ground position(ixl, ixf) =
//////////   first matchable ground position + ixf.

  long first = firstMatchableGroundPosition();

///// test for being at (or past) beginning or end:

  float x   = cmp_matchable_gp;
  float xa  = (float)first;
//float xa1 = (float)(first + 1);
  float xb  = (float)(first + nflags - 1);
//float xb1 = (float)(first + nflags - 2);

  if(x < xa)
      {
      *ixfa = 0;
      *ixfb = 0;
      *wa   = 0.5;
      *wb   = 0.5;
      *past = AbsoluteValue(xa - x);
      return;
      }

  if(x > xb)
      {
      *ixfa = nflags-1;
      *ixfb = nflags-1;
      *wa   = 0.5;
      *wb   = 0.5;
      *past = AbsoluteValue(xb - x);
      return;
      }

///// find nearest flag to this x location:

  float dista = 0.0;
  for(long i = 0; i < nflags; i++)
      {
      float test = x - (float)(first + i);
      test = AbsoluteValue(test);
      if(i == 0 || test < dista)
          {
          *ixfa = i;
          dista = test;
          }
      }
  if(dista < 0.01)
      {
      *ixfb = *ixfa;
      *wa   = 0.5;
      *wb   = 0.5;
      *past = 0.0;  // added 9/23/98.
      return;
      }

///// find second nearest flag to this x location:

  long  ixfa_prev = MaximumValue(*ixfa - 1, 0);
  long  ixfa_next = MinimumValue(*ixfa + 1, nflags - 1);
  float distb;
  float dist_prev = x - (float)(first + ixfa_prev);
  float dist_next = x - (float)(first + ixfa_next);
  dist_prev = AbsoluteValue(dist_prev);
  dist_next = AbsoluteValue(dist_next);
  if     (*ixfa == 0)
            {
            *ixfb = ixfa_next;
            distb = dist_next;
            }
  else if(*ixfa == nflags-1)
            {
            *ixfb = ixfa_prev;
            distb = dist_prev;
            }
  else if(dist_prev <= dist_next)
            {
            *ixfb = ixfa_prev;
            distb = dist_prev;
            }
  else
            {
            *ixfb = ixfa_next;
            distb = dist_next;
            }

///// get corresponding weights:

  *wa = 1.0 / dista;
  *wb = 1.0 / distb;
  float sum = *wa + *wb;
  *wa /= sum;
  *wb /= sum;
  *past = 0.0;  // added 9/23/98.
}



//-------------------------- end -------------------------------//
//-------------------------- end -------------------------------//
//-------------------------- end -------------------------------//

