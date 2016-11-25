
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
//--------------------- field_flag.cc ---------------------//
//--------------------- field_flag.cc ---------------------//
//--------------------- field_flag.cc ---------------------//

//         implementation file for the FieldFlag class
//                  not derived from any class
//                      subdirectory geom



#include "geom/field_flag.hh"
#include "geom/fg_constants.hh"
#include "oprim/integer_list.hh"
#include "cprim.h"
#include "named_constants.h"
#include <iostream.h>
#include <math.h>
#include <stdlib.h>
#include <string.h>
#include <assert.h>
#include <limits.h>



//----------------------- initialization ----------------------//
//----------------------- initialization ----------------------//
//----------------------- initialization ----------------------//

//Redefined to prevent Linux compiler warning of anonymous enums
//enum { SOURCE_MAYBE_DEAD   = LAST_FIELD_FLAG_VARIABLE + 1 };
//enum { RECEIVER_MAYBE_DEAD = LAST_FIELD_FLAG_VARIABLE + 2 };
//enum { KOUNT               = sizeof(int) * CHAR_BIT };
enum { SOURCE_MAYBE_DEAD   = LAST_FIELD_FLAG_VARIABLE + 1,
       RECEIVER_MAYBE_DEAD = LAST_FIELD_FLAG_VARIABLE + 2,
       KOUNT               = sizeof(int) * CHAR_BIT       };


static       int    offset[KOUNT];     //   set in init_stuff and set_stuff.
static       int    starting = TRUE;   // reset in init_stuff.
static const int    BAD_OFFSET = -9999;
static const float  FZERO = 0.0;
static const double DZERO = 0.0;
static const double DONE  = 1.0;
/*
static const double DEGREES_PER_RADIAN = 57.2957795131;
      // now defined in named_constants.h
*/



// variable is float  if offset is negative.
// variable is double if offset is positive.


static void init_stuff()
{
  assert(SOURCE_MAYBE_DEAD   >= 0 && SOURCE_MAYBE_DEAD   < KOUNT);
  assert(RECEIVER_MAYBE_DEAD >= 0 && RECEIVER_MAYBE_DEAD < KOUNT);
  for(int ident = 0; ident < KOUNT; ident++)
          {
          offset[ident] = BAD_OFFSET;
          }
  starting = FALSE;
}


static void set_stuff(int ident, void *variable, void *origin, int isfloat)
{
  assert(ident >= 0 && ident < KOUNT);
  offset[ident] = (char*)(variable) - (char*)origin;
  if(isfloat) offset[ident] = -offset[ident];
}


#define  ISFLOAT(ident)  ( offset[ident] < 0 )
#define   FVALUE(ident)  ( *((float *)((char*)this - offset[ident])) )
#define   DVALUE(ident)  ( *((double*)((char*)this + offset[ident])) )



//---------------------- constructor -----------------------//
//---------------------- constructor -----------------------//
//---------------------- constructor -----------------------//


FieldFlag::FieldFlag(class SeisLine *line)
           :
          _line            (line),
          _sss             (NULL),
          _rrr             (NULL),
          _ixf             (0),
          _depend          (~0),
          _shotpoint       (DZERO),
          _dist            (DZERO),
          _xloc            (DZERO),
          _yloc            (DZERO),
          _elev            (FZERO),
          _hd              (FZERO),
          _tuh             (FZERO),
          _rstat           (FZERO),
          _sstat           (FZERO),
          _xskid           (FZERO),
          _yskid           (FZERO),
          _eskid           (FZERO),
          _cum             (DZERO),
          _select          (FZERO),
          _sina            (DZERO),
          _cosa            (DONE),
          _sdead           (ZT_CODE_NONE),
          _rdead           (ZT_CODE_NONE)

{
  if(starting)
      {
      init_stuff();
      set_stuff(FG_SHOT , &_shotpoint, this, TRUE );
      set_stuff(FG_DIST , &_dist     , this, FALSE);
      set_stuff(FG_XLOC , &_xloc     , this, FALSE);
      set_stuff(FG_YLOC , &_yloc     , this, FALSE);
      set_stuff(FG_ELEV , &_elev     , this, TRUE );
      set_stuff(FG_HD   , &_hd       , this, TRUE );
      set_stuff(FG_TUH  , &_tuh      , this, TRUE );
      set_stuff(FG_RSTAT, &_rstat    , this, TRUE );
      set_stuff(FG_SSTAT, &_sstat    , this, TRUE );
      set_stuff(FG_XSKID, &_xskid    , this, TRUE );
      set_stuff(FG_YSKID, &_yskid    , this, TRUE );
      set_stuff(FG_ESKID, &_eskid    , this, TRUE );
      set_stuff(FG_SEL  , &_select   , this, TRUE );
      set_stuff(FG_CUM  , &_cum      , this, FALSE);
      }
  TURN_OFF_FLAG(SOURCE_MAYBE_DEAD);
  TURN_OFF_FLAG(RECEIVER_MAYBE_DEAD);
  TURN_ON_FLAG (FG_CUM);
  TURN_ON_FLAG (FG_AZIM);
}



//------------------------ destructor --------------------------//
//------------------------ destructor --------------------------//
//------------------------ destructor --------------------------//

FieldFlag::~FieldFlag()
{
  if(_sss) delete _sss;    // or IntegerList::clearList(_sss);
  if(_rrr) delete _rrr;    // or IntegerList::clearList(_rrr);
}



//----------------- pass-thru to sources and receivers ---------------//
//----------------- pass-thru to sources and receivers ---------------//
//----------------- pass-thru to sources and receivers ---------------//
//----------------- pass-thru to sources and receivers ---------------//
//----------------- pass-thru to sources and receivers ---------------//
//----------------- pass-thru to sources and receivers ---------------//
//----------------- pass-thru to sources and receivers ---------------//
//----------------- pass-thru to sources and receivers ---------------//
//----------------- pass-thru to sources and receivers ---------------//
//----------------- pass-thru to sources and receivers ---------------//
//----------------- pass-thru to sources and receivers ---------------//
//----------------- pass-thru to sources and receivers ---------------//
//----------------- pass-thru to sources and receivers ---------------//

   // ixs2 = index of desired source at this flag.
   // ixr2 = index of desired receiver at this flag.


//------------------- get source/receiver values ----------------//
//------------------- get source/receiver values ----------------//
//------------------- get source/receiver values ----------------//


long FieldFlag::numSourcesAtFlag()  const
      { return IntegerList::numElements(_sss); }

long FieldFlag::numReceiversAtFlag()  const
      { return IntegerList::numElements(_rrr); }

int FieldFlag::flagHasSource()  const
      { return IntegerList::numElements(_sss) > 0; }

int FieldFlag::flagHasReceiver()  const
      { return IntegerList::numElements(_rrr) > 0; }

long FieldFlag::sourceGroupNumber(long ixs2)  const
{ return IntegerList::getElement(_sss, ixs2); }

long FieldFlag::receiverTraceNumber(long ixr2)  const
  { return IntegerList::getElement(_rrr, ixr2); }



//------------------- set source/receiver values ----------------//
//------------------- set source/receiver values ----------------//
//------------------- set source/receiver values ----------------//


void       FieldFlag::addSourceToFlag(long sgroup)
{ _sss = IntegerList::addElement    (_sss, sgroup); }

void       FieldFlag::addReceiverToFlag(long rtrace)
{ _rrr = IntegerList::addElement      (_rrr, rtrace); }

void FieldFlag::removeSourcesFromFlag()
   { _sss = IntegerList::clearList(_sss); }

void FieldFlag::removeReceiversFromFlag()
   { _rrr = IntegerList::clearList(_rrr); }

void FieldFlag::trimSourceAllocation()
       { IntegerList::trimAllocation(_sss); }

void FieldFlag::trimReceiverAllocation()
         { IntegerList::trimAllocation(_rrr); }


//------------------- public access to this flag ------------------//
//------------------- public access to this flag ------------------//
//------------------- public access to this flag ------------------//
//------------------- public access to this flag ------------------//
//------------------- public access to this flag ------------------//
//------------------- public access to this flag ------------------//
//------------------- public access to this flag ------------------//
//------------------- public access to this flag ------------------//
//------------------- public access to this flag ------------------//
//------------------- public access to this flag ------------------//
//------------------- public access to this flag ------------------//
//------------------- public access to this flag ------------------//
//------------------- public access to this flag ------------------//


//----------------------- get flag values ------------------------//
//----------------------- get flag values ------------------------//
//----------------------- get flag values ------------------------//


double FieldFlag::distanceToFlag(double xloc, double yloc)  const
{
  double distance2 = distanceSquaredToFlag(xloc, yloc);
  return sqrt(distance2);
}



double FieldFlag::distanceSquaredToFlag(double xloc, double yloc)  const
{
  double xdist = xloc - _xloc;
  double ydist = yloc - _yloc;
  return (xdist * xdist + ydist * ydist);
}


float FieldFlag::getAzimuth()  const
{
  return DEGREES_PER_RADIAN * atan2(_cosa, _sina);
}



/// for speed, the following code does not protect against ident
/// out of range.

double FieldFlag::getFlagValue(int ident)  const
{
  if(ident == FG_AZIM) return getAzimuth();
  if(ISFLOAT(ident)) return (double)FVALUE(ident);
  return                            DVALUE(ident);
}



int FieldFlag::sourceMaybeDead()  const
{
  return FLAG_IS_ON(SOURCE_MAYBE_DEAD);
}


int FieldFlag::receiverMaybeDead()  const
{
  return FLAG_IS_ON(RECEIVER_MAYBE_DEAD);
}



float FieldFlag::defaultSourceDatumStatic(float ref, float ve)  const
{
  return _sstat - 1000.0 * (_elev - _hd - ref) / ve;
}



float FieldFlag::defaultReceiverDatumStatic(float ref, float ve)  const
{
  return _rstat - _tuh - 1000.0 * (_elev + _eskid  - _hd - ref) / ve;
}



//----------------------- set flag values ------------------------//
//----------------------- set flag values ------------------------//
//----------------------- set flag values ------------------------//


///// set the value.
///// also set the dependency flag to FALSE.

void FieldFlag::setFlagValue(int ident, double value)
{
  assert(ident != FG_AZIM);
  if(ISFLOAT(ident)) FVALUE(ident) = (float)value;
  else               DVALUE(ident) = value;
  if(value == DNIL) TURN_ON_FLAG (ident);
  else              TURN_OFF_FLAG(ident);
}



///// set the value.
///// also set the dependency flag to TRUE.

void FieldFlag::setDependentFlagValue(int ident, double value)
{
  assert(ident != FG_AZIM);
  if(ISFLOAT(ident)) FVALUE(ident) = (float)value;
  else               DVALUE(ident) = value;
  TURN_ON_FLAG(ident);
}


/*
void FieldFlag::setSimpleFlagValue(int ident, double value)
{
  // omitted for efficiency: assert(ident != FG_AZIM);
  if(ISFLOAT(ident)) FVALUE(ident) = (float)value;
  else               DVALUE(ident) = value;
}
*/



///// set the value.
///// also set the dependency flag to TRUE or FALSE.


void FieldFlag::setShotpoint(float value)
{
  _shotpoint = value;
  if(value == FNIL) TURN_ON_FLAG (FG_SHOT);
  else              TURN_OFF_FLAG(FG_SHOT);
}


void FieldFlag::setIncrDistance(double value)
{
  _dist = value;
  if(value == DNIL) TURN_ON_FLAG (FG_DIST);
  else              TURN_OFF_FLAG(FG_DIST);
}


void FieldFlag::setXloc(double value)
{
  _xloc = value;
  if(value == DNIL) TURN_ON_FLAG (FG_XLOC);
  else              TURN_OFF_FLAG(FG_XLOC);
}


void FieldFlag::setYloc(double value)
{
  _yloc = value;
  if(value == DNIL) TURN_ON_FLAG (FG_YLOC);
  else              TURN_OFF_FLAG(FG_YLOC);
}


void FieldFlag::setElevation(float value)
{
  _elev = value;
  if(value == FNIL) TURN_ON_FLAG (FG_ELEV);
  else              TURN_OFF_FLAG(FG_ELEV);
}


void FieldFlag::setHoleDepth(float value)
{
  _hd = value;
  if(value == FNIL) TURN_ON_FLAG (FG_HD);
  else              TURN_OFF_FLAG(FG_HD);
}


void FieldFlag::setUpholeTime(float value)
{
  _tuh = value;
  if(value == FNIL) TURN_ON_FLAG (FG_TUH);
  else              TURN_OFF_FLAG(FG_TUH);
}


void FieldFlag::setReceiverStatic(float value)
{
  _rstat = value;
  if(value == FNIL) TURN_ON_FLAG (FG_RSTAT);
  else              TURN_OFF_FLAG(FG_RSTAT);
}


void FieldFlag::setSourceStatic(float value)
{
  _sstat = value;
  if(value == FNIL) TURN_ON_FLAG (FG_SSTAT);
  else              TURN_OFF_FLAG(FG_SSTAT);
}


void FieldFlag::setReceiverXskid(float value)
{
  _xskid = value;
  if(value == FNIL) TURN_ON_FLAG (FG_XSKID);
  else              TURN_OFF_FLAG(FG_XSKID);
}


void FieldFlag::setReceiverYskid(float value)
{
  _yskid = value;
  if(value == FNIL) TURN_ON_FLAG (FG_YSKID);
  else              TURN_OFF_FLAG(FG_YSKID);
}


void FieldFlag::setReceiverEskid(float value)
{
  _eskid = value;
  if(value == FNIL) TURN_ON_FLAG (FG_ESKID);
  else              TURN_OFF_FLAG(FG_ESKID);
}



void FieldFlag::setSourceMaybeDead(int value)
{
  if(value) TURN_ON_FLAG (SOURCE_MAYBE_DEAD);
  else      TURN_OFF_FLAG(SOURCE_MAYBE_DEAD);
}


void FieldFlag::setReceiverMaybeDead(int value)
{
  if(value) TURN_ON_FLAG (RECEIVER_MAYBE_DEAD);
  else      TURN_OFF_FLAG(RECEIVER_MAYBE_DEAD);
}



//------------------ get skidded coords ------------------------//
//------------------ get skidded coords ------------------------//
//------------------ get skidded coords ------------------------//

          // public.

int FieldFlag::receiverIsSkidded()  const
{
  return (_xskid != FZERO || _yskid != FZERO);
}



//------------------ update cum dist ----------------------------//
//------------------ update cum dist ----------------------------//
//------------------ update cum dist ----------------------------//

     // public.
     // sets cumulative inline distance, plus sine and cosine of
     //   azimuth, using _xloc and _yloc from this and previous flag.
     // if previous flag is NULL, sets sine and cosine of azimuth,
     //   using _xloc and _yloc from this and next flag, and sets
     //   cumulative inline distance to zero.
     // if both prev and next are NULL, sets sine and cosine of
     //   azimuth to default values, and sets cumulative inline
     //   distance to zero.

void FieldFlag::updateCumDist(FieldFlag *prev, FieldFlag *next)
{
  double xdiff, ydiff, hyp;
  if(prev)                           // this is not the first flag.
      {
      xdiff = _xloc - prev->_xloc;
      ydiff = _yloc - prev->_yloc;
      hyp   = sqrt(xdiff * xdiff + ydiff * ydiff);
      _cum  = prev->_cum + hyp;
      }
  else if(next)                      // this is the first flag.
      {
      xdiff = next->_xloc - _xloc;
      ydiff = next->_yloc - _yloc;
      hyp   = sqrt(xdiff * xdiff + ydiff * ydiff);
      _cum  = DZERO;
      }
  else                               // this is the only flag.
      {
      hyp = DZERO;
      _cum = DZERO;
      }
  if(hyp > DZERO)
      {
      _sina = ydiff / hyp;
      _cosa = xdiff / hyp;
      }
  else
      {
      _sina = DZERO;
      _cosa = DONE;
      }
}



//-------------------------- end -------------------------------//
//-------------------------- end -------------------------------//
//-------------------------- end -------------------------------//

