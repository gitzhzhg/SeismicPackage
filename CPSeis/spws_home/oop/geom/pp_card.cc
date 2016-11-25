
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
//--------------------- pp_card.cc ---------------------//
//--------------------- pp_card.cc ---------------------//
//--------------------- pp_card.cc ---------------------//

//         implementation file for the PpCard class
//                  not derived from any class
//                      subdirectory geom



#include "geom/pp_card.hh"
#include "geom/fg_constants.hh"
#include "geom/fg_connect.hh"
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


enum { KOUNT = sizeof(int) * CHAR_BIT };

static       int    offset[KOUNT];     //   set in init_stuff and set_stuff.
static       int    starting = TRUE;   // reset in init_stuff.
static const int    BAD_OFFSET = -9999;
static const float  FZERO = 0.0;


// variable is float if offset is negative.
// variable is long  if offset is positive.


static void init_stuff()
{
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


#define       ISFLOAT(ident)  ( offset[ident] < 0 )
#define        FVALUE(ident)  ( *((float*)((char*)this - offset[ident])) )
#define        IVALUE(ident)  ( *((long *)((char*)this + offset[ident])) )
/*
#define      VALIDATE(ident)    assert(ident >= 0 && ident < KOUNT); \
                                assert(offset[ident] != BAD_OFFSET);
*/
#define      VALIDATE(ident)



//---------------------- constructor -----------------------//
//---------------------- constructor -----------------------//
//---------------------- constructor -----------------------//


PpCard::PpCard()
           :
          _depend          (~0),
          _file            (0),
          _sshot           (FZERO),
          _sline           (0),
          _rshot           (FZERO),
          _rline           (0),
          _pattern         (0),
          _xskid           (FZERO),
          _yskid           (FZERO),
          _hold            (1),
          _elev            (FNIL),
          _hd              (FNIL),
          _tuh             (FNIL),
          _smove           (0),
          _rmove           (0),
          _ngroups         (1),
          _ntraces         (0),
          _thru_gr         (0),
          _thru_tr         (0),

          _first_group     (0),
          _ixl_pp_source   (-1),
          _ixf_pp_source   (-1),
          _source_error    (TRUE),

          _first_trace     (0),
          _nchan           (0),
          _ixl_pp_rec      (-1),
          _ixf_pp_rec      (-1),
          _ixrp_first      (-1),
          _ixrp_last       (-1),
          _ixrp_first_irreg(-1),
          _ixrp_last_irreg (-1),
          _ixrp_chan1      (-1),
          _rp_line_chan1   (INIL),
          _rp_mgp_chan1    (INIL),
          _rec_error       (TRUE)
{
  if(starting)
      {
      init_stuff();
      set_stuff(PP_FILE   , &_file   , this, FALSE);
      set_stuff(PP_SSHOT  , &_sshot  , this, TRUE);
      set_stuff(PP_SLINE  , &_sline  , this, FALSE);
      set_stuff(PP_RSHOT  , &_rshot  , this, TRUE);
      set_stuff(PP_RLINE  , &_rline  , this, FALSE);
      set_stuff(PP_PAT    , &_pattern, this, FALSE);
      set_stuff(PP_XSKID  , &_xskid  , this, TRUE);
      set_stuff(PP_YSKID  , &_yskid  , this, TRUE);
      set_stuff(PP_HOLD   , &_hold   , this, FALSE);
      set_stuff(PP_ELEV   , &_elev   , this, TRUE);
      set_stuff(PP_HD     , &_hd     , this, TRUE);
      set_stuff(PP_TUH    , &_tuh    , this, TRUE);
      set_stuff(PP_SMOVE  , &_smove  , this, FALSE);
      set_stuff(PP_RMOVE  , &_rmove  , this, FALSE);
      set_stuff(PP_THRU_GR, &_thru_gr, this, FALSE);
      set_stuff(PP_THRU_TR, &_thru_tr, this, FALSE);
      set_stuff(PP_NGROUPS, &_ngroups, this, FALSE);
      set_stuff(PP_NTRACES, &_ntraces, this, FALSE);
      }
  TURN_ON_PP(PP_THRU_GR);
  TURN_ON_PP(PP_THRU_TR);
}



//------------------------ destructor --------------------------//
//------------------------ destructor --------------------------//
//------------------------ destructor --------------------------//

PpCard::~PpCard()
{
}



//----------------- public access to this PP card ------------------//
//----------------- public access to this PP card ------------------//
//----------------- public access to this PP card ------------------//
//----------------- public access to this PP card ------------------//
//----------------- public access to this PP card ------------------//
//----------------- public access to this PP card ------------------//
//----------------- public access to this PP card ------------------//
//----------------- public access to this PP card ------------------//
//----------------- public access to this PP card ------------------//
//----------------- public access to this PP card ------------------//
//----------------- public access to this PP card ------------------//



//--------------------- get pp value ------------------------------//
//--------------------- get pp value ------------------------------//
//--------------------- get pp value ------------------------------//


/*
float PpCard::getPpValue(int ident)  const
{
  VALIDATE(ident);
  if(ISFLOAT(ident)) return FVALUE(ident);
  return             (float)IVALUE(ident);
}
*/
double PpCard::getPpValue(int ident)  const
{
  VALIDATE(ident);
  if(ISFLOAT(ident)) return (double)FVALUE(ident);
  return                    (double)IVALUE(ident);
}



//---------------------- set pp value ----------------------------//
//---------------------- set pp value ----------------------------//
//---------------------- set pp value ----------------------------//


/*
void PpCard::setPpValue(int ident, float value)
{
  VALIDATE(ident);
  if     (ident == PP_HOLD)    _hold   = MaximumValue(NearestInteger(value),1);
  else if(ident == PP_NGROUPS) _ngroups= MaximumValue(NearestInteger(value),1);
  else if(ident == PP_NTRACES) _ntraces= MaximumValue(NearestInteger(value),0);
  else if(ISFLOAT(ident)) FVALUE(ident) = value;
  else                    IVALUE(ident) = NearestInteger(value);
  TURN_OFF_PP(ident);
}
*/
void PpCard::setPpValue(int ident, double value)
{
  VALIDATE(ident);
  if     (ident == PP_HOLD)    _hold   = MaximumValue(NearestInteger(value),1);
  else if(ident == PP_NGROUPS) _ngroups= MaximumValue(NearestInteger(value),1);
  else if(ident == PP_NTRACES) _ntraces= MaximumValue(NearestInteger(value),0);
  else if(ISFLOAT(ident)) FVALUE(ident) = (float)value;
  else                    IVALUE(ident) = NearestInteger(value);
  TURN_OFF_PP(ident);
}



//------------------ set dependent pp value ------------------------//
//------------------ set dependent pp value ------------------------//
//------------------ set dependent pp value ------------------------//

     /// the value argument is not used for some of the variables:

/*
void PpCard::setDependentPpValue(int ident, float value)
{
  VALIDATE(ident);
  if     (ident == PP_HOLD   ) _hold    = 1;
  else if(ident == PP_NGROUPS) _ngroups = 1;
//else if(ident == PP_NTRACES) _ntraces = 0;
  else if(ident == PP_ELEV   ) _elev    = FNIL;
  else if(ident == PP_HD     ) _hd      = FNIL;
  else if(ident == PP_TUH    ) _tuh     = FNIL;
  else if(ISFLOAT(ident)) FVALUE(ident) = value;
  else                    IVALUE(ident) = NearestInteger(value);
  TURN_ON_PP(ident);
}
*/
void PpCard::setDependentPpValue(int ident, double value)
{
  VALIDATE(ident);
  if     (ident == PP_HOLD   ) _hold    = 1;
  else if(ident == PP_NGROUPS) _ngroups = 1;
//else if(ident == PP_NTRACES) _ntraces = 0;
  else if(ident == PP_ELEV   ) _elev    = FNIL;
  else if(ident == PP_HD     ) _hd      = FNIL;
  else if(ident == PP_TUH    ) _tuh     = FNIL;
  else if(ISFLOAT(ident)) FVALUE(ident) = (float)value;
  else                    IVALUE(ident) = NearestInteger(value);
  TURN_ON_PP(ident);
}



/*
//---------------------- set simple pp value ----------------------------//
//---------------------- set simple pp value ----------------------------//
//---------------------- set simple pp value ----------------------------//


void PpCard::setSimplePpValue(int ident, float value)
{
  VALIDATE(ident);
  assert(ident != PP_HOLD);
  assert(ident != PP_NGROUPS);
  assert(ident != PP_NTRACES);
  assert(ident != PP_ELEV);
  assert(ident != PP_HD);
  assert(ident != PP_TUH);
  if(ISFLOAT(ident)) FVALUE(ident) = value;
  else               IVALUE(ident) = NearestInteger(value);
}
*/



//----------------------- get card values ------------------------//
//----------------------- get card values ------------------------//
//----------------------- get card values ------------------------//


long PpCard::getNumChannelsOnCard()  const
{
  if(_ntraces == INIL || _ngroups == INIL || _ngroups == 0) return 0;
  assert(_ngroups >  0);
  assert(_ntraces >= 0);
  long nchan = _ntraces / _ngroups;
  assert(_ntraces == _ngroups * nchan);
  return nchan;
}


long PpCard::getThruFileNumber()  const
{
  if(_file == INIL || _ngroups == INIL) return INIL;
  return (_file + _ngroups - 1);
}


long PpCard::getFirstGroupNumber()  const
{
  if(_thru_gr == INIL || _ngroups == INIL) return INIL;
  return (_thru_gr - _ngroups + 1);
}


long PpCard::getFirstTraceNumber()  const
{
  if(_thru_tr == INIL || _ntraces == INIL) return INIL;
  return (_thru_tr - _ntraces + 1);
}



//----------------------- set card values ------------------------//
//----------------------- set card values ------------------------//
//----------------------- set card values ------------------------//


void   PpCard::setFirstFileNumber   (long  value)
{
  _file = value;
  TURN_OFF_PP(PP_FILE);
}


void   PpCard::setSourceShotpoint   (float value)
{
  _sshot = value;
  TURN_OFF_PP(PP_SSHOT);
}


void   PpCard::setSourceLine        (long  value)
{
  _sline = value;
  TURN_OFF_PP(PP_SLINE);
}


void   PpCard::setReceiverShotpoint (float value)
{
  _rshot = value;
  TURN_OFF_PP(PP_RSHOT);
}


void   PpCard::setReceiverLine      (long  value)
{
  _rline = value;
  TURN_OFF_PP(PP_RLINE);
}


void   PpCard::setPatternNumber     (long  value)
{
  _pattern = value;
  TURN_OFF_PP(PP_PAT);
}


void   PpCard::setSourceXskid       (float value)
{
  _xskid = value;
  TURN_OFF_PP(PP_XSKID);
}


void   PpCard::setSourceYskid       (float value)
{
  _yskid = value;
  TURN_OFF_PP(PP_YSKID);
}


void   PpCard::setSkidHold          (long  value)
{
  _hold = MaximumValue(value, 1);
  TURN_OFF_PP(PP_HOLD);
}


void   PpCard::setNewElevation      (float value)
{
  _elev = value;
  TURN_OFF_PP(PP_ELEV);
}


void   PpCard::setNewHoleDepth      (float value)
{
  _hd = value;
  TURN_OFF_PP(PP_HD);
}


void   PpCard::setNewUpholeTime     (float value)
{
  _tuh = value;
  TURN_OFF_PP(PP_TUH);
}


void   PpCard::setSourceMove        (long  value)
{
  _smove = value;
  TURN_OFF_PP(PP_SMOVE);
}


void   PpCard::setReceiverMove      (long  value)
{
  _rmove = value;
  TURN_OFF_PP(PP_RMOVE);
}


void   PpCard::setThruGroupNumber   (long  value)
{
  _thru_gr = value;
  TURN_OFF_PP(PP_THRU_GR);
}


void   PpCard::setThruTraceNumber   (long  value)
{
  _thru_tr = value;
  TURN_OFF_PP(PP_THRU_TR);
}


void   PpCard::setNumGroupsOnCard   (long  value)
{
  _ngroups = MaximumValue(value, 1);
  TURN_OFF_PP(PP_NGROUPS);
}


void   PpCard::setNumTracesOnCard   (long  value)
{
  _ntraces = MaximumValue(value, 0);
  TURN_OFF_PP(PP_NTRACES);
}


long   PpCard::updateThruGroupNumber (long  prev_thru_gr)
{
  _thru_gr = prev_thru_gr + _ngroups;
  return _thru_gr;
}


long   PpCard::updateThruTraceNumber (long  prev_thru_tr)
{
  _thru_tr = prev_thru_tr + _ntraces;
  return _thru_tr;
}



//------------------- get source skids -----------------------//
//------------------- get source skids -----------------------//
//------------------- get source skids -----------------------//

    // returns nils if group number is out of range.

void PpCard::getSourceSkids(long group, float *inline_skid,
                                        float *crossline_skid)  const
{
  if(_xskid == 0.0 && _yskid == 0.0)
      {                       // this test is for speedup only.
      *inline_skid    = 0.0;
      *crossline_skid = 0.0;
      return;
      }
  *inline_skid    = FNIL;
  *crossline_skid = FNIL;
  if(_thru_gr == INIL || _ngroups == INIL) return;
  long first_group = _thru_gr - _ngroups + 1;
  if(group < first_group || group > _thru_gr) return;
  long ixg = group - first_group;
  if(_hold > ixg || _hold >= 999)
      {
      *inline_skid    = _xskid;
      *crossline_skid = _yskid;
      }
  else
      {
      *inline_skid    = 0.0;
      *crossline_skid = 0.0;
      }
}



void PpCard::getSourceSkidsQuickly(long ixg, float *inline_skid,
                                             float *crossline_skid)  const
{
  if(ixg == -1)
      {
      *inline_skid    = FNIL;
      *crossline_skid = FNIL;
      }
  else if(_hold > ixg || _hold >= 999)
      {
      *inline_skid    = _xskid;
      *crossline_skid = _yskid;
      }
  else
      {
      *inline_skid    = 0.0;
      *crossline_skid = 0.0;
      }
}



//------------------ get group and channel -------------------//
//------------------ get group and channel -------------------//
//------------------ get group and channel -------------------//

    // returns zeroes if trace number is out of range.

void PpCard::getGroupAndChannel(long trace, long *group, long *channel)
                  const
{
  *group   = 0;
  *channel = 0;
  if(_ngroups == 0) return;
  if(_ntraces == 0) return;
  long first_group = _thru_gr - _ngroups + 1;
  long first_trace = _thru_tr - _ntraces + 1;
  if(trace < first_trace || trace > _thru_tr) return;
  long num_channels = _ntraces / _ngroups;
  assert(_ntraces == _ngroups * num_channels);
  *group = first_group + (trace - first_trace) / num_channels;
  *channel = (trace - first_trace + 1)
                      - (*group - first_group) * num_channels;
}



//---------------- set num traces on card -------------------//
//---------------- set num traces on card -------------------//
//---------------- set num traces on card -------------------//

       // public.

void PpCard::setNumTracesOnCard(FgConnect *connect)
{
  long num_channels = connect->numChannelsInPattern(_pattern);
  _ntraces          = num_channels * _ngroups;
  TURN_ON_PP(PP_NTRACES);
}



//-------------------------- end -------------------------------//
//-------------------------- end -------------------------------//
//-------------------------- end -------------------------------//

