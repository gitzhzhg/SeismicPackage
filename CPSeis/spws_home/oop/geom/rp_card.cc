
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
//--------------------- rp_card.cc ---------------------//
//--------------------- rp_card.cc ---------------------//
//--------------------- rp_card.cc ---------------------//

//         implementation file for the RpCard class
//                  not derived from any class
//                      subdirectory geom



#include "geom/rp_card.hh"
#include "geom/fg_constants.hh"
#include "cprim.h"
#include "named_constants.h"
#include <iostream.h>
#include <math.h>
#include <stdlib.h>
#include <string.h>
#include <assert.h>
#include <limits.h>


static const long  default_pattern     = 0;
static const long  default_nchannels   = 0;
static const long  default_cumchannels = 0;
static const int   default_rpflag      = RP_FLAG_X;
static const float default_shot        = 0.0;
static const long  default_line        = 0;
static const long  default_nx          = 1;
static const long  default_xinc        = 1;
static const long  default_ny          = 1;
static const long  default_yinc        = 1;
static const float default_xskid       = 0.0;
static const float default_yskid       = 0.0;
static const float default_eskid       = 0.0;
static const long  default_ixl         = -1;
static const long  default_ixf         = -1;


//---------------------- constructor -----------------------//
//---------------------- constructor -----------------------//
//---------------------- constructor -----------------------//


RpCard::RpCard()
           :
          _pattern      (default_pattern),
          _nchannels    (default_nchannels),
          _cumchannels  (default_cumchannels),
          _rpflag       (default_rpflag),
          _shot         (default_shot),
          _line         (default_line),
          _nx           (default_nx),
          _xinc         (default_xinc),
          _ny           (default_ny),
          _yinc         (default_yinc),
          _xskid        (default_xskid),
          _yskid        (default_yskid),
          _eskid        (default_eskid),
          _ixl          (default_ixl),
          _ixf          (default_ixf)
{
}



//------------------------ destructor --------------------------//
//------------------------ destructor --------------------------//
//------------------------ destructor --------------------------//

RpCard::~RpCard()
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



//----------------- get rp channels on card ---------------------//
//----------------- get rp channels on card ---------------------//
//----------------- get rp channels on card ---------------------//

     // public.
     // returns first and last channel described by this card.

void RpCard::getRpChannelsOnCard
                (long *first_channel, long *last_channel)  const
{
  *last_channel  = _cumchannels;
  *first_channel = _cumchannels - _nx * _ny + 1;
}



//---------------------- get rp increments -----------------------//
//---------------------- get rp increments -----------------------//
//---------------------- get rp increments -----------------------//

     // public.
     // returns X and Y increments on RP card.
     // also returns actual increments for the specified channel.

     // asserts if the channel number is not on this card,
     // or if this card is a skip or dup card.

void RpCard::getRpIncrements (long channel,
                              long *xinc,  long *yinc,
                              long *incrx, long *incry)  const
{
  *xinc = _xinc;
  *yinc = _yinc;
  long chan_index = channel - 1 - (_cumchannels - _nx * _ny);
  assert(chan_index >= 0 && chan_index < _nx * _ny);
  assert(_nx > 0 && _ny > 0);
  switch(_rpflag)
      {
      case RP_FLAG_X: *incry = chan_index / _nx;
                      *incrx = chan_index - _nx * *incry;
                      break;
      case RP_FLAG_Y: *incrx = chan_index / _ny;
                      *incry = chan_index - _ny * *incrx;
                      break;
      default: assert(FALSE);
      }
      assert(*incrx >= 0 && *incrx < _nx);
      assert(*incry >= 0 && *incry < _ny);
}



//----------------------- set card values ------------------------//
//----------------------- set card values ------------------------//
//----------------------- set card values ------------------------//


void RpCard::setAllValues(RpCard *other)
{
  if(!other) return;
  _pattern     = other->_pattern;
  _nchannels   = other->_nchannels;
  _cumchannels = other->_cumchannels;
  _rpflag      = other->_rpflag;
  _shot        = other->_shot;
  _line        = other->_line;
  _nx          = other->_nx;
  _xinc        = other->_xinc;
  _ny          = other->_ny;
  _yinc        = other->_yinc;
  _xskid       = other->_xskid;
  _yskid       = other->_yskid;
  _eskid       = other->_eskid;
  _ixl         = other->_ixl;
  _ixf         = other->_ixf;
}



void RpCard::setRpPatternNumber   (long  value)
{
  if(value == INIL) value = default_pattern;
  _pattern = value;
}


void RpCard::setRpNumChannels   (long  value)
{
  if(value == INIL) value = default_nchannels;
  _nchannels = value;
}


void RpCard::setRpCumChannels   (long  value)
{
  if(value == INIL) value = default_cumchannels;
  _cumchannels = value;
}


void RpCard::setRpFlag          (int  value)
{
  if(value == INIL) value = default_rpflag;
  assert(value == RP_FLAG_X    ||
         value == RP_FLAG_Y    ||
         value == RP_FLAG_SKIP ||
         value == RP_FLAG_DUP  );
  _rpflag = value;
}


void RpCard::setRpShotpoint  (float value)
{
  if(value == FNIL) value = default_shot;
  _shot = value;
}


void RpCard::setRpLineNumber (long  value)
{
  if(value == INIL) value = default_line;
  _line = value;
}


void RpCard::setRpNumX     (long  value)
{
  if(value == INIL) value = default_nx;
  if(value <= 0) value = 1;
  _nx = value;
}


void RpCard::setRpXinc     (long  value)
{
  if(value == INIL) value = default_xinc;
  _xinc = value;
}


void RpCard::setRpNumY     (long  value)
{
  if(value == INIL) value = default_ny;
  if(value <= 0) value = 1;
  _ny = value;
}


void RpCard::setRpYinc     (long  value)
{
  if(value == INIL) value = default_yinc;
  _yinc = value;
}


void RpCard::setRpXskid    (float value)
{
  if(value == FNIL) value = default_xskid;
  _xskid = value;
}


void RpCard::setRpYskid    (float value)
{
  if(value == FNIL) value = default_yskid;
  _yskid = value;
}


void RpCard::setRpEskid    (float value)
{
  if(value == FNIL) value = default_eskid;
  _eskid = value;
}



//------------- update cumulative number of channels -------------//
//------------- update cumulative number of channels -------------//
//------------- update cumulative number of channels -------------//

    // public.
    // num = number of channels thru previous RP card of same pattern.
    // returns updated value of num.

long RpCard::updateRpCumChannels(long num)
{
  if(_rpflag == RP_FLAG_X || _rpflag == RP_FLAG_Y)
       {
       num += _nx * _ny;
       _cumchannels = num;
       }
  else
       {
       _cumchannels = 0;
       }
  return num;
}



//-------------------------- end -------------------------------//
//-------------------------- end -------------------------------//
//-------------------------- end -------------------------------//

