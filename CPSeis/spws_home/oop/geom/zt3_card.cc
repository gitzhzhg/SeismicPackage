
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
//--------------------- zt3_card.cc ---------------------//
//--------------------- zt3_card.cc ---------------------//
//--------------------- zt3_card.cc ---------------------//

//         implementation file for the Zt3Card class
//                  not derived from any class
//                      subdirectory geom



#include "geom/zt3_card.hh"
#include "geom/fg_constants.hh"
#include "cprim.h"
#include "named_constants.h"
#include <iostream.h>
#include <stdlib.h>
#include <string.h>
#include <assert.h>
#include <limits.h>


static const int   default_code        = ZT_CODE_ZERO;
static const long  default_from_group  = 0;
static const long  default_to_group    = 0;
static const long  default_from_trace  = 0;
static const long  default_to_trace    = 0;


//---------------------- constructor -----------------------//
//---------------------- constructor -----------------------//
//---------------------- constructor -----------------------//


Zt3Card::Zt3Card()
           :
          _code         (default_code),
          _from_group   (default_from_group),
          _to_group     (default_to_group),
          _from_trace   (default_from_trace),
          _to_trace     (default_to_trace)
{
}



//------------------------ destructor --------------------------//
//------------------------ destructor --------------------------//
//------------------------ destructor --------------------------//

Zt3Card::~Zt3Card()
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


//--------------------- get matching code -----------------------//
//--------------------- get matching code -----------------------//
//--------------------- get matching code -----------------------//

       // public.
       // returns code if arguments are within range.
       // otherwise returns ZT_CODE_LIVE.

int Zt3Card::getMatchingZt3Code(long group, long channel)  const
{
  if( (group   < _from_group || group   > _to_group) &&
      (group   > _from_group || group   < _to_group) ) return ZT_CODE_LIVE;
  if( (channel < _from_trace || channel > _to_trace) &&
      (channel > _from_trace || channel < _to_trace) ) return ZT_CODE_LIVE;
  return _code;
/*
  if(group   < _from_group || group   > _to_group) return ZT_CODE_LIVE;
  if(channel < _from_trace || channel > _to_trace) return ZT_CODE_LIVE;
  return _code;
*/
}



//---------------------- group partly dead ------------------------//
//---------------------- group partly dead ------------------------//
//---------------------- group partly dead ------------------------//

     // public.
     // returns TRUE if this group is mentioned on the ZT3 cards.
     // returns FALSE otherwise.

int Zt3Card::groupPartlyDead(long group)  const
{
  if(group   >= _from_group && group   <= _to_group) return TRUE;
  if(group   <= _from_group && group   >= _to_group) return TRUE;
  return FALSE;
}



//----------------------- set card values ------------------------//
//----------------------- set card values ------------------------//
//----------------------- set card values ------------------------//


void Zt3Card::setAllValues(Zt3Card *other)
{
  if(!other) return;
  _code        = other->_code;
  _from_group  = other->_from_group;
  _to_group    = other->_to_group;
  _from_trace  = other->_from_trace;
  _to_trace    = other->_to_trace;
}



void Zt3Card::setZt3Code   (int  value)
{
  if(value == INIL) value = default_code;
  _code = value;
}


void Zt3Card::setZt3FromGroupNumber   (long  value)
{
  if(value == INIL) value = default_from_group;
  _from_group = value;
}


void Zt3Card::setZt3ToGroupNumber   (long  value)
{
  if(value == INIL) value = default_to_group;
  _to_group = value;
}


void Zt3Card::setZt3FromTraceNumber   (long  value)
{
  if(value == INIL) value = default_from_trace;
  _from_trace = value;
}


void Zt3Card::setZt3ToTraceNumber   (long  value)
{
  if(value == INIL) value = default_to_trace;
  _to_trace = value;
}



//-------------------------- end -------------------------------//
//-------------------------- end -------------------------------//
//-------------------------- end -------------------------------//

