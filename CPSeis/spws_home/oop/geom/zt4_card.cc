
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
//--------------------- zt4_card.cc ---------------------//
//--------------------- zt4_card.cc ---------------------//
//--------------------- zt4_card.cc ---------------------//

//         implementation file for the Zt4Card class
//                  not derived from any class
//                      subdirectory geom



#include "geom/zt4_card.hh"
#include "geom/fg_constants.hh"
#include "cprim.h"
#include "named_constants.h"
#include <iostream.h>
#include <stdlib.h>
#include <string.h>
#include <assert.h>
#include <limits.h>


static const int   default_code        = ZT_CODE_ZERO;
static const float default_from_sshot  = 0.0;
static const float default_to_sshot    = 0.0;
static const long  default_sline       = 0;
static const float default_from_rshot  = 0.0;
static const float default_to_rshot    = 0.0;
static const long  default_rline       = 0;


//---------------------- constructor -----------------------//
//---------------------- constructor -----------------------//
//---------------------- constructor -----------------------//


Zt4Card::Zt4Card()
           :
          _code         (default_code),
          _from_sshot   (default_from_sshot),
          _to_sshot     (default_to_sshot),
          _sline        (default_sline),
          _from_rshot   (default_from_rshot),
          _to_rshot     (default_to_rshot),
          _rline        (default_rline)
{
}



//------------------------ destructor --------------------------//
//------------------------ destructor --------------------------//
//------------------------ destructor --------------------------//

Zt4Card::~Zt4Card()
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

int Zt4Card::getMatchingZt4Code(long source_line, float source_shot,
                                long    rec_line, float    rec_shot)  const
{
  if(source_line != _sline) return ZT_CODE_LIVE;
  if(   rec_line != _rline) return ZT_CODE_LIVE;
/*
  if(source_shot < _from_sshot || source_shot > _to_sshot) return ZT_CODE_LIVE;
  if(   rec_shot < _from_rshot ||    rec_shot > _to_rshot) return ZT_CODE_LIVE;
*/
  if( (source_shot < _from_sshot || source_shot > _to_sshot) &&
      (source_shot > _from_sshot || source_shot < _to_sshot) )
                                                    return ZT_CODE_LIVE;
  if( (   rec_shot < _from_rshot ||    rec_shot > _to_rshot) &&
      (   rec_shot > _from_rshot ||    rec_shot < _to_rshot) )
                                                    return ZT_CODE_LIVE;
  return _code;
}



//------------------ source or receiver maybe dead ----------------//
//------------------ source or receiver maybe dead ----------------//
//------------------ source or receiver maybe dead ----------------//

        // public.
        // returns TRUE or FALSE.

int Zt4Card::sourceMaybeDead(long source_line, float source_shot)  const
{
  if(source_line != _sline) return FALSE;
/*
  if(source_shot < _from_sshot || source_shot > _to_sshot) return FALSE;
  return TRUE;
*/
  if(source_shot >= _from_sshot && source_shot <= _to_sshot) return TRUE;
  if(source_shot <= _from_sshot && source_shot >= _to_sshot) return TRUE;
  return FALSE;
}


int Zt4Card::receiverMaybeDead(long rec_line, float rec_shot)  const
{
  if(rec_line != _rline) return FALSE;
/*
  if(rec_shot < _from_rshot || rec_shot > _to_rshot) return FALSE;
  return TRUE;
*/
  if(rec_shot >= _from_rshot && rec_shot <= _to_rshot) return TRUE;
  if(rec_shot <= _from_rshot && rec_shot >= _to_rshot) return TRUE;
  return FALSE;
}



//----------------------- set card values ------------------------//
//----------------------- set card values ------------------------//
//----------------------- set card values ------------------------//


void Zt4Card::setAllValues(Zt4Card *other)
{
  if(!other) return;
  _code        = other->_code;
  _from_sshot  = other->_from_sshot;
  _to_sshot    = other->_to_sshot;
  _sline       = other->_sline;
  _from_rshot  = other->_from_rshot;
  _to_rshot    = other->_to_rshot;
  _rline       = other->_rline;
}



void Zt4Card::setZt4Code   (int  value)
{
  if(value == INIL) value = default_code;
  _code = value;
}


void Zt4Card::setZt4FromSourceShotpoint   (float  value)
{
  if(value == FNIL) value = default_from_sshot;
  _from_sshot = value;
}


void Zt4Card::setZt4ToSourceShotpoint   (float  value)
{
  if(value == FNIL) value = default_to_sshot;
  _to_sshot = value;
}


void Zt4Card::setZt4SourceLineNumber (long  value)
{
  if(value == INIL) value = default_sline;
  _sline = value;
}


void Zt4Card::setZt4FromReceiverShotpoint   (float  value)
{
  if(value == FNIL) value = default_from_rshot;
  _from_rshot = value;
}


void Zt4Card::setZt4ToReceiverShotpoint   (float  value)
{
  if(value == FNIL) value = default_to_rshot;
  _to_rshot = value;
}


void Zt4Card::setZt4ReceiverLineNumber (long  value)
{
  if(value == INIL) value = default_rline;
  _rline = value;
}



//-------------------------- end -------------------------------//
//-------------------------- end -------------------------------//
//-------------------------- end -------------------------------//

