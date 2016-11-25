
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
//--------------------- zt1_card.cc ---------------------//
//--------------------- zt1_card.cc ---------------------//
//--------------------- zt1_card.cc ---------------------//

//         implementation file for the Zt1Card class
//                  not derived from any class
//                      subdirectory geom



#include "geom/zt1_card.hh"
#include "geom/fg_constants.hh"
#include "cprim.h"
#include "named_constants.h"
#include <iostream.h>
#include <stdlib.h>
#include <string.h>
#include <assert.h>
#include <limits.h>


static const int   default_code        = ZT_CODE_ZERO;
static const float default_from_shot   = 0.0;
static const float default_to_shot     = 0.0;
static const long  default_line        = 0;


//---------------------- constructor -----------------------//
//---------------------- constructor -----------------------//
//---------------------- constructor -----------------------//


Zt1Card::Zt1Card()
           :
          _code         (default_code),
          _from_shot    (default_from_shot),
          _to_shot      (default_to_shot),
          _line         (default_line)
{
}



//------------------------ destructor --------------------------//
//------------------------ destructor --------------------------//
//------------------------ destructor --------------------------//

Zt1Card::~Zt1Card()
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

int Zt1Card::getMatchingZt1Code(long source_line, float source_shot)  const
{
  if(source_line != _line) return ZT_CODE_LIVE;
  if(source_shot >= _from_shot && source_shot <= _to_shot) return _code;
  if(source_shot <= _from_shot && source_shot >= _to_shot) return _code;
/*
  if(source_shot < _from_shot || source_shot > _to_shot) return ZT_CODE_LIVE;
  return _code;
*/
  return ZT_CODE_LIVE;
}



//----------------------- set card values ------------------------//
//----------------------- set card values ------------------------//
//----------------------- set card values ------------------------//


void Zt1Card::setAllValues(Zt1Card *other)
{
  if(!other) return;
  _code        = other->_code;
  _from_shot   = other->_from_shot;
  _to_shot     = other->_to_shot;
  _line        = other->_line;
}



void Zt1Card::setZt1Code   (int  value)
{
  if(value == INIL) value = default_code;
  _code = value;
}


void Zt1Card::setZt1FromSourceShotpoint   (float  value)
{
  if(value == FNIL) value = default_from_shot;
  _from_shot = value;
}


void Zt1Card::setZt1ToSourceShotpoint   (float  value)
{
  if(value == FNIL) value = default_to_shot;
  _to_shot = value;
}


void Zt1Card::setZt1SourceLineNumber (long  value)
{
  if(value == INIL) value = default_line;
  _line = value;
}



//-------------------------- end -------------------------------//
//-------------------------- end -------------------------------//
//-------------------------- end -------------------------------//

