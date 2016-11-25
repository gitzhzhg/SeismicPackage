
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
//---------------------- fg_connect.cc -----------------------//
//---------------------- fg_connect.cc -----------------------//
//---------------------- fg_connect.cc -----------------------//

//         implementation file for the FgConnect class
//                 not derived from any class
//                     subdirectory geom


#include "geom/fg_connect.hh"
#include "geom/fg_constants.hh"
#include "geom/seis_survey.hh"
#include "geom/rp_cards.hh"
#include "geom/pp_cards.hh"
#include "geom/zt_cards.hh"
#include "cprim.h"
#include <iostream.h>
#include <stdlib.h>
#include <string.h>
#include <math.h>
#include <assert.h>



//---------------------- constructor -----------------------//
//---------------------- constructor -----------------------//
//---------------------- constructor -----------------------//


FgConnect::FgConnect(FieldGeometry *fg)
        :
             _fg            (fg),
             _survey        (NULL),
             _rp_cards      (NULL),
             _pp_cards      (NULL),
             _zt_cards      (NULL)
{
}



//------------------------ destructor --------------------------//
//------------------------ destructor --------------------------//
//------------------------ destructor --------------------------//

FgConnect::~FgConnect()
{
}



//------------------ num channels in pattern -----------------//
//------------------ num channels in pattern -----------------//
//------------------ num channels in pattern -----------------//

    // public
    // pass-thru to RpCards.
    // called from PpCards.

long FgConnect::numChannelsInPattern(long pattern)  const
{
  assert(_rp_cards);
  return _rp_cards->numChannelsInPattern(pattern);
}



//------------ get dead source or receiver code ------------------//
//------------ get dead source or receiver code ------------------//
//------------ get dead source or receiver code ------------------//

    // public
    // pass-thru to ZtCards.
    // called from SeisSurvey.

int FgConnect::getDeadSourceCode(long line_number, float shotpoint)  const
{
  assert(_zt_cards);
  return _zt_cards->getDeadSourceCode(line_number, shotpoint);
}


int FgConnect::getDeadReceiverCode(long line_number, float shotpoint)  const
{
  assert(_zt_cards);
  return _zt_cards->getDeadReceiverCode(line_number, shotpoint);
}


int FgConnect::sourceMaybeDead(long line_number, float shotpoint)  const
{
  assert(_zt_cards);
  return _zt_cards->sourceMaybeDead(line_number, shotpoint);
}


int FgConnect::receiverMaybeDead(long line_number, float shotpoint)  const
{
  assert(_zt_cards);
  return _zt_cards->receiverMaybeDead(line_number, shotpoint);
}



//--------------- receiver patterns have changed -----------------//
//--------------- receiver patterns have changed -----------------//
//--------------- receiver patterns have changed -----------------//

    // public.
    // pass-thru to PpCards.
    // called from RpCards.

void FgConnect::receiverPatternsHaveChanged()
{
  assert(_pp_cards);
  _pp_cards->receiverPatternsHaveChanged();
}



//-------------------- ZT cards have changed ----------------------//
//-------------------- ZT cards have changed ----------------------//
//-------------------- ZT cards have changed ----------------------//

    // public.
    // pass-thru to SeisSurvey.
    // called from ZT cards object(s).

void FgConnect::zt1CardsHaveChanged()
{
  _survey->clearDeadSourceCodes();
}


void FgConnect::zt2CardsHaveChanged()
{
  _survey->clearDeadReceiverCodes();
}


void FgConnect::zt3CardsHaveChanged()
{
}


void FgConnect::zt4CardsHaveChanged()
{
}



//------------------ set dead source codes --------------------//
//------------------ set dead source codes --------------------//
//------------------ set dead source codes --------------------//

      // public.
      // uses information in ZtCards to set the dead source
      //   codes onto all flags in SeisSurvey.

void FgConnect::setDeadSourceCodes()
{
  assert(_survey && _zt_cards);
  long nlines = _survey->numLines();
  for(long ixl = 0; ixl < nlines; ixl++)
      {
      long nflags = _survey->numFlagsOnLine(ixl);
      for(long ixf = 0; ixf < nflags; ixf++)
          {
             ///// to do
             ///// to do
             ///// to do
             ///// to do
             ///// to do
             ///// to do
             ///// to do
             ///// to do
          }
      }
}



//-------------------------- end -------------------------------//
//-------------------------- end -------------------------------//
//-------------------------- end -------------------------------//

