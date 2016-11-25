
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
//---------------------- acc_nchan.cc -----------------------//
//---------------------- acc_nchan.cc -----------------------//
//---------------------- acc_nchan.cc -----------------------//

//         implementation file for the AccNchan class
//                derived from the AccBase class
//                     subdirectory geom


#include "geom/acc_nchan.hh"
#include "geom/rp_cards.hh"
#include "geom/rp_card.hh"
#include "geom/fg_connect.hh"
#include "geom/fg_constants.hh"
#include "cprim.h"
#include <iostream.h>
#include <stdlib.h>
#include <string.h>
#include <assert.h>



//---------------------- constructor -----------------------//
//---------------------- constructor -----------------------//
//---------------------- constructor -----------------------//


AccNchan::AccNchan(RpCards *rp_cards, FgConnect *connect)
           : AccBase(rp_cards, RP_NCHAN),
                _rp_cards              (rp_cards),
                _connect               (connect)
{
  assert(_rp_cards);
  assert(_connect);
}



//------------------------ destructor --------------------------//
//------------------------ destructor --------------------------//
//------------------------ destructor --------------------------//

AccNchan::~AccNchan()
{
}



//---------------- private set num channels ------------------------//
//---------------- private set num channels ------------------------//
//---------------- private set num channels ------------------------//

     // private.
     // given index ixrp to start at, returns index of start of next
     //   receiver pattern which does not match the pattern at index ixrp.
     // sets the number of channels on all cards starting at ixrp
     //   and ending on the last card with the same pattern number as
     //   that on card with index ixrp.

long AccNchan::privateSetNumChannels(long ixrp)  const
{
  long pattern = _rp_cards->getRpPatternNumber(ixrp);
  long keep = ixrp;
  long num = 0;
  long n = _rp_cards->numElements();
  while(ixrp < n && _rp_cards->getRpPatternNumber(ixrp) == pattern)
      {
      RpCard *rpcard = _rp_cards->rpCard(ixrp);
      num = rpcard->updateRpCumChannels(num);
      ixrp++;
      }
  for(long index = keep; index < ixrp; index++)
      {
      RpCard *rpcard = _rp_cards->rpCard(index);
      rpcard->setRpNumChannels(num);
      }
  return ixrp;
}



//-------------------- get range ---------------------------------//
//-------------------- get range ---------------------------------//
//-------------------- get range ---------------------------------//

        // private virtual function overriding AccBase

void AccNchan::getRange(long n, long *index1, long *index2)
{
  if(n == 0) return;
  long i1 = *index1;
  long i2 = *index2;
  if(i1 >   0) i1--;
  if(i2 < n-1) i2++;
  long pattern1 = _rp_cards->getRpPatternNumber(i1);
  long pattern2 = _rp_cards->getRpPatternNumber(i2);
  while(i1 >   0 && _rp_cards->getRpPatternNumber(i1-1) == pattern1) { i1--; }
  while(i2 < n-1 && _rp_cards->getRpPatternNumber(i2+1) == pattern2) { i2++; }
  *index1 = i1;
  *index2 = i2;
}



//-------------------- fix range ---------------------------------//
//-------------------- fix range ---------------------------------//
//-------------------- fix range ---------------------------------//

        // private virtual function overriding AccBase

void AccNchan::fixRange(long /*n*/, long index1, long index2)
{
  long ixrp = index1;
  while(ixrp <= index2)
      {
      ixrp = privateSetNumChannels(ixrp);
      }
}



//-------------------------- end -------------------------------//
//-------------------------- end -------------------------------//
//-------------------------- end -------------------------------//

