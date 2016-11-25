
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
//---------------------- acc_ntraces.cc -----------------------//
//---------------------- acc_ntraces.cc -----------------------//
//---------------------- acc_ntraces.cc -----------------------//

//         implementation file for the AccNtraces class
//                derived from the AccBase class
//                     subdirectory geom


#include "geom/acc_ntraces.hh"
#include "geom/pp_cards.hh"
#include "geom/pp_card.hh"
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


AccNtraces::AccNtraces(PpCards *pp_cards, FgConnect *connect)
           : AccBase(pp_cards, PP_NTRACES),
                _pp_cards              (pp_cards),
                _connect               (connect)
{
  assert(_pp_cards);
  assert(_connect);
}



//------------------------ destructor --------------------------//
//------------------------ destructor --------------------------//
//------------------------ destructor --------------------------//

AccNtraces::~AccNtraces()
{
}



//-------------------- fix range ---------------------------------//
//-------------------- fix range ---------------------------------//
//-------------------- fix range ---------------------------------//

                     // private

void AccNtraces::fixRange(long /*n*/, long index1, long index2)
{
  for(long ixpp = index1; ixpp <= index2; ixpp++)
      {
      PpCard *pp_card = _pp_cards->ppCard(ixpp);
      pp_card->setNumTracesOnCard(_connect);
      }
}



//-------------------------- end -------------------------------//
//-------------------------- end -------------------------------//
//-------------------------- end -------------------------------//

