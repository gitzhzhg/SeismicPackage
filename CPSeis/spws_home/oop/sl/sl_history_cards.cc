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

//---------------------- sl_history_cards.cc -----------------------//
//---------------------- sl_history_cards.cc -----------------------//
//---------------------- sl_history_cards.cc -----------------------//

//          implementation file for the SLHistoryCards class
//                 derived from the SLDatabox class
//                         subdirectory sl


#include "sl/sl_history_cards.hh"
#include "oprim/history_cards.hh"
#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <assert.h>


enum { IDENT = 1 };


//---------------------- constructor -----------------------//
//---------------------- constructor -----------------------//
//---------------------- constructor -----------------------//


SLHistoryCards::SLHistoryCards
                     (SLDelay *slparent, HistoryCards *history, int allow)
           : SLDatabox(slparent, "sl_history_cards", NULL, 4),
                 _history   (history),
                 _allow     (allow)
{
}



//------------------------ destructor --------------------------//
//------------------------ destructor --------------------------//
//------------------------ destructor --------------------------//

SLHistoryCards::~SLHistoryCards()
{
}



//------------------- show last history card --------------------//
//------------------- show last history card --------------------//
//------------------- show last history card --------------------//


void SLHistoryCards::showLastHistoryCard()
{
  if(_history) setFocus(IDENT, (int)_history->numHistoryCards());
}



//---------------- variable update functions --------------------//
//---------------- variable update functions --------------------//
//---------------- variable update functions --------------------//


static void card_trap(void *data, long /*ident*/, long index,
                      char *value, long nread, char* /*endkey*/)
{
  if(nread == 0) return;
  SLHistoryCards *table = (SLHistoryCards*)data;
  if(!table->getHistory()) return;
  table->getHistory()->addHistoryCard(value);
}

static char *card_update(void *data, long /*ident*/, long index)
{
  static const char buffer[] = " ";
  SLHistoryCards *table = (SLHistoryCards*)data;
  if(!table->getHistory()) return (char*)buffer;
  return (char*)table->getHistory()->getHistoryCard(index);
}


static long card_switch(void *data, long /*ident*/, long index)
{
  SLHistoryCards *table = (SLHistoryCards*)data;
  if(!table->getHistory()) return 0;
  if(!table->getAllow()) return 0;
  if (index == table->getHistory()->numHistoryCards()) return 1;
  return 0;
}


static long n_update(void *data)
{
  SLHistoryCards *table = (SLHistoryCards*)data;
  if(!table->getHistory()) return 0;
  return table->getHistory()->numHistoryCards();
}


static long nmax_update(void *data)
{
  SLHistoryCards *table = (SLHistoryCards*)data;
  if(!table->getHistory()) return 0;
  if(!table->getAllow()) return table->getHistory()->numHistoryCards();
  return table->getHistory()->numHistoryCards() + 1;
}



//---------------------- make helper ------------------------//
//---------------------- make helper ------------------------//
//---------------------- make helper ------------------------//

void SLHistoryCards::makeHelper()
{
  static long zero =   0; 
  static long m44  = -44; 

    //         N         NMAX     ROW COL NCHAR MAXROWS
  regArrays(n_update, nmax_update, 0,  0,   5,    35);

    //  ID          PROMPT         SWITCH   SWITCH  COL NCHAR NDEC
  regCarray(IDENT, "history cards", &m44  , &zero,   0,   80);

  funCvar (IDENT,  card_trap, card_update, card_switch);
}



//-------------------------- end -------------------------------//
//-------------------------- end -------------------------------//
//-------------------------- end -------------------------------//
