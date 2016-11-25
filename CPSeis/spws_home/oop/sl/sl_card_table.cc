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

//---------------------- sl_card_table.cc -----------------------//
//---------------------- sl_card_table.cc -----------------------//
//---------------------- sl_card_table.cc -----------------------//

//          implementation file for the SLCardTable class
//                derived from the SLDatabox class
//                         subdirectory sl


#include "sl/sl_card_table.hh"
#include "sl/sl_prim.hh"
#include "oprim/history_cards.hh"
#include "oprim/generic_cards.hh"
#include "oprim/generic_decode.hh"
#include "cprim.h"
#include <iostream.h>
#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <assert.h>


#define TABLE  SLCardTable   *table  = (SLCardTable*)data;
#define HIST   HistoryCards  *hist   = table->hist();
#define CARDS  GenericCards  *cards  = table->cards();
#define DECODE GenericDecode *decode = table->decode();

enum { BAD = 1, HOW, CRD, ALLOW };


//---------------------- constructor -----------------------//
//---------------------- constructor -----------------------//
//---------------------- constructor -----------------------//


SLCardTable::SLCardTable(SLDelay *slparent, char *name,
                         char *title, char *message,
                         GenericCards  *cards,
                         GenericDecode *decode, int ncol, int omit)
           : SLDatabox(slparent, name, NULL, 4, omit),
                     _title        (newstr(title)),
                     _message      (newstr(message)),
                     _hist         (NULL),
                     _cards        (cards),
                     _decode       (decode),
                     _ncol         (ncol)
{
  assert(_title && _message && _cards);
}



SLCardTable::SLCardTable(SLDelay *slparent, char *name,
                         char *title,
                         HistoryCards  *hist, int ncol, int omit)
           : SLDatabox(slparent, name, NULL, 4, omit),
                     _title        (newstr(title)),
                     _message      (newstr(" ")),
                     _hist         (hist),
                     _cards        (NULL),
                     _decode       (NULL),
                     _ncol         (ncol)
{
  assert(_title && _hist);
}



//------------------------ destructor --------------------------//
//------------------------ destructor --------------------------//
//------------------------ destructor --------------------------//

SLCardTable::~SLCardTable()
{
  free(_title);
  free(_message);
}



//------------------------ scroll to end --------------------------//
//------------------------ scroll to end --------------------------//
//------------------------ scroll to end --------------------------//

      // public.

void SLCardTable::scrollToEnd()
{
  long index;
  if(_cards) index = _cards->numCards();
  else       index = _hist->numHistoryCards();
  setFocus(CRD, (int)index);
}



//---------------- variable traps -------------------------------//
//---------------- variable traps -------------------------------//
//---------------- variable traps -------------------------------//


static void how_trap(void *data, long /*ident*/, long /*index*/,
                     char *value, long nread, char* /*endkey*/)
{
  if(nread == 0) return;
  TABLE
  DECODE
  decode->howDecode(value);
}



//---------------- variable update functions --------------------//
//---------------- variable update functions --------------------//
//---------------- variable update functions --------------------//


static char *how_update(void *data, long /*ident*/, long /*index*/)
{
  TABLE
  DECODE
  return (char*)decode->howDecode();
}


static char *allow_update(void *data, long /*ident*/, long /*index*/)
{
  TABLE
  DECODE
  return (char*)decode->allowedCodes();
}


static char *bad_update(void *data, long /*ident*/, long /*index*/)
{
  TABLE
  DECODE
  return (char*)decode->badCodeInfo();
}


static char *card_update(void *data, long /*ident*/, long index)
{
  TABLE
  CARDS
  HIST
  if(cards) return (char*)cards->getCard(index);
  return (char*)hist->getHistoryCard(index);
}


static long n_update(void *data)
{
  TABLE
  CARDS
  HIST
  if(cards) return cards->numCards();
  return hist->numHistoryCards();
}



//---------------------- make helper ------------------------//
//---------------------- make helper ------------------------//
//---------------------- make helper ------------------------//

void SLCardTable::makeHelper()
{
  static long zero =   0; 
  static long one  =   1; 
  static long p12  =  12; 
  static long m44  = -44; 

  if(_decode)
    {
      //      ID     PROMPT                     SWITCH ROW    COL  NCHAR
    
      if (strcmp(_decode->requiredCodes(),"") != 0)
	regCvar2(ALLOW,"Required and allowed letters in format code:",
                                               &zero, &zero,   0,    1,   40);
      else
	regCvar2(ALLOW,"Allowed letters in format code:",
                                                &zero, &zero,  0,    1,   40);
      
    //regCvar (BAD  ,                           &zero, -1, _ncol-38 ,40 );
      regCvar (BAD  ,                           &zero, -1, _ncol-28 ,30 );
      regMsg  (      _message,                          0,      1       );
      regCvar (HOW  ,                           &one ,  0,      5   , _ncol);
    }

    //         N       NMAX    ROW COL NCHAR MAXROWS
  regArrays(n_update, n_update, 0,  0,   3,    35);

    //       ID      PROMPT           SWITCH   SWITCH  COL  NCHAR NDEC
  regCarray(CRD,    _title  ,          &m44  ,  &p12 ,  0, _ncol);

  updateCvar (ALLOW,  allow_update);
  updateCvar (BAD  ,    bad_update);
  trapCvar   (HOW  ,    how_trap);
  updateCvar (HOW  ,    how_update);
  updateCvar (CRD  ,   card_update);
}



//-------------------------- end -------------------------------//
//-------------------------- end -------------------------------//
//-------------------------- end -------------------------------//
