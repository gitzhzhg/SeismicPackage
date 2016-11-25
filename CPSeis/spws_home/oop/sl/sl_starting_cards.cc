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

//---------------------- sl_starting_cards.cc -----------------------//
//---------------------- sl_starting_cards.cc -----------------------//
//---------------------- sl_starting_cards.cc -----------------------//

//       implementation file for the SLStartingCards class
//                derived from the SLDatabox class
//                        subdirectory sl


#include "sl/sl_starting_cards.hh"
#include "sl/sl_prim.hh"
#include "oprim/file_support_interface.hh"
#include "named_constants.h"
#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <assert.h>


    // This class understands that the template is relevent only for
    // ascii encoding and for input files.  This class calls only the
    // following functions in FileSupportInterface:
    //           _interface->setTemplate()
    //           _interface->getTemplate()
    //           _interface->getEncoding()
    //           _interface->isInput()
    //           _interface->getDataCard(index)
    //           _interface->numDataCards
    //           _cards->getStartingCard(index)
    //           _cards->numStartingCards



enum { TITLE = 1, PROMPT, CARD, DEBUG };
/******
enum { TITLE = 1, PROMPT, CARD, DEBUG, FILTER };
******/


#define TABLE      SLStartingCards      *table     = (SLStartingCards*)data;
#define INTERFACE  FileSupportInterface *interface = table->getInterface();
#define CARDS      FileSupportInterface *cards     = table->getCards();


//---------------------- constructor -----------------------//
//---------------------- constructor -----------------------//
//---------------------- constructor -----------------------//


SLStartingCards::SLStartingCards (SLDelay *slparent,
                                  FileSupportInterface *interface,
                                  FileSupportInterface *cards,
                                  int nrows_init)
      : SLDatabox(slparent, "sl_starting_cards", NULL, 4, TRUE, nrows_init),
                _interface   (interface),
                _cards       (cards),
/*******
                _debug       (FALSE),
                _filter      (FALSE)
*******/
                _debug       (FALSE)
{
  assert(_interface && _cards);
}



//------------------------ destructor ---------------------------------//
//------------------------ destructor ---------------------------------//
//------------------------ destructor ---------------------------------//

SLStartingCards::~SLStartingCards()
{
}



//---------------------- static strings ------------------------------//
//---------------------- static strings ------------------------------//
//---------------------- static strings ------------------------------//


static const char input_title[] =
"                               First Part of Input File        ";

static const char output_title[] =
"        First Part of Existing Output File  (which might be overwritten)";

static const char debug_title[] =
"                      Parameter Data Cards  (for debugging)";

/********
static const char filter_title[] =
"         First Part of Input File  (filtered with MAXCHARS and TEMPLATE)";
********/

static const char template_prompt[] =
"Type template for reading foreign file if necessary \
(press Help for instructions):";

//       1         2         3         4
//34567890123456789012345678901234567890
static const char blank_string[] =
"                                        \
                                        ";


//------------------------- traps --------------------------------------//
//------------------------- traps --------------------------------------//
//------------------------- traps --------------------------------------//


static void template_trap(void *data, long /*ident*/, long /*index*/,
                          char *value, long nread, char* /*endkey*/)
{
  if(nread == 0) return;
  TABLE
  INTERFACE
  interface->setTemplate(value);
}



static void debug_trap(void *data, long /*ident*/, long /*index*/,
                       long value, long nread, char* /*endkey*/)
{
  if(nread == 0) return;
  TABLE
  table->setDebug(value);
}



/********
static void filter_trap(void *data, long / *ident* /, long / *index* /,
                        long value, long nread, char* / *endkey* /)
{
  if(nread == 0) return;
  TABLE
  table->setFilter(value);
}
********/



//------------------------- update functions ---------------------------//
//------------------------- update functions ---------------------------//
//------------------------- update functions ---------------------------//


static char *template_update(void *data, long /*ident*/, long /*index*/)
{
  TABLE
  INTERFACE
  return (char*)interface->getTemplate();
}



static long debug_update(void *data, long /*ident*/, long /*index*/)
{
  TABLE
  return table->isDebug();
}



/********
static long filter_update(void *data, long / *ident* /, long / *index* /)
{
  TABLE
  INTERFACE
  if(interface->getWrap() > 1) return 0;
  return table->isFilter();
}
********/



static char *title_update(void *data, long /*ident*/, long /*index*/)
{
  TABLE
  INTERFACE
  if(table->isDebug()) return (char*)debug_title;
  if(interface->isOutput()) return (char*)output_title;
/********
  if(table->isFilter()) return (char*)filter_title;
********/
  return (char*)input_title;
}



static char *card_update(void *data, long /*ident*/, long index)
{
  TABLE
  CARDS
  INTERFACE
  if(table->isDebug ()) return (char*)interface->getDataCard    (index);
/********
  if(table->isFilter()) return (char*)interface->getFilteredCard(index);
********/
  return (char*)cards->getStartingCard((int)index);
}



static long n_update(void *data)
{
  TABLE
  CARDS
  INTERFACE
  if(table->isDebug()) return interface->numDataCards();
  return cards->numStartingCards();
}



//-------------------- switch update functions ----------------------------//
//-------------------- switch update functions ----------------------------//
//-------------------- switch update functions ----------------------------//


static long debug_switch(void *data, long ident, long /*index*/)
{
  TABLE
  if(!table->isVisible(CARD)) return -77;
  if(ident < 0) return 0;
  return 3;
}



/********
static long filter_switch(void *data, long ident, long / *index* /)
{
  TABLE
  INTERFACE
  if(!table->isVisible(CARD)) return -77;
  if(ident < 0) return 0;
  if(table->isDebug()) return -3;
  if(interface->getWrap() > 1) return -3;
  return 3;
}
********/



static long template_switch(void *data, long /*ident*/, long /*index*/)
{
  TABLE
  INTERFACE
  if(table->isDebug()) return -22;
  if(strcmp(interface->getEncoding(), "ascii") == 0) return 1;
  return -22;
}



//---------------------- make helper ------------------------//
//---------------------- make helper ------------------------//
//---------------------- make helper ------------------------//


void SLStartingCards::makeHelper()
{
  static long zero  =   0; 
  static long one   =   1; 
  static long three =   3; 
  static long m44   = -44; 

    //      ID    PROMPT  SWITCH  SWITCH  ROW COL NCHAR NDEC
  regIvar3(DEBUG, "debug", &zero, &three,  1,  82,  2);

  if(_interface->isInput())
    {
/********
    //        ID      PROMPT  SWITCH  SWITCH  ROW COL NCHAR NDEC
    regIvar3(FILTER, "filter", &zero, &three,  1,  1,  2);
********/

    //         ID     STRING           SWITCH  ROW COL  NCHAR NDEC
    regString(TITLE , input_title    , &m44 ,   1,  1);
    regString(PROMPT, template_prompt, &zero,   0,  0);

    //           N       NMAX    ROW COL NCHAR MAXROWS
    regArrays(n_update, n_update, 0,  0,   3,    35);

    //         ID    PROMPT        SWITCH   SWITCH  COL NCHAR NDEC
    regCarray(CARD , blank_string , &one,    &zero,  0,  80);

    funCvar(TITLE  ,          NULL,    title_update,            NULL);
    funCvar(-CARD  , template_trap, template_update, template_switch);
/********
    funIvar( FILTER,   filter_trap,   filter_update,   filter_switch);
    funCvar(-FILTER,          NULL,            NULL,   filter_switch);
********/
    }

  else
    {
    //           N         NMAX    ROW COL NCHAR MAXROWS
    regArrays(n_update, n_update,   1,  1,   3,    35);

    //         ID     PROMPT     SWITCH  SWITCH  COL NCHAR NDEC
    regCarray(CARD, output_title,  &m44,  &zero,   0,   80);

    funCvar(-CARD, NULL, title_update, NULL);
    }

  funCvar( CARD  ,        NULL,   card_update);
  funIvar( DEBUG ,  debug_trap,  debug_update,  debug_switch);
  funCvar(-DEBUG ,        NULL,          NULL,  debug_switch);
}



//-------------------------- end -------------------------------//
//-------------------------- end -------------------------------//
//-------------------------- end -------------------------------//
