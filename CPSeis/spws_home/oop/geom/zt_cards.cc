
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
//------------------------ zt_cards.cc ------------------------//
//------------------------ zt_cards.cc ------------------------//
//------------------------ zt_cards.cc ------------------------//

//          implementation file for the ZtCards class
//                 not derived from any class
//                      subdirectory geom


#include "geom/zt_cards.hh"
#include "geom/fg_constants.hh"
#include "geom/zt1_cards.hh"
#include "geom/zt2_cards.hh"
#include "geom/zt3_cards.hh"
#include "geom/zt4_cards.hh"
#include "geom/zt1_card.hh"
#include "geom/zt2_card.hh"
#include "geom/zt3_card.hh"
#include "geom/zt4_card.hh"
#include "cprim.h"
#include "named_constants.h"
#include <iostream.h>
#include <math.h>
#include <stdlib.h>
#include <string.h>
#include <assert.h>



//---------------------- constructor -----------------------//
//---------------------- constructor -----------------------//
//---------------------- constructor -----------------------//


ZtCards::ZtCards(FgInformer *informer, FgConnect *connect)
           :
                 _zt1_cards           (NULL),  // reset below
                 _zt2_cards           (NULL),  // reset below
                 _zt3_cards           (NULL),  // reset below
                 _zt4_cards           (NULL)   // reset below
{
  _zt1_cards = new Zt1Cards      (informer, connect);
  _zt2_cards = new Zt2Cards      (informer, connect);
  _zt3_cards = new Zt3Cards      (informer, connect);
  _zt4_cards = new Zt4Cards      (informer, connect);
}



//------------------------ destructor --------------------------//
//------------------------ destructor --------------------------//
//------------------------ destructor --------------------------//

ZtCards::~ZtCards()
{
  delete _zt4_cards;
  delete _zt3_cards;
  delete _zt2_cards;
  delete _zt1_cards;
}



//------------------- combine dead trace codes -------------------------//
//------------------- combine dead trace codes -------------------------//
//------------------- combine dead trace codes -------------------------//

  // public.
  // combines two dead trace codes into a new one.

int ZtCards::combineDeadTraceCodes(int code1, int code2)  const
{
  if(code1 == ZT_CODE_NONE || code2 == ZT_CODE_NONE) return ZT_CODE_NONE;
  if(code1 == ZT_CODE_MISS || code2 == ZT_CODE_MISS) return ZT_CODE_MISS;
  if(code1 == ZT_CODE_ZERO || code2 == ZT_CODE_ZERO) return ZT_CODE_ZERO;
  if(code1 == ZT_CODE_REV  || code2 == ZT_CODE_REV ) return ZT_CODE_REV ;
  assert(code1 == ZT_CODE_LIVE && code2 == ZT_CODE_LIVE);
  return ZT_CODE_LIVE;
}



//------------------ get dead trace code ---------------------------//
//------------------ get dead trace code ---------------------------//
//------------------ get dead trace code ---------------------------//

     // public.

int ZtCards::getDeadTraceCode(long       group, long      channel,
                              long source_line, float source_shot,
                              long    rec_line, float    rec_shot)  const
{
  int dead_source_code = getDeadSourceCode  (source_line, source_shot);
  int dead_rec_code    = getDeadReceiverCode(   rec_line,    rec_shot);
  return getDeadTraceCode(dead_source_code, dead_rec_code,
                                     FALSE,         FALSE,
                                     group,       channel,
                               source_line,   source_shot,
                                  rec_line,      rec_shot);
}



//------------------ get dead source code ---------------------------//
//------------------ get dead source code ---------------------------//
//------------------ get dead source code ---------------------------//

     // public.
     // get code from all ZT1 cards.

int ZtCards::getDeadSourceCode(long source_line, float source_shot)  const
{

  if(source_line == INIL) return ZT_CODE_NONE;
  if(source_shot == FNIL) return ZT_CODE_NONE;

  int dead_trace_code = ZT_CODE_LIVE;
  long           nzt1 = _zt1_cards->numZt1Cards();

  for(long i = 0; i < nzt1; i++)
      {
      int code = _zt1_cards->zt1Card(i)->
                     getMatchingZt1Code(source_line, source_shot);
      dead_trace_code = combineDeadTraceCodes(code, dead_trace_code);
      }
  return dead_trace_code;
}



//------------------ get dead receiver code ------------------------//
//------------------ get dead receiver code ------------------------//
//------------------ get dead receiver code ------------------------//

     // public.
     // get code from all ZT2 cards.

int ZtCards::getDeadReceiverCode(long rec_line, float rec_shot)  const
{

  if(rec_line == INIL) return ZT_CODE_NONE;
  if(rec_shot == FNIL) return ZT_CODE_NONE;

  int dead_trace_code = ZT_CODE_LIVE;
  long           nzt2 = _zt2_cards->numZt2Cards();

  for(long i = 0; i < nzt2; i++)
      {
      int code = _zt2_cards->zt2Card(i)->
                     getMatchingZt2Code(rec_line, rec_shot);
      dead_trace_code = combineDeadTraceCodes(code, dead_trace_code);
      }
  return dead_trace_code;
}



//------------------ get dead zt3 code ------------------------//
//------------------ get dead zt3 code ------------------------//
//------------------ get dead zt3 code ------------------------//

     // public.
     // get code from all ZT3 cards.

int ZtCards::getDeadZt3Code(long group, long channel)  const
{

  if(group   == INIL) return ZT_CODE_NONE;
  if(channel == INIL) return ZT_CODE_NONE;

  int dead_trace_code = ZT_CODE_LIVE;
  long           nzt3 = _zt3_cards->numZt3Cards();

  for(long i = 0; i < nzt3; i++)
      {
      int code = _zt3_cards->zt3Card(i)->
                     getMatchingZt3Code(group, channel);
      dead_trace_code = combineDeadTraceCodes(code, dead_trace_code);
      }
  return dead_trace_code;
}



//------------------ get dead zt4 code ------------------------//
//------------------ get dead zt4 code ------------------------//
//------------------ get dead zt4 code ------------------------//

     // public.
     // get code from all ZT4 cards.

int ZtCards::getDeadZt4Code(long source_line, float source_shot,
                            long    rec_line, float    rec_shot)  const
{
  if(source_line == INIL) return ZT_CODE_NONE;
  if(source_shot == FNIL) return ZT_CODE_NONE;
  if(rec_line    == INIL) return ZT_CODE_NONE;
  if(rec_shot    == FNIL) return ZT_CODE_NONE;

  int dead_trace_code = ZT_CODE_LIVE;
  long           nzt4 = _zt4_cards->numZt4Cards();

  for(long i = 0; i < nzt4; i++)
      {
      int code = _zt4_cards->zt4Card(i)->
                     getMatchingZt4Code(source_line, source_shot,
                                           rec_line,    rec_shot);
      dead_trace_code = combineDeadTraceCodes(code, dead_trace_code);
      }
  return dead_trace_code;
}



//------------------ get dead trace code ---------------------------//
//------------------ get dead trace code ---------------------------//
//------------------ get dead trace code ---------------------------//

     // public.
     // uses previously-determined dead source and receiver codes,
     //   plus additional conditional information, to obtain the
     //   dead trace code for an individual trace.

int ZtCards::getDeadTraceCode(int dead_source_code, int dead_rec_code,
                              int use_zt3_cards,    int use_zt4_cards,
                              long       group, long      channel,
                              long source_line, float source_shot,
                              long    rec_line, float    rec_shot)  const
{
  if(group            ==    0) return ZT_CODE_NONE;
  if(channel          ==    0) return ZT_CODE_NONE;
  if(source_line      == INIL) return ZT_CODE_NONE;
  if(source_shot      == FNIL) return ZT_CODE_NONE;
  if(rec_line         == INIL) return ZT_CODE_NONE;
  if(rec_shot         == FNIL) return ZT_CODE_NONE;

  int dead_trace_code = combineDeadTraceCodes
                                   (dead_source_code, dead_rec_code);
  if(use_zt3_cards)
      {
      int code = getDeadZt3Code(group, channel);
      dead_trace_code = combineDeadTraceCodes(code, dead_trace_code);
      }
  if(use_zt4_cards)
      {
      int code = getDeadZt4Code(source_line, source_shot,
                                   rec_line,    rec_shot);
      dead_trace_code = combineDeadTraceCodes(code, dead_trace_code);
      }
  return dead_trace_code;
}



//------------------ source maybe dead ------------------------------//
//------------------ source maybe dead ------------------------------//
//------------------ source maybe dead ------------------------------//

     // public.
     // return TRUE or FALSE.

int ZtCards::sourceMaybeDead(long source_line, float source_shot)  const
{

  if(source_line == INIL) return FALSE;
  if(source_shot == FNIL) return FALSE;

  long nzt4 = _zt4_cards->numZt4Cards();
  long i;

  for(i = 0; i < nzt4; i++)
      {
      int maybe = _zt4_cards->zt4Card(i)->
                     sourceMaybeDead(source_line, source_shot);
      if(maybe) return TRUE;
      }
  return FALSE;
}



//------------------ receiver maybe dead ------------------------------//
//------------------ receiver maybe dead ------------------------------//
//------------------ receiver maybe dead ------------------------------//

     // public.
     // return TRUE or FALSE.

int ZtCards::receiverMaybeDead(long rec_line, float rec_shot)  const
{

  if(rec_line == INIL) return FALSE;
  if(rec_shot == FNIL) return FALSE;

  long nzt4 = _zt4_cards->numZt4Cards();
  long i;

  for(i = 0; i < nzt4; i++)
      {
      int maybe = _zt4_cards->zt4Card(i)->
                     receiverMaybeDead(rec_line, rec_shot);
      if(maybe) return TRUE;
      }
  return FALSE;
}



//------------------ group partly dead --------------------------------//
//------------------ group partly dead --------------------------------//
//------------------ group partly dead --------------------------------//

     // public.
     // return TRUE or FALSE.

int ZtCards::groupPartlyDead(long group)  const
{

  if(group == INIL) return FALSE;

  long nzt3 = _zt3_cards->numZt3Cards();
  long i;

  for(i = 0; i < nzt3; i++)
      {
      int maybe = _zt3_cards->zt3Card(i)->groupPartlyDead(group);
      if(maybe) return TRUE;
      }
  return FALSE;
}



//-------------- get num ZT cards or active index ------------//
//-------------- get num ZT cards or active index ------------//
//-------------- get num ZT cards or active index ------------//


#define ZT_NUM(_zt1_cards, numZt1Cards)    \
long ZtCards::numZt1Cards()  const         \
{                                          \
  return _zt1_cards->numZt1Cards();        \
}


ZT_NUM (_zt1_cards, numZt1Cards)
ZT_NUM (_zt2_cards, numZt2Cards)
ZT_NUM (_zt3_cards, numZt3Cards)
ZT_NUM (_zt4_cards, numZt4Cards)

ZT_NUM (_zt1_cards, getActiveZt1CardIndex)
ZT_NUM (_zt2_cards, getActiveZt2CardIndex)
ZT_NUM (_zt3_cards, getActiveZt3CardIndex)
ZT_NUM (_zt4_cards, getActiveZt4CardIndex)



//-------------- set active ZT card index or allocate space ----------//
//-------------- set active ZT card index or allocate space ----------//
//-------------- set active ZT card index or allocate space ----------//


#define ZT_ACTIVE(_zt1_cards, setActiveZt1CardIndex)    \
void ZtCards::setActiveZt1CardIndex(long value)         \
{                                                       \
  _zt1_cards->setActiveZt1CardIndex(value);             \
}


ZT_ACTIVE (_zt1_cards, setActiveZt1CardIndex)
ZT_ACTIVE (_zt2_cards, setActiveZt2CardIndex)
ZT_ACTIVE (_zt3_cards, setActiveZt3CardIndex)
ZT_ACTIVE (_zt4_cards, setActiveZt4CardIndex)

ZT_ACTIVE (_zt1_cards, allocateSpaceForZt1Cards)
ZT_ACTIVE (_zt2_cards, allocateSpaceForZt2Cards)
ZT_ACTIVE (_zt3_cards, allocateSpaceForZt3Cards)
ZT_ACTIVE (_zt4_cards, allocateSpaceForZt4Cards)



//---------------------- append ZT card -------------------------//
//---------------------- append ZT card -------------------------//
//---------------------- append ZT card -------------------------//


#define ZT_APPEND(_zt1_cards, appendNewZt1Card)   \
long ZtCards::appendNewZt1Card()                  \
{                                                 \
  return _zt1_cards->appendNewZt1Card();          \
}


ZT_APPEND (_zt1_cards, appendNewZt1Card)
ZT_APPEND (_zt2_cards, appendNewZt2Card)
ZT_APPEND (_zt3_cards, appendNewZt3Card)
ZT_APPEND (_zt4_cards, appendNewZt4Card)



//---------------- insert or delete ZT card ------------------//
//---------------- insert or delete ZT card ------------------//
//---------------- insert or delete ZT card ------------------//


#define ZT_INSERT(_zt1_cards, insertNewZt1Card)   \
long ZtCards::insertNewZt1Card(long ixzt)         \
{                                                 \
  return _zt1_cards->insertNewZt1Card(ixzt);      \
}


ZT_INSERT (_zt1_cards, insertNewZt1Card)
ZT_INSERT (_zt2_cards, insertNewZt2Card)
ZT_INSERT (_zt3_cards, insertNewZt3Card)
ZT_INSERT (_zt4_cards, insertNewZt4Card)

ZT_INSERT (_zt1_cards, insertNewZt1CardFromBuffer)
ZT_INSERT (_zt2_cards, insertNewZt2CardFromBuffer)
ZT_INSERT (_zt3_cards, insertNewZt3CardFromBuffer)
ZT_INSERT (_zt4_cards, insertNewZt4CardFromBuffer)

ZT_INSERT (_zt1_cards, deleteZt1Card)
ZT_INSERT (_zt2_cards, deleteZt2Card)
ZT_INSERT (_zt3_cards, deleteZt3Card)
ZT_INSERT (_zt4_cards, deleteZt4Card)

ZT_INSERT (_zt1_cards, deleteZt1CardToBuffer)
ZT_INSERT (_zt2_cards, deleteZt2CardToBuffer)
ZT_INSERT (_zt3_cards, deleteZt3CardToBuffer)
ZT_INSERT (_zt4_cards, deleteZt4CardToBuffer)



//-------------- delete all ZT cards or free space -------------//
//-------------- delete all ZT cards or free space -------------//
//-------------- delete all ZT cards or free space -------------//


#define ZT_DELETE(_zt1_cards, deleteAllZt1Cards)   \
void ZtCards::deleteAllZt1Cards()                  \
{                                                  \
  _zt1_cards->deleteAllZt1Cards();                 \
}


ZT_DELETE (_zt1_cards, deleteAllZt1Cards)
ZT_DELETE (_zt2_cards, deleteAllZt2Cards)
ZT_DELETE (_zt3_cards, deleteAllZt3Cards)
ZT_DELETE (_zt4_cards, deleteAllZt4Cards)

ZT_DELETE (_zt1_cards, freeSpaceForZt1Cards)
ZT_DELETE (_zt2_cards, freeSpaceForZt2Cards)
ZT_DELETE (_zt3_cards, freeSpaceForZt3Cards)
ZT_DELETE (_zt4_cards, freeSpaceForZt4Cards)



//---------------------- get ZT card value ------------------------//
//---------------------- get ZT card value ------------------------//
//---------------------- get ZT card value ------------------------//


#define ZT_GET(long2, _zt1_cards, getXxxx)       \
long2  ZtCards::getXxxx(long ixzt)  const        \
{                                                \
  return _zt1_cards->getXxxx(ixzt);              \
}


ZT_GET (int  , _zt1_cards, getZt1Code               )
ZT_GET (float, _zt1_cards, getZt1FromSourceShotpoint)
ZT_GET (float, _zt1_cards, getZt1ToSourceShotpoint  )
ZT_GET (long , _zt1_cards, getZt1SourceLineNumber   )

ZT_GET (int  , _zt2_cards, getZt2Code                 )
ZT_GET (float, _zt2_cards, getZt2FromReceiverShotpoint)
ZT_GET (float, _zt2_cards, getZt2ToReceiverShotpoint  )
ZT_GET (long , _zt2_cards, getZt2ReceiverLineNumber   )

ZT_GET (int  , _zt3_cards, getZt3Code           )
ZT_GET (long , _zt3_cards, getZt3FromGroupNumber)
ZT_GET (long , _zt3_cards, getZt3ToGroupNumber  )
ZT_GET (long , _zt3_cards, getZt3FromTraceNumber)
ZT_GET (long , _zt3_cards, getZt3ToTraceNumber  )

ZT_GET (int  , _zt4_cards, getZt4Code                 )
ZT_GET (float, _zt4_cards, getZt4FromSourceShotpoint  )
ZT_GET (float, _zt4_cards, getZt4ToSourceShotpoint    )
ZT_GET (long , _zt4_cards, getZt4SourceLineNumber     )
ZT_GET (float, _zt4_cards, getZt4FromReceiverShotpoint)
ZT_GET (float, _zt4_cards, getZt4ToReceiverShotpoint  )
ZT_GET (long , _zt4_cards, getZt4ReceiverLineNumber   )



//---------------------- set ZT card value ---------------------------//
//---------------------- set ZT card value ---------------------------//
//---------------------- set ZT card value ---------------------------//


#define ZT_SET(_zt1_cards, setXxxx, long2)          \
void ZtCards::setXxxx(long ixzt, long2 value)       \
{                                                   \
  _zt1_cards->setXxxx(ixzt, value);                 \
}


ZT_SET (_zt1_cards, setZt1Code                 , int  )
ZT_SET (_zt1_cards, setZt1FromSourceShotpoint  , float)
ZT_SET (_zt1_cards, setZt1ToSourceShotpoint    , float)
ZT_SET (_zt1_cards, setZt1SourceLineNumber     , long )

ZT_SET (_zt2_cards, setZt2Code                 , int  )
ZT_SET (_zt2_cards, setZt2FromReceiverShotpoint, float)
ZT_SET (_zt2_cards, setZt2ToReceiverShotpoint  , float)
ZT_SET (_zt2_cards, setZt2ReceiverLineNumber   , long )

ZT_SET (_zt3_cards, setZt3Code                 , int  )
ZT_SET (_zt3_cards, setZt3FromGroupNumber      , long )
ZT_SET (_zt3_cards, setZt3ToGroupNumber        , long )
ZT_SET (_zt3_cards, setZt3FromTraceNumber      , long )
ZT_SET (_zt3_cards, setZt3ToTraceNumber        , long )

ZT_SET (_zt4_cards, setZt4Code                 , int  )
ZT_SET (_zt4_cards, setZt4FromSourceShotpoint  , float)
ZT_SET (_zt4_cards, setZt4ToSourceShotpoint    , float)
ZT_SET (_zt4_cards, setZt4SourceLineNumber     , long )
ZT_SET (_zt4_cards, setZt4FromReceiverShotpoint, float)
ZT_SET (_zt4_cards, setZt4ToReceiverShotpoint  , float)
ZT_SET (_zt4_cards, setZt4ReceiverLineNumber   , long )



//-------------------------- end -------------------------------//
//-------------------------- end -------------------------------//
//-------------------------- end -------------------------------//

