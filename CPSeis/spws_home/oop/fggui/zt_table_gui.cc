
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
//---------------------- zt_table_gui.cc -----------------------//
//---------------------- zt_table_gui.cc -----------------------//
//---------------------- zt_table_gui.cc -----------------------//

//          implementation file for the ZtTableGui class
//                derived from the SLDatabox class
//              also derived from the FgInform class
//                       subdirectory fggui


#include "fggui/zt_table_gui.hh"
#include "geom/field_geometry.hh"
#include "sl/sl_prim.hh"
#include "wbox.h"
#include "cprim.h"
#include <iostream.h>
#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <assert.h>


    // the index of a switch is its ident minus _first_ident.

    // note that this class makes use of the fact that the
    // idents for all four types of ZT cards are unique.


//---------------------- constructor -----------------------//
//---------------------- constructor -----------------------//
//---------------------- constructor -----------------------//


ZtTableGui::ZtTableGui(SLDelay *slparent, char *name, FieldGeometry *fg,
                                                 int which)
           : SLDatabox(slparent, name, NULL, 4),
             FgInform(fg),
                  _which           (which),
                  _num_switches    (0),
                  _first_ident     (0),
                  _diamond_ident   (0),
                  _code_ident      (0)
{
  assert(fg);
  switch(which)
      {
      case 1: _num_switches  =   NUM_ZT1_VARIABLES + 1;
              _first_ident   = FIRST_ZT1_VARIABLE;
              _diamond_ident =  LAST_ZT1_VARIABLE + 1;
              _code_ident    =       ZT1_CODE;
              break;
      case 2: _num_switches  =   NUM_ZT2_VARIABLES + 1;
              _first_ident   = FIRST_ZT2_VARIABLE;
              _diamond_ident =  LAST_ZT2_VARIABLE + 1;
              _code_ident    =       ZT2_CODE;
              break;
      case 3: _num_switches  =   NUM_ZT3_VARIABLES + 1;
              _first_ident   = FIRST_ZT3_VARIABLE;
              _diamond_ident =  LAST_ZT3_VARIABLE + 1;
              _code_ident    =       ZT3_CODE;
              break;
      case 4: _num_switches  =   NUM_ZT4_VARIABLES + 1;
              _first_ident   = FIRST_ZT4_VARIABLE;
              _diamond_ident =  LAST_ZT4_VARIABLE + 1;
              _code_ident    =       ZT4_CODE;
              break;
      default: assert(FALSE);
      }
  _sw = new long [_num_switches];
  for(long i = 0; i < _num_switches; i++) { _sw[i] = 1; }
  _sw[_diamond_ident - _first_ident] = -4;
  _sw[_code_ident    - _first_ident] =  2;
}


//------------------------ destructor --------------------------//
//------------------------ destructor --------------------------//
//------------------------ destructor --------------------------//

ZtTableGui::~ZtTableGui()
{
  delete [] _sw;
}



//-------------------- post new active zt card -------------------//
//-------------------- post new active zt card -------------------//
//-------------------- post new active zt card -------------------//

       // virtual functions overriding FgInform.

void ZtTableGui::postNewActiveZt1Card(FieldGeometry *fg)
{
  if(_which != 1) return;
  void *box = getBox();
  long ixzt1 = fg->getActiveZt1CardIndex();
  if(ixzt1 >= 0) wbox_set_focus(box, _diamond_ident, (int)ixzt1+1);
}


void ZtTableGui::postNewActiveZt2Card(FieldGeometry *fg)
{
  if(_which != 2) return;
  void *box = getBox();
  long ixzt2 = fg->getActiveZt2CardIndex();
  if(ixzt2 >= 0) wbox_set_focus(box, _diamond_ident, (int)ixzt2+1);
}


void ZtTableGui::postNewActiveZt3Card(FieldGeometry *fg)
{
  if(_which != 3) return;
  void *box = getBox();
  long ixzt3 = fg->getActiveZt3CardIndex();
  if(ixzt3 >= 0) wbox_set_focus(box, _diamond_ident, (int)ixzt3+1);
}


void ZtTableGui::postNewActiveZt4Card(FieldGeometry *fg)
{
  if(_which != 4) return;
  void *box = getBox();
  long ixzt4 = fg->getActiveZt4CardIndex();
  if(ixzt4 >= 0) wbox_set_focus(box, _diamond_ident, (int)ixzt4+1);
}



//--------------------- convenience functions ----------------------//
//--------------------- convenience functions ----------------------//
//--------------------- convenience functions ----------------------//

     // private.

long ZtTableGui::numCards()
{
  long n;
  switch(_which)
      {
      case 1: n = _fg->numZt1Cards(); break;
      case 2: n = _fg->numZt2Cards(); break;
      case 3: n = _fg->numZt3Cards(); break;
      case 4: n = _fg->numZt4Cards(); break;
      default: assert(FALSE);
      }
  return n;
}



long ZtTableGui::insertCard(long index)
{
  long ixzt;
  switch(_which)
      {
      case 1: ixzt = _fg->insertNewZt1CardFromBuffer(index); break;
      case 2: ixzt = _fg->insertNewZt2CardFromBuffer(index); break;
      case 3: ixzt = _fg->insertNewZt3CardFromBuffer(index); break;
      case 4: ixzt = _fg->insertNewZt4CardFromBuffer(index); break;
      default: assert(FALSE);
      }
  return ixzt;
}



long ZtTableGui::removeCard(long index)
{
  long ixzt;
  switch(_which)
      {
      case 1: ixzt = _fg->deleteZt1CardToBuffer(index); break;
      case 2: ixzt = _fg->deleteZt2CardToBuffer(index); break;
      case 3: ixzt = _fg->deleteZt3CardToBuffer(index); break;
      case 4: ixzt = _fg->deleteZt4CardToBuffer(index); break;
      default: assert(FALSE);
      }
  return ixzt;
}



long ZtTableGui::appendCard()
{
  long ixzt;
  switch(_which)
      {
      case 1: ixzt = _fg->appendNewZt1Card(); break;
      case 2: ixzt = _fg->appendNewZt2Card(); break;
      case 3: ixzt = _fg->appendNewZt3Card(); break;
      case 4: ixzt = _fg->appendNewZt4Card(); break;
      default: assert(FALSE);
      }
  return ixzt;
}



long ZtTableGui::getCode(long index)
{
  long code;
  switch(_which)
      {
      case 1: code = _fg->getZt1Code(index); break;
      case 2: code = _fg->getZt2Code(index); break;
      case 3: code = _fg->getZt3Code(index); break;
      case 4: code = _fg->getZt4Code(index); break;
      default: assert(FALSE);
      }
  return code;
}



void ZtTableGui::setCode(long index, long code)
{
  switch(_which)
      {
      case 1: _fg->setZt1Code(index, (int)code); break;
      case 2: _fg->setZt2Code(index, (int)code); break;
      case 3: _fg->setZt3Code(index, (int)code); break;
      case 4: _fg->setZt4Code(index, (int)code); break;
      default: assert(FALSE);
      }
}



long ZtTableGui::getLongValue(int ident, long index)
{
  long value;
  switch(ident)
     {
     case ZT1_LINE  : value = _fg->getZt1SourceLineNumber     (index); break;
     case ZT2_LINE  : value = _fg->getZt2ReceiverLineNumber   (index); break;
     case ZT3_GROUP1: value = _fg->getZt3FromGroupNumber      (index); break;
     case ZT3_GROUP2: value = _fg->getZt3ToGroupNumber        (index); break;
     case ZT3_TRACE1: value = _fg->getZt3FromTraceNumber      (index); break;
     case ZT3_TRACE2: value = _fg->getZt3ToTraceNumber        (index); break;
     case ZT4_SLINE : value = _fg->getZt4SourceLineNumber     (index); break;
     case ZT4_RLINE : value = _fg->getZt4ReceiverLineNumber   (index); break;
     default: assert(FALSE);
     }
  return value;
}



void ZtTableGui::setLongValue(int ident, long index, long value)
{
  switch(ident)
     {
     case ZT1_LINE  : _fg->setZt1SourceLineNumber     (index, value); break;
     case ZT2_LINE  : _fg->setZt2ReceiverLineNumber   (index, value); break;
     case ZT3_GROUP1: _fg->setZt3FromGroupNumber      (index, value); break;
     case ZT3_GROUP2: _fg->setZt3ToGroupNumber        (index, value); break;
     case ZT3_TRACE1: _fg->setZt3FromTraceNumber      (index, value); break;
     case ZT3_TRACE2: _fg->setZt3ToTraceNumber        (index, value); break;
     case ZT4_SLINE : _fg->setZt4SourceLineNumber     (index, value); break;
     case ZT4_RLINE : _fg->setZt4ReceiverLineNumber   (index, value); break;
     default: assert(FALSE);
     }                                               
}



float ZtTableGui::getFloatValue(int ident, long index)
{
  float value;
  switch(ident)
     {                                                 
     case ZT1_SHOT1 : value = _fg->getZt1FromSourceShotpoint  (index); break;
     case ZT1_SHOT2 : value = _fg->getZt1ToSourceShotpoint    (index); break;
     case ZT2_SHOT1 : value = _fg->getZt2FromReceiverShotpoint(index); break;
     case ZT2_SHOT2 : value = _fg->getZt2ToReceiverShotpoint  (index); break;
     case ZT4_SSHOT1: value = _fg->getZt4FromSourceShotpoint  (index); break;
     case ZT4_SSHOT2: value = _fg->getZt4ToSourceShotpoint    (index); break;
     case ZT4_RSHOT1: value = _fg->getZt4FromReceiverShotpoint(index); break;
     case ZT4_RSHOT2: value = _fg->getZt4ToReceiverShotpoint  (index); break;
     default: assert(FALSE);
     }
  return value;
}



void ZtTableGui::setFloatValue(int ident, long index, float value)
{
  switch(ident)
     {                                                 
     case ZT1_SHOT1 : _fg->setZt1FromSourceShotpoint  (index, value); break;
     case ZT1_SHOT2 : _fg->setZt1ToSourceShotpoint    (index, value); break;
     case ZT2_SHOT1 : _fg->setZt2FromReceiverShotpoint(index, value); break;
     case ZT2_SHOT2 : _fg->setZt2ToReceiverShotpoint  (index, value); break;
     case ZT4_SSHOT1: _fg->setZt4FromSourceShotpoint  (index, value); break;
     case ZT4_SSHOT2: _fg->setZt4ToSourceShotpoint    (index, value); break;
     case ZT4_RSHOT1: _fg->setZt4FromReceiverShotpoint(index, value); break;
     case ZT4_RSHOT2: _fg->setZt4ToReceiverShotpoint  (index, value); break;
     default: assert(FALSE);
     }                                               
}



long ZtTableGui::getActiveCardIndex()
{
  long index;
  switch(_which)
      {
      case 1: index = _fg->getActiveZt1CardIndex(); break;
      case 2: index = _fg->getActiveZt2CardIndex(); break;
      case 3: index = _fg->getActiveZt3CardIndex(); break;
      case 4: index = _fg->getActiveZt4CardIndex(); break;
      default: assert(FALSE);
      }
  return index;
}



void ZtTableGui::setActiveCardIndex(long index)
{
  switch(_which)
      {
      case 1: _fg->setActiveZt1CardIndex(index); break;
      case 2: _fg->setActiveZt2CardIndex(index); break;
      case 3: _fg->setActiveZt3CardIndex(index); break;
      case 4: _fg->setActiveZt4CardIndex(index); break;
      default: assert(FALSE);
      }
}



//--------------------- maybe append card ------------------------//
//--------------------- maybe append card ------------------------//
//--------------------- maybe append card ------------------------//

     // private.
     // to be called from a trap.
     // tries to append a ZT card if index == n.
     // tries to insert/remove ZT card if endkey so dictates.
     // returns -1 if unsuccessfully tried to append ZT card.
     // returns 0 if not necessary to append/insert/remove ZT card.
     // returns 0 if ZT card is successfully appended/inserted/removed.

int ZtTableGui::maybeAppendCard(long index, long nread, char *endkey)
{
  long n = numCards();
  if(nread == 0)
      {
      if(strings_equal(endkey, "INSERT"))
          {
          long ixzt = insertCard(index);
          if(ixzt == -1) return -1;
          }
      else if(index < n && strings_equal(endkey, "REMOVE"))
          {
          removeCard(index);
          }
      }
  else if(index == n)
      {
      long ixzt = appendCard();
      if(ixzt == -1) return -1;
      }
  return 0;
}



//---------------------- traps ------------------------------//
//---------------------- traps ------------------------------//
//---------------------- traps ------------------------------//


void ZtTableGui::diamondTrap(void *data, long ident, long index,
                            long /*value*/, long nread, char *endkey)
{
  if(strings_equal(endkey, "ARRIVED")) return;
  ZtTableGui    *table = (ZtTableGui*)data;
  FieldGeometry *fg    = table->getFieldGeometry();
  fg->preMultipleOperations();
  table->disableMessages();
  int error = table->maybeAppendCard(index, nread, endkey);
  table->enableMessages();
  long n = table->numCards();
  if(error || nread == 0 || index >= n)
      {
      fg->postMultipleOperations();
      return;
      }
  assert(ident == table->_diamond_ident);
  table->setActiveCardIndex(index);
  fg->postMultipleOperations();
}



void ZtTableGui::codeTrap(void *data, long ident, long index,
                            char * /*value*/, long nread, char *endkey)
{
  if(strings_equal(endkey, "ARRIVED")) return;
  ZtTableGui    *table = (ZtTableGui*)data;
  FieldGeometry *fg    = table->getFieldGeometry();
  fg->preMultipleOperations();
  table->disableMessages();
  int error = table->maybeAppendCard(index, nread, endkey);
  table->enableMessages();
  long n = table->numCards();
  if(error || !strings_equal(endkey, "RETURN") || index >= n)
      {
      fg->postMultipleOperations();
      return;
      }
  assert(ident == table->_code_ident);
  long code = table->getCode(index);
  switch(code)
      {
      case ZT_CODE_ZERO: table->setCode(index, ZT_CODE_REV ); break;
      case ZT_CODE_REV : table->setCode(index, ZT_CODE_MISS); break;
      case ZT_CODE_MISS: table->setCode(index, ZT_CODE_ZERO); break;
      default          : break;
      }
  fg->postMultipleOperations();
}



void ZtTableGui::longTrap(void *data, long ident, long index,
                          long value, long nread, char *endkey)
{                                                          
  if(strings_equal(endkey, "ARRIVED")) return;            
  ZtTableGui *table = (ZtTableGui*)data;                 
  FieldGeometry *fg = table->getFieldGeometry();        
  fg->preMultipleOperations();                         
  table->disableMessages();                           
  int error = table->maybeAppendCard(index, nread, endkey);
  table->enableMessages();                                
  if(!error && nread > 0)                                
      {
      table->setLongValue((int)ident, index, value);
      long n = table->numCards();
      if(index == n-1 && (ident == 32 || ident == 34))
          {
          int ident2 = (int)ident + 1;
          long value2 = table->getLongValue(ident2, index);
          if(value2 == 0) table->setLongValue(ident2, index, value);
          }
      }                                               
  fg->postMultipleOperations();                      
}



void ZtTableGui::floatTrap(void *data, long ident, long index,
                          float value, long nread, char *endkey)
{                                                          
  if(strings_equal(endkey, "ARRIVED")) return;            
  ZtTableGui *table = (ZtTableGui*)data;                 
  FieldGeometry *fg = table->getFieldGeometry();        
  fg->preMultipleOperations();                         
  table->disableMessages();                           
  int error = table->maybeAppendCard(index, nread, endkey);
  table->enableMessages();                                
  if(!error && nread > 0)                                
      {
      table->setFloatValue((int)ident, index, value);
      long n = table->numCards();
      if(index == n-1 &&
            (ident == 12 || ident == 22 || ident == 42 || ident == 45))
          {
          int ident2 = (int)ident + 1;
          float value2 = table->getFloatValue(ident2, index);
          if(value2 == 0.0) table->setFloatValue(ident2, index, value);
          }
      }                                               
  fg->postMultipleOperations();                      
}



void ZtTableGui::promptTrap(void *data, long ident, long /*index*/,
                            char* /*value*/, long /*nread*/, char *endkey)
{
  if(strings_equal(endkey, "ARRIVED")) return;
  if(!strings_equal(endkey, "RETURN")) return;
  ZtTableGui *table = (ZtTableGui*)data;
        table->_sw[-ident - table->_first_ident]
    = - table->_sw[-ident - table->_first_ident]; 
}



//---------------- variable update functions --------------------//
//---------------- variable update functions --------------------//
//---------------- variable update functions --------------------//

           // private static functions.

long ZtTableGui::diamondUpdate(void *data, long /*ident*/, long index)
{
  ZtTableGui *table = (ZtTableGui*)data;
  long active_index = table->getActiveCardIndex();
  return (active_index == index);
}



char *ZtTableGui::codeUpdate(void *data, long /*ident*/, long index)
{
  static char buffer1[] = "ZERO";
  static char buffer2[] = "REV ";
  static char buffer3[] = "MISS";
  static char buffer4[] = "????";
  ZtTableGui *table = (ZtTableGui*)data;
  long flag = table->getCode(index);
  char *buffer = buffer4;
  switch(flag)
      {
      case ZT_CODE_ZERO: buffer = buffer1; break;
      case ZT_CODE_REV : buffer = buffer2; break;
      case ZT_CODE_MISS: buffer = buffer3; break;
      default          : buffer = buffer4; break;
      }
  return buffer;
}


long ZtTableGui::nUpdate(void *data)
{
  ZtTableGui *table = (ZtTableGui*)data;
  return table->numCards();
}


long ZtTableGui::nmaxUpdate(void *data)
{
  ZtTableGui *table = (ZtTableGui*)data;
  return table->numCards() + 1;
}


long ZtTableGui::longUpdate(void *data, long ident, long index)
{                                         
  ZtTableGui    *table = (ZtTableGui*)data;
  return table->getLongValue((int)ident, index);
}


float ZtTableGui::floatUpdate(void *data, long ident, long index)
{                                         
  ZtTableGui    *table = (ZtTableGui*)data;
  return table->getFloatValue((int)ident, index);
}



//---------------- switch update functions --------------------//
//---------------- switch update functions --------------------//
//---------------- switch update functions --------------------//

  // private static function.
  // switch update functions might be called even when index >= n:

long ZtTableGui::switchUpdate(void *data, long ident, long index)
{
  ZtTableGui *table = (ZtTableGui*)data;
  long        n     = table->numCards();
  if(index >= n && ident == table->_diamond_ident) return -4;
  return table->_sw[ident - table->_first_ident];
}



//---------------------- make helper ------------------------//
//---------------------- make helper ------------------------//
//---------------------- make helper ------------------------//

void ZtTableGui::makeHelper()
{
  static long two  = 2; 
  static long junk = 0; 

         //    N       NMAX     ROW COL NCHAR MAXROWS
  dbox_rega(nUpdate, nmaxUpdate, 2,  0,   5,    35);

  wbox_mreg("active", 1, 4);

         //      ID          PROMPT      SWITCH   SWITCH  COL NCHAR NDEC
  dbox_irega(_diamond_ident, "  "       , &two ,  &junk,   0,   2,   0);
  dbox_crega(   _code_ident, "code"     , &two ,  &junk,   0,   4,   0);

  dbox_set_itrap(_diamond_ident, diamondTrap);
  dbox_set_ifun (_diamond_ident, diamondUpdate);
  dbox_set_ctrap(   _code_ident,    codeTrap);
  dbox_set_cfun (   _code_ident,    codeUpdate);

         //  ID          PROMPT      SWITCH   SWITCH  COL NCHAR NDEC

  switch(_which)
     {
     case 1:
     wbox_mreg("<-------SOURCE-------->", 1, 16);
     dbox_frega(ZT1_SHOT1 , "from SP#" , &two ,  &junk,  16,   8,   3);
     dbox_frega(ZT1_SHOT2 , " to SP# " , &two ,  &junk,   0,   8,   3);
     dbox_irega(ZT1_LINE  , "line#"    , &two ,  &junk,   0,   5,   0);
     dbox_set_ftrap(ZT1_SHOT1 , floatTrap);
     dbox_set_ftrap(ZT1_SHOT2 , floatTrap);
     dbox_set_itrap(ZT1_LINE  ,  longTrap);
     dbox_set_ffun (ZT1_SHOT1 , floatUpdate);
     dbox_set_ffun (ZT1_SHOT2 , floatUpdate);
     dbox_set_ifun (ZT1_LINE  ,  longUpdate);
     break;

     case 2:
     wbox_mreg("<------RECEIVER------->", 1,16);
     dbox_frega(ZT2_SHOT1 , "from SP#" , &two ,  &junk,  16,   8,   3);
     dbox_frega(ZT2_SHOT2 , " to SP# " , &two ,  &junk,   0,   8,   3);
     dbox_irega(ZT2_LINE  , "line#"    , &two ,  &junk,   0,   5,   0);
     dbox_set_ftrap(ZT2_SHOT1 , floatTrap);
     dbox_set_ftrap(ZT2_SHOT2 , floatTrap);
     dbox_set_itrap(ZT2_LINE  ,  longTrap);
     dbox_set_ffun (ZT2_SHOT1 , floatUpdate);
     dbox_set_ffun (ZT2_SHOT2 , floatUpdate);
     dbox_set_ifun (ZT2_LINE  ,  longUpdate);
     break;

     case 3:
     dbox_irega(ZT3_GROUP1, "from GROUP#" , &two , &junk, 16,  11,  0);
     dbox_irega(ZT3_GROUP2, " to GROUP# " , &two , &junk,  0,  11,  0);
     dbox_irega(ZT3_TRACE1, "from trace#" , &two , &junk, 41,  11,  0);
     dbox_irega(ZT3_TRACE2, " to trace# " , &two , &junk,  0,  11,  0);
     dbox_set_itrap(ZT3_GROUP1,  longTrap);
     dbox_set_itrap(ZT3_GROUP2,  longTrap);
     dbox_set_itrap(ZT3_TRACE1,  longTrap);
     dbox_set_itrap(ZT3_TRACE2,  longTrap);
     dbox_set_ifun (ZT3_GROUP1,  longUpdate);
     dbox_set_ifun (ZT3_GROUP2,  longUpdate);
     dbox_set_ifun (ZT3_TRACE1,  longUpdate);
     dbox_set_ifun (ZT3_TRACE2,  longUpdate);
     break;

     case 4:
     wbox_mreg("<-------SOURCE-------->", 1, 16);
     wbox_mreg("<------RECEIVER------->", 1, 41);
     dbox_frega(ZT4_SSHOT1 , "from SP#"  , &two ,  &junk,  16,   8,   3);
     dbox_frega(ZT4_SSHOT2 , " to SP# "  , &two ,  &junk,   0,   8,   3);
     dbox_irega(ZT4_SLINE  , "line#"     , &two ,  &junk,   0,   5,   0);
     dbox_frega(ZT4_RSHOT1 , "from SP#"  , &two ,  &junk,  41,   8,   3);
     dbox_frega(ZT4_RSHOT2 , " to SP# "  , &two ,  &junk,   0,   8,   3);
     dbox_irega(ZT4_RLINE  , "line#"     , &two ,  &junk,   0,   5,   0);
     dbox_set_ftrap(ZT4_SSHOT1 , floatTrap);
     dbox_set_ftrap(ZT4_SSHOT2 , floatTrap);
     dbox_set_itrap(ZT4_SLINE  ,  longTrap);
     dbox_set_ftrap(ZT4_RSHOT1 , floatTrap);
     dbox_set_ftrap(ZT4_RSHOT2 , floatTrap);
     dbox_set_itrap(ZT4_RLINE  ,  longTrap);
     dbox_set_ffun (ZT4_SSHOT1 , floatUpdate);
     dbox_set_ffun (ZT4_SSHOT2 , floatUpdate);
     dbox_set_ifun (ZT4_SLINE  ,  longUpdate);
     dbox_set_ffun (ZT4_RSHOT1 , floatUpdate);
     dbox_set_ffun (ZT4_RSHOT2 , floatUpdate);
     dbox_set_ifun (ZT4_RLINE  ,  longUpdate);
     break;

     default: assert(FALSE);
     }

  for(int i = 0; i < _num_switches; i++)
       {
       int ident = (int)_first_ident + i;
       dbox_set_ctrap(-ident, promptTrap);
       dbox_set_sfun ( ident, switchUpdate);
       }
}



//-------------------------- end -------------------------------//
//-------------------------- end -------------------------------//
//-------------------------- end -------------------------------//
