
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
//---------------------- rp_table_gui.cc -----------------------//
//---------------------- rp_table_gui.cc -----------------------//
//---------------------- rp_table_gui.cc -----------------------//

//          implementation file for the RpTableGui class
//                derived from the SLDatabox class
//              also derived from the FgInform class
//                       subdirectory fggui


#include "fggui/rp_table_gui.hh"
#include "geom/field_geometry.hh"
#include "sl/sl_prim.hh"
#include "wbox.h"
#include "cprim.h"
#include <iostream.h>
#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <assert.h>


enum { DIAMOND  = LAST_RP_VARIABLE  + 1 };


//---------------------- constructor -----------------------//
//---------------------- constructor -----------------------//
//---------------------- constructor -----------------------//


RpTableGui::RpTableGui(SLDelay *slparent, char *name, FieldGeometry *fg)
           : SLDatabox(slparent, name, NULL, 4),
             FgInform(fg) 
{
  assert(fg);
  for(long i = 0; i < NCOLUMNS; i++) { _sw[i] = -1; }
  _sw[RP_PAT   - FIRST_RP_VARIABLE] = 1;
  _sw[RP_SHOT  - FIRST_RP_VARIABLE] = 1;
  _sw[RP_NX    - FIRST_RP_VARIABLE] = 1;
}


//------------------------ destructor --------------------------//
//------------------------ destructor --------------------------//
//------------------------ destructor --------------------------//

RpTableGui::~RpTableGui()
{
}



//-------------------- post new active rp card -------------------//
//-------------------- post new active rp card -------------------//
//-------------------- post new active rp card -------------------//

       // virtual function overriding FgInform.

void RpTableGui::postNewActiveRpCard(FieldGeometry *fg)
{
  void *box = getBox();
  long ixrp = fg->getActiveRpCardIndex();
  if(ixrp >= 0) wbox_set_focus(box, DIAMOND, (int)ixrp+1);
  _sw[DIAMOND - FIRST_RP_VARIABLE] = 1;
}



//----------------- append card if necessary ---------------------//
//----------------- append card if necessary ---------------------//
//----------------- append card if necessary ---------------------//

     // to be called from a trap.
     // tries to append a RP card if index == n.
     // tries to insert/remove RP card if endkey so dictates.
     // returns -1 if unsuccessfully tried to append RP card.
     // returns 0 if not necessary to append/insert/remove RP card.
     // returns 0 if RP card is successfully appended/inserted/removed.

static int append_card_if_necessary(FieldGeometry *fg,
                           long index, long nread, char *endkey)
{
  long n = fg->numRpCards();
  if(nread == 0)
      {
      if(strings_equal(endkey, "INSERT"))
          {
          long ixrp = fg->insertNewRpCardFromBuffer(index);
          if(ixrp == -1) return -1;
          }
      else if(index < n && strings_equal(endkey, "REMOVE"))
          {
          fg->deleteRpCardToBuffer(index);
          }
      }
  else if(index == n)
      {
      long ixrp = fg->appendNewRpCard();
      if(ixrp == -1) return -1;
      }
  return 0;
}



//---------------------- traps ------------------------------//
//---------------------- traps ------------------------------//
//---------------------- traps ------------------------------//


static void diamond_trap(void *data, long /*ident*/, long index,
                            long /*value*/, long nread, char *endkey)
{
  if(strings_equal(endkey, "ARRIVED")) return;
  RpTableGui    *table = (RpTableGui*)data;
  FieldGeometry *fg    = table->getFieldGeometry();
  fg->preMultipleOperations();
  table->disableMessages();
  int error = append_card_if_necessary(fg, index, nread, endkey);
  table->enableMessages();
  long n = fg->numRpCards();
  if(error || nread == 0 || index >= n)
      {
      fg->postMultipleOperations();
      return;
      }
  fg->setActiveRpCardIndex(index);
  fg->postMultipleOperations();
}



static void flag_trap(void *data, long /*ident*/, long index,
                            char * /*value*/, long nread, char *endkey)
{
  if(strings_equal(endkey, "ARRIVED")) return;
  RpTableGui    *table = (RpTableGui*)data;
  FieldGeometry *fg    = table->getFieldGeometry();
  fg->preMultipleOperations();
  table->disableMessages();
  int error = append_card_if_necessary(fg, index, nread, endkey);
  table->enableMessages();
  long n = fg->numRpCards();
  if(error || !strings_equal(endkey, "RETURN") || index >= n)
      {
      fg->postMultipleOperations();
      return;
      }
  long flag = fg->getRpFlag(index);
  switch(flag)
      {
      case RP_FLAG_X   : fg->setRpFlag(index, RP_FLAG_Y   ); break;
      case RP_FLAG_Y   : fg->setRpFlag(index, RP_FLAG_SKIP); break;
      case RP_FLAG_SKIP: fg->setRpFlag(index, RP_FLAG_DUP ); break;
      case RP_FLAG_DUP : fg->setRpFlag(index, RP_FLAG_X   ); break;
      default          : break;
      }
  fg->postMultipleOperations();
}


static void nchan_trap(void *data, long /*ident*/, long index,
                      long /*value*/, long nread, char *endkey) 
{                                                           
  if(strings_equal(endkey, "ARRIVED")) return;
  RpTableGui *table = (RpTableGui*)data;                   
  FieldGeometry *fg = table->getFieldGeometry();          
  table->disableMessages();
  int error = append_card_if_necessary(fg, index, nread, endkey);
  table->enableMessages();
}


#define TRAP(long2, xxxx_trap, setXxxx)                             \
static void xxxx_trap(void *data, long /*ident*/, long index,       \
                      long2 value, long nread, char *endkey)        \
{                                                                   \
  if(strings_equal(endkey, "ARRIVED")) return;                      \
  RpTableGui *table = (RpTableGui*)data;                            \
  FieldGeometry *fg = table->getFieldGeometry();                    \
  fg->preMultipleOperations();                                      \
  table->disableMessages();                                         \
  int error = append_card_if_necessary(fg, index, nread, endkey);   \
  table->enableMessages();                                          \
  if(!error && nread > 0)                                           \
      {                                                             \
      fg->setXxxx(index, value);                                    \
      }                                                             \
  fg->postMultipleOperations();                                     \
}


TRAP(long ,   pat_trap, setRpPatternNumber);
TRAP(float,  shot_trap, setRpShotpoint);
TRAP(long ,  line_trap, setRpLineNumber);
TRAP(long ,    nx_trap, setRpNumX);
TRAP(long ,  xinc_trap, setRpXinc);
TRAP(long ,    ny_trap, setRpNumY);
TRAP(long ,  yinc_trap, setRpYinc);
TRAP(float, xskid_trap, setRpXskid);
TRAP(float, yskid_trap, setRpYskid);
TRAP(float, eskid_trap, setRpEskid);



/*
static void table_trap(void *data, long ident, long index,
                            float value, long nread, char *endkey)
{
  RpTableGui *table = (RpTableGui*)data;
  FieldGeometry *fg = table->getFieldGeometry();
  table->disableMessages();
  int error = append_card_if_necessary(fg, index, nread, endkey);
  table->enableMessages();
  if(error) return;
  if(nread == 0) return;
  fg->setRpValue         (index, ident, value);
}
*/


void RpTableGui::promptTrap(void *data, long ident, long /*index*/,
                            char* /*value*/, long /*nread*/, char *endkey)
{
  if(strings_equal(endkey, "ARRIVED")) return;
  if(!strings_equal(endkey, "RETURN")) return;
  RpTableGui *table = (RpTableGui*)data;
        table->_sw[-ident - FIRST_RP_VARIABLE]
    = - table->_sw[-ident - FIRST_RP_VARIABLE]; 
}



//---------------- variable update functions --------------------//
//---------------- variable update functions --------------------//
//---------------- variable update functions --------------------//


/*
static float table_update(void *data, long ident, long index)
{
  RpTableGui    *table = (RpTableGui*)data;
  FieldGeometry *fg    = table->getFieldGeometry();
  return fg->getRpValue(index, ident);
}
*/



static long diamond_update(void *data, long /*ident*/, long index)
{
  RpTableGui    *table = (RpTableGui*)data;
  FieldGeometry *fg    = table->getFieldGeometry();
  long active_index = fg->getActiveRpCardIndex();
  return (active_index == index);
}



static char *flag_update(void *data, long /*ident*/, long index)
{
  static char buffer1[] = " X";
  static char buffer2[] = " Y";
  static char buffer3[] = "SKIP";
  static char buffer4[] = "DUP";
  static char buffer5[] = "????";
  RpTableGui    *table = (RpTableGui*)data;
  FieldGeometry *fg    = table->getFieldGeometry();
  long flag = fg->getRpFlag(index);
  char *buffer = buffer5;
  switch(flag)
      {
      case RP_FLAG_X   : buffer = buffer1; break;
      case RP_FLAG_Y   : buffer = buffer2; break;
      case RP_FLAG_SKIP: buffer = buffer3; break;
      case RP_FLAG_DUP : buffer = buffer4; break;
      default          : buffer = buffer5; break;
      }
  return buffer;
}


static long n_update(void *data)
{
  RpTableGui    *table = (RpTableGui*)data;
  FieldGeometry *fg    = table->getFieldGeometry();
  return fg->numRpCards();
}


static long nmax_update(void *data)
{
  return n_update(data) + 1;
}


#define UPDATE(long2, xxxx_update, getXxxx)                       \
static long2 xxxx_update(void *data, long /*ident*/, long index)  \
{                                                                 \
  RpTableGui    *table = (RpTableGui*)data;                       \
  FieldGeometry *fg    = table->getFieldGeometry();               \
  return fg->getXxxx(index);                                      \
}


UPDATE(long ,     pat_update, getRpPatternNumber);
UPDATE(long ,   nchan_update, getRpNumChannels);
UPDATE(long , cumchan_update, getRpCumChannels);
UPDATE(float,    shot_update, getRpShotpoint);
UPDATE(long ,    line_update, getRpLineNumber);
UPDATE(long ,      nx_update, getRpNumX);
UPDATE(long ,    xinc_update, getRpXinc);
UPDATE(long ,      ny_update, getRpNumY);
UPDATE(long ,    yinc_update, getRpYinc);
UPDATE(float,   xskid_update, getRpXskid);
UPDATE(float,   yskid_update, getRpYskid);
UPDATE(float,   eskid_update, getRpEskid);



//---------------- switch update functions --------------------//
//---------------- switch update functions --------------------//
//---------------- switch update functions --------------------//

//  switch update functions might be called even when index >= n:

long RpTableGui::tableSwitchUpdate(void *data, long ident, long index)
{
  RpTableGui    *table = (RpTableGui*)data;
  FieldGeometry *fg    = table->getFieldGeometry();
  long           sw    = table->_sw[ident - FIRST_RP_VARIABLE];
  if     (ident == RP_FLAG)    sw *= 2;
  else if(ident == RP_NCHAN)   sw *= 5;
  else if(ident == RP_CUMCHAN) sw *= 5;
  else if(ident > RP_LINE && index < fg->numRpCards())
      {             
      long rpflag = fg->getRpFlag(index);
      if(rpflag != RP_FLAG_X && rpflag != RP_FLAG_Y) sw *= 22;
      }            
  return sw;
}


long RpTableGui::diamondSwitchUpdate(void *data, long ident, long index)
{
  RpTableGui    *table = (RpTableGui*)data;
  FieldGeometry *fg    = table->getFieldGeometry();
  long           n     = fg->numRpCards();
  if(index >= n) return -77;
  return 4 * table->_sw[ident - FIRST_RP_VARIABLE];
}



//---------------------- make helper ------------------------//
//---------------------- make helper ------------------------//
//---------------------- make helper ------------------------//

void RpTableGui::makeHelper()
{
  static long two  = 2; 
  static long four = 4; 
  static long junk = 0; 

    //            PROMPT               ROW COL
  wbox_mreg("active"                 ,  0,  6);
  wbox_mreg("total  cumulative"      , -1, 20);
  wbox_mreg("---RECEIVER---"         , -1, 43);
  wbox_mreg("-INLINE--"              , -1, -1);
  wbox_mreg("CROSSLINE"              , -1, -1);
  wbox_mreg("--PATTERN-SKID---"      , -1, -1);

         //    N       NMAX       ROW COL NCHAR MAXROWS
  dbox_rega(n_update, nmax_update, 0,  0,   6,    35);

         //  ID          PROMPT      SWITCH   SWITCH  COL NCHAR NDEC
  dbox_irega(DIAMOND   , "  "       , &two ,  &four,   0,   2,   0);
  dbox_irega(RP_PAT    , "pat#"     , &two ,  &junk,   0,   6,   0);
  dbox_irega(RP_NCHAN  , "#channels", &two ,  &junk,   0,   9,   0);
  dbox_irega(RP_CUMCHAN, "#channels", &two ,  &junk,   0,   9,   0);
  dbox_crega(RP_FLAG   , "flag"     , &two ,  &junk,   0,   4,   0);
  dbox_frega(RP_SHOT   , "  SP#  "  , &two ,  &junk,   0,   7,   3);
  dbox_irega(RP_LINE   , "line# "   , &two ,  &junk,   0,   6,   0);
  dbox_irega(RP_NX     , " #X "     , &two ,  &junk,   0,   4,   0);
  dbox_irega(RP_XINC   , "XINC"     , &two ,  &junk,   0,   4,   0);
  dbox_irega(RP_NY     , " #Y "     , &two ,  &junk,   0,   4,   0);
  dbox_irega(RP_YINC   , "YINC"     , &two ,  &junk,   0,   4,   0);
  dbox_frega(RP_XSKID  , "XSKID"    , &two ,  &junk,   0,   5,   0);
  dbox_frega(RP_YSKID  , "YSKID"    , &two ,  &junk,   0,   5,   0);
  dbox_frega(RP_ESKID  , "ESKID"    , &two ,  &junk,   0,   5,   0);

  for(int i = 0; i < NUM_RP_VARIABLES; i++)
       {
       int ident = (int)FIRST_RP_VARIABLE + i;
       dbox_set_ctrap(-ident, promptTrap);
       dbox_set_sfun ( ident, tableSwitchUpdate);
       }

  dbox_set_itrap( DIAMOND, diamond_trap);
  dbox_set_ifun ( DIAMOND, diamond_update);
  dbox_set_sfun ( DIAMOND, diamondSwitchUpdate);
  dbox_set_ctrap(-DIAMOND, promptTrap);

  dbox_set_itrap(RP_PAT    ,   pat_trap);
  dbox_set_itrap(RP_NCHAN  , nchan_trap);  // for REMOVE/INSERT only
  dbox_set_itrap(RP_CUMCHAN, nchan_trap);  // for REMOVE/INSERT only
  dbox_set_ctrap(RP_FLAG   ,  flag_trap);
  dbox_set_ftrap(RP_SHOT   ,  shot_trap);
  dbox_set_itrap(RP_LINE   ,  line_trap);
  dbox_set_itrap(RP_NX     ,    nx_trap);
  dbox_set_itrap(RP_XINC   ,  xinc_trap);
  dbox_set_itrap(RP_NY     ,    ny_trap);
  dbox_set_itrap(RP_YINC   ,  yinc_trap);
  dbox_set_ftrap(RP_XSKID  , xskid_trap);
  dbox_set_ftrap(RP_YSKID  , yskid_trap);
  dbox_set_ftrap(RP_ESKID  , eskid_trap);

  dbox_set_ifun (RP_PAT    ,     pat_update);
  dbox_set_ifun (RP_NCHAN  ,   nchan_update);
  dbox_set_ifun (RP_CUMCHAN, cumchan_update);
  dbox_set_cfun (RP_FLAG   ,    flag_update);
  dbox_set_ffun (RP_SHOT   ,    shot_update);
  dbox_set_ifun (RP_LINE   ,    line_update);
  dbox_set_ifun (RP_NX     ,      nx_update);
  dbox_set_ifun (RP_XINC   ,    xinc_update);
  dbox_set_ifun (RP_NY     ,      ny_update);
  dbox_set_ifun (RP_YINC   ,    yinc_update);
  dbox_set_ffun (RP_XSKID  ,   xskid_update);
  dbox_set_ffun (RP_YSKID  ,   yskid_update);
  dbox_set_ffun (RP_ESKID  ,   eskid_update);
}



//-------------------------- end -------------------------------//
//-------------------------- end -------------------------------//
//-------------------------- end -------------------------------//
