
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
//---------------------- linelist_table_gui.cc -----------------------//
//---------------------- linelist_table_gui.cc -----------------------//
//---------------------- linelist_table_gui.cc -----------------------//

//        implementation file for the LinelistTableGui class
//                derived from the SLDatabox class
//                       subdirectory fggui


#include "fggui/linelist_table_gui.hh"
#include "geom/field_geometry.hh"
#include "sl/sl_prim.hh"
#include "wbox.h"
#include "cprim.h"
#include "named_constants.h"
#include <iostream.h>
#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <assert.h>

//#define BOTH_GP TRUE
#define BOTH_GP FALSE

//---------------------- constructor -----------------------//
//---------------------- constructor -----------------------//
//---------------------- constructor -----------------------//


LinelistTableGui::LinelistTableGui(SLDelay *slparent, char *name,
                                   FieldGeometry *fg)
           : SLDatabox(slparent, name, NULL, 4),
             FgInform(fg)
{
  assert(fg);
}


//------------------------ destructor --------------------------//
//------------------------ destructor --------------------------//
//------------------------ destructor --------------------------//

LinelistTableGui::~LinelistTableGui()
{
}



//-------------------- post new active line ----------------------//
//-------------------- post new active line ----------------------//
//-------------------- post new active line ----------------------//

       // virtual function overriding FgInform.

void LinelistTableGui::postNewActiveLine(FieldGeometry *fg)
{
  void *box = getBox();
  long ixl = fg->getActiveLineIndex();
  if(ixl >= 0) wbox_set_focus(box, 66, (int)ixl+1);
}



//------------------- get field geometry --------------------------//
//------------------- get field geometry --------------------------//
//------------------- get field geometry --------------------------//

static inline FieldGeometry *get_field_geometry(void *data)
{
  LinelistTableGui *table = (LinelistTableGui*)data;
  return table->getFieldGeometry();
}



//----------------- first things first ---------------------------//
//----------------- first things first ---------------------------//
//----------------- first things first ---------------------------//

     // to be called from a trap.
     // tries to append a line if index == n.
     // tries to insert/remove line if endkey so dictates.
     // returns fg if nread > 0 and index < n and any attempted
     //   operation succeeds.
     // otherwise returns NULL.

static FieldGeometry *first_things_first(void *data,
                           long index, long nread, char *endkey)
{
  LinelistTableGui *table = (LinelistTableGui*)data;
  FieldGeometry *fg = table->getFieldGeometry();
  fg->preMultipleOperations();
  long n = fg->numLines();
  if(nread == 0)
      {
      table->disableMessages();
      if(strings_equal(endkey, "INSERT"))
          {
          fg->insertNewLineFromBuffer(index);
          }
      else if(index < n && strings_equal(endkey, "REMOVE"))
          {
          fg->deleteLineToBuffer(index);
          }
      table->enableMessages();
      fg->postMultipleOperations();
      return NULL;
      }
  else if(index == n)
      {
      long ixl = fg->appendNewLine();
      if(ixl == -1)
          {
          fg->postMultipleOperations();
          return NULL;
          }
      }
  return fg;
}



//---------------------- traps ------------------------------//
//---------------------- traps ------------------------------//
//---------------------- traps ------------------------------//


static void diamond_trap(void *data, long /*ident*/, long index,
                            long /*value*/, long nread, char *endkey)
{
  if(strings_equal(endkey, "ARRIVED")) return;
  FieldGeometry *fg = first_things_first(data, index, nread, endkey);
  if(!fg) return;
  fg->setActiveLineIndex(index);
  fg->postMultipleOperations();
}


static void select_trap(void *data, long /*ident*/, long index,
                         char* /*value*/, long /*nread*/, char *endkey)
{
  if(!strings_equal(endkey, "RETURN")) return;
  FieldGeometry *fg = get_field_geometry(data);
  long n = fg->numLines();
  if(index < n) fg->incrementLineSelectValue(index);
}


static void line_trap(void *data, long /*ident*/, long index,
                            long value, long nread, char *endkey)
{
  if(strings_equal(endkey, "ARRIVED")) return;
  FieldGeometry *fg = first_things_first(data, index, nread, endkey);
  if(!fg) return;
  fg->setLineNumber(index, value);
  fg->postMultipleOperations();
}



//------------------------ update functions --------------------//
//------------------------ update functions --------------------//
//------------------------ update functions --------------------//


static long diamond_update(void *data, long /*ident*/, long index)
{
  FieldGeometry *fg = get_field_geometry(data);
  long active_index = fg->getActiveLineIndex();
  return (active_index == index);
}


static char *select_update(void *data, long /*ident*/, long index)
{
  static char buffer[2];
  FieldGeometry *fg = get_field_geometry(data);
  buffer[0] = fg->getLineSelectValue(index);
  buffer[1] = '\0';
  return buffer;
}


static long line_update(void *data, long /*ident*/, long index)
{
  FieldGeometry *fg = get_field_geometry(data);
  return fg->getLineNumber(index);
}


static long n_update(void *data)
{
  FieldGeometry *fg = get_field_geometry(data);
  return fg->numLines();
}


static long nmax_update(void *data)
{
  return n_update(data) + 1;
}



static long flags_update(void *data, long /*ident*/, long index)
{
  FieldGeometry *fg = get_field_geometry(data);
  return fg->numFlagsOnLine(index);
}


static long sources_update(void *data, long /*ident*/, long index)
{
  FieldGeometry *fg = get_field_geometry(data);
  if(fg->sourceGathersOutOfDate()) return INIL;
  return fg->numSourcesOnLine(index);
}



static long receivers_update(void *data, long /*ident*/, long index)
{
  FieldGeometry *fg = get_field_geometry(data);
  if(fg->receiverGathersOutOfDate()) return INIL;
  return fg->numReceiversOnLine(index);
}


static float first_sp_update(void *data, long /*ident*/, long index)
{
  FieldGeometry *fg = get_field_geometry(data);
  return fg->getFirstShotpointOnLine(index);
}


static float last_sp_update(void *data, long /*ident*/, long index)
{
  FieldGeometry *fg = get_field_geometry(data);
  return fg->getLastShotpointOnLine(index);
}


static float active_sp_update(void *data, long /*ident*/, long index)
{
  FieldGeometry *fg = get_field_geometry(data);
  return fg->getActiveShotpointOnLine(index);
}


static float min_incr_update(void *data, long /*ident*/, long index)
{
  FieldGeometry *fg = get_field_geometry(data);
  return fg->getMinShotpointIncrOnLine(index);
}


static float max_incr_update(void *data, long /*ident*/, long index)
{
  FieldGeometry *fg = get_field_geometry(data);
  return fg->getMaxShotpointIncrOnLine(index);
}


static long gp1_update(void *data, long /*ident*/, long index)
{
  FieldGeometry *fg = get_field_geometry(data);
#if(BOTH_GP)
  return fg->firstCumulativeGroundPosition(index);
#else
  return fg->firstGroundPosition(index);
#endif
}


static long gp2_update(void *data, long /*ident*/, long index)
{
  FieldGeometry *fg = get_field_geometry(data);
#if(BOTH_GP)
  return fg->firstMatchableGroundPosition(index);
#else
  long nflags = fg->numFlagsOnLine(index);
  if(nflags == 0) return INIL;
  return fg->firstGroundPosition(index) + nflags - 1;
#endif
}



//----------------- switch update functions ----------------------//
//----------------- switch update functions ----------------------//
//----------------- switch update functions ----------------------//

static long diamond_switch_update(void *data, long /*ident*/, long index)
{
  FieldGeometry *fg = get_field_geometry(data);
  long        n     = fg->numLines();
  if(index >= n) return -4;
  return 4;
}


static long select_switch_update(void *data, long /*ident*/, long index)
{
  FieldGeometry *fg = get_field_geometry(data);
  long        n     = fg->numLines();
  if(index >= n) return -2;
  if(fg->lineIsSelected(index)) return 6;
  return 2;
}


static long table_switch_update(void *data, long /*ident*/, long index)
{
  FieldGeometry *fg = get_field_geometry(data);
  long        n     = fg->numLines();
  if(index >= n) return -5;
  return 5;
}



//---------------------- make helper ------------------------//
//---------------------- make helper ------------------------//
//---------------------- make helper ------------------------//

void LinelistTableGui::makeHelper()
{
  static long zero =  0; 
  static long one  =  1; 
  static long two  =  2; 
  static long four =  4; 
  static long five =  5; 

    //              PROMPT                ROW COL
  wbox_mreg("active    select"          ,  0,  3);
  wbox_mreg("-------number of-------"   , -1, 21);
  wbox_mreg("---------shotpoint--------", -1, 46);
  wbox_mreg("shotpoint-step  "          , -1, 74);
#if(BOTH_GP)
  wbox_mreg("first-ground-position"     , -1, -1);
#else
  wbox_mreg("ground-position"           , -1, -1);
#endif

         //    N       NMAX       ROW COL NCHAR MAXROWS
  dbox_rega(n_update, nmax_update, 0,  0,   5,    35);

         //  ID    PROMPT     SWITCH   SWITCH  COL NCHAR NDEC
  dbox_irega(66, "|"         , &zero,  &four,   0,   2,   0);
  dbox_irega( 1, "line#  "   , &zero,  &one ,   0,   6,   0);
  dbox_crega(77, "| "        , &zero,  &two ,   0,   1,   0);
  dbox_irega( 2, "flags"     , &zero,  &five,   0,   7,   0);
  dbox_irega( 3, "sources"   , &zero,  &five,   0,   7,   0);
  dbox_irega( 4, "recvers "  , &zero,  &five,   0,   7,   0);
  dbox_frega( 5, "first"     , &zero,  &five,   0,   8,   3);
  dbox_frega( 6, "last"      , &zero,  &five,   0,   8,   3);
  dbox_frega( 7, "active   " , &zero,  &five,   0,   8,   3);
  dbox_frega( 8, "minimum"   , &zero,  &five,   0,   7,   3);
  dbox_frega( 9, "maximum "  , &zero,  &five,   0,   7,   3);
#if(BOTH_GP)
//dbox_irega(10, "cumulative", &zero,  &five,   0,  10,   0);
//dbox_irega(11, "matchable ", &zero,  &five,   0,  10,   0);
  dbox_irega(10, "fixdist<=0", &zero,  &five,   0,  10,   0);
  dbox_irega(11, "fixdist>0 ", &zero,  &five,   0,  10,   0);
#else
  dbox_irega(10, "first"     , &zero,  &five,   0,   7,   0);
  dbox_irega(11, "last"      , &zero,  &five,   0,   7,   0);
#endif

  dbox_set_itrap(66, diamond_trap);
  dbox_set_ifun (66, diamond_update);
  dbox_set_sfun (66, diamond_switch_update);
  dbox_set_itrap( 1, line_trap);
  dbox_set_ifun ( 1, line_update);
  dbox_set_ctrap(77, select_trap);
  dbox_set_cfun (77, select_update);
  dbox_set_sfun (77, select_switch_update);
  dbox_set_ifun ( 2, flags_update);
  dbox_set_sfun ( 2, table_switch_update);
  dbox_set_ifun ( 3, sources_update);
  dbox_set_sfun ( 3, table_switch_update);
  dbox_set_ifun ( 4, receivers_update);
  dbox_set_sfun ( 4, table_switch_update);
  dbox_set_ffun ( 5, first_sp_update);
  dbox_set_sfun ( 5, table_switch_update);
  dbox_set_ffun ( 6, last_sp_update);
  dbox_set_sfun ( 6, table_switch_update);
  dbox_set_ffun ( 7, active_sp_update);
  dbox_set_sfun ( 7, table_switch_update);
  dbox_set_ffun ( 8, min_incr_update);
  dbox_set_sfun ( 8, table_switch_update);
  dbox_set_ffun ( 9, max_incr_update);
  dbox_set_sfun ( 9, table_switch_update);
  dbox_set_ifun (10, gp1_update);
  dbox_set_sfun (10, table_switch_update);
  dbox_set_ifun (11, gp2_update);
  dbox_set_sfun (11, table_switch_update);
}



//-------------------------- end -------------------------------//
//-------------------------- end -------------------------------//
//-------------------------- end -------------------------------//
