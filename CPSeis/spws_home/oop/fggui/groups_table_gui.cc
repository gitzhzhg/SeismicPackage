
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
//---------------------- groups_table_gui.cc -----------------------//
//---------------------- groups_table_gui.cc -----------------------//
//---------------------- groups_table_gui.cc -----------------------//

//         implementation file for the GroupsTableGui class
//                derived from the SLDatabox class
//                       subdirectory fggui


#include "fggui/groups_table_gui.hh"
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



//---------------------- constructor -----------------------//
//---------------------- constructor -----------------------//
//---------------------- constructor -----------------------//


GroupsTableGui::GroupsTableGui(SLDelay *slparent, char *name,
                                   FieldGeometry *fg)
           : SLDatabox(slparent, name, NULL, 4),
             FgInform(fg)
{
  assert(fg);
}


//------------------------ destructor --------------------------//
//------------------------ destructor --------------------------//
//------------------------ destructor --------------------------//

GroupsTableGui::~GroupsTableGui()
{
}



//-------------------- post new active group -----------------------//
//-------------------- post new active group -----------------------//
//-------------------- post new active group -----------------------//

       // virtual function overriding FgInform.

void GroupsTableGui::postNewActiveGroup(FieldGeometry *fg)
{
  void *box = getBox();
  long active = fg->getActiveGroupNumber();
  if(active >= 1) wbox_set_focus(box, 66, (int)active);
}



//------------------- get field geometry --------------------------//
//------------------- get field geometry --------------------------//
//------------------- get field geometry --------------------------//

static inline FieldGeometry *get_field_geometry(void *data)
{
  GroupsTableGui *table = (GroupsTableGui*)data;
  return table->getFieldGeometry();
}



//---------------------- traps ------------------------------//
//---------------------- traps ------------------------------//
//---------------------- traps ------------------------------//

static void diamond_trap(void *data, long /*ident*/, long index,
                         long /*value*/, long nread, char* /*endkey*/)
{
  if(nread == 0) return;
  GroupsTableGui *table = (GroupsTableGui*)data;
  FieldGeometry *fg = table->getFieldGeometry();
  if(index >= fg->numGroups()) return;
  table->disableMessages();
  fg->setActiveGroupNumberPlus(index+1);
  table->enableMessages();
}



/****
static void select_trap(void *data, long / *ident* /, long index,
                        char* / *value* /, long / *nread* /, char* endkey)
{
  if(!strings_equal(endkey, "RETURN")) return;
  FieldGeometry *fg = get_field_geometry(data);
  if(index >= fg->numGroups()) return;
  fg->incrementCmpSelectValue(index);
}
****/



//------------------------ update functions --------------------//
//------------------------ update functions --------------------//
//------------------------ update functions --------------------//


static long n_update(void *data)
{
  FieldGeometry *fg = get_field_geometry(data);
  return fg->numGroups();
}



static long diamond_update(void *data, long /*ident*/, long index)
{
  FieldGeometry *fg = get_field_geometry(data);
  long active = fg->getActiveGroupNumber();
  return (active == index+1);
}


/****
static char *select_update(void *data, long / *ident* /, long index)
{
  static char buffer[2];
  FieldGeometry *fg = get_field_geometry(data);
  buffer[0] = fg->getCmpSelectValue(index);
  buffer[1] = '\0';
  return buffer;
}
****/



static float source_sp_update(void *data, long /*ident*/, long index)
{
  FieldGeometry *fg = get_field_geometry(data);
  long ixl = fg->getSourceLineIndex(index+1);
  long ixf = fg->getSourceFlagIndex(index+1);
  if(ixl == -1) return FNIL;
  if(ixf == -1) return FNIL;
  return fg->getShotpoint(ixl, ixf);
}


static long source_line_update(void *data, long /*ident*/, long index)
{
  FieldGeometry *fg = get_field_geometry(data);
  long ixl = fg->getSourceLineIndex(index+1);
  if(ixl == -1) return INIL;
  return fg->getLineNumber(ixl);
}



static float rec1_sp_update(void *data, long /*ident*/, long index)
{
  FieldGeometry *fg = get_field_geometry(data);
  long ixl = fg->getReceiverLineIndex(index+1, 1);
  long ixf = fg->getReceiverFlagIndex(index+1, 1);
  if(ixl == -1) return FNIL;
  if(ixf == -1) return FNIL;
  return fg->getShotpoint(ixl, ixf);
}


static long rec1_line_update(void *data, long /*ident*/, long index)
{
  FieldGeometry *fg = get_field_geometry(data);
  long ixl = fg->getReceiverLineIndex(index+1, 1);
  if(ixl == -1) return INIL;
  return fg->getLineNumber(ixl);
}



static long nchan_update(void *data, long /*ident*/, long index)
{
  FieldGeometry *fg = get_field_geometry(data);
  return fg->findNumChannelsInGroup(index+1);
}



static long unplaced_update(void *data, long /*ident*/, long index)
{
  FieldGeometry *fg = get_field_geometry(data);
  return fg->numUnplacedTraces(index+1);
}



static char *dead_update(void *data, long /*ident*/, long index)
{
  static char buffer[2];
  FieldGeometry *fg = get_field_geometry(data);
  int partly = fg->groupPartlyDead(index+1);
  buffer[0] = ' ';
  buffer[1] = '\0';
  if(partly) buffer[0] = '*';
  return buffer;
}



static char *missing_update(void *data, long /*ident*/, long index)
{
  static char buffer[20];
  strcpy(buffer, "  rp  ");
  FieldGeometry *fg = get_field_geometry(data);
  long ixf_source = fg->getSourceFlagIndex(index+1);
  long ixl_source = fg->getSourceLineIndex(index+1);
  long ixf_rec    = fg->getReceiverFlagIndex(index+1, 1);
  long ixl_rec    = fg->getReceiverLineIndex(index+1, 1);
  if(ixf_source == -1) buffer[0] = 'F';
  if(ixl_source == -1) buffer[1] = 'L';
  if(ixf_rec    == -1) buffer[4] = 'F';
  if(ixl_rec    == -1) buffer[5] = 'L';
  long ixpp = fg->findPpCardWithDesiredGroup(index+1);
  if(ixpp == -1) return buffer;
  long pattern = fg->getPatternNumber(ixpp);
  long ixrp    = fg->findReceiverPattern(pattern);
  if(ixrp == -1) return buffer;
  buffer[2] = ' ';
  buffer[3] = ' ';
  return buffer;
}



static float incr_source_sp_update(void *data, long ident, long index)
{
  FieldGeometry *fg = get_field_geometry(data);
  if(index+1 >= fg->numGroups()) return FNIL;
  float sp1 = source_sp_update(data, ident, index);
  float sp2 = source_sp_update(data, ident, index+1);
  if(sp1 == FNIL || sp2 == FNIL) return FNIL;
  return (sp2 - sp1);
}


static long incr_source_line_update(void *data, long ident, long index)
{
  FieldGeometry *fg = get_field_geometry(data);
  if(index+1 >= fg->numGroups()) return INIL;
  long line1 = source_line_update(data, ident, index);
  long line2 = source_line_update(data, ident, index+1);
  if(line1 == INIL || line2 == INIL) return INIL;
  return (line2 - line1);
}



static float incr_rec1_sp_update(void *data, long ident, long index)
{
  FieldGeometry *fg = get_field_geometry(data);
  if(index+1 >= fg->numGroups()) return FNIL;
  float sp1 = rec1_sp_update(data, ident, index);
  float sp2 = rec1_sp_update(data, ident, index+1);
  if(sp1 == FNIL || sp2 == FNIL) return FNIL;
  return (sp2 - sp1);
}


static long incr_rec1_line_update(void *data, long ident, long index)
{
  FieldGeometry *fg = get_field_geometry(data);
  if(index+1 >= fg->numGroups()) return INIL;
  long line1 = rec1_line_update(data, ident, index);
  long line2 = rec1_line_update(data, ident, index+1);
  if(line1 == INIL || line2 == INIL) return INIL;
  return (line2 - line1);
}



static float incr_sp_update(void *data, long ident, long index)
{
  FieldGeometry *fg = get_field_geometry(data);
  float sp1 = source_sp_update(data, ident, index);
  float sp2 =   rec1_sp_update(data, ident, index);
  if(sp1 == FNIL || sp2 == FNIL) return FNIL;
  return (sp2 - sp1);
}


static long incr_line_update(void *data, long ident, long index)
{
  FieldGeometry *fg = get_field_geometry(data);
  long line1 = source_line_update(data, ident, index);
  long line2 =   rec1_line_update(data, ident, index);
  if(line1 == INIL || line2 == INIL) return INIL;
  return (line2 - line1);
}



//----------------- switch update functions ----------------------//
//----------------- switch update functions ----------------------//
//----------------- switch update functions ----------------------//


/****
static long select_switch_update(void *data, long / *ident* /, long index)
{
  FieldGeometry *fg = get_field_geometry(data);
  long        n     = fg->numGroups();
  if(index < n && fg->groupIsSelected(index+1)) return 6;
  return 2;
}
****/


/****
static long incr_switch_update(void *data, long / *ident* /, long index)
{
  FieldGeometry *fg = get_field_geometry(data);
  long        n     = fg->numGroups();
  if(index < n-1) return 5;
  return -5;
}
****/



//---------------------- make helper ------------------------//
//---------------------- make helper ------------------------//
//---------------------- make helper ------------------------//

void GroupsTableGui::makeHelper()
{
  static long zero =   0; 
//static long two  =   2; 
  static long four =   4; 
  static long five =   5; 

    //              PROMPT                    ROW  COL
  wbox_mreg("dead"                          ,  0,  57);
  wbox_mreg("   increments to next group  " , -1,  69);
  wbox_mreg("  --increments-- "             , -1,  -1);
  wbox_mreg("    active "                   ,  0,   1);
//wbox_mreg("    active select "            ,  0,   1);
  wbox_mreg("----SOURCE---- "               , -1,  -1);
  wbox_mreg("---RECEIVER1-- "               , -1,  -1);
  wbox_mreg("   #unplaced | "               , -1,  -1);
  wbox_mreg("MISSING"                       , -1,  -1);
  wbox_mreg("----source---- "               , -1,  -1);
  wbox_mreg("---receiver1-- "               , -1,  -1);
  wbox_mreg("source to rec1 "               , -1,  -1);

         //    N       NMAX    ROW COL NCHAR MAXROWS
  dbox_rega(n_update, n_update, 0,  0,   7,    35);

         //  ID    PROMPT     SWITCH   SWITCH  COL NCHAR NDEC
  dbox_irega(66, "|  "       , &zero,  &four,   0,   2,   0);
//dbox_crega(77, "| "        , &zero,  &two ,   0,   1,   0);
  dbox_frega( 1, "  SP#  "   , &zero,  &five,   0,   7,   3);
  dbox_irega( 2, "line#  "   , &zero,  &five,   0,   6,   0);
  dbox_frega( 3, "  SP#  "   , &zero,  &five,   0,   7,   3);
  dbox_irega( 4, "line#  "   , &zero,  &five,   0,   6,   0);
  dbox_irega(21, "#CHAN"     , &zero,  &five,   0,   5,   0);
  dbox_irega(22, "traces"    , &zero,  &five,   0,   5,   0);
  dbox_crega(23, "| "        , &zero,  &five,   0,   1,   0);
  dbox_crega(88, "S-RP-R "   , &zero,  &five,   0,   6,   0);
  dbox_frega( 5, "  SP   "   , &zero,  &five,   0,   7,   3);
  dbox_irega( 6, "line   "   , &zero,  &five,   0,   6,   0);
  dbox_frega( 7, "  SP   "   , &zero,  &five,   0,   7,   3);
  dbox_irega( 8, "line   "   , &zero,  &five,   0,   6,   0);
  dbox_frega( 9, "  SP   "   , &zero,  &five,   0,   7,   3);
  dbox_irega(10, "line   "   , &zero,  &five,   0,   6,   0);


  dbox_set_itrap(66, diamond_trap);
  dbox_set_ifun (66, diamond_update);

//dbox_set_ctrap(77, select_trap);
//dbox_set_cfun (77, select_update);
//dbox_set_sfun (77, select_switch_update);

  dbox_set_ffun ( 1, source_sp_update);
  dbox_set_ifun ( 2, source_line_update);
  dbox_set_ffun ( 3, rec1_sp_update);
  dbox_set_ifun ( 4, rec1_line_update);
  dbox_set_ifun (21, nchan_update);
  dbox_set_ifun (22, unplaced_update);
  dbox_set_cfun (23, dead_update);
  dbox_set_cfun (88, missing_update);
  dbox_set_ffun ( 5, incr_source_sp_update);
  dbox_set_ifun ( 6, incr_source_line_update);
  dbox_set_ffun ( 7, incr_rec1_sp_update);
  dbox_set_ifun ( 8, incr_rec1_line_update);
  dbox_set_ffun ( 9, incr_sp_update);
  dbox_set_ifun (10, incr_line_update);

/****
  dbox_set_sfun ( 5, incr_switch_update);
  dbox_set_sfun ( 6, incr_switch_update);
  dbox_set_sfun ( 7, incr_switch_update);
  dbox_set_sfun ( 8, incr_switch_update);
****/
}



//-------------------------- end -------------------------------//
//-------------------------- end -------------------------------//
//-------------------------- end -------------------------------//
