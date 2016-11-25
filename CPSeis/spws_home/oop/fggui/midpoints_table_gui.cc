
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
//---------------------- midpoints_table_gui.cc -----------------------//
//---------------------- midpoints_table_gui.cc -----------------------//
//---------------------- midpoints_table_gui.cc -----------------------//

//        implementation file for the MidpointsTableGui class
//                derived from the SLDatabox class
//                       subdirectory fggui


#include "fggui/midpoints_table_gui.hh"
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


MidpointsTableGui::MidpointsTableGui(SLDelay *slparent, char *name,
                                   FieldGeometry *fg)
           : SLDatabox(slparent, name, NULL, 4),
             FgInform(fg)
{
  assert(fg);
}


//------------------------ destructor --------------------------//
//------------------------ destructor --------------------------//
//------------------------ destructor --------------------------//

MidpointsTableGui::~MidpointsTableGui()
{
}



//-------------------- post new active cmp -----------------------//
//-------------------- post new active cmp -----------------------//
//-------------------- post new active cmp -----------------------//

       // virtual function overriding FgInform.

void MidpointsTableGui::postNewActiveCmp(FieldGeometry *fg)
{
  void *box = getBox();
  long active = fg->getActiveCmpIndex();
  if(active >= 0) wbox_set_focus(box, 66, (int)active+1);
}



//------------------- get field geometry --------------------------//
//------------------- get field geometry --------------------------//
//------------------- get field geometry --------------------------//

static inline FieldGeometry *get_field_geometry(void *data)
{
  MidpointsTableGui *table = (MidpointsTableGui*)data;
  return table->getFieldGeometry();
}



//---------------------- traps ------------------------------//
//---------------------- traps ------------------------------//
//---------------------- traps ------------------------------//


static void diamond_trap(void *data, long /*ident*/, long index,
                         long /*value*/, long nread, char* /*endkey*/)
{
  if(nread == 0) return;
  MidpointsTableGui *table = (MidpointsTableGui*)data;
  FieldGeometry *fg = table->getFieldGeometry();
  if(index >= fg->numCmpGathers()) return;
  table->disableMessages();
  fg->setActiveCmpIndex(index);
  table->enableMessages();
}



static void select_trap(void *data, long /*ident*/, long index,
                        char* /*value*/, long /*nread*/, char* endkey)
{
  if(!strings_equal(endkey, "RETURN")) return;
  FieldGeometry *fg = get_field_geometry(data);
  if(index >= fg->numCmpGathers()) return;
  fg->incrementCmpSelectValue(index);
}



static void min_xgrid_trap(void *data, long /*ident*/, long /*index*/,
                           long value, long nread, char* /*endkey*/)
{
  if(nread == 0) return;
  if(value == INIL) return;
  FieldGeometry *fg = get_field_geometry(data);
  double xgridmin = fg->getMinimumXgridBinCenter();
  if(xgridmin == DNIL) return;
  double xstep = value - NearestInteger(xgridmin);
  fg->incrementGridCoords(xstep, 0.0);
}



static void min_ygrid_trap(void *data, long /*ident*/, long /*index*/,
                           long value, long nread, char* /*endkey*/)
{
  if(nread == 0) return;
  if(value == INIL) return;
  FieldGeometry *fg = get_field_geometry(data);
  double ygridmin = fg->getMinimumYgridBinCenter();
  if(ygridmin == DNIL) return;
  double ystep = value - NearestInteger(ygridmin);
  fg->incrementGridCoords(0.0, ystep);
}



static void max_xgrid_trap(void *data, long /*ident*/, long /*index*/,
                           long value, long nread, char* /*endkey*/)
{
  if(nread == 0) return;
  if(value == INIL) return;
  FieldGeometry *fg = get_field_geometry(data);
  double xgridmax = fg->getMaximumXgridBinCenter();
  if(xgridmax == DNIL) return;
  double xstep = value - NearestInteger(xgridmax);
  fg->incrementGridCoords(xstep, 0.0);
}



static void max_ygrid_trap(void *data, long /*ident*/, long /*index*/,
                           long value, long nread, char* /*endkey*/)
{
  if(nread == 0) return;
  if(value == INIL) return;
  FieldGeometry *fg = get_field_geometry(data);
  double ygridmax = fg->getMaximumYgridBinCenter();
  if(ygridmax == DNIL) return;
  double ystep = value - NearestInteger(ygridmax);
  fg->incrementGridCoords(0.0, ystep);
}



static void xgrid_trap(void *data, long /*ident*/, long index,
                         long value, long nread, char* /*endkey*/)
{
  if(nread == 0) return;
  if(value == INIL) return;
  FieldGeometry *fg = get_field_geometry(data);
  double xgrid, ygrid;
  fg->getCmpGridBinCenter(index, &xgrid, &ygrid);
  if(xgrid == DNIL) return;
  double xstep = value - NearestInteger(xgrid);
  fg->incrementGridCoords(xstep, 0.0);
}



static void ygrid_trap(void *data, long /*ident*/, long index,
                         long value, long nread, char* /*endkey*/)
{
  if(nread == 0) return;
  if(value == INIL) return;
  FieldGeometry *fg = get_field_geometry(data);
  double xgrid, ygrid;
  fg->getCmpGridBinCenter(index, &xgrid, &ygrid);
  if(ygrid == DNIL) return;
  double ystep = value - NearestInteger(ygrid);
  fg->incrementGridCoords(0.0, ystep);
}



//------------------------ update functions --------------------//
//------------------------ update functions --------------------//
//------------------------ update functions --------------------//


static long n_update(void *data)
{
  FieldGeometry *fg = get_field_geometry(data);
  return fg->numCmpGathers();
}



static long diamond_update(void *data, long /*ident*/, long index)
{
  FieldGeometry *fg = get_field_geometry(data);
  long active = fg->getActiveCmpIndex();
  return (active == index);
}


static char *select_update(void *data, long /*ident*/, long index)
{
  static char buffer[2];
  FieldGeometry *fg = get_field_geometry(data);
  buffer[0] = fg->getCmpSelectValue(index);
  buffer[1] = '\0';
  return buffer;
}


static long hwd3_update(void *data, long /*ident*/, long index)
{
  FieldGeometry *fg = get_field_geometry(data);
  return fg->headerWord3(index);
}


static double hwd37_update(void *data, long /*ident*/, long index)
{
  FieldGeometry *fg = get_field_geometry(data);
  long fold = fg->foldOfStack(index);
  if(fold == 0) return DNIL;
  long ixcmp = fg->originalTraceIndex(index, 0);
  fg->calculateHeaderWords(ixcmp + 1, TRUE);
  return fg->getHeaderWordValue(37);
}


static double hwd38_update(void *data, long /*ident*/, long index)
{
  FieldGeometry *fg = get_field_geometry(data);
  long fold = fg->foldOfStack(index);
  if(fold == 0) return DNIL;
  long ixcmp = fg->originalTraceIndex(index, 0);
  fg->calculateHeaderWords(ixcmp + 1, TRUE);
  return fg->getHeaderWordValue(38);
}


static long fold_update(void *data, long /*ident*/, long index)
{
  FieldGeometry *fg = get_field_geometry(data);
  return fg->foldOfStack(index);
}


static long live_update(void *data, long /*ident*/, long index)
{
  FieldGeometry *fg = get_field_geometry(data);
  if(index == 0 && fg->numUnplacedTraces() > 0) return INIL;
  return fg->liveFoldOfStack(index);
}


static long dead_update(void *data, long ident, long index)
{
  long fold = fold_update(data, ident, index);
  long live = live_update(data, ident, index);
  if(fold == INIL || live == INIL) return INIL;
  return (fold - live);
}


static float offmin_update(void *data, long /*ident*/, long index)
{
  FieldGeometry *fg = get_field_geometry(data);
  long nfold = fg->foldOfStack(index);
  if(nfold == 0) return FNIL;
  return fg->getCmpTraceOffset(index, 0);
}


static float offmax_update(void *data, long /*ident*/, long index)
{
  FieldGeometry *fg = get_field_geometry(data);
  long nfold = fg->foldOfStack(index);
  if(nfold == 0) return FNIL;
  return fg->getCmpTraceOffset(index, nfold-1);
}



static long fattest_update(void *data, long /*ident*/, long index)
{
  FieldGeometry *fg = get_field_geometry(data);
  long fattest = fg->cmpFattestBinette(index);
  if(fattest == 0) return INIL;
  return fattest;
}



static double xloc_update(void *data, long /*ident*/, long index)
{
  FieldGeometry *fg = get_field_geometry(data);
  double xloc, yloc;
  fg->getCmpLocBinCenter(index, &xloc, &yloc);
  return xloc;
}


static double yloc_update(void *data, long /*ident*/, long index)
{
  FieldGeometry *fg = get_field_geometry(data);
  double xloc, yloc;
  fg->getCmpLocBinCenter(index, &xloc, &yloc);
  return yloc;
}


static long xgrid_update(void *data, long /*ident*/, long index)
{
  FieldGeometry *fg = get_field_geometry(data);
  double xgrid, ygrid;
  fg->getCmpGridBinCenter(index, &xgrid, &ygrid);
  if(xgrid == DNIL) return INIL;
  return NearestInteger(xgrid);
}


static long ygrid_update(void *data, long /*ident*/, long index)
{
  FieldGeometry *fg = get_field_geometry(data);
  double xgrid, ygrid;
  fg->getCmpGridBinCenter(index, &xgrid, &ygrid);
  if(ygrid == DNIL) return INIL;
  return NearestInteger(ygrid);
}



//----------------- scalar min update functions ------------------//
//----------------- scalar min update functions ------------------//
//----------------- scalar min update functions ------------------//


static long min_fold_update(void *data, long, long)
{
  FieldGeometry *fg = get_field_geometry(data);
  return fg->getMinimumFold();
}


static float min_offmin_update(void *data, long, long)
{
  FieldGeometry *fg = get_field_geometry(data);
  return fg->getMinimumOffset();
}


static float min_offmax_update(void *data, long, long)
{
  FieldGeometry *fg = get_field_geometry(data);
  return fg->getMinimumOffset();
}


static double min_xloc_update(void *data, long, long)
{
  FieldGeometry *fg = get_field_geometry(data);
  return fg->getMinimumXlocBinCenter();
}


static double min_yloc_update(void *data, long, long)
{
  FieldGeometry *fg = get_field_geometry(data);
  return fg->getMinimumYlocBinCenter();
}


static long min_xgrid_update(void *data, long, long)
{
  FieldGeometry *fg = get_field_geometry(data);
  double xgrid = fg->getMinimumXgridBinCenter();
  if(xgrid == DNIL) return INIL;
  return NearestInteger(xgrid);
}


static long min_ygrid_update(void *data, long, long)
{
  FieldGeometry *fg = get_field_geometry(data);
  double ygrid = fg->getMinimumYgridBinCenter();
  if(ygrid == DNIL) return INIL;
  return NearestInteger(ygrid);
}



//----------------- scalar max update functions ------------------//
//----------------- scalar max update functions ------------------//
//----------------- scalar max update functions ------------------//


static long max_fold_update(void *data, long, long)
{
  FieldGeometry *fg = get_field_geometry(data);
  return fg->getMaximumFold();
}


static float max_offmin_update(void *data, long, long)
{
  FieldGeometry *fg = get_field_geometry(data);
  return fg->getMaximumOffset();
}


static float max_offmax_update(void *data, long, long)
{
  FieldGeometry *fg = get_field_geometry(data);
  return fg->getMaximumOffset();
}


static double max_xloc_update(void *data, long, long)
{
  FieldGeometry *fg = get_field_geometry(data);
  return fg->getMaximumXlocBinCenter();
}


static double max_yloc_update(void *data, long, long)
{
  FieldGeometry *fg = get_field_geometry(data);
  return fg->getMaximumYlocBinCenter();
}


static long max_xgrid_update(void *data, long, long)
{
  FieldGeometry *fg = get_field_geometry(data);
  double xgrid = fg->getMaximumXgridBinCenter();
  if(xgrid == DNIL) return INIL;
  return NearestInteger(xgrid);
}


static long max_ygrid_update(void *data, long, long)
{
  FieldGeometry *fg = get_field_geometry(data);
  double ygrid = fg->getMaximumYgridBinCenter();
  if(ygrid == DNIL) return INIL;
  return NearestInteger(ygrid);
}



//----------------- switch update functions ----------------------//
//----------------- switch update functions ----------------------//
//----------------- switch update functions ----------------------//


static long select_switch_update(void *data, long /*ident*/, long index)
{
  FieldGeometry *fg = get_field_geometry(data);
  long        n     = fg->numCmpGathers();
  if(index < n && fg->cmpIsSelected(index)) return 6;
  return 2;
}


static long minmax_grid_switch_update(void *data, long, long)
{
  FieldGeometry *fg = get_field_geometry(data);
  if(fg->dependentUpdatesFrozen  ()) return -1;
  if(fg->midpointGathersOutOfDate()) return -1;
  return 1;
}


static long grid_switch_update(void *data, long, long index)
{
  FieldGeometry *fg = get_field_geometry(data);
  if(fg->dependentUpdatesFrozen  ()) return -1;
  if(fg->midpointGathersOutOfDate()) return -1;
  if(index == 0 && fg->firstBinHasNilCoords()) return -1;
  return 1;
}



//---------------------- make helper ------------------------//
//---------------------- make helper ------------------------//
//---------------------- make helper ------------------------//

void MidpointsTableGui::makeHelper()
{
  static long zero =   0; 
  static long one  =   1; 
  static long two  =   2; 
  static long four =   4; 
  static long five =   5; 
  static long m5   =  -5; 
  static long m77  = -77; 

         // ID    PROMPT       SWITCH   ROW  COL     NCHAR NDEC
  wbox_mreg(    "minimum -> ",           0,   3+7+16  );
  dbox_ireg(11,                &m5  ,   -1,  16+7+16  ,   5,   0);
  dbox_freg(12,                &m5  ,   -1,  29+6+7+16,   5,   0);
  dbox_freg(13,                &m77 ,   -1,  -1       ,   5,   0);
  dbox_dreg(14,                &m5  ,   -1,  42+6+7+19,   7,   0);
  dbox_dreg(15,                &m5  ,   -1,  -1       ,   7,   0);
  dbox_ireg(16,                &one ,   -1,  59+6+7+19,   7,   0);
  dbox_ireg(17,                &one ,   -1,  -1       ,   7,   0);
  wbox_mreg(    "maximum -> ",           0,   3+7+16  );
  dbox_ireg(21,                &m5  ,   -1,  16+7+16  ,   5,   0);
  dbox_freg(22,                &m77 ,   -1,  29+6+7+16,   5,   0);
  dbox_freg(23,                &m5  ,   -1,  -1       ,   5,   0);
  dbox_dreg(24,                &m5  ,   -1,  42+6+7+19,   7,   0);
  dbox_dreg(25,                &m5  ,   -1,  -1       ,   7,   0);
  dbox_ireg(26,                &one ,   -1,  59+6+7+19,   7,   0);
  dbox_ireg(27,                &one ,   -1,  -1       ,   7,   0);

    //              PROMPT           ROW  COL
  wbox_mreg("    active select    ",  0,   1);
  wbox_mreg(" SP#    line#  "      , -1,  -1);
  wbox_mreg("------fold------- "   , -1,  -1);
  wbox_mreg("--offset--- "         , -1,  -1);
  wbox_mreg("   Xloc    Yloc  "    , -1,  -1);
  wbox_mreg("  Xgrid   Ygrid   "   , -1,  -1);

         //    N       NMAX    ROW COL NCHAR MAXROWS
  dbox_rega(n_update, n_update, 0,  0,   7,    35);

         //  ID    PROMPT     SWITCH   SWITCH  COL NCHAR NDEC
  dbox_irega(66, "|  "       , &zero,  &four,   0,   2,   0);
  dbox_crega(77, "| "        , &zero,  &two ,   0,   1,   0);
  dbox_irega(55, "hwd 3 "    , &zero,  &five,   0,   5,   0);
  dbox_drega(56, "hwd 37 "   , &zero,  &five,   0,   7,   3);
  dbox_drega(57, "hwd 38 "   , &zero,  &five,   0,   6,   0);
  dbox_irega( 1, "total"     , &zero,  &five,   0,   5,   0);
  dbox_irega(88, "live "     , &zero,  &five,   0,   5,   0);
  dbox_irega(89, "dead  "    , &zero,  &five,   0,   5,   0);
  dbox_frega( 2, " min "     , &zero,  &five,   0,   5,   0);
  dbox_frega( 3, " max  "    , &zero,  &five,   0,   5,   0);
  dbox_irega(33, "  "        , &zero,  &five,   0,   1,   0);
  dbox_drega( 4, "hwd 17"    , &zero,  &five,   0,   7,   0);
  dbox_drega( 5, "hwd 18  "  , &zero,  &five,   0,   7,   0);
  dbox_irega( 6, "hwd 7"     , &zero,  &one ,   0,   7,   0);
  dbox_irega( 7, "hwd 8"     , &zero,  &one ,   0,   7,   0);

  dbox_set_itrap(66, diamond_trap);
  dbox_set_ifun (66, diamond_update);
//dbox_set_sfun (66, diamond_switch_update);

  dbox_set_ctrap(77, select_trap);
  dbox_set_cfun (77, select_update);
  dbox_set_sfun (77, select_switch_update);

  dbox_set_ifun (55, hwd3_update);
  dbox_set_dfun (56, hwd37_update);
  dbox_set_dfun (57, hwd38_update);
  dbox_set_ifun ( 1, fold_update);
  dbox_set_ifun (88, live_update);
  dbox_set_ifun (89, dead_update);
  dbox_set_ffun ( 2, offmin_update);
  dbox_set_ffun ( 3, offmax_update);
  dbox_set_ifun (33, fattest_update);
  dbox_set_dfun ( 4, xloc_update);
  dbox_set_dfun ( 5, yloc_update);
  dbox_set_ifun ( 6, xgrid_update);
  dbox_set_ifun ( 7, ygrid_update);

  dbox_set_ifun (11, min_fold_update);
  dbox_set_ffun (12, min_offmin_update);
  dbox_set_ffun (13, min_offmax_update);
  dbox_set_dfun (14, min_xloc_update);
  dbox_set_dfun (15, min_yloc_update);
  dbox_set_ifun (16, min_xgrid_update);
  dbox_set_ifun (17, min_ygrid_update);

  dbox_set_sfun (16, minmax_grid_switch_update);
  dbox_set_sfun (17, minmax_grid_switch_update);
  dbox_set_sfun (26, minmax_grid_switch_update);   // new
  dbox_set_sfun (27, minmax_grid_switch_update);   // new
  dbox_set_sfun ( 6, grid_switch_update);      // new
  dbox_set_sfun ( 7, grid_switch_update);      // new

  dbox_set_itrap(16, min_xgrid_trap);
  dbox_set_itrap(17, min_ygrid_trap);
  dbox_set_itrap(26, max_xgrid_trap);   // new
  dbox_set_itrap(27, max_ygrid_trap);   // new
  dbox_set_itrap( 6, xgrid_trap);      // new
  dbox_set_itrap( 7, ygrid_trap);      // new

  dbox_set_ifun (21, max_fold_update);
  dbox_set_ffun (22, max_offmin_update);
  dbox_set_ffun (23, max_offmax_update);
  dbox_set_dfun (24, max_xloc_update);
  dbox_set_dfun (25, max_yloc_update);
  dbox_set_ifun (26, max_xgrid_update);
  dbox_set_ifun (27, max_ygrid_update);
}



//-------------------------- end -------------------------------//
//-------------------------- end -------------------------------//
//-------------------------- end -------------------------------//
