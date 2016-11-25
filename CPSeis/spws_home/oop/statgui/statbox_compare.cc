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

//---------------------- statbox_compare.cc -----------------------//
//---------------------- statbox_compare.cc -----------------------//
//---------------------- statbox_compare.cc -----------------------//

//         implementation file for the StatboxCompare class
//                derived from the SLDatabox class
//                     subdirectory statgui


#include "statgui/statbox_compare.hh"
#include "statgui/statgui_compare.hh"
#include "stat/static_manager.hh"
#include "stat/static_dataset.hh"
#include "sl/sl_prim.hh"
#include "named_constants.h"
#include <iostream.h>
#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <assert.h>


#define TABLE   StatboxCompare *table  = (StatboxCompare*)data;
#define DATASET StaticDataset *dataset = table->manager()->activeDataset();
#define REF     StaticDataset *ref     = table->manager()->referenceDataset();

#define NCHAR 27

enum { SHOW_ONE = 51, SHOW_EACH, SHOW_SUM, SHOW_AV, SHOW_DIFF,
       SHOW_WEIGHT };

//---------------------- constructor -----------------------//
//---------------------- constructor -----------------------//
//---------------------- constructor -----------------------//


StatboxCompare::StatboxCompare(SLDelay *slparent, StaticManager *manager)
           : SLDatabox(slparent, "statbox_compare", NULL, 4),
                _manager         (manager),
                _gui             (NULL),
                _interp          (StaticDataset::INTERP_NEAR),
                _extrap          (StaticDataset::EXTRAP_NILS),
                _show            (SHOW_ONE),
                _xdist           (10),
                _ydist           (10)
{
}



//------------------------ destructor --------------------------//
//------------------------ destructor --------------------------//
//------------------------ destructor --------------------------//

StatboxCompare::~StatboxCompare()
{
}



//--------------------------- scroll table ------------------------//
//--------------------------- scroll table ------------------------//
//--------------------------- scroll table ------------------------//

       // public.

void StatboxCompare::scrollTable()
{
  StaticDataset *dataset = _manager->activeDataset();
  long nx = dataset->getNx();
  long ny = dataset->getNy();
  long ix = _gui->getChosenXindex();
  long iy = _gui->getChosenYindex();
  int direction = _gui->getDirection();
  long index;
  switch(direction)
     {
     case StatguiCompare::XDIRECTION    : index = ix          ; break;
     case StatguiCompare::YDIRECTION    : index = iy          ; break;
     case StatguiCompare::XDIRECTION_ALL: index = ix + nx * iy; break;
     case StatguiCompare::YDIRECTION_ALL: index = iy + ny * ix; break;
     default                            : assert(FALSE);
     }
  setFocus(2, (int)index);
}



//---------------------- get indices ---------------------------//
//---------------------- get indices ---------------------------//
//---------------------- get indices ---------------------------//

          // public.

long StatboxCompare::getXindex(long index)  const
{
  StaticDataset *dataset = _manager->activeDataset();
  long nx = dataset->getNx();
  long ny = dataset->getNy();
  long ix_chosen = _gui->getChosenXindex();
  int direction = _gui->getDirection();
  long ix;
  switch(direction)
     {
     case StatguiCompare::XDIRECTION    : ix = index;                     break;
     case StatguiCompare::YDIRECTION    : ix = ix_chosen;                 break;
     case StatguiCompare::XDIRECTION_ALL: ix = index - nx * (index / nx); break;
     case StatguiCompare::YDIRECTION_ALL: ix = index / ny;                break;
     default                            : assert(FALSE);
     }
  return ix;
}


long StatboxCompare::getYindex(long index)  const
{
  StaticDataset *dataset = _manager->activeDataset();
  long nx = dataset->getNx();
  long ny = dataset->getNy();
  long iy_chosen = _gui->getChosenYindex();
  int direction = _gui->getDirection();
  long iy;
  switch(direction)
     {
     case StatguiCompare::XDIRECTION    : iy = iy_chosen;                 break;
     case StatguiCompare::YDIRECTION    : iy = index;                     break;
     case StatguiCompare::XDIRECTION_ALL: iy = index / nx;                break;
     case StatguiCompare::YDIRECTION_ALL: iy = index - ny * (index / ny); break;
     default                            : assert(FALSE);
     }
  return iy;
}



//------------------------ step values ------------------------------//
//------------------------ step values ------------------------------//
//------------------------ step values ------------------------------//

       // public.

void StatboxCompare::stepShow(int step)
{
  _show += step;
  if     (_show > SHOW_WEIGHT) _show = SHOW_ONE;
  else if(_show < SHOW_ONE   ) _show = SHOW_WEIGHT;
}


void StatboxCompare::stepInterp()
{
  if(_interp == StaticDataset::INTERP_NEAR)
       _interp = StaticDataset::INTERP_TERP;
  else _interp = StaticDataset::INTERP_NEAR;
}


void StatboxCompare::stepExtrap()
{
  if(_extrap == StaticDataset::EXTRAP_EDGE)
       _extrap = StaticDataset::EXTRAP_NILS;
  else if(_extrap == StaticDataset::EXTRAP_NILS)
       _extrap = StaticDataset::EXTRAP_ZERO;
  else _extrap = StaticDataset::EXTRAP_EDGE;
}



//-------------------- get combined value -------------------------//
//-------------------- get combined value -------------------------//
//-------------------- get combined value -------------------------//


static float get_combined_value(int show, float value1, float value2)
{
  if(value1 == FNIL && value2 == FNIL) return FNIL;
  float value;
  switch(show)
     {
     case SHOW_SUM    : if     (value1 == FNIL) value = 2.0 * value2;
                        else if(value2 == FNIL) value = 2.0 * value1;
                        else                    value = value1 + value2;
                        break;
     case SHOW_AV     : if     (value1 == FNIL) value = value2;
                        else if(value2 == FNIL) value = value1;
                        else                 value = 0.5 * (value1 + value2);
                        break;
     case SHOW_DIFF   : if     (value1 == FNIL) value = FNIL;
                        else if(value2 == FNIL) value = FNIL;
                        else                    value = value1 - value2;
                        break;
     default: assert(FALSE);
     }
  return value;
}



//-------------------------- traps ------------------------------//
//-------------------------- traps ------------------------------//
//-------------------------- traps ------------------------------//

static void xdist_trap(void *data, long /*ident*/, long /*index*/,
                            long value, long nread, char* /*endkey*/)
{
  if(nread == 0) return;
  TABLE
  table->setXdist(value);
}


static void ydist_trap(void *data, long /*ident*/, long /*index*/,
                            long value, long nread, char* /*endkey*/)
{
  if(nread == 0) return;
  TABLE
  table->setYdist(value);
}


static void interp_trap(void *data, long /*ident*/, long /*index*/,
                        char* /*value*/, long /*nread*/, char* endkey)
{
  TABLE
  if     (strcmp(endkey, "RETURN" ) == 0) table->stepInterp();
  else if(strcmp(endkey, "BUTTON2") == 0) table->stepInterp();
}



static void extrap_trap(void *data, long /*ident*/, long /*index*/,
                        char* /*value*/, long /*nread*/, char* endkey)
{
  TABLE
  if     (strcmp(endkey, "RETURN" ) == 0) table->stepExtrap();
  else if(strcmp(endkey, "BUTTON2") == 0) table->stepExtrap();
}



static void prompt_trap(void *data, long /*ident*/, long /*index*/,
                            char* /*value*/, long /*nread*/, char* endkey)
{
  TABLE
  if     (strcmp(endkey, "RETURN" ) == 0) table->stepShow( 1);
  else if(strcmp(endkey, "BUTTON2") == 0) table->stepShow(-1);
}



static void select_trap(void *data, long /*ident*/, long /*index*/,
                            char* /*value*/, long /*nread*/, char* endkey)
{
  if(strcmp(endkey, "RETURN") != 0) return;
  TABLE
  DATASET
  if(dataset->isSelected(0, 0)) dataset->clearSelections();
  else dataset->setSelections(0, 0, dataset->getNx(), dataset->getNy(), TRUE);
}



static void xactive_trap(void *data, long /*ident*/, long index,
                            long /*value*/, long nread, char* /*endkey*/)
{
  if(nread == 0) return;
  TABLE
  DATASET
  long ix = table->getXindex(index);
  StatguiCompare::freezeChosenIndices();
  dataset->setActiveIx(ix);
  StatguiCompare::unfreezeChosenIndices();
}



static void yactive_trap(void *data, long /*ident*/, long index,
                            long /*value*/, long nread, char* /*endkey*/)
{
  if(nread == 0) return;
  TABLE
  DATASET
  long iy = table->getYindex(index);
  StatguiCompare::freezeChosenIndices();
  dataset->setActiveIy(iy);
  StatguiCompare::unfreezeChosenIndices();
}



static void select_trap(void *data, long /*ident*/, long index,
                            long value, long nread, char* /*endkey*/)
{
  if(nread == 0) return;
  TABLE
  DATASET
  long ix = table->getXindex(index);
  long iy = table->getYindex(index);
  dataset->setSelections(ix, iy, 1, 1, (int)value);
}



static void value_trap(void *data, long /*ident*/, long index,
                            float value, long nread, char* /*endkey*/)
{
  if(nread == 0) return;
  TABLE
  DATASET
  long ix = table->getXindex(index);
  long iy = table->getYindex(index);
  dataset->setValue(ix, iy, value);
}



static void show_trap(void *data, long ident, long /*index*/,
                            char* /*value*/, long /*nread*/, char* endkey)
{
  if(strcmp(endkey, "RETURN") != 0) return;
  TABLE
  table->setShow((int)ident);
}



//---------------- variable update functions --------------------//
//---------------- variable update functions --------------------//
//---------------- variable update functions --------------------//


static long xdist_update(void *data, long /*ident*/, long /*index*/)
{
  TABLE
  return table->getXdist();
}


static long ydist_update(void *data, long /*ident*/, long /*index*/)
{
  TABLE
  return table->getYdist();
}


static char *interp_update(void *data, long /*ident*/, long /*index*/)
{
  TABLE
  static char *buffer1 = "use nearest values";
  static char *buffer2 = "interpolate among non-nil values";
  if(table->getInterp() == StaticDataset::INTERP_NEAR) return buffer1;
  return buffer2;
}


static char *extrap_update(void *data, long /*ident*/, long /*index*/)
{
  TABLE
  static char *buffer1 = "extrapolate with edge value";
  static char *buffer2 = "extrapolate with nils";
  static char *buffer3 = "extrapolate with zeros";
  if(table->getExtrap() == StaticDataset::EXTRAP_EDGE) return buffer1;
  if(table->getExtrap() == StaticDataset::EXTRAP_NILS) return buffer2;
  return buffer3;
}


static char *prompt_update(void *data, long /*ident*/, long /*index*/)
{
  TABLE
  static char *buffer0 = "    xxxxxxxxxxxxxxxxxxx";
  static char *buffer1 = "         plot sum";
  static char *buffer2 = "       plot average";
  static char *buffer3 = "      plot difference";
  static char *buffer4 = "      plot two values";
  static char *buffer5 = "      plot one value";
  static char *buffer6 = " plot weight for splicing";
  if(table->getShow() == SHOW_SUM    ) return buffer1;
  if(table->getShow() == SHOW_AV     ) return buffer2;
  if(table->getShow() == SHOW_DIFF   ) return buffer3;
  if(table->getShow() == SHOW_EACH   ) return buffer4;
  if(table->getShow() == SHOW_ONE    ) return buffer5;
  if(table->getShow() == SHOW_WEIGHT ) return buffer6;
  return buffer0;
}


static char *which_update(void *data, long /*ident*/, long /*index*/)
{
  TABLE
  static char *buffer1 = " ";
  static char *buffer2 = "sum";
  static char *buffer3 = "average";
  static char *buffer4 = "difference";
  static char *buffer5 = "weight";
  if(table->getShow() == SHOW_SUM    ) return buffer2;
  if(table->getShow() == SHOW_AV     ) return buffer3;
  if(table->getShow() == SHOW_DIFF   ) return buffer4;
  if(table->getShow() == SHOW_WEIGHT ) return buffer5;
  return buffer1;
}


static long xactive_update(void *data, long /*ident*/, long index)
{
  TABLE
  DATASET
  long ix = table->getXindex(index);
  return (dataset->getActiveIx() == ix);
}


static long yactive_update(void *data, long /*ident*/, long index)
{
  TABLE
  DATASET
  long iy = table->getYindex(index);
  return (dataset->getActiveIy() == iy);
}


static long select_update(void *data, long /*ident*/, long index)
{
  TABLE
  DATASET
  long ix = table->getXindex(index);
  long iy = table->getYindex(index);
  return dataset->isSelected(ix, iy);
}


static float xbin_update(void *data, long /*ident*/, long index)
{
  TABLE
  DATASET
  long ix = table->getXindex(index);
  return dataset->getXbin(ix);
}


static float ybin_update(void *data, long /*ident*/, long index)
{
  TABLE
  DATASET
  long iy = table->getYindex(index);
  return dataset->getYbin(iy);
}


static float value_update(void *data, long /*ident*/, long index)
{
  TABLE
  DATASET
  long ix = table->getXindex(index);
  long iy = table->getYindex(index);
  return dataset->getValue(ix, iy);
}


static float reference_update(void *data, long /*ident*/, long index)
{
  TABLE
  if(table->getShow() == SHOW_ONE   ) return FNIL;
  if(table->getShow() == SHOW_WEIGHT) return FNIL;
  DATASET
  REF
  long  ix   = table->getXindex(index);
  long  iy   = table->getYindex(index);
  float xbin = dataset->getXbin(ix);
  float ybin = dataset->getYbin(iy);
  int interp = (int)table->getInterp();
  int extrap = (int)table->getExtrap();
  return ref->getResampledValue(xbin, ybin, interp, extrap);
}


static float combined_update(void *data, long /*ident*/, long index)
{
  TABLE
  if(table->getShow() == SHOW_ONE ) return FNIL;
  if(table->getShow() == SHOW_EACH) return FNIL;
  DATASET
  long  ix     = table->getXindex(index);
  long  iy     = table->getYindex(index);
  if(table->getShow() == SHOW_WEIGHT)
      return dataset->getWeight(ix, iy, table->getXdist(), table->getYdist());
  REF
  float value1 = dataset->getValue(ix, iy);
  float xbin   = dataset->getXbin(ix);
  float ybin   = dataset->getYbin(iy);
  int   interp = (int)table->getInterp();
  int   extrap = (int)table->getExtrap();
  float value2 =  ref->getResampledValue(xbin, ybin, interp, extrap);
  return get_combined_value(table->getShow(), value1, value2);
}


static long n_update(void *data)
{
  TABLE
  DATASET
  StatguiCompare *gui = table->getStatguiCompare();
  int       direction = gui->getDirection();
  long n;
  switch(direction)
     {
     case StatguiCompare::XDIRECTION    :
                             n = dataset->getNx()                   ; break;
     case StatguiCompare::YDIRECTION    :
                             n = dataset->getNy()                   ; break;
     case StatguiCompare::XDIRECTION_ALL:
                             n = dataset->getNx() * dataset->getNy(); break;
     case StatguiCompare::YDIRECTION_ALL:
                             n = dataset->getNx() * dataset->getNy(); break;
     default: assert(FALSE);
     }
  return n;
}



//------------------ plot update functions -----------------------//
//------------------ plot update functions -----------------------//
//------------------ plot update functions -----------------------//

static char *plot_update(void *data, long /*ident*/, long index)
{
  TABLE
  DATASET
  long  ix     = table->getXindex(index);
  long  iy     = table->getYindex(index);
  if(table->getShow() == SHOW_WEIGHT)
      {
      float value = dataset->getWeight
                              (ix, iy, table->getXdist(), table->getYdist());
      return SLDatabox::histogramBar(value);
      }
  float value1 = dataset->getValue(ix, iy);
  if(table->getShow() == SHOW_ONE) return SLDatabox::histogramBar(value1);
  REF
  float xbin   = dataset->getXbin(ix);
  float ybin   = dataset->getYbin(iy);
  int   interp = (int)table->getInterp();
  int   extrap = (int)table->getExtrap();
  float value2 = ref->getResampledValue(xbin, ybin, interp, extrap);
  if(table->getShow() == SHOW_EACH)
         return SLDatabox::histogramBar2(value1, value2);
  float value = get_combined_value(table->getShow(), value1, value2);
  return SLDatabox::histogramBar(value);
}


static char *head_update(void *data, long /*ident*/, long /*index*/)
{
  TABLE
  if(table->getShow() == SHOW_WEIGHT)
              return SLDatabox::histogramHeader(0.0, 1.0, NCHAR);
  DATASET
  float vmin1 = dataset->minimumValue();
  float vmax1 = dataset->maximumValue();
  if(table->getShow() == SHOW_ONE)
              return SLDatabox::histogramHeader(vmin1, vmax1, NCHAR);
  REF
  long  live1 = dataset->numLiveValues();
  float vmin2 = ref    ->minimumValue();
  float vmax2 = ref    ->maximumValue();
  long  live2 = ref    ->numLiveValues();
  float vmin  = 0.0;
  float vmax  = 0.0;
  if(live1 == 0 && live2 == 0)
                      return SLDatabox::histogramHeader(vmin, vmax, NCHAR);
  if(table->getShow() == SHOW_DIFF)
      {
      if(live1 == 0 || live2 == 0)
                          return SLDatabox::histogramHeader(vmin, vmax, NCHAR);
      return SLDatabox::histogramHeader(vmin1 - vmax2, vmax1 - vmin2, NCHAR);
      }
  if(live1 == 0) return SLDatabox::histogramHeader(vmin2, vmax2, NCHAR);
  if(live2 == 0) return SLDatabox::histogramHeader(vmin1, vmax1, NCHAR);
  if(table->getShow() == SHOW_SUM)
      {
      return SLDatabox::histogramHeader(vmin1 + vmin2, vmax1 + vmax2, NCHAR);
      }
  if(table->getShow() == SHOW_AV)
      {
      vmin = 0.5 * (vmin1 + vmin2);
      vmax = 0.5 * (vmax1 + vmax2);
      return SLDatabox::histogramHeader(vmin, vmax, NCHAR);
      }
  vmin = MinimumValue(vmin1, vmin2);
  vmax = MaximumValue(vmax1, vmax2);
  return SLDatabox::histogramHeader(vmin, vmax, NCHAR);
}



//---------------- switch update functions --------------------//
//---------------- switch update functions --------------------//
//---------------- switch update functions --------------------//


static long dist_switch_update(void *data, long ident, long /*index*/)
{
  TABLE
  if(table->getShow() != SHOW_WEIGHT) return -77;
  if(ident < 0) return 0;
  return 1;
}


static long ref_switch_update(void *data, long ident, long /*index*/)
{
  TABLE
  if(table->getShow() == SHOW_ONE ||
     table->getShow() == SHOW_WEIGHT) return -77;
  if(ident < 0 || ident == 77) return 0;
  return 2;
}


static long value_switch_update(void *data, long /*ident*/, long /*index*/)
{
  TABLE
  DATASET
  if(dataset->isLocked()) return 5;
  return 1;
}


static long show_switch_update(void *data, long ident, long /*index*/)
{
  TABLE
  int show = table->getShow();
  if(ident == show) return 6;
  return 2;
}



//---------------------- make helper ------------------------//
//---------------------- make helper ------------------------//
//---------------------- make helper ------------------------//

void StatboxCompare::makeHelper()
{
  static long zero  =   0; 
  static long one   =   1; 
  static long two   =   2; 
  static long three =   3; 
  static long four  =   4; 
  static long five  =   5;
  static long p11   =  11;
  static long m5    =  -5;
  static long m25   = -25;
  static long m66   = -66;

        //                                  SWITCH         ROW COL
  regCvar2  (41, "interpolation to get reference value:",
                                    &zero,  &two,           1,  1, 32);
  regCvar2  (42, "extrapolation to get reference value:",
                                    &zero,  &two,           2,  1, 32);
  regIvar2  (31, "xdist",      &zero,       &one,           1, 75, 3);
  regIvar2  (32, "ydist",      &zero,       &one,           1, 86, 3);
  regString (SHOW_ONE   , " 1 "           , &two ,          3, 74);
  regString (SHOW_EACH  , " 2 "           , &two ,          3, -1);
  regString (SHOW_SUM   , "sum"           , &two ,          3, -1);
  regString (SHOW_AV    , "av"            , &two ,          3, -1);
  regString (SHOW_DIFF  , "diff"          , &two ,          3, -1);
  regString (SHOW_WEIGHT, " w "           , &two ,          3, -1);
  regMsg    ("       active        active",                 4,  1);
  regString (88, "select",                  &two,           4, 36);
  regString (77, "reference",               &zero,          4, 50);
  regCvar   (22,                            &two,           4, 72, NCHAR);

        //      N       NMAX    ROW COL NCHAR MAXROWS
  regArrays (n_update, n_update, 0,  1,   6,    35);

        //   ID  PROMPT       SWITCH   SWITCH   COL NCHAR NDEC
  regIarray (2, "||"        , &m66  ,  &four ,   0,   2);
  regFarray (7, "XBIN      ", &zero ,  &p11  ,   0,   9,   3);
  regIarray (3, "||"        , &m66  ,  &four ,   0,   2);
  regFarray (8, "YBIN      ", &zero ,  &p11  ,   0,   9,   3);
  regIarray (5, "||"        , &m66  ,  &three,   0,   2);
  regFarray (9, "VALUE"     , &zero ,  &one  ,   0,   9,   3);
  regFarray (4, "value"     , &zero ,  &five ,  50,   9,   3);
  regFarray (6, "difference", &zero ,  &five ,  61,   9,   3);
  regCarray (1, NULL        , &m5   ,  &m25  ,   0,   NCHAR);

  funIvar (-31,          NULL,             NULL,   dist_switch_update);
  funIvar (-32,          NULL,             NULL,   dist_switch_update);
  funIvar  (31,    xdist_trap,     xdist_update,   dist_switch_update);
  funIvar  (32,    ydist_trap,     ydist_update,   dist_switch_update);
  funCvar ( 77,          NULL,             NULL,    ref_switch_update);
  funCvar ( -4,          NULL,             NULL,    ref_switch_update);
  funCvar (-41,          NULL,             NULL,    ref_switch_update);
  funCvar (-42,          NULL,             NULL,    ref_switch_update);
  funCvar  (41,   interp_trap,    interp_update,    ref_switch_update);
  funCvar  (42,   extrap_trap,    extrap_update,    ref_switch_update);
  funCvar  (22,   prompt_trap,    prompt_update);
  funCvar  (88,   select_trap);
  funIvar  ( 2,  xactive_trap,   xactive_update);
  funIvar  ( 3,  yactive_trap,   yactive_update);
  funIvar  ( 5,   select_trap,    select_update);
  funFvar  ( 7,          NULL,      xbin_update);
  funFvar  ( 8,          NULL,      ybin_update);
  funFvar  ( 9,    value_trap,     value_update,  value_switch_update);
  funFvar  ( 4,          NULL, reference_update);
  funFvar  ( 6,          NULL,  combined_update);
  funCvar  (-6,          NULL,     which_update);
  funCvar  ( 1,          NULL,      plot_update);
  funCvar  (-1,          NULL,      head_update);

  funCvar  (SHOW_ONE   ,   show_trap,  NULL,  show_switch_update);
  funCvar  (SHOW_EACH  ,   show_trap,  NULL,  show_switch_update);
  funCvar  (SHOW_SUM   ,   show_trap,  NULL,  show_switch_update);
  funCvar  (SHOW_AV    ,   show_trap,  NULL,  show_switch_update);
  funCvar  (SHOW_DIFF  ,   show_trap,  NULL,  show_switch_update);
  funCvar  (SHOW_WEIGHT,   show_trap,  NULL,  show_switch_update);
}



//-------------------------- end -------------------------------//
//-------------------------- end -------------------------------//
//-------------------------- end -------------------------------//
