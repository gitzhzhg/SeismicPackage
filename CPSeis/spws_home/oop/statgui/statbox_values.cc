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

//---------------------- statbox_values.cc -----------------------//
//---------------------- statbox_values.cc -----------------------//
//---------------------- statbox_values.cc -----------------------//

//         implementation file for the StatboxValues class
//                derived from the SLDatabox class
//                     subdirectory statgui


#include "statgui/statbox_values.hh"
#include "statgui/statgui_values.hh"
#include "stat/static_manager.hh"
#include "stat/static_dataset.hh"
#include "sl/sl_prim.hh"
#include "named_constants.h"
#include <iostream.h>
#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <assert.h>


#define TABLE   StatboxValues *table   = (StatboxValues*)data;
#define GUI     StatguiValues *gui     = table->getStatguiValues();
#define DATASET StaticDataset *dataset = table->manager()->activeDataset();
#define REF     StaticDataset *ref     = table->manager()->referenceDataset();

#define NCOL 5

enum { SHOW_ACTIVE = 51, SHOW_REF, SHOW_SUM, SHOW_AV, SHOW_DIFF,
       SHOW_WEIGHT };

enum { ISEL = 60, IVAL = 70, IACT = 80, IBIN = 90 };

enum { XDIST = 1, YDIST, INTERP, EXTRAP, PROMPT, HEAD, ACT, BIN };


//---------------------- constructor -----------------------//
//---------------------- constructor -----------------------//
//---------------------- constructor -----------------------//


StatboxValues::StatboxValues(SLDelay *slparent, StaticManager *manager)
           : SLDatabox(slparent, "statbox_values", NULL, 4),
                _manager         (manager),
                _gui             (NULL),
                _interp          (StaticDataset::INTERP_NEAR),
                _extrap          (StaticDataset::EXTRAP_NILS),
                _show            (SHOW_ACTIVE),
                _xdist           (10),
                _ydist           (10)
{
}



//------------------------ destructor --------------------------//
//------------------------ destructor --------------------------//
//------------------------ destructor --------------------------//

StatboxValues::~StatboxValues()
{
}



//--------------------------- scroll table ------------------------//
//--------------------------- scroll table ------------------------//
//--------------------------- scroll table ------------------------//

       // public.

void StatboxValues::scrollTable()
{
  long index;
  if(_gui->isXdirection()) index = _gui->getChosenXindex();
  else                     index = _gui->getChosenYindex();
  setFocus(ACT, (int)index);
}



//---------------------- get indices ---------------------------//
//---------------------- get indices ---------------------------//
//---------------------- get indices ---------------------------//

    // public.
    // index = vertical index (assumed valid).
    // iadd = number to add to "first chosen index" to get horizontal index.
    // set index to -1 if you only want a horizontal index.
    // set iadd  to -1 if you only want a vertical   index.
    // getXindex returns the X index (which might be horizontal or vertical).
    // getYindex returns the Y index (which might be horizontal or vertical).
    // these return -1 if the index will be out-of-range.
    // these return -1 if the index is horizontal and you only wanted vertical.
    // these return -1 if the index is vertical and you only wanted horizontal.


long StatboxValues::getXindex(long iadd, long index)  const
{
  long ix;
  if(_gui->isXdirection())
      {
      if(index == -1) return -1;
      ix = index;
      }
  else
      {
      if(iadd == -1) return -1;
      ix = _gui->getChosenXindex() + iadd;
      }
  if(ix < 0 || ix >= _manager->activeDataset()->getNx()) return -1;
  return ix;
}


long StatboxValues::getYindex(long iadd, long index)  const
{
  long iy;
  if(_gui->isYdirection())
      {
      if(index == -1) return -1;
      iy = index;
      }
  else
      {
      if(iadd == -1) return -1;
      iy = _gui->getChosenYindex() + iadd;
      }
  if(iy < 0 || iy >= _manager->activeDataset()->getNy()) return -1;
  return iy;
}



//------------------------ step values ------------------------------//
//------------------------ step values ------------------------------//
//------------------------ step values ------------------------------//

       // public.

void StatboxValues::stepShow(int step)
{
  _show += step;
  if     (_show > SHOW_WEIGHT) _show = SHOW_ACTIVE;
  else if(_show < SHOW_ACTIVE) _show = SHOW_WEIGHT;
}


void StatboxValues::stepInterp()
{
  if(_interp == StaticDataset::INTERP_NEAR)
       _interp = StaticDataset::INTERP_TERP;
  else _interp = StaticDataset::INTERP_NEAR;
}


void StatboxValues::stepExtrap()
{
  if(_extrap == StaticDataset::EXTRAP_EDGE)
       _extrap = StaticDataset::EXTRAP_NILS;
  else if(_extrap == StaticDataset::EXTRAP_NILS)
       _extrap = StaticDataset::EXTRAP_ZERO;
  else _extrap = StaticDataset::EXTRAP_EDGE;
}



//------------------ maybe scroll left or right --------------------//
//------------------ maybe scroll left or right --------------------//
//------------------ maybe scroll left or right --------------------//

static void maybe_jump_laterally(void *data, char *endkey)
{
  static const long MANY = 9999999;
  TABLE
  GUI
  if(strcmp(endkey, "^LEFT") == 0)
      {
      gui->stepChosenIndex(-NCOL);
      strcpy(endkey, "JUNK");
      }
  else if(strcmp(endkey, "@LEFT") == 0)
      {
      gui->stepChosenIndex(-MANY);
      strcpy(endkey, "JUNK");
      }
  else if(strcmp(endkey, "^RIGHT") == 0)
      {
      gui->stepChosenIndex(NCOL);
      strcpy(endkey, "JUNK");
      }
  else if(strcmp(endkey, "@RIGHT") == 0)
      {
      gui->stepChosenIndex(MANY);
      strcpy(endkey, "JUNK");
      }
}


static void maybe_scroll_left(void *data, long ident, int ibase, char *endkey)
{
  TABLE
  GUI
  if(strcmp(endkey, "LEFT") == 0)
      {
      if(ident == ibase)
          {
          gui->stepChosenIndex(-1);
          strcpy(endkey, "JUNK");
          }
      }
  maybe_jump_laterally(data, endkey);
}


static void maybe_scroll_right(void *data, long ident, int ibase, char *endkey)
{
  TABLE
  GUI
  if(strcmp(endkey, "RIGHT") == 0)
      {
      if(ident == ibase + NCOL - 1 || !table->isVisible(ident + 1))
          {
          gui->stepChosenIndex(1);
          strcpy(endkey, "JUNK");
          }
      }
  maybe_jump_laterally(data, endkey);
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



static void act_trap_helper(void *data, long iadd, long index)
{
  TABLE
  DATASET
  StatguiValues::freezeChosenIndices();
  long ix = table->getXindex(iadd, index);
  long iy = table->getYindex(iadd, index);
  if(ix != -1) dataset->setActiveIx(ix);
  if(iy != -1) dataset->setActiveIy(iy);
  StatguiValues::unfreezeChosenIndices();
}



static void vert_act_trap(void *data, long /*ident*/, long index,
                            long /*value*/, long nread, char* /*endkey*/)
{
  if(nread == 0) return;
  act_trap_helper(data, -1, index);
}



static void horiz_act_trap(void *data, long ident, long /*index*/,
                            long /*value*/, long nread, char* endkey)
{
  if(nread > 0)
      {
      act_trap_helper(data, ident - IACT, -1);
      }
  maybe_scroll_left(data, ident, IACT, endkey);
}



static void horiz_bin_trap(void *data, long ident, long /*index*/,
                            float /*value*/, long /*nread*/, char* endkey)
{
  maybe_scroll_right(data, ident, IBIN, endkey);
}



static void select_trap(void *data, long ident, long index,
                            long value, long nread, char* endkey)
{
  if(nread > 0)
      {
      TABLE
      DATASET
      long ix = table->getXindex(ident - ISEL, index);
      long iy = table->getYindex(ident - ISEL, index);
      if(ix == -1 || iy == -1) return;
      dataset->setSelections(ix, iy, 1, 1, (int)value);
      }
  maybe_scroll_left (data, ident, ISEL, endkey);
}



static void value_trap(void *data, long ident, long index,
                            float value, long nread, char* endkey)
{
  if(nread > 0)
      {
      TABLE
      DATASET
      long ix = table->getXindex(ident - IVAL, index);
      long iy = table->getYindex(ident - IVAL, index);
      if(ix == -1 || iy == -1) return;
      dataset->setValue(ix, iy, value);
      }
  maybe_scroll_right(data, ident, IVAL, endkey);
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
  static char buffer1[] = "use nearest values";
  static char buffer2[] = "interpolate among non-nil values";
  if(table->getInterp() == StaticDataset::INTERP_NEAR) return buffer1;
                                                       return buffer2;
}


static char *extrap_update(void *data, long /*ident*/, long /*index*/)
{
  TABLE
  static char buffer1[] = "extrapolate with edge value";
  static char buffer2[] = "extrapolate with nils";
  static char buffer3[] = "extrapolate with zero";
  if(table->getExtrap() == StaticDataset::EXTRAP_EDGE) return buffer1;
  if(table->getExtrap() == StaticDataset::EXTRAP_NILS) return buffer2;
                                                       return buffer3;
}


static char *vert_head_update(void *data, long /*ident*/, long /*index*/)
{
  TABLE
  GUI
  static char buffer1[] = "XBIN";
  static char buffer2[] = "YBIN";
  if(gui->isXdirection()) return buffer1;
                          return buffer2;
}


static char *horiz_head_update(void *data, long /*ident*/, long /*index*/)
{
  TABLE
  GUI
  static char buffer1[] = "XBIN-->";
  static char buffer2[] = "YBIN-->";
  if(gui->isYdirection()) return buffer1;
                          return buffer2;
}


static char *prompt_update(void *data, long /*ident*/, long /*index*/)
{
  TABLE
  static char buffer0[] = "            xxxxxxxxxxxxxxxxxxx";
  static char buffer1[] = "       show SUM (active + reference)";
  static char buffer2[] = "    show AVERAGE (active + reference)/2";
  static char buffer3[] = "    show DIFFERENCE (active - reference)";
  static char buffer4[] = "     show value from REFERENCE dataset";
  static char buffer5[] = "       show VALUE from active dataset";
  static char buffer6[] = " show WEIGHT from active dataset (for splicing)";
  if(table->getShow() == SHOW_SUM    ) return buffer1;
  if(table->getShow() == SHOW_AV     ) return buffer2;
  if(table->getShow() == SHOW_DIFF   ) return buffer3;
  if(table->getShow() == SHOW_REF    ) return buffer4;
  if(table->getShow() == SHOW_ACTIVE ) return buffer5;
  if(table->getShow() == SHOW_WEIGHT ) return buffer6;
                                       return buffer0;
}


static char *pvalue_update(void *data, long /*ident*/, long /*index*/)
{
  TABLE
  static char buffer0[] = "xxxxx";
  static char buffer1[] = "sum";
  static char buffer2[] = "average";
  static char buffer3[] = "difference";
  static char buffer4[] = "reference";
  static char buffer5[] = "value";
  static char buffer6[] = "weight";
  if(table->getShow() == SHOW_SUM    ) return buffer1;
  if(table->getShow() == SHOW_AV     ) return buffer2;
  if(table->getShow() == SHOW_DIFF   ) return buffer3;
  if(table->getShow() == SHOW_REF    ) return buffer4;
  if(table->getShow() == SHOW_ACTIVE ) return buffer5;
  if(table->getShow() == SHOW_WEIGHT ) return buffer6;
                                       return buffer0;
}


static long act_update_helper(void *data, long iadd, long index)
{
  TABLE
  DATASET
  long ix = table->getXindex(iadd, index);
  long iy = table->getYindex(iadd, index);
  if(ix != -1) return (dataset->getActiveIx() == ix);
  if(iy != -1) return (dataset->getActiveIy() == iy);
  return FALSE;
}


static long vert_act_update(void *data, long /*ident*/, long index)
{
  return act_update_helper(data, -1, index);
}


static long horiz_act_update(void *data, long ident, long /*index*/)
{
  return act_update_helper(data, ident - IACT, -1);
}


static long select_update(void *data, long ident, long index)
{
  TABLE
  DATASET
  long ix = table->getXindex(ident - ISEL, index);
  long iy = table->getYindex(ident - ISEL, index);
  if(ix == -1 || iy == -1) return FALSE;
  return dataset->isSelected(ix, iy);
}



static float bin_update_helper(void *data, long iadd, long index)
{
  TABLE
  DATASET
  long ix = table->getXindex(iadd, index);
  long iy = table->getYindex(iadd, index);
  if(ix != -1) return dataset->getXbin(ix);
  if(iy != -1) return dataset->getYbin(iy);
  return FNIL;
}


static float vert_bin_update(void *data, long /*ident*/, long index)
{
  return bin_update_helper(data, -1, index);
}


static float horiz_bin_update(void *data, long ident, long /*index*/)
{
  return bin_update_helper(data, ident - IBIN, -1);
}


static float value_update(void *data, long ident, long index)
{
  TABLE
  DATASET
  long ix = table->getXindex(ident - IVAL, index);
  long iy = table->getYindex(ident - IVAL, index);
  if(ix == -1 || iy == -1) return FNIL;
  switch(table->getShow())
     {
     case SHOW_WEIGHT: return dataset->getWeight(ix, iy,
                                  table->getXdist(), table->getYdist());
     case SHOW_ACTIVE: return dataset->getValue(ix, iy);
     }
  REF
  float value1 = dataset->getValue(ix, iy);
  float xbin   = dataset->getXbin(ix);
  float ybin   = dataset->getYbin(iy);
  int   interp = (int)table->getInterp();
  int   extrap = (int)table->getExtrap();
  float value2 =  ref->getResampledValue(xbin, ybin, interp, extrap);
  if(value1 == FNIL && value2 == FNIL) return FNIL;
  float value;
  switch(table->getShow())
     {
     case SHOW_REF : value = value2;
                     break;
     case SHOW_SUM : if     (value1 == FNIL) value = 2.0 * value2;
                     else if(value2 == FNIL) value = 2.0 * value1;
                     else                    value = value1 + value2;
                     break;
     case SHOW_AV  : if     (value1 == FNIL) value = value2;
                     else if(value2 == FNIL) value = value1;
                     else                 value = 0.5 * (value1 + value2);
                     break;
     case SHOW_DIFF: if     (value1 == FNIL) value = FNIL;
                     else if(value2 == FNIL) value = FNIL;
                     else                    value = value1 - value2;
                     break;
     default: assert(FALSE);
     }
  return value;
}



static long n_update(void *data)
{
  TABLE
  GUI
  DATASET
  if(gui->isXdirection()) return dataset->getNx();
                          return dataset->getNy();
}




//---------------- switch update functions --------------------//
//---------------- switch update functions --------------------//
//---------------- switch update functions --------------------//


static long dist_switch(void *data, long ident, long /*index*/)
{
  TABLE
  if(table->getShow() != SHOW_WEIGHT) return -77;
  if(ident < 0) return 0;
  return 1;
}


static long ref_switch(void *data, long ident, long /*index*/)
{
  TABLE
  if(table->getShow() == SHOW_ACTIVE ||
     table->getShow() == SHOW_WEIGHT) return -77;
  if(ident < 0) return 0;
  return 2;
}


static long select_switch(void *data, long ident, long index)
{
  TABLE
  long ix = table->getXindex(ident - ISEL, index);
  long iy = table->getYindex(ident - ISEL, index);
  if(ix == -1 || iy == -1) return -3;
  return 3;
}


static long value_switch(void *data, long ident, long index)
{
  TABLE
  DATASET
  long isw = 1;
  if(dataset->isLocked()) isw = 5;
  if(table->getShow() != SHOW_ACTIVE) isw = 5;
  long ix = table->getXindex(ident - IVAL, index);
  long iy = table->getYindex(ident - IVAL, index);
  if(ix == -1 || iy == -1) isw = -isw;
  return isw;
}


static long horiz_act_switch(void *data, long ident, long /*index*/)
{
  TABLE
  long ix = table->getXindex(ident - IACT, -1);
  long iy = table->getYindex(ident - IACT, -1);
  if(ix == -1 && iy == -1) return -4;
  return 4;
}


static long show_switch(void *data, long ident, long /*index*/)
{
  TABLE
  int show = table->getShow();
  if(ident == show) return 6;
  return 2;
}



//---------------------- make helper ------------------------//
//---------------------- make helper ------------------------//
//---------------------- make helper ------------------------//

void StatboxValues::makeHelper()
{
  static long zero  =   0; 
  static long one   =   1; 
  static long two   =   2; 
  static long three =   3; 
  static long four  =   4; 
  static long p11   =  11;
  int i;

        //    ID          PROMPT   SWITCH   SWITCH         ROW COL NCHAR
  regCvar   (PROMPT     ,                   &two,           1,  1, 48);
  regString (SHOW_ACTIVE, "active"        , &two ,          1, 51);
  regString (SHOW_REF   , "ref"           , &two ,          1, -1);
  regString (SHOW_SUM   , "sum"           , &two ,          1, -1);
  regString (SHOW_AV    , "av"            , &two ,          1, -1);
  regString (SHOW_DIFF  , "diff"          , &two ,          1, -1);
  regString (SHOW_WEIGHT, " w "           , &two ,          1, -1);
  regCvar2  (INTERP     , "interpolation to get reference values:",
                                    &zero,  &two,           2,  1, 32);
  regCvar2  (EXTRAP     , "extrapolation to get reference values:",
                                    &zero,  &two,           3,  1, 32);
  regIvar2  (XDIST      , "xdist",  &zero,  &one,           2, 75, 3);
  regIvar2  (YDIST      , "ydist",  &zero,  &one,           2, 86, 3);
  regCvar   (HEAD       ,                   &zero,          4, 14, 7);

  for(i = 0; i < NCOL; i++)
     {
          //   ID            SWITCH   ROW   COL    NCHAR NDEC
     regIvar (IACT+i,        &four ,   4, 22+14*i,   2);
     regFvar (IBIN+i,        &p11  ,   4, 25+14*i,   9,   3);
     }

        //      N       NMAX    ROW COL NCHAR MAXROWS
  regArrays (n_update, n_update, 5,  1,   6,    35);

        //   ID  PROMPT         SWITCH   SWITCH   COL NCHAR NDEC
  regIarray (ACT, "  "        , &zero ,  &four ,   0,   2);
  regFarray (BIN, "XBIN      ", &zero ,  &p11  ,   0,   9,   3);

  for(i = 0; i < NCOL; i++)
     {
              //  ID    PROMPT        SWITCH   SWITCH   COL NCHAR NDEC
     regIarray (ISEL+i, "  "        , &zero ,  &three,   0,   2);
     regFarray (IVAL+i, "VALUE     ", &zero ,  &one  ,   0,   9,   3);
     }

  funIvar (-XDIST ,           NULL,               NULL,   dist_switch);
  funIvar (-YDIST ,           NULL,               NULL,   dist_switch);
  funIvar ( XDIST ,     xdist_trap,       xdist_update,   dist_switch);
  funIvar ( YDIST ,     ydist_trap,       ydist_update,   dist_switch);

  funCvar (-INTERP,           NULL,               NULL,    ref_switch);
  funCvar (-EXTRAP,           NULL,               NULL,    ref_switch);
  funCvar ( INTERP,    interp_trap,      interp_update,    ref_switch);
  funCvar ( EXTRAP,    extrap_trap,      extrap_update,    ref_switch);

  funCvar ( HEAD  ,           NULL,  horiz_head_update);
  funCvar (-BIN   ,           NULL,   vert_head_update);
  funCvar ( PROMPT,    prompt_trap,      prompt_update);
  funIvar ( ACT   ,  vert_act_trap,    vert_act_update);
  funFvar ( BIN   ,           NULL,    vert_bin_update);

  for(i = 0; i < NCOL; i++)
     {
     funIvar ( ISEL+i,     select_trap,     select_update,     select_switch);
     funFvar ( IVAL+i,      value_trap,      value_update,      value_switch);
     funCvar (-IVAL-i,            NULL,     pvalue_update);
     funIvar ( IACT+i,  horiz_act_trap,  horiz_act_update,  horiz_act_switch);
     funFvar ( IBIN+i,  horiz_bin_trap,  horiz_bin_update);
     }

  funCvar (SHOW_ACTIVE,   show_trap,  NULL,  show_switch);
  funCvar (SHOW_REF   ,   show_trap,  NULL,  show_switch);
  funCvar (SHOW_SUM   ,   show_trap,  NULL,  show_switch);
  funCvar (SHOW_AV    ,   show_trap,  NULL,  show_switch);
  funCvar (SHOW_DIFF  ,   show_trap,  NULL,  show_switch);
  funCvar (SHOW_WEIGHT,   show_trap,  NULL,  show_switch);
}



//-------------------------- end -------------------------------//
//-------------------------- end -------------------------------//
//-------------------------- end -------------------------------//
