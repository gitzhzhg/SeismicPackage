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

//---------------------- statbox_several.cc -----------------------//
//---------------------- statbox_several.cc -----------------------//
//---------------------- statbox_several.cc -----------------------//

//         implementation file for the StatboxSeveral class
//                derived from the SLDatabox class
//                     subdirectory statgui


#include "statgui/statbox_several.hh"
#include "statgui/statgui_several.hh"
#include "stat/static_manager.hh"
#include "stat/static_dataset.hh"
#include "sl/sl_prim.hh"
#include "named_constants.h"
#include <iostream.h>
#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <assert.h>


#define TABLE   StatboxSeveral *table  = (StatboxSeveral*)data;
#define DATASET StaticDataset *dataset = table->manager()->activeDataset();

#define NCOL 5

enum { ACTDAT = 9, FRST = 51, LFT = 61, RGHT = 71 };


//---------------------- constructor -----------------------//
//---------------------- constructor -----------------------//
//---------------------- constructor -----------------------//


StatboxSeveral::StatboxSeveral(SLDelay *slparent, StaticManager *manager)
           : SLDatabox(slparent, "statbox_several", NULL, 4),
                _manager         (manager),
                _gui             (NULL),
                _interp          (StaticDataset::INTERP_NEAR),
                _extrap          (StaticDataset::EXTRAP_NILS),
                _dindex          (NULL)
{
  _dindex = new long [NCOL];
  for(int i = 0; i < NCOL; i++) { _dindex[i] = i; }
}



//------------------------ destructor --------------------------//
//------------------------ destructor --------------------------//
//------------------------ destructor --------------------------//

StatboxSeveral::~StatboxSeveral()
{
  delete [] _dindex;
}



//--------------------------- scroll table ------------------------//
//--------------------------- scroll table ------------------------//
//--------------------------- scroll table ------------------------//

       // public.

void StatboxSeveral::scrollTable()
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
     case StatguiSeveral::XDIRECTION    : index = ix          ; break;
     case StatguiSeveral::YDIRECTION    : index = iy          ; break;
     case StatguiSeveral::XDIRECTION_ALL: index = ix + nx * iy; break;
     case StatguiSeveral::YDIRECTION_ALL: index = iy + ny * ix; break;
     default                            : assert(FALSE);
     }
  setFocus(2, (int)index);
}



//---------------------- get indices ---------------------------//
//---------------------- get indices ---------------------------//
//---------------------- get indices ---------------------------//

          // public.

long StatboxSeveral::getXindex(long index)  const
{
  StaticDataset *dataset = _manager->activeDataset();
  long nx = dataset->getNx();
  long ny = dataset->getNy();
  long ix_chosen = _gui->getChosenXindex();
  int direction = _gui->getDirection();
  long ix;
  switch(direction)
     {
     case StatguiSeveral::XDIRECTION    : ix = index;                     break;
     case StatguiSeveral::YDIRECTION    : ix = ix_chosen;                 break;
     case StatguiSeveral::XDIRECTION_ALL: ix = index - nx * (index / nx); break;
     case StatguiSeveral::YDIRECTION_ALL: ix = index / ny;                break;
     default                            : assert(FALSE);
     }
  return ix;
}


long StatboxSeveral::getYindex(long index)  const
{
  StaticDataset *dataset = _manager->activeDataset();
  long nx = dataset->getNx();
  long ny = dataset->getNy();
  long iy_chosen = _gui->getChosenYindex();
  int direction = _gui->getDirection();
  long iy;
  switch(direction)
     {
     case StatguiSeveral::XDIRECTION    : iy = iy_chosen;                 break;
     case StatguiSeveral::YDIRECTION    : iy = index;                     break;
     case StatguiSeveral::XDIRECTION_ALL: iy = index / nx;                break;
     case StatguiSeveral::YDIRECTION_ALL: iy = index - ny * (index / ny); break;
     default                            : assert(FALSE);
     }
  return iy;
}



//------------------------ step values ------------------------------//
//------------------------ step values ------------------------------//
//------------------------ step values ------------------------------//

       // public.

void StatboxSeveral::stepInterp()
{
  if(_interp == StaticDataset::INTERP_NEAR)
       _interp = StaticDataset::INTERP_TERP;
  else _interp = StaticDataset::INTERP_NEAR;
}


void StatboxSeveral::stepExtrap()
{
  if(_extrap == StaticDataset::EXTRAP_EDGE)
       _extrap = StaticDataset::EXTRAP_NILS;
  else if(_extrap == StaticDataset::EXTRAP_NILS)
       _extrap = StaticDataset::EXTRAP_ZERO;
  else _extrap = StaticDataset::EXTRAP_EDGE;
}



//------------------------ get dataset index ------------------------//
//------------------------ get dataset index ------------------------//
//------------------------ get dataset index ------------------------//

        // returns -1 if index not available.

static long get_dataset_index(void *data, long ident)
{
  TABLE
  ident = AbsoluteValue(ident);
  if(ident == ACTDAT)
      {
      return table->manager()->getActiveDatasetIndex();
      }
  long dindex = table->getDatasetIndex(ident-FRST);
  if(dindex == INIL || dindex < 0 ||
     dindex >= table->manager()->numDatasets())
      {
      return -1;
      }
  return dindex;
}



//-------------------------- traps ------------------------------//
//-------------------------- traps ------------------------------//
//-------------------------- traps ------------------------------//


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
  StatguiSeveral::freezeChosenIndices();
  dataset->setActiveIx(ix);
  StatguiSeveral::unfreezeChosenIndices();
}



static void yactive_trap(void *data, long /*ident*/, long index,
                            long /*value*/, long nread, char* /*endkey*/)
{
  if(nread == 0) return;
  TABLE
  DATASET
  long iy = table->getYindex(index);
  StatguiSeveral::freezeChosenIndices();
  dataset->setActiveIy(iy);
  StatguiSeveral::unfreezeChosenIndices();
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


static void prompt_trap(void *data, long ident, long /*index*/,
                            char* value, long nread, char* /*endkey*/)
{
  if(nread == 0) return;
  TABLE
  long dindex = atoi(value)-1;
  table->setDatasetIndex(-ident-FRST, dindex);
}


static void arrow_trap(void *data, long ident, long /*index*/,
                            char* /*value*/, long /*nread*/, char* endkey)
{
  if(strcmp(endkey, "RETURN") != 0) return;
  TABLE
  if(ident < RGHT) table->stepDatasetIndex(ident-LFT, -1);
  else             table->stepDatasetIndex(ident-RGHT, 1);
}



//---------------- variable update functions --------------------//
//---------------- variable update functions --------------------//
//---------------- variable update functions --------------------//


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


static char *prompt_update(void *data, long ident, long /*index*/)
{
/*
  TABLE
*/
  static char buffer[16];
  long dindex = get_dataset_index(data, ident);
  if(dindex == -1) strcpy(buffer, " ");
  else             sprintf(buffer, "%d", dindex+1);
  return buffer;
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



static float value_update(void *data, long ident, long index)
{
  TABLE
  long dindex = get_dataset_index(data, ident);
  if(dindex == -1)
      {
      return FNIL;
      }
  DATASET
  long  ix   = table->getXindex(index);
  long  iy   = table->getYindex(index);
  if(ident == ACTDAT)
      {
      return dataset->getValue(ix, iy);
      }
  float xbin = dataset->getXbin(ix);
  float ybin = dataset->getYbin(iy);
  int interp = (int)table->getInterp();
  int extrap = (int)table->getExtrap();
  return table->manager()->dataset(dindex)->
                      getResampledValue(xbin, ybin, interp, extrap);
}



static long n_update(void *data)
{
  TABLE
  DATASET
  StatguiSeveral *gui = table->getStatguiSeveral();
  int       direction = gui->getDirection();
  long n;
  switch(direction)
     {
     case StatguiSeveral::XDIRECTION    :
                             n = dataset->getNx()                   ; break;
     case StatguiSeveral::YDIRECTION    :
                             n = dataset->getNy()                   ; break;
     case StatguiSeveral::XDIRECTION_ALL:
                             n = dataset->getNx() * dataset->getNy(); break;
     case StatguiSeveral::YDIRECTION_ALL:
                             n = dataset->getNx() * dataset->getNy(); break;
     default: assert(FALSE);
     }
  return n;
}



//---------------- switch update functions --------------------//
//---------------- switch update functions --------------------//
//---------------- switch update functions --------------------//


static long value_switch_update(void *data, long ident, long /*index*/)
{
  TABLE
  long dindex = get_dataset_index(data, ident);
  if(dindex == -1) return -5;
  if(ident != ACTDAT) return 5;
  if(table->manager()->dataset(dindex)->isLocked()) return 5;
  return 1;
}



//---------------------- make helper ------------------------//
//---------------------- make helper ------------------------//
//---------------------- make helper ------------------------//

void StatboxSeveral::makeHelper()
{
  static long zero  =   0; 
  static long one   =   1; 
  static long two   =   2; 
  static long three =   3; 
  static long four  =   4; 
  static long five  =   5;
  static long p11   =  11;
//static long m5    =  -5;
//static long m25   = -25;
  static long m66   = -66;
  int i;

        //                                  SWITCH             ROW COL
  regCvar2  (41, "interpolation to get comparison values:",
                                    &zero,  &two,               1,  1, 32);
  regCvar2  (42, "extrapolation to get comparison values:",
                                    &zero,  &two,               2,  1, 32);
  regMsg    (    "ACTIVE"                                 ,     3, 39);
  regMsg    (    "       active        active"            ,     4,  1);
  regMsg    (    "DATASET"                                ,     4, 39);
  regString (88, "select",                  &two,               4, 32);

  for(i = 0; i < NCOL; i++)
      {
      regMsg    (        "DATASET",         3, 49+10*i);
      regString ( LFT+i, "<--"    , &two,   4, 49+10*i);
      regString (RGHT+i, "-->"    , &two,   4, 53+10*i);
      }

        //      N       NMAX    ROW COL NCHAR MAXROWS
  regArrays (n_update, n_update, 0,  1,   6,    35);

        //   ID       PROMPT       SWITCH   SWITCH   COL NCHAR NDEC
  regIarray (2     , "||"        , &m66  ,  &four ,   0,   2);
  regFarray (7     , "XBIN      ", &zero ,  &p11  ,   0,   9,   3);
  regIarray (3     , "||"        , &m66  ,  &four ,   0,   2);
  regFarray (8     , "YBIN      ", &zero ,  &p11  ,   0,   9,   3);
  regIarray (5     , "||"        , &m66  ,  &three,   0,   2);
  regFarray (ACTDAT, "xxx"       , &five ,  &one  ,   0,   9,   3);

  for(i = 0; i < NCOL; i++)
      {
      regFarray (FRST+i, "xxx", &one,  &one,   0, 9,   3);
      }

  funCvar  (41     ,   interp_trap,     interp_update);
  funCvar  (42     ,   extrap_trap,     extrap_update);
  funCvar  (22     ,   prompt_trap,     prompt_update);
  funCvar  (88     ,   select_trap);
  funIvar  ( 2     ,  xactive_trap,    xactive_update);
  funIvar  ( 3     ,  yactive_trap,    yactive_update);
  funIvar  ( 5     ,   select_trap,     select_update);
  funFvar  ( 7     ,          NULL,       xbin_update);
  funFvar  ( 8     ,          NULL,       ybin_update);
  funFvar  ( ACTDAT,    value_trap,      value_update,  value_switch_update);
  funCvar  (-ACTDAT,          NULL,     prompt_update);

  for(i = 0; i < NCOL; i++)
      {
      funCvar (  LFT+i,  arrow_trap);
      funCvar ( RGHT+i,  arrow_trap);
      funFvar ( FRST+i,  value_trap,  value_update,  value_switch_update);
      funCvar (-FRST-i, prompt_trap, prompt_update);
      }
}



//-------------------------- end -------------------------------//
//-------------------------- end -------------------------------//
//-------------------------- end -------------------------------//
