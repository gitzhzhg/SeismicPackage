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

//---------------------- vfbox_horizon_list.cc -----------------------//
//---------------------- vfbox_horizon_list.cc -----------------------//
//---------------------- vfbox_horizon_list.cc -----------------------//

//          implementation file for the VfboxHorizonList class
//                   derived from the SLDatabox class
//                        subdirectory vfgui


#include "vfgui/vfbox_horizon_list.hh"
#include "vf/vf_horizons.hh"
#include "sl/sl_prim.hh"
#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <assert.h>


enum { ACTIVE = 1, SELECT, ERROR, NAME, COLOR };


#define TABLE     VfboxHorizonList *table    = (VfboxHorizonList*)data;
#define HORIZONS  VfHorizons       *horizons = table->horizons();

static const char *colors[] = { "red", "gold", "green", "blue",
                                "orange", "purple", "yellow",
                                "white", "cyan", "magenta",
                                "brown", "pink", "black", "gray" };
static const int num_colors = 14;


//---------------------- constructor -----------------------//
//---------------------- constructor -----------------------//
//---------------------- constructor -----------------------//


VfboxHorizonList::VfboxHorizonList(SLDelay *slparent, VfHorizons *horizons)
           : SLDatabox(slparent, "vfbox_horizon_list", NULL, 4),
                     _horizons   (horizons)
{
  assert(_horizons);
}



//------------------------ destructor --------------------------//
//------------------------ destructor --------------------------//
//------------------------ destructor --------------------------//

VfboxHorizonList::~VfboxHorizonList()
{
}



//---------------------- trap functions --------------------------//
//---------------------- trap functions --------------------------//
//---------------------- trap functions --------------------------//



static void select_all_trap(void *data, long /*ident*/, long /*index*/,
                            char* /*value*/, long /*nread*/, char *endkey)
{
  if(strcmp(endkey, "RETURN") != 0) return;
  TABLE
  HORIZONS
  if(horizons->numHorizons() == 0) return;
  int selected = horizons->isSelected(0);
  horizons->selectAllHorizons(!selected);
}



static void active_trap(void *data, long /*ident*/, long index,
                            long /*value*/, long nread, char* /*endkey*/)
{
  if(nread == 0) return;
  TABLE
  HORIZONS
  horizons->setActiveHorizonIndex(index);
}



static void select_trap(void *data, long /*ident*/, long index,
                            long /*value*/, long nread, char* /*endkey*/)
{
  if(nread == 0) return;
  TABLE
  HORIZONS
  int selected = horizons->isSelected(index);
  horizons->setSelected(index, !selected);
}



static void color_trap(void *data, long /*ident*/, long index,
                            char* /*value*/, long /*nread*/, char* endkey)
{
  int step = 0;
  if(strcmp(endkey, "RETURN" ) == 0) step =  1;
  if(strcmp(endkey, "BUTTON2") == 0) step = -1;
  if(step == 0) return;
  TABLE
  HORIZONS
  const char *color = horizons->getColor(index);
  int i2 = 0;
  for(int i = 0; i < num_colors; i++)
      {
      if(strcmp(color, colors[i]) == 0)
          {
          i2 = i + step;
          if     (i2 >= num_colors) i2 = 0;
          else if(i2 <           0) i2 = num_colors - 1;
          break;
          }
      }
  horizons->setColor(index, colors[i2]);
}


/*
static void color_trap(void *data, long / *ident* /, long index,
                            char* value, long nread, char* / *endkey* /)
{
  if(nread == 0) return;
  TABLE
  HORIZONS
  horizons->setColor(index, value);
}
*/



//------------------------- update functions -------------------------//
//------------------------- update functions -------------------------//
//------------------------- update functions -------------------------//


static long active_update(void *data, long /*ident*/, long index)
{
  TABLE
  HORIZONS
  long ihorizon = horizons->getActiveHorizonIndex();
  return (ihorizon == index);
}



static long select_update(void *data, long /*ident*/, long index)
{
  TABLE
  HORIZONS
  return horizons->isSelected(index);
}



static long error_update(void *data, long /*ident*/, long index)
{
  TABLE
  HORIZONS
  return horizons->readError(index);
}



static char *name_update(void *data, long /*ident*/, long index)
{
  TABLE
  HORIZONS
  return (char*)horizons->getName(index);
}



static char *color_update(void *data, long /*ident*/, long index)
{
  TABLE
  HORIZONS
  return (char*)horizons->getColor(index);
}



static long n_update(void *data)
{
  TABLE
  HORIZONS
  return horizons->numHorizons();
}



//---------------------- switch functions --------------------------//
//---------------------- switch functions --------------------------//
//---------------------- switch functions --------------------------//


//--------------------------- make helper -----------------------------//
//--------------------------- make helper -----------------------------//
//--------------------------- make helper -----------------------------//

void VfboxHorizonList::makeHelper()
{
  static long zero  =   0; 
//static long one   =   1; 
  static long two   =   2; 
  static long three =   3; 
  static long four  =   4; 
  static long five  =   5; 

  regMsg   ("read"            , 1, 24);
  regMsg   ("LIST OF HORIZONS", 1, 49);

    //        N        NMAX    ROW COL NCHAR MAXROWS
  regArrays(n_update, n_update, 2,  0,   4,    35);

    //        ID       PROMPT          JSW      ISW     COL NCHAR NDEC
  regIarray (ACTIVE, "active"       , &zero ,  &four ,   8,    2);
  regIarray (SELECT, "select"       , &two  ,  &three,  16,    2);
  regIarray (ERROR , "error"        , &zero ,  &three,  24,    2);
  regCarray (NAME  , "horizon file" , &zero ,  &five ,  32,   32);
  regCarray (COLOR , "color"        , &zero ,  &two  ,  66,   16);

  funCvar  (-SELECT,  select_all_trap);
  funIvar  ( ACTIVE,      active_trap, active_update);
  funIvar  ( SELECT,      select_trap, select_update);
  funIvar  ( ERROR ,             NULL,  error_update);
  funCvar  ( NAME  ,             NULL,   name_update);
  funCvar  ( COLOR ,       color_trap,  color_update);
}



//-------------------------- end -------------------------------//
//-------------------------- end -------------------------------//
//-------------------------- end -------------------------------//
