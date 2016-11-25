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

//---------------------- vfbox_horizon_picks.cc -----------------------//
//---------------------- vfbox_horizon_picks.cc -----------------------//
//---------------------- vfbox_horizon_picks.cc -----------------------//

//        implementation file for the VfboxHorizonPicks class
//                    derived from the SLDatabox class
//                    derived from the VfInform class
//                         subdirectory vfgui


#include "vfgui/vfbox_horizon_picks.hh"
#include "vf/vf_horizons.hh"
#include "named_constants.h"
#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <assert.h>

#define TABLE    VfboxHorizonPicks *table    = (VfboxHorizonPicks*)data;
#define HORIZONS VfHorizons        *horizons = table->horizons();


enum { ACTIVE = 1, XLOC, YLOC, TIME, SP  , LINE,
                   XMIN, YMIN, TMIN, SMIN, LMIN,
                   XMAX, YMAX, TMAX, SMAX, LMAX, XGRID, YGRID,
                   INFO };


//-------------------------- constructor -------------------------------//
//-------------------------- constructor -------------------------------//
//-------------------------- constructor -------------------------------//


VfboxHorizonPicks::VfboxHorizonPicks
                  (SLDelay *slparent, VfManager *manager, VfHorizons *horizons)
           : SLDatabox(slparent, "vfbox_horizon_picks", NULL, 4),
             VfInform(manager),
                      _horizons   (horizons)
{
  assert(_horizons);
}



//---------------------------- destructor ------------------------------//
//---------------------------- destructor ------------------------------//
//---------------------------- destructor ------------------------------//

VfboxHorizonPicks::~VfboxHorizonPicks()
{
}



//----------------- virtual functions overriding VfInform ------------//
//----------------- virtual functions overriding VfInform ------------//
//----------------- virtual functions overriding VfInform ------------//

       // private.

void VfboxHorizonPicks::postNewActiveHorizon()
{
  long ihorizon = horizons()->getActiveHorizonIndex();
  if(ihorizon == -1) return;
  long act = horizons()->getActivePick(ihorizon);
  setFocus(ACTIVE, (int)act);
}


void VfboxHorizonPicks::postNewActiveHorizonPick(long ihorizon)
{
  long index = horizons()->getActiveHorizonIndex();
  if(index != ihorizon) return;
  long act = horizons()->getActivePick(ihorizon);
  setFocus(ACTIVE, (int)act);
}



//------------------------- trap functions -----------------------------//
//------------------------- trap functions -----------------------------//
//------------------------- trap functions -----------------------------//


static void active_trap(void *data, long /*ident*/, long index,
                        long /*ivar*/, long /*nread*/, char *endkey)
{
  TABLE
  HORIZONS
  if(strcmp(endkey, "RETURN") != 0) return;
  long ihorizon = horizons->getActiveHorizonIndex();
  if(ihorizon == -1) return;
  horizons->setActivePick(ihorizon, index);
}



//------------------------- update functions -------------------------//
//------------------------- update functions -------------------------//
//------------------------- update functions -------------------------//


static long active_update(void *data, long /*ident*/, long index)
{
  TABLE
  HORIZONS
  long ihorizon = horizons->getActiveHorizonIndex();
  if(ihorizon == -1) return FALSE;
  return (horizons->getActivePick(ihorizon) == index);
}



static float minmax_update(void *data, long ident, long /*index*/)
{
  TABLE
  HORIZONS
  long ihorizon = horizons->getActiveHorizonIndex();
  if(ihorizon == -1) return FNIL;
  switch(ident)
      {
      case XMIN: return horizons->minimumXloc      (ihorizon);
      case YMIN: return horizons->minimumYloc      (ihorizon);
      case TMIN: return horizons->minimumTime      (ihorizon);
      case SMIN: return horizons->minimumShotpoint (ihorizon);
      case LMIN: return horizons->minimumLineNumber(ihorizon);
      case XMAX: return horizons->maximumXloc      (ihorizon);
      case YMAX: return horizons->maximumYloc      (ihorizon);
      case TMAX: return horizons->maximumTime      (ihorizon);
      case SMAX: return horizons->maximumShotpoint (ihorizon);
      case LMAX: return horizons->maximumLineNumber(ihorizon);
      default: assert(FALSE);
    }
  assert(FALSE);
  return 0.0;
}



static char *info_update(void *data, long /*ident*/, long /*index*/)
{
  TABLE
  HORIZONS
  long ihorizon = horizons->getActiveHorizonIndex();
  if(ihorizon == -1) return (char*)CNIL;
  static char buffer[44];
  strcpy(buffer, horizons->getPicktype(ihorizon));
  if(buffer[0] == '\0') return buffer;
  strcat(buffer, " in ");
  strcat(buffer, horizons->getUnits   (ihorizon));
  return buffer;
}



static float float_update(void *data, long ident, long index)
{
  TABLE
  HORIZONS
  long ihorizon = horizons->getActiveHorizonIndex();
  if(ihorizon == -1) return 0.0;
  switch(ident)
      {
      case XLOC : return horizons->getXloc      (ihorizon, index);
      case YLOC : return horizons->getYloc      (ihorizon, index);
      case TIME : return horizons->getTime      (ihorizon, index);
      case SP   : return horizons->getShotpoint (ihorizon, index);
      case LINE : return horizons->getLineNumber(ihorizon, index);
      case XGRID: return horizons->getXgrid     (ihorizon, index);
      case YGRID: return horizons->getYgrid     (ihorizon, index);
      default: assert(FALSE);
    }
  assert(FALSE);
  return 0.0;
}



static long n_update(void *data)
{
  TABLE
  HORIZONS
  long ihorizon = horizons->getActiveHorizonIndex();
  if(ihorizon == -1) return 0;
  return horizons->numPicks(ihorizon);
}



//------------------------- switch functions -------------------------//
//------------------------- switch functions -------------------------//
//------------------------- switch functions -------------------------//




//-------------------------- make helper ----------------------------//
//-------------------------- make helper ----------------------------//
//-------------------------- make helper ----------------------------//

void VfboxHorizonPicks::makeHelper()
{
  static long p0  =   0; 
  static long p4  =   4; 
  static long p5  =   5; 

     //      ID       PROMPT            ISW   ROW  COL NCHAR
  regMsg    ("ACTIVE HORIZON"         ,        1,  26);    
  regMsg    ("minimum:"               ,        2,   2);
  regMsg    ("maximum:"               ,        3,   2);
  regMsg    ("transformed"            ,        3,  62);

  regCvar   (INFO   ,                    &p0,  1,  64,   26);

  regFvar   (XMIN   ,                    &p5,  2,  11,   8,   4);
  regFvar   (YMIN   ,                    &p5, -1,  -1,   8,   4);
  regFvar   (TMIN   ,                    &p5, -1,  -1,   8,   4);
  regFvar   (SMIN   ,                    &p5, -1,  -1,   8,   4);
  regFvar   (LMIN   ,                    &p5, -1,  -1,   8,   4);

  regFvar   (XMAX   ,                    &p5,  3,  11,   8,   4);
  regFvar   (YMAX   ,                    &p5, -1,  -1,   8,   4);
  regFvar   (TMAX   ,                    &p5, -1,  -1,   8,   4);
  regFvar   (SMAX   ,                    &p5, -1,  -1,   8,   4);
  regFvar   (LMAX   ,                    &p5, -1,  -1,   8,   4);

     //       N        NMAX    ROW COL NCHAR MAXROWS
  regArrays(n_update, n_update, 4,  0,   6,    35);

     //      ID       PROMPT        JSW     ISW     COL NCHAR NDEC
  regIarray (ACTIVE ," "           ,&p0  ,  &p4   ,  0,  2);
  regFarray (XLOC   ,"XLOC"        ,&p0  ,  &p5   ,  0,  8,   4);
  regFarray (YLOC   ,"YLOC"        ,&p0  ,  &p5   ,  0,  8,   4);
  regFarray (TIME   ,"TIME"        ,&p0  ,  &p5   ,  0,  8,   4);
  regFarray (SP     ,"SP  "        ,&p0  ,  &p5   ,  0,  8,   4);
  regFarray (LINE   ,"LINE        ",&p0  ,  &p5   ,  0,  8,   4);
  regFarray (XGRID  ,"XGRID"       ,&p0  ,  &p5   ,  0,  8,   4);
  regFarray (YGRID  ,"YGRID"       ,&p0  ,  &p5   ,  0,  8,   4);


  funCvar (INFO     ,       NULL,   info_update);

  funIvar (ACTIVE ,  active_trap, active_update);
  funFvar (XLOC   ,         NULL,  float_update);
  funFvar (YLOC   ,         NULL,  float_update);
  funFvar (TIME   ,         NULL,  float_update);
  funFvar (SP     ,         NULL,  float_update);
  funFvar (LINE   ,         NULL,  float_update);
  funFvar (XGRID  ,         NULL,  float_update);
  funFvar (YGRID  ,         NULL,  float_update);

  funFvar (XMIN   ,         NULL, minmax_update);
  funFvar (YMIN   ,         NULL, minmax_update);
  funFvar (TMIN   ,         NULL, minmax_update);
  funFvar (SMIN   ,         NULL, minmax_update);
  funFvar (LMIN   ,         NULL, minmax_update);

  funFvar (XMAX   ,         NULL, minmax_update);
  funFvar (YMAX   ,         NULL, minmax_update);
  funFvar (TMAX   ,         NULL, minmax_update);
  funFvar (SMAX   ,         NULL, minmax_update);
  funFvar (LMAX   ,         NULL, minmax_update);
}



//------------------------------ end -----------------------------------//
//------------------------------ end -----------------------------------//
//------------------------------ end -----------------------------------//
