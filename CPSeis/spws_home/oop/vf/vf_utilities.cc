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

//-------------------------- vf_utilities.cc --------------------------//
//-------------------------- vf_utilities.cc --------------------------//
//-------------------------- vf_utilities.cc --------------------------//

//          implementation file for the VfUtilities class
//                    not derived from any class
//                         subdirectory vf



#include "vf/vf_utilities.hh"
#include "vf/vf_constants.hh"
#include "vf/vf_offsets.hh"
#include "vf/vf_update.hh"
#include "named_constants.h"
#include "str.h"
#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <assert.h>
#include <limits.h>
#include <math.h>



//-------------------------- constructor ----------------------------//
//-------------------------- constructor ----------------------------//
//-------------------------- constructor ----------------------------//


VfUtilities::VfUtilities(int bogus_velocities)
           :
              _offsets      (NULL),
              _update       (NULL),
              _timetol      (0.02),
              _depthtol     (10.0),
              _mm           (1.5),
              _xcenter      (0.0),
              _ycenter      (0.0),
              _xwidth       (5.0),
              _ywidth       (5.0)
{
  _offsets   = new VfOffsets ();
  _update    = new VfUpdate  (this, _offsets, bogus_velocities);
}



//--------------------------- destructor --------------------------//
//--------------------------- destructor --------------------------//
//--------------------------- destructor --------------------------//


VfUtilities::~VfUtilities()
{
  delete _offsets;
  delete _update;
}



//------------------------- set values -----------------------------//
//------------------------- set values -----------------------------//
//------------------------- set values -----------------------------//


void VfUtilities::setTimeTolerance    (float value)
{
  _timetol = ConstrainValue(value, 0.001, 0.1);
}


void VfUtilities::setDepthTolerance   (float value)
{
  _depthtol = ConstrainValue(value, 1.0, 1000.0);
}


void VfUtilities::setPickingTolerance   (float value)
{
  _mm = ConstrainValue(value, 0.1, 10.0);
}


void VfUtilities::setXcenterByVfManagerOnly(float value)
{
  _xcenter = value;
}


void VfUtilities::setYcenterByVfManagerOnly(float value)
{
  _ycenter = value;
}


void VfUtilities::setXwidthByVfManagerOnly(float value)
{
  _xwidth = ConstrainValue(value, 0.01, 100000.0);
}


void VfUtilities::setYwidthByVfManagerOnly(float value)
{
  _ywidth = ConstrainValue(value, 0.01, 100000.0);
}



//----------------------- get bin center or bin number --------------------//
//----------------------- get bin center or bin number --------------------//
//----------------------- get bin center or bin number --------------------//

       // public.

float VfUtilities::xbinCenter (float xloc)  const
{
  return BinCenter(xloc, _xcenter, _xwidth);
}


float VfUtilities::ybinCenter (float yloc)  const
{
  return BinCenter(yloc, _ycenter, _ywidth);
}


long VfUtilities::xbinNumber (float xloc)  const
{
  return BinNumber(xloc, _xcenter, _xwidth);
}


long VfUtilities::ybinNumber (float yloc)  const
{
  return BinNumber(yloc, _ycenter, _ywidth);
}



//---------------- get type symbol and description --------------------//
//---------------- get type symbol and description --------------------//
//---------------- get type symbol and description --------------------//

            // public static functions.

static char *type_description[] = {
    "VTNM: NMO velocity versus 2-way time     ",  // type - FIRSTTYPE =  0
    "VTRM: RMS velocity versus 2-way time     ",  // type - FIRSTTYPE =  1
    "VZRM: RMS velocity versus depth          ",  // type - FIRSTTYPE =  2
    "VLRM: RMS velocity versus thickness      ",  // type - FIRSTTYPE =  3
    "VTAV: average velocity versus 2-way time ",  // type - FIRSTTYPE =  4
    "VZAV: average velocity versus depth      ",  // type - FIRSTTYPE =  5
    "VLAV: average velocity versus thickness  ",  // type - FIRSTTYPE =  6
    "VTIN: interval velocity versus 2-way time",  // type - FIRSTTYPE =  7
    "VZIN: interval velocity versus depth     ",  // type - FIRSTTYPE =  8
    "VLIN: interval velocity versus thickness ",  // type - FIRSTTYPE =  9
    "VTDP: depth versus 2-way time            "   // type - FIRSTTYPE = 10
};


const char *VfUtilities::typeSymbol (int type)
{
  static char buffer[8];
  const char *temporary = typeDescription(type);
  strncpy(buffer, temporary, 4);
  buffer[4] = '\0';
  return buffer;
}


const char *VfUtilities::typeDescription (int type)
{
  assert(type >= FIRSTTYPE && type <= LASTTYPE);
  return type_description[type - FIRSTTYPE];
}



int VfUtilities::getTypeFromSymbol (const char *symbol)
{
  if(!symbol) return -1;
  char upper[8];
  strncpy(upper, symbol, 4);
  upper[4] = '\0';
  str_to_upper(upper, upper);
  for(int type = FIRSTTYPE; type <= LASTTYPE; type++)
      {
      if(strcmp(upper, typeSymbol(type)) == 0) return type;
      }
  return -1;
}



//------------------ find out about abscissa or ordinate --------------//
//------------------ find out about abscissa or ordinate --------------//
//------------------ find out about abscissa or ordinate --------------//

            // public static functions.

int VfUtilities::abscissaIsTime (int type)
{
  assert(type >= FIRSTTYPE && type <= LASTTYPE);
  return (type == VTNM ||
          type == VTRM ||
          type == VTIN ||
          type == VTAV ||
          type == VTDP);
}


int VfUtilities::abscissaIsDepth (int type)
{
  assert(type >= FIRSTTYPE && type <= LASTTYPE);
  return (type == VZRM ||
          type == VZIN ||
          type == VZAV);
}


int VfUtilities::abscissaIsThickness (int type)
{
  assert(type >= FIRSTTYPE && type <= LASTTYPE);
  return (type == VLRM ||
          type == VLIN ||
          type == VLAV);
}


int VfUtilities::ordinateIsVelocity (int type)
{
  assert(type >= FIRSTTYPE && type <= LASTTYPE);
  return (type != VTDP);
}


int VfUtilities::ordinateIsDepth (int type)
{
  assert(type >= FIRSTTYPE && type <= LASTTYPE);
  return (type == VTDP);
}


int VfUtilities::ordinateIsVNMO (int type)
{
  assert(type >= FIRSTTYPE && type <= LASTTYPE);
  return (type == VTNM);
}


int VfUtilities::ordinateIsVRMS (int type)
{
  assert(type >= FIRSTTYPE && type <= LASTTYPE);
  return (type == VTRM ||
          type == VZRM ||
          type == VLRM);
}


int VfUtilities::ordinateIsVAV (int type)
{
  assert(type >= FIRSTTYPE && type <= LASTTYPE);
  return (type == VTAV ||
          type == VZAV ||
          type == VLAV);
}


int VfUtilities::ordinateIsVINT (int type)
{
  assert(type >= FIRSTTYPE && type <= LASTTYPE);
  return (type == VTIN ||
          type == VZIN ||
          type == VLIN);
}



//------------------------- get units information -----------------------//
//------------------------- get units information -----------------------//
//------------------------- get units information -----------------------//

            // public static functions.

const char *VfUtilities::getSymbolFromUnits (int units)
{
  static const char *symbol_feet        = "feet/sec";
  static const char *symbol_meters      = "meters/sec";
  static const char *symbol_unspecified = "unspecified";
  if(units ==   FEET_PER_SECOND) return symbol_feet;
  if(units == METERS_PER_SECOND) return symbol_meters;
  return symbol_unspecified;
}


int VfUtilities::getUnitsFromSymbol (const char *symbol)
{
  if(!symbol) return UNSPECIFIED_UNITS;
  if(symbol[0] == 'f' || symbol[0] == 'F') return FEET_PER_SECOND;
  if(symbol[0] == 'm' || symbol[0] == 'M') return METERS_PER_SECOND;
  return UNSPECIFIED_UNITS;
}



//----------------- derive moveout information -----------------------//
//----------------- derive moveout information -----------------------//
//----------------- derive moveout information -----------------------//

            // public static functions.

int VfUtilities::deriveMoveoutOrder(float /*nhosign*/, float nhoexp)
{
  if(nhoexp <= 2.0) return 2;
  return 4;
}


float VfUtilities::deriveNhosign(int order)
{
  if(order <= 2) return 1.0;
  return -1.0;
}


float VfUtilities::deriveNhoexp(int order)
{
  if(order <= 2) return 2.0;
  return 4.0;
}



//------------------------------- end -------------------------------//
//------------------------------- end -------------------------------//
//------------------------------- end -------------------------------//

