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

//---------------------- vfbox_mute.cc -----------------------//
//---------------------- vfbox_mute.cc -----------------------//
//---------------------- vfbox_mute.cc -----------------------//

//          implementation file for the VfboxMute class
//                derived from the SLDatabox class
//                     subdirectory vfgui


#include "vfgui/vfbox_mute.hh"
#include "vf/vf_manager.hh"
#include "vf/vf_dataset.hh"
#include "vf/vf_informer.hh"
#include "vf/vf_utilities.hh"
#include "vf/vf_offsets.hh"
#include "vf/vf_constants.hh"
#include "sl/sl_prim.hh"
#include "sl/sl_dialog.hh"
#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <assert.h>


enum { IMMEDIATE = 99, METERS, FEET, MINIMUM, MAXIMUM, MOFFSET, MTIME };

#define TABLE   VfboxMute *table = (VfboxMute*)data;
#define OFFSETS VfOffsets *offsets = table->manager()->utilities()->offsets();


//---------------------- constructor -----------------------//
//---------------------- constructor -----------------------//
//---------------------- constructor -----------------------//


VfboxMute::VfboxMute(SLDelay *slparent, char *name, VfManager *manager,
                                                    SLDialog *pickpop)
           : SLDatabox(slparent, name, NULL, 4),
                _manager    (manager),
                _pickpop    (pickpop),
                _immediate  (FALSE)
{
  assert(_manager);
}



//------------------------ destructor --------------------------//
//------------------------ destructor --------------------------//
//------------------------ destructor --------------------------//

VfboxMute::~VfboxMute()
{
}



//--------------------------- traps --------------------------------//
//--------------------------- traps --------------------------------//
//--------------------------- traps --------------------------------//


static void immediate_action(VfboxMute *table)
{
  if(table->getImmediate() == FALSE) return;
  VfDataset *dataset = table->manager()->activeDataset();
  if(dataset->notEditable()) return;
  if(dataset->isLocked   ()) return;
  long ifun = dataset->getActiveVelocityFunction();
  if(ifun == -1) return;
  if(dataset->getRaytraceFlag(ifun) == RAYTRACE_NO) return;
  if(!table->pickpop()) return;
  if(!table->pickpop()->isManaged()) return;
  dataset->informer()->beforeChanges();
  dataset->cancelRayTracing(ifun);
  dataset->invokeRayTracing(ifun);
  dataset->informer()->afterChanges();
}



static void immediate_trap(void *data, long /*ident*/, long /*index*/,
                           long ivar, long nread, char* /*endkey*/)
{
  if(nread == 0) return;
  TABLE
  table->setImmediate((int)ivar);
}



static void meters_trap(void *data, long /*ident*/, long /*index*/,
                        char* /*cvar*/, long /*nread*/, char *endkey)
{
  if(strcmp(endkey, "RETURN") != 0) return;
  TABLE
  OFFSETS
  offsets->convertOffsetFeetToMeters();
  immediate_action(table);
}


static void feet_trap(void *data, long /*ident*/, long /*index*/,
                        char* /*cvar*/, long /*nread*/, char *endkey)
{
  if(strcmp(endkey, "RETURN") != 0) return;
  TABLE
  OFFSETS
  offsets->convertOffsetMetersToFeet();
  immediate_action(table);
}



static void muteflag_trap(void *data, long ident, long /*index*/,
                          long /*ivar*/, long /*nread*/, char *endkey)
{
  if(strcmp(endkey, "RETURN") != 0) return;
  TABLE
  OFFSETS
  offsets->setMuteFlag((int)ident);
  immediate_action(table);
}



static void minimum_trap(void *data, long /*ident*/, long /*index*/,
                          float fvar, long nread, char* /*endkey*/)
{
  if(nread == 0) return;
  TABLE
  OFFSETS
  offsets->setMinimumOffset(fvar);
  immediate_action(table);
}



static void maximum_trap(void *data, long /*ident*/, long /*index*/,
                          float fvar, long nread, char* /*endkey*/)
{
  if(nread == 0) return;
  TABLE
  OFFSETS
  offsets->setMaximumOffset(fvar);
  immediate_action(table);
}



static int maybe_remove_insert_append
             (VfOffsets *offsets, long index, long nread, char *endkey)
{
  if(strcmp(endkey, "INSERT") == 0)
      {
      offsets->insertMutePairFromBuffer(index);
      return TRUE;
      }
  else if(strcmp(endkey, "REMOVE") == 0)
      {
      offsets->removeMutePairToBuffer(index);
      return TRUE;
      }
  else if(nread > 0 && index == offsets->numMuteTimes())
      {
      offsets->appendMutePair();
      return TRUE;
      } 
  return FALSE;
}



static void offset_trap(void *data, long /*ident*/, long index,
                          float fvar, long nread, char* endkey)
{
  TABLE
  OFFSETS
  int yes = maybe_remove_insert_append(offsets, index, nread, endkey);
  if(nread > 0) offsets->setMuteOffset(index, fvar);
  if(nread > 0 || yes) immediate_action(table);
}



static void time_trap(void *data, long /*ident*/, long index,
                          float fvar, long nread, char* endkey)
{
  TABLE
  OFFSETS
  int yes = maybe_remove_insert_append(offsets, index, nread, endkey);
  if(nread > 0) offsets->setMuteTime(index, fvar);
  if(nread > 0 || yes) immediate_action(table);
}



//------------------------- update functions --------------------//
//------------------------- update functions --------------------//
//------------------------- update functions --------------------//


static long immediate_update(void *data, long /*ident*/, long /*index*/)
{
  TABLE
  return table->getImmediate();
}


static long muteflag_update(void *data, long ident, long /*index*/)
{
  TABLE
  OFFSETS
  return (offsets->getMuteFlag() == ident);
}


static float minimum_update(void *data, long /*ident*/, long /*index*/)
{
  TABLE
  OFFSETS
  return offsets->getMinimumOffset();
}


static float maximum_update(void *data, long /*ident*/, long /*index*/)
{
  TABLE
  OFFSETS
  return offsets->getMaximumOffset();
}


static float offset_update(void *data, long /*ident*/, long index)
{
  TABLE
  OFFSETS
  return offsets->getMuteOffset(index);
}


static float time_update(void *data, long /*ident*/, long index)
{
  TABLE
  OFFSETS
  return offsets->getMuteTime(index);
}



static long n_update(void *data)
{
  TABLE
  OFFSETS
  return offsets->numMuteTimes();
}


static long nmax_update(void *data)
{
  TABLE
  OFFSETS
  return offsets->numMuteTimes() + 1;
}



//---------------------- switch update functions ------------------//
//---------------------- switch update functions ------------------//
//---------------------- switch update functions ------------------//


static long array_switch_update(void *data, long /*ident*/, long /*index*/)
{
  TABLE
  OFFSETS
  if(offsets->getMuteFlag() == VfOffsets::MUTEFLAG_ARRAY) return 1;
  return -1;
}



//--------------------------- make helper -----------------------------//
//--------------------------- make helper -----------------------------//
//--------------------------- make helper -----------------------------//

void VfboxMute::makeHelper()
{
  static long p0  =   0; 
  static long p1  =   1; 
  static long p2  =   2; 
  static long p3  =   3; 
  static long p4  =   4; 

    //        ID        PROMPT                          JSW  ISW   R  C  NC ND
  regIvar3 (IMMEDIATE, "Immediate action"             , &p0, &p3,  1, 1, 2);
  regMsg   (          "      (see help)"              ,            2, 1);
  regString(METERS   ,"Convert feet to meters"        ,      &p2,  1,26);
  regString(FEET     ,"Convert meters to feet"        ,      &p2,  2,26);
  regFvar2 (MINIMUM  ,"Minimum offset"                , &p0, &p1,  4, 1, 6, 0);
  regFvar2 (MAXIMUM  ,"Maximum offset"                , &p0, &p1, -1,27, 6, 0);
  regIvar3 (VfOffsets::MUTEFLAG_NONE,
                      "Do not mute the data"          , &p0, &p4,  0, 1, 2);
  regIvar3 (VfOffsets::MUTEFLAG_MATCH,
                      "Mute at maximum offset = depth", &p0, &p4,  0, 1, 2);
  regIvar3 (VfOffsets::MUTEFLAG_ARRAY,
                      "Use the following mute after NMO correction",
                                                        &p0, &p4,  0, 1, 2);

    //        N        NMAX       ROW  COL NCHAR MAXROWS
  regArrays(n_update, nmax_update, 0,  11,   3,    5);

    //        ID        PROMPT     JSW      ISW  COL NCHAR NDEC
  regFarray (MOFFSET, "OFFSET"   , &p0  ,   &p1,  0,  8,  0);
  regFarray (MTIME  , "MUTE TIME", &p0  ,   &p1,  0,  8,  3);

  funIvar (IMMEDIATE, immediate_trap, immediate_update);
  funCvar (METERS   ,    meters_trap);
  funCvar (FEET     ,      feet_trap);
  funFvar (MINIMUM  ,   minimum_trap, minimum_update);
  funFvar (MAXIMUM  ,   maximum_trap, maximum_update);

  funIvar (VfOffsets::MUTEFLAG_NONE , muteflag_trap, muteflag_update);
  funIvar (VfOffsets::MUTEFLAG_MATCH, muteflag_trap, muteflag_update);
  funIvar (VfOffsets::MUTEFLAG_ARRAY, muteflag_trap, muteflag_update);

  funFvar (MOFFSET  ,   offset_trap, offset_update, array_switch_update);
  funFvar (MTIME    ,     time_trap,   time_update, array_switch_update);
}



//-------------------------- end -------------------------------//
//-------------------------- end -------------------------------//
//-------------------------- end -------------------------------//
