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

//---------------------- vfbox_list.cc -----------------------//
//---------------------- vfbox_list.cc -----------------------//
//---------------------- vfbox_list.cc -----------------------//

//          implementation file for the VfboxList class
//                derived from the SLDatabox class
//                     subdirectory vfgui


#include "vfgui/vfbox_list.hh"
#include "vf/vf_manager.hh"
#include "vf/vf_dataset.hh"
#include "sl/sl_prim.hh"
#include "named_constants.h"
#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <assert.h>


enum { ACTIVE = 1, REF, SELECT, NHX, NHY, NFUN, LOCK, PROMPT, PSELECT, PLOCK };

enum { SHOW_READ = 55, SHOW_SAVED, SHOW_BACKUP, SHOW_STATUS, SHOW_NUMBERS,
       SHOW_MINIMA, SHOW_MAXIMA, SHOW_MINVELS, SHOW_MAXVELS };

enum { FIRST_SHOW = SHOW_READ, LAST_SHOW  = SHOW_MAXVELS    };
//enum { LAST_SHOW  = SHOW_MAXVELS };


//---------------------- constructor -----------------------//
//---------------------- constructor -----------------------//
//---------------------- constructor -----------------------//


VfboxList::VfboxList(SLDelay *slparent, char *name, VfManager *manager)
           : SLDatabox(slparent, name, NULL, 4),
                     _manager   (manager),
                     _show      (SHOW_STATUS)
{
  assert(_manager);
}



//------------------------ destructor --------------------------//
//------------------------ destructor --------------------------//
//------------------------ destructor --------------------------//

VfboxList::~VfboxList()
{
}



//------------------------ set or step show option ----------------------//
//------------------------ set or step show option ----------------------//
//------------------------ set or step show option ----------------------//

       // public.

void VfboxList::setShowOption(int show)
{
  _show = ConstrainValue(show, FIRST_SHOW, LAST_SHOW);
}


void VfboxList::stepShowOption(int direction)
{
  _show += direction;
  if(_show >  LAST_SHOW) _show = FIRST_SHOW;
  if(_show < FIRST_SHOW) _show =  LAST_SHOW;
}



//---------------------- trap functions --------------------------//
//---------------------- trap functions --------------------------//
//---------------------- trap functions --------------------------//



static void select_all_trap(void *data, long /*ident*/, long /*index*/,
                            char* /*value*/, long /*nread*/, char *endkey)
{
  if(strcmp(endkey, "RETURN") != 0) return;
  VfboxList *table = (VfboxList*)data;
  int selected = table->manager()->dataset(0)->isSelected();
  if(selected) table->manager()->unselectAllDatasets();
  else         table->manager()->selectAllDatasets();
}



static void lock_all_trap(void *data, long /*ident*/, long /*index*/,
                            char* /*value*/, long /*nread*/, char *endkey)
{
  if(strcmp(endkey, "RETURN") != 0) return;
  VfboxList *table = (VfboxList*)data;
  int locked = table->manager()->dataset(0)->isLocked();
  if(locked) table->manager()->unlockAllDatasets();
  else       table->manager()->lockAllDatasets();
}



static void active_trap(void *data, long /*ident*/, long index,
                            long /*value*/, long nread, char* /*endkey*/)
{
  if(nread == 0) return;
  VfboxList *table = (VfboxList*)data;
  table->manager()->setActiveDatasetIndex(index);
}



static void ref_trap(void *data, long /*ident*/, long index,
                            long /*value*/, long nread, char* /*endkey*/)
{
  if(nread == 0) return;
  VfboxList *table = (VfboxList*)data;
  table->manager()->setReferenceDatasetIndex(index);
}



static void select_trap(void *data, long /*ident*/, long index,
                            long /*value*/, long nread, char* /*endkey*/)
{
  if(nread == 0) return;
  VfboxList *table   = (VfboxList*)data;
  VfDataset *dataset = table->manager()->dataset(index);
  if(dataset->isSelected()) dataset->unselectThisDataset();
  else                      dataset->selectThisDataset();
}



static void lock_trap(void *data, long /*ident*/, long index,
                            long /*value*/, long nread, char* /*endkey*/)
{
  if(nread == 0) return;
  VfboxList *table   = (VfboxList*)data;
  VfDataset *dataset = table->manager()->dataset(index);
  if(dataset->isLocked()) dataset->unlockData();
  else                    dataset->lockData();
}



static void prompt_trap(void *data, long /*ident*/, long /*index*/,
                            char* /*value*/, long /*nread*/, char* endkey)
{
  VfboxList *table = (VfboxList*)data;
  if     (strcmp(endkey, "RETURN" ) == 0) table->stepShowOption( 1);
  else if(strcmp(endkey, "BUTTON2") == 0) table->stepShowOption(-1);
}



static void show_trap(void *data, long ident, long /*index*/,
                            char* /*value*/, long /*nread*/, char* endkey)
{
  if(strcmp(endkey, "RETURN") != 0) return;
  VfboxList *table = (VfboxList*)data;
  table->setShowOption((int)ident);
}



//------------------------- update functions -------------------------//
//------------------------- update functions -------------------------//
//------------------------- update functions -------------------------//


static long active_update(void *data, long /*ident*/, long index)
{
  VfboxList *table = (VfboxList*)data;
  VfDataset  *dataset  = table->manager()->dataset(index);
  return dataset->isActive();
}



static long ref_update(void *data, long /*ident*/, long index)
{
  VfboxList *table = (VfboxList*)data;
  VfDataset  *dataset  = table->manager()->dataset(index);
  return dataset->isReference();
}



static long select_update(void *data, long /*ident*/, long index)
{
  VfboxList *table   = (VfboxList*)data;
  VfDataset *dataset = table->manager()->dataset(index);
  return dataset->isSelected();
}



static long lock_update(void *data, long /*ident*/, long index)
{
  VfboxList *table   = (VfboxList*)data;
  VfDataset *dataset = table->manager()->dataset(index);
  return dataset->isLocked();
}



static char *prompt_update(void *data, long /*ident*/, long /*index*/)
{
  VfboxList *table = (VfboxList*)data;
  static char *buffer1  = " last file read";
  static char *buffer2  = " last file saved";
  static char *buffer3  = " last backup file saved";
  static char *buffer4  = " data save status";
  static char *buffer5  = "     order     #errors  #raytraced";
  static char *buffer7  = " minimum    X coord   Y coord     time     depth";
  static char *buffer8  = " maximum    X coord   Y coord     time     depth";
  static char *buffer9  = " minimum   NMO vel   RMS vel    AV vel   INT vel";
  static char *buffer10 = " maximum   NMO vel   RMS vel    AV vel   INT vel";
  int show = table->getShowOption();
  switch(show)
      {
      case SHOW_READ   : return buffer1;
      case SHOW_SAVED  : return buffer2;
      case SHOW_BACKUP : return buffer3;
      case SHOW_STATUS : return buffer4;
      case SHOW_NUMBERS: return buffer5;
      case SHOW_MINIMA : return buffer7;
      case SHOW_MAXIMA : return buffer8;
      case SHOW_MINVELS: return buffer9;
      case SHOW_MAXVELS: return buffer10;
      default: assert(FALSE);
      }
  return buffer1;
}



static char *text_update(void *data, long /*ident*/, long index)
{
  static char *buffer1 = "dataset needs saving (backed up)";
  static char *buffer2 = "dataset needs saving (not backed up)";
  static char *buffer3 = "dataset does not need saving";
  static char *buffer4 = "(uneditable dataset)";
  VfboxList *table   = (VfboxList*)data;
  VfDataset *dataset = table->manager()->dataset(index);
  int show = table->getShowOption();
  static char buffer[111];
  switch(show)
      {
      case SHOW_READ   : return (char*)dataset->lastFileRead();
      case SHOW_SAVED  : if(dataset->notEditable()) return buffer4;
                         return (char*)dataset->lastFileSaved();
      case SHOW_BACKUP : if(dataset->notEditable()) return buffer4;
                         return (char*)dataset->lastBackupFileSaved();
      case SHOW_STATUS : if(dataset->notEditable()) return buffer4;
                         if(!dataset->dataNeedsSaving()) return buffer3;
                         if(dataset->dataBackedUp()) return buffer1;
                         return buffer2;
      case SHOW_NUMBERS: sprintf(buffer, "%8d   %8d  %8d",
                                 dataset->getMoveoutOrder(),
                                 dataset->numVelocityFunctionsWithErrors(),
                                 dataset->numRaytracedVelocityFunctions());
                         break;
      case SHOW_MINIMA : sprintf(buffer, " minimum  %8.0f  %8.0f  %8.3f  %8.0f",
                                 dataset->minimumXloc(),
                                 dataset->minimumYloc(),
                                 dataset->minimumTime(),
                                 dataset->minimumDepth());
                         break;
      case SHOW_MAXIMA : sprintf(buffer, " maximum  %8.0f  %8.0f  %8.3f  %8.0f",
                                 dataset->maximumXloc(),
                                 dataset->maximumYloc(),
                                 dataset->maximumTime(),
                                 dataset->maximumDepth());
                         break;
      case SHOW_MINVELS: sprintf(buffer, " minimum  %8.0f  %8.0f  %8.0f  %8.0f",
                                 dataset->minimumVnmo(),
                                 dataset->minimumVrms(),
                                 dataset->minimumVav(),
                                 dataset->minimumVint());
                         break;
      case SHOW_MAXVELS: sprintf(buffer, " maximum  %8.0f  %8.0f  %8.0f  %8.0f",
                                 dataset->maximumVnmo(),
                                 dataset->maximumVrms(),
                                 dataset->maximumVav(),
                                 dataset->maximumVint());
                         break;
      default: assert(FALSE);
      }
  return buffer;
}



static long n_update(void *data)
{
  VfboxList *table = (VfboxList*)data;
  return table->manager()->numDatasets();
}



static long nhx_update(void *data, long /*ident*/, long index)
{
  VfboxList *table   = (VfboxList*)data;
  return table->manager()->dataset(index)->getNhx();
}


static long nhy_update(void *data, long /*ident*/, long index)
{
  VfboxList *table   = (VfboxList*)data;
  return table->manager()->dataset(index)->getNhy();
}


static long nfun_update(void *data, long /*ident*/, long index)
{
  VfboxList *table   = (VfboxList*)data;
  return table->manager()->dataset(index)->numVelocityFunctions();
}



//---------------------- switch functions --------------------------//
//---------------------- switch functions --------------------------//
//---------------------- switch functions --------------------------//


static long show_switch_update(void *data, long ident, long /*index*/)
{
  VfboxList *table = (VfboxList*)data;
  int show = table->getShowOption();
  if(ident == show) return 6;
  return 2;
}


static long switch_update(void *data, long ident, long index)
{
  VfboxList *table = (VfboxList*)data;
  int show = table->getShowOption();
  long number;
  if(ident == LOCK) number = table->manager()->numEditableDatasets();
  else              number = table->manager()->numDatasets();
  if(index >= number) return -77;
  if(ident == ACTIVE) return 4;
  if(ident == REF   ) return 4;
  if(ident == SELECT) return 3;
  if(ident == LOCK  ) return 3;
  return 0;
}



//--------------------------- make helper -----------------------------//
//--------------------------- make helper -----------------------------//
//--------------------------- make helper -----------------------------//

void VfboxList::makeHelper()
{
  static long zero  =   0; 
  static long two   =   2; 
  static long three =   3; 
  static long four  =   4; 
  static long m66   = -66; 

    //        ID            PROMPT                  ISW   ROW   COL
  regMsg    (              "active"              ,         1,    1);
  regMsg    (              "comparison"          ,         1,    8);
  regString (PSELECT     , "select"              , &two,   1,   19);
  regString (PLOCK       , "lock"                , &two,   1,   31);
  regMsg    (              "LIST OF ALL DATASETS",         1,   43);
  regString (SHOW_READ   , "R"                   , &two,   1,   68);
  regString (SHOW_SAVED  , "S"                   , &two,   1,   -1);
  regString (SHOW_BACKUP , "B"                   , &two,   1,   -1);
  regString (SHOW_STATUS , "D"                   , &two,   1,   -1);
  regString (SHOW_NUMBERS, "#"                   , &two,   1,   -1);
  regString (SHOW_MINIMA , "-"                   , &two,   1,   -1);
  regString (SHOW_MAXIMA , "+"                   , &two,   1,   -1);
  regString (SHOW_MINVELS, "-"                   , &two,   1,   -1);
  regString (SHOW_MAXVELS, "+"                   , &two,   1,   -1);

    //        N        NMAX    ROW COL NCHAR MAXROWS
  regArrays(n_update, n_update, 2,  0,   2,    35);

    //        ID       PROMPT          JSW      ISW     COL NCHAR NDEC
  regIarray (ACTIVE, "||"           , &m66  ,  &four ,   0,    2);
  regIarray (REF   , "||"           , &m66  ,  &four ,   0,    2);
  regIarray (NHX   , "nhx"          , &zero ,  &zero ,   0,    2);
  regIarray (NHY   , "nhy"          , &zero ,  &zero ,   0,    2);
  regIarray (SELECT, "||"           , &m66  ,  &three,   0,    2);
  regIarray (NFUN  , "#functions"   , &zero ,  &zero ,   0,    7);
  regIarray (LOCK  , "||"           , &m66  ,  &three,   0,    2);
  regCarray (PROMPT, NULL           , &two  ,  &zero ,   0,   53);

  funCvar  (PSELECT,  select_all_trap);
  funCvar  (PLOCK  ,    lock_all_trap);
  funCvar  (-PROMPT,      prompt_trap, prompt_update);
  funCvar  (SHOW_READ   ,   show_trap, NULL, show_switch_update);
  funCvar  (SHOW_SAVED  ,   show_trap, NULL, show_switch_update);
  funCvar  (SHOW_BACKUP ,   show_trap, NULL, show_switch_update);
  funCvar  (SHOW_STATUS ,   show_trap, NULL, show_switch_update);
  funCvar  (SHOW_NUMBERS,   show_trap, NULL, show_switch_update);
  funCvar  (SHOW_MINIMA ,   show_trap, NULL, show_switch_update);
  funCvar  (SHOW_MAXIMA ,   show_trap, NULL, show_switch_update);
  funCvar  (SHOW_MINVELS,   show_trap, NULL, show_switch_update);
  funCvar  (SHOW_MAXVELS,   show_trap, NULL, show_switch_update);

  funIvar  ( ACTIVE,      active_trap, active_update, switch_update);
  funIvar  ( REF   ,         ref_trap,    ref_update, switch_update);
  funIvar  ( SELECT,      select_trap, select_update, switch_update);
  funIvar  ( NHX,                NULL,    nhx_update);
  funIvar  ( NHY,                NULL,    nhy_update);
  funIvar  ( NFUN,               NULL,   nfun_update);
  funIvar  ( LOCK,          lock_trap,   lock_update, switch_update);
  funCvar  ( PROMPT,             NULL,   text_update);
}



//-------------------------- end -------------------------------//
//-------------------------- end -------------------------------//
//-------------------------- end -------------------------------//
