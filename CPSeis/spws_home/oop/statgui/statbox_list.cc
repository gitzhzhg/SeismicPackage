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

//---------------------- statbox_list.cc -----------------------//
//---------------------- statbox_list.cc -----------------------//
//---------------------- statbox_list.cc -----------------------//

//          implementation file for the StatboxList class
//                derived from the SLDatabox class
//                     subdirectory statgui


#include "statgui/statbox_list.hh"
#include "stat/static_manager.hh"
#include "stat/static_dataset.hh"
#include "sl/sl_prim.hh"
#include "str.h"
#include <iostream.h>
#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <assert.h>


enum { ACTIVE = 1, REF, SELECT, NHX, NHY, NHX2, NHY2,
       LOCK, PROMPT, NX, NY, PSELECT, PLOCK };

#define TABLE  StatboxList *table = (StatboxList*)data;


//---------------------- constructor -----------------------//
//---------------------- constructor -----------------------//
//---------------------- constructor -----------------------//


StatboxList::StatboxList(SLDelay *slparent, StaticManager *manager)
           : SLDatabox(slparent, "statbox_list", NULL, 4),
                     _manager   (manager),
                     _show      (SHOW_STATUS)
{
  assert(_manager);
}



//------------------------ destructor --------------------------//
//------------------------ destructor --------------------------//
//------------------------ destructor --------------------------//

StatboxList::~StatboxList()
{
}



//------------------------ set or step show option ----------------------//
//------------------------ set or step show option ----------------------//
//------------------------ set or step show option ----------------------//

       // public.

void StatboxList::setShowOption(int show)
{
  assert(show == SHOW_READ   || show == SHOW_SAVE || show == SHOW_BACKUP ||
         show == SHOW_STATUS || show == SHOW_NILS || show == SHOW_BINS   ||
         show == SHOW_MINMAX);
  _show = show;
}



void StatboxList::stepShowOption(int step)
{
  int first = SHOW_READ;
  int last  = SHOW_MINMAX;
  _show += step;
  if(_show > last)  _show = first;
  if(_show < first) _show = last;
}



//---------------------- traps ------------------------------//
//---------------------- traps ------------------------------//
//---------------------- traps ------------------------------//



static void select_all_trap(void *data, long /*ident*/, long /*index*/,
                            char* /*value*/, long /*nread*/, char *endkey)
{
  if(strcmp(endkey, "RETURN") != 0) return;
  TABLE
  int selected = table->manager()->dataset(0)->isSelected();
  if(selected) table->manager()->unselectAllDatasets();
  else         table->manager()->selectAllDatasets();
}



static void lock_all_trap(void *data, long /*ident*/, long /*index*/,
                            char* /*value*/, long /*nread*/, char *endkey)
{
  if(strcmp(endkey, "RETURN") != 0) return;
  TABLE
  int locked = table->manager()->dataset(0)->isLocked();
  if(locked) table->manager()->unlockAllDatasets();
  else       table->manager()->lockAllDatasets();
}



static void active_trap(void *data, long /*ident*/, long index,
                            long /*value*/, long nread, char* /*endkey*/)
{
  if(nread == 0) return;
  TABLE
  table->manager()->setActiveDatasetIndex(index);
}


static void ref_trap(void *data, long /*ident*/, long index,
                            long /*value*/, long nread, char* /*endkey*/)
{
  if(nread == 0) return;
  TABLE
  table->manager()->setReferenceDatasetIndex(index);
}



static void select_trap(void *data, long /*ident*/, long index,
                            long /*value*/, long nread, char* /*endkey*/)
{
  if(nread == 0) return;
  TABLE
  StaticDataset *dataset = table->manager()->dataset(index);
  if(dataset->isSelected()) dataset->unselectThisDataset();
  else                      dataset->selectThisDataset();
}



static void lock_trap(void *data, long /*ident*/, long index,
                            long /*value*/, long nread, char* /*endkey*/)
{
  if(nread == 0) return;
  TABLE
  StaticDataset *dataset = table->manager()->dataset(index);
  if(dataset->isLocked()) dataset->unlockData();
  else                    dataset->lockData();
}



static void prompt_trap(void *data, long /*ident*/, long /*index*/,
                            char* /*value*/, long /*nread*/, char* endkey)
{
  TABLE
  if     (strcmp(endkey, "RETURN" ) == 0) table->stepShowOption( 1);
  else if(strcmp(endkey, "BUTTON2") == 0) table->stepShowOption(-1);
}



static void show_trap(void *data, long ident, long /*index*/,
                            char* /*value*/, long /*nread*/, char* endkey)
{
  if(strcmp(endkey, "RETURN") != 0) return;
  TABLE
  table->setShowOption((int)ident);
}



//---------------- variable update functions --------------------//
//---------------- variable update functions --------------------//
//---------------- variable update functions --------------------//


static long active_update(void *data, long /*ident*/, long index)
{
  TABLE
  StaticDataset *dataset = table->manager()->dataset(index);
  return dataset->isActive();
}


static long ref_update(void *data, long /*ident*/, long index)
{
  TABLE
  StaticDataset *dataset = table->manager()->dataset(index);
  return dataset->isReference();
}



static long select_update(void *data, long /*ident*/, long index)
{
  TABLE
  StaticDataset *dataset = table->manager()->dataset(index);
  return dataset->isSelected();
}



static long lock_update(void *data, long /*ident*/, long index)
{
  TABLE
  StaticDataset *dataset = table->manager()->dataset(index);
  return dataset->isLocked();
}



static char *prompt_update(void *data, long /*ident*/, long /*index*/)
{
  TABLE
  static char *buffer1 = "           last file read";
  static char *buffer2 = "           last file saved";
  static char *buffer3 = "        last backup file saved";
  static char *buffer4 = "           data save status";
  static char *buffer5 = "   type       #values    #nils    #live";
  static char *buffer6 = "   x1    xinc   xend  |  y1    yinc   yend";
  static char *buffer7 = "     minimum     maximum     average";
  int show = table->getShowOption();
  switch(show)
      {
      case StatboxList::SHOW_READ  : return buffer1;
      case StatboxList::SHOW_SAVE  : return buffer2;
      case StatboxList::SHOW_BACKUP: return buffer3;
      case StatboxList::SHOW_STATUS: return buffer4;
      case StatboxList::SHOW_NILS  : return buffer5;
      case StatboxList::SHOW_BINS  : return buffer6;
      case StatboxList::SHOW_MINMAX: return buffer7;
      default: assert(FALSE);
      }
  return buffer1;
}



static char *text_update(void *data, long /*ident*/, long index)
{
  static char *buffer1 = "dataset needs saving (backed up)";
  static char *buffer2 = "dataset needs saving (not backed up)";
  static char *buffer3 = "dataset does not need saving";
  TABLE
  StaticDataset *dataset = table->manager()->dataset(index);
  int show = table->getShowOption();
  static char buffer[111];
  switch(show)
      {
      case StatboxList::SHOW_READ:
                        return (char*)dataset->lastFileRead();
      case StatboxList::SHOW_SAVE:
                        return (char*)dataset->lastFileSaved();
      case StatboxList::SHOW_BACKUP:
                        return (char*)dataset->lastBackupFileSaved();
      case StatboxList::SHOW_STATUS:
                        if(!dataset->dataNeedsSaving()) return buffer3;
                        if(dataset->dataBackedUp()) return buffer1;
                        return buffer2;
      case StatboxList::SHOW_NILS:
                        sprintf(buffer, "   %-8s  %8d %8d %8d",
                                dataset->getStattype(),
                                dataset->numValues(),
                                dataset->numNilValues(),
                                dataset->numLiveValues());
                        break;
      case StatboxList::SHOW_BINS:
                        {
                        float x1   = dataset->getX1();
                        float y1   = dataset->getY1();
                        float xinc = dataset->getXinc();
                        float yinc = dataset->getYinc();
                        float xend = dataset->getXend();
                        float yend = dataset->getYend();
                        int nchar = 6, ndec = 3;
                        char sx1[16], sxinc[16], sxend[16];
                        char sy1[16], syinc[16], syend[16];
                        str_ff2ss_simple(x1  , sx1  , nchar, ndec);
                        str_ff2ss_simple(y1  , sy1  , nchar, ndec);
                        str_ff2ss_simple(xinc, sxinc, nchar, ndec);
                        str_ff2ss_simple(yinc, syinc, nchar, ndec);
                        str_ff2ss_simple(xend, sxend, nchar, ndec);
                        str_ff2ss_simple(yend, syend, nchar, ndec);
                        sprintf(buffer, "  %-6s %-6s %-6s  %-6s %-6s %-6s",
                            sx1, sxinc, sxend, sy1, syinc, syend);
                        }
                        break;
      case StatboxList::SHOW_MINMAX:
                        sprintf(buffer, "   %9.1f   %9.1f   %9.1f",
                                dataset->minimumValue(),
                                dataset->maximumValue(),
                                dataset->averageValue());
                        break;
      default: assert(FALSE);
      }
  return buffer;
}



static long n_update(void *data)
{
  TABLE
  return table->manager()->numDatasets();
}



#define UPDATE(long2, nhx_update, getNhx)                         \
static long2 nhx_update(void *data, long /*ident*/, long index)   \
{                                                                 \
  TABLE                                                           \
  StaticDataset *dataset = table->manager()->dataset(index);      \
  return dataset->getNhx();                                       \
}

  UPDATE(long ,  nhx_update, getNhx )
  UPDATE(long ,  nhy_update, getNhy )
  UPDATE(long , nhx2_update, getNhx2)
  UPDATE(long , nhy2_update, getNhy2)
  UPDATE(long ,   nx_update, getNx  )
  UPDATE(long ,   ny_update, getNy  )



//--------------------- switch update functions -------------------------//
//--------------------- switch update functions -------------------------//
//--------------------- switch update functions -------------------------//


static long show_switch_update(void *data, long ident, long /*index*/)
{
  TABLE
  int show = table->getShowOption();
  if(ident == show) return 6;
  return 2;
}


static long switch_update(void *data, long ident, long index)
{
  TABLE
  long number = table->manager()->numDatasets();
  if(index >= number) return -77;
  if(ident == ACTIVE) return 4;
  if(ident == REF   ) return 4;
  if(ident == SELECT) return 3;
  if(ident == LOCK)   return 3;
  return 0;
}



//---------------------- make helper ------------------------//
//---------------------- make helper ------------------------//
//---------------------- make helper ------------------------//

void StatboxList::makeHelper()
{
  static long zero  =   0; 
  static long two   =   2; 
  static long three =   3; 
  static long four  =   4; 
  static long m66   = -66; 

    //        ID           PROMPT                  ISW    ROW   COL
  regString (0          , "active"              , &zero,   1,    1);
  regString (0          , "ref"                 , &zero,   1,    8);
  regString (PSELECT    , "select"              , &two ,   1,   12);
  regString (PLOCK      , "lock"                , &two ,   1,   29);
  regString (SHOW_READ  , "read"                , &two ,   1,   37);
  regString (SHOW_SAVE  , "save"                , &two ,   1,   -1);
  regString (SHOW_BACKUP, "backup"              , &two ,   1,   -1);
  regString (SHOW_STATUS, "status"              , &two ,   1,   -1);
  regString (SHOW_NILS  , "nils"                , &two ,   1,   -1);
  regString (SHOW_BINS  , "bins"                , &two ,   1,   -1);
  regString (SHOW_MINMAX, "minmax"              , &two ,   1,   -1);

    //        N        NMAX    ROW COL NCHAR MAXROWS
  regArrays(n_update, n_update, 0,  0,   2,    35);

    //      ID       PROMPT         SWITCH   SWITCH    COL NCHAR NDEC
  regIarray(ACTIVE, "||"           , &m66  ,  &four ,   0,    2);
  regIarray(REF   , "||"           , &m66  ,  &four ,   0,    2);
  regIarray(SELECT, "||"           , &m66  ,  &three,   0,    2);
  regIarray(NHX   , "nhx"          , &zero ,  &zero ,   0,    2);
  regIarray(NHY   , "nhy"          , &zero ,  &zero ,   0,    2);
  regIarray(NHX2  , "nhx2"         , &zero ,  &zero ,   0,    2);
  regIarray(NHY2  , "nhy2"         , &zero ,  &zero ,   0,    2);
  regIarray(LOCK  , "||"           , &m66  ,  &three,   0,    2);
  regCarray(PROMPT, NULL           , &two  ,  &zero ,   0,   45);
  regIarray(NX    , "nx"           , &zero ,  &zero ,   0,    5);
  regIarray(NY    , "ny"           , &zero ,  &zero ,   0,    5);

  funCvar  (PSELECT, select_all_trap);
  funCvar  (PLOCK  ,   lock_all_trap);
  funCvar  (-PROMPT,     prompt_trap,  prompt_update);

  funIvar  (ACTIVE ,     active_trap,  active_update,  switch_update);
  funIvar  (REF    ,        ref_trap,     ref_update,  switch_update);
  funIvar  (SELECT ,     select_trap,  select_update,  switch_update);
  funIvar  (NHX    ,            NULL,     nhx_update,  switch_update);
  funIvar  (NHY    ,            NULL,     nhy_update,  switch_update);
  funIvar  (NHX2   ,            NULL,    nhx2_update,  switch_update);
  funIvar  (NHY2   ,            NULL,    nhy2_update,  switch_update);
  funIvar  (LOCK   ,       lock_trap,    lock_update,  switch_update);
  funCvar  (PROMPT ,            NULL,    text_update,  switch_update);
  funIvar  (NX     ,            NULL,      nx_update,  switch_update);
  funIvar  (NY     ,            NULL,      ny_update,  switch_update);

  funCvar  (SHOW_READ  ,   show_trap,  NULL,  show_switch_update);
  funCvar  (SHOW_SAVE  ,   show_trap,  NULL,  show_switch_update);
  funCvar  (SHOW_BACKUP,   show_trap,  NULL,  show_switch_update);
  funCvar  (SHOW_STATUS,   show_trap,  NULL,  show_switch_update);
  funCvar  (SHOW_NILS  ,   show_trap,  NULL,  show_switch_update);
  funCvar  (SHOW_BINS  ,   show_trap,  NULL,  show_switch_update);
  funCvar  (SHOW_MINMAX,   show_trap,  NULL,  show_switch_update);
}



//-------------------------- end -------------------------------//
//-------------------------- end -------------------------------//
//-------------------------- end -------------------------------//
