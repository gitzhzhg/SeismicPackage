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

//-------------------------- vfbox_fun.cc ---------------------------//
//-------------------------- vfbox_fun.cc ---------------------------//
//-------------------------- vfbox_fun.cc ---------------------------//

//          implementation file for the VfboxFun class
//                derived from the SLDatabox class
//                derived from the VfInform class
//                     subdirectory vfgui


#include "vfgui/vfbox_fun.hh"
#include "vf/vf_manager.hh"
#include "vf/vf_dataset.hh"
#include "vf/vf_informer.hh"
#include "vf/vf_utilities.hh"
#include "vf/vf_update.hh"
#include "vf/vf_constants.hh"
#include "vf/vf_edit_sort.hh"
#include "sl/sl_prim.hh"
#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <assert.h>


enum { NSEL = 1, NERR, NRAY, PSELECT, NHX, NHY, ORDER,        // scalars
       XINCR, XDECR, XEITHER, YINCR, YDECR, YEITHER,          // scalars
       ACTIVE, NEXT, REF, NAME, SELECT, XCOORD, YCOORD,       // arrays
       TYPE, EEE, RRR,                                        // arrays
       PICKS, PROJ, LINE, RDATE, PDATE, ID, COMM,             // arrays
       XFAST, YFAST };                                        // scalars


//-------------------------- constructor -------------------------------//
//-------------------------- constructor -------------------------------//
//-------------------------- constructor -------------------------------//


VfboxFun::VfboxFun(SLDelay *slparent, char *name,
                                  VfManager *manager, VfEditSort *edit)
           : SLDatabox(slparent, name, NULL, 4),
             VfInform(manager),
                _edit  (edit)
{
}



//---------------------------- destructor ------------------------------//
//---------------------------- destructor ------------------------------//
//---------------------------- destructor ------------------------------//

VfboxFun::~VfboxFun()
{
}



//----------------- virtual functions overriding VfInform ------------//
//----------------- virtual functions overriding VfInform ------------//
//----------------- virtual functions overriding VfInform ------------//

       // private.

void VfboxFun::postNewActiveVelocityFunction(VfDataset *dataset)
{
  postTotalChanges(dataset);
}


void VfboxFun::postTotalChanges(VfDataset *dataset)
{
  if(dataset->notActive()) return;
  long act = dataset->getActiveVelocityFunction();
  if(act == -1) act = dataset->numVelocityFunctions();
  setFocus(ACTIVE, (int)act);
}



//----------------------- maybe remove insert ----------------------------//
//----------------------- maybe remove insert ----------------------------//
//----------------------- maybe remove insert ----------------------------//

     // to be called from a trap.
     // removes velocity function if endkey is REMOVE.
     // inserts velocity function if endkey is INSERT.
     // returns done = TRUE if removed or inserted.
     // otherwise returns done = FALSE.

static int maybe_remove_insert(VfboxFun *table, VfDataset *dataset,
                               long index, char *endkey)
{
  if(strcmp(endkey, "INSERT") == 0)
      {
      table->disableMessages();
      dataset->insertVelocityFunctionFromBuffer(index);
      table->enableMessages();
      return TRUE;
      }
  if(strcmp(endkey, "REMOVE") == 0)
      {
      long nfun = dataset->numVelocityFunctions();
      if(index == nfun) return TRUE;        // nothing to remove.
      table->disableMessages();
      dataset->removeVelocityFunctionToBuffer(index);
      table->enableMessages();
      return TRUE;
      }
  return FALSE;
}



//----------------------- maybe append ----------------------------//
//----------------------- maybe append ----------------------------//
//----------------------- maybe append ----------------------------//

     // to be called from a trap.
     // appends velocity function if index == nfun.

static void maybe_append(VfboxFun *table, VfDataset *dataset, long index)
{
  long nfun = dataset->numVelocityFunctions();
  if(index == nfun)
      {
      table->disableMessages();
      dataset->appendVelocityFunction();
      table->enableMessages();
      }
}



//------------------------- trap functions -----------------------------//
//------------------------- trap functions -----------------------------//
//------------------------- trap functions -----------------------------//


static void pselect_trap(void *data, long /*ident*/, long /*index*/,
                         char* /*cvar*/, long /*nread*/, char *endkey)
{
  VfboxFun   *table   = (VfboxFun*)data;
  VfDataset  *dataset = table->manager()->activeDataset();
  long        nfun    = dataset->numVelocityFunctions();
  if(nfun == 0) return;
  if(strcmp(endkey, "ARRIVED") == 0)
      {
      table->SLDatabox::showMessage("press to set or clear all selections");
      return;
      }
  if(strcmp(endkey, "RETURN") != 0) return;
  if(dataset->velocityFunctionIsSelected(0))
      {
      dataset->clearSelectFlags();
      }
  else
      {
      dataset->informer()->beforeChanges();
      dataset->clearSelectFlags();
      dataset->setSelectFlag(0, SELECT_TOP);
      dataset->informer()->afterChanges();
      }
}



static void ivar_trap(void *data, long ident, long /*index*/,
                      long ivar, long nread, char* /*endkey*/)
{
  if(nread == 0) return;
  VfboxFun   *table   = (VfboxFun*)data;
  VfEditSort *edit    = table->edit();
  VfDataset  *dataset = table->manager()->activeDataset();
  switch(ident)
      {
      case NHX    : dataset->setNhx         ((int)ivar); break;
      case NHY    : dataset->setNhy         ((int)ivar); break;
      case ORDER  : dataset->setMoveoutOrder((int)ivar); break;
      case XINCR  : edit->setXdirection(VfEditSort::DIR_ASCENDING ); break;
      case XDECR  : edit->setXdirection(VfEditSort::DIR_DESCENDING); break;
      case XEITHER: edit->setXdirection(VfEditSort::DIR_EITHER    ); break;
      case XFAST  : edit->setXfast     (TRUE);                       break;
      case YINCR  : edit->setYdirection(VfEditSort::DIR_ASCENDING ); break;
      case YDECR  : edit->setYdirection(VfEditSort::DIR_DESCENDING); break;
      case YEITHER: edit->setYdirection(VfEditSort::DIR_EITHER    ); break;
      case YFAST  : edit->setXfast     (FALSE);                      break;
      default: assert(FALSE);
      }
}



static void fvar_trap(void *data, long ident, long index,
                      float fvar, long nread, char *endkey)
{
  VfboxFun   *table   = (VfboxFun*)data;
  VfDataset  *dataset = table->manager()->activeDataset();
  int done = maybe_remove_insert(table, dataset, index, endkey);
  if(done) return;
  if(nread == 0) return;
  dataset->informer()->beforeChanges();
  maybe_append(table, dataset, index);
  switch(ident)
      {
      case XCOORD: dataset->setXloc(index, fvar); break;
      case YCOORD: dataset->setYloc(index, fvar); break;
      default: assert(FALSE);
      }
  dataset->informer()->afterChanges();
}



static void ivar9_trap(void *data, long /*ident*/, long index,
                      long /*ivar*/, long /*nread*/, char *endkey)
{
  VfboxFun   *table   = (VfboxFun*)data;
  VfDataset  *dataset = table->manager()->activeDataset();
  maybe_remove_insert(table, dataset, index, endkey);
}


static void cvar9_trap(void *data, long /*ident*/, long index,
                      char* /*cvar*/, long /*nread*/, char *endkey)
{
  VfboxFun   *table   = (VfboxFun*)data;
  VfDataset  *dataset = table->manager()->activeDataset();
  maybe_remove_insert(table, dataset, index, endkey);
}



static void cvar_trap(void *data, long ident, long index,
                      char *cvar, long nread, char *endkey)
{
  VfboxFun   *table   = (VfboxFun*)data;
  VfDataset  *dataset = table->manager()->activeDataset();
  int done = maybe_remove_insert(table, dataset, index, endkey);
  if(done) return;
  if(nread == 0) return;
  dataset->informer()->beforeChanges();
  maybe_append(table, dataset, index);
  switch(ident)
      {
      case NAME   : dataset->setVfid   (index, cvar); break;
      case PROJ   : dataset->setProject(index, cvar); break;
      case LINE   : dataset->setLine   (index, cvar); break;
      case RDATE  : dataset->setRdate  (index, cvar); break;
      case PDATE  : dataset->setPdate  (index, cvar); break;
      case ID     : dataset->setUserid (index, cvar); break;
      case COMM   : dataset->setComment(index, cvar); break;
      default: assert(FALSE);
      }
  dataset->informer()->afterChanges();
}



static void active_trap(void *data, long ident, long index,
                        long /*ivar*/, long /*nread*/, char *endkey)
{
  VfboxFun   *table   = (VfboxFun*)data;
  VfDataset  *dataset = table->manager()->activeDataset();
  int done = maybe_remove_insert(table, dataset, index, endkey);
  if(done) return;
  if(strcmp(endkey, "RETURN") != 0) return;
  dataset->informer()->beforeChanges();
  maybe_append(table, dataset, index);
  if(index == dataset->numVelocityFunctions()) index = -1;
  if(ident == ACTIVE) dataset->setActiveVelocityFunction   (index);
  else                dataset->setReferenceVelocityFunction(index);
  dataset->informer()->afterChanges();
}



static void button_trap(void *data, long ident, long index,
                        char* /*cvar*/, long /*nread*/, char *endkey)
{
  VfboxFun   *table   = (VfboxFun*)data;
  VfDataset  *dataset = table->manager()->activeDataset();
  int done = maybe_remove_insert(table, dataset, index, endkey);
  if(done) return;
  if(strcmp(endkey, "ARRIVED") == 0)
      {
      if   (ident == SELECT) table->SLDatabox::showMessage
                                     ("press to increment this selection");
      else if(ident == TYPE) table->SLDatabox::showMessage
                                     (dataset->getTypeDescription(index));
      return;
      }
  if(strcmp(endkey, "RETURN") != 0) return;
  dataset->informer()->beforeChanges();
  maybe_append(table, dataset, index);
  int type;
  switch(ident)
      {
      case SELECT : dataset->incrementSelectFlag(index); break;
      case TYPE   : type = dataset->getDefaultType(index);
                    type++;
                    if(type > LASTTYPE) type = FIRSTTYPE;
                    dataset->setDefaultType(index, type);
                    break;
      default: assert(FALSE);
      }
  dataset->informer()->afterChanges();
}



//------------------------- update functions -------------------------//
//------------------------- update functions -------------------------//
//------------------------- update functions -------------------------//


static long ivar2_update(void *data, long ident, long index)
{
  VfboxFun   *table   = (VfboxFun*)data;
  VfDataset  *dataset = table->manager()->activeDataset();
  switch(ident)
    {
    case ACTIVE : return (dataset->getActiveVelocityFunction   () == index);
    case REF    : return (dataset->getReferenceVelocityFunction() == index);
    case PICKS  : return dataset->numPicks(index);
    default: assert(FALSE);
    }
  assert(FALSE);
  return 0;
}



static long ivar_update(void *data, long ident, long /*index*/)
{
  VfboxFun   *table   = (VfboxFun*)data;
  VfEditSort *edit    = table->edit();
  VfDataset  *dataset = table->manager()->activeDataset();
  switch(ident)
    {
    case NHX    : return dataset->getNhx         ();
    case NHY    : return dataset->getNhy         ();
    case ORDER  : return dataset->getMoveoutOrder();
    case NSEL   : return dataset->numSelectedVelocityFunctions();
    case NERR   : return dataset->numVelocityFunctionsWithErrors();
    case NRAY   : return dataset->numRaytracedVelocityFunctions();
    case XINCR  : return (edit->getXdirection() == VfEditSort::DIR_ASCENDING);
    case XDECR  : return (edit->getXdirection() == VfEditSort::DIR_DESCENDING);
    case XEITHER: return (edit->getXdirection() == VfEditSort::DIR_EITHER);
    case XFAST  : return edit->getXfast();
    case YINCR  : return (edit->getYdirection() == VfEditSort::DIR_ASCENDING);
    case YDECR  : return (edit->getYdirection() == VfEditSort::DIR_DESCENDING);
    case YEITHER: return (edit->getYdirection() == VfEditSort::DIR_EITHER);
    case YFAST  : return !edit->getXfast();
    default: assert(FALSE);
    }
  assert(FALSE);
  return 0;
}



static float fvar_update(void *data, long ident, long index)
{
  VfboxFun   *table   = (VfboxFun*)data;
  VfDataset  *dataset = table->manager()->activeDataset();
  switch(ident)
      {
      case XCOORD: return dataset->getXloc(index);
      case YCOORD: return dataset->getYloc(index);
      default: assert(FALSE);
    }
  assert(FALSE);
  return 0.0;
}



static char *cvar_update(void *data, long ident, long index)
{
  VfboxFun   *table   = (VfboxFun*)data;
  VfDataset  *dataset = table->manager()->activeDataset();
  static char buffer[2];
  static char *xnext  = "X+";
  static char *xprev  = "X-";
  static char *ynext  = "Y+";
  static char *yprev  = "Y-";
  static char *blank  = " ";
  switch(ident)
      {
      case NAME   : return (char*)dataset->getVfid      (index);
      case PROJ   : return (char*)dataset->getProject   (index);
      case LINE   : return (char*)dataset->getLine      (index);
      case RDATE  : return (char*)dataset->getRdate     (index);
      case PDATE  : return (char*)dataset->getPdate     (index);
      case ID     : return (char*)dataset->getUserid    (index);
      case COMM   : return (char*)dataset->getComment   (index);
      case TYPE   : return (char*)dataset->getTypeSymbol(index);
      case SELECT : buffer[0] = (char)dataset->getSelectFlag(index);
                    return buffer;
      case NEXT   : if(index == dataset->findNextXloc()) return xnext;
                    if(index == dataset->findPrevXloc()) return xprev;
                    if(index == dataset->findNextYloc()) return ynext;
                    if(index == dataset->findPrevYloc()) return yprev;
                    return blank;
      case EEE    : buffer[0] = (char)dataset->getErrorFlag(index);
                    return buffer;
      case RRR    : buffer[0] = (char)dataset->getRaytraceFlag(index);
                    return buffer;
      default: assert(FALSE);
      }
  assert(FALSE);
  return buffer;
}



static long n_update(void *data)
{
  VfboxFun  *table   = (VfboxFun*)data;
  VfDataset *dataset = table->manager()->activeDataset();
  return dataset->numVelocityFunctions();
}


static long nmax_update(void *data)
{
  VfboxFun  *table   = (VfboxFun*)data;
  VfDataset *dataset = table->manager()->activeDataset();
  if(dataset->notEditable() || dataset->isLocked())
                        return dataset->numVelocityFunctions();
  return dataset->numVelocityFunctions() + 1;
}




//------------------------- switch functions -------------------------//
//------------------------- switch functions -------------------------//
//------------------------- switch functions -------------------------//


static long switch_update(void *data, long /*ident*/, long /*index*/)
{
  VfboxFun   *table   = (VfboxFun*)data;
  VfDataset  *dataset = table->manager()->activeDataset();
  if(dataset->notEditable() || dataset->isLocked()) return 5;
  return 1;
}


static long pselect_switch_update(void *data, long /*ident*/, long /*index*/)
{
  VfboxFun   *table   = (VfboxFun*)data;
  VfDataset  *dataset = table->manager()->activeDataset();
  if(dataset->numVelocityFunctions() == 0) return -2;
  return 2;
}


static long string_switch_update(void *data, long /*ident*/, long index)
{
  VfboxFun   *table   = (VfboxFun*)data;
  VfDataset  *dataset = table->manager()->activeDataset();
  long ivar = 1;
  if(dataset->notEditable() || dataset->isLocked()) ivar = 5;
  if(index == dataset->numVelocityFunctions()) return -ivar;
  return ivar;
}


static long type_switch_update(void *data, long /*ident*/, long index)
{
  VfboxFun   *table   = (VfboxFun*)data;
  VfDataset  *dataset = table->manager()->activeDataset();
  long ivar = 2;
  if(dataset->notEditable() || dataset->isLocked()) ivar = 5;
  if(index == dataset->numVelocityFunctions()) return -ivar;
  return ivar;
}



static long select_switch_update(void *data, long /*ident*/, long index)
{
  VfboxFun   *table   = (VfboxFun*)data;
  VfDataset  *dataset = table->manager()->activeDataset();
  if(index >= dataset->numVelocityFunctions()) return -2;
  if(dataset->velocityFunctionIsSelected(index)) return 6;
  return 2;
}



static long misc_switch_update(void *data, long /*ident*/, long index)
{
  VfboxFun   *table   = (VfboxFun*)data;
  VfDataset  *dataset = table->manager()->activeDataset();
  if(index == dataset->numVelocityFunctions()) return -5;
  return 5;
}



//-------------------------- make helper ----------------------------//
//-------------------------- make helper ----------------------------//
//-------------------------- make helper ----------------------------//

void VfboxFun::makeHelper()
{
  static long p0  =   0; 
  static long p1  =   1; 
  static long p2  =   2; 
  static long p3  =   3; 
  static long p4  =   4; 
  static long m5  =  -5; 
  static long p5  =   5; 
  static long m66 = -66; 


     //     ID       PROMPT                         JSW   ISW   ROW  COL NCHAR
  regIvar2 (NSEL   ,"velocity functions selected:", &p0,  &m5,   1,   1,  6);
  regIvar2 (NERR   ," with errors:"               , &p0,  &m5,  -1,  -1,  6);
  regIvar2 (NRAY   ," raytraced:"                 , &p0,  &m5,  -1,  -1,  6);
  regMsg   (        "active"                      ,              2,   4);    
  regMsg   (        "ref"                         ,              2,  13);    
  regString(PSELECT,"select"                      ,       &p2,   2,  23);    
  regIvar2 (NHX    ," NHX"                        , &p0,  &p1,   2,  36,  2);
  regIvar2 (NHY    ," NHY"                        , &p0,  &p1,   2,  -1,  2);
  regIvar2 (ORDER  ," ORDER"                      , &p0,  &p1,   2,  -1,  1);

  regMsg   (        "X sort:"                     ,              1,  78);
  regIvar3 (XINCR  ,"incr"                        , &p0,  &p4,   1,  86,  2);
  regIvar3 (XDECR  ,"decr"                        , &p0,  &p4,   1,  94,  2);
  regIvar3 (XEITHER,"either"                      , &p0,  &p4,   1, 102,  2);
  regIvar3 (XFAST  ,"fast"                        , &p0,  &p4,   1, 112,  2);
  regMsg   (       "Y sort:"                      ,              2,  78);
  regIvar3 (YINCR  ,"incr"                        , &p0,  &p4,   2,  86,  2);
  regIvar3 (YDECR  ,"decr"                        , &p0,  &p4,   2,  94,  2);
  regIvar3 (YEITHER,"either"                      , &p0,  &p4,   2, 102,  2);
  regIvar3 (YFAST  ,"fast"                        , &p0,  &p4,   2, 112,  2);


     //       N        NMAX       ROW COL NCHAR MAXROWS
  regArrays(n_update, nmax_update, 3,  0,   5,    35);

     //      ID       PROMPT     JSW     ISW     COL NCHAR NDEC
  regIarray (ACTIVE ,"||"       ,&m66 ,  &p4   ,  0,  2);
  regCarray (NEXT   ," "        ,&p0  ,  &p5   ,  0,  2);
  regIarray (REF    ,"||"       ,&m66 ,  &p4   ,  0,  2);
  regCarray (NAME   ,"NAME"     ,&p0  ,  &p1   ,  0,  8);
  regCarray (SELECT ,"|"        ,&m66 ,  &p2   ,  0,  1);
  regFarray (XCOORD ,"XCOORD"   ,&p0  ,  &p1   ,  0,  8,   4);
  regFarray (YCOORD ,"YCOORD"   ,&p0  ,  &p1   ,  0,  8,   4);
  regCarray (TYPE   ,"type"     ,&p0  ,  &p2   ,  0,  4);
  regCarray (EEE    ,"E"        ,&p0  ,  &p5   ,  0,  1);
  regCarray (RRR    ,"R"        ,&p0  ,  &p5   ,  0,  1);
  regIarray (PICKS  ,"picks"    ,&p0  ,  &p5   ,  0,  5);
  regCarray (PROJ   ,"project"  ,&p0  ,  &p1   ,  0, 10);
  regCarray (LINE   ,"line"     ,&p0  ,  &p1   ,  0, 10);
  regCarray (RDATE  ,"rdate"    ,&p0  ,  &p1   ,  0,  5);
  regCarray (PDATE  ,"pdate"    ,&p0  ,  &p1   ,  0,  5);
  regCarray (ID     ,"id"       ,&p0  ,  &p1   ,  0,  3);
  regCarray (COMM   ,"comment"  ,&p0  ,  &p1   ,  0, 15);


  funIvar (NSEL   ,      NULL   ,  ivar_update);
  funIvar (NERR   ,      NULL   ,  ivar_update);
  funIvar (NRAY   ,      NULL   ,  ivar_update);
  funCvar (PSELECT, pselect_trap,      NULL   , pselect_switch_update);
  funIvar (NHX    ,    ivar_trap,  ivar_update,         switch_update);
  funIvar (NHY    ,    ivar_trap,  ivar_update,         switch_update);
  funIvar (ORDER  ,    ivar_trap,  ivar_update,         switch_update);

  funIvar (XINCR  ,    ivar_trap,  ivar_update);
  funIvar (XDECR  ,    ivar_trap,  ivar_update);
  funIvar (XEITHER,    ivar_trap,  ivar_update);
  funIvar (XFAST  ,    ivar_trap,  ivar_update);
  funIvar (YFAST  ,    ivar_trap,  ivar_update);
  funIvar (YINCR  ,    ivar_trap,  ivar_update);
  funIvar (YDECR  ,    ivar_trap,  ivar_update);
  funIvar (YEITHER,    ivar_trap,  ivar_update);

  funIvar (ACTIVE ,  active_trap, ivar2_update);
  funCvar (NEXT   ,   cvar9_trap,  cvar_update,    misc_switch_update);
  funIvar (REF    ,  active_trap, ivar2_update);
  funCvar (NAME   ,    cvar_trap,  cvar_update,         switch_update);
  funCvar (SELECT ,  button_trap,  cvar_update,  select_switch_update);
  funFvar (XCOORD ,    fvar_trap,  fvar_update,         switch_update);
  funFvar (YCOORD ,    fvar_trap,  fvar_update,         switch_update);
  funCvar (TYPE   ,  button_trap,  cvar_update,    type_switch_update);
  funCvar (EEE    ,   cvar9_trap,  cvar_update,    misc_switch_update);
  funCvar (RRR    ,   cvar9_trap,  cvar_update,    misc_switch_update);
  funIvar (PICKS  ,   ivar9_trap, ivar2_update,    misc_switch_update);
  funCvar (PROJ   ,    cvar_trap,  cvar_update,  string_switch_update);
  funCvar (LINE   ,    cvar_trap,  cvar_update,  string_switch_update);
  funCvar (RDATE  ,    cvar_trap,  cvar_update,  string_switch_update);
  funCvar (PDATE  ,    cvar_trap,  cvar_update,  string_switch_update);
  funCvar (ID     ,    cvar_trap,  cvar_update,  string_switch_update);
  funCvar (COMM   ,    cvar_trap,  cvar_update,  string_switch_update);
}



//------------------------------ end -----------------------------------//
//------------------------------ end -----------------------------------//
//------------------------------ end -----------------------------------//
