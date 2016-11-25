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

//---------------------- vfbox_pick.cc -----------------------//
//---------------------- vfbox_pick.cc -----------------------//
//---------------------- vfbox_pick.cc -----------------------//

//          implementation file for the VfboxPick class
//                derived from the SLDatabox class
//                derived from the VfInform class
//                     subdirectory vfgui


#include "vfgui/vfbox_pick.hh"
#include "vf/vf_manager.hh"
#include "vf/vf_dataset.hh"
#include "vf/vf_informer.hh"
#include "vf/vf_update.hh"
#include "vf/vf_utilities.hh"
#include "vf/vf_constants.hh"
#include "sl/sl_prim.hh"
#include "named_constants.h"
#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <assert.h>

/****
static int showsel = TRUE;     // do show selection array.
****/
static int showsel = FALSE;    // do not show selection array.


enum { ACTIVE = 1,
       DEPTH, TIME, NMO, RMS, AV, INT, THICK, OFF,        // arrays
       NAME, REF, NHX, NHY, ORDER, INVOKE,                // row one
       XCOORD, YCOORD, NPICKS, TYPE,                      // row two
       STARTV, MAXTIME, TTOL, DTOL,                       // upper right
       PROJ, LINE, RDATE, PDATE, USERID, COMM,            // strings
       SEL, ERR, RAY,                                     // char
       TD, SELP, SELPALL };


//---------------------- constructor -----------------------//
//---------------------- constructor -----------------------//
//---------------------- constructor -----------------------//


VfboxPick::VfboxPick(SLDelay *slparent, char *name, VfManager *manager)
           : SLDatabox(slparent, name, NULL, 4),
             VfInform(manager)
{
}



//------------------------ destructor --------------------------//
//------------------------ destructor --------------------------//
//------------------------ destructor --------------------------//

VfboxPick::~VfboxPick()
{
}



//----------------- virtual functions overriding VfInform ------------//
//----------------- virtual functions overriding VfInform ------------//
//----------------- virtual functions overriding VfInform ------------//

       // private.

void VfboxPick::postNewActivePicks(VfDataset *dataset, long ifun, long nchng)
{
  if(nchng > 1) return;
  if(dataset->notActive()) return;
  if(dataset->getActiveVelocityFunction() != ifun) return;
  setFocus(ACTIVE, (int)dataset->getActivePick(ifun));
}



//--------------------- trap functions -----------------------------//
//--------------------- trap functions -----------------------------//
//--------------------- trap functions -----------------------------//


static void type_trap(void *data, long /*ident*/, long /*index*/,
                       char* /*cvar*/, long /*nread*/, char *endkey)
{
  VfboxPick *table = (VfboxPick*)data;
  if(strcmp(endkey, "ARRIVED") == 0)
      {
      table->SLDatabox::showMessage
         ("press this to increment to next velocity type for data entry");
      return;
      }
  if(strcmp(endkey, "RETURN") != 0) return;
  VfDataset *dataset = table->manager()->activeDataset();
  long ifun = dataset->getActiveVelocityFunction();
  if(ifun == -1) return;
  int type = dataset->getDefaultType(ifun);
  type++;
  if(type > LASTTYPE) type = FIRSTTYPE;
  dataset->setDefaultType(ifun, type);
  table->SLDatabox::showMessage
             (table->manager()->utilities()->typeDescription(type));
}



static void active_trap(void *data, long /*ident*/, long index,
                       long /*ivar*/, long nread, char* /*endkey*/)
{
  if(nread == 0) return;
  VfboxPick *table   = (VfboxPick*)data;
  VfDataset *dataset = table->manager()->activeDataset();
  long ifun = dataset->getActiveVelocityFunction();
  if(ifun == -1) return;
  table->disableMessages();
  dataset->setActivePick(ifun, index);
  table->enableMessages();
}



static void invoke_trap(void *data, long /*ident*/, long /*index*/,
                       long ivar, long nread, char* /*endkey*/)
{
  if(nread == 0) return;
  VfboxPick *table   = (VfboxPick*)data;
  VfDataset *dataset = table->manager()->activeDataset();
  long ifun = dataset->getActiveVelocityFunction();
  if(ifun == -1) return;
  table->disableMessages();
  if(ivar) dataset->invokeRayTracing(ifun);
  else     dataset->cancelRayTracing(ifun);
  table->enableMessages();
}



static void array_trap(void *data, long ident, long index,
                       float fvar, long nread, char *endkey)
{
  VfboxPick *table   = (VfboxPick*)data;
  VfDataset *dataset = table->manager()->activeDataset();
  long ifun = dataset->getActiveVelocityFunction();
  if(ifun == -1) return;
  long npicks = dataset->numPicks(ifun);
  if(strcmp(endkey, "INSERT") == 0)
      {
      table->disableMessages();
      dataset->insertPickFromBuffer(ifun, index);
      table->enableMessages();
      return;
      }
  if(strcmp(endkey, "REMOVE") == 0)
      {
      if(index == npicks) return;
      table->disableMessages();
      dataset->removePickToBuffer(ifun, index);
      table->enableMessages();
      return;
      }
  if(nread == 0) return;
  table->disableMessages();
  if(index == npicks)
      {
      dataset->informer()->beforeChanges();
      dataset->appendNilPick(ifun);
      }
  switch(ident)
      {
      case DEPTH : dataset->setDepth     (ifun, index, fvar); break;
      case TIME  : dataset->setTime      (ifun, index, fvar); break;
      case NMO   : dataset->setVnmo      (ifun, index, fvar); break;
      case RMS   : dataset->setVrms      (ifun, index, fvar); break;
      case AV    : dataset->setVav       (ifun, index, fvar); break;
      case INT   : dataset->setVint      (ifun, index, fvar); break;
      case THICK : dataset->setThickness (ifun, index, fvar); break;
      default: assert(FALSE);
      }
  if(index == npicks)
      {
      dataset->informer()->afterChanges();
      }
  table->enableMessages();
}



static void tol_trap(void *data, long ident, long /*index*/,
                       float fvar, long nread, char* /*endkey*/)
{
  if(nread == 0) return;
  VfboxPick   *table     = (VfboxPick*)data;
  VfUtilities *utilities = table->manager()->utilities();
  VfUpdate    *update    = table->manager()->utilities()->update();
  switch(ident)
      {
      case STARTV : if(fvar == 0.0) fvar = FNIL;
                    update   ->setStartvel      (fvar); break;
      case MAXTIME: update   ->setMaxtime       (fvar); break;
      case TTOL   : utilities->setTimeTolerance (fvar); break;
      case DTOL   : utilities->setDepthTolerance(fvar); break;
      default: assert(FALSE);
      }
}


static void fvar_trap(void *data, long ident, long /*index*/,
                       float fvar, long nread, char* /*endkey*/)
{
  if(nread == 0) return;
  VfboxPick *table   = (VfboxPick*)data;
  VfDataset *dataset = table->manager()->activeDataset();
  long ifun = dataset->getActiveVelocityFunction();
  if(ifun == -1) return;
  switch(ident)
      {
      case XCOORD  : dataset->setXloc(ifun, fvar); break;
      case YCOORD  : dataset->setYloc(ifun, fvar); break;
      default: assert(FALSE);
      }
}



static void cvar_trap(void *data, long ident, long /*index*/,
                       char *cvar, long nread, char* /*endkey*/)
{
  if(nread == 0) return;
  VfboxPick *table   = (VfboxPick*)data;
  VfDataset *dataset = table->manager()->activeDataset();
  long ifun = dataset->getActiveVelocityFunction();
  if(ifun == -1)
      {
      dataset->appendVelocityFunction();
      ifun = dataset->getActiveVelocityFunction();
      }
  switch(ident)
      {
      case NAME  :  dataset->setVfid      (ifun, cvar); break;
      case PROJ  :  dataset->setProject   (ifun, cvar); break;
      case LINE  :  dataset->setLine      (ifun, cvar); break;
      case RDATE :  dataset->setRdate     (ifun, cvar); break;
      case PDATE :  dataset->setPdate     (ifun, cvar); break;
      case USERID:  dataset->setUserid    (ifun, cvar); break;
      case COMM  :  dataset->setComment   (ifun, cvar); break;
      default: assert(FALSE);
      }
}



static void prompt_trap(void *data, long ident, long /*index*/,
                       char* /*cvar*/, long /*nread*/, char *endkey)
{
  VfboxPick *table = (VfboxPick*)data;
  if(strcmp(endkey, "ARRIVED") == 0)
      {
      switch(ident)
          {
          case -DEPTH:
          case -TIME :
          case -THICK:
                       table->SLDatabox::showMessage
           ("press this to select this DEPTH/TIME/THICKNESS for data entry");
                       break;
          case -NMO  :
                       table->SLDatabox::showMessage
           ("press this to select NMO VELOCITY versus TIME for data entry");
                       break;
          case -RMS  :
          case -AV   :
          case -INT  :
                       table->SLDatabox::showMessage
           ("press this to select this VELOCITY for data entry");
                       break;
          case  TD   :
                       table->SLDatabox::showMessage
           ("press this to select TIME versus DEPTH for data entry");
                       break;
          default: assert(FALSE);
          }
      return;
      }
  if(strcmp(endkey, "RETURN") != 0) return;
  table->SLDatabox::showMessage("testing");

  VfDataset *dataset = table->manager()->activeDataset();
  long ifun = dataset->getActiveVelocityFunction();
  if(ifun == -1) return;
  int type = dataset->getDefaultType(ifun);
  switch(ident)
      {
      case -DEPTH: if     (type == VTRM || type == VLRM) type = VZRM;
                   else if(type == VTAV || type == VLAV) type = VZAV;
                   else if(type == VTIN || type == VLIN) type = VZIN;
                   else return;
                   break;
      case -TIME : if     (type == VZRM || type == VLRM) type = VTRM;
                   else if(type == VZAV || type == VLAV) type = VTAV;
                   else if(type == VZIN || type == VLIN) type = VTIN;
                   else return;
                   break;
      case -NMO  : if(type != VTNM) type = VTNM;
                   else return;
                   break;
      case -RMS  : if     (type == VTNM || type == VTDP) type = VTRM;
                   else if(type == VTAV || type == VTIN) type = VTRM;
                   else if(type == VZAV || type == VZIN) type = VZRM;
                   else if(type == VLAV || type == VLIN) type = VLRM;
                   else return;
                   break;
      case -AV   : if     (type == VTNM || type == VTDP) type = VTAV;
                   else if(type == VTRM || type == VTIN) type = VTAV;
                   else if(type == VZRM || type == VZIN) type = VZAV;
                   else if(type == VLRM || type == VLIN) type = VLAV;
                   else return;
                   break;
      case -INT  : if     (type == VTNM || type == VTDP) type = VTIN;
                   else if(type == VTAV || type == VTRM) type = VTIN;
                   else if(type == VZAV || type == VZRM) type = VZIN;
                   else if(type == VLAV || type == VLRM) type = VLIN;
                   else return;
                   break;
      case -THICK: if     (type == VTRM || type == VZRM) type = VLRM;
                   else if(type == VTAV || type == VZAV) type = VLAV;
                   else if(type == VTIN || type == VZIN) type = VLIN;
                   else return;
                   break;
      case  TD   : if(type != VTDP) type = VTDP;
                   else return;
                   break;
      default: assert(FALSE);
      }
  dataset->setDefaultType(ifun, type);
  table->SLDatabox::showMessage
             (table->manager()->utilities()->typeDescription(type));
}



static void select_trap(void *data, long /*ident*/, long index,
                        char* /*cvar*/, long /*nread*/, char* endkey)
{
  if(strcmp(endkey, "RETURN") != 0) return;
  VfboxPick *table   = (VfboxPick*)data;
  VfDataset *dataset = table->manager()->activeDataset();
  long ifun = dataset->getActiveVelocityFunction();
  if(ifun == -1) return;
  dataset->incrementPickSelectFlag(ifun, index);
}


static void select_all_trap(void *data, long /*ident*/, long /*index*/,
                            char* /*cvar*/, long /*nread*/, char* endkey)
{
  if(strcmp(endkey, "RETURN") != 0) return;
  VfboxPick *table   = (VfboxPick*)data;
  VfDataset *dataset = table->manager()->activeDataset();
  long ifun = dataset->getActiveVelocityFunction();
  if(ifun == -1) return;
  dataset->toggleAllPickSelections(ifun);
}



//------------------------ update functions ------------------------//
//------------------------ update functions ------------------------//
//------------------------ update functions ------------------------//



static long active_update(void *data, long /*ident*/, long index)
{
  VfboxPick *table   = (VfboxPick*)data;
  VfDataset *dataset = table->manager()->activeDataset();
  long ifun = dataset->getActiveVelocityFunction();
  if(ifun == -1) return FALSE;
  return (dataset->getActivePick(ifun) == index);
}



static long invoke_update(void *data, long /*ident*/, long /*index*/)
{
  VfboxPick *table   = (VfboxPick*)data;
  VfDataset *dataset = table->manager()->activeDataset();
  long ifun = dataset->getActiveVelocityFunction();
  if(ifun == -1) return FALSE;
  int raytrace = dataset->getRaytraceFlag(ifun);
  if(raytrace == RAYTRACE_NO) return FALSE;
  return TRUE;
}



static long ivar_update(void *data, long ident, long /*index*/)
{
  VfboxPick *table   = (VfboxPick*)data;
  VfDataset *dataset = table->manager()->activeDataset();
  if(ident == NPICKS)
      {
      long ifun = dataset->getActiveVelocityFunction();
      if(ifun == -1) return INIL;
      return dataset->numPicks(ifun);
      }
  switch(ident)
      {
      case NHX   :  return dataset->getNhx();
      case NHY   :  return dataset->getNhy();
      case ORDER :  return dataset->getMoveoutOrder();
      default: assert(FALSE);
      }
  assert(FALSE);
  return 0;
}



static float fvar_update(void *data, long ident, long /*index*/)
{
  VfboxPick *table   = (VfboxPick*)data;
  VfDataset *dataset = table->manager()->activeDataset();
  long ifun = dataset->getActiveVelocityFunction();
  if(ifun == -1) return FNIL;
  switch(ident)
      {
      case XCOORD  :  return dataset->getXloc(ifun);
      case YCOORD  :  return dataset->getYloc(ifun);
      default: assert(FALSE);
      }
  assert(FALSE);
  return 0.0;
}



static float tol_update(void *data, long ident, long /*index*/)
{
  VfboxPick   *table     = (VfboxPick*)data;
  VfUtilities *utilities = table->manager()->utilities();
  VfUpdate    *update    = table->manager()->utilities()->update();
  switch(ident)
      {
      case STARTV : return update   ->getStartvel      ();
      case MAXTIME: return update   ->getMaxtime       ();
      case TTOL   : return utilities->getTimeTolerance ();
      case DTOL   : return utilities->getDepthTolerance();
      default: assert(FALSE);
      }
  assert(FALSE);
  return 0.0;
}



static char *cvar_update(void *data, long ident, long /*index*/)
{
  static char *blank   = " ";
  static char *ref     = "reference";
  static char letter[2];
  VfboxPick *table   = (VfboxPick*)data;
  VfDataset *dataset = table->manager()->activeDataset();
  long ifun = dataset->getActiveVelocityFunction();
  if(ifun == -1) return blank;
  switch(ident)
      {
      case NAME  :  return (char*)dataset->getVfid      (ifun);
      case REF   :  { long iref = dataset->getReferenceVelocityFunction();
                      if(iref == ifun) return ref; return blank; }
      case TYPE  :  return (char*)dataset->getTypeSymbol(ifun);
      case PROJ  :  return (char*)dataset->getProject   (ifun);
      case LINE  :  return (char*)dataset->getLine      (ifun);
      case RDATE :  return (char*)dataset->getRdate     (ifun);
      case PDATE :  return (char*)dataset->getPdate     (ifun);
      case USERID:  return (char*)dataset->getUserid    (ifun);
      case COMM  :  return (char*)dataset->getComment   (ifun);
      case SEL   :  letter[0] = dataset->getSelectFlag  (ifun); return letter;
      case ERR   :  letter[0] = dataset->getErrorFlag   (ifun); return letter;
      case RAY   :  letter[0] = dataset->getRaytraceFlag(ifun); return letter;
      default: assert(FALSE);
      }
  assert(FALSE);
  return blank;
}



static float array_update(void *data, long ident, long index)
{
  VfboxPick *table   = (VfboxPick*)data;
  VfDataset *dataset = table->manager()->activeDataset();
  long ifun = dataset->getActiveVelocityFunction();
  if(ifun == -1) return FNIL;
  switch(ident)
      {
      case DEPTH :  return dataset->getDepth     (ifun, index);
      case TIME  :  return dataset->getTime      (ifun, index);
      case NMO   :  return dataset->getVnmo      (ifun, index);
      case RMS   :  return dataset->getVrms      (ifun, index);
      case AV    :  return dataset->getVav       (ifun, index);
      case INT   :  return dataset->getVint      (ifun, index);
      case THICK :  return dataset->getThickness (ifun, index);
      case OFF   :  return dataset->getOffset    (ifun, index);
      default: assert(FALSE);
      }
  assert(FALSE);
  return 0.0;
}



static char *select_update(void *data, long /*ident*/, long index)
{
  static char *buffer = " ";
  VfboxPick *table   = (VfboxPick*)data;
  VfDataset *dataset = table->manager()->activeDataset();
  long ifun = dataset->getActiveVelocityFunction();
  if(ifun == -1) return buffer;
  return (char*)dataset->getPickSelectString(ifun, index);
}



static long n_update(void *data)
{
  VfboxPick *table   = (VfboxPick*)data;
  VfDataset *dataset = table->manager()->activeDataset();
  long ifun = dataset->getActiveVelocityFunction();
  if(ifun == -1) return 0;
  long npicks = dataset->numPicks(ifun);
  return npicks;
}


static long nmax_update(void *data)
{
  VfboxPick *table   = (VfboxPick*)data;
  VfDataset *dataset = table->manager()->activeDataset();
  long ifun = dataset->getActiveVelocityFunction();
  if(ifun == -1) return 0;
  long npicks = dataset->numPicks(ifun);
  if(dataset->notEditable() || dataset->isLocked()) return npicks;
  return MinimumValue(npicks + 1, MAXPICKS);
}


//---------------------- switch functions ---------------------------//
//---------------------- switch functions ---------------------------//
//---------------------- switch functions ---------------------------//


static long active_switch(void *data, long /*ident*/, long index)
{
  VfboxPick *table   = (VfboxPick*)data;
  VfDataset *dataset = table->manager()->activeDataset();
  long ifun = dataset->getActiveVelocityFunction();
  if(ifun == -1) return -4;
  if(index >= dataset->numPicks(ifun)) return -4;
  return 4;
}



static long invoke_switch(void *data, long /*ident*/, long /*index*/)
{
  VfboxPick *table   = (VfboxPick*)data;
  VfDataset *dataset = table->manager()->activeDataset();
  if(dataset->notEditable() || dataset->isLocked()) return -3;
  if(dataset->getActiveVelocityFunction() == -1) return -3;
  return 3;
}



static long type_switch(void *data, long /*ident*/, long /*index*/)
{
  VfboxPick *table   = (VfboxPick*)data;
  VfDataset *dataset = table->manager()->activeDataset();
  if(dataset->notEditable() || dataset->isLocked()) return -2;
  if(dataset->getActiveVelocityFunction() == -1) return -2;
  return 2;
}



static long text_switch(void *data, long /*ident*/, long /*index*/)
{
  VfboxPick *table   = (VfboxPick*)data;
  VfDataset *dataset = table->manager()->activeDataset();
  long isw = 1;
  if(dataset->notEditable() || dataset->isLocked()) isw = 5;
  if(dataset->getActiveVelocityFunction() == -1) return -isw;
  return isw;
}



static long array_switch(void *data, long ident, long /*index*/)
{
  VfboxPick   *table     = (VfboxPick*)data;
  VfDataset   *dataset   = table->manager()->activeDataset();
  VfUtilities *utilities = table->manager()->utilities();
  long isw = 1;
  if(dataset->notEditable() || dataset->isLocked()) isw = 5;
  long ifun = dataset->getActiveVelocityFunction();
  if(ifun == -1) return -isw;
  int type = dataset->getDefaultType(ifun);
  switch(ident)
      {
      case DEPTH: if(!utilities->abscissaIsDepth    (type) &&
                     !utilities->ordinateIsDepth    (type)) isw = -isw; break;
      case TIME : if(!utilities->abscissaIsTime     (type)) isw = -isw; break;
      case NMO  : if(!utilities->ordinateIsVNMO     (type)) isw = -isw; break;
      case RMS  : if(!utilities->ordinateIsVRMS     (type)) isw = -isw; break;
      case AV   : if(!utilities->ordinateIsVAV      (type)) isw = -isw; break;
      case INT  : if(!utilities->ordinateIsVINT     (type)) isw = -isw; break;
      case THICK: if(!utilities->abscissaIsThickness(type)) isw = -isw; break;
      default   : assert(FALSE);
      }
  return isw;
}



static long prompt_switch(void *data, long ident, long /*index*/)
{
  VfboxPick   *table     = (VfboxPick*)data;
  VfDataset   *dataset   = table->manager()->activeDataset();
  VfUtilities *utilities = table->manager()->utilities();
  if(dataset->notEditable() || dataset->isLocked()) return -2;
  long ifun = dataset->getActiveVelocityFunction();
  if(ifun == -1) return -2;
  int type = dataset->getDefaultType(ifun);
  long isw = 2;
  switch(ident)
      {
      case -DEPTH: if( utilities->ordinateIsVNMO(type)) isw = -2; break;
      case -THICK: if( utilities->ordinateIsVNMO(type) ||
                                          type == VTDP) isw = -2; break;
      }
  return isw;
}



static long select_switch(void *data, long /*ident*/, long index)
{
  VfboxPick *table   = (VfboxPick*)data;
  VfDataset *dataset = table->manager()->activeDataset();
  long ifun = dataset->getActiveVelocityFunction();
  if(ifun == -1) return -2;
  if(index >= dataset->numPicks(ifun)) return -2;
  if(dataset->pickIsSelected(ifun, index)) return 6;
  return 2;
}


static long select_all_switch(void *data, long /*ident*/, long /*index*/)
{
  VfboxPick *table   = (VfboxPick*)data;
  VfDataset *dataset = table->manager()->activeDataset();
  long ifun = dataset->getActiveVelocityFunction();
  if(ifun == -1) return -2;
  if(dataset->numPicks(ifun) == 0) return -2;
  return 2;
}



//---------------------- make helper ------------------------//
//---------------------- make helper ------------------------//
//---------------------- make helper ------------------------//

void VfboxPick::makeHelper()
{
  static long p0  =   0; 
  static long p1  =   1; 
  static long p2  =   2; 
  static long p3  =   3; 
  static long p4  =   4; 
//static long p5  =   5; 
  static long m5  =  -5; 
  static long m44 = -44; 
  static long m66 = -66; 

          /// 10/9/98 changed startvel and maxtime from p5 to p1.


     //      ID       PROMPT                  JSW     ISW  ROW COL NCHAR NDEC
  regCvar2 (NAME   , "NAME"                 , &p0   , &p1 , 1,  1,  8);
  regCvar  (REF    ,                                  &m44, 1, 18,  9);
  regIvar2 (NHX    , " nhx"                 , &p0   , &m5 , 1, 28,  2);
  regIvar2 (NHY    , " nhy"                 , &p0   , &m5 , 1, -1,  2);
  regIvar2 (ORDER  , " order"               , &p0   , &m5 , 1, -1,  1);
  regIvar3 (INVOKE , "invoke ray tracing"   , &p0   , &p3 , 1, 59,  2);

  if(showsel)
      {
      regMsg   (         "active"           ,               2,  1);
      regString(SELPALL, "select"           ,         &p2 , 2,  8);
      }
  else
      {
      regMsg   (         "active"           ,               2,  3);
      }

  regFvar2 (XCOORD , "XCOORD"               , &p0   , &p1 , 2, 20,  8, 4);
  regFvar2 (YCOORD , "YCOORD"               , &p0   , &p1 , 2, 37,  8, 4);
  regIvar3 (NPICKS , "picks"                , &p0   , &m5 , 2, 57,  5);
  regCvar2 (TYPE   , "type"                 , &p0   , &p2 , 2, 72,  4);
  regFvar2 (STARTV , "  startvel"           , &p0   , &p1 , 1, 86,  5, 0);
  regFvar2 (MAXTIME, "   maxtime"           , &p0   , &p1 , 0,  0,  5, 3);
  regFvar2 (TTOL   , "   timetol"           , &p0   , &p1 , 0,  0,  5, 3);
  regFvar2 (DTOL   , "  depthtol"           , &p0   , &p1 , 0,  0,  5, 0);
  regCvar2 (PROJ   , "project"              , &p0   , &p1 , 0, 79, 10);
  regCvar2 (LINE   , "   line"              , &p0   , &p1 , 0,  0, 10);
  regCvar2 (RDATE  , "  rdate"              , &p0   , &p1 , 0,  0,  5);
  regCvar2 (PDATE  , "  pdate"              , &p0   , &p1 , 0,  0,  5);
  regCvar2 (USERID , " userid"              , &p0   , &p1 , 0,  0,  3);
  regCvar2 (COMM   , "comment"              , &p0   , &p1 , 0,  0, 15);
  regCvar2 (SEL    , " selected"            , &p0   , &m5 , 0, 84,  1);
  regCvar2 (ERR    , "    error"            , &p0   , &m5 , 0,  0,  1);
  regCvar2 (RAY    , "raytraced"            , &p0   , &m5 , 0,  0,  1);
  regString(TD     , " "                    ,         &p2 , 3, 17);

     //       N        NMAX       ROW COL NCHAR MAXROWS
  regArrays(n_update, nmax_update, 3,  0,   5,    35);

     //       ID     PROMPT       JSW    ISW      COL NCHAR NDEC
  regIarray (ACTIVE,"||"        , &m66,  &p4     ,  0,  2);

  if(showsel)
      {
      regCarray (SELP  ,"|"     , &m66,  &p2     ,  0,  1);
      }

  regFarray (DEPTH ,"depth"     , &p0 ,  &p1     , 11,  7,  0);
  regFarray (TIME  ,"time "     , &p2 ,  &p1     ,  0,  5,  3);
  regFarray (NMO   ,"NMO VEL"   , &p0 ,  &p1     , 26,  7,  0);
  regFarray (RMS   ,"RMS VEL"   , &p2 ,  &p1     ,  0,  7,  0);
  regFarray (AV    ,"AV VEL "   , &p2 ,  &p1     ,  0,  7,  0);
  regFarray (INT   ,"INT VEL"   , &p2 ,  &p1     ,  0,  7,  0);
  regFarray (THICK ,"thickness" , &p0 ,  &p1     , 59,  7,  0);
  regFarray (OFF   ,"max offset", &p0 ,  &m5     ,  0,  6,  0);

  funCvar   (NAME   ,   cvar_trap,    cvar_update,   text_switch);
  funCvar   (REF    ,        NULL,    cvar_update);
  funIvar   (NHX    ,        NULL,    ivar_update);
  funIvar   (NHY    ,        NULL,    ivar_update);
  funIvar   (ORDER  ,        NULL,    ivar_update);
  funIvar   (INVOKE , invoke_trap,  invoke_update, invoke_switch);
  funFvar   (XCOORD ,   fvar_trap,    fvar_update,   text_switch);
  funFvar   (YCOORD ,   fvar_trap,    fvar_update,   text_switch);
  funIvar   (NPICKS ,        NULL,    ivar_update);
  funCvar   (TYPE   ,   type_trap,    cvar_update,   type_switch);
  funFvar   (STARTV ,    tol_trap,     tol_update);
  funFvar   (MAXTIME,    tol_trap,     tol_update);
  funFvar   (TTOL   ,    tol_trap,     tol_update);
  funFvar   (DTOL   ,    tol_trap,     tol_update);
  funCvar   (PROJ   ,   cvar_trap,    cvar_update,   text_switch);
  funCvar   (LINE   ,   cvar_trap,    cvar_update,   text_switch);
  funCvar   (RDATE  ,   cvar_trap,    cvar_update,   text_switch);
  funCvar   (PDATE  ,   cvar_trap,    cvar_update,   text_switch);
  funCvar   (USERID ,   cvar_trap,    cvar_update,   text_switch);
  funCvar   (COMM   ,   cvar_trap,    cvar_update,   text_switch);
  funCvar   (SEL    ,        NULL,    cvar_update);
  funCvar   (ERR    ,        NULL,    cvar_update);
  funCvar   (RAY    ,        NULL,    cvar_update);
  funCvar   (TD     , prompt_trap,           NULL, prompt_switch);

  funIvar   (ACTIVE , active_trap,  active_update, active_switch);
  funFvar   (DEPTH  ,  array_trap,   array_update,  array_switch);
  funFvar   (TIME   ,  array_trap,   array_update,  array_switch);
  funFvar   (NMO    ,  array_trap,   array_update,  array_switch);
  funFvar   (RMS    ,  array_trap,   array_update,  array_switch);
  funFvar   (AV     ,  array_trap,   array_update,  array_switch);
  funFvar   (INT    ,  array_trap,   array_update,  array_switch);
  funFvar   (THICK  ,  array_trap,   array_update,  array_switch);
  funFvar   (OFF    ,        NULL,   array_update);

  funCvar   (-DEPTH , prompt_trap,           NULL, prompt_switch);
  funCvar   (-TIME  , prompt_trap,           NULL, prompt_switch);
  funCvar   (-NMO   , prompt_trap,           NULL, prompt_switch);
  funCvar   (-RMS   , prompt_trap,           NULL, prompt_switch);
  funCvar   (-AV    , prompt_trap,           NULL, prompt_switch);
  funCvar   (-INT   , prompt_trap,           NULL, prompt_switch);
  funCvar   (-THICK , prompt_trap,           NULL, prompt_switch);

  funCvar   (SELP   , select_trap,  select_update, select_switch);
  funCvar   (SELPALL, select_all_trap,       NULL, select_all_switch);
}



//-------------------------- end -------------------------------//
//-------------------------- end -------------------------------//
//-------------------------- end -------------------------------//
