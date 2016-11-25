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

//------------------------- vf_diskfile.cc ---------------------------//
//------------------------- vf_diskfile.cc ---------------------------//
//------------------------- vf_diskfile.cc ---------------------------//

//            implementation file for the VfDiskfile class
//                 derived from the VelioWrapper class
//                          subdirectory vf


#include "vf/vf_diskfile.hh"
#include "vf/vf_kernal.hh"
#include "vf/vf_informer.hh"
#include "vf/vf_function.hh"
#include "vf/vf_utilities.hh"
#include "oprim/history_cards.hh"
#include "str.h"
#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <assert.h>



//----------------------- constructor -------------------------------//
//----------------------- constructor -------------------------------//
//----------------------- constructor -------------------------------//


VfDiskfile::VfDiskfile(int io, const char *defname)
          :
             VelioWrapper(io, defname)
{
}



//----------------------- destructor -------------------------------//
//----------------------- destructor -------------------------------//
//----------------------- destructor -------------------------------//


VfDiskfile::~VfDiskfile()
{
}



//----------------------- update pjar from kernal -----------------------//
//----------------------- update pjar from kernal -----------------------//
//----------------------- update pjar from kernal -----------------------//

                  // call this prior to saving the file,
                  // or when the active kernal changes,
                  // or when something in the active kernal changes,
                  // or any time the user requests.


void VfDiskfile::updatePjarFromKernal (const VfKernal *kernal,
                                       const HistoryCards *history,
                                       int skiphist)
{
  setName            (kernal->getName             ());
  setNhx             (kernal->getNhx              ());
  setNhy             (kernal->getNhy              ());
  setMoveoutOrder    (kernal->getMoveoutOrder     ());
  setDistanceUnits   (kernal->getDistanceUnits    ());
  setAttributeName   (kernal->getAttributeName    ());
  setAttributeUnits  (kernal->getAttributeUnits   ());
  setTimeDepthUnits  (kernal->getTimeDepthUnits   ());

  putHistoryCardsIntoPjar(history, skiphist);

  augmentOutputParameters();
}



//---------------------- prepare read file ----------------------//
//---------------------- prepare read file ----------------------//
//---------------------- prepare read file ----------------------//


        // public.

int VfDiskfile::prepareReadFile (char *msg)
{
  int err = verifyParameters(msg);
  if(err != STATUS_OK) return TRUE;
  strcpy(msg, "");
  return FALSE;
}



//---------------------- prepare save file ----------------------//
//---------------------- prepare save file ----------------------//
//---------------------- prepare save file ----------------------//


        // public.

int VfDiskfile::prepareSaveFile (char *msg)
{
  int err = verifyParameters(msg);
  if(err != VelioWrapper::STATUS_OK) return TRUE;
  strcpy(msg, "");
  return FALSE;
}



//---------------------- read velocity file ----------------------//
//---------------------- read velocity file ----------------------//
//---------------------- read velocity file ----------------------//

        // public.

int VfDiskfile::readVelocityFile (const char *working,
                                  VfKernal *kernal,
                                  HistoryCards *history,
                                  const char *filename, char *msg)
{
  int error = openInputFile(filename, msg);

  if(error)
      {
      closeFile();
      return TRUE;
      }

  getHistoryCardsFromPjar(history);

  int nfun   = getScalarInteger("nfun");
  int active = getScalarInteger("active");

  if(nfun <= 0 || nfun == INIL) nfun = getScalarInteger("npackets");
  if(nfun <= 0) nfun = INIL;

  if     (nfun   == INIL) active = -1;
  else if(active == INIL) active = MinimumValue(0,nfun-1);
  else                    active--;      // was saved as fortran-style index.

  kernal->setName           (getName());
  kernal->setNhx            (getNhx());
  kernal->setNhy            (getNhy());
  kernal->setMoveoutOrder   (getNhosign(), getNhoexp());
  kernal->setDistanceUnits  (getDistanceUnits());
  kernal->setAttributeName  (getAttributeName());
  kernal->setAttributeUnits (getAttributeUnits());
  kernal->setTimeDepthUnits (getTimeDepthUnits());

  long nstart = kernal->numVelocityFunctions();

  if(nfun != INIL) kernal->appendNumVelocityFunctions(nfun);

  kernal->setActiveVelocityFunction(active);
  kernal->beforeSettingSeveralSelectFlags();

  float xcoord,ycoord;
  int   npicks;
  float tpicks[MAXPICKS];
  float vpicks[MAXPICKS];
  char  velname[50];
  char  veltype[50];
  char  project[50];
  char  line   [50];
  char  rdate  [50];
  char  pdate  [50];
  char  userid [50];
  char  comment[50];
  char  suffix[50];

  for(long ifun = 0; ifun < nfun || nfun == INIL; ifun++)
      {
      if(nfun==INIL) kernal->informer()->showWorkingMessage(working,ifun,ifun);
      else           kernal->informer()->showWorkingMessage(working,ifun,nfun);

      int err;
      readVelfun  (&xcoord , &ycoord, &npicks, MAXPICKS,
                   tpicks, vpicks, &err, msg,
                   velname, veltype, project, line,
                   rdate  , pdate, userid , comment);

      assert(npicks <= MAXPICKS);
      if(err == STATUS_EOF)
          {
          nfun = ifun;
          kernal->afterSettingSeveralSelectFlags();
          sprintf(suffix, " (%d functions)", nfun);
          strcat(msg, suffix);
          closeFile();
          return FALSE;
          }
      else if(err != STATUS_OK)
          {
          kernal->afterSettingSeveralSelectFlags();
          if(nfun == INIL) sprintf(suffix, " (function %d)", ifun+1);
          else sprintf(suffix, " (function %d of %d)", ifun+1, nfun);
          strcat(msg, suffix);
          closeFile();
          return TRUE;
          }

      int type = VfUtilities::getTypeFromSymbol(veltype);
      if(type == -1) type = VTNM;

      if(nfun == INIL)
          {
          kernal->afterSettingSeveralSelectFlags();
          kernal->appendNumVelocityFunctions(1);
          kernal->beforeSettingSeveralSelectFlags();
          }
      VfFunction *velfun = kernal->velfun(nstart + ifun);

      velfun->beforeSeveralOperations();
      velfun->setXloc                (xcoord);
      velfun->setYloc                (ycoord);
      velfun->setDefaultType         (type);
      velfun->setVfid                (velname);
      velfun->setProject             (project);
      velfun->setLine                (line);
      velfun->setRdate               (rdate);
      velfun->setPdate               (pdate);
      velfun->setUserid              (userid);
      velfun->setComment             (comment);
      velfun->resetNumPicks          (npicks, tpicks, vpicks, type);
      velfun->afterSeveralOperations ();
      }
  kernal->afterSettingSeveralSelectFlags();
  strcpy(msg, "velocity file successfully read");
  closeFile();
  return FALSE;
}



//--------------------- save velocity file ----------------------//
//--------------------- save velocity file ----------------------//
//--------------------- save velocity file ----------------------//

        // public.

int VfDiskfile::saveVelocityFile (const char *working,
                                  int save_choice,
                                  const VfKernal *kernal,
                                  const HistoryCards *history,
                                  const char *filename, char *msg,
                                  int skiphist)
{
  int  nfun   = kernal->numVelocityFunctions();
  int  nsel   = kernal->numSelectedVelocityFunctions();
  int  act    = kernal->getActiveVelocityFunction();
  int  kount  = 0;
  int  active = MinimumValue(0,nfun-1);

  if     (save_choice == SAVE_ACTIVE && act >= 0)   kount = 1;
  else if(save_choice == SAVE_ALL || nsel == nfun)  kount = nfun;
  else if(save_choice == SAVE_SELECTED && nsel > 0) kount = nsel;

  if(save_choice == SAVE_ALL) active = act;

  setScalarInteger("nfun"    , kount);
  setScalarInteger("npackets", kount);
  setScalarInteger("active"  , active+1);    // save as fortran-style index.

  updatePjarFromKernal(kernal, history, skiphist);

  int error = openOutputFile(filename, msg);

  if(error)
      {
      closeFile();
      return TRUE;
      }

  float tpicks[MAXPICKS];
  float vpicks[MAXPICKS];
  char  suffix[50];
  int   type         = getType();
  int   default_type = getType();

  for(long ifun = 0; ifun < nfun; ifun++)
      {
      kernal->informer()->showWorkingMessage(working, ifun, nfun);

      if(save_choice == SAVE_SELECTED &&
             !kernal->velocityFunctionIsSelected(ifun)) continue;
      if(save_choice == SAVE_ACTIVE   &&
              kernal->getActiveVelocityFunction() != ifun) continue;

      VfFunction *velfun = kernal->velfun(ifun);

      if(default_type == -1) type = velfun->getDefaultType();

      int         npicks   = velfun->numPicks       ();
      float       xcoord   = velfun->getXloc        ();
      float       ycoord   = velfun->getYloc        ();
      const char *velname  = velfun->getVfid        ();
      const char *veltype  = VfUtilities::typeSymbol(type);
      const char *project  = velfun->getProject     ();
      const char *line     = velfun->getLine        ();
      const char *rdate    = velfun->getRdate       ();
      const char *pdate    = velfun->getPdate       ();
      const char *userid   = velfun->getUserid      ();
      const char *comment  = velfun->getComment     ();

      velfun->getAbscissaArray(tpicks, type);
      velfun->getOrdinateArray(vpicks, type);

      int err;
      writeVelfun (xcoord ,ycoord, npicks,
                   tpicks, vpicks, &err, msg,
                   velname, veltype, project, line,
                   rdate  , pdate, userid , comment);

      if(err != STATUS_OK)
          {
          sprintf(suffix, " (function %d of %d)", ifun+1, nfun);
          strcat(msg, suffix);
          closeFile();
          return TRUE;
          }
      }
  sprintf(msg, "velocity file successfully saved with %d functions", kount);
  closeFile();
  return FALSE;
}



//---------------------------- end --------------------------------------//
//---------------------------- end --------------------------------------//
//---------------------------- end --------------------------------------//
