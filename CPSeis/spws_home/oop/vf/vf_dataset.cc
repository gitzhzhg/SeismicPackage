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
// $Id: vf_dataset.cc,v 1.2 2004/06/07 12:56:38 wjdone Exp spws $
// $Name:  $

//-------------------------- vf_dataset.cc ----------------------------//
//-------------------------- vf_dataset.cc ----------------------------//
//-------------------------- vf_dataset.cc ----------------------------//

//            implementation file for the VfDataset class
//                    not derived from any class
//                         subdirectory vf


#include "vf/vf_dataset.hh"
#include "vf/vf_kernal.hh"
#include "vf/vf_helper.hh"
#include "vf/vf_function.hh"
#include "vf/vf_edit_base.hh"
#include "vf/vf_read_save.hh"
#include "named_constants.h"
#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <assert.h>
#include <limits.h>
#include <math.h>

      // header files for VfInformer and VfUtilities not needed.

static const char *COPYING = "copying velocity function";

#define NBUF 111

#define FORBIDDEN  if(_helper->dataChangesAreForbidden()) return;


//-------------------------- constructor ----------------------------//
//-------------------------- constructor ----------------------------//
//-------------------------- constructor ----------------------------//


VfDataset::VfDataset(int editable, VfInformer *informer,
                                   VfUtilities *utilities,
                                   const char *progname)
           :
        _index          (-1),
        _active         (FALSE),
        _reference      (FALSE),
        _selected       (FALSE),
        _informer       (informer),
        _utilities      (utilities),
        _kernal         (NULL),
        _helper         (NULL)
{
  assert(_informer);
  assert(_utilities);
  _kernal = new VfKernal (_informer, _utilities);
  _helper = new VfHelper (editable, _informer, this, _kernal, progname);
}



//--------------------------- destructor --------------------------//
//--------------------------- destructor --------------------------//
//--------------------------- destructor --------------------------//


VfDataset::~VfDataset()
{
  delete _helper;
  delete _kernal;
}



//----------------- new bin tolerances by vf manager only ---------------//
//----------------- new bin tolerances by vf manager only ---------------//
//----------------- new bin tolerances by vf manager only ---------------//

       // public.

void VfDataset::newBinTolerancesByVfManagerOnly()
{
  _kernal->newBinTolerancesByVfDatasetOnly();
}



//----------------------- get values ---------------------------------//
//----------------------- get values ---------------------------------//
//----------------------- get values ---------------------------------//

        // public.

class HistoryCards *VfDataset::history()  const
{
  return _helper->history();
}


int VfDataset::dataNeedsSaving()  const
{
  return _helper->dataNeedsSaving();
}


int VfDataset::dataBackedUp()  const
{
  return _helper->dataBackedUp();
}


int VfDataset::isLocked()  const
{
  return _helper->isLocked();
}


int VfDataset::notLocked()  const
{
  return _helper->notLocked();
}


int VfDataset::isEditable()  const
{
  return _helper->isEditable();
}


int VfDataset::notEditable()  const
{
  return _helper->notEditable();
}



//-------------------- last file read or saved -------------------//
//-------------------- last file read or saved -------------------//
//-------------------- last file read or saved -------------------//

        // public.

const char *VfDataset::lastFileRead        ()  const
{
  return _helper->lastFileRead();
}


const char *VfDataset::lastFileSaved       ()  const
{
  return _helper->lastFileSaved();
}


const char *VfDataset::lastBackupFileSaved ()  const
{
  return _helper->lastBackupFileSaved();
}



//------------------- select and unselect this dataset --------------//
//------------------- select and unselect this dataset --------------//
//------------------- select and unselect this dataset --------------//

         // public.

void VfDataset::selectThisDataset()
{
  if(_selected) return;
  _helper->preSelectDataset();
  _selected = TRUE;
  _helper->postSelectDataset();
}



void VfDataset::unselectThisDataset()
{
  if(!_selected) return;
  _helper->preUnselectDataset();
  _selected = FALSE;
  _helper->postUnselectDataset();
}



//--------------------- lock and unlock data -------------------------//
//--------------------- lock and unlock data -------------------------//
//--------------------- lock and unlock data -------------------------//

        // public.

void VfDataset::lockData()
{
  _helper->lockData();
}


void VfDataset::unlockData()
{
  _helper->unlockData();
}



//------------------------ edit dataset --------------------------------//
//------------------------ edit dataset --------------------------------//
//------------------------ edit dataset --------------------------------//

           // public.

void VfDataset::editDataset (VfEditBase *edit, void *doer)
{
  FORBIDDEN
  char msg[NBUF];
  int error = edit->checkForErrors(_kernal, msg);
  _helper->preEdit(edit, msg, error, doer);
  if(error) return;
  error = edit->editKernal(_kernal, msg);
  _helper->postEdit(edit, msg, error);
}



//------------------------ read velocity file --------------------------//
//------------------------ read velocity file --------------------------//
//------------------------ read velocity file --------------------------//

           // public.

int VfDataset::readVelocityFile (const char *filename, char *msg,
                                 VfReadSave *readsave, void *doer)
{
  assert(filename && msg);
  if(_helper->readVelocityFileIsForbidden(msg)) return TRUE;
  _helper->preRead(doer);

  int replaced_all;
  int error = readsave->readVelocityFile(filename, msg, _kernal,
                                         _helper->history(), &replaced_all);

  _helper->postRead(filename, msg, error, replaced_all);
  return error;
}



int VfDataset::readVelocityFile (const char *filename, char *msg, void *doer)
{
  VfReadSave *readsave = new VfReadSave(_informer, _utilities, FALSE, 0);
  int error = readsave->validateInputFile(filename, msg);
  error     = readVelocityFile           (filename, msg, readsave, doer);
  delete readsave;
  return error;
}



//------------------------ save velocity file --------------------------//
//------------------------ save velocity file --------------------------//
//------------------------ save velocity file --------------------------//

           // public.

int VfDataset::saveVelocityFile (const char *filename, char *msg,
                                 VfReadSave *readsave, int force)
{
  assert(filename && msg);
  if(!force && _helper->saveVelocityFileIsForbidden(msg)) return TRUE;
  _helper->preSave();

  int saved_all;
  int error = readsave->saveVelocityFile(filename, msg, _kernal,
                                         _helper->history(), &saved_all);

  _helper->postSave(filename, msg, error, saved_all);
  return error;
}



int VfDataset::saveVelocityFile (const char *filename, char *msg, int force)
{
  VfReadSave *readsave = new VfReadSave(_informer, _utilities, FALSE, 1);
  int error = saveVelocityFile (filename, msg, readsave, force);
  delete readsave;
  return error;
}



//--------------------- save backup file --------------------------------//
//--------------------- save backup file --------------------------------//
//--------------------- save backup file --------------------------------//

           // public.

void VfDataset::saveBackupFile()
{
  _helper->saveBackupFile();
}



//-------------------- interact with undo file -----------------------//
//-------------------- interact with undo file -----------------------//
//-------------------- interact with undo file -----------------------//

         // public.

void VfDataset::saveUndoFile(void *doer)
{
  _helper->saveUndoFile(doer);
}



int VfDataset::allowReadDeleteUndoFile(void *doer)  const
{
  return _helper->allowReadDeleteUndoFile(doer);
}



void VfDataset::maybeDeleteUndoFile(void *doer)
{
  _helper->maybeDeleteUndoFile(doer);
}



void VfDataset::maybeReadUndoFile(void *doer)
{
  _helper->maybeReadUndoFile(doer);
}




          //-------- pass thru to VfKernal ---------//
          //-------- pass thru to VfKernal ---------//
          //-------- pass thru to VfKernal ---------//
          //-------- pass thru to VfKernal ---------//
          //-------- pass thru to VfKernal ---------//
          //-------- pass thru to VfKernal ---------//
          //-------- pass thru to VfKernal ---------//
          //-------- pass thru to VfKernal ---------//
          //-------- pass thru to VfKernal ---------//
          //-------- pass thru to VfKernal ---------//


//------------- replace or append velocity functions from storage -------//
//------------- replace or append velocity functions from storage -------//
//------------- replace or append velocity functions from storage -------//

       // public.
       // replace or append velocity functions from storage.
       // also copies _nhx, _nhy, _order, _nhosign, and _nhoexp.
       // also resets  active   velocity function if previously empty.
       // also resets reference velocity function if previously empty.
       // does NOT copy the _informer and _utilities pointers.
       // does NOT copy _xcenter, _xwidth, _ycenter, and _ywidth.

void VfDataset::replaceVelocityFunctions(const VfDataset *storage)
{
  FORBIDDEN
  _helper->preTotalChanges();
  _kernal->replaceVelocityFunctions(COPYING, storage->_kernal);
  _helper->postTotalChanges();
}


void VfDataset::appendVelocityFunctions(const VfDataset *storage)
{
  FORBIDDEN
  _helper->preTotalChanges();
  _kernal->appendVelocityFunctions(COPYING, storage->_kernal);
  _helper->postTotalChanges();
}



//-------------------------- get values -----------------------------//
//-------------------------- get values -----------------------------//
//-------------------------- get values -----------------------------//

          // public.

int VfDataset::getNhx()  const
{
  return _kernal->getNhx();
}


int VfDataset::getNhy()  const
{
  return _kernal->getNhy();
}


int VfDataset::getMoveoutOrder()  const
{
  return _kernal->getMoveoutOrder();
}


float VfDataset::getNhosign()  const
{
  return _kernal->getNhosign();
}


float VfDataset::getNhoexp()  const
{
  return _kernal->getNhoexp();
}


int VfDataset::getDistanceUnits()  const
{
  return _kernal->getDistanceUnits();
}


const char *VfDataset::getSymbolFromUnits()  const
{
  return _kernal->getSymbolFromUnits();
}


const char *VfDataset::getAttributeName()  const
{
  return _kernal->getAttributeName();
}


const char *VfDataset::getName()  const
{
  return _kernal->getName();
}


const char *VfDataset::getAttributeUnits()  const
{
  return _kernal->getAttributeUnits();
}


const char *VfDataset::getTimeDepthUnits()  const
{
  return _kernal->getTimeDepthUnits();
}



//-------------------------- set values --------------------------------//
//-------------------------- set values --------------------------------//
//-------------------------- set values --------------------------------//

          // public.

void VfDataset::setNhx(int value)
{
  FORBIDDEN
  _helper->preChangeHeaderWords();
  _kernal->setNhx(value);
  _helper->postChangeHeaderWords();
}


void VfDataset::setNhy(int value)
{
  FORBIDDEN
  _helper->preChangeHeaderWords();
  _kernal->setNhy(value);
  _helper->postChangeHeaderWords();
}


void VfDataset::setMoveoutOrder(int value)
{
  FORBIDDEN
  _helper->preChangeMoveoutOrder();
  _kernal->setMoveoutOrder(value);
  _helper->postChangeMoveoutOrder();
}


void VfDataset::setDistanceUnits(int value)
{
  FORBIDDEN
  _helper->preChangeUnits();
  _kernal->setDistanceUnits(value);
  _helper->postChangeUnits();
}


void VfDataset::setUnitsFromSymbol(const char *value)
{
  FORBIDDEN
  _helper->preChangeUnits();
  _kernal->setUnitsFromSymbol(value);
  _helper->postChangeUnits();
}


void VfDataset::setName(const char *value)
{
  FORBIDDEN
  _helper->preChangeUnits();
  _kernal->setName(value);
  _helper->postChangeUnits();
}


void VfDataset::setAttributeName(const char *value)
{
  FORBIDDEN
  _helper->preChangeUnits();
  _kernal->setAttributeName(value);
  _helper->postChangeUnits();
}


void VfDataset::setAttributeUnits(const char *value)
{
  FORBIDDEN
  _helper->preChangeUnits();
  _kernal->setAttributeUnits(value);
  _helper->postChangeUnits();
}


void VfDataset::setTimeDepthUnits(const char *value)
{
  FORBIDDEN
  _helper->preChangeUnits();
  _kernal->setTimeDepthUnits(value);
  _helper->postChangeUnits();
}



//------------------- get minimum and maximum values ------------------//
//------------------- get minimum and maximum values ------------------//
//------------------- get minimum and maximum values ------------------//

       // public.

float  VfDataset::minimumXloc() { return _kernal->minimumXloc(); }
float  VfDataset::maximumXloc() { return _kernal->maximumXloc(); }

float  VfDataset::minimumYloc() { return _kernal->minimumYloc(); }
float  VfDataset::maximumYloc() { return _kernal->maximumYloc(); }


float  VfDataset::minimumXbinCenter() { return _kernal->minimumXbinCenter(); }
float  VfDataset::maximumXbinCenter() { return _kernal->maximumXbinCenter(); }

float  VfDataset::minimumYbinCenter() { return _kernal->minimumYbinCenter(); }
float  VfDataset::maximumYbinCenter() { return _kernal->maximumYbinCenter(); }


float  VfDataset::minimumAbscissa  (int type)
{ return _kernal->minimumAbscissa      (type); }

float  VfDataset::maximumAbscissa  (int type)
{ return _kernal->maximumAbscissa      (type); }

float  VfDataset::minimumOrdinate  (int type)
{ return _kernal->minimumOrdinate      (type); }

float  VfDataset::maximumOrdinate  (int type)
{ return _kernal->maximumOrdinate      (type); }


float  VfDataset::minimumDepth() { return _kernal->minimumDepth(); }
float  VfDataset::maximumDepth() { return _kernal->maximumDepth(); }

float  VfDataset::minimumTime() { return _kernal->minimumTime(); }
float  VfDataset::maximumTime() { return _kernal->maximumTime(); }

float  VfDataset::minimumVrms() { return _kernal->minimumVrms(); }
float  VfDataset::maximumVrms() { return _kernal->maximumVrms(); }

float  VfDataset::minimumVav() { return _kernal->minimumVav(); }
float  VfDataset::maximumVav() { return _kernal->maximumVav(); }

float  VfDataset::minimumVint() { return _kernal->minimumVint(); }
float  VfDataset::maximumVint() { return _kernal->maximumVint(); }

float  VfDataset::minimumVnmo() { return _kernal->minimumVnmo(); }
float  VfDataset::maximumVnmo() { return _kernal->maximumVnmo(); }

float  VfDataset::minimumThickness() { return _kernal->minimumThickness(); }
float  VfDataset::maximumThickness() { return _kernal->maximumThickness(); }

float  VfDataset::minimumOffset() { return _kernal->minimumOffset(); }
float  VfDataset::maximumOffset() { return _kernal->maximumOffset(); }


float  VfDataset::getMinimumTime(float depth) const
{
  return _kernal->getMinimumTime(depth);
}

float  VfDataset::getMaximumTime(float depth) const
{
  return _kernal->getMaximumTime(depth);
}


float  VfDataset::getMinimumDepth(float time) const
{
  return _kernal->getMinimumDepth(time);
}

float  VfDataset::getMaximumDepth(float time) const
{
  return _kernal->getMaximumDepth(time);
}



//------------------------ misc functions --------------------------//
//------------------------ misc functions --------------------------//
//------------------------ misc functions --------------------------//

       // public.
       // convenience functions.

long   VfDataset::findMatchingVelfun (float xloc, float yloc)  const
{ return _kernal->findMatchingVelfun(xloc, yloc); }

long   VfDataset::findNearestVelfun (float xloc, float yloc,
                                     float xperpix, float yperpix)  const
{ return _kernal->findNearestVelfun (xloc, yloc, xperpix, yperpix); }

void   VfDataset::findNearestVelfuns (float xloc, float yloc,
                                      float xnorm, float ynorm,
                                      long *ifun1, long *ifun2,
                                      long *ifun3, long *ifun4,
                                      float *weight1, float *weight2,
                                      float *weight3, float *weight4,
                                      int type)  const
       { _kernal->findNearestVelfuns (xloc, yloc, xnorm, ynorm,
                                      ifun1, ifun2, ifun3, ifun4,
                                      weight1, weight2, weight3, weight4,
                                      type); }

int    VfDataset::checkSort (int *xdir, int *ydir)  const
{
  int *xfast;
  return _kernal->checkSort (xdir, ydir, xfast);
}

int    VfDataset::checkXdirection()                      const
{ return _kernal->checkXdirection(); }

int    VfDataset::checkYdirection()                      const
{ return _kernal->checkYdirection(); }

long   VfDataset::findNearbyXloc (long ifun, int direction)  const
{ return _kernal->findNearbyXloc      (ifun,     direction); }

long   VfDataset::findNearbyYloc (long ifun, int direction)  const
{ return _kernal->findNearbyYloc      (ifun,     direction); }

long   VfDataset::findNextXloc (long ifun, float xloc, float yloc)
{ return _kernal->findNextXloc      (ifun,       xloc,       yloc); }

long   VfDataset::findPrevXloc (long ifun, float xloc, float yloc)
{ return _kernal->findPrevXloc      (ifun,       xloc,       yloc); }

long   VfDataset::findNextYloc (long ifun, float xloc, float yloc)
{ return _kernal->findNextYloc      (ifun,       xloc,       yloc); }

long   VfDataset::findPrevYloc (long ifun, float xloc, float yloc)
{ return _kernal->findPrevYloc      (ifun,       xloc,       yloc); }

long   VfDataset::findWhereToInsert (float xloc, float yloc, int xflag)  const
{ return _kernal->findWhereToInsert       (xloc,       yloc,     xflag); }


long VfDataset::findOrInsertVelfun(float xloc, float yloc, int xflag)
{
  if(_helper->dataChangesAreForbidden()) return -1;
  long ifun = _kernal->findMatchingVelfun(xloc, yloc);
  if(ifun >= 0)
       {
       setActiveVelocityFunction(ifun);
       return ifun;
       }
  ifun = _kernal->findWhereToInsert(xloc, yloc, xflag);
  assert(ifun >= 0);
  _helper->preRemoveInsertVelocityFunctions(ifun, 0, 1, TRUE);
  _kernal->insertVelocityFunction(ifun);
  _kernal->velfun(ifun)->setXloc(xloc);
  _kernal->velfun(ifun)->setYloc(yloc);
  _kernal->setActiveVelocityFunction(ifun);
  if(numVelocityFunctions() == 1) _kernal->setReferenceVelocityFunction(0);
  _helper->postRemoveInsertVelocityFunctions(ifun, 0, 1);
  return ifun;
}
 

void VfDataset::getVelfunInline (float xmin, float xmax,
                           float ybin_choice, long *list, long *nlist)  const
{
  _kernal->getVelfunInline(xmin, xmax, ybin_choice, list, nlist);
}


void VfDataset::getVelfunCrossline (float ymin, float ymax,
                           float xbin_choice, long *list, long *nlist)  const
{
  _kernal->getVelfunCrossline(ymin, ymax, xbin_choice, list, nlist);
}


void VfDataset::xbinToFloat (float ymin, float ymax,
                    float xbin_choice, int type, float time,
                    long nsamp, float *array, int vint_grade)  const
{
  _kernal->xbinToFloat(ymin, ymax, xbin_choice, type, time,
                                    nsamp, array, vint_grade);
}



float VfDataset::getInterpolatedVelocity
                    (float xloc, float yloc, float abscissa,
                     int type, int vint_grade,
                     float xnorm, float ynorm)  const
{
  return _kernal->getInterpolatedVelocity
                    (xloc, yloc, abscissa, type, vint_grade, xnorm, ynorm);
}


float VfDataset::getInterpolatedTime
                    (float xloc, float yloc, float depth,
                     float xnorm, float ynorm)  const
{
  return _kernal->getInterpolatedTime(xloc, yloc, depth, xnorm, ynorm);
}


float VfDataset::getInterpolatedDepth
                    (float xloc, float yloc, float time,
                     float xnorm, float ynorm)  const
{
  return _kernal->getInterpolatedDepth(xloc, yloc, time, xnorm, ynorm);
}



long VfDataset::getInterpolatedVelocityFunction
                   (long npicks, float *abscissae, float *ordinates,
                    float xloc, float yloc,
                    int type, int vint_grade,
                    float xnorm, float ynorm)  const
{
  return _kernal->getInterpolatedVelocityFunction
                       (npicks, abscissae, ordinates,
                        xloc, yloc, type, vint_grade, xnorm, ynorm);
}


void VfDataset::velToFloat (float xloc, float yloc, float tmin, float tmax,
                            long npicks, float *ordinates,
                            int type, int vint_grade)  const
{
  _kernal->velToFloat
              (xloc, yloc, tmin, tmax, npicks, ordinates, type, vint_grade);
}



void VfDataset::timeToDepth (float xloc, float yloc, float tmin, float tmax,
                             long nsamp, void *array, int array_type,
                             float dmin, float dmax)  const
{
  _kernal->timeToDepth
              (xloc, yloc, tmin, tmax, nsamp, array, array_type, dmin, dmax);
}


void VfDataset::depthToTime (float xloc, float yloc, float tmin, float tmax,
                             long nsamp, void *array, int array_type,
                             float dmin, float dmax)  const
{
  _kernal->depthToTime
              (xloc, yloc, tmin, tmax, nsamp, array, array_type, dmin, dmax);
}


void VfDataset::startMultipleInterpolations(long npicks)
{
  _kernal->startMultipleInterpolations(npicks);
}



void VfDataset::stopMultipleInterpolations()
{
  _kernal->stopMultipleInterpolations();
}



//----------------- set general values -------------------------------//
//----------------- set general values -------------------------------//
//----------------- set general values -------------------------------//

         // public.


          //-------- pass thru to VfFunctionArray ---------//
          //-------- pass thru to VfFunctionArray ---------//
          //-------- pass thru to VfFunctionArray ---------//
          //-------- pass thru to VfFunctionArray ---------//
          //-------- pass thru to VfFunctionArray ---------//
          //-------- pass thru to VfFunctionArray ---------//
          //-------- pass thru to VfFunctionArray ---------//
          //-------- pass thru to VfFunctionArray ---------//
          //-------- pass thru to VfFunctionArray ---------//
          //-------- pass thru to VfFunctionArray ---------//


//------------------------- delete velocity functions ----------------//
//------------------------- delete velocity functions ----------------//
//------------------------- delete velocity functions ----------------//

          // public.

void VfDataset::deleteAllVelocityFunctions()
{
  FORBIDDEN
  _helper->preTotalChanges();
  _kernal->deleteAllVelocityFunctions();
  _helper->postTotalChanges();
}


void VfDataset::deleteSelectedVelocityFunctions()
{
  FORBIDDEN
  _helper->preTotalChanges();
  _kernal->deleteSelectedVelocityFunctions();
  _helper->postTotalChanges();
}


void VfDataset::deleteEmptyVelocityFunctions()
{
  FORBIDDEN
  _helper->preTotalChanges();
  _kernal->deleteEmptyVelocityFunctions();
  _helper->postTotalChanges();
}



//-------------------------- get values -----------------------------//
//-------------------------- get values -----------------------------//
//-------------------------- get values -----------------------------//

          // public.

long VfDataset::numVelocityFunctions()  const
{
  return _kernal->numVelocityFunctions();
}


long  VfDataset::numVelocityFunctionsWithErrors  ()  const
{
  return _kernal->numVelocityFunctionsWithErrors();
}


long  VfDataset::numRaytracedVelocityFunctions  ()  const
{
  return _kernal->numRaytracedVelocityFunctions();
}


long  VfDataset::numVelocityFunctionsWithBlankNames  ()  const
{
  return _kernal->numVelocityFunctionsWithBlankNames();
}


long  VfDataset::numVelocityFunctionsWithTypeErrors  (int type)  const
{
  return _kernal->numVelocityFunctionsWithTypeErrors(type);
}


long VfDataset::getActiveVelocityFunction()  const
{
  return _kernal->getActiveVelocityFunction();
}


long VfDataset::getReferenceVelocityFunction()  const
{
  return _kernal->getReferenceVelocityFunction();
}



//-------------------------- set values --------------------------------//
//-------------------------- set values --------------------------------//
//-------------------------- set values --------------------------------//

        // public.

void VfDataset::setActiveVelocityFunction(long ifun)
{
  _helper->preNewActiveVelocityFunction (ifun);
  _kernal->setActiveVelocityFunction    (ifun);
  _helper->postNewActiveVelocityFunction();
}


void VfDataset::setReferenceVelocityFunction(long ifun)
{
  _helper->preNewReferenceVelocityFunction (ifun);
  _kernal->setReferenceVelocityFunction    (ifun);
  _helper->postNewReferenceVelocityFunction();
}



//---------------- insert or remove one velocity function ---------------//
//---------------- insert or remove one velocity function ---------------//
//---------------- insert or remove one velocity function ---------------//

          // public.

void VfDataset::appendVelocityFunction()
{
  FORBIDDEN
  long ifun = _kernal->numVelocityFunctions();
  _helper->preRemoveInsertVelocityFunctions(ifun, 0, 1);
  _kernal->appendVelocityFunction();
  _helper->postRemoveInsertVelocityFunctions(ifun, 0, 1);
}


void VfDataset::insertVelocityFunction (long ifun)
{
  FORBIDDEN
  _helper->preRemoveInsertVelocityFunctions(ifun, 0, 1);
  _kernal->insertVelocityFunction(ifun);
  _helper->postRemoveInsertVelocityFunctions(ifun, 0, 1);
}


void VfDataset::insertVelocityFunctionFromBuffer (long ifun)
{
  FORBIDDEN
  _helper->preRemoveInsertVelocityFunctions(ifun, 0, 1);
  _kernal->insertVelocityFunctionFromBuffer(ifun);
  _helper->postRemoveInsertVelocityFunctions(ifun, 0, 1);
}


void VfDataset::removeVelocityFunction(long ifun)
{
  FORBIDDEN
  _helper->preRemoveInsertVelocityFunctions(ifun, 1, 0);
  _kernal->removeVelocityFunction(ifun);
  _helper->postRemoveInsertVelocityFunctions(ifun, 1, 0);
}


void VfDataset::removeVelocityFunctionToBuffer(long ifun)
{
  FORBIDDEN
  _helper->preRemoveInsertVelocityFunctions(ifun, 1, 0);
  _kernal->removeVelocityFunctionToBuffer(ifun);
  _helper->postRemoveInsertVelocityFunctions(ifun, 1, 0);
}





            //--------- pass thru to VfFunctionSelect ---------//
            //--------- pass thru to VfFunctionSelect ---------//
            //--------- pass thru to VfFunctionSelect ---------//
            //--------- pass thru to VfFunctionSelect ---------//
            //--------- pass thru to VfFunctionSelect ---------//
            //--------- pass thru to VfFunctionSelect ---------//
            //--------- pass thru to VfFunctionSelect ---------//
            //--------- pass thru to VfFunctionSelect ---------//
            //--------- pass thru to VfFunctionSelect ---------//
            //--------- pass thru to VfFunctionSelect ---------//
            //--------- pass thru to VfFunctionSelect ---------//
            //--------- pass thru to VfFunctionSelect ---------//


//----------------------- get values ---------------------------------//
//----------------------- get values ---------------------------------//
//----------------------- get values ---------------------------------//

         // public.

long   VfDataset::numSelectedVelocityFunctions()     const
{ return _kernal->numSelectedVelocityFunctions(); }


int    VfDataset::velocityFunctionIsSelected(long ifun)    const
{ return _kernal->velocityFunctionIsSelected     (ifun); }


int    VfDataset::getSelectFlag(long ifun)    const
{ return _kernal->getSelectFlag     (ifun); }



//----------------------- set values ---------------------------------//
//----------------------- set values ---------------------------------//
//----------------------- set values ---------------------------------//

         // public.

void VfDataset::clearSelectFlags    ()
{
  long nselect = _kernal->numSelectedVelocityFunctions();
  if(nselect == 0) return;
  _helper->preChangeSelections(0);
  _kernal->clearSelectFlags();
  _helper->postChangeSelections(0);
}


void VfDataset::incrementSelectFlag (long ifun)
{
  _helper->preChangeSelections(ifun);
  _kernal->incrementSelectFlag(ifun);
  _helper->postChangeSelections(ifun);
}


void VfDataset::setSelectFlag       (long ifun, int select)
{
  _helper->preChangeSelections(ifun);
  _kernal->setSelectFlag(ifun, select);
  _helper->postChangeSelections(ifun);
}



void VfDataset::beforeSettingSeveralSelectFlags ()
{
  _helper->preChangeSelections(0);
  _kernal->beforeSettingSeveralSelectFlags();
}



void VfDataset::afterSettingSeveralSelectFlags ()
{
  _kernal->afterSettingSeveralSelectFlags();
  _helper->postChangeSelections(0);
}




            //--------- pass thru to VfFunction ---------//
            //--------- pass thru to VfFunction ---------//
            //--------- pass thru to VfFunction ---------//
            //--------- pass thru to VfFunction ---------//
            //--------- pass thru to VfFunction ---------//
            //--------- pass thru to VfFunction ---------//
            //--------- pass thru to VfFunction ---------//
            //--------- pass thru to VfFunction ---------//
            //--------- pass thru to VfFunction ---------//
            //--------- pass thru to VfFunction ---------//



//----------------- reallocate and reset picks ------------------------//
//----------------- reallocate and reset picks ------------------------//
//----------------- reallocate and reset picks ------------------------//

         // public.

void VfDataset::deleteAllVelocityFunctionPicks (long ifun)
{
  FORBIDDEN
  _helper->preModifyPicks(ifun, -1, 0, -1);
  _kernal->velfun(ifun)->deleteAllVelocityFunctionPicks();
  _helper->postModifyPicks(ifun, -1, 0);
}



void VfDataset::resetNumPicks (long ifun, long npicks, int type)
{
  FORBIDDEN
  _helper->preModifyPicks(ifun, type, 0, -1);
  _kernal->velfun(ifun)->resetNumPicks(npicks, type);
  _helper->postModifyPicks(ifun, type, 0);
}



void VfDataset::resetNumPicks (long ifun, long npicks, float *abscissae,
                                            float *ordinates, int type)
{
  FORBIDDEN
  _helper->preModifyPicks(ifun, type, 0, -1);
  _kernal->velfun(ifun)->resetNumPicks(npicks, abscissae, ordinates, type);
  _helper->postModifyPicks(ifun, type, 0);
}



//----------------- get and set velocity function contents --------------//
//----------------- get and set velocity function contents --------------//
//----------------- get and set velocity function contents --------------//

     // public.

void VfDataset::getVelocityFunctionContents
                              (long ifun, VfFunction *function)  const
{
  assert(function);
  function->copyVelocityFunctionContents(_kernal->velfun(ifun));
}



void VfDataset::setVelocityFunctionContents
                              (long ifun, const VfFunction *function)
{
  FORBIDDEN
  assert(function);
  _helper->preChangeCoords     (ifun, 1);
//_helper->preNewActivePicks   (ifun, 1); // called by _helper->preModifyPicks.
  _helper->preNewDefaultTypes  (ifun, 1);
  _helper->preModifyStrings    (ifun, 1);
  _helper->preModifyPicks      (ifun, -1, 0, -1);
  _kernal->velfun(ifun)->copyVelocityFunctionContents(function);
  _helper->postModifyPicks     (ifun, -1, 0);
  _helper->postModifyStrings   (ifun, 1);
  _helper->postNewDefaultTypes (ifun, 1);
//_helper->postNewActivePicks  (ifun, 1); // called by _helper->postModifyPicks.
  _helper->postChangeCoords    (ifun, 1);
}



//----------------- get strings corresponding to type ---------------//
//----------------- get strings corresponding to type ---------------//
//----------------- get strings corresponding to type ---------------//

         // public.

const char *VfDataset::getTypeSymbol       (long ifun, int type)  const
{
  return _kernal->velfun(ifun)->getTypeSymbol(type);
}


const char *VfDataset::getTypeDescription  (long ifun, int type)  const
{
  return _kernal->velfun(ifun)->getTypeDescription(type);
}



//-------------------- get general values -----------------------------//
//-------------------- get general values -----------------------------//
//-------------------- get general values -----------------------------//

         // public.

float VfDataset::getXloc         (long ifun)  const
{
  return _kernal->velfun(ifun)->getXloc();
}


float VfDataset::getYloc         (long ifun)  const
{
  return _kernal->velfun(ifun)->getYloc();
}


long VfDataset::numPicks        (long ifun)  const
{
  return _kernal->velfun(ifun)->numPicks();
}


long VfDataset::getActivePick   (long ifun)  const
{
  return _kernal->velfun(ifun)->getActivePick();
}


int VfDataset::getDefaultType  (long ifun)  const
{
  return _kernal->velfun(ifun)->getDefaultType();
}


int VfDataset::getErrorFlag    (long ifun)  const
{
  return _kernal->velfun(ifun)->getErrorFlag();
}


int VfDataset::getRaytraceFlag (long ifun)  const
{
  return _kernal->velfun(ifun)->getRaytraceFlag();
}


int VfDataset::typeError (long ifun, int type)  const
{
  return _kernal->velfun(ifun)->typeError(type);
}



const char *VfDataset::getVfid       (long ifun)  const
{
  return _kernal->velfun(ifun)->getVfid();
}


const char *VfDataset::getProject    (long ifun)  const
{
  return _kernal->velfun(ifun)->getProject();
}


const char *VfDataset::getLine       (long ifun)  const
{
  return _kernal->velfun(ifun)->getLine();
}


const char *VfDataset::getRdate      (long ifun)  const
{
  return _kernal->velfun(ifun)->getRdate();
}


const char *VfDataset::getPdate      (long ifun)  const
{
  return _kernal->velfun(ifun)->getPdate();
}


const char *VfDataset::getUserid     (long ifun)  const
{
  return _kernal->velfun(ifun)->getUserid();
}


const char *VfDataset::getComment    (long ifun)  const
{
  return _kernal->velfun(ifun)->getComment();
}


float VfDataset::getElevation    (long ifun)  const
{
  return _kernal->velfun(ifun)->getElevation();
}


float VfDataset::getWaterDepth    (long ifun)  const
{
  return _kernal->velfun(ifun)->getWaterDepth();
}



//-------------- get individual pick values ---------------------------//
//-------------- get individual pick values ---------------------------//
//-------------- get individual pick values ---------------------------//

         // public.

float VfDataset::getDepth     (long ifun, long ipick)  const
{
  return _kernal->velfun(ifun)->getDepth (ipick);
}


float VfDataset::getTime      (long ifun, long ipick)  const
{
  return _kernal->velfun(ifun)->getTime (ipick);
}


float VfDataset::getThickness (long ifun, long ipick)  const
{
  return _kernal->velfun(ifun)->getThickness (ipick);
}


float VfDataset::getVrms      (long ifun, long ipick)  const
{
  return _kernal->velfun(ifun)->getVrms (ipick);
}


float VfDataset::getVav       (long ifun, long ipick)  const
{
  return _kernal->velfun(ifun)->getVav (ipick);
}


float VfDataset::getVint      (long ifun, long ipick)  const
{
  return _kernal->velfun(ifun)->getVint (ipick);
}


float VfDataset::getVnmo      (long ifun, long ipick)  const
{
  return _kernal->velfun(ifun)->getVnmo (ipick);
}


float VfDataset::getOffset    (long ifun, long ipick)  const
{
  return _kernal->velfun(ifun)->getOffset (ipick);
}



float VfDataset::getAbscissa  (long ifun, long ipick, int type)  const
{
  return _kernal->velfun(ifun)->getAbscissa (ipick, type);
}


float VfDataset::getOrdinate  (long ifun, long ipick, int type)  const
{
  return _kernal->velfun(ifun)->getOrdinate (ipick, type);
}



//-------------- get arrays of pick values ---------------------------//
//-------------- get arrays of pick values ---------------------------//
//-------------- get arrays of pick values ---------------------------//

         // public.

void VfDataset::getDepthArray     (long ifun, float *values)  const
{
  _kernal->velfun(ifun)->getDepthArray (values);
}


void VfDataset::getTimeArray      (long ifun, float *values)  const
{
  _kernal->velfun(ifun)->getTimeArray (values);
}


void VfDataset::getThicknessArray (long ifun, float *values)  const
{
  _kernal->velfun(ifun)->getThicknessArray (values);
}


void VfDataset::getVrmsArray      (long ifun, float *values)  const
{
  _kernal->velfun(ifun)->getVrmsArray (values);
}


void VfDataset::getVavArray       (long ifun, float *values)  const
{
  _kernal->velfun(ifun)->getVavArray (values);
}


void VfDataset::getVintArray      (long ifun, float *values)  const
{
  _kernal->velfun(ifun)->getVintArray (values);
}


void VfDataset::getVnmoArray      (long ifun, float *values)  const
{
  _kernal->velfun(ifun)->getVnmoArray (values);
}


void VfDataset::getOffsetArray    (long ifun, float *values)  const
{
  _kernal->velfun(ifun)->getOffsetArray (values);
}



void VfDataset::getAbscissaArray  (long ifun, float *values, int type)  const
{
  _kernal->velfun(ifun)->getAbscissaArray (values, type);
}


void VfDataset::getOrdinateArray  (long ifun, float *values, int type)  const
{
  _kernal->velfun(ifun)->getOrdinateArray (values, type);
}



void VfDataset::findBracketingAbscissae
              (long ifun, float value, int type, long *ia, long *ib)  const
{
  _kernal->velfun(ifun)->findBracketingAbscissae(value, type, ia, ib);
}


float VfDataset::getInterpolatedVelocity
        (long ifun, float abscissa, int type, int vint_grade)  const
{
  return _kernal->velfun(ifun)->getInterpolatedVelocity
                                         (abscissa, type, vint_grade);
}


float VfDataset::getInterpolatedTime (long ifun, float depth)  const
{
  return _kernal->velfun(ifun)->getInterpolatedTime(depth);
}


float VfDataset::getInterpolatedDepth (long ifun, float time)  const
{
  return _kernal->velfun(ifun)->getInterpolatedDepth(time);
}



void VfDataset::velToFloat (long ifun, float tmin, float tmax,
                            long npicks, float *ordinates,
                            int type, int vint_grade)  const
{
  _kernal->velfun(ifun)->velToFloat
              (tmin, tmax, npicks, ordinates, type, vint_grade);
}



//----------------- set general values -------------------------------//
//----------------- set general values -------------------------------//
//----------------- set general values -------------------------------//

         // public.

void VfDataset::setXloc        (long ifun, float value)
{
  FORBIDDEN
  _helper->preChangeCoords(ifun, 1);
  _kernal->velfun(ifun)->setXloc(value);
  _helper->postChangeCoords(ifun, 1);
}


void VfDataset::setYloc        (long ifun, float value)
{
  FORBIDDEN
  _helper->preChangeCoords(ifun, 1);
  _kernal->velfun(ifun)->setYloc(value);
  _helper->postChangeCoords(ifun, 1);
}


void VfDataset::setActivePick  (long ifun, long  value)
{
  _helper->preNewActivePicks(ifun, 1, value);
  _kernal->velfun(ifun)->setActivePick(value);
  _helper->postNewActivePicks(ifun, 1);
}


void VfDataset::setDefaultType (long ifun, int   value)
{
  FORBIDDEN
  _helper->preNewDefaultTypes(ifun, 1);
  _kernal->velfun(ifun)->setDefaultType(value);
  _helper->postNewDefaultTypes(ifun, 1);
}



void VfDataset::setVfid        (long ifun, const char *value)
{
  FORBIDDEN
  _helper->preModifyStrings(ifun, 1);
  _kernal->velfun(ifun)->setVfid(value);
  _helper->postModifyStrings(ifun, 1);
}


void VfDataset::setProject     (long ifun, const char *value)
{
  FORBIDDEN
  _helper->preModifyStrings(ifun, 1);
  _kernal->velfun(ifun)->setProject(value);
  _helper->postModifyStrings(ifun, 1);
}


void VfDataset::setLine        (long ifun, const char *value)
{
  FORBIDDEN
  _helper->preModifyStrings(ifun, 1);
  _kernal->velfun(ifun)->setLine(value);
  _helper->postModifyStrings(ifun, 1);
}


void VfDataset::setRdate       (long ifun, const char *value)
{
  FORBIDDEN
  _helper->preModifyStrings(ifun, 1);
  _kernal->velfun(ifun)->setRdate(value);
  _helper->postModifyStrings(ifun, 1);
}


void VfDataset::setPdate       (long ifun, const char *value)
{
  FORBIDDEN
  _helper->preModifyStrings(ifun, 1);
  _kernal->velfun(ifun)->setPdate(value);
  _helper->postModifyStrings(ifun, 1);
}


void VfDataset::setUserid      (long ifun, const char *value)
{
  FORBIDDEN
  _helper->preModifyStrings(ifun, 1);
  _kernal->velfun(ifun)->setUserid(value);
  _helper->postModifyStrings(ifun, 1);
}


void VfDataset::setComment     (long ifun, const char *value)
{
  FORBIDDEN
  _helper->preModifyStrings(ifun, 1);
  _kernal->velfun(ifun)->setComment(value);
  _helper->postModifyStrings(ifun, 1);
}


void VfDataset::setElevation     (long ifun, float value)
{
  FORBIDDEN
  _helper->preModifyStrings(ifun, 1);
  _kernal->velfun(ifun)->setElevation(value);
  _helper->postModifyStrings(ifun, 1);
}


void VfDataset::setWaterDepth     (long ifun, float value)
{
  FORBIDDEN
  _helper->preModifyStrings(ifun, 1);
  _kernal->velfun(ifun)->setWaterDepth(value);
  _helper->postModifyStrings(ifun, 1);
}



//-------------- set individual pick values ---------------------------//
//-------------- set individual pick values ---------------------------//
//-------------- set individual pick values ---------------------------//

         // public.

void VfDataset::setDepth     (long ifun, long ipick, float value, int type)
{
  FORBIDDEN
  _helper->preModifyPicks(ifun, type, ipick, 1);
  _kernal->velfun(ifun)->setDepth(ipick, value, type);
  _helper->postModifyPicks(ifun, type, ipick);
}


void VfDataset::setTime      (long ifun, long ipick, float value, int type)
{
  FORBIDDEN
  _helper->preModifyPicks(ifun, type, ipick, 1);
  _kernal->velfun(ifun)->setTime(ipick, value, type);
  _helper->postModifyPicks(ifun, type, ipick);
}


void VfDataset::setThickness (long ifun, long ipick, float value, int type)
{
  FORBIDDEN
  _helper->preModifyPicks(ifun, type, ipick, 1);
  _kernal->velfun(ifun)->setThickness(ipick, value, type);
  _helper->postModifyPicks(ifun, type, ipick);
}


void VfDataset::setVrms      (long ifun, long ipick, float value, int type)
{
  FORBIDDEN
  _helper->preModifyPicks(ifun, type, ipick, 1);
  _kernal->velfun(ifun)->setVrms(ipick, value, type);
  _helper->postModifyPicks(ifun, type, ipick);
}


void VfDataset::setVav       (long ifun, long ipick, float value, int type)
{
  FORBIDDEN
  _helper->preModifyPicks(ifun, type, ipick, 1);
  _kernal->velfun(ifun)->setVav(ipick, value, type);
  _helper->postModifyPicks(ifun, type, ipick);
}


void VfDataset::setVint      (long ifun, long ipick, float value, int type)
{
  FORBIDDEN
  _helper->preModifyPicks(ifun, type, ipick, 1);
  _kernal->velfun(ifun)->setVint(ipick, value, type);
  _helper->postModifyPicks(ifun, type, ipick);
}


void VfDataset::setVnmo      (long ifun, long ipick, float value, int type)
{
  FORBIDDEN
  _helper->preModifyPicks(ifun, type, ipick, 1);
  _kernal->velfun(ifun)->setVnmo(ipick, value, type);
  _helper->postModifyPicks(ifun, type, ipick);
}


void VfDataset::setAbscissa (long ifun, long ipick, float abscissa, int type)
{
  FORBIDDEN
  _helper->preModifyPicks(ifun, type, ipick, 1);
  _kernal->velfun(ifun)->setAbscissa(ipick, abscissa, type);
  _helper->postModifyPicks(ifun, type, ipick);
}


void VfDataset::setOrdinate (long ifun, long ipick, float ordinate, int type)
{
  FORBIDDEN
  _helper->preModifyPicks(ifun, type, ipick, 1);
  _kernal->velfun(ifun)->setOrdinate(ipick, ordinate, type);
  _helper->postModifyPicks(ifun, type, ipick);
}


void VfDataset::replacePick (long ifun, long ipick, float abscissa,
                                           float ordinate, int type)
{
  FORBIDDEN
  _helper->preModifyPicks(ifun, type, ipick, 1);
  _kernal->velfun(ifun)->replacePick(ipick, abscissa, ordinate, type);
  _helper->postModifyPicks(ifun, type, ipick);
}



//---------------- set arrays of pick values ---------------------------//
//---------------- set arrays of pick values ---------------------------//
//---------------- set arrays of pick values ---------------------------//

         // public.

void VfDataset::setDepthArray     (long ifun, const float *values, int type)
{
  FORBIDDEN
  _helper->preModifyPicks(ifun, type, 0, -1);
  _kernal->velfun(ifun)->setDepthArray(values, type);
  _helper->postModifyPicks(ifun, type, 0);
}


void VfDataset::setTimeArray      (long ifun, const float *values, int type)
{
  FORBIDDEN
  _helper->preModifyPicks(ifun, type, 0, -1);
  _kernal->velfun(ifun)->setTimeArray(values, type);
  _helper->postModifyPicks(ifun, type, 0);
}


void VfDataset::setThicknessArray (long ifun, const float *values, int type)
{
  FORBIDDEN
  _helper->preModifyPicks(ifun, type, 0, -1);
  _kernal->velfun(ifun)->setThicknessArray(values, type);
  _helper->postModifyPicks(ifun, type, 0);
}


void VfDataset::setVrmsArray      (long ifun, const float *values, int type)
{
  FORBIDDEN
  _helper->preModifyPicks(ifun, type, 0, -1);
  _kernal->velfun(ifun)->setVrmsArray(values, type);
  _helper->postModifyPicks(ifun, type, 0);
}


void VfDataset::setVavArray       (long ifun, const float *values, int type)
{
  FORBIDDEN
  _helper->preModifyPicks(ifun, type, 0, -1);
  _kernal->velfun(ifun)->setVavArray(values, type);
  _helper->postModifyPicks(ifun, type, 0);
}


void VfDataset::setVintArray      (long ifun, const float *values, int type)
{
  FORBIDDEN
  _helper->preModifyPicks(ifun, type, 0, -1);
  _kernal->velfun(ifun)->setVintArray(values, type);
  _helper->postModifyPicks(ifun, type, 0);
}


void VfDataset::setVnmoArray      (long ifun, const float *values, int type)
{
  FORBIDDEN
  _helper->preModifyPicks(ifun, type, 0, -1);
  _kernal->velfun(ifun)->setVnmoArray(values, type);
  _helper->postModifyPicks(ifun, type, 0);
}


void VfDataset::setOffsetArray      (long ifun, const float *values, int type)
{
  FORBIDDEN
  _helper->preModifyPicks(ifun, type, 0, -1);
  _kernal->velfun(ifun)->setOffsetArray(values, type);
  _helper->postModifyPicks(ifun, type, 0);
}


void VfDataset::setAbscissaArray (long ifun, const float *abscissae, int type)
{
  FORBIDDEN
  _helper->preModifyPicks(ifun, type, 0, -1);
  _kernal->velfun(ifun)->setAbscissaArray(abscissae, type);
  _helper->postModifyPicks(ifun, type, 0);
}


void VfDataset::setOrdinateArray (long ifun, const float *ordinates, int type)
{
  FORBIDDEN
  _helper->preModifyPicks(ifun, type, 0, -1);
  _kernal->velfun(ifun)->setOrdinateArray(ordinates, type);
  _helper->postModifyPicks(ifun, type, 0);
}


void VfDataset::replaceAllPicks  (long ifun, const float *abscissae,
                                             const float *ordinates, int type)
{
  FORBIDDEN
  _helper->preModifyPicks(ifun, type, 0, -1);
  _kernal->velfun(ifun)->replaceAllPicks(abscissae, ordinates, type);
  _helper->postModifyPicks(ifun, type, 0);
}



//---------------- insert or remove one pick ---------------------------//
//---------------- insert or remove one pick ---------------------------//
//---------------- insert or remove one pick ---------------------------//

         // public.

void VfDataset::appendNilPick        (long ifun,             int type)
{
  FORBIDDEN
  _helper->preModifyPicks(ifun, type, -1, 0);
  _kernal->velfun(ifun)->appendNilPick(type);
  _helper->postModifyPicks(ifun, type, -1);
}


void VfDataset::insertNilPick        (long ifun, long ipick, int type)
{
  FORBIDDEN
  _helper->preModifyPicks(ifun, type, ipick, 0);
  _kernal->velfun(ifun)->insertNilPick(ipick, type);
  _helper->postModifyPicks(ifun, type, ipick);
}


void VfDataset::insertPickFromBuffer (long ifun, long ipick, int type)
{
  FORBIDDEN
  _helper->preModifyPicks(ifun, type, ipick, 0);
  _kernal->velfun(ifun)->insertPickFromBuffer(ipick, type);
  _helper->postModifyPicks(ifun, type, ipick);
}


void VfDataset::removePick           (long ifun, long ipick, int type)
{
  FORBIDDEN
  _helper->preModifyPicks(ifun, type, ipick, 1);
  _kernal->velfun(ifun)->removePick(ipick, type);
  _helper->postModifyPicks(ifun, type, ipick);
}


void VfDataset::removePickToBuffer   (long ifun, long ipick, int type)
{
  FORBIDDEN
  _helper->preModifyPicks(ifun, type, ipick, 1);
  _kernal->velfun(ifun)->removePickToBuffer(ipick, type);
  _helper->postModifyPicks(ifun, type, ipick);
}


void VfDataset::appendPick  (long ifun, float abscissa,
                                        float ordinate, int type)
{
  FORBIDDEN
  _helper->preModifyPicks(ifun, type, -1, 0);
  _kernal->velfun(ifun)->appendPick(abscissa, ordinate, type);
  _helper->postModifyPicks(ifun, type, -1);
}


void VfDataset::insertPick (long ifun, long ipick, float abscissa,
                                                   float ordinate, int type)
{
  FORBIDDEN
  _helper->preModifyPicks(ifun, type, ipick, 0);
  _kernal->velfun(ifun)->insertPick(ipick, abscissa, ordinate, type);
  _helper->postModifyPicks(ifun, type, ipick);
}



//------------- invoke or cancel ray tracing -------------------------//
//------------- invoke or cancel ray tracing -------------------------//
//------------- invoke or cancel ray tracing -------------------------//

         // public.

void VfDataset::invokeRayTracing (long ifun, int type)
{
  FORBIDDEN
  _helper->preModifyPicks(ifun, type, 0, -1);
  _kernal->velfun(ifun)->invokeRayTracing(type);
  _helper->postModifyPicks(ifun, type, 0);
}


void VfDataset::cancelRayTracing (long ifun, int type)
{
  FORBIDDEN
  _helper->preModifyPicks(ifun, type, 0, -1);
  _kernal->velfun(ifun)->cancelRayTracing(type);
  _helper->postModifyPicks(ifun, type, 0);
}



//------------------------ get and set pick select flags -----------------//
//------------------------ get and set pick select flags -----------------//
//------------------------ get and set pick select flags -----------------//

         // public.

long VfDataset::numSelectedPicks          (long ifun)            const
{
  return _kernal->velfun(ifun)->numSelectedPicks();
}


int  VfDataset::pickIsSelected            (long ifun, long ipick)  const
{
  return _kernal->velfun(ifun)->pickIsSelected(ipick);
}


int  VfDataset::getPickSelectFlag         (long ifun, long ipick)  const
{
  return _kernal->velfun(ifun)->getPickSelectFlag(ipick);
}


const char *VfDataset::getPickSelectString (long ifun, long ipick)  const
{
  return _kernal->velfun(ifun)->getPickSelectString(ipick);
}



void VfDataset::beforeSettingSeveralPickSelectFlags (long ifun)
{
  _helper->preChangePickSelections(ifun);
}


void VfDataset::setOneOfSeveralPickSelectFlags
                                       (long ifun, long ipick, int select)
{
  _kernal->velfun(ifun)->setOneOfSeveralPickSelectFlags(ipick, select);
}


void VfDataset::afterSettingSeveralPickSelectFlags (long ifun)
{
  _helper->postChangePickSelections(ifun);
}


void VfDataset::clearPickSelectFlags      (long ifun)
{
  _helper->preChangePickSelections(ifun);
  _kernal->velfun(ifun)->clearPickSelectFlags();
  _helper->postChangePickSelections(ifun);
}


void VfDataset::incrementPickSelectFlag   (long ifun, long ipick)
{
  _helper->preChangePickSelections(ifun);
  _kernal->velfun(ifun)->incrementPickSelectFlag(ipick);
  _helper->postChangePickSelections(ifun);
}


void VfDataset::setPickSelectFlag         (long ifun, long ipick, int select)
{
  _helper->preChangePickSelections(ifun);
  _kernal->velfun(ifun)->setPickSelectFlag(ipick, select);
  _helper->postChangePickSelections(ifun);
}


void VfDataset::toggleAllPickSelections   (long ifun)
{
  _helper->preChangePickSelections(ifun);
  _kernal->velfun(ifun)->toggleAllPickSelections();
  _helper->postChangePickSelections(ifun);
}



//-------------------------- end -------------------------------//
//-------------------------- end -------------------------------//
//-------------------------- end -------------------------------//

