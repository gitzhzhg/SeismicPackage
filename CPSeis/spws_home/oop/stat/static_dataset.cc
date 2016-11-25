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

//--------------------- static_dataset.cc ---------------------//
//--------------------- static_dataset.cc ---------------------//
//--------------------- static_dataset.cc ---------------------//

//         implementation file for the StaticDataset class
//                  not derived from any class
//                      subdirectory stat



#include "stat/static_dataset.hh"
#include "stat/static_kernal.hh"
#include "stat/static_helper.hh"
#include "named_constants.h"
#include <string.h>
#include <iostream.h>
#include <stdlib.h>
#include <stdio.h>
#include <assert.h>
#include <limits.h>

           // StaticInformer header file not needed here.


#define ARGS            0, 0, getNx(), getNy()


//---------------------- constructor -----------------------//
//---------------------- constructor -----------------------//
//---------------------- constructor -----------------------//


StaticDataset::StaticDataset (const char *progname, StaticInformer *informer)
           :
          _index               (-999),
          _active              (FALSE),
          _reference           (FALSE),
          _selected            (FALSE),
          _informer            (informer),    // can be NULL.
          _kernal              (NULL),
          _helper              (NULL)
{
  _kernal  = new StaticKernal (progname);
  _helper  = new StaticHelper (_informer, this, _kernal);
  assert(_kernal);
  assert(_helper);
}



//------------------------ destructor --------------------------//
//------------------------ destructor --------------------------//
//------------------------ destructor --------------------------//

StaticDataset::~StaticDataset()
{
  delete _helper;
  delete _kernal;
}



                 //-------- public functions ---------//
                 //-------- public functions ---------//
                 //-------- public functions ---------//
                 //-------- public functions ---------//
                 //-------- public functions ---------//
                 //-------- public functions ---------//
                 //-------- public functions ---------//
                 //-------- public functions ---------//
                 //-------- public functions ---------//
                 //-------- public functions ---------//
                 //-------- public functions ---------//



//-------------------- allow changing file size -------------------//
//-------------------- allow changing file size -------------------//
//-------------------- allow changing file size -------------------//

       // public.
       // returns TRUE if nx and ny can be changed.
       // returns FALSE otherwise.

int StaticDataset::allowChangingFileSize()  const
{
  return _kernal->allowChangingFileSize();
}



//----------------------- miscellaneous functions -----------------//
//----------------------- miscellaneous functions -----------------//
//----------------------- miscellaneous functions -----------------//

       // public.

int StaticDataset::isLocked()  const
{
  return _helper->isLocked();
}


int StaticDataset::notLocked()  const
{
  return _helper->notLocked();
}


int StaticDataset::dataNeedsSaving()  const
{
  return _helper->dataNeedsSaving();
}


int StaticDataset::dataBackedUp()  const
{
  return _helper->dataBackedUp();
}



//-------------------- lock or unlock data --------------------------//
//-------------------- lock or unlock data --------------------------//
//-------------------- lock or unlock data --------------------------//

       // public.

void StaticDataset::lockData()
{
  _helper->lockData();
}


void StaticDataset::unlockData()
{
  _helper->unlockData();
}



//------------------- select and unselect this dataset --------------//
//------------------- select and unselect this dataset --------------//
//------------------- select and unselect this dataset --------------//

         // public.

void StaticDataset::selectThisDataset()
{
  _helper->preSelectDataset();
  _selected = TRUE;
  _helper->postSelectDataset();
}



void StaticDataset::unselectThisDataset()
{
  _helper->preUnselectDataset();
  _selected = FALSE;
  _helper->postUnselectDataset();
}



//---------------------------- get values --------------------------//
//---------------------------- get values --------------------------//
//---------------------------- get values --------------------------//

       // public.

class HistoryCards *StaticDataset::history()  const
{
  return _kernal->history();
}


const char *StaticDataset::getStattype      ()  const
{
  return _kernal->getStattype();
}


int    StaticDataset::getNhx       ()  const
{
  return _kernal->getNhx();
}


int    StaticDataset::getNhy       ()  const
{
  return _kernal->getNhy();
}


int    StaticDataset::getNhx2      ()  const
{
  return _kernal->getNhx2();
}


int    StaticDataset::getNhy2      ()  const
{
  return _kernal->getNhy2();
}


float  StaticDataset::getX1        ()  const
{
  return _kernal->getX1();
}


float  StaticDataset::getY1        ()  const
{
  return _kernal->getY1();
}


float  StaticDataset::getXinc      ()  const
{
  return _kernal->getXinc();
}


float  StaticDataset::getYinc      ()  const
{
  return _kernal->getYinc();
}


int    StaticDataset::getNx        ()  const
{
  return _kernal->getNx();
}


int    StaticDataset::getNy        ()  const
{
  return _kernal->getNy();
}


float  StaticDataset::getXend      ()  const
{
  return _kernal->getXend();
}


float  StaticDataset::getYend      ()  const
{
  return _kernal->getYend();
}


int  StaticDataset::numValues()  const
{
  return _kernal->numValues();
}


int  StaticDataset::numNilValues()  const
{
  return _kernal->numNilValues();
}


int  StaticDataset::numLiveValues()  const
{
  return _kernal->numLiveValues();
}


float StaticDataset::minimumValue()  const
{
  return _kernal->minimumValue();
}


float StaticDataset::maximumValue()  const
{
  return _kernal->maximumValue();
}


float StaticDataset::sumValues()  const
{
  return _kernal->sumValues();
}


float StaticDataset::averageValue()  const
{
  return _kernal->averageValue();
}



//--------------------- set active or selected items --------------------//
//--------------------- set active or selected items --------------------//
//--------------------- set active or selected items --------------------//

     // public.

float  StaticDataset::getActiveXbin   ()  const
{
  return _kernal->getActiveXbin();
}


float  StaticDataset::getActiveYbin   ()  const
{
  return _kernal->getActiveYbin();
}


int    StaticDataset::getActiveIx()  const
{
  return _kernal->getActiveIx();
}


int    StaticDataset::getActiveIy()  const
{
  return _kernal->getActiveIy();
}


int    StaticDataset::numSelections()  const
{
  return _kernal->numSelections();
}


int    StaticDataset::isSelected     (int ival)  const
{
  return _kernal->isSelected(ival);
}


int    StaticDataset::isSelected     (int ix, int iy)  const
{
  return _kernal->isSelected(ix, iy);
}



//------------------------- get bin or index -------------------------//
//------------------------- get bin or index -------------------------//
//------------------------- get bin or index -------------------------//

      // public.

int   StaticDataset::getMatchingIx (float xbin)  const
{
  return _kernal->getMatchingIx(xbin);
}


int   StaticDataset::getMatchingIy (float ybin)  const
{
  return _kernal->getMatchingIy(ybin);
}


int   StaticDataset::getNearestIx (float xbin)  const
{
  return _kernal->getNearestIx(xbin);
}


int   StaticDataset::getUnconstrainedIy (float ybin)  const
{
  return _kernal->getUnconstrainedIy(ybin);
}


int   StaticDataset::getUnconstrainedIx (float xbin)  const
{
  return _kernal->getUnconstrainedIx(xbin);
}


int   StaticDataset::getNearestIy (float ybin)  const
{
  return _kernal->getNearestIy(ybin);
}


float  StaticDataset::getXbin   (int ix)  const
{
  return _kernal->getXbin(ix);
}


float  StaticDataset::getYbin   (int iy)  const
{
  return _kernal->getYbin(iy);
}


float StaticDataset::getMatchingXbinCenter (float xbin)  const
{
  return _kernal->getMatchingXbinCenter(xbin);
}


float StaticDataset::getMatchingYbinCenter (float ybin)  const
{
  return _kernal->getMatchingYbinCenter(ybin);
}


float StaticDataset::getNearestXbinCenter (float xbin)  const
{
  return _kernal->getNearestXbinCenter(xbin);
}


float StaticDataset::getNearestYbinCenter (float ybin)  const
{
  return _kernal->getNearestYbinCenter(ybin);
}



//------------------------ get one static value ---------------------//
//------------------------ get one static value ---------------------//
//------------------------ get one static value ---------------------//

       // public.

float  StaticDataset::getValue     (int ival)  const
{
  return _kernal->getValue(ival);
}


float  StaticDataset::getValue     (int ix, int iy)  const
{
  return _kernal->getValue(ix, iy);
}


float  StaticDataset::getMatchingValue (float xbin, float ybin)  const
{
  return _kernal->getMatchingValue(xbin, ybin);
}


float  StaticDataset::getNearestValue (float xbin, float ybin)  const
{
  return _kernal->getNearestValue(xbin, ybin);
}


float  StaticDataset::getCenterValue (float xbin, float ybin)  const
{
  return _kernal->getCenterValue(xbin, ybin);
}


float  StaticDataset::getTerpValue (float xbin, float ybin)  const
{
  return _kernal->getTerpValue(xbin, ybin);
}


float  StaticDataset::getResampledValue (float xbin, float ybin,
                                         int interp, int extrap)  const
{
  return _kernal->getResampledValue(xbin, ybin, interp, extrap);
}



//------------------------ get static values -----------------------//
//------------------------ get static values -----------------------//
//------------------------ get static values -----------------------//

      // public.

void   StaticDataset::getStaticValues (float *values)  const
{
  _kernal->getStaticValues(values);
}



//------------------------- get weight ----------------------------//
//------------------------- get weight ----------------------------//
//------------------------- get weight ----------------------------//

      // public.

float  StaticDataset::getWeight (int ival, int xdist, int ydist)  const
{
  return _kernal->getWeight(ival, xdist, ydist);
}


float  StaticDataset::getWeight
                         (int ix, int iy, int xdist, int ydist)  const
{
  return _kernal->getWeight(ix, iy, xdist, ydist);
}



//---------------------------- set values --------------------------//
//---------------------------- set values --------------------------//
//---------------------------- set values --------------------------//

     // public.

void   StaticDataset::setStattype      (const char *stattype)
{
  int error = _helper->preChangeStattype();
  if(error) return;
  _kernal ->setStattype(stattype);
  _helper ->postChangeStattype();
}


void   StaticDataset::setNhx    (int nhx)
{
  int error = _helper->preChangeHeaderWords();
  if(error) return;
  _kernal->setNhx(nhx);
  _helper->postChangeHeaderWords();
}


void   StaticDataset::setNhy    (int nhy)
{
  int error = _helper->preChangeHeaderWords();
  if(error) return;
  _kernal->setNhy(nhy);
  _helper->postChangeHeaderWords();
}


void   StaticDataset::setNhx2   (int nhx2)
{
  int error = _helper->preChangeHeaderWords();
  if(error) return;
  _kernal->setNhx2(nhx2);
  _helper->postChangeHeaderWords();
}


void   StaticDataset::setNhy2   (int nhy2)
{
  int error = _helper->preChangeHeaderWords();
  if(error) return;
  _kernal->setNhy2(nhy2);
  _helper->postChangeHeaderWords();
}


void   StaticDataset::setX1     (float x1)
{
  int error = _helper->preTransformGroundPositions();
  if(error) return;
  _kernal->setX1(x1);
  _helper->postTransformGroundPositions();
}


void   StaticDataset::setY1     (float y1)
{
  int error = _helper->preTransformGroundPositions();
  if(error) return;
  _kernal->setY1(y1);
  _helper->postTransformGroundPositions();
}


void   StaticDataset::setXinc   (float xinc)
{
  int error = _helper->preTransformGroundPositions();
  if(error) return;
  _kernal->setXinc(xinc);
  _helper->postTransformGroundPositions();
}


void   StaticDataset::setYinc   (float yinc)
{
  int error = _helper->preTransformGroundPositions();
  if(error) return;
  _kernal->setYinc(yinc);
  _helper->postTransformGroundPositions();
}


void   StaticDataset::setNx   (int nx)
{
  if(nx == getNx()) return;
  if(!allowChangingFileSize()) return;
  int error = _helper->preTotalChanges();
  if(error) return;
  _kernal->setNx(nx);
  _helper->postTotalChanges();
}


void   StaticDataset::setNy   (int ny)
{
  if(ny == getNy()) return;
  if(!allowChangingFileSize()) return;
  int error = _helper->preTotalChanges();
  if(error) return;
  _kernal->setNy(ny);
  _helper->postTotalChanges();
}


void   StaticDataset::setXend   (float xend)
{
  if(!allowChangingFileSize()) return;
  int error = _helper->preTotalChanges();
  if(error) return;
  _kernal->setXend(xend);
  _helper->postTotalChanges();
}


void   StaticDataset::setYend   (float yend)
{
  if(!allowChangingFileSize()) return;
  int error = _helper->preTotalChanges();
  if(error) return;
  _kernal->setYend(yend);
  _helper->postTotalChanges();
}



//--------------------- set active or selected items --------------------//
//--------------------- set active or selected items --------------------//
//--------------------- set active or selected items --------------------//

     // public.

void   StaticDataset::setActiveXbin   (float xbin)
{
  _helper->preNewActiveGroundPosition();
  _kernal->setActiveXbin(xbin);
  _helper->postNewActiveGroundPosition();
}


void   StaticDataset::setActiveYbin   (float ybin)
{
  _helper->preNewActiveGroundPosition();
  _kernal->setActiveYbin(ybin);
  _helper->postNewActiveGroundPosition();
}



void   StaticDataset::setActiveIx   (int ix)
{
  _helper->preNewActiveGroundPosition();
  _kernal->setActiveIx(ix);
  _helper->postNewActiveGroundPosition();
}


void   StaticDataset::setActiveIy   (int iy)
{
  _helper->preNewActiveGroundPosition();
  _kernal->setActiveIy(iy);
  _helper->postNewActiveGroundPosition();
}



void   StaticDataset::setSelections (int ix, int iy,
                                  int nxsel, int nysel, int value)
{
  _helper->preChangeSelections(ix, iy, nxsel, nysel);
  _kernal->setSelections(ix, iy, nxsel, nysel, value);
  _helper->postChangeSelections(ix, iy, nxsel, nysel);
}


void   StaticDataset::clearSelections()
{
  _helper->preChangeSelections(0, 0, getNx(), getNy());
  _kernal->clearSelections();
  _helper->postChangeSelections(0, 0, getNx(), getNy());
}



//---------------------- set one static value -------------------------//
//---------------------- set one static value -------------------------//
//---------------------- set one static value -------------------------//

        // public.

void   StaticDataset::setValue     (int ival, float value)
{
  int iy = ival / getNx();
  int ix = ival - iy * getNx();
  assert(ix >= 0 && ix < getNx());
  assert(iy >= 0 && iy < getNy());
  int error = _helper->preChangeStaticValues(NULL, NULL, FALSE, ix, iy, 1, 1);
  if(error) return;
  _kernal->setValue(ival, value);
  _helper->postChangeStaticValues(NULL, FALSE, ix, iy, 1, 1);
}


void   StaticDataset::setValue     (int ix, int iy, float value)
{
  int error = _helper->preChangeStaticValues(NULL, NULL, FALSE, ix, iy, 1, 1);
  if(error) return;
  _kernal->setValue(ix, iy, value);
  _helper->postChangeStaticValues(NULL, FALSE, ix, iy, 1, 1);
}


void   StaticDataset::setMatchingValue (float xbin, float ybin, float value)
{
  int ix = getMatchingIx(xbin);
  int iy = getMatchingIy(ybin);
  setValue(ix, iy, value);
}



//---------------------- set static values ----------------------//
//---------------------- set static values ----------------------//
//---------------------- set static values ----------------------//

     // public.

void   StaticDataset::setStaticValues (void *doer, const float *values)
{
  int error = _helper->preChangeStaticValues
                      (doer, "resetting all static values...", FALSE, ARGS);
  if(error) return;
  _kernal->setStaticValues(values);
  _helper->postChangeStaticValues("reset all static values", FALSE, ARGS);
}



//--------------------------- copy data ---------------------------//
//--------------------------- copy data ---------------------------//
//--------------------------- copy data ---------------------------//

     // public.

void   StaticDataset::copyData (void *doer, const StaticDataset *other)
{
  int error = _helper->preCopy(doer, "copying all data...");
  if(error) return;
  _kernal->copyData(other->_kernal);
  _helper->postCopy("all data has been copied");
}



//----------------------- major edits ---------------------------//
//----------------------- major edits ---------------------------//
//----------------------- major edits ---------------------------//

         // public.

void  StaticDataset::gradeStaticValues
               (void *doer, float xmin, float xmax, float ymin, float ymax)
{
  int ixmin = getUnconstrainedIx(xmin);
  int ixmax = getUnconstrainedIx(xmax);
  int iymin = getUnconstrainedIy(ymin);
  int iymax = getUnconstrainedIy(ymax);
  gradeStaticValues(doer, ixmin, ixmax, iymin, iymax);
}



void  StaticDataset::gradeStaticValues
             (void *doer, int ixmin, int ixmax, int iymin, int iymax)
{
  char msg[111];
  int error = _kernal->gradeStaticValues
                            (TRUE, ixmin, ixmax, iymin, iymax, msg);
  int nxchng = ixmax - ixmin + 1;
  int nychng = iymax - iymin + 1;
  error = _helper->preChangeStaticValues
                           (doer, msg, error, ixmin, iymin, nxchng, nychng);
  if(error) return;
  error = _kernal->gradeStaticValues
                            (FALSE, ixmin, ixmax, iymin, iymax, msg);
  _helper->postChangeStaticValues(msg, error, ixmin, iymin, nxchng, nychng);
}



void  StaticDataset::removeRunningAverage
              (void *doer, int trim, float xrun, float yrun, int endflag)
{
  char msg[111];
  int error = _kernal->removeRunningAverage
                         (TRUE, StaticKernal::OPTION_REMOVE,
                          trim, xrun, yrun, endflag, msg);
  error = _helper->preChangeStaticValues(doer, msg, error, ARGS);
  if(error) return;
  error = _kernal->removeRunningAverage
                         (FALSE, StaticKernal::OPTION_REMOVE,
                          trim, xrun, yrun, endflag, msg);
  _helper->postChangeStaticValues(msg, error, ARGS);
}



void  StaticDataset::smoothStaticValues
                (void *doer, int trim, float xrun, float yrun, int endflag)
{
  char msg[111];
  int error = _kernal->removeRunningAverage
                         (TRUE, StaticKernal::OPTION_SMOOTH,
                          trim, xrun, yrun, endflag, msg);
  error = _helper->preChangeStaticValues(doer, msg, error, ARGS);
  if(error) return;
  error = _kernal->removeRunningAverage
                         (FALSE, StaticKernal::OPTION_SMOOTH,
                          trim, xrun, yrun, endflag, msg);
  _helper->postChangeStaticValues(msg, error, ARGS);
}



void  StaticDataset::integrateStaticValues(void *doer)
{
  char msg[111];
  int error = _kernal->integrateStaticValues(TRUE, msg);
  error = _helper->preChangeStaticValues(doer, msg, error, ARGS);
  if(error) return;
  error = _kernal->integrateStaticValues(FALSE, msg);
  if(error == FALSE && strcmp(getStattype(), "INC") == 0) setStattype("RESID");
  _helper->postChangeStaticValues(msg, error, ARGS);
}



void  StaticDataset::clearStaticValues(void *doer)
{
  char msg[111];
  int error = _kernal->clearStaticValues(TRUE, msg);
  error = _helper->preChangeStaticValues(doer, msg, error, ARGS);
  if(error) return;
  error = _kernal->clearStaticValues(FALSE, msg);
  _helper->postChangeStaticValues(msg, error, ARGS);
}



void  StaticDataset::randomizeStaticValues(void *doer)
{
  char msg[111];
  int error = _kernal->randomizeStaticValues(TRUE, msg);
  error = _helper->preChangeStaticValues(doer, msg, error, ARGS);
  if(error) return;
  error = _kernal->randomizeStaticValues(FALSE, msg);
  _helper->postChangeStaticValues(msg, error, ARGS);
}



void  StaticDataset::multiplyStaticValuesByConstant
                                           (void *doer, float constant)
{
  char msg[111];
  int error = _kernal->multiplyStaticValuesByConstant(TRUE, constant, msg);
  error = _helper->preChangeStaticValues(doer, msg, error, ARGS);
  if(error) return;
  error = _kernal->multiplyStaticValuesByConstant(FALSE, constant, msg);
  _helper->postChangeStaticValues(msg, error, ARGS);
}



void  StaticDataset::addConstantToStaticValues
                                           (void *doer, float constant)
{
  char msg[111];
  int error = _kernal->addConstantToStaticValues(TRUE, constant, msg);
  error = _helper->preChangeStaticValues(doer, msg, error, ARGS);
  if(error) return;
  error = _kernal->addConstantToStaticValues(FALSE, constant, msg);
  _helper->postChangeStaticValues(msg, error, ARGS);
}



void  StaticDataset::replaceNilsWithSpecifiedValue(void *doer, float constant)
{
  char msg[111];
  int error = _kernal->replaceNilsWithSpecifiedValue(TRUE, constant, msg);
  error = _helper->preChangeStaticValues(doer, msg, error, ARGS);
  if(error) return;
  error = _kernal->replaceNilsWithSpecifiedValue(FALSE, constant, msg);
  _helper->postChangeStaticValues(msg, error, ARGS);
}



void  StaticDataset::replaceRangeOfValuesWithNils
                                   (void *doer, float range1, float range2)
{
  char msg[111];
  int error = _kernal->replaceRangeOfValuesWithNils
                                   (TRUE, range1, range2, msg);
  error = _helper->preChangeStaticValues(doer, msg, error, ARGS);
  if(error) return;
  error = _kernal->replaceRangeOfValuesWithNils
                                   (FALSE, range1, range2, msg);
  _helper->postChangeStaticValues(msg, error, ARGS);
}



void  StaticDataset::replaceNilsWithTerpValues(void *doer)
{
  char msg[111];
  int error = _kernal->replaceNilsWithTerpValues(TRUE, msg);
  error = _helper->preChangeStaticValues(doer, msg, error, ARGS);
  if(error) return;
  error = _kernal->replaceNilsWithTerpValues(FALSE, msg);
  _helper->postChangeStaticValues(msg, error, ARGS);
}



void  StaticDataset::interpNilsInXdirection(void *doer)
{
  char msg[111];
  int error = _kernal->interpNilsInXdirection(TRUE, msg);
  error = _helper->preChangeStaticValues(doer, msg, error, ARGS);
  if(error) return;
  error = _kernal->interpNilsInXdirection(FALSE, msg);
  _helper->postChangeStaticValues(msg, error, ARGS);
}



void  StaticDataset::interpNilsInYdirection(void *doer)
{
  char msg[111];
  int error = _kernal->interpNilsInYdirection(TRUE, msg);
  error = _helper->preChangeStaticValues(doer, msg, error, ARGS);
  if(error) return;
  error = _kernal->interpNilsInYdirection(FALSE, msg);
  _helper->postChangeStaticValues(msg, error, ARGS);
}



void  StaticDataset::interpNilsFromNearby
                          (void *doer, int ixdist, int iydist, int require)
{
  char msg[111];
  int error = _kernal->interpNilsFromNearby
                                   (TRUE, ixdist, iydist, require, msg);
  error = _helper->preChangeStaticValues(doer, msg, error, ARGS);
  if(error) return;
  error = _kernal->interpNilsFromNearby
                                   (FALSE, ixdist, iydist, require, msg);
  _helper->postChangeStaticValues(msg, error, ARGS);
}



void  StaticDataset::transformGroundPositions          (void *doer,
                        float x1old, float x2old, float y1old, float y2old,
                        float x1new, float x2new, float y1new, float y2new)
{
  char msg[111];
  int reversing;
  int error = _kernal->transformGroundPositions
                        (TRUE, x1old, x2old, y1old, y2old,
                               x1new, x2new, y1new, y2new, &reversing, msg);
  error = _helper->preTransformGroundPositions(doer, msg, error);
  if(error) return;
  if(reversing) _helper->preChangeStaticValues(NULL, NULL, FALSE, ARGS);
  error = _kernal->transformGroundPositions
                       (FALSE, x1old, x2old, y1old, y2old,
                               x1new, x2new, y1new, y2new, &reversing, msg);
  if(reversing) _helper->postChangeStaticValues(NULL, FALSE, ARGS);
  _helper->postTransformGroundPositions(msg, error);
}



void  StaticDataset::resampleStaticValues
                              (void *doer, int interp, int extrap,
                                           float  x1, float  y1,
                                           float  xinc, float  yinc,
                                           int  nx, int  ny)
{
  char msg[111];
  int error = _kernal->resampleStaticValues(TRUE, interp, extrap,
                                          x1, y1, xinc, yinc, nx, ny, msg);
  error = _helper->preTotalChanges(doer, msg, error);
  if(error) return;
  error = _kernal->resampleStaticValues(FALSE, interp, extrap,
                                          x1, y1, xinc, yinc, nx, ny, msg);
  _helper->postTotalChanges(msg, error);
}



//-------------------- interact with undo file -----------------------//
//-------------------- interact with undo file -----------------------//
//-------------------- interact with undo file -----------------------//

     // public.

int StaticDataset::allowReadDeleteUndoFile(void *doer)  const
{
  return _helper->allowReadDeleteUndoFile(doer);
}



void StaticDataset::maybeDeleteUndoFile(void *doer)
{
  _helper->maybeDeleteUndoFile(doer);
}



void StaticDataset::maybeReadUndoFile(void *doer)
{
  _helper->maybeReadUndoFile(doer);
}



void StaticDataset::saveUndoFile(void *doer)
{
  _helper->saveUndoFile(doer);
}



//------------------ save backup file --------------------------//
//------------------ save backup file --------------------------//
//------------------ save backup file --------------------------//

        // public.

void StaticDataset::saveBackupFile()
{
  _helper->saveBackupFile();
}



//------------------------ read and save file --------------------------//
//------------------------ read and save file --------------------------//
//------------------------ read and save file --------------------------//

     // public.


int  StaticDataset::readForeign (void *doer, const char *filename,
                                 class StatioWrapper *statio, char *msg)
{
  int error = _helper->preRead(doer, "reading static file...", msg);
  if(error) return TRUE;
  error = _kernal->readForeign(filename, statio, msg);
  _helper->postRead(filename, msg, error);
  return error;
}



int  StaticDataset::saveFile (const char *filename,
                              class StatioWrapper *statio, char *msg)
{
  _helper->preSave("saving static file...");
  int error = _kernal->saveFile(filename, statio, msg);
  _helper->postSave(filename, msg, error);
  return error;
}



//------------------------- update pjar -------------------------------------//
//------------------------- update pjar -------------------------------------//
//------------------------- update pjar -------------------------------------//

         // public.

void StaticDataset::updatePjar (class StatioWrapper *statio, int skiphist)
{
  _kernal->updatePjar(statio, skiphist);
}



//------------------- learn last file read or saved ----------------//
//------------------- learn last file read or saved ----------------//
//------------------- learn last file read or saved ----------------//

         // public.

const char *StaticDataset::lastFileSaved       ()  const
{
  return _helper->lastFileSaved();
}


const char *StaticDataset::lastFileRead        ()  const
{
  return _helper->lastFileRead();
}


const char *StaticDataset::lastBackupFileSaved ()  const
{
  return _helper->lastBackupFileSaved();
}



//------------------------------- end -------------------------------//
//------------------------------- end -------------------------------//
//------------------------------- end -------------------------------//

