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



//-------------------------- static_manager.cc -------------------------//
//-------------------------- static_manager.cc -------------------------//
//-------------------------- static_manager.cc -------------------------//

//            implementation file for the StaticManager class
//                     not derived from any class
//                          subdirectory stat


#include "stat/static_manager.hh"
#include "stat/static_dataset_array.hh"
#include "stat/static_dataset.hh"
#include "stat/static_informer.hh"
#include "named_constants.h"
#include <string.h>
#include <iostream.h>
#include <stdlib.h>
#include <stdio.h>
#include <assert.h>
#include <limits.h>


#define BEFORE  _informer->beforeChanges();
#define AFTER   _informer->afterChanges();


//---------------------- constructor ---------------------------//
//---------------------- constructor ---------------------------//
//---------------------- constructor ---------------------------//


StaticManager::StaticManager(const char *progname)
           :
          _array               (NULL),
          _informer            (NULL)
{
  _informer  = new StaticInformer();
  _array     = new StaticDatasetArray(progname, _informer);
}



//------------------------ destructor --------------------------//
//------------------------ destructor --------------------------//
//------------------------ destructor --------------------------//

 // _informer->dataGoingAway() must be called before anything is destroyed.
 // _informer must not be deleted until after all data are destroyed.

StaticManager::~StaticManager()
{
  saveBackupFiles();
  _informer->dataGoingAway();
  delete _array;
  delete _informer;
}



//-------------------- miscellaneous functions -------------------//
//-------------------- miscellaneous functions -------------------//
//-------------------- miscellaneous functions -------------------//

        // public.

int StaticManager::numDatasets()  const
{
  return _array->numElements();
}


int StaticManager::getActiveDatasetIndex()  const
{
  return _array->getActiveIndex();
}


int StaticManager::getReferenceDatasetIndex()  const
{
  return _array->getReferenceIndex();
}



void  StaticManager::setActiveDatasetIndex (int index)
{
  BEFORE
  saveBackupFiles();
  _informer->preNewActiveDataset();
  _array->setActiveDatasetIndex(index);
  _informer->showMessage("active dataset changed");
  _informer->postNewActiveDataset();
  AFTER
}



void  StaticManager::setReferenceDatasetIndex (int index)
{
  BEFORE
  saveBackupFiles();
  _informer->preNewReferenceDataset();
  _array->setReferenceDatasetIndex(index);
  _informer->showMessage("reference dataset changed");
  _informer->postNewReferenceDataset();
  AFTER
}



StaticDataset *StaticManager::dataset(int index)  const
{
  return _array->dataset(index);
}



StaticDataset *StaticManager::activeDataset()  const
{
  return _array->activeDataset();
}



StaticDataset *StaticManager::referenceDataset()  const
{
  return _array->referenceDataset();
}



//------------------ insert and remove dataset -----------------------//
//------------------ insert and remove dataset -----------------------//
//------------------ insert and remove dataset -----------------------//

         // public.

StaticDataset *StaticManager::appendNewDataset()
{
  BEFORE
  saveBackupFiles();
  int n = _array->numElements();
  _informer->preRemoveInsertDatasets(n, 0, 1);
  _array->appendNewDataset();
  _informer->showMessage("new dataset created");
  _informer->postRemoveInsertDatasets(n, 0, 1);
  AFTER
  return dataset(n);
}



StaticDataset *StaticManager::appendNewActiveDataset()
{
  BEFORE
  saveBackupFiles();
  int n = _array->numElements();
  _informer->preNewActiveDataset();
  _informer->preRemoveInsertDatasets(n, 0, 1);
  _array->appendNewActiveDataset();
  _informer->showMessage("new dataset created and made active");
  _informer->postRemoveInsertDatasets(n, 0, 1);
  _informer->postNewActiveDataset();
  AFTER
  return dataset(n);
}



StaticDataset *StaticManager::appendNewReferenceDataset()
{
  BEFORE
  saveBackupFiles();
  int n = _array->numElements();
  _informer->preNewReferenceDataset();
  _informer->preRemoveInsertDatasets(n, 0, 1);
  _array->appendNewReferenceDataset();
  _informer->showMessage("new dataset created and made the reference dataset");
  _informer->postRemoveInsertDatasets(n, 0, 1);
  _informer->postNewReferenceDataset();
  AFTER
  return dataset(n);
}



StaticDataset *StaticManager::duplicateDataset(int index)
{
  BEFORE
  StaticDataset *old_dataset = dataset(index);
  StaticDataset *new_dataset = appendNewDataset();
  new_dataset->copyData(NULL, old_dataset);
  AFTER
  return new_dataset;
}


StaticDataset *StaticManager::duplicateActiveDataset()
{
  int index = getActiveDatasetIndex();
  return duplicateDataset(index);
}


StaticDataset *StaticManager::duplicateReferenceDataset()
{
  int index = getReferenceDatasetIndex();
  return duplicateDataset(index);
}



void StaticManager::removeDataset(int index)
{
  if(numDatasets() == 1)
      {
      _informer->showMessage("cannot remove the only dataset");
      _informer->ringBell();
      return;
      }
  int active    = _array->getActiveIndex();
  int reference = _array->getReferenceIndex();
  if(active    >= index) _informer->preNewActiveDataset();
  if(reference >= index) _informer->preNewReferenceDataset();
  _informer->preRemoveInsertDatasets(index, 1, 0);
  _array->removeDataset(index);
  _informer->postRemoveInsertDatasets(index, 1, 0);
  if(reference >= index) _informer->postNewReferenceDataset();
  if(active    >= index) _informer->postNewActiveDataset();
}


void StaticManager::removeActiveDataset()
{
  int index = getActiveDatasetIndex();
  removeDataset(index);
}


void StaticManager::removeReferenceDataset()
{
  int index = getReferenceDatasetIndex();
  removeDataset(index);
}



//---------------------- merge selected datasets ---------------------//
//---------------------- merge selected datasets ---------------------//
//---------------------- merge selected datasets ---------------------//

         // public.

void StaticManager::mergeSelectedDatasets (void *doer,
                                           int action, int size, int where,
                                           int nils, int interp, int extrap,
                                           int xdist, int ydist)
{
  assert(action == ACTION_ADD || action == ACTION_AV ||
         action == ACTION_SUB);

  assert(size == SIZE_ALL || size == SIZE_ACTIVE);

  assert(where == WHERE_ACTIVE || where == WHERE_NEW ||
         where == WHERE_NEWACTIVE);

  assert(nils == NILS_KEEP || nils == NILS_ZERO || nils == NILS_TERP ||
         nils == NILS_XDIR || nils == NILS_YDIR);

  assert(interp == INTERP_NEAR || interp == INTERP_TERP);
  assert(extrap == EXTRAP_EDGE || extrap == EXTRAP_NILS
                               || extrap == EXTRAP_ZERO);

  int           num    = numDatasets();
  int           nsel   = numDatasetsSelected();
  StaticDataset *active = activeDataset();

  if(nsel <= 1)
      {
      _informer->showMessage
                   ("you need at least two selected datasets to merge");
      _informer->ringBell();
      return;
      }
  if(where == WHERE_ACTIVE && active->isLocked())
      {
      _informer->showMessage
         ("active dataset is locked and cannot be replaced by merged data");
      _informer->ringBell();
      return;
      }
  if(action == ACTION_SUB && active->notSelected())
      {
      _informer->showMessage
         ("active dataset must also be selected for subtraction action");
      _informer->ringBell();
      return;
      }
  if(action == ACTION_SUB && nsel != 2)
      {
      _informer->showMessage
        ("there must be exactly two selected datasets for subtraction action");
      _informer->ringBell();
      return;
      }
  if(xdist < 0 || ydist < 0)
      {
      _informer->showMessage
        ("xdist and ydist must be >= 0");
      _informer->ringBell();
      return;
      }

////////// verify matching header words.

  int  nhx  = active->getNhx();
  int  nhy  = active->getNhy();
  int  nhx2 = active->getNhx2();
  int  nhy2 = active->getNhy2();
  int  index;
  char type[12];
  strcpy(type, "");
  for(index = 0; index < num; index++)
      {
      if(dataset(index)->isSelected())
          {
          if(nhx  != dataset(index)->getNhx()  ||
             nhy  != dataset(index)->getNhy()  ||
             nhx2 != dataset(index)->getNhx2() ||
             nhy2 != dataset(index)->getNhy2())
               {
               _informer->showMessage
              ("mismatching header words among active and selected datasets");
               _informer->ringBell();
               return;
               }
          const char *type2 = dataset(index)->getStattype();
          if     (strcmp(type, ""   ) == 0) strcpy(type, type2);
          else if(strcmp(type, type2) != 0) strcpy(type, "MISC");
          }
      }

////////// get resampling parameters x1, y1, xinc, yinc, nx, ny.

  float x1   = active->getX1();
  float y1   = active->getY1();
  float xinc = active->getXinc();
  float yinc = active->getYinc();
  int   nx   = active->getNx();
  int   ny   = active->getNy();
  if(size == SIZE_ALL)
      {
      float xend = active->getXend();
      float yend = active->getYend();
      for(index = 0; index < num; index++)
          {
          if(dataset(index)->isSelected())
              {
              x1   = MinimumValue(x1  , dataset(index)->getX1());
              y1   = MinimumValue(y1  , dataset(index)->getY1());
              xend = MaximumValue(xend, dataset(index)->getXend());
              yend = MaximumValue(yend, dataset(index)->getYend());
              }
          }
      int  ix1   = active->getUnconstrainedIx(x1);
      int  iy1   = active->getUnconstrainedIy(y1);
      int  ixend = active->getUnconstrainedIx(xend);
      int  iyend = active->getUnconstrainedIy(yend);
    
      x1 = active->getX1() + ix1 * xinc;
      y1 = active->getY1() + iy1 * yinc;
      nx = ixend - ix1 + 1;
      ny = iyend - iy1 + 1;
      }

////////// get merged static values.

  _informer->beforeChanges();
  _informer->showMessage("merging selected datasets...");
  float *values = new float [nx * ny];
  getMergedValues(action, nils, interp, extrap, xdist, ydist,
                  x1, y1, xinc, yinc, nx, ny, values);

////////// create merged dataset.

  StaticDataset *merged = new StaticDataset();
  merged->copyData(NULL, active);
  if(merged->allowChangingFileSize() == FALSE) merged->clearStaticValues(NULL);
  merged->setX1(x1);
  merged->setY1(y1);
  merged->setNx(nx);
  merged->setNy(ny);
  merged->setStattype(type);
  merged->setStaticValues(NULL, values);
  delete [] values;

////////// copy merged dataset into desired location.

  StaticDataset *desired;
  if(where == WHERE_NEWACTIVE)
      {
      desired = appendNewActiveDataset();
      }
  else if(where == WHERE_NEW)
      {
      desired = appendNewDataset();
      }
  else
      {
      desired = activeDataset();
      }
  assert(desired->notLocked());
  desired->copyData(doer, merged);
  delete merged;
  _informer->showMessage("selected datasets have been merged");
  _informer->afterChanges();
}



//---------------------- get merged values --------------------------//
//---------------------- get merged values --------------------------//
//---------------------- get merged values --------------------------//

      // private.
      // returns values[nx * ny].

void StaticManager::getMergedValues
               (int action, int nils, int interp, int extrap,
                int xdist, int ydist,
                float x1, float y1, float xinc, float yinc, int nx, int ny,
                float *values)
{
  int num  = numDatasets();
  int nsel = numDatasetsSelected();
  int nval = nx * ny;

  float         *weights = new float [nval];
  StaticDataset *scratch = new StaticDataset();
  memset(values , 0, (unsigned int)nval * sizeof(float));
  memset(weights, 0, (unsigned int)nval * sizeof(float));
  for(int index = 0; index < num; index++)
      {
      if(dataset(index)->notSelected()) continue;
      scratch->copyData(NULL, dataset(index));
      switch(nils)
         {
         case NILS_KEEP: break;
         case NILS_ZERO: scratch->replaceNilsWithSpecifiedValue(NULL, 0.0);
                         break;
         case NILS_TERP: scratch->replaceNilsWithTerpValues(NULL);
                         break;
         case NILS_XDIR: scratch->interpNilsInXdirection(NULL);
                         break;
         case NILS_YDIR: scratch->interpNilsInYdirection(NULL);
                         break;
         default: assert(FALSE);
         }
      scratch->resampleStaticValues(NULL, interp, extrap,
                                    x1, y1, xinc, yinc, nx, ny);
      for(int ival = 0; ival < nval; ival++)
          {
          float value  = scratch->getValue (ival);
          float weight = scratch->getWeight(ival, xdist, ydist);
          if(value == FNIL) continue;
          if(action == ACTION_SUB)
              {
              if(dataset(index)->notActive()) value = -value;
              weight = 1.0;
              }
          values [ival] += weight * value;
          weights[ival] += weight;
          }
      }
  delete scratch;
  for(int ival = 0; ival < nval; ival++)
      {
      float value  = values [ival];
      float weight = weights[ival];
      if(weight > 0.0)
          {
          value /= weight;
          if(action == ACTION_ADD)
              {
              value *= nsel;
              }
          else if(action == ACTION_SUB)
              {
              if(weight != 2.0) value = FNIL;
              }
          }
      else
          {
          value = FNIL;
          }
      values[ival] = value;
      }
  delete [] weights;
}



//--------------------- convenience functions --------------------//
//--------------------- convenience functions --------------------//
//--------------------- convenience functions --------------------//

         // public.

int StaticManager::numDatasetsNeedingSaving()  const
{
  int need = 0;
  for(int index = 0; index < numDatasets(); index++)
      {
      if(_array->dataset(index)->dataNeedsSaving()) need++;
      }
  return need;
}



int StaticManager::numDatasetsBackedUp()  const
{
  int backedup = 0;
  for(int index = 0; index < numDatasets(); index++)
      {
      if(_array->dataset(index)->dataBackedUp()) backedup++;
      }
  return backedup;
}



int StaticManager::numDatasetsSelected()  const
{
  int selected = 0;
  for(int index = 0; index < numDatasets(); index++)
      {
      if(_array->dataset(index)->isSelected()) selected++;
      }
  return selected;
}



int StaticManager::numDatasetsLocked()  const
{
  int locked = 0;
  for(int index = 0; index < numDatasets(); index++)
      {
      if(_array->dataset(index)->isLocked()) locked++;
      }
  return locked;
}



int StaticManager::numDatasetsWithUndoFile(void *doer)  const
{
  int num = 0;
  for(int index = 0; index < numDatasets(); index++)
      {
      if(_array->dataset(index)->allowReadDeleteUndoFile(doer)) num++;
      }
  return num;
}



void StaticManager::selectAllDatasets()
{
  if(numDatasetsSelected() == numDatasets()) return;
  BEFORE
  for(int index = 0; index < numDatasets(); index++)
      {
      _array->dataset(index)->selectThisDataset();
      }
  AFTER
}


void StaticManager::unselectAllDatasets()
{
  if(numDatasetsSelected() == 0) return;
  BEFORE
  for(int index = 0; index < numDatasets(); index++)
      {
      _array->dataset(index)->unselectThisDataset();
      }
  AFTER
}


void StaticManager::lockAllDatasets()
{
  if(numDatasetsLocked() == numDatasets()) return;
  BEFORE
  for(int index = 0; index < numDatasets(); index++)
      {
      _array->dataset(index)->lockData();
      }
  AFTER
}


void StaticManager::unlockAllDatasets()
{
  if(numDatasetsLocked() == 0) return;
  BEFORE
  for(int index = 0; index < numDatasets(); index++)
      {
      _array->dataset(index)->unlockData();
      }
  AFTER
}



void StaticManager::maybeDeleteUndoFiles(void *doer)
{
  if(numDatasetsWithUndoFile(doer) == 0) return;
  BEFORE
  for(int index = 0; index < numDatasets(); index++)
      {
      _array->dataset(index)->maybeDeleteUndoFile(doer);
      }
  AFTER
}



void StaticManager::saveBackupFiles()
{
  if(numDatasetsBackedUp() == numDatasets()) return;
  BEFORE
  for(int index = 0; index < numDatasets(); index++)
      {
      _array->dataset(index)->saveBackupFile();
      }
  AFTER
}



//-------------------------- end -------------------------------//
//-------------------------- end -------------------------------//
//-------------------------- end -------------------------------//

