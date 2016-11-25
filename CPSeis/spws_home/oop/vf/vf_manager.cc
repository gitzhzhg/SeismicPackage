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



//-------------------------- vf_manager.cc -------------------------//
//-------------------------- vf_manager.cc -------------------------//
//-------------------------- vf_manager.cc -------------------------//

//            implementation file for the VfManager class
//                     not derived from any class
//                          subdirectory vf


#include "vf/vf_manager.hh"
#include "vf/vf_dataset_array.hh"
#include "vf/vf_dataset.hh"
#include "vf/vf_informer.hh"
#include "vf/vf_utilities.hh"
#include "named_constants.h"
#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <assert.h>
#include <limits.h>


#define BEFORE  _informer->beforeChanges();
#define AFTER   _informer->afterChanges();


//---------------------- constructor ---------------------------//
//---------------------- constructor ---------------------------//
//---------------------- constructor ---------------------------//


VfManager::VfManager(long number, long numedit, const char *progname,
                                                int bogus_velocities)
           :
          _array               (NULL),
          _informer            (NULL),
          _utilities           (NULL)
{
  _informer  = new VfInformer();
  _utilities = new VfUtilities(bogus_velocities);
  _array     = new VfDatasetArray
                       (number, numedit, progname, _informer, _utilities);
}



//------------------------ destructor --------------------------//
//------------------------ destructor --------------------------//
//------------------------ destructor --------------------------//

 // _informer->dataGoingAway() must be called before anything is destroyed.
 // _informer must not be deleted until after all data are destroyed.

VfManager::~VfManager()
{
  saveBackupFiles();
  _informer->dataGoingAway();
  delete _array;
  delete _utilities;
  delete _informer;
}



//-------------------- miscellaneous functions -------------------//
//-------------------- miscellaneous functions -------------------//
//-------------------- miscellaneous functions -------------------//

        // public.

long VfManager::numDatasets()  const
{
  return _array->numElements();
}



long VfManager::numEditableDatasets()  const
{
  return _array->numEditableDatasets();
}



long VfManager::getActiveDatasetIndex()  const
{
  return _array->getActiveIndex();
}



void  VfManager::setActiveDatasetIndex (long index)
{
  BEFORE
  saveBackupFiles();
  _informer->preNewActiveDataset();
  _array->setActiveDatasetIndex(index);
  _informer->showMessage("new active velocity dataset");
  _informer->postNewActiveDataset();
  AFTER
}



long VfManager::getReferenceDatasetIndex()  const
{
  return _array->getReferenceIndex();
}



void  VfManager::setReferenceDatasetIndex (long index)
{
  BEFORE
  saveBackupFiles();
  _informer->preNewReferenceDataset();
  _array->setReferenceDatasetIndex(index);
  _informer->showMessage("new comparison velocity dataset");
  _informer->postNewReferenceDataset();
  AFTER
}



VfDataset *VfManager::dataset(long index)  const
{
  return _array->dataset(index);
}



VfDataset *VfManager::activeDataset()  const
{
  return _array->activeDataset();
}



VfDataset *VfManager::referenceDataset()  const
{
  return _array->referenceDataset();
}



//--------------- pre and post change bin tolerances ----------------//
//--------------- pre and post change bin tolerances ----------------//
//--------------- pre and post change bin tolerances ----------------//

         // private.

void VfManager::preChangeBinTolerances()
{
  for(long index = 0; index < _array->numElements(); index++)
      {
      _informer->preNewNeighbors(dataset(index));
      }
  _informer->preChangeBinTolerances();
}



void VfManager::postChangeBinTolerances()
{
long index;

  _informer->postChangeBinTolerances();
  for(index = 0; index < _array->numElements(); index++)
      {
      dataset(index)->newBinTolerancesByVfManagerOnly();
      }
  for(index = 0; index < _array->numElements(); index++)
      {
      _informer->postNewNeighbors(dataset(index));
      }
}



//------------------------- set bin tolerances ---------------------//
//------------------------- set bin tolerances ---------------------//
//------------------------- set bin tolerances ---------------------//

         // public.

void VfManager::setXcenter(float value)
{
  preChangeBinTolerances();
  _utilities->setXcenterByVfManagerOnly(value);
  postChangeBinTolerances();
}

void VfManager::setYcenter(float value)
{
  preChangeBinTolerances();
  _utilities->setYcenterByVfManagerOnly(value);
  postChangeBinTolerances();
}


void VfManager::setXwidth(float value)
{
  preChangeBinTolerances();
  _utilities->setXwidthByVfManagerOnly(value);
  postChangeBinTolerances();
}


void VfManager::setYwidth(float value)
{
  preChangeBinTolerances();
  _utilities->setYwidthByVfManagerOnly(value);
  postChangeBinTolerances();
}



//------------------ insert and remove comparison dataset ------------//
//------------------ insert and remove comparison dataset ------------//
//------------------ insert and remove comparison dataset ------------//

         // public.

void VfManager::appendNewComparisonDataset()
{
  long n = _array->numElements();
  _informer->preRemoveInsertDatasets(n, 0, 1);
  _array->appendDataset();
  _informer->postRemoveInsertDatasets(n, 0, 1);
}



void VfManager::removeComparisonDataset(long index)
{
  long active    = _array->getActiveIndex();
  long reference = _array->getReferenceIndex();
  if(active    >= index) _informer->preNewActiveDataset();
  if(reference >= index) _informer->preNewReferenceDataset();
  _informer->preRemoveInsertDatasets(index, 1, 0);
  _array->removeDataset(index);
  _informer->postRemoveInsertDatasets(index, 1, 0);
  if(reference >= index) _informer->postNewReferenceDataset();
  if(active    >= index) _informer->postNewActiveDataset();
}



//--------------------- convenience functions --------------------//
//--------------------- convenience functions --------------------//
//--------------------- convenience functions --------------------//

         // public.

long VfManager::numDatasetsNeedingSaving()  const
{
  long need = 0;
  for(long index = 0; index < _array->numEditableDatasets(); index++)
      {
      if(_array->dataset(index)->dataNeedsSaving()) need++;
      }
  return need;
}



long VfManager::numDatasetsBackedUp()  const
{
  long backedup = 0;
  for(long index = 0; index < _array->numEditableDatasets(); index++)
      {
      if(_array->dataset(index)->dataBackedUp()) backedup++;
      }
  return backedup;
}



long VfManager::numDatasetsSelected()  const
{
  long selected = 0;
  for(long index = 0; index < _array->numElements(); index++)
      {
      if(_array->dataset(index)->isSelected()) selected++;
      }
  return selected;
}



long VfManager::numDatasetsLocked()  const
{
  long locked = 0;
  for(long index = 0; index < _array->numEditableDatasets(); index++)
      {
      if(_array->dataset(index)->isLocked()) locked++;
      }
  return locked;
}



long VfManager::numDatasetsWithUndoFile(void *doer)  const
{
  long num = 0;
  for(long index = 0; index < _array->numEditableDatasets(); index++)
      {
      if(_array->dataset(index)->allowReadDeleteUndoFile(doer)) num++;
      }
  return num;
}



void VfManager::selectAllDatasets()
{
  if(numDatasetsSelected() == numDatasets()) return;
  BEFORE
  for(long index = 0; index < _array->numElements(); index++)
      {
      _array->dataset(index)->selectThisDataset();
      }
  AFTER
}


void VfManager::unselectAllDatasets()
{
  if(numDatasetsSelected() == 0) return;
  BEFORE
  for(long index = 0; index < _array->numElements(); index++)
      {
      _array->dataset(index)->unselectThisDataset();
      }
  AFTER
}


void VfManager::lockAllDatasets()
{
  if(numDatasetsLocked() == numEditableDatasets()) return;
  BEFORE
  for(long index = 0; index < _array->numEditableDatasets(); index++)
      {
      _array->dataset(index)->lockData();
      }
  AFTER
}


void VfManager::unlockAllDatasets()
{
  if(numDatasetsLocked() == 0) return;
  BEFORE
  for(long index = 0; index < _array->numEditableDatasets(); index++)
      {
      _array->dataset(index)->unlockData();
      }
  AFTER
}



void VfManager::maybeDeleteUndoFiles(void *doer)
{
  if(numDatasetsWithUndoFile(doer) == 0) return;
  BEFORE
  for(long index = 0; index < _array->numEditableDatasets(); index++)
      {
      _array->dataset(index)->maybeDeleteUndoFile(doer);
      }
  AFTER
}



void VfManager::saveBackupFiles()
{
  if(numDatasetsBackedUp() == numEditableDatasets()) return;
  BEFORE
  for(long index = 0; index < _array->numEditableDatasets(); index++)
      {
      _array->dataset(index)->saveBackupFile();
      }
  AFTER
}



//-------------------------- end -------------------------------//
//-------------------------- end -------------------------------//
//-------------------------- end -------------------------------//

