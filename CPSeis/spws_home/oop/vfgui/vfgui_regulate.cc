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

//---------------------- vfgui_regulate.cc ------------------------//
//---------------------- vfgui_regulate.cc ------------------------//
//---------------------- vfgui_regulate.cc ------------------------//

//         implementation file for the VfguiRegulate class
//                   not derived from any class
//                       subdirectory vfgui


#include "vfgui/vfgui_regulate.hh"
#include "vf/vf_manager.hh"
#include "vf/vf_inform.hh"
#include "vf/vf_dataset.hh"
#include "sl/slp_push.hh"
#include "sl/slp_toggle.hh"
#include "sl/sl_quest_pop.hh"
#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <assert.h>



//--------------------- constructor and destructor --------------------//
//--------------------- constructor and destructor --------------------//
//--------------------- constructor and destructor --------------------//


VfguiRegulate::VfguiRegulate (VfManager *manager)
       :
               _manager     (manager),
               _parent      (NULL),
               _inform      (NULL)
{
  assert(_manager);
}



VfguiRegulate::~VfguiRegulate()
{
  if(_inform) delete _inform;
}



//--------------------------- backup -----------------------------//
//--------------------------- backup -----------------------------//
//--------------------------- backup -----------------------------//


static void backup1_trap(void *data, long /*ident*/)
{
  VfManager *manager = (VfManager*)data;
  manager->activeDataset()->saveBackupFile();
}


static void backup2_trap(void *data, long /*ident*/)
{
  VfManager *manager = (VfManager*)data;
  manager->saveBackupFiles();
}


static void backup3_trap(void *data, long /*ident*/)
{
  VfManager *manager = (VfManager*)data;
  manager->dataset(0)->saveBackupFile();
}



static long backup1_sense_fun(void *data)
{
  VfManager *manager = (VfManager*)data;
  return !manager->activeDataset()->dataBackedUp();
}


static long backup2_sense_fun(void *data)
{
  VfManager *manager = (VfManager*)data;
  long numedit = manager->numEditableDatasets();
  long backed  = manager->numDatasetsBackedUp();
  return (backed < numedit);
}


static long backup3_sense_fun(void *data)
{
  VfManager *manager = (VfManager*)data;
  return !manager->dataset(0)->dataBackedUp();
}



void VfguiRegulate::regulateBackup (SLpPush *backup1, SLpPush *backup2)
{
  assert(backup1 && backup2);
  assert(_manager->numEditableDatasets() > 1);

  backup1->setAtrap      (backup1_trap     , _manager);
  backup2->setAtrap      (backup2_trap     , _manager);
  backup1->setupSenseFun (backup1_sense_fun, _manager);
  backup2->setupSenseFun (backup2_sense_fun, _manager);
}



void VfguiRegulate::regulateBackup (SLpPush *backup3)
{
  assert(backup3);
  assert(_manager->numEditableDatasets() == 1);

  backup3->setAtrap      (backup3_trap     , _manager);
  backup3->setupSenseFun (backup3_sense_fun, _manager);
}



//--------------------------- compare -----------------------------//
//--------------------------- compare -----------------------------//
//--------------------------- compare -----------------------------//


static void answer_yes(void *data)
{
  VfManager *manager = (VfManager*)data;
  long index = manager->getActiveDatasetIndex();
  manager->removeComparisonDataset(index);
}


static void answer_no(void* /*data*/)
{
}


static void compare1_trap(void *data, long /*ident*/)
{
  VfManager *manager = (VfManager*)data;
  manager->appendNewComparisonDataset();
}


static void compare2_trap(void *data, long /*ident*/)
{
  VfguiRegulate *regulate = (VfguiRegulate*)data;
  VfManager     *manager  = regulate->manager();
  SLDelay       *parent   = regulate->parent();
  long numedit = manager->numEditableDatasets();
  long index   = manager->getActiveDatasetIndex();
  if(index >= numedit)
      {
      long nfun = manager->dataset(index)->numVelocityFunctions();
      if(nfun > 0)
          {
          new SLQuestPop(parent, "Question",
             "Are you sure you want to delete\nthis uneditable dataset?",
              FALSE, answer_yes, answer_no, manager);
          }
      else
          {
          manager->removeComparisonDataset(index);
          //////// equivalent: answer_yes(manager).
          }
      }
}



static long compare2_sense_fun(void *data)
{
  VfManager *manager = (VfManager*)data;
  long numedit = manager->numEditableDatasets();
  long index   = manager->getActiveDatasetIndex();
  return (index >= numedit);
}



void VfguiRegulate::regulateCompare (SLpPush *compare1, SLpPush *compare2)
{
  if(compare2) _parent = compare2->slParent();
  assert(_parent);
  if(compare1) compare1->setAtrap      (compare1_trap     , _manager);
  if(compare2) compare2->setAtrap      (compare2_trap     , this);
  if(compare2) compare2->setupSenseFun (compare2_sense_fun, _manager);
}



//--------------------------- lock ---------------------------------//
//--------------------------- lock ---------------------------------//
//--------------------------- lock ---------------------------------//


static void lock1_trap
               (void *data, long /*ident*/, long /*oldvar*/, long newvar)
{
  VfManager *manager = (VfManager*)data;
  VfDataset *dataset = manager->activeDataset();
  if(newvar) dataset->lockData();
  else       dataset->unlockData();
}


static void lock2_trap(void *data, long /*ident*/)
{
  VfManager *manager = (VfManager*)data;
  manager->lockAllDatasets();
}


static void lock3_trap(void *data, long /*ident*/)
{
  VfManager *manager = (VfManager*)data;
  manager->unlockAllDatasets();
}


static void lock4_trap
               (void *data, long /*ident*/, long /*oldvar*/, long newvar)
{
  VfManager *manager = (VfManager*)data;
  VfDataset *dataset = manager->dataset(0);
  if(newvar) dataset->lockData();
  else       dataset->unlockData();
}



static long lock1_fun(void *data)
{
  VfManager *manager = (VfManager*)data;
  VfDataset *dataset = manager->activeDataset();
  return dataset->isLocked();
}


static long lock4_fun(void *data)
{
  VfManager *manager = (VfManager*)data;
  VfDataset *dataset = manager->dataset(0);
  return dataset->isLocked();
}


static long lock1_sense_fun(void *data)
{
  VfManager *manager = (VfManager*)data;
  VfDataset *dataset = manager->activeDataset();
  return (dataset->isEditable());
}


static long lock2_sense_fun(void *data)
{
  VfManager *manager = (VfManager*)data;
  return (manager->numDatasetsLocked() < manager->numEditableDatasets());
}


static long lock3_sense_fun(void *data)
{
  VfManager *manager = (VfManager*)data;
  return (manager->numDatasetsLocked() > 0);
}



void VfguiRegulate::regulateLock
                        (SLpToggle *lock1, SLpPush *lock2, SLpPush *lock3)
{
  assert(lock1 && lock2 && lock3);
  assert(_manager->numEditableDatasets() > 1);
  lock1->setItrap      (lock1_trap     , _manager);
  lock2->setAtrap      (lock2_trap     , _manager);
  lock3->setAtrap      (lock3_trap     , _manager);
  lock1->setupIvarFun  (lock1_fun      , _manager);
  lock1->setupSenseFun (lock1_sense_fun, _manager);
  lock2->setupSenseFun (lock2_sense_fun, _manager);
  lock3->setupSenseFun (lock3_sense_fun, _manager);
}



void VfguiRegulate::regulateLock(SLpToggle *lock4)
{
  assert(lock4);
  assert(_manager->numEditableDatasets() == 1);
  lock4->setItrap     (lock4_trap     , _manager);
  lock4->setupIvarFun (lock4_fun      , _manager);
}



//-------------------------- select ------------------------------//
//-------------------------- select ------------------------------//
//-------------------------- select ------------------------------//


static void select1_trap
                 (void *data, long /*ident*/, long /*oldvar*/, long newvar)
{
  VfManager *manager = (VfManager*)data;
  VfDataset *dataset = manager->activeDataset();
  if(newvar) dataset->selectThisDataset();
  else       dataset->unselectThisDataset();
}


static void select2_trap(void *data, long /*ident*/)
{
  VfManager *manager = (VfManager*)data;
  manager->selectAllDatasets();
}


static void select3_trap(void *data, long /*ident*/)
{
  VfManager *manager = (VfManager*)data;
  manager->unselectAllDatasets();
}



static long select1_fun(void *data)
{
  VfManager *manager = (VfManager*)data;
  VfDataset *dataset = manager->activeDataset();
  return dataset->isSelected();
}


static long select2_sense_fun(void *data)
{
  VfManager *manager = (VfManager*)data;
  return (manager->numDatasetsSelected() < manager->numDatasets());
}


static long select3_sense_fun(void *data)
{
  VfManager *manager = (VfManager*)data;
  return (manager->numDatasetsSelected() > 0);
}



void VfguiRegulate::regulateSelect
                      (SLpToggle *select1, SLpPush *select2, SLpPush *select3)
{
  assert(select1 && select2 && select3);
  select1->setItrap      (select1_trap     , _manager);
  select2->setAtrap      (select2_trap     , _manager);
  select3->setAtrap      (select3_trap     , _manager);
  select1->setupIvarFun  (select1_fun      , _manager);
  select2->setupSenseFun (select2_sense_fun, _manager);
  select3->setupSenseFun (select3_sense_fun, _manager);
}



//--------------------------- inform ------------------------------//
//--------------------------- inform ------------------------------//
//--------------------------- inform ------------------------------//


static void inform1_trap
                 (void *data, long /*ident*/, long /*oldvar*/, long newvar)
{
  VfInform *inform = (VfInform*)data;
  if(newvar) inform->enableMessages();
  else       inform->disableMessages();
}


static long inform1_fun(void *data)
{
  VfInform *inform = (VfInform*)data;
  return (inform->messagesAreEnabled());
}



void VfguiRegulate::regulateInform(SLpToggle *inform1)
{
  assert(inform1);
  assert(!_inform);
  _inform = new VfInform (_manager, TRUE);
  _inform->disableMessages();
  inform1->setItrap      (inform1_trap, _inform);
  inform1->setupIvarFun  (inform1_fun , _inform);
}



//---------------------------- end --------------------------------------//
//---------------------------- end --------------------------------------//
//---------------------------- end --------------------------------------//

