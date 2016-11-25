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

//---------------------- regulate_statgui.cc ------------------------//
//---------------------- regulate_statgui.cc ------------------------//
//---------------------- regulate_statgui.cc ------------------------//

//         implementation file for the RegulateStatgui class
//                   not derived from any class
//                       subdirectory statgui


#include "statgui/regulate_statgui.hh"
#include "stat/static_manager.hh"
#include "stat/static_inform.hh"
#include "stat/static_dataset.hh"
#include "sl/slp_push.hh"
#include "sl/slp_toggle.hh"
#include "sl/sl_quest_pop.hh"
#include <stdlib.h>
#include <stdio.h>
#include <iostream.h>
#include <string.h>
#include <assert.h>



//--------------------- constructor and destructor --------------------//
//--------------------- constructor and destructor --------------------//
//--------------------- constructor and destructor --------------------//


RegulateStatgui::RegulateStatgui (StaticManager *manager)
       :
               _manager     (manager),
               _parent      (NULL),
               _inform      (NULL)
{
  assert(_manager);
}



RegulateStatgui::~RegulateStatgui()
{
  if(_inform) delete _inform;
}



//--------------------------- backup -----------------------------//
//--------------------------- backup -----------------------------//
//--------------------------- backup -----------------------------//


static void backup1_trap(void *data, long /*ident*/)
{
  StaticManager *manager = (StaticManager*)data;
  manager->activeDataset()->saveBackupFile();
}


static void backup2_trap(void *data, long /*ident*/)
{
  StaticManager *manager = (StaticManager*)data;
  manager->saveBackupFiles();
}


static long backup1_sense_fun(void *data)
{
  StaticManager *manager = (StaticManager*)data;
  return !manager->activeDataset()->dataBackedUp();
}


static long backup2_sense_fun(void *data)
{
  StaticManager *manager = (StaticManager*)data;
  return (manager->numDatasetsBackedUp() < manager->numDatasets());
}


void RegulateStatgui::regulateBackup (SLpPush *backup1, SLpPush *backup2)
{
  assert(backup1 && backup2);
  backup1->setAtrap      (backup1_trap     , _manager);
  backup2->setAtrap      (backup2_trap     , _manager);
  backup1->setupSenseFun (backup1_sense_fun, _manager);
  backup2->setupSenseFun (backup2_sense_fun, _manager);
}



//--------------------------- compare -----------------------------//
//--------------------------- compare -----------------------------//
//--------------------------- compare -----------------------------//


static void answer_yes(void *data)
{
  StaticManager *manager = (StaticManager*)data;
  long index = manager->getActiveDatasetIndex();
  manager->removeDataset(index);
}


static void answer_no(void* /*data*/)
{
}


static void compare1_trap(void *data, long /*ident*/)
{
  StaticManager *manager = (StaticManager*)data;
  manager->appendNewDataset();
}


static void compare2_trap(void *data, long /*ident*/)
{
  StaticManager *manager = (StaticManager*)data;
  manager->duplicateActiveDataset();
/*
  StaticDataset *dataset = manager->appendNewDataset();
  dataset->copyData(NULL, manager->activeDataset());
*/
}


static void compare3_trap(void *data, long /*ident*/)
{
  RegulateStatgui *regulate = (RegulateStatgui*)data;
  StaticManager   *manager  = regulate->manager();
  SLDelay         *parent   = regulate->parent();
  long index  = manager->getActiveDatasetIndex();
  if(index >= 0)
      {
      if(manager->activeDataset()->numLiveValues() > 0)
          {
          new SLQuestPop(parent, "Question",
             "This dataset contains\nlive static values.\n------\n\
Are you sure\nyou want to delete it?",
              FALSE, answer_yes, answer_no, manager);
          }
      else
          {
          manager->removeDataset(index);
          //////// equivalent: answer_yes(manager).
          }
      }
}



static long compare3_sense_fun(void *data)
{
  StaticManager *manager = (StaticManager*)data;
  return (manager->numDatasets() >= 2);
}



void RegulateStatgui::regulateCompare
                      (SLpPush *compare1, SLpPush *compare2, SLpPush *compare3)
{
  assert(compare1 && compare2 && compare3);
  _parent = compare3->slParent();
  assert(_parent);
  compare1->setAtrap      (compare1_trap     , _manager);
  compare2->setAtrap      (compare2_trap     , _manager);
  compare3->setAtrap      (compare3_trap     , this);
  compare3->setupSenseFun (compare3_sense_fun, _manager);
}



//--------------------------- lock ---------------------------------//
//--------------------------- lock ---------------------------------//
//--------------------------- lock ---------------------------------//


static void lock1_trap
               (void *data, long /*ident*/, long /*oldvar*/, long newvar)
{
  StaticManager *manager = (StaticManager*)data;
  StaticDataset *dataset = manager->activeDataset();
  if(newvar) dataset->lockData();
  else       dataset->unlockData();
}


static void lock2_trap(void *data, long /*ident*/)
{
  StaticManager *manager = (StaticManager*)data;
  manager->lockAllDatasets();
}


static void lock3_trap(void *data, long /*ident*/)
{
  StaticManager *manager = (StaticManager*)data;
  manager->unlockAllDatasets();
}



static long lock1_fun(void *data)
{
  StaticManager *manager = (StaticManager*)data;
  StaticDataset *dataset = manager->activeDataset();
  return dataset->isLocked();
}



static long lock1_sense_fun(void *data)
{
  StaticManager *manager = (StaticManager*)data;
  return (manager->numDatasets() > 0);
}


static long lock2_sense_fun(void *data)
{
  StaticManager *manager = (StaticManager*)data;
  return (manager->numDatasetsLocked() < manager->numDatasets());
}


static long lock3_sense_fun(void *data)
{
  StaticManager *manager = (StaticManager*)data;
  return (manager->numDatasetsLocked() > 0);
}



void RegulateStatgui::regulateLock
                        (SLpToggle *lock1, SLpPush *lock2, SLpPush *lock3)
{
  assert(lock1 && lock2 && lock3);
  lock1->setItrap      (lock1_trap     , _manager);
  lock2->setAtrap      (lock2_trap     , _manager);
  lock3->setAtrap      (lock3_trap     , _manager);
  lock1->setupIvarFun  (lock1_fun      , _manager);
  lock1->setupSenseFun (lock1_sense_fun, _manager);
  lock2->setupSenseFun (lock2_sense_fun, _manager);
  lock3->setupSenseFun (lock3_sense_fun, _manager);
}



//-------------------------- select ------------------------------//
//-------------------------- select ------------------------------//
//-------------------------- select ------------------------------//


static void select1_trap
                 (void *data, long /*ident*/, long /*oldvar*/, long newvar)
{
  StaticManager *manager = (StaticManager*)data;
  StaticDataset *dataset = manager->activeDataset();
  if(newvar) dataset->selectThisDataset();
  else       dataset->unselectThisDataset();
}


static void select2_trap(void *data, long /*ident*/)
{
  StaticManager *manager = (StaticManager*)data;
  manager->selectAllDatasets();
}


static void select3_trap(void *data, long /*ident*/)
{
  StaticManager *manager = (StaticManager*)data;
  manager->unselectAllDatasets();
}



static long select1_fun(void *data)
{
  StaticManager *manager = (StaticManager*)data;
  return manager->activeDataset()->isSelected();
}


static long select1_sense_fun(void *data)
{
  StaticManager *manager = (StaticManager*)data;
  return (manager->numDatasets() > 0);
}


static long select2_sense_fun(void *data)
{
  StaticManager *manager = (StaticManager*)data;
  return (manager->numDatasetsSelected() < manager->numDatasets());
}


static long select3_sense_fun(void *data)
{
  StaticManager *manager = (StaticManager*)data;
  return (manager->numDatasetsSelected() > 0);
}



void RegulateStatgui::regulateSelect
                      (SLpToggle *select1, SLpPush *select2, SLpPush *select3)
{
  assert(select1 && select2 && select3);
  select1->setItrap      (select1_trap     , _manager);
  select2->setAtrap      (select2_trap     , _manager);
  select3->setAtrap      (select3_trap     , _manager);
  select1->setupIvarFun  (select1_fun      , _manager);
  select1->setupSenseFun (select1_sense_fun, _manager);
  select2->setupSenseFun (select2_sense_fun, _manager);
  select3->setupSenseFun (select3_sense_fun, _manager);
}



//--------------------------- inform ------------------------------//
//--------------------------- inform ------------------------------//
//--------------------------- inform ------------------------------//


static void inform1_trap
                 (void *data, long /*ident*/, long /*oldvar*/, long newvar)
{
  StaticInform *inform = (StaticInform*)data;
  if(newvar) inform->enableMessages();
  else       inform->disableMessages();
}


static long inform1_fun(void *data)
{
  StaticInform *inform = (StaticInform*)data;
  return (inform->messagesAreEnabled());
}



void RegulateStatgui::regulateInform(SLpToggle *inform1)
{
  assert(inform1);
  assert(!_inform);
  _inform = new StaticInform (_manager, TRUE);
  _inform->disableMessages();
  inform1->setItrap      (inform1_trap, _inform);
  inform1->setupIvarFun  (inform1_fun , _inform);
}



//---------------------------- end --------------------------------------//
//---------------------------- end --------------------------------------//
//---------------------------- end --------------------------------------//

