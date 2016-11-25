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

//---------------------- statgui_status.cc ------------------------//
//---------------------- statgui_status.cc ------------------------//
//---------------------- statgui_status.cc ------------------------//

//          implementation file for the StatguiStatus class
//                derived from the SLSmartForm class
//                derived from the StaticInform class
//                        subdirectory statgui


#include "statgui/statgui_status.hh"
#include "stat/static_manager.hh"
#include "stat/static_dataset.hh"
#include "sl/slp_text.hh"
#include "sl/slp_label.hh"
#include "sl/sl2_arrows.hh"
#include "sl/container_list.hh"
#include "sl/sl_poplist_button.hh"
#include <stdlib.h>
#include <stdio.h>
#include <iostream.h>
#include <string.h>
#include <assert.h>



//------------------------ static functions -------------------------//
//------------------------ static functions -------------------------//
//------------------------ static functions -------------------------//


static void arrows_num1_trap(void *data, long newvar)
{
  StaticManager *manager = (StaticManager*)data;
  manager->setActiveDatasetIndex((int)newvar - 1);
}


static long arrows_num1_update(void *data)
{
  StaticManager *manager = (StaticManager*)data;
  return manager->getActiveDatasetIndex() + 1;
}


static long arrows_num2_update(void *data)
{
  StaticManager *manager = (StaticManager*)data;
  return manager->numDatasets();
}


static long arrows_sense_update(void *data)
{
  StaticManager *manager = (StaticManager*)data;
  return (manager->numDatasets() > 1);
}


static char *info_update(void *data)
{
  StaticManager *manager = (StaticManager*)data;
  static char buffer[60];
  int nx = manager->activeDataset()->getNx();
  int ny = manager->activeDataset()->getNy();
  int nn = manager->activeDataset()->numNilValues();
/*
  int ns = manager->activeDataset()->numSelections();
  sprintf(buffer, "#x %d  #y %d  #nil %d  #sel %d", nx, ny, nn, ns);
  sprintf(buffer, "%dx %dy %dnil %dsel", nx, ny, nn, ns);
*/
  sprintf(buffer, "nx %d  ny %d  %d nils", nx, ny, nn);
  return buffer;
}



//--------------------- constructors -------------------------------//
//--------------------- constructors -------------------------------//
//--------------------- constructors -------------------------------//


StatguiStatus::StatguiStatus  (  SLDelay       *slparent,
                                 StaticManager *manager,
                                 ContainerList *clist,
                                 int            wide   )
       : SLSmartForm(slparent, "statgui_status", NULL, FALSE, FALSE),
         StaticInform(manager),
                 _clear_flag  (FALSE),
                 _msg         (NULL),
                 _status2     (NULL),
                 _status3     (NULL),
                 _status4     (NULL),
                 _status5     (NULL)
{
  constructorHelper(clist, wide);
  if(slparent->made()) make();
}



StatguiStatus::StatguiStatus  (  Widget         wparent,
                                 StaticManager *manager,
                                 ContainerList *clist,
                                 int            wide  )
       : SLSmartForm(wparent, "statgui_status", NULL, FALSE, FALSE),
         StaticInform(manager),
                 _msg         (NULL),
                 _clear_flag  (FALSE),
                 _status2     (NULL),
                 _status3     (NULL),
                 _status4     (NULL),
                 _status5     (NULL)
{
  constructorHelper(clist, wide);
  make();
}



//----------------------- destructor ---------------------------//
//----------------------- destructor ---------------------------//
//----------------------- destructor ---------------------------//


StatguiStatus::~StatguiStatus()
{
}



//----------------------- static functions ------------------------//
//----------------------- static functions ------------------------//
//----------------------- static functions ------------------------//

#define LEN2   6
#define LEN3  28
#define LEN4   8
#define LEN5   9

static char *status2_upfun(void *data)
{
  StaticManager *manager = (StaticManager*)data;
  StaticDataset *dataset = manager->activeDataset();
  static char *buffer1 = "LOCKED";
  static char *buffer3 = " ";
  if(dataset->isLocked()) return buffer1;
  return buffer3;
}


static char *status3_upfun(void *data)
{
  StaticManager *manager = (StaticManager*)data;
  StaticDataset *dataset = manager->activeDataset();
  static char *buffer1 = "NEEDS SAVING (backed up)";
  static char *buffer2 = "NEEDS SAVING (not backed up)";
  static char *buffer3 = " ";
  if(dataset->dataNeedsSaving())
      {
      if(dataset->dataBackedUp()) return buffer1;
      else                        return buffer2;
      }
  return buffer3;
}


static char *status4_upfun(void *data)
{
  StaticManager *manager = (StaticManager*)data;
  StaticDataset *dataset = manager->activeDataset();
  static char *buffer1 = "selected";
  static char *buffer3 = " ";
  if(dataset->isSelected()) return buffer1;
  return buffer3;
}


static char *status5_upfun(void *data)
{
  StaticManager *manager = (StaticManager*)data;
  StaticDataset *dataset = manager->activeDataset();
  static char *buffer1 = "reference";
  static char *buffer3 = " ";
  if(dataset->isReference()) return buffer1;
  return buffer3;
}



//--------------------- fallback resources --------------------//
//--------------------- fallback resources --------------------//
//--------------------- fallback resources --------------------//


static const char *defres[] = {
/*
        "*mes1.fontList:       8x13bold",
        "*mes2.fontList:       fixed",
        "*mes3.fontList:       fixed",
*/
        "*mes4.fontList:       fixed",
        "*mes5.fontList:       fixed",
        "*mes1.foreground:     brown",
        "*mes2.foreground:     green4",
        "*mes3.foreground:     red",
        "*mes4.foreground:     purple",
        "*mes5.foreground:     blue",
        ".borderWidth:         2",
/*
        "*background:          gray76",
*/
        "*background:          wheat",
        "*goto*background:     green3",
            NULL };



//------------------- constructor helper ----------------------//
//------------------- constructor helper ----------------------//
//------------------- constructor helper ----------------------//

        // private.

void StatguiStatus::constructorHelper(ContainerList *clist, int wide)
{
  assert(clist);
  setFallbackResources(defres);

  SL2Arrows  *arrows  = new SL2Arrows(this, "arrows", "active dataset", 2);
  SLpLabel   *info    = new SLpLabel (this, "info");

  _msg     = new SLpText  (this, "mes1");
  _status2 = new SLpText  (this, "mes2", 0, SLpText::_CHAR, LEN2 + 2);
  _status3 = new SLpText  (this, "mes3", 0, SLpText::_CHAR, LEN3 + 2);
  _status4 = new SLpText  (this, "mes4", 0, SLpText::_CHAR, LEN4 + 2);
  _status5 = new SLpText  (this, "mes5", 0, SLpText::_CHAR, LEN5 + 2);

   arrows ->registerNum1Trap   (arrows_num1_trap   , manager());
   arrows ->registerNum1Update (arrows_num1_update , manager());
   arrows ->registerNum2Update (arrows_num2_update , manager());
   arrows ->registerSenseUpdate(arrows_sense_update, manager());
   info   ->setupCvarFun       (info_update        , manager());
  _msg    ->flushWhenValueChanges();
  _msg    ->showLabelAppearance();
  _status2->showLabelAppearance();
  _status3->showLabelAppearance();
  _status4->showLabelAppearance();
  _status5->showLabelAppearance();
  _status2->setupCvarFun  (status2_upfun, manager());
  _status3->setupCvarFun  (status3_upfun, manager());
  _status4->setupCvarFun  (status4_upfun, manager());
  _status5->setupCvarFun  (status5_upfun, manager());

  SLPopListButton *go_to = new SLPopListButton(this, "goto", clist);

///                     left     right     top       bottom
  if(wide)
      {
      attach(_msg    ,  this   ,  this   ,  this   ,  NULL);
      attach( arrows ,  this   ,  NULL   , _msg    ,  NULL, 6, 0, 0, 0);
      attach( info   ,  arrows ,  go_to  , _msg    ,  NULL, 6, 6, 0, 0);
      attach(_status2,  this   ,  NULL   ,  arrows ,  this);
      attach(_status4, _status2,  NULL   ,  arrows ,  this);
      attach(_status5, _status4,  NULL   ,  arrows ,  this);
      attach(_status3, _status5,  go_to  ,  arrows ,  this);
      attach( go_to  ,  NULL   ,  this   , _msg    ,  this, 0, 6, 0, 6);
      }
  else
      {
      attach(_msg    ,  this   ,  this   ,  this   ,  NULL);
      attach( arrows ,  this   ,  NULL   , _msg    ,  NULL, 6, 6, 0, 0);
      attach( info   ,  this   ,  this   ,  arrows ,  NULL, 6, 6, 0, 0);
      attach(_status2,  this   ,  NULL   ,  NULL   , _status3);
      attach(_status4, _status2,  NULL   ,  NULL   , _status3);
      attach(_status5, _status4,  go_to  ,  NULL   , _status3);
      attach(_status3,  this   ,  this   ,  NULL   ,  this);
      attach( go_to  ,  NULL   ,  this   ,  info   , _status3, 0, 6, 6, 0);
      }
}



//--------------- overriding virtual functions -------------------//
//--------------- overriding virtual functions -------------------//
//--------------- overriding virtual functions -------------------//

      // protected virtual functions overriding StaticInform.

void StatguiStatus::showMessage(const char *msg)
{
  const char *old = _msg->cvar();
  if(old == NULL || strcmp(msg, (char*)old) == 0) _msg->setCvar(" ");
  _msg->setCvar((char*)msg);
  _clear_flag = FALSE;
}



void StatguiStatus::returningToEventLoop()
{
  if(_clear_flag) _msg->setCvar(" ");
  _clear_flag = TRUE;
}



//---------------------------- end --------------------------------------//
//---------------------------- end --------------------------------------//
//---------------------------- end --------------------------------------//

