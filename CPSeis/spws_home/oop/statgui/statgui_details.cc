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

//---------------------- statgui_details.cc ----------------------//
//---------------------- statgui_details.cc ----------------------//
//---------------------- statgui_details.cc ----------------------//

//          implementation file for the StatguiDetails class
//                 derived from the SLSmartForm class
//                       subdirectory statgui


#include "statgui/statgui_details.hh"
#include "stat/static_manager.hh"
#include "stat/static_dataset.hh"
#include "sl/sl_smart_form.hh"
#include "sl/sl_row.hh"
#include "sl/sl_quest_pop.hh"
#include "sl/slp_push.hh"
#include "sl/slp_toggle.hh"
#include "sl/slp_label.hh"
#include "sl/slp_text.hh"
#include "named_constants.h"
#include <math.h>
#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <assert.h>



//-------------------- get active data object ------------------------//
//-------------------- get active data object ------------------------//
//-------------------- get active data object ------------------------//

      // public.

StaticDataset *StatguiDetails::activeDataset()  const
{
  return manager()->activeDataset();
}



//-------------------------- cancel undo ------------------------//
//-------------------------- cancel undo ------------------------//
//-------------------------- cancel undo ------------------------//

      // public.
      // call this function when unmanaging the dialog box.

void StatguiDetails::cancelUndo()
{
  manager()->maybeDeleteUndoFiles(_v_doer);
}



//------------------------ static traps ----------------------------//
//------------------------ static traps ----------------------------//
//------------------------ static traps ----------------------------//


static void lock_trap(void *data, long /*ident*/,
                 long /*oldvar*/, long newvar)
{
  StatguiDetails    *gui = (StatguiDetails*)data;
  StaticDataset *dataset = gui->activeDataset();
  if(newvar != 0) dataset->lockData();
  else            dataset->unlockData();
}


static void sel_trap(void *data, long /*ident*/,
                 long /*oldvar*/, long newvar)
{
  StatguiDetails    *gui = (StatguiDetails*)data;
  StaticDataset *dataset = gui->activeDataset();
  if(newvar != 0) dataset->selectThisDataset();
  else            dataset->unselectThisDataset();
}



static void yes_clearv_trap(void *data)
{
  StatguiDetails    *gui = (StatguiDetails*)data;
  StaticDataset *dataset = gui->activeDataset();
  void           *v_doer = gui->getVdoer();
  dataset->clearStaticValues(v_doer);
}


static void no_clearv_trap(void* /*data*/)
{
}


static void clearv_trap(void *data, long /*ident*/)
{
  StatguiDetails    *gui = (StatguiDetails*)data;
  StaticDataset *dataset = gui->activeDataset();
  char msg[500];
  msg[0] = '\0';
  if(dataset->dataNeedsSaving())
      {
      strcat(msg, "You have data which\nhas NOT been saved.\n----\n");
      }
  strcat(msg, "Are you sure you want\nto set all data values to nil?");
  new SLQuestPop(gui, "Question", msg, FALSE,
                            yes_clearv_trap, no_clearv_trap, gui);
}



static void clears_trap(void *data, long /*ident*/)
{
  StatguiDetails    *gui = (StatguiDetails*)data;
  StaticDataset *dataset = gui->activeDataset();
  if(dataset->isSelected(0, 0)) dataset->clearSelections();
  else dataset->setSelections(0, 0, dataset->getNx(), dataset->getNy(), TRUE);
}


static void backup_trap(void *data, long /*ident*/)
{
  StatguiDetails    *gui = (StatguiDetails*)data;
  StaticDataset *dataset = gui->activeDataset();
  dataset->saveBackupFile();
}



static void v_undo_trap(void *data, long /*ident*/)
{
  StatguiDetails    *gui = (StatguiDetails*)data;
  StaticDataset *dataset = gui->activeDataset();
  void           *v_doer = gui->getVdoer();
  dataset->maybeReadUndoFile(v_doer);
}



//--------------------- static update functions ----------------//
//--------------------- static update functions ----------------//
//--------------------- static update functions ----------------//


static long lock_upfun(void *data)
{
  StatguiDetails    *gui = (StatguiDetails*)data;
  StaticDataset *dataset = gui->activeDataset();
  return dataset->isLocked();
}


static long sel_upfun(void *data)
{
  StatguiDetails    *gui = (StatguiDetails*)data;
  StaticDataset *dataset = gui->activeDataset();
  return dataset->isSelected();
}


static char *select_upfun(void *data)
{
  static char buffer[30];
  StatguiDetails    *gui = (StatguiDetails*)data;
  StaticDataset *dataset = gui->activeDataset();
  int n = dataset->numSelections();
  sprintf(buffer, "(%d selected locations)", n);
  return buffer;
}


static char *nil_upfun(void *data)
{
  static char buffer[30];
  StatguiDetails    *gui = (StatguiDetails*)data;
  StaticDataset *dataset = gui->activeDataset();
  int nil  = dataset->numNilValues();
  int live = dataset->numLiveValues();
  sprintf(buffer, "(%d nil  %d live)", nil, live);
  return buffer;
}


static char *range_upfun(void *data)
{
  static char buffer[30];
  StatguiDetails    *gui = (StatguiDetails*)data;
  StaticDataset *dataset = gui->activeDataset();
  float vmin = dataset->minimumValue();
  float vmax = dataset->maximumValue();
  float vav  = dataset->averageValue();
  int   live = dataset->numLiveValues();
  if(live == 0)
      {
      sprintf(buffer, "(all values are nil)");
      }
  else
      {
      int ivmin = NearestInteger(vmin);
      int ivmax = NearestInteger(vmax);
      int ivav  = NearestInteger(vav);
      sprintf(buffer, "(min %d  max %d  av %d)", ivmin, ivmax, ivav);
      }
  return buffer;
}



static char *lastread_upfun(void *data)
{
  StatguiDetails    *gui = (StatguiDetails*)data;
  StaticDataset *dataset = gui->activeDataset();
  return (char*)dataset->lastFileRead();
}


static char *lastsaved_upfun(void *data)
{
  StatguiDetails    *gui = (StatguiDetails*)data;
  StaticDataset *dataset = gui->activeDataset();
  return (char*)dataset->lastFileSaved();
}


static char *lastbackup_upfun(void *data)
{
  StatguiDetails    *gui = (StatguiDetails*)data;
  StaticDataset *dataset = gui->activeDataset();
  return (char*)dataset->lastBackupFileSaved();
}



//----------------- static sense update functions ----------------//
//----------------- static sense update functions ----------------//
//----------------- static sense update functions ----------------//


static long clearv_sense_upfun(void *data)
{
  StatguiDetails    *gui = (StatguiDetails*)data;
  StaticDataset *dataset = gui->activeDataset();
  if(dataset->isLocked()) return FALSE;
  if(dataset->numLiveValues() == 0) return FALSE;
  return TRUE;
}


/*
static long clears_sense_upfun(void *data)
{
  StatguiDetails    *gui = (StatguiDetails*)data;
  StaticDataset *dataset = gui->activeDataset();
  if(dataset->numSelections() == 0) return FALSE;
  return TRUE;
}
*/


static long backup_sense_upfun(void *data)
{
  StatguiDetails    *gui = (StatguiDetails*)data;
  StaticDataset *dataset = gui->activeDataset();
  if(dataset->dataBackedUp()) return FALSE;
  return TRUE;
}


static long v_undo_sense_upfun(void *data)
{
  StatguiDetails    *gui = (StatguiDetails*)data;
  StaticDataset *dataset = gui->activeDataset();
  void      *v_doer = gui->getVdoer();
  return (dataset->allowReadDeleteUndoFile(v_doer) &&
            !dataset->isLocked());
}



//--------------------- fallback resources --------------------//
//--------------------- fallback resources --------------------//
//--------------------- fallback resources --------------------//


static const char *defres[] = {
        ".borderWidth:           2",
        "*background:            gray80",
        "*Question*background:   goldenrod",
            NULL };



//--------------------- constructor ------------------------//
//--------------------- constructor ------------------------//
//--------------------- constructor ------------------------//


StatguiDetails::StatguiDetails(SLDelay *slparent, StaticManager *manager)
            : SLSmartForm(slparent, "statgui_details"),
                        _manager      (manager),
                        _v_doer       (NULL)
{
  assert(_manager);
  setFallbackResources(defres);

  SLSmartForm *s1 = new SLSmartForm (this, "s1");
  SLSmartForm *s9 = new SLSmartForm (this, "s9");
  SLSmartForm *s3 = new SLSmartForm (this, "s3");
  SLSmartForm *s4 = new SLSmartForm (this, "s4");
  SLSmartForm *s5 = new SLSmartForm (this, "s5");
  SLRow       *r3 = new SLRow       (s3  , "r3");

  s1->showEvenSpacing();
  s9->showEvenSpacing();
  s3->showEvenSpacing();
  s4->showEvenSpacing();
  s5->showEvenSpacing();

  SLpToggle *lock    = new SLpToggle (s1  , "Lock Changes To Data");
  SLpToggle *sel     = new SLpToggle (s9  , "Select This Dataset");
  SLpPush   *clearv  = new SLpPush   (r3  , "r3", 0,
      "Set All Static Values to Nil\n(allows resetting #bins)");
  SLpPush   *clears  = new SLpPush   (s4  , "Select or Unselect All Locations");
  SLpLabel  *select  = new SLpLabel  (this, "select");
  SLpLabel  *nil     = new SLpLabel  (this, "nil");
  SLpLabel  *range   = new SLpLabel  (this, "range");
  SLpPush   *backup  = new SLpPush   (s5  , "Save Backup File");
  SLpPush   *v_undo   = new SLpPush  (r3  , "Undo");

  _v_doer = clearv;

  lock  ->setItrap       (lock_trap        ,  this);
  sel   ->setItrap       (sel_trap         ,  this);
  clearv->setAtrap       (clearv_trap      ,  this);
  clears->setAtrap       (clears_trap      ,  this);
  backup->setAtrap       (backup_trap      ,  this);
  v_undo->setAtrap       (v_undo_trap      ,  this);

  lock  ->setupIvarFun   (lock_upfun        ,  this);
  sel   ->setupIvarFun   (sel_upfun         ,  this);
  select->setupCvarFun   (select_upfun      ,  this);
  nil   ->setupCvarFun   (nil_upfun         ,  this);
  range ->setupCvarFun   (range_upfun       ,  this);
  clearv->setupSenseFun  (clearv_sense_upfun,  this);
/*
  clears->setupSenseFun  (clears_sense_upfun,  this);
*/
  backup->setupSenseFun  (backup_sense_upfun,  this);
  v_undo->setupSenseFun  (v_undo_sense_upfun,  this);

  SLpText *lastread_prompt   = new SLpText  (this, "lastread_prompt");
  SLpText *lastsaved_prompt  = new SLpText  (this, "lastsaved_prompt");
  SLpText *lastbackup_prompt = new SLpText  (this, "lastbackup_prompt");
  SLpText *lastread          = new SLpText  (this, "lastread");
  SLpText *lastsaved         = new SLpText  (this, "lastsaved");
  SLpText *lastbackup        = new SLpText  (this, "lastbackup");

  lastread_prompt  ->showLabelAppearance();
  lastsaved_prompt ->showLabelAppearance();
  lastbackup_prompt->showLabelAppearance();
  lastread         ->showFramedLabelAppearance();
  lastsaved        ->showFramedLabelAppearance();
  lastbackup       ->showFramedLabelAppearance();

  lastread_prompt  ->setCvar        ("Last file read:");
  lastsaved_prompt ->setCvar        ("Last file saved:");
  lastbackup_prompt->setCvar        ("Last backup file saved:");
  lastread         ->setupCvarFun   (lastread_upfun    ,  this);
  lastsaved        ->setupCvarFun   (lastsaved_upfun   ,  this);
  lastbackup       ->setupCvarFun   (lastbackup_upfun  ,  this);

         /////       LEFT     RIGHT    TOP      BOTTOM

  attach(s1        , this  ,  this   , this   ,  NULL ,  0, 0,10);
  attach(s9        , this  ,  this   , s1     ,  NULL ,  0, 0, 0);
  attach(s3        , this  ,  this   , s9     ,  NULL ,  0, 0,10);
  attach(nil       , this  ,  this   , s3     ,  NULL ,  0, 0, 0);
  attach(range     , this  ,  this   , nil    ,  NULL ,  0, 0, 0);
  attach(s4        , this  ,  this   , range  ,  NULL ,  0, 0,10);
  attach(select    , this  ,  this   , s4     ,  NULL ,  0, 0, 0);
  attach(s5        , this  ,  this   , select ,  NULL ,  0, 0,10);
  attach(lastread_prompt  , this,  NULL, s5               , NULL, 0,0,10,0);
  attach(lastread         , this,  this, lastread_prompt  , NULL, 20,8);
  attach(lastsaved_prompt , this,  NULL, lastread         , NULL);
  attach(lastsaved        , this,  this, lastsaved_prompt , NULL, 20,8);
  attach(lastbackup_prompt, this,  NULL, lastsaved        , NULL);
  attach(lastbackup       , this,  this, lastbackup_prompt, this, 20,8,0,10);

  update();
}



//------------------------- destructor -------------------------//
//------------------------- destructor -------------------------//
//------------------------- destructor -------------------------//


StatguiDetails::~StatguiDetails()
{
}



//-------------------------- end -------------------------------//
//-------------------------- end -------------------------------//
//-------------------------- end -------------------------------//
