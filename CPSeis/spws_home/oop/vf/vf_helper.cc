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

//-------------------------- vf_helper.cc ----------------------------//
//-------------------------- vf_helper.cc ----------------------------//
//-------------------------- vf_helper.cc ----------------------------//

//            implementation file for the VfHelper class
//                    not derived from any class
//                         subdirectory vf


#include "vf/vf_helper.hh"
#include "vf/vf_kernal.hh"
#include "vf/vf_informer.hh"
#include "vf/vf_function.hh"
#include "vf/vf_edit_base.hh"
#include "vf/vf_backup.hh"
#include "vf/vf_undo.hh"
#include "oprim/history_cards.hh"
#include "named_constants.h"
#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <assert.h>
#include <limits.h>
#include <math.h>


      // The _dataset pointer is needed only for calling _informer
      // functions.  The VfDataset header file is not needed.

      // The _kernal pointer is needed only for making these calls:
      //   long nfun   = _kernal->numVelocityFunctions();
      //   long npicks = _kernal->velfun(ifun)->numPicks();
      //   int  type   = _kernal->velfun(ifun)->getDefaultType();
      // Likewise for the VfKernal and VfFunction header files.

      // The VfEditBase header file is needed only for making this call:
      //   edit->workingMessage();
      // and for accessing enums.


#define TURN_ON        turnOnDataNeedsSavingFlag  ();
#define TURN_OFF       turnOffDataNeedsSavingFlag ();
#define BACKUP         saveBackupFile             ();
#define DOER(doer)     saveUndoFile               (doer);
#define MESSAGE(msg)   broadcastMessage           (msg);
#define BELL(error)    broadcastBell              (error);
#define HISTORY(card)  broadcastHistory           (card);


//-------------------------- constructor ----------------------------//
//-------------------------- constructor ----------------------------//
//-------------------------- constructor ----------------------------//


VfHelper::VfHelper(int editable, VfInformer *informer,
                   VfDataset *dataset, VfKernal *kernal,
                   const char *progname)
           :
        _informer       (informer),
        _dataset        (dataset),
        _kernal         (kernal),
        _backup         (NULL),
        _undo           (NULL),
        _history        (NULL),
        _needs_saving   (FALSE),
        _backed_up      (TRUE),
        _locked         (FALSE),
        _editable       (editable)
{
  assert(_informer);
  assert(_dataset);
  assert(_kernal);
  assert(EDIT_BUT_SUPPRESS_BACKUPS != TRUE);
  _history = new HistoryCards (500, progname);
  _backup  = new VfBackup     (_kernal, _history);
  _undo    = new VfUndo       (_kernal, _history);
}



//--------------------------- destructor --------------------------//
//--------------------------- destructor --------------------------//
//--------------------------- destructor --------------------------//


VfHelper::~VfHelper()
{
  BACKUP
  delete _backup;
  delete _undo;
  delete _history;
}


 
//--------------------- lock and unlock data ---------------------------//
//--------------------- lock and unlock data ---------------------------//
//--------------------- lock and unlock data ---------------------------//

       // public.

void VfHelper::lockData()
{
  if(!_editable) return;
  if(_locked) return;
  _informer->preChangeDataLock(_dataset);
  _locked = TRUE;
  _informer->postChangeDataLock(_dataset);
}


void VfHelper::unlockData()
{
  if(!_editable) return;
  if(!_locked) return;
  _informer->preChangeDataLock(_dataset);
  _locked = FALSE;
  _informer->postChangeDataLock(_dataset);
}



//--------------- data changes are forbidden -------------------------//
//--------------- data changes are forbidden -------------------------//
//--------------- data changes are forbidden -------------------------//

   // public.
   // to be called first thing if any changes to the data are requested.
   // should NOT be called if no changes are being made to the data.
   // should NOT be called if you are simply setting an "active" item
   //   or a "reference" item or selecting/unselecting items or resetting
   //   the data lock.
   // returns error = TRUE  if data changes are forbidden.
   // returns error = FALSE if data changes are allowed.

   // changes are forbidden on uneditable datasets (which are never locked).
   // changes are forbidden on locked datasets (which are always editable).

int VfHelper::dataChangesAreForbidden()
{
  if(!_editable)
      {
      _informer->showMessage("this dataset is not editable");
      _informer->ringBell();
      return TRUE;
      }
  else if(_locked)
      {
      _informer->showMessage("attempt to change data when locked");
      _informer->ringBell();
      return TRUE;
      }
  return FALSE;
}


int VfHelper::readVelocityFileIsForbidden(char *msg)
{
  assert(msg);
  if(_locked)
      {
      strcpy(msg, "cannot read file while data changes are locked");
      return TRUE;
      }
  return FALSE;
}


int VfHelper::saveVelocityFileIsForbidden(char *msg)
{
  assert(msg);
  if(!_editable)
      {
      strcpy(msg, "there is no need to save uneditable dataset");
      return TRUE;
      }
  return FALSE;
}



/*
//-------------------- history cards ---------------------------------//
//-------------------- history cards ---------------------------------//
//-------------------- history cards ---------------------------------//

        // public.

long VfHelper::numHistoryCards()  const
{
  return _history->numHistoryCards();
}



const char *VfHelper::getHistoryCard(long icard)  const
{
  return _history->getHistoryCard(icard);
}
*/



//-------------------- last file read or saved -------------------//
//-------------------- last file read or saved -------------------//
//-------------------- last file read or saved -------------------//

         // public

const char *VfHelper::lastFileRead        ()  const
{
  return _backup->lastFileRead();
}


const char *VfHelper::lastFileSaved       ()  const
{
  return _backup->lastFileSaved();
}


const char *VfHelper::lastBackupFileSaved ()  const
{
  return _backup->lastBackupFileSaved();
}



//--------------------- save backup file --------------------------------//
//--------------------- save backup file --------------------------------//
//--------------------- save backup file --------------------------------//

           // public.

void VfHelper::saveBackupFile()
{
  if(!_editable)     return;
  if(!_needs_saving) return;
  if( _backed_up)    return;
  if( _editable == EDIT_BUT_SUPPRESS_BACKUPS) return;
  _informer->beginSlowOperations();
  MESSAGE("saving data to backup workfile...")
  char info[222];
  int error = _backup->saveBackupFile(info);
  if(!error) _backed_up = TRUE;
  BELL(error)
  MESSAGE(info)
/*****
  HISTORY(info)
*****/
  _informer->endSlowOperations();
}



//-------------------- interact with undo file -----------------------//
//-------------------- interact with undo file -----------------------//
//-------------------- interact with undo file -----------------------//

         // public.

void VfHelper::saveUndoFile(void *doer)
{
  if(!_editable)   return;
  if(doer == NULL) return;
  _informer->beginSlowOperations();
  MESSAGE("saving data to undo file...")
  char msg[80];
  int error = _undo->saveUndoFile(doer, msg);
  BELL(error)
  MESSAGE(msg)
/*****
  HISTORY(msg)
*****/
  _informer->endSlowOperations();
}



int VfHelper::allowReadDeleteUndoFile(void *doer)  const
{
  return _undo->allowReadDeleteUndoFile(doer);
}



void VfHelper::maybeDeleteUndoFile(void *doer)
{
  _undo->maybeDeleteUndoFile(doer);
}



void VfHelper::maybeReadUndoFile(void *doer)
{
  if(dataChangesAreForbidden()) return;
  if(!_undo->allowReadDeleteUndoFile(doer)) return;
  _informer->preTotalChanges(_dataset);
  MESSAGE("restoring data from undo file...");
  char msg[80];
  int error = _undo->maybeReadUndoFile(doer, msg);
  BELL(error)
  MESSAGE(msg)
/*****
  HISTORY(msg)
*****/
  TURN_ON
  if(!error)
      {
      BACKUP
      MESSAGE(msg)
      }
  _informer->postTotalChanges(_dataset);
}




                //-------- private functions ----------//
                //-------- private functions ----------//
                //-------- private functions ----------//
                //-------- private functions ----------//
                //-------- private functions ----------//
                //-------- private functions ----------//
                //-------- private functions ----------//
                //-------- private functions ----------//
                //-------- private functions ----------//
                //-------- private functions ----------//
                //-------- private functions ----------//



//---------------- turn on and off data needs saving flag ---------------//
//---------------- turn on and off data needs saving flag ---------------//
//---------------- turn on and off data needs saving flag ---------------//

   // private.
   // should NOT be called if no changes are being made to the data.
   // should NOT be called if you are simply setting an "active" item
   //   or a "reference" item or selecting/unselecting items or resetting
   //   the data lock.
   // the first function is to be called before any data changes are made.
   // the second function is to be called when the data in memory is
   //   cleared, or when the data in memory no longer differs from
   //   data on a disk file (e.g. after a velocity file is read or saved).

   // note: it is assumed that data still needs saving if it is saved
   //   to a backup file.
   // (1) data is considered "backed up" if it has been saved to a
   //       user-named file or to a backup file since it was last modified,
   //       or if it has not yet been modified since it was read in.
   // (2) data is considered "saved" if it has been saved to a
   //       user-named file since it was last modified, or if it has not
   //       yet been modified since it was read in.


void VfHelper::turnOnDataNeedsSavingFlag()
{
  if(!_editable) return;
  if(!_needs_saving)
      {
      _needs_saving = TRUE;
      _informer->dataNeedsSavingFlagTurnedOn(_dataset);
      }
  _backed_up = FALSE;
}


void VfHelper::turnOffDataNeedsSavingFlag()
{
  if(!_editable) return;
  if(_needs_saving)
      {
      _needs_saving = FALSE;
      _informer->dataNeedsSavingFlagTurnedOff(_dataset);
      }
  _backed_up = TRUE;
}



//--------------------- various broadcast functions --------------------//
//--------------------- various broadcast functions --------------------//
//--------------------- various broadcast functions --------------------//

   // private.

void VfHelper::broadcastMessage(const char *msg)
{
  assert(msg);
  _informer->showMessage(msg);
}



void VfHelper::broadcastBell(int error)
{
  if(error)
      {
      _informer->ringBell();
      }
}



void VfHelper::broadcastHistory(const char *card)
{
  assert(card);
  _history->addHistoryCard(card);
  _informer->newHistoryCard(_dataset);
}




              //-------- pre and post operations --------/
              //-------- pre and post operations --------/
              //-------- pre and post operations --------/
              //-------- pre and post operations --------/
              //-------- pre and post operations --------/
              //-------- pre and post operations --------/
              //-------- pre and post operations --------/
              //-------- pre and post operations --------/
              //-------- pre and post operations --------/
              //-------- pre and post operations --------/
              //-------- pre and post operations --------/



//------------------- pre and post select dataset -------------------//
//------------------- pre and post select dataset -------------------//
//------------------- pre and post select dataset -------------------//

           // public.

void VfHelper::preSelectDataset()
{
  _informer->preSelectDataset(_dataset);
}


void VfHelper::postSelectDataset()
{
  _informer->postSelectDataset(_dataset);
}


void VfHelper::preUnselectDataset()
{
  _informer->preUnselectDataset(_dataset);
}


void VfHelper::postUnselectDataset()
{
  _informer->postUnselectDataset(_dataset);
}



//--------------------- pre and post total changes --------------------//
//--------------------- pre and post total changes --------------------//
//--------------------- pre and post total changes --------------------//

           // public.

void VfHelper::preTotalChanges()
{
  _informer->preTotalChanges(_dataset);
}


void VfHelper::postTotalChanges()
{
  _informer->postTotalChanges(_dataset);
}



//------------------- pre and post edit ---------------------------//
//------------------- pre and post edit ---------------------------//
//------------------- pre and post edit ---------------------------//

           // public.

void VfHelper::preEdit(VfEditBase *edit, const char *msg, int error, void *doer)
{
  assert(edit);
  if(error)
      {
      BELL(error)
      MESSAGE(msg)
      return;
      }
  long nfun = _kernal->numVelocityFunctions();
  switch(edit->informStyle())
      {
      case VfEditBase::INFORM_TOTAL:
                          _informer->preTotalChanges(_dataset);
                          break;
      case VfEditBase::INFORM_STRINGS:
                          _informer->preModifyStrings(_dataset, 0, nfun);
                          break;
      case VfEditBase::INFORM_TYPE:
                          _informer->preNewDefaultTypes(_dataset, 0, nfun);
                          break;
      default: assert(FALSE);
      }

  DOER(doer)
  MESSAGE(msg)
}



void VfHelper::postEdit(VfEditBase *edit, const char *msg, int error)
{
  BELL(error)
  MESSAGE(msg)
  HISTORY(msg)
  TURN_ON
  if(!error)
      {
      BACKUP
      MESSAGE(msg)
      }

  assert(edit);
  long nfun = _kernal->numVelocityFunctions();
  switch(edit->informStyle())
      {
      case VfEditBase::INFORM_TOTAL:
                          _informer->postTotalChanges(_dataset);
                          break;
      case VfEditBase::INFORM_STRINGS:
                          _informer->postModifyStrings(_dataset, 0, nfun);
                          break;
      case VfEditBase::INFORM_TYPE:
                          _informer->postNewDefaultTypes(_dataset, 0, nfun);
                          break;
      default: assert(FALSE);
      }
}



//------------------- pre and post read ---------------------------//
//------------------- pre and post read ---------------------------//
//------------------- pre and post read ---------------------------//

      // public.
      // VfReadSave calls showMessage/ringBell so they need not be done here.

void VfHelper::preRead(void *doer)
{
  _informer->preTotalChanges(_dataset);
  BACKUP
  DOER(doer)
}



void VfHelper::postRead(const char *filename, const char *msg, int error,
                                                         int replaced_all)
{
  char info   [222];
  char history[222];
  char title  [222];
  _backup->afterReadingFile(filename, error, replaced_all,
                                                   info, history, title);
/*****
  HISTORY(msg)
  HISTORY(history)
  HISTORY(info)
*****/
  if(_editable)
      {
      _informer->sendMessage("title", title);
      }
  if(!error &&  replaced_all) TURN_OFF
  if(!error && !replaced_all)
      {
      BACKUP
      MESSAGE(msg)
      }
  _informer->postTotalChanges(_dataset);
}



//------------------- pre and post save ---------------------------//
//------------------- pre and post save ---------------------------//
//------------------- pre and post save ---------------------------//

      // public.
      // VfReadSave calls showMessage/ringBell so they need not be done here.

void VfHelper::preSave()
{
  _informer->beginSlowOperations();
}



void VfHelper::postSave(const char *filename, const char *msg, int error,
                                                         int saved_all)
{
  char info   [222];
  char history[222];
  char title  [222];
  _backup->afterSavingFile(filename, error, saved_all,
                                                 info, history, title);
/*****
  HISTORY(msg)
  HISTORY(history)
  HISTORY(info)
*****/
  if(_editable)
      {
      _informer->sendMessage("title", title);
      }
  if(!error && saved_all) TURN_OFF
  _informer->endSlowOperations();
}



//------------------- pre and post change header words --------------//
//------------------- pre and post change header words --------------//
//------------------- pre and post change header words --------------//

           // public.

void VfHelper::preChangeHeaderWords()
{
  _informer->preChangeHeaderWords(_dataset);
}


void VfHelper::postChangeHeaderWords()
{
  HISTORY("header word changed")
  TURN_ON
  _informer->postChangeHeaderWords(_dataset);
}



//------------------- pre and post change moveout order -------------//
//------------------- pre and post change moveout order -------------//
//------------------- pre and post change moveout order -------------//

           // public.

void VfHelper::preChangeMoveoutOrder()
{
  _informer->preChangeMoveoutOrder(_dataset);
}


void VfHelper::postChangeMoveoutOrder()
{
  HISTORY("moveout changed")
  TURN_ON
  _informer->postChangeMoveoutOrder(_dataset);
}



//------------------- pre and post change units ---------------------//
//------------------- pre and post change units ---------------------//
//------------------- pre and post change units ---------------------//

           // public.

void VfHelper::preChangeUnits()
{
  _informer->preChangeUnits(_dataset);
}


void VfHelper::postChangeUnits()
{
  HISTORY("units changed")
  TURN_ON
  _informer->postChangeUnits(_dataset);
}



//------------ pre and post new active velocity function ------------//
//------------ pre and post new active velocity function ------------//
//------------ pre and post new active velocity function ------------//

           // public.

static long do_active_function_informs = FALSE;

void VfHelper::preNewActiveVelocityFunction(long ifun)
{
  long index = _kernal->getActiveVelocityFunction();
  do_active_function_informs = (ifun != index);
  if(!do_active_function_informs) return;
  _informer->preNewNeighbors             (_dataset);
  _informer->preNewActiveVelocityFunction(_dataset);
}


void VfHelper::postNewActiveVelocityFunction()
{
  if(!do_active_function_informs) return;
  BACKUP     // backup file is saved when scanning to next velocity function.
  MESSAGE("new active velocity function")
  _informer->postNewActiveVelocityFunction(_dataset);
  _informer->postNewNeighbors             (_dataset);
}



//------------ pre and post new reference velocity function ------------//
//------------ pre and post new reference velocity function ------------//
//------------ pre and post new reference velocity function ------------//

           // public.

static long do_reference_function_informs = FALSE;

void VfHelper::preNewReferenceVelocityFunction(long ifun)
{
  long index = _kernal->getReferenceVelocityFunction();
  do_reference_function_informs = (ifun != index);
  if(!do_reference_function_informs) return;
  _informer->preNewReferenceVelocityFunction(_dataset);
}


void VfHelper::postNewReferenceVelocityFunction()
{
  if(!do_reference_function_informs) return;
  MESSAGE("new reference velocity function")
  _informer->postNewReferenceVelocityFunction(_dataset);
}



//---------- pre and post remove insert velocity functions ----------//
//---------- pre and post remove insert velocity functions ----------//
//---------- pre and post remove insert velocity functions ----------//

   // public.
   // also sends new active and/or reference function inform if needed.
   // always sends new active and reference function inform if always is true.

static int do_active_inform    = FALSE;
static int do_reference_inform = FALSE;

void VfHelper::preRemoveInsertVelocityFunctions
                          (long ifun, long nrem, long nins, int always)
{
  _informer->preNewNeighbors(_dataset);
  long active    = _kernal->getActiveVelocityFunction();
  long reference = _kernal->getReferenceVelocityFunction();
  do_active_inform    = (active    >= ifun || active    == -1 || always);
  do_reference_inform = (reference >= ifun || reference == -1 || always);
  if(do_active_inform)
      {
      _informer->preNewActiveVelocityFunction(_dataset);
      }
  if(do_reference_inform)
      {
      _informer->preNewReferenceVelocityFunction(_dataset);
      }
  _informer->preRemoveInsertVelocityFunctions(_dataset, ifun, nrem, nins);
}


void VfHelper::postRemoveInsertVelocityFunctions
                                    (long ifun, long nrem, long nins)
{
  if     (nrem >  0 && nins == 0) HISTORY("remove velocity functions")
  else if(nrem == 0 && nins == 1) HISTORY("add or edit velocity function")
  else if(nrem == 0 && nins >  0) HISTORY("insert velocity functions")
  else                            HISTORY("remove/insert velocity functions")
  TURN_ON
  _informer->postRemoveInsertVelocityFunctions(_dataset, ifun, nrem, nins);
  if(do_reference_inform)
      {
      _informer->postNewReferenceVelocityFunction(_dataset);
      }
  if(do_active_inform)
      {
      _informer->postNewActiveVelocityFunction(_dataset);
      }
  _informer->postNewNeighbors(_dataset);
}



//------------------- pre and post change selections ----------------//
//------------------- pre and post change selections ----------------//
//------------------- pre and post change selections ----------------//

           // public.

void VfHelper::preChangeSelections(long ifun)
{
  if(_kernal->severalSelectionsInProgress()) return;
  long nfun = _kernal->numVelocityFunctions();
  long nchng = nfun - ifun;
  _informer->preChangeSelections(_dataset, ifun, nchng);
}


void VfHelper::postChangeSelections(long ifun)
{
  if(_kernal->severalSelectionsInProgress()) return;
  long nfun = _kernal->numVelocityFunctions();
  long nchng = nfun - ifun;
  _informer->postChangeSelections(_dataset, ifun, nchng);
}



//------------------- pre and post modify picks ---------------------//
//------------------- pre and post modify picks ---------------------//
//------------------- pre and post modify picks ---------------------//

           // public.

static int  active_pick_changed = FALSE;
static long          ipick_keep = -1;
static long         npicks_keep = -1;
static long           nrem_keep = -1;


void VfHelper::preModifyPicks(long ifun, int type, long ipick, long nrem)
{
  assert(npicks_keep == -1 && nrem_keep == -1);
  long npicks = _kernal->velfun(ifun)->numPicks();
  long active = _kernal->velfun(ifun)->getActivePick();
  int  error  = _kernal->velfun(ifun)->getErrorFlag();
  if(type  == -1) type  = _kernal->velfun(ifun)->getDefaultType();
  if(ipick == -1) ipick = npicks;
  if(nrem  == -1) nrem  = npicks - ipick;
  assert(ipick >= 0 && ipick <= npicks && nrem >= 0);
  active_pick_changed = (ipick != active || active == npicks - 1);
  if(error != ERROR_NONE || ipick <= 1) { ipick = 0; nrem = npicks; }
  ipick_keep          = ipick;
  npicks_keep         = npicks;
  nrem_keep           = nrem;
  if(active_pick_changed) _informer->preNewActivePicks(_dataset, ifun, 1);
  _informer->preChangePickSelections(_dataset, ifun);
  _informer->preModifyPicks(_dataset, ifun, type, ipick_keep, nrem_keep);
}


void VfHelper::postModifyPicks(long ifun, int type, long ipick)
{
  assert(npicks_keep != -1 && nrem_keep != -1);
  long npicks = _kernal->velfun(ifun)->numPicks();
  long nins   = nrem_keep + npicks - npicks_keep;
  if(type  == -1) type  = _kernal->velfun(ifun)->getDefaultType();
  if(ipick == -1) ipick = npicks_keep;
  assert(ipick >= 0 && ipick <= npicks_keep && nins >= 0);
/****
  HISTORY("modify velocity function picks");
****/
  HISTORY("add or edit velocity function");
  TURN_ON
  _informer->postModifyPicks(_dataset, ifun, type, ipick_keep, nrem_keep, nins);
  _informer->postChangePickSelections(_dataset, ifun);
  if(active_pick_changed) _informer->postNewActivePicks(_dataset, ifun, 1);
  active_pick_changed = FALSE;
  ipick_keep          = -1;
  npicks_keep         = -1;
  nrem_keep           = -1;
}



//------------------- pre and post change coords --------------------//
//------------------- pre and post change coords --------------------//
//------------------- pre and post change coords --------------------//

           // public.

void VfHelper::preChangeCoords(long ifun, long nchng)
{
  _informer->preNewNeighbors(_dataset);
  _informer->preChangeCoords(_dataset, ifun, nchng);
}


void VfHelper::postChangeCoords(long ifun, long nchng)
{
/****
  HISTORY("change velocity function coordinate")
****/
  HISTORY("add or edit velocity function");
  TURN_ON
  _informer->postChangeCoords(_dataset, ifun, nchng);
  _informer->postNewNeighbors(_dataset);
}



//------------------- pre and post new active picks -----------------//
//------------------- pre and post new active picks -----------------//
//------------------- pre and post new active picks -----------------//

           // public.

static long do_active_pick_informs = FALSE;

void VfHelper::preNewActivePicks(long ifun, long nchng, long ipick)
{
  long index = _kernal->velfun(ifun)->getActivePick();
  do_active_pick_informs = (ipick != index || nchng > 1);
  if(!do_active_pick_informs) return;
  _informer->preNewActivePicks(_dataset, ifun, nchng);
}


void VfHelper::postNewActivePicks(long ifun, long nchng)
{
  if(!do_active_pick_informs) return;
  _informer->postNewActivePicks(_dataset, ifun, nchng);
}



//------------------- pre and post new default types ----------------//
//------------------- pre and post new default types ----------------//
//------------------- pre and post new default types ----------------//

           // public.

void VfHelper::preNewDefaultTypes(long ifun, long nchng)
{
  _informer->preNewDefaultTypes(_dataset, ifun, nchng);
}


void VfHelper::postNewDefaultTypes(long ifun, long nchng)
{
/****
  HISTORY("change velocity function type")
****/
  HISTORY("add or edit velocity function");
  TURN_ON
  _informer->postNewDefaultTypes(_dataset, ifun, nchng);
}



//------------------- pre and post modify strings -------------------//
//------------------- pre and post modify strings -------------------//
//------------------- pre and post modify strings -------------------//

           // public.

void VfHelper::preModifyStrings(long ifun, long nchng)
{
  _informer->preModifyStrings(_dataset, ifun, nchng);
}


void VfHelper::postModifyStrings(long ifun, long nchng)
{
/****
  HISTORY("change velocity function name or string")
****/
  HISTORY("add or edit velocity function");
  TURN_ON;
  _informer->postModifyStrings(_dataset, ifun, nchng);
}



//------------------- pre and post change pick selections -------------//
//------------------- pre and post change pick selections -------------//
//------------------- pre and post change pick selections -------------//

           // public.

void VfHelper::preChangePickSelections(long ifun)
{
  _informer->preChangePickSelections(_dataset, ifun);
}


void VfHelper::postChangePickSelections(long ifun)
{
  _informer->postChangePickSelections(_dataset, ifun);
}


//-------------------------- end -------------------------------//
//-------------------------- end -------------------------------//
//-------------------------- end -------------------------------//

