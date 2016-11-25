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

//--------------------- static_helper.cc ---------------------//
//--------------------- static_helper.cc ---------------------//
//--------------------- static_helper.cc ---------------------//

//         implementation file for the StaticHelper class
//                  not derived from any class
//                      subdirectory stat



#include "stat/static_helper.hh"
#include "stat/static_undo.hh"
#include "stat/static_backup.hh"
#include "stat/static_informer.hh"
#include "named_constants.h"
#include <string.h>
#include <iostream.h>
#include <stdlib.h>
#include <stdio.h>
#include <assert.h>
#include <limits.h>


         // StaticDataset header file not needed.
         // StaticKernal  header file not needed.


#define INFORMER        if(_informer) _informer
#define LOCKED          dataChangesAreLocked()
#define TURN_ON         turnOnDataNeedsSavingFlag();
#define TURN_OFF        turnOffDataNeedsSavingFlag();
#define BACKUP          saveBackupFile();
#define DOER(doer)      saveUndoFile(doer);
#define MESSAGE(msg)    if(msg && _informer) _informer->showMessage(msg);
#define BELL(error)     if(error && _informer) _informer->ringBell();


//---------------------- constructor -----------------------//
//---------------------- constructor -----------------------//
//---------------------- constructor -----------------------//


StaticHelper::StaticHelper (StaticInformer *informer,
                            StaticDataset  *dataset,
                            StaticKernal   *kernal)
           :
          _locked              (FALSE),
          _needs_saving        (FALSE),
          _backed_up           (TRUE),

          _informer            (informer),    // can be NULL.
          _dataset             (dataset),
          _undo                (NULL),
          _backup              (NULL)
{
  _undo   = new StaticUndo   (kernal);
  _backup = new StaticBackup (kernal);
  assert(_dataset);
  assert(_undo);
  assert(_backup);
}



//------------------------ destructor --------------------------//
//------------------------ destructor --------------------------//
//------------------------ destructor --------------------------//

StaticHelper::~StaticHelper()
{
  BACKUP
  delete _undo;
  delete _backup;
}



                   // ----- private functions ------//
                   // ----- private functions ------//
                   // ----- private functions ------//
                   // ----- private functions ------//
                   // ----- private functions ------//
                   // ----- private functions ------//
                   // ----- private functions ------//
                   // ----- private functions ------//
                   // ----- private functions ------//
                   // ----- private functions ------//
                   // ----- private functions ------//



//---------------- turn on and off data needs saving flag ---------------//
//---------------- turn on and off data needs saving flag ---------------//
//---------------- turn on and off data needs saving flag ---------------//

   // private.
   // should NOT be called if no changes are being made to the data.
   // should NOT be called if you are simply setting an "active" item
   //   or selecting/unselecting items or resetting the data lock.
   // the first function is to be called before any data changes are made.
   // the second function is to be called when the data in memory is
   //   cleared, or when the data in memory no longer differs from
   //   data on a disk file (e.g. after a static file is read or saved).

   // note: it is assumed that data still needs saving if it is saved
   //   to a backup file.
   // (1) data is considered "backed up" if it has been saved to a
   //       user-named file or to a backup file since it was last modified,
   //       or if it has not yet been modified since it was read in.
   // (2) data is considered "saved" if it has been saved to a
   //       user-named file since it was last modified, or if it has not
   //       yet been modified since it was read in.

void StaticHelper::turnOnDataNeedsSavingFlag()
{
  if(!_needs_saving)
      {
      _needs_saving = TRUE;
      INFORMER->dataNeedsSavingFlagTurnedOn(_dataset);
      }
  _backed_up = FALSE;
}


void StaticHelper::turnOffDataNeedsSavingFlag()
{
  if(_needs_saving)
      {
      _needs_saving = FALSE;
      INFORMER->dataNeedsSavingFlagTurnedOff(_dataset);
      }
  _backed_up = TRUE;
}



//------------------- data changes are locked ------------------//
//------------------- data changes are locked ------------------//
//------------------- data changes are locked ------------------//

   // private.
   // to be called first thing if any changes to the data are requested.
   // should NOT be called if no changes are being made to the data.
   // should NOT be called if you are simply setting an "active" item
   //   or selecting/unselecting items or resetting the data lock.
   // rings bell and shows special message if data changes are not allowed.
   // returns error = TRUE  if data changes are not allowed.
   // returns error = FALSE if data changes are allowed.

int StaticHelper::dataChangesAreLocked()
{
  if(!_locked) return FALSE;
  BELL(TRUE)
  MESSAGE("attempt to change data when locked")
  return TRUE;
}



//----------------------- error happened --------------------------//
//----------------------- error happened --------------------------//
//----------------------- error happened --------------------------//

   // private.
   // to be called to if a user error might have occurred.
   // rings bell and shows specified message if the error argument is TRUE.
   // returns error = TRUE  if the error argument is TRUE.
   // returns error = FALSE if the error argument is FALSE. 

int StaticHelper::errorHappened(const char *msg, int error)
{
  if(error)
      {
      BELL(error)
      MESSAGE(msg)
      }
  return error;
}



                   //------ public functions ------//
                   //------ public functions ------//
                   //------ public functions ------//
                   //------ public functions ------//
                   //------ public functions ------//
                   //------ public functions ------//
                   //------ public functions ------//
                   //------ public functions ------//
                   //------ public functions ------//
                   //------ public functions ------//
                   //------ public functions ------//
                   //------ public functions ------//



//-------------------- lock or unlock data --------------------------//
//-------------------- lock or unlock data --------------------------//
//-------------------- lock or unlock data --------------------------//

       // public.

void StaticHelper::lockData()
{
  if(_locked) return;
  INFORMER->preChangeDataLock(_dataset);
  _locked = TRUE;
  INFORMER->postChangeDataLock(_dataset);
}


void StaticHelper::unlockData()
{
  if(!_locked) return;
  INFORMER->preChangeDataLock(_dataset);
  _locked = FALSE;
  INFORMER->postChangeDataLock(_dataset);
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

void StaticHelper::preSelectDataset()
{
  INFORMER->preSelectDataset(_dataset);
}


void StaticHelper::postSelectDataset()
{
  INFORMER->postSelectDataset(_dataset);
}


void StaticHelper::preUnselectDataset()
{
  INFORMER->preUnselectDataset(_dataset);
}


void StaticHelper::postUnselectDataset()
{
  INFORMER->postUnselectDataset(_dataset);
}



//------------------- pre and post change selections ----------------//
//------------------- pre and post change selections ----------------//
//------------------- pre and post change selections ----------------//

           // public.

void StaticHelper::preChangeSelections
                            (int ix, int iy, int nxchng, int nychng)
{
  INFORMER->preChangeSelections(_dataset, ix, iy, nxchng, nychng);
}


void StaticHelper::postChangeSelections
                            (int ix, int iy, int nxchng, int nychng)
{
  INFORMER->postChangeSelections(_dataset, ix, iy, nxchng, nychng);
}



//---------------- pre and post new active ground position -----------//
//---------------- pre and post new active ground position -----------//
//---------------- pre and post new active ground position -----------//

           // public.

void StaticHelper::preNewActiveGroundPosition()
{
  INFORMER->preNewActiveGroundPosition(_dataset);
}


void StaticHelper::postNewActiveGroundPosition()
{
  INFORMER->postNewActiveGroundPosition(_dataset);
}



//------------------- pre and post read ---------------------------//
//------------------- pre and post read ---------------------------//
//------------------- pre and post read ---------------------------//

           // public.

int StaticHelper::preRead(void *doer, const char *working, char *msg)
{
  if(_locked)
      {
      strcpy(msg, "cannot read static file while data changes are locked");
      BELL(TRUE)
      MESSAGE(msg)
      return TRUE;
      }
  strcpy(msg, working);
  INFORMER->preTotalChanges(_dataset);
  BACKUP
  DOER(doer)
  MESSAGE(msg)
  return FALSE;
}



void StaticHelper::postRead(const char *filename, const char *msg, int error)
{
  char title[222];
  _backup->afterReadingFile(filename, error, TRUE, NULL, NULL, title);
  TURN_OFF
  BELL(error)
  MESSAGE(msg)
  INFORMER->sendMessage("title", title);
  INFORMER->postTotalChanges(_dataset);
}



//------------------- pre and post save ---------------------------//
//------------------- pre and post save ---------------------------//
//------------------- pre and post save ---------------------------//

           // public.

void StaticHelper::preSave(const char *msg)
{
  INFORMER->beginSlowOperations();
  MESSAGE(msg)
}



void StaticHelper::postSave(const char *filename, const char *msg, int error)
{
  char title[222];
  _backup->afterSavingFile(filename, error, TRUE, NULL, NULL, title);
  BELL(error)
  MESSAGE(msg)
  if(!error) TURN_OFF
  INFORMER->sendMessage("title", title);
  INFORMER->endSlowOperations();
}



//--------------------- pre and post copy -----------------------------//
//--------------------- pre and post copy -----------------------------//
//--------------------- pre and post copy -----------------------------//

           // public.

int StaticHelper::preCopy(void *doer, const char *msg)
{
  if(LOCKED) return TRUE;
  INFORMER->preTotalChanges(_dataset);
  BACKUP
  DOER(doer)
  MESSAGE(msg)
  return FALSE;
}


void StaticHelper::postCopy(const char *msg)
{
  TURN_OFF
  MESSAGE(msg)
  INFORMER->postTotalChanges(_dataset);
}



//--------------------- pre and post total changes --------------------//
//--------------------- pre and post total changes --------------------//
//--------------------- pre and post total changes --------------------//

           // public.
           // doer and/or msg can be NULL or omitted.
           // error can be FALSE or omitted.

int StaticHelper::preTotalChanges(void *doer, const char *msg, int error)
{
  if(LOCKED) return TRUE;
  if(errorHappened(msg, error)) return TRUE;
  INFORMER->preTotalChanges(_dataset);
  DOER(doer)
  MESSAGE(msg)
  return FALSE;
}


void StaticHelper::postTotalChanges(const char *msg, int error)
{
  TURN_ON
  BACKUP
  BELL(error)
  MESSAGE(msg)
  INFORMER->postTotalChanges(_dataset);
}



//---------------- pre and post change static values ----------------//
//---------------- pre and post change static values ----------------//
//---------------- pre and post change static values ----------------//

           // public.

int StaticHelper::preChangeStaticValues(void *doer, const char *msg, int error,
                             int ix, int iy, int nxchng, int nychng)
{
  if(LOCKED) return TRUE;
  if(errorHappened(msg, error)) return TRUE;
  INFORMER->preChangeStaticValues(_dataset, ix, iy, nxchng, nychng);
  DOER(doer)
  MESSAGE(msg)
  return FALSE;
}


void StaticHelper::postChangeStaticValues(const char *msg, int error,
                             int ix, int iy, int nxchng, int nychng)
{
  TURN_ON
  if(nxchng > 1 || nychng > 1) BACKUP
  BELL(error)
  MESSAGE(msg)
  INFORMER->postChangeStaticValues(_dataset, ix, iy, nxchng, nychng);
}



//------------------- pre and post change stattype ----------------------//
//------------------- pre and post change stattype ----------------------//
//------------------- pre and post change stattype ----------------------//

           // public.

int StaticHelper::preChangeStattype()
{
  if(LOCKED) return TRUE;
  INFORMER->preChangeStattype(_dataset);
  return FALSE;
}


void StaticHelper::postChangeStattype()
{
  TURN_ON
  INFORMER->postChangeStattype(_dataset);
}



//------------------- pre and post change header words --------------//
//------------------- pre and post change header words --------------//
//------------------- pre and post change header words --------------//

           // public.

int StaticHelper::preChangeHeaderWords()
{
  if(LOCKED) return TRUE;
  INFORMER->preChangeHeaderWords(_dataset);
  return FALSE;
}


void StaticHelper::postChangeHeaderWords()
{
  TURN_ON
  INFORMER->postChangeHeaderWords(_dataset);
}



//-------------- pre and post transform ground positions ---------------//
//-------------- pre and post transform ground positions ---------------//
//-------------- pre and post transform ground positions ---------------//

           // public.
           // doer and/or msg can be NULL or omitted.
           // error can be FALSE or omitted.

int StaticHelper::preTransformGroundPositions
                                (void *doer, const char *msg, int error)
{
  if(LOCKED) return TRUE;
  if(errorHappened(msg, error)) return TRUE;
  INFORMER->preTransformGroundPositions(_dataset);
  DOER(doer)
  MESSAGE(msg)
  return FALSE;
}


void StaticHelper::postTransformGroundPositions(const char *msg, int error)
{
  TURN_ON
  if(msg) BACKUP
  BELL(error)
  MESSAGE(msg)
  INFORMER->postTransformGroundPositions(_dataset);
}




                 //----- interact with disk files -----//
                 //----- interact with disk files -----//
                 //----- interact with disk files -----//
                 //----- interact with disk files -----//
                 //----- interact with disk files -----//
                 //----- interact with disk files -----//
                 //----- interact with disk files -----//
                 //----- interact with disk files -----//
                 //----- interact with disk files -----//
                 //----- interact with disk files -----//


//-------------------- interact with undo file -----------------------//
//-------------------- interact with undo file -----------------------//
//-------------------- interact with undo file -----------------------//

     // public.

int StaticHelper::allowReadDeleteUndoFile(void *doer)  const
{
  return _undo->allowReadDeleteUndoFile(doer);
}



void StaticHelper::maybeDeleteUndoFile(void *doer)
{
  _undo->maybeDeleteUndoFile(doer);
}



void StaticHelper::maybeReadUndoFile(void *doer)
{
  if(LOCKED) return;
  INFORMER->preTotalChanges(_dataset);
  MESSAGE("restoring data from UNDO file...")
  char msg[200];
  int error = _undo->maybeReadUndoFile(doer, msg);
  TURN_ON
  if(!error) BACKUP
  BELL(error)
  MESSAGE(msg)
  INFORMER->postTotalChanges(_dataset);
}



void StaticHelper::saveUndoFile(void *doer)
{
  if(doer == NULL) return;
  INFORMER->beginSlowOperations();
  MESSAGE("saving data to UNDO file...")
  char msg[200];
  int error = _undo->saveUndoFile(doer, msg);
  BELL(error)
  MESSAGE(msg)
  INFORMER->endSlowOperations();
}



//------------------ save backup file --------------------------//
//------------------ save backup file --------------------------//
//------------------ save backup file --------------------------//

        // public.

void StaticHelper::saveBackupFile()
{
  if(!_needs_saving) return;
  if(_backed_up) return;
  INFORMER->beginSlowOperations();
  MESSAGE("saving data to backup workfile...")
  char msg[200];
  int error = _backup->saveBackupFile(msg);
  BELL(error)
  MESSAGE(msg)
  if(!error) _backed_up = TRUE;
  INFORMER->endSlowOperations();
}



//------------------- learn last file read or saved ----------------//
//------------------- learn last file read or saved ----------------//
//------------------- learn last file read or saved ----------------//

         // public.

const char *StaticHelper::lastFileSaved       ()  const
{
  return _backup->lastFileSaved();
}


const char *StaticHelper::lastFileRead        ()  const
{
  return _backup->lastFileRead();
}


const char *StaticHelper::lastBackupFileSaved ()  const
{
  return _backup->lastBackupFileSaved();
}



//------------------------------- end -------------------------------//
//------------------------------- end -------------------------------//
//------------------------------- end -------------------------------//

