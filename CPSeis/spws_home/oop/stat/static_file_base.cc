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

//---------------------- static_file_base.cc ------------------------//
//---------------------- static_file_base.cc ------------------------//
//---------------------- static_file_base.cc ------------------------//

//         implementation file for the StaticFileBase class
//                 derived from the FileBase class
//              derived from the PjarFileSupport class
//                        subdirectory stat


#include "stat/static_file_base.hh"
#include "stat/static_manager.hh"
#include "stat/static_dataset.hh"
#include "stat/static_kernal.hh"
#include "stat/static_informer.hh"
#include "stat/statio_wrapper.hh"
#include "oprim/history_cards.hh"
#include "named_constants.h"
#include "str.h"
#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <assert.h>

#define VALIDATE_ONLY_WHEN_CHANGED  TRUE


//-------------------- constructor -------------------------------//
//-------------------- constructor -------------------------------//
//-------------------- constructor -------------------------------//


StaticFileBase::StaticFileBase(StaticManager *manager, void *doer,
                               StatioWrapper *statio,
                               StatioWrapper *statio2)
       : FileBase("static file", "stat",
                  (statio->isInput() ? USE_FOR_INPUT : USE_FOR_OUTPUT),
                  VALIDATE_ONLY_WHEN_CHANGED),
               _read_choice (READ_ACTIVE),
               _manager     (manager),
               _doer        (doer),
               _statio      (statio),
               _statio2     (statio2)
{
  assert(_manager && _statio);
  assert(_statio2 || statio->isInput());
}



//-------------------- destructor -------------------------------//
//-------------------- destructor -------------------------------//
//-------------------- destructor -------------------------------//


StaticFileBase::~StaticFileBase()
{
}



//-------------------------- set read choice -----------------------//
//-------------------------- set read choice -----------------------//
//-------------------------- set read choice -----------------------//

     // public.

void StaticFileBase::setReadChoice(int read_choice)
{
  assert(read_choice == READ_ACTIVE    ||
         read_choice == READ_REF       ||
         read_choice == READ_NEW       ||
         read_choice == READ_NEWACTIVE ||
         read_choice == READ_NEWREF    ||
         read_choice == READ_NOTHING);
  _read_choice = read_choice;
}



//----------------------- update pjar from kernal -----------------------//
//----------------------- update pjar from kernal -----------------------//
//----------------------- update pjar from kernal -----------------------//

                  // call this prior to saving the file,
                  // or when the active kernal changes,
                  // or when something in the active kernal changes,
                  // or any time the user requests.


void StaticFileBase::updatePjarFromKernal (int skiphist)
{
  _manager->activeDataset()->updatePjar(_statio, skiphist);
}



//-------------------- virtual validate -------------------------//
//-------------------- virtual validate -------------------------//
//-------------------- virtual validate -------------------------//

    // protected virtual function overriding FileBase.
    // sets info to a string of information about the file (if valid).

FileBase::Validity
StaticFileBase::virtualValidate(const char *filename, char *info)
{
  strcpy(info, " ");
  int io = ioIndex();
  int error;
  if(io == 0)
      {
      error = _statio->validateFile(filename, info);
      resetFileType(_statio->filetype());
      if(_read_choice == READ_NOTHING)
           {
           StaticDataset *dataset = _manager->activeDataset();
           int nx     = dataset->getNx();
           int ny     = dataset->getNy();
           int ncards = dataset->history()->numHistoryCards();
           if(nx <= 1 && ny <= 1 && ncards <= 0) _read_choice = READ_ACTIVE;
           else                                  _read_choice = READ_NEW;
           }
      }
  else
      {
      error = _statio2->validateFile(filename, info);
      resetFileType(_statio2->filetype());
      }
  if(error) return VALID_NOBUT;
  return VALID_YES;
}



//---------------pre and post new filename ------------------------//
//---------------pre and post new filename ------------------------//
//---------------pre and post new filename ------------------------//

    // protected virtual functions overriding FileBase.
    // called from base class FileBase (indirectly from SLFileChoice).

void StaticFileBase::preNewFilename()
{
  _manager->informer()->beginSlowOperations();
}


void StaticFileBase::postNewFilename()
{
  _manager->informer()->endSlowOperations();
}



//----------------- virtual prepare read -------------------------//
//----------------- virtual prepare read -------------------------//
//----------------- virtual prepare read -------------------------//

    // protected virtual function overriding FileBase.
    // this function does not change anything in StaticDataset.

FileBase::Prepare
StaticFileBase::virtualPrepareRead(const char* /*filename*/, char *errmsg)
{
  StaticInformer *informer = _manager->informer();

  if(_read_choice == READ_NOTHING)
      {
      strcpy(errmsg, "This static file has already been read.\n");
      strcat(errmsg, "Change your read choice if you want to read it again.");
      informer->showMessage("this static file has already been read");
      informer->ringBell();
      return PROHIBIT;
      }

  if(_read_choice == READ_ACTIVE)
      {
      StaticDataset *dataset = _manager->activeDataset();
      if(dataset->isLocked())
          {
          strcpy(errmsg, "static file cannot be input\nwhile data are locked");
          informer->showMessage
                       ("static file cannot be input while data are locked");
          informer->ringBell();
          return PROHIBIT;
          }
      }

  if(_read_choice == READ_REF)
      {
      StaticDataset *dataset = _manager->referenceDataset();
      if(dataset->isLocked())
          {
          strcpy(errmsg, "static file cannot be input\nwhile data are locked");
          informer->showMessage
                       ("static file cannot be input while data are locked");
          informer->ringBell();
          return PROHIBIT;
          }
      }

  int error = _statio->verifyParameters(errmsg);
  if(error)
      {
      informer->showMessage(errmsg);
      informer->ringBell();
      strcat (errmsg, "\n---\nMissing or incorrect information\n");
      strcat (errmsg, "must be supplied in the\n");
      strcat (errmsg, "\"Static File to Read\" area\n");
      strcat (errmsg, "or in the\n");
      strcat (errmsg, "\"How to Read the File\" area\n");
      strcat (errmsg, "before reading this file.");
      return PROHIBIT;
      }

  if(_read_choice == READ_NEW || _read_choice == READ_NEWACTIVE ||
     _read_choice == READ_NEWREF)
      {
      return GODSPEED;
      }

  StaticDataset *dataset = _manager->activeDataset();
  int nx     = dataset->getNx();
  int ny     = dataset->getNy();
  int ncards = dataset->history()->numHistoryCards();
  if(nx == 1 && ny == 1 && ncards <= 1) return GODSPEED;
  strcpy(errmsg, "The static dataset in memory\n");
  strcat(errmsg, "will disappear\nby being replaced from the file\n");
  strcat(errmsg, "if you continue with this action.\n----\n");
  if(dataset->dataNeedsSaving())
      {
      strcat(errmsg, "You have data which\nhas NOT been saved.\n----\n");
      }
  strcat(errmsg, "Are you sure you want\nto replace this data\n");
  strcat(errmsg, "while reading this file?");
  return CAUTION;
}



//----------------- virtual prepare save -------------------------//
//----------------- virtual prepare save -------------------------//
//----------------- virtual prepare save -------------------------//

    // protected virtual function overriding FileBase.
    // this function does not change anything in StaticDataset.

FileBase::Prepare
StaticFileBase::virtualPrepareSave(const char* /*filename*/, char *errmsg)
{
  StaticInformer *informer = _manager->informer();
  int error = _statio->verifyParameters(errmsg);
  if(error)
      {
      informer->showMessage(errmsg);
      informer->ringBell();
      strcat (errmsg, "\n---\nMissing or incorrect information\n");
      strcat (errmsg, "must be supplied in the\n");
      strcat (errmsg, "\"How to Write the File\" area\n");
      strcat (errmsg, "before saving this file.");
      return PROHIBIT;
      }
  return GODSPEED;
}



//--------------------- virtual read -----------------------------//
//--------------------- virtual read -----------------------------//
//--------------------- virtual read -----------------------------//

    // protected virtual function overriding FileBase.

FileBase::Result
StaticFileBase::virtualRead(const char *filename, char *errmsg)
{
  StaticDataset *dataset;
  switch(_read_choice)
      {
      case READ_ACTIVE   : dataset = _manager->activeDataset();
                           break;
      case READ_REF      : dataset = _manager->referenceDataset();
                           break;
      case READ_NEW      : dataset = _manager->appendNewDataset();
                           break;
      case READ_NEWACTIVE: dataset = _manager->appendNewActiveDataset();
                           break;
      case READ_NEWREF   : dataset = _manager->appendNewReferenceDataset();
                           break;
      case READ_NOTHING  : strcpy(errmsg,
                            "you have requested not to read the static file");
                           return FAILURE;
      default: assert(FALSE);
      }
  int error = dataset->readForeign(_doer, filename, _statio, errmsg);
  if(!error) _read_choice = READ_NOTHING;
  if(error) return FAILURE;
  return SUCCESS;
}



//--------------------- virtual save -----------------------------//
//--------------------- virtual save -----------------------------//
//--------------------- virtual save -----------------------------//

    // protected virtual function overriding FileBase.

FileBase::Result
StaticFileBase::virtualSave(const char *filename, char *errmsg)
{
  StaticDataset *dataset = _manager->activeDataset();
  int error = dataset->saveFile(filename, _statio, errmsg);
  if(error) return FAILURE;
  return SUCCESS;
}



//---------------------------- end --------------------------------------//
//---------------------------- end --------------------------------------//
//---------------------------- end --------------------------------------//
