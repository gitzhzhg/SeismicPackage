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

//---------------------- vf_file_base.cc ------------------------//
//---------------------- vf_file_base.cc ------------------------//
//---------------------- vf_file_base.cc ------------------------//

//           implementation file for the VfFileBase class
//                 derived from the FileBase class
//                         subdirectory vf


#include "vf/vf_file_base.hh"
#include "vf/vf_read_save.hh"
#include "vf/vf_manager.hh"
#include "vf/vf_dataset.hh"
#include "vf/vf_informer.hh"
#include "vf/vf_constants.hh"
#include "vf/vf_utilities.hh"
#include "vf/vf_edit_sort.hh"
#include "vf/vf_edit_names.hh"
#include "vf/velio_wrapper.hh"
#include "str.h"
#include "named_constants.h"
#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <assert.h>

#define NBUF  222
#define VALIDATE_ONLY_WHEN_CHANGED  TRUE


//-------------------- constructor -------------------------------//
//-------------------- constructor -------------------------------//
//-------------------- constructor -------------------------------//


VfFileBase::VfFileBase(const char *filetype, const char *extension,
                       VfManager *manager, Intent intent,
                       int fill_kernal_when_validating)
       : FileBase(filetype, extension, intent, VALIDATE_ONLY_WHEN_CHANGED),
             _manager             (manager),
             _readsave            (NULL),
             _velio2              (NULL),
             _edit_sort           (NULL),
             _edit_names          (NULL),
             _sort_choice         (TRUE),
             _names_choice        (TRUE)
{
  assert(_manager);
  int io = 0;
  if(intent == USE_FOR_OUTPUT) io = 1;
  _readsave   = new VfReadSave   (_manager->informer(), _manager->utilities(),
                                  fill_kernal_when_validating, io);
  _velio2     = new VelioWrapper (io, "velocity");
  _edit_sort  = new VfEditSort   ();
  _edit_names = new VfEditNames  ();
  _edit_names->setChoice(VfEditBase::CHOICE_BLANK);
}



//-------------------- destructor -------------------------------//
//-------------------- destructor -------------------------------//
//-------------------- destructor -------------------------------//


VfFileBase::~VfFileBase()
{
  delete _readsave;
  delete _velio2;
  delete _edit_sort;
  delete _edit_names;
}



//----------------------- get values -------------------------------//
//----------------------- get values -------------------------------//
//----------------------- get values -------------------------------//

      // public.

int     VfFileBase::getReadChoice()  const
{ return _readsave->getReadChoice(); }

int     VfFileBase::getSaveChoice()  const
{ return _readsave->getSaveChoice(); }


int         VfFileBase::getType()  const
    { return _readsave->getType(); }

int     VfFileBase::getNhx()  const
{ return _readsave->getNhx(); }

int     VfFileBase::getNhy()  const
{ return _readsave->getNhy(); }

int     VfFileBase::getMoveoutOrder()  const
{ return _readsave->getMoveoutOrder(); }


float   VfFileBase::minimumXloc()  const
{ return _readsave->minimumXloc(); }

float   VfFileBase::maximumXloc()  const
{ return _readsave->maximumXloc(); }

float   VfFileBase::minimumYloc()  const
{ return _readsave->minimumYloc(); }

float   VfFileBase::maximumYloc()  const
{ return _readsave->maximumYloc(); }



float   VfFileBase::minimumAbscissa(int type)  const
{ return _readsave->minimumAbscissa    (type); }

float   VfFileBase::maximumAbscissa(int type)  const
{ return _readsave->maximumAbscissa    (type); }

float   VfFileBase::minimumOrdinate(int type)  const
{ return _readsave->minimumOrdinate    (type); }

float   VfFileBase::maximumOrdinate(int type)  const
{ return _readsave->maximumOrdinate    (type); }



//----------------------- set values -------------------------------//
//----------------------- set values -------------------------------//
//----------------------- set values -------------------------------//

      // public.

void VfFileBase::setReadChoice(int value)
    { _readsave->setReadChoice    (value); }

void VfFileBase::setSaveChoice(int value)
    { _readsave->setSaveChoice    (value); }


void VfFileBase::setType(int value)
    { _readsave->setType    (value); }



//----------------- virtual validate input and output ---------------------//
//----------------- virtual validate input and output ---------------------//
//----------------- virtual validate input and output ---------------------//

    // protected virtual functions overriding FileBase.
    // called from base class FileBase (indirectly from SLFileChoice).
    // sets info to a string of information about the file (if valid).
    // these functions do not change anything in VfManager or VfDataset.

FileBase::Validity
VfFileBase::virtualValidateInput
                     (const char *filename, char *info, What /*what*/)
{
  int error = _readsave->validateInputFile (filename, info);
  if(error && _readsave->fillKernalWhileValidating())
      {
      resetFileType("velocity file");
      return VALID_NO;
      }
  resetFileType(_readsave->getFiletype());
  return (error ? VALID_NOBUT : VALID_YES);
}


FileBase::Validity
VfFileBase::virtualValidateOutput
                     (const char *filename, char *info, What /*what*/)
{
  int error = _velio2->validateFile(filename, info);
  resetFileType(_velio2->filetype());
  return (error ? VALID_NOBUT : VALID_YES);
}



//---------------pre and post new filename ------------------------//
//---------------pre and post new filename ------------------------//
//---------------pre and post new filename ------------------------//

    // protected virtual functions overriding FileBase.
    // called from base class FileBase (indirectly from SLFileChoice).

static char *temporary = NULL;

void VfFileBase::preNewFilename()
{
  assert(!temporary);
  temporary = str_newstr(inputFilename());
  _manager->informer()->beginSlowOperations();
}


void VfFileBase::postNewFilename()
{
  assert(temporary);
  if(_readsave->getReadChoice() == READ_NOTHING &&
     strcmp(inputFilename(), temporary) != 0)
      {
      _readsave->setReadChoice(READ_REPLACE);
      }
  free(temporary);
  temporary = NULL;
  _manager->informer()->endSlowOperations();
}



//----------------- virtual prepare read -------------------------//
//----------------- virtual prepare read -------------------------//
//----------------- virtual prepare read -------------------------//

    // protected virtual function overriding FileBase.
    // called from base class FileBase (indirectly from SLFileChoice).
    // this function accesses the active dataset.
    // this function does not change anything in VfManager or VfDataset.

FileBase::Prepare
VfFileBase::virtualPrepareRead(const char* /*filename*/, char *msg)
{ 
  int error = _readsave->prepareReadFile(msg);
  if(error) return PROHIBIT;

  int read_choice = _readsave->getReadChoice();
  if(read_choice == READ_NEW)
      {
      return GODSPEED;
      }

  if(_manager->activeDataset()->isLocked())
      {
      strcpy(msg, "velocity file cannot be input\nwhile dataset is locked");
      _manager->informer()->showMessage
                  ("velocity file cannot be input while dataset is locked");
      _manager->informer()->ringBell();
      return PROHIBIT;
      }

  long nfun = _manager->activeDataset()->numVelocityFunctions();
  if(nfun == 0) return GODSPEED;

  if(read_choice == READ_ADD)
      {
      if(_readsave->getNhx         () != _manager->activeDataset()->getNhx() ||
         _readsave->getNhy         () != _manager->activeDataset()->getNhy() ||
         _readsave->getMoveoutOrder() != _manager->activeDataset()->
                                                         getMoveoutOrder())
          {
          strcpy(msg, "velocity file cannot be input\n");
          strcat(msg, "and added to data in memory\n");
          strcat(msg, "since header words and/or moveout order\n");
          strcat(msg, "do not match");
          _manager->informer()->showMessage
                     ("velocity file has mismatching header words or order");
          _manager->informer()->ringBell();
          return PROHIBIT;
          }
      return GODSPEED;
      }

  strcpy(msg, "The active velocity dataset in memory\n");
  strcat(msg, "will disappear\nby being replaced from the file\n");
  strcat(msg, "if you continue with this action.\n----\n");
  if(_manager->activeDataset()->dataNeedsSaving())
      {
      strcat(msg, "You have data which\nhas NOT been saved.\n----\n");
      }
  strcat(msg, "Are you sure you want\nto replace this data\n");
  strcat(msg, "while reading this file?");
  return CAUTION;
}



//----------------- virtual prepare save -------------------------//
//----------------- virtual prepare save -------------------------//
//----------------- virtual prepare save -------------------------//

    // protected virtual function overriding FileBase.
    // called from base class FileBase (indirectly from SLFileChoice).
    // this function accesses the active dataset.
    // this function does not change anything in VfManager or VfDataset.

FileBase::Prepare
VfFileBase::virtualPrepareSave(const char* filename, char *msg)
{
  int error = _readsave->prepareSaveFile(msg);
  if(error)
      {
      _manager->informer()->showMessage(msg);
      _manager->informer()->ringBell();
      return PROHIBIT;
      }
  if(!_manager->activeDataset()->isEditable())
      {
      strcpy(msg, "there is no need to save uneditable dataset");
      _manager->informer()->showMessage(msg);
      _manager->informer()->ringBell();
      return PROHIBIT;
      }
  if(_manager->activeDataset()->numVelocityFunctionsWithErrors() > 0)
      {
      sprintf(msg, "warning: %d velocity functions have errors\n",
              _manager->activeDataset()->numVelocityFunctionsWithErrors());
      strcat(msg, "(either nil picks, or zero or one pick only).\n");
      int save_type = _readsave->getType();
      long nerr = _manager->activeDataset()->
                          numVelocityFunctionsWithTypeErrors(save_type);
      char phrase[88];
      sprintf(phrase,
           "(%d of these are of the type you have elected to save)\n", nerr);
      strcat(msg, phrase);
      strcat(msg, "----------\n");
      strcat(msg, "Information may be lost in this circumstance.\n");
      strcat(msg, "You should fix the errors before saving the file.");
/*
      strcat(msg, "You should fix the errors before saving the file.\n");
      strcat(msg, "----------\n");
      if(outputStatus() == OUTPUT_OVERWRITE)
          { 
          strcat(msg, "The file you are about to save\n");
          strcat(msg, "will overwrite this existing file:\n");
          strcat(msg, "----\n");
          strcat(msg, filename);
          strcat(msg, "\n----\nAre you sure you want\n");
          strcat(msg, "to save the file\nat this time?");
          }
      else
          {
          strcat(msg, "Are you sure you want\n");
          strcat(msg, "to save the file\nat this time?");
          }
*/
      return CAUTION;
      }
/*
  if(outputStatus() == OUTPUT_OVERWRITE) return CAUTION;
*/
  return GODSPEED;
}



//--------------------- virtual read -----------------------------//
//--------------------- virtual read -----------------------------//
//--------------------- virtual read -----------------------------//

    // protected virtual function overriding FileBase.
    // called from base class FileBase (indirectly from SLFileChoice).
    // this function accesses the active dataset or a new dataset.

FileBase::Result
VfFileBase::virtualRead(const char *filename, char *msg)
{
  int error = _readsave->prepareReadFile(msg);
  if(error) return FAILURE;

  int read_choice = _readsave->getReadChoice();
  if(read_choice == READ_NOTHING)
      {
      assert(FALSE);
      }
  else if(read_choice == READ_NEW)
      {
      _manager->informer()->beforeChanges();
      long index1 = _manager->numDatasets();
      _manager->appendNewComparisonDataset();
      long index2 = _manager->numDatasets();
      if(index2 != index1 + 1)
          {
          strcpy(msg, "cannot create new uneditable dataset");
          error = TRUE;
          }
      else
          {
          error = _manager->dataset(index1)->readVelocityFile
                                              (filename, msg, _readsave, this);
          _manager->setReferenceDatasetIndex(_manager->numDatasets()-1);
          }
      _manager->informer()->afterChanges();
      }
  else
      {
      error = _manager->activeDataset()->readVelocityFile
                                              (filename, msg, _readsave, this);
      }
  _readsave->setReadChoice(READ_NOTHING);
  if(error) return FAILURE;
  return SUCCESS;
}



//--------------------- virtual save -----------------------------//
//--------------------- virtual save -----------------------------//
//--------------------- virtual save -----------------------------//

    // protected virtual function overriding FileBase.
    // called from base class FileBase (indirectly from SLFileChoice).
    // this function accesses the active dataset.

FileBase::Result
VfFileBase::virtualSave(const char *filename, char *msg)
{
  int error = _readsave->prepareSaveFile(msg);
  if(error) return FAILURE;

  _manager->informer()->beforeChanges();
  if(_sort_choice)
      {
      int xdir, ydir;
      int sorted = _manager->activeDataset()->checkSort(&xdir, &ydir);
      if(!sorted) _manager->activeDataset()->editDataset(_edit_sort, NULL);
      }
  if(_names_choice)
      {
      long nblank =
             _manager->activeDataset()->numVelocityFunctionsWithBlankNames();
      if(nblank > 0) _manager->activeDataset()->editDataset(_edit_names, NULL);
      }
  _manager->informer()->afterChanges();

  error = _manager->activeDataset()->saveVelocityFile
                                              (filename, msg, _readsave);
  if(error) return FAILURE;
  return SUCCESS;
}



//---------------------------- end --------------------------------------//
//---------------------------- end --------------------------------------//
//---------------------------- end --------------------------------------//
