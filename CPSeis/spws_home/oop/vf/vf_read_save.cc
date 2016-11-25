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

//------------------------- vf_read_save.cc ---------------------------//
//------------------------- vf_read_save.cc ---------------------------//
//------------------------- vf_read_save.cc ---------------------------//

//           implementation file for the VfReadSave class
//                    not derived from any class
//                         subdirectory vf


#include "vf/vf_read_save.hh"
#include "vf/vf_diskfile.hh"
#include "vf/vf_informer.hh"
#include "vf/vf_kernal.hh"
#include "vf/vf_constants.hh"
#include "oprim/history_cards.hh"
#include "oprim/file_base.hh"
#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <assert.h>


#define NBUF 222


    // VfInformer is needed only for showMessage, ringBell,
    // begin/endSlowOperations, and before/afterChanges.


static const char *VALIDATING  = "validating velocity function";
static const char *READING     = "reading velocity function";
static const char *SAVING      = "saving velocity function";



//----------------------- constructor -------------------------------//
//----------------------- constructor -------------------------------//
//----------------------- constructor -------------------------------//


VfReadSave::VfReadSave(VfInformer *informer, VfUtilities *utilities,
                       int fill_kernal_while_validating, int io,
                       const char *defname)
          :
             _informer                      (informer),
             _diskfile                      (NULL),
             _storage                       (NULL),
             _hstorage                      (NULL),
             _fill_kernal_while_validating  (fill_kernal_while_validating),
             _read_choice                   (READ_REPLACE),
             _save_choice                   (SAVE_ALL)
{
  assert(_informer);
  _diskfile  = new VfDiskfile  (io, defname);
  _storage   = new VfKernal    (informer, utilities);
  _hstorage  = new HistoryCards();
  if(io == 1)
      {
      _diskfile->initializeOutputParameters();
 //   _diskfile->setEncoding("oldcps");            // removed 2003-09-23
      }
}



//----------------------- destructor -------------------------------//
//----------------------- destructor -------------------------------//
//----------------------- destructor -------------------------------//


VfReadSave::~VfReadSave()
{
  delete _diskfile;
  delete _storage;
  delete _hstorage;
}


//----------------------- get values -------------------------------//
//----------------------- get values -------------------------------//
//----------------------- get values -------------------------------//

      // public.

const char *VfReadSave::getFiletype()
       { return _diskfile->filetype(); }


                              ///////////////



float  VfReadSave::minimumXloc()  const
{ return _storage->minimumXloc(); }

float  VfReadSave::maximumXloc()  const
{ return _storage->maximumXloc(); }

float  VfReadSave::minimumYloc()  const
{ return _storage->minimumYloc(); }

float  VfReadSave::maximumYloc()  const
{ return _storage->maximumYloc(); }



float  VfReadSave::minimumAbscissa(int type)  const
{ return _storage->minimumAbscissa    (type); }

float  VfReadSave::maximumAbscissa(int type)  const
{ return _storage->maximumAbscissa    (type); }

float  VfReadSave::minimumOrdinate(int type)  const
{ return _storage->minimumOrdinate    (type); }

float  VfReadSave::maximumOrdinate(int type)  const
{ return _storage->maximumOrdinate    (type); }


                              ///////////////


int     VfReadSave::getType()  const
{ return _diskfile->getType(); }


                              ///////////////


int    VfReadSave::getNhx()  const
{
  if(_fill_kernal_while_validating) return _storage ->getNhx();
                                    return _diskfile->getNhx();
}


int    VfReadSave::getNhy()  const
{
  if(_fill_kernal_while_validating) return _storage ->getNhy();
                                    return _diskfile->getNhy();
}


int    VfReadSave::getMoveoutOrder()  const
{
  if(_fill_kernal_while_validating) return _storage ->getMoveoutOrder();
                                    return _diskfile->getMoveoutOrder();
}



//--------------------------- set values -------------------------------//
//--------------------------- set values -------------------------------//
//--------------------------- set values -------------------------------//

    // public.

void   VfReadSave::setReadChoice (int   value)
{
  assert(value == READ_REPLACE ||
         value == READ_ADD     ||
         value == READ_NEW     ||
         value == READ_NOTHING);
  _read_choice = value;
}



void   VfReadSave::setSaveChoice (int   value)
{
  assert(value == SAVE_SELECTED ||
         value == SAVE_ALL      ||
         value == SAVE_ACTIVE);
  _save_choice = value;
}



void VfReadSave::setType(int value)
    { _diskfile->setType(value); }



//-------------------- validate input file ------------------------------//
//-------------------- validate input file ------------------------------//
//-------------------- validate input file ------------------------------//

   // public.
   // called from a FileBase object.

int VfReadSave::validateInputFile(const char *filename, char *info)
{
  int error = _diskfile->validateFile(filename, info);

  if(_fill_kernal_while_validating)
      {
      char msg[111];
      _informer->beginSlowOperations();
      _informer->showMessage("validating velocity file...");
      _storage->clearEverything();
      _hstorage->deleteHistoryCards();
      if(!error)
          {
          error = _diskfile->readVelocityFile
                        (VALIDATING, _storage, _hstorage, filename, msg);
          if(error) strcpy(info, msg);
          }
      _informer->showMessage("velocity file validation completed");
      _informer->endSlowOperations();
      }
  return error;
}



//-------------------- validate output file ------------------------------//
//-------------------- validate output file ------------------------------//
//-------------------- validate output file ------------------------------//

   // public.
   // called from a FileBase object.

int VfReadSave::validateOutputFile(const char *filename, char *info)
{
  return _diskfile->validateFile(filename, info);
}



//--------------------- prepare read file -------------------------------//
//--------------------- prepare read file -------------------------------//
//--------------------- prepare read file -------------------------------//

   // public.
   // called from a FileBase object.

int VfReadSave::prepareReadFile(char *msg)
{
  assert(msg);
  if(_read_choice == READ_NOTHING)
      {
      strcpy(msg, "you have requested not to read the velocity file");
      return TRUE;
      }
  return _diskfile->prepareReadFile(msg);
}



//--------------------- prepare save file -------------------------------//
//--------------------- prepare save file -------------------------------//
//--------------------- prepare save file -------------------------------//

   // public.
   // called from a FileBase object.

int VfReadSave::prepareSaveFile(char *msg)
{
  assert(msg);
  return _diskfile->prepareSaveFile(msg);
}



//------------------------- read velocity file ---------------------------//
//------------------------- read velocity file ---------------------------//
//------------------------- read velocity file ---------------------------//

   // public.
   // called from VfDataset.

int VfReadSave::readVelocityFile (const char *filename, char *msg,
                                  VfKernal *kernal,
                                  HistoryCards *history, int *replaced_all)
{
  assert(filename && msg && kernal && history && replaced_all);
  assert(_read_choice == READ_REPLACE || _read_choice == READ_ADD ||
         _read_choice == READ_NEW);

  //////////

  sprintf(msg, "reading %s...", getFiletype());
  _informer->beforeChanges();
  _informer->showMessage(msg);

  //////////

  long nfun = kernal->numVelocityFunctions();

  *replaced_all = (nfun == 0 || _read_choice == READ_REPLACE);

  if(_read_choice == READ_REPLACE)
      {
      kernal->clearEverything();
      history->deleteHistoryCards();
      }

  //////////

  int error;
  if(_fill_kernal_while_validating)
      {
      kernal->appendVelocityFunctions(READING, _storage);
      history->addHistoryCards(_hstorage);
      sprintf(msg, "%s successfully read", getFiletype());
      error = FALSE;
      }
  else
      {
      error = _diskfile->readVelocityFile
                              (READING, kernal, history, filename, msg);
      }

  //////////

  if(error) _informer->ringBell();
  _informer->showMessage(msg);
  _informer->afterChanges();
  return error;
}



//------------------------- save velocity file ---------------------------//
//------------------------- save velocity file ---------------------------//
//------------------------- save velocity file ---------------------------//

   // public.
   // called from VfDataset.

int VfReadSave::saveVelocityFile (const char *filename, char *msg,
                                  VfKernal *kernal,
                                  HistoryCards *history, int *saved_all)
{
  assert(filename && msg && kernal && history && saved_all);
  assert(_save_choice == SAVE_ALL || _save_choice == SAVE_SELECTED ||
         _save_choice == SAVE_ACTIVE);

  //////////

  sprintf(msg, "saving %s...", getFiletype());
  _informer->beginSlowOperations();
  _informer->showMessage(msg);

  //////////

  long nfun = kernal->numVelocityFunctions();
  long nsel = kernal->numSelectedVelocityFunctions();

  *saved_all = (_save_choice == SAVE_ALL ||
               (_save_choice == SAVE_SELECTED && nfun == nsel) ||
               (_save_choice == SAVE_ACTIVE && nfun <= 1));

  //////////

  int error = _diskfile->saveVelocityFile (SAVING, _save_choice,
                                           kernal, history, filename, msg);

  //////////

  if(error) _informer->ringBell();
  _informer->showMessage(msg);
  _informer->endSlowOperations();
  return error;
}



//---------------------------- end --------------------------------------//
//---------------------------- end --------------------------------------//
//---------------------------- end --------------------------------------//
