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

//-------------------------- vf_undo.cc ----------------------------//
//-------------------------- vf_undo.cc ----------------------------//
//-------------------------- vf_undo.cc ----------------------------//

//            implementation file for the VfUndo class
//                 derived from the UndoBase class
//                         subdirectory vf


#include "vf/vf_undo.hh"
#include "vf/vf_kernal.hh"
#include "vf/vf_diskfile.hh"
#include "vf/vf_constants.hh"
#include "oprim/history_cards.hh"
#include "named_constants.h"
#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <assert.h>
#include <limits.h>
#include <math.h>

/******
static char *IDENTIFIER = NULL;      // use memory-resident file.
******/
static char *IDENTIFIER = "vf";      // use disk-resident file.


   // An experiment shows that disk-resident files are much faster to
   // save but slower to restore.  Statistics for 16,000 velocity
   // functions:
   //               1 second  to save to disk
   //              12 seconds to restore from disk
   //               4 seconds to save to memory
   //               6 seconds to restore from memory
   //
   // Theoretically, memory saves and restores should take the same time.
   // The above figures may not be entirely accurate.
   //
   // An advantage of disk-resident files is that the files are saved
   // very often but restored rarely.  A disadvantage of disk-resident
   // files is that they remain around in temporary space when the
   // program terminates abnormally.



//-------------------------- constructor ----------------------------//
//-------------------------- constructor ----------------------------//
//-------------------------- constructor ----------------------------//


VfUndo::VfUndo(VfKernal *kernal, HistoryCards *history)
           : UndoBase(IDENTIFIER),
                    _kernal      (kernal),
                    _history     (history),
                    _storage     (NULL),
                    _hstorage    (NULL),
                    _diskfile    (NULL)
{
  assert(_kernal && _history);
  if(IDENTIFIER)
      {
      _diskfile = new VfDiskfile(1);
      _diskfile->initializeOutputParameters();
      }
  else
      {
      _storage  = new VfKernal(_kernal->informer(), _kernal->utilities());
      _hstorage = new HistoryCards();
      }
}



//--------------------------- destructor --------------------------//
//--------------------------- destructor --------------------------//
//--------------------------- destructor --------------------------//

VfUndo::~VfUndo()
{
  if(_storage ) delete _storage;
  if(_hstorage) delete _hstorage;
  if(_diskfile) delete _diskfile;
}



//------------------------- prototype --------------------------------//
//------------------------- prototype --------------------------------//
//------------------------- prototype --------------------------------//


#if NEED_UNDERSCORE
#define cio_set_cpsdisk_control_c    cio_set_cpsdisk_control_c_
#elif NEED_CAPITALS
#define cio_set_cpsdisk_control_c    CIO_SET_CPSDISK_CONTROL_C
#endif


extern "C"
{
  void cio_set_cpsdisk_control_c(INTEGER *isw);
}



//-------------------- virtual save undo file ------------------------//
//-------------------- virtual save undo file ------------------------//
//-------------------- virtual save undo file ------------------------//

         // private.
         // returns error TRUE or FALSE.

int VfUndo::virtualSaveUndoFile(const char *filename)
{
  int error = FALSE;
  if(filename)
      {
      INTEGER isw = 0;
      cio_set_cpsdisk_control_c(&isw);
      char msg[80];
      _diskfile->initializeOutputParameters();
                     // the above is needed in case previous read had an error.
      _diskfile->setEncoding("binary");
      _diskfile->setType(-1);
      error = _diskfile->saveVelocityFile("remembering velocity function",
                         SAVE_ALL, _kernal, _history, filename, msg, TRUE);
      }
  else
      {
      _storage->deleteAllVelocityFunctions();
      _storage->appendVelocityFunctions("remembering velocity function",
                                               _kernal);
      _hstorage->copyHistoryCards(_history);
      }
  return error;
}



//-------------------- virtual read undo file ------------------------//
//-------------------- virtual read undo file ------------------------//
//-------------------- virtual read undo file ------------------------//

         // private.
         // returns error TRUE or FALSE.

int VfUndo::virtualReadUndoFile(const char *filename)
{
  int error = FALSE;
  long nkeep  = _kernal->numVelocityFunctions();
  long active = _kernal->getActiveVelocityFunction();
  _kernal->deleteAllVelocityFunctions();
  if(filename)
      {
      char msg[80];
      error = _diskfile->validateFile(filename, msg);
      if(!error)
      error = _diskfile->readVelocityFile("restoring velocity function",
                                          _kernal, _history, filename, msg);
      }
  else
      {
      _kernal->appendVelocityFunctions("restoring velocity function",
                                                _storage);
      _history->copyHistoryCards(_hstorage);
      }
  long nfun = _kernal->numVelocityFunctions();
  if(nfun == nkeep) _kernal->setActiveVelocityFunction(active);
  return error;
}



//--------------- notify removing undo file -------------------//
//--------------- notify removing undo file -------------------//
//--------------- notify removing undo file -------------------//

      // private.

void VfUndo::notifyRemovingUndoFile(const char *filename)
{
  if(filename == NULL)
      {
      _storage->deleteAllVelocityFunctions();
      }
}



//-------------------------- end -------------------------------//
//-------------------------- end -------------------------------//
//-------------------------- end -------------------------------//

