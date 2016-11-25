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

//---------------------- vf_horizon_file.cc ------------------------//
//---------------------- vf_horizon_file.cc ------------------------//
//---------------------- vf_horizon_file.cc ------------------------//

//           implementation file for the VfHorizonFile class
//                 derived from the FileBase class
//                         subdirectory vf


#include "vf/vf_horizon_file.hh"
#include "vf/vf_manager.hh"
#include "vf/vf_horizons.hh"
#include "vf/vf_informer.hh"
#include "vf/vf_horizonio.hh"
#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <assert.h>


#define NBUF  999
#define VALIDATE_ONLY_WHEN_CHANGED  TRUE

#define BELL             _manager->informer()->ringBell();
#define MESSAGE(msg)     _manager->informer()->showMessage(msg);
#define SLOW1            _manager->informer()->beginSlowOperations();
#define SLOW2            _manager->informer()->endSlowOperations();


//-------------------- constructor -------------------------------//
//-------------------- constructor -------------------------------//
//-------------------- constructor -------------------------------//


VfHorizonFile::VfHorizonFile(VfManager *manager, VfHorizons *horizons,
                             VfHorizonio *horizonio, Intent intent)
       : FileBase("horizon file", "dat", intent, VALIDATE_ONLY_WHEN_CHANGED),
             _manager      (manager),
             _horizons     (horizons),
             _horizonio    (horizonio)
{
  assert(_manager && _horizons && _horizonio);
}



//-------------------- destructor -------------------------------//
//-------------------- destructor -------------------------------//
//-------------------- destructor -------------------------------//


VfHorizonFile::~VfHorizonFile()
{
}



//-------------------- virtual validate -------------------------//
//-------------------- virtual validate -------------------------//
//-------------------- virtual validate -------------------------//

    // private.

FileBase::Validity
VfHorizonFile::virtualValidate(const char *filename, char *info)
{
  int io = ioIndex();
  if(io == 1) return VALID_NO;
  SLOW1

  char filetype[NBUF];
  strcpy(filetype, getFileType());

  int error = _horizons->doValidateInput(_horizonio, filename, filetype, info);

  resetFileType(filetype);

  SLOW2
  if(error) return VALID_NOBUT;
  return VALID_YES;
}



//--------------------- virtual prepare read ---------------------//
//--------------------- virtual prepare read ---------------------//
//--------------------- virtual prepare read ---------------------//


FileBase::Prepare
VfHorizonFile::virtualPrepareRead (const char* filename, char* msg)
{
  const char *filetype = getFileType();
  int error = _horizons->doPrepareImport(_horizonio, filename, filetype, msg);
/*
  int error = _horizonio->verifyParameters(msg);
*/
  if(error) return PROHIBIT;
  return GODSPEED;
}



//--------------------- virtual read -----------------------------//
//--------------------- virtual read -----------------------------//
//--------------------- virtual read -----------------------------//

    // private.

FileBase::Result
VfHorizonFile::virtualRead(const char *filename, char *msg)
{
  SLOW1
  sprintf(msg, "importing horizon file...");
  MESSAGE(msg)

  const char *filetype = getFileType();
  int error = _horizons->doImport(_horizonio, filename, filetype, msg);

  if(error)
      {
      BELL
      }
  else
      {
      strcpy(msg, "horizon file successfully imported");
      }

  MESSAGE(msg)
  SLOW2
  if(error) return FAILURE;
  return SUCCESS;
}



//---------------------------- end --------------------------------------//
//---------------------------- end --------------------------------------//
//---------------------------- end --------------------------------------//
