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

//-------------------------- vf_backup.cc ----------------------------//
//-------------------------- vf_backup.cc ----------------------------//
//-------------------------- vf_backup.cc ----------------------------//

//            implementation file for the VfBackup class
//                 derived from the BackupBase class
//                         subdirectory vf


#include "vf/vf_backup.hh"
#include "vf/vf_constants.hh"
#include "vf/vf_diskfile.hh"
#include "oprim/pjar_wrapper.hh"
#include "named_constants.h"
#include "c2f_interface.h"
#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <assert.h>
#include <limits.h>
#include <math.h>


//-------------------------- constructor ----------------------------//
//-------------------------- constructor ----------------------------//
//-------------------------- constructor ----------------------------//


VfBackup::VfBackup(VfKernal *kernal, HistoryCards *history)
           : BackupBase("velocity", "work"),
                 _kernal      (kernal),
                 _history     (history),
                 _diskfile    (NULL)
{
  assert(_kernal && _history);
  _diskfile = new VfDiskfile(1);
  _diskfile->initializeOutputParameters();
}



//--------------------------- destructor --------------------------//
//--------------------------- destructor --------------------------//
//--------------------------- destructor --------------------------//

VfBackup::~VfBackup()
{
  delete _diskfile;
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



//-------------------- virtual save backup file ----------------------//
//-------------------- virtual save backup file ----------------------//
//-------------------- virtual save backup file ----------------------//

         // private.
         // returns error TRUE or FALSE.

int VfBackup::virtualSaveBackupFile(const char *filename)
{
  INTEGER isw = 0;
  cio_set_cpsdisk_control_c(&isw);
  char msg[80];
  _diskfile->setEncoding("binary");
  _diskfile->setType(-1);
  return _diskfile->saveVelocityFile
                     (NULL, SAVE_ALL, _kernal, _history, filename, msg);
}



//-------------------------- end -------------------------------//
//-------------------------- end -------------------------------//
//-------------------------- end -------------------------------//

