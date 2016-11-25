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

//-------------------------- static_backup.cc ----------------------------//
//-------------------------- static_backup.cc ----------------------------//
//-------------------------- static_backup.cc ----------------------------//

//            implementation file for the StaticBackup class
//                 derived from the BackupBase class
//                         subdirectory stat


#include "stat/static_backup.hh"
#include "stat/static_kernal.hh"
/*
#include "oprim/pjar_wrapper.hh"
*/
#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <assert.h>
#include <limits.h>
#include <math.h>


//-------------------------- constructor ----------------------------//
//-------------------------- constructor ----------------------------//
//-------------------------- constructor ----------------------------//


StaticBackup::StaticBackup(StaticKernal *kernal)
           : BackupBase("static", "work"),
                 _kernal      (kernal)
{
  assert(_kernal);
}



//--------------------------- destructor --------------------------//
//--------------------------- destructor --------------------------//
//--------------------------- destructor --------------------------//

StaticBackup::~StaticBackup()
{
}



//-------------------- virtual save backup file ----------------------//
//-------------------- virtual save backup file ----------------------//
//-------------------- virtual save backup file ----------------------//

         // private.
         // returns error TRUE or FALSE.

int StaticBackup::virtualSaveBackupFile(const char *filename)
{
  char msg[80];
  return _kernal->saveFile(filename, msg, "binary");
}



//-------------------------- end -------------------------------//
//-------------------------- end -------------------------------//
//-------------------------- end -------------------------------//

