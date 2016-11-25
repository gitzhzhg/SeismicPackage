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

//-------------------------- static_undo.cc ----------------------------//
//-------------------------- static_undo.cc ----------------------------//
//-------------------------- static_undo.cc ----------------------------//

//            implementation file for the StaticUndo class
//                 derived from the UndoBase class
//                         subdirectory stat


#include "stat/static_undo.hh"
#include "stat/static_kernal.hh"
#include "named_constants.h"
#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <assert.h>
#include <limits.h>
#include <math.h>



//-------------------------- constructor ----------------------------//
//-------------------------- constructor ----------------------------//
//-------------------------- constructor ----------------------------//


StaticUndo::StaticUndo(StaticKernal *kernal)
           : UndoBase("stat"),
                _kernal     (kernal)
{
  assert(_kernal);
}



//--------------------------- destructor --------------------------//
//--------------------------- destructor --------------------------//
//--------------------------- destructor --------------------------//

StaticUndo::~StaticUndo()
{
}



//-------------------- virtual save undo file ------------------------//
//-------------------- virtual save undo file ------------------------//
//-------------------- virtual save undo file ------------------------//

         // private.
         // returns error TRUE or FALSE.

int StaticUndo::virtualSaveUndoFile(const char *filename)
{
  char msg[80];
  return _kernal->saveFile(filename, msg, "binary", TRUE);
}



//-------------------- virtual read undo file ------------------------//
//-------------------- virtual read undo file ------------------------//
//-------------------- virtual read undo file ------------------------//

         // private.
         // returns error TRUE or FALSE.

int StaticUndo::virtualReadUndoFile(const char *filename)
{
  char msg[80];
  return _kernal->readFile(filename, msg);
}



//-------------------------- end -------------------------------//
//-------------------------- end -------------------------------//
//-------------------------- end -------------------------------//

