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

//-------------------------- undo_base.cc ----------------------------//
//-------------------------- undo_base.cc ----------------------------//
//-------------------------- undo_base.cc ----------------------------//

//            implementation file for the VfArray class
//                    not derived from any class
//                        subdirectory oprim


#include "oprim/undo_base.hh"
#include "cprim.h"
#include <iostream.h>
#include <unistd.h>
#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <assert.h>
#include <limits.h>
#include <math.h>


//-------------------------- constructor ----------------------------//
//-------------------------- constructor ----------------------------//
//-------------------------- constructor ----------------------------//


UndoBase::UndoBase(const char *identifier)
           :
        _filename     (NULL),
        _doer         (NULL)
{
  if(identifier)
      {
      const char *buffer = tmpnam(NULL);
      const char *user   = getlogin();
      assert(buffer);
      assert(buffer[0] != '\0');
      if(user == NULL)
          {
          /////// the intel solaris getlogin returns NULL.
          _filename = newstrcat((char*)buffer, "_", (char*)identifier,
                                               ".junk", NULL);
          }
      else
          {
          _filename = newstrcat((char*)buffer, "_", (char*)identifier,
                                               ".", (char*)user, NULL);
          }
      assert(_filename);
      assert(_filename[0] != '\0');
      }
}



//--------------------------- destructor --------------------------//
//--------------------------- destructor --------------------------//
//--------------------------- destructor --------------------------//

UndoBase::~UndoBase()
{
  if(_filename) remove(_filename);
  if(_filename) free  (_filename);
}



//-------------------- interact with undo file -----------------------//
//-------------------- interact with undo file -----------------------//
//-------------------- interact with undo file -----------------------//

         // public.

int UndoBase::saveUndoFile(void *doer, char *msg)
{
  assert(doer);
  assert(msg);
  notifyRemovingUndoFile(_filename);
  if(_filename) remove(_filename);
  _doer = NULL;
  int error = virtualSaveUndoFile(_filename);
  if(error)
     {
     strcpy(msg, "ERRORS trying to save data to UNDO file");
     notifyRemovingUndoFile(_filename);
     if(_filename) remove(_filename);
     _doer = NULL;
     }
  else
     {
     strcpy(msg, "data successfully saved to UNDO file");
     _doer = doer;
     }
  return error;
}



int UndoBase::allowReadDeleteUndoFile(void *doer)  const
{
  assert(doer);
  return (doer == _doer);
}



void UndoBase::maybeDeleteUndoFile(void *doer)
{
  assert(doer);
  if(doer == _doer)
     {
     notifyRemovingUndoFile(_filename);
     if(_filename) remove(_filename);
     _doer = NULL;
     }
}



int UndoBase::maybeReadUndoFile(void *doer, char *msg)
{
  assert(doer);
  assert(msg);
  if(_doer == NULL)
      {
      strcpy(msg, "data CANNOT BE RESTORED from nonexistent UNDO file");
      return TRUE;
      }
  if(doer != _doer)
      {
      strcpy(msg, "data CANNOT BE RESTORED from UNDO file");
      return TRUE;
      }
  int error = virtualReadUndoFile(_filename);
  if(error)
      {
      strcpy(msg, "data RESTORED WITH ERRORS from UNDO file");
      }
  else
      {
      strcpy(msg, "data successfully restored from UNDO file");
      }
  notifyRemovingUndoFile(_filename);
  if(_filename) remove(_filename);
  _doer = NULL;
  return error;
}



//-------------------------- end -------------------------------//
//-------------------------- end -------------------------------//
//-------------------------- end -------------------------------//

