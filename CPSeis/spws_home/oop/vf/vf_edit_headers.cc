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

//-------------------------- vf_edit_headers.cc ---------------------------//
//-------------------------- vf_edit_headers.cc ---------------------------//
//-------------------------- vf_edit_headers.cc ---------------------------//

//            implementation file for the VfEditHeaders class
//                  derived from the VfEditBase class
//                         subdirectory vf


#include "vf/vf_edit_headers.hh"
#include "vf/vf_kernal.hh"
#include "vf/vf_function.hh"
#include "str.h"
#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <assert.h>
#include <limits.h>
#include <math.h>


//------------------ constructor and destructor ---------------------//
//------------------ constructor and destructor ---------------------//
//------------------ constructor and destructor ---------------------//


VfEditHeaders::VfEditHeaders()
           : VfEditBase(INFORM_STRINGS, "edit headers"),
             _default_project     (str_newstr("none")),
             _default_line        (str_newstr("none")),
             _default_rdate       (str_newstr("none")),
             _default_pdate       (str_newstr("none")),
             _default_userid      (str_newstr("non")),
             _default_comment     (str_newstr("none")),
             _edit_project        (FALSE),
             _edit_line           (FALSE),
             _edit_rdate          (FALSE),
             _edit_pdate          (FALSE),
             _edit_userid         (FALSE),
             _edit_comment        (FALSE),
             _set_comment_to_vfid (FALSE)
{
}



VfEditHeaders::~VfEditHeaders()
{
  free(_default_project);
  free(_default_line);
  free(_default_rdate);
  free(_default_pdate);
  free(_default_userid);
  free(_default_comment);
}



//-------------------------- set values ----------------------------//
//-------------------------- set values ----------------------------//
//-------------------------- set values ----------------------------//


void   VfEditHeaders::setDefaultProject (const char *value)
{
  assert(value);
  free(_default_project);
  _default_project = str_newstr(value);
}

void   VfEditHeaders::setDefaultLine    (const char *value)
{
  assert(value);
  free(_default_line);
  _default_line = str_newstr(value);
}

void   VfEditHeaders::setDefaultRdate   (const char *value)
{
  assert(value);
  free(_default_rdate);
  _default_rdate = str_newstr(value);
}

void   VfEditHeaders::setDefaultPdate   (const char *value)
{
  assert(value);
  free(_default_pdate);
  _default_pdate = str_newstr(value);
}

void   VfEditHeaders::setDefaultUserid  (const char *value)
{
  assert(value);
  free(_default_userid);
  _default_userid = str_newstr(value);
}

void   VfEditHeaders::setDefaultComment (const char *value)
{
  assert(value);
  free(_default_comment);
  _default_comment = str_newstr(value);
}



//---------------------- virtual check -------------------------------//
//---------------------- virtual check -------------------------------//
//---------------------- virtual check -------------------------------//

        // public virtual function overriding VfEditBase.

int VfEditHeaders::virtualCheck (VfKernal* /*kernal*/, char *msg)
{
  if(_edit_project        == FALSE &&
     _edit_line           == FALSE &&
     _edit_rdate          == FALSE &&
     _edit_pdate          == FALSE &&
     _edit_userid         == FALSE &&
     _edit_comment        == FALSE &&
     _set_comment_to_vfid == FALSE)
      {
      strcpy(msg, "no headers chosen for editing");
      return TRUE;
      }
  strcpy(msg, "editing velocity function headers...");
  return FALSE;
}



//----------------------- virtual edit ----------------------------------//
//----------------------- virtual edit ----------------------------------//
//----------------------- virtual edit ----------------------------------//

        // public virtual function overriding VfEditBase.

int VfEditHeaders::virtualEdit (VfKernal *kernal, char *msg)
{
  long nfun = kernal->numVelocityFunctions();
  long kount = 0;
  for(long ifun = 0; ifun < nfun; ifun++)
      {
      if(reject(kernal, ifun, NULL)) continue;
      VfFunction *velfun = kernal->velfun(ifun);
      if(_edit_project)        velfun->setProject(_default_project);
      if(_edit_line   )        velfun->setLine   (_default_line   );
      if(_edit_rdate  )        velfun->setRdate  (_default_rdate  );
      if(_edit_pdate  )        velfun->setPdate  (_default_pdate  );
      if(_edit_userid )        velfun->setUserid (_default_userid );
      if(_edit_comment)        velfun->setComment(_default_comment);
      if(_set_comment_to_vfid) velfun->setComment(velfun->getVfid());
      kount++;
      }
  sprintf(msg, "headers edited on %d velocity functions", kount);
  return FALSE;
}



//-------------------------- end -------------------------------//
//-------------------------- end -------------------------------//
//-------------------------- end -------------------------------//

