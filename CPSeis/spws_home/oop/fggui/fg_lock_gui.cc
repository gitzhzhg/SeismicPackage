
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
//---------------------- fg_lock_gui.cc ------------------------//
//---------------------- fg_lock_gui.cc ------------------------//
//---------------------- fg_lock_gui.cc ------------------------//

//           implementation file for the FgLockGui class
//                 derived from the SLpOption class
//                        subdirectory fggui


#include "fggui/fg_lock_gui.hh"
#include "geom/field_geometry.hh"
#include "geom/fg_constants.hh"
#include "cprim.h"
#include <stdlib.h>
#include <stdio.h>
#include <iostream.h>
#include <string.h>
#include <assert.h>



//------------------ static functions -------------------------//
//------------------ static functions -------------------------//
//------------------ static functions -------------------------//

static void lock_trap
         (void *data, long /*ident*/, long /*oldvar*/, long newvar)
{
  FieldGeometry *fg = (FieldGeometry*)data;
  fg->setDataLock((int)newvar);
}



static long lock_update(void *data)
{
  FieldGeometry *fg = (FieldGeometry*)data;
  return fg->getDataLock();
}


static long lock_sense_update(void * /*data*/)
{
  return TRUE;
}



//--------------------- constructor -------------------------------//
//--------------------- constructor -------------------------------//
//--------------------- constructor -------------------------------//


FgLockGui::FgLockGui(SLDelay *slparent, char *name, FieldGeometry *fg)
       : SLpOption(slparent, name, 0, "")
{
  assert(fg);
  addOption("allow changing any data"        , LOCK_NONE);
  addOption("prohibit data deletions"        , LOCK_DEL);
  addOption("lock changes to S gathers"      , LOCK_S);
  addOption("lock changes to S/R gathers"    , LOCK_S_R);
  addOption("lock changes to S/R/CMP gathers", LOCK_S_R_CMP);
  addOption("prohibit all changes to data"   , LOCK_ALL);

  setItrap      (lock_trap        , fg);
  setupIvarFun  (lock_update      , fg);
  setupSenseFun (lock_sense_update, fg);
}



//----------------------- destructor ---------------------------//
//----------------------- destructor ---------------------------//
//----------------------- destructor ---------------------------//


FgLockGui::~FgLockGui()
{
}



//---------------------------- end --------------------------------------//
//---------------------------- end --------------------------------------//
//---------------------------- end --------------------------------------//
