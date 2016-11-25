
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
//---------------------- fg_watch_inform.cc ------------------------//
//---------------------- fg_watch_inform.cc ------------------------//
//---------------------- fg_watch_inform.cc ------------------------//

//         implementation file for the FgWatchInform class
//               derived from the FgInform class
//               derived from the SLDelay class
//                     subdirectory fggui


#include "fggui/fg_watch_inform.hh"
#include "geom/field_geometry.hh"
#include "sl/sl_app.hh"
#include "sl/prim_support.hh"
#include "sl/shell_watch.hh"
#include "plot/pick_watch.hh"
#include "cprim.h"
#include <stdlib.h>
#include <stdio.h>
#include <iostream.h>
#include <string.h>
#include <assert.h>



//--------------------- constructor -------------------------------//
//--------------------- constructor -------------------------------//
//--------------------- constructor -------------------------------//


FgWatchInform::FgWatchInform(FieldGeometry *fg, Widget w, SLApp *app)
         : FgInform (fg),
           SLDelay  (w, "fg_watch_inform"),
               _w       (w),
               _app     (app),
               _watch1  (NULL),
               _watch2  (NULL)
{
}



//----------------------- destructor ---------------------------//
//----------------------- destructor ---------------------------//
//----------------------- destructor ---------------------------//


FgWatchInform::~FgWatchInform()
{
}



//----------------- overriding virtual functions ---------------------//
//----------------- overriding virtual functions ---------------------//
//----------------- overriding virtual functions ---------------------//


void FgWatchInform::ringBell(FieldGeometry *)
{
  if(_w) XBell(XtDisplay(_w), 50);
}


void FgWatchInform::preSlowOperations(FieldGeometry *)
{
  if(!_watch1) _watch1 = new ShellWatch();
  if(!_watch2) _watch2 = new PickWatch();
}


void FgWatchInform::sendMessage(FieldGeometry *, char *msg,
                                     long, long, long, long, long)
{
  static char *prefix1 = "JD file last";
  static char *prefix2 = "survey file last";
  if(!_app) return;
  if(!strncmp(msg, prefix1, strlen(prefix1)) ||
     !strncmp(msg, prefix2, strlen(prefix2)))
                             _app->setTitle(msg, FALSE);
}


/*
void FgWatchInform::postSlowOperations(FieldGeometry *fg)
{
  finishedChanges(fg);
}


void FgWatchInform::finishedChanges(FieldGeometry *)
{
  if(_watch1) delete _watch1;
  if(_watch2) delete _watch2;
  _watch1 = NULL;
  _watch2 = NULL;
}
*/



void FgWatchInform::update()
{
  _fg->returningToEventLoop();
  if(_watch1) delete _watch1;
  if(_watch2) delete _watch2;
  _watch1 = NULL;
  _watch2 = NULL;
}



//---------------------------- end --------------------------------------//
//---------------------------- end --------------------------------------//
//---------------------------- end --------------------------------------//
