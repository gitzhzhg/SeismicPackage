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

//---------------------- statgui_watch.cc ------------------------//
//---------------------- statgui_watch.cc ------------------------//
//---------------------- statgui_watch.cc ------------------------//

//          implementation file for the StatguiWatch class
//               derived from the StaticInform class
//                 derived from the SLDelay class
//                      subdirectory statgui


#include "statgui/statgui_watch.hh"
#include "stat/static_manager.hh"
#include "stat/static_informer.hh"
#include "sl/sl_app.hh"
#include "sl/prim_support.hh"
#include "sl/shell_watch.hh"
#include "plot/pick_watch.hh"
#include <stdlib.h>
#include <stdio.h>
#include <iostream.h>
#include <string.h>
#include <assert.h>



//--------------------- constructor -------------------------------//
//--------------------- constructor -------------------------------//
//--------------------- constructor -------------------------------//


StatguiWatch::StatguiWatch(StaticManager *manager, SLApp *app)
         : StaticInform (manager),
           SLDelay      (app, "statgui_watch"),
                  _app     (app),
                  _watch1  (NULL),
                  _watch2  (NULL)
{
}



//----------------------- destructor ---------------------------//
//----------------------- destructor ---------------------------//
//----------------------- destructor ---------------------------//


StatguiWatch::~StatguiWatch()
{
}



//----------------- overriding virtual functions ---------------------//
//----------------- overriding virtual functions ---------------------//
//----------------- overriding virtual functions ---------------------//


void StatguiWatch::beginSlowOperations()
{
  if(!_watch1) _watch1 = new ShellWatch();
  if(!_watch2) _watch2 = new PickWatch();
}


void StatguiWatch::endSlowOperations()
{
  if(_watch1) delete _watch1;
  if(_watch2) delete _watch2;
  _watch1 = NULL;
  _watch2 = NULL;
}


void StatguiWatch::ringBell()
{
  XBell(_app->display(), 15);
}


void StatguiWatch::showMessage(const char *msg)
{
  _app->setMessage((char*)msg);
  XmUpdateDisplay(_app->W());              // to force all expose events.
}


void StatguiWatch::sendMessage(const char *code, const char *msg,
                               int, int, int, int, int)
{
  if(strcmp(code, "title") == 0) _app->setTitle((char*)msg, FALSE);
}


void StatguiWatch::update()
{
  manager()->informer()->returningToEventLoop();
  endSlowOperations();           // to make sure the watch gets turned off.
  _app->setMessage(" ");
}



//---------------------------- end --------------------------------------//
//---------------------------- end --------------------------------------//
//---------------------------- end --------------------------------------//
