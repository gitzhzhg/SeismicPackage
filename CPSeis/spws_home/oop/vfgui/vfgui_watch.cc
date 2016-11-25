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

//---------------------- vfgui_watch.cc ------------------------//
//---------------------- vfgui_watch.cc ------------------------//
//---------------------- vfgui_watch.cc ------------------------//

//          implementation file for the VfguiWatch class
//                derived from the VfInform class
//                 derived from the SLDelay class
//                      subdirectory vfgui


#include "vfgui/vfgui_watch.hh"
#include "vf/vf_manager.hh"
#include "vf/vf_informer.hh"
#include "sl/sl_app.hh"
#include "sl/prim_support.hh"
#include "sl/shell_stat_msg.hh"
#include "sl/shell_watch.hh"
#include "plot/pick_watch.hh"
#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <assert.h>



//--------------------- constructor -------------------------------//
//--------------------- constructor -------------------------------//
//--------------------- constructor -------------------------------//


VfguiWatch::VfguiWatch(VfManager *manager, SLApp *app)
         : VfInform (manager),
           SLDelay      (app, "vfgui_watch"),
                  _app     (app),
                  _msg     (NULL),            // new 2/19/98.
                  _watch1  (NULL),
                  _watch2  (NULL)
{
}



//----------------------- destructor ---------------------------//
//----------------------- destructor ---------------------------//
//----------------------- destructor ---------------------------//


VfguiWatch::~VfguiWatch()
{
  if(_msg)    delete _msg;
  if(_watch1) delete _watch1;
  if(_watch2) delete _watch2;
}



//----------------- overriding virtual functions ---------------------//
//----------------- overriding virtual functions ---------------------//
//----------------- overriding virtual functions ---------------------//


void VfguiWatch::beginSlowOperations()
{
/********
  int beginning = (!_watch1 && !_watch2);
********/
  if(!_watch1) _watch1 = new ShellWatch();
  if(!_watch2) _watch2 = new PickWatch();
/********
  if(beginning) XmUpdateDisplay(_app->W());  //////// doesn't help (too soon?)
********/
  XmUpdateDisplay(_app->W());
}


void VfguiWatch::endSlowOperations()
{
  if(_watch1) delete _watch1;
  if(_watch2) delete _watch2;
  _watch1 = NULL;
  _watch2 = NULL;
  XmUpdateDisplay(_app->W());
}


void VfguiWatch::ringBell()
{
  XBell(_app->display(), 15);
  XmUpdateDisplay(_app->W());
}


void VfguiWatch::showMessage(const char *msg)
{
  if(_msg) _msg->changeCurrentMsg((char*)msg);                 // new 2/19/98.
  else     _msg = new ShellStatMsg(_app->W(), (char*)msg, FALSE); // new 2/19.
//_app->setMessage((char*)msg);                            // removed 2/19/98.
  XmUpdateDisplay(_app->W());                   // to force all expose events.
}


void VfguiWatch::sendMessage(const char *code, const char *msg,
                             long, long, long, long, long)
{
  if(strcmp(code, "title") == 0) _app->setTitle((char*)msg, FALSE);
  XmUpdateDisplay(_app->W());
}


void VfguiWatch::update()
{
////// no longer needed:
////// kept for diagnostic reasons (optional printouts in VfInform):
  manager()->informer()->returningToEventLoop();
/***************
***************/
  endSlowOperations();           // to make sure the watch gets turned off.
  if(_msg) delete _msg;                      // new 2/19/98.
  _msg = NULL;                               // new 2/19/98.
//_app->setMessage(" ");                 // removed 2/19/98.
}



//---------------------------- end --------------------------------------//
//---------------------------- end --------------------------------------//
//---------------------------- end --------------------------------------//
