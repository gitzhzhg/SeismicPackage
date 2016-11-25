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

//---------------------- statpop_details.cc ----------------------//
//---------------------- statpop_details.cc ----------------------//
//---------------------- statpop_details.cc ----------------------//

//          implementation file for the StatpopDetails class
//                 derived from the SLDialog class
//                       subdirectory statgui


#include "statgui/statpop_details.hh"
#include "statgui/statgui_details.hh"
#include "statgui/statgui_header.hh"
#include "statgui/statgui_status.hh"
#include "sl/sl_smart_form.hh"
#include "sl/slp_push.hh"
#include <math.h>
#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <assert.h>



//-------------------- overriding virtual functions ---------------//
//-------------------- overriding virtual functions ---------------//
//-------------------- overriding virtual functions ---------------//

     // these private virtual functions override SLDialog.

void StatpopDetails::postUnmanageNotify()
{
  _gui->cancelUndo();
}



//--------------------- constructor ------------------------//
//--------------------- constructor ------------------------//
//--------------------- constructor ------------------------//


StatpopDetails::StatpopDetails(SLDelay *slparent, char *name,
                          StaticManager *manager,
                          ContainerList *clist)
            : SLDialog(slparent, name, NULL, FALSE)
{
  SLSmartForm     *work = workArea();
  StatguiHeader   *head = new StatguiHeader  (work, NULL, manager,
                                        "Active Dataset in Memory",
                                        StatguiHeader::EDITABLE_LARGE_PRINT,
                                        StatguiHeader::ACTIVE);
                   _gui = new StatguiDetails (work, manager);
  StatguiStatus *status = new StatguiStatus  (work, manager, clist, FALSE);
 
                  addBottomRemove();
                  addBottomHelp("STATIC_DETAILS");

               /////       LEFT     RIGHT    TOP      BOTTOM
  work->attach(status    , work  ,  work   , work   ,  NULL , 20, 20);
  work->attach(head      , work  ,  work   , status ,  NULL ,  0,  0, 10);
  work->attach(_gui      , work  ,  work   , head   ,  work ,  0,  0, 10);

  update();
}



//------------------------- destructor -------------------------//
//------------------------- destructor -------------------------//
//------------------------- destructor -------------------------//


StatpopDetails::~StatpopDetails()
{
}



//-------------------------- end -------------------------------//
//-------------------------- end -------------------------------//
//-------------------------- end -------------------------------//
