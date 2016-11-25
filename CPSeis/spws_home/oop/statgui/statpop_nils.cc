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

//--------------------------- statpop_nils.cc ---------------------------//
//--------------------------- statpop_nils.cc ---------------------------//
//--------------------------- statpop_nils.cc ---------------------------//

//          implementation file for the StatpopNils class
//                 derived from the SLDialog class
//                       subdirectory statgui


#include "statgui/statpop_nils.hh"
#include "statgui/statgui_nils.hh"
#include "statgui/statgui_status.hh"
#include "stat/static_manager.hh"
#include "sl/sl_smart_form.hh"
#include "sl/slp_push.hh"
#include <math.h>
#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <assert.h>



//-------------------- overriding virtual functions --------------------//
//-------------------- overriding virtual functions --------------------//
//-------------------- overriding virtual functions --------------------//

     // these private virtual functions override SLDialog.

void StatpopNils::postUnmanageNotify()
{
  _gui->cancelUndo();
}



//-------------------------- constructor -----------------------------//
//-------------------------- constructor -----------------------------//
//-------------------------- constructor -----------------------------//


StatpopNils::StatpopNils(SLDelay *slparent, char *name,
                          StaticManager    *manager,
                          ContainerList    *clist)
            : SLDialog(slparent, name, NULL, FALSE),
                        _manager      (manager),
                        _gui          (NULL)
{
  assert(_manager);

  SLSmartForm *work = workArea();
               _gui = new StatguiNils   (work, _manager);
  SLDelay   *status = new StatguiStatus (work, _manager, clist, FALSE);
 
                      addBottomRemove();
                      addBottomHelp("EDIT_STATIC_NILS");

               /////       LEFT     RIGHT    TOP      BOTTOM

  work->attach(status    , work  ,  work   , work   ,  NULL , 20, 20);
  work->attach(_gui      , work  ,  work   , status ,  work ,  0,  0,  10);

  update();
}



//------------------------------ destructor -------------------------//
//------------------------------ destructor -------------------------//
//------------------------------ destructor -------------------------//


StatpopNils::~StatpopNils()
{
}



//------------------------------- end -------------------------------//
//------------------------------- end -------------------------------//
//------------------------------- end -------------------------------//
