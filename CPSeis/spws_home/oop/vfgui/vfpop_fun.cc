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

//---------------------- vfpop_fun.cc ------------------------------//
//---------------------- vfpop_fun.cc ------------------------------//
//---------------------- vfpop_fun.cc ------------------------------//

//         implementation file for the VfpopFun class
//                 derived from the SLDialog class
//                      subdirectory vfgui


#include "vfgui/vfpop_fun.hh"
#include "vfgui/vfbox_fun.hh"
#include "vfgui/vfgui_status.hh"
#include "vf/vf_manager.hh"
#include "vf/vf_dataset.hh"
#include "vf/vf_edit_sort.hh"
#include "sl/sl_smart_form.hh"
#include "sl/slp_push.hh"
#include <stream.h>
#include <assert.h>

#define SORT 33

//--------------------- activate notify -------------------------//
//--------------------- activate notify -------------------------//
//--------------------- activate notify -------------------------//

       // private.

void VfpopFun::activateNotify(long ident)
{
  if(ident == SORT) _manager->activeDataset()->editDataset(_edit, NULL);
}



//--------------------- constructor ------------------------//
//--------------------- constructor ------------------------//
//--------------------- constructor ------------------------//


VfpopFun::VfpopFun(SLDelay *slparent, char *name,
                                VfManager     *manager,
                                ContainerList *clist)
            : SLDialog(slparent, name, NULL, FALSE),
                  _manager   (manager),
                  _edit      (NULL)
{
  assert(manager);
  SLSmartForm  *work  = workArea();

                _edit = new VfEditSort   ();
  VfguiStatus *status = new VfguiStatus  (work, manager, clist);
  VfboxFun    *table  = new VfboxFun     (work, "fun_table", manager, _edit);

  work->attach(status , work, work, work  , NULL, 20, 20);
  work->attach(table  , work, work, status, work,  0,  0,  10);

  addBottomPush(" Sort ", SORT);
  addBottomRemove();
  addBottomKeyhelp();
  addBottomHelp("FUN_OVERVIEW");

  update();
}



//------------------------- destructor -------------------------//
//------------------------- destructor -------------------------//
//------------------------- destructor -------------------------//


VfpopFun::~VfpopFun()
{
  delete _edit;
}



//-------------------------- end -------------------------------//
//-------------------------- end -------------------------------//
//-------------------------- end -------------------------------//
