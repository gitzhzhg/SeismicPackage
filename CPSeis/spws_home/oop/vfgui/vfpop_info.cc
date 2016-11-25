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

//---------------------- vfpop_info.cc ------------------------------//
//---------------------- vfpop_info.cc ------------------------------//
//---------------------- vfpop_info.cc ------------------------------//

//         implementation file for the VfpopInfo class
//                 derived from the SLDialog class
//                      subdirectory vfgui


#include "vfgui/vfpop_info.hh"
#include "vfgui/vfbox_info.hh"
#include "vfgui/vfgui_status.hh"
#include "vf/vf_manager.hh"
#include "vf/vf_dataset.hh"
#include "sl/sl_smart_form.hh"
#include "sl/slp_push.hh"
#include <stream.h>
#include <assert.h>


//--------------------- constructor ------------------------//
//--------------------- constructor ------------------------//
//--------------------- constructor ------------------------//


VfpopInfo::VfpopInfo(SLDelay *slparent, char *name,
                                VfManager     *manager,
                                ContainerList *clist)
            : SLDialog(slparent, name, NULL, FALSE),
                  _manager   (manager)
{
  assert(manager);
  SLSmartForm  *work  = workArea();

  VfguiStatus *status = new VfguiStatus  (work, manager, clist);
              _table  = new VfboxInfo    (work, "info_table", manager);

  work->attach(status , work, work, work  , NULL, 20, 20);
  work->attach(_table , work, work, status, work,  0,  0,  10);

  addBottomRemove();
  addBottomKeyhelp();
  addBottomHelp("INFO_OVERVIEW");

  update();
}



//------------------------- destructor -------------------------//
//------------------------- destructor -------------------------//
//------------------------- destructor -------------------------//


VfpopInfo::~VfpopInfo()
{
}



//------------------------- set plots ------------------------------//
//------------------------- set plots ------------------------------//
//------------------------- set plots ------------------------------//

     // public.

void VfpopInfo::setVaSemblancePlot (VaSemblancePlot *splot)
{
  _table->setVaSemblancePlot(splot);
}


void VfpopInfo::setVaCmpPlot       (VaCmpPlot       *cplot)
{
  _table->setVaCmpPlot(cplot);
}



//-------------------------- end -------------------------------//
//-------------------------- end -------------------------------//
//-------------------------- end -------------------------------//
