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

//------------------- pick_gui_pop.cc ----------------------//
//------------------- pick_gui_pop.cc ----------------------//
//------------------- pick_gui_pop.cc ----------------------//

//         implementation file for the PickGuiPop class
//                 derived from the SLDialog class


#include <X11/Intrinsic.h>
#include "pick_gui_pop.hh"
#include "pick_gui.hh"
#include "sl/slp_push.hh"
#include "wproc.h"
#include "cprim.h"

//------------------- constructors and destructor -----------------//
//------------------- constructors and destructor -----------------//
//------------------- constructors and destructor -----------------//

PickGuiPop::PickGuiPop(SLDelay *slparent, char *name, HelpCtx Hctx,
         void *cvmapp)
 : SLDialog(slparent, name, Hctx,True)
{ 
 setup(Hctx,cvmapp);

}

PickGuiPop::PickGuiPop(Widget parent, char *name, HelpCtx Hctx,
         void *cvmapp)
 : SLDialog(parent, name, Hctx,True)
{ 
 setup(Hctx,cvmapp);
}

void PickGuiPop::setup(HelpCtx Hctx,void *cvmapp)
{

  _pickgui = new PickGui(this,"modpick", cvmapp,
             Hctx, False, True, True);
  setWorkArea(_pickgui);
//  work->attach(_pickgui, work, work, work, work);
  SLpPush     *remove = addBottomRemove();
  SLpPush     *help   = addBottomHelp();
  remove->unmanageShellWhenPressed(this);
  remove->setLabel("REMOVE");
  help->setLabel("HELP");
//  setTitle("Model Picking");

}

PickGuiPop::~PickGuiPop(void)
{if(_pickgui) delete _pickgui;
}

int PickGuiPop::isActive()
{ return _pickgui->isActive();
}
//------------------------ end ----------------------------------//
//------------------------ end ----------------------------------//
//------------------------ end ----------------------------------//

