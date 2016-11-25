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
//------------------- seg_edit_pop.cc ----------------------//
//------------------- seg_edit_pop.cc ----------------------//
//------------------- seg_edit_pop.cc ----------------------//

//         implementation file for the SegEditPop class
//                 derived from the SLDialog class


#include "seg_edit_pop.hh"
#include <seg_edit.hh>
#include <sl/slp_push.hh>
#include "wproc.h"
#include "cprim.h"

//------------------- constructors and destructor -----------------//
//------------------- constructors and destructor -----------------//
//------------------- constructors and destructor -----------------//

SegEditPop::SegEditPop (SLDelay *slparent, char *name, HelpCtx Hctx,
         void *vector_list,char *segment_name)
        : SLDialog(slparent, name, Hctx,True)
{ SLSmartForm *work = workArea();
  SLpPush     *remove = addBottomRemove();
                        addBottomKeyhelp();
                        addBottomHelp();
  remove->unmanageShellWhenPressed(this);

  _segedit = new SegEdit(work,name, vector_list, segment_name,
             Hctx, False, True, True, 1);
  work->attach(_segedit, work, work, work, work);
}

SegEditPop::SegEditPop (Widget parent, char *name, HelpCtx Hctx,
         void *vector_list,char *segment_name)
        : SLDialog(parent, name, Hctx,True)
{ SLSmartForm *work = workArea();
  SLpPush     *remove = addBottomRemove();
                        addBottomKeyhelp();
                        addBottomHelp();
  remove->unmanageShellWhenPressed(this);

  _segedit = new SegEdit(work,name, vector_list, segment_name,
             Hctx, False, True, True, 1);
  work->attach(_segedit, work, work, work, work);
}


SegEditPop::~SegEditPop(void)
{if(_segedit != NULL) delete _segedit;
}

//------------------------ end ----------------------------------//
//------------------------ end ----------------------------------//
//------------------------ end ----------------------------------//

