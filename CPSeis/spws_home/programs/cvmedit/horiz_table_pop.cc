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
//------------------- horiz_table_pop.cc ----------------------//
//------------------- horiz_table_pop.cc ----------------------//
//------------------- horiz_table_pop.cc ----------------------//

//         implementation file for the HorizTablePop class
//                 derived from the SLDialog class


#include "horiz_table_pop.hh"
#include "horiz_table.hh"
#include "seg_edit.hh"
#include "sl/slp_push.hh"
#include "wbox.h"
#include "wproc.h"
#include "cprim.h"

//------------------- constructors and destructor -----------------//
//------------------- constructors and destructor -----------------//
//------------------- constructors and destructor -----------------//


HorizTablePop::HorizTablePop(SLDelay *slparent, char *name,
     HelpCtx hctx,void *user_data)
        : SLDialog(slparent, name, hctx,True)
{ SLSmartForm *work = workArea();
  SLpPush     *remove = addBottomRemove();
                        addBottomHelp();
  remove->unmanageShellWhenPressed(this);

  _htable = new HorizTable(work,"htable",user_data);
  work->attach(_htable, work, work, work, work);
}

HorizTablePop::HorizTablePop(Widget parent, char *name,
     HelpCtx hctx,void *user_data)
        : SLDialog(parent, name, hctx,True)
{ SLSmartForm *work = workArea();
  SLpPush     *remove = addBottomRemove();
                        addBottomHelp();
  remove->unmanageShellWhenPressed(this);

  _htable = new HorizTable(work,"htable",user_data);
  work->attach(_htable, work, work, work, work);
}

HorizTablePop::~HorizTablePop(void)
{
 delete _htable;
// delete _segedit;
}

//------------------------ end ----------------------------------//
//------------------------ end ----------------------------------//
//------------------------ end ----------------------------------//

