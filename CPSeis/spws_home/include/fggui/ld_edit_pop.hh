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

//------------------------ ld_edit_pop.hh ----------------------------//
//------------------------ ld_edit_pop.hh ----------------------------//
//------------------------ ld_edit_pop.hh ----------------------------//

//                header file for the LdEditPop class
//                  derived from the SLDialog class
//                         subdirectory fggui

         // This dialog box is used to build/edit LD cards.

#ifndef _LD_EDIT_POP_HH_
#define _LD_EDIT_POP_HH_

#include "sl/sl_dialog.hh"
#include <X11/Intrinsic.h>


class LdEditPop : public SLDialog
{

//------------------------ data -------------------------------//
//------------------------ data -------------------------------//
//------------------------ data -------------------------------//

private:

  class FieldGeometry  *_fg; // pointer to field geometry.
  class LdChoiceGui    *_cc; // GUI which chooses lines to operate on.
  class LdOperationGui *_oo; // GUI which chooses operation to perform.

//-------------------- functions --------------------------------//
//-------------------- functions --------------------------------//
//-------------------- functions --------------------------------//

public:

  LdEditPop(SLDelay *slparent, char *name, HelpCtx hctx,
                                       class FieldGeometry *fg);
  virtual ~LdEditPop();

  FieldGeometry  *getFieldGeometry  ()    const  { return _fg; }
  LdChoiceGui    *getLdChoiceGui    ()    const  { return _cc; }
  LdOperationGui *getLdOperationGui ()    const  { return _oo; }
  long            getChoice         ()    const;
  long            getOperation      ()    const;

//---------------------- end of functions -----------------------//
//---------------------- end of functions -----------------------//
//---------------------- end of functions -----------------------//

} ;

#endif

//---------------------------- end --------------------------------//
//---------------------------- end --------------------------------//
//---------------------------- end --------------------------------//
