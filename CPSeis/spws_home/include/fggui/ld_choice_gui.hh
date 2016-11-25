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

//------------------------ ld_choice_gui.hh ----------------------------//
//------------------------ ld_choice_gui.hh ----------------------------//
//------------------------ ld_choice_gui.hh ----------------------------//

//                header file for the LdChoiceGui class
//                  derived from the SLSmartForm class
//                         subdirectory fggui

                // This class is used to choose which
                // seismic lines will be operated on.

#ifndef _LD_CHOICE_GUI_HH_
#define _LD_CHOICE_GUI_HH_

#include "sl/sl_smart_form.hh"
#include <X11/Intrinsic.h>


class LdChoiceGui : public SLSmartForm
{

//------------------------ data -------------------------------//
//------------------------ data -------------------------------//
//------------------------ data -------------------------------//

public:

  enum { ACTIVE_LINE = 1, SELECTED_LINES, ALL_LINES };

private:

  class FieldGeometry *_fg;      // pointer to field geometry.
  long                 _choice;  // which lines to operate on (enum).

//-------------------- functions --------------------------------//
//-------------------- functions --------------------------------//
//-------------------- functions --------------------------------//

public:

  LdChoiceGui(SLDelay *slparent, char *name, HelpCtx hctx,
                                       class FieldGeometry *fg);
  virtual ~LdChoiceGui();

  FieldGeometry *getFieldGeometry ()    const  { return _fg; }
  long           getChoice        ()    const  { return _choice; }

//---------------------- end of functions -----------------------//
//---------------------- end of functions -----------------------//
//---------------------- end of functions -----------------------//

} ;

#endif

//---------------------------- end --------------------------------//
//---------------------------- end --------------------------------//
//---------------------------- end --------------------------------//
