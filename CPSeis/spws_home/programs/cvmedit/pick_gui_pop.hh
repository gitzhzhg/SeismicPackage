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
//------------------- pick_gui_pop.hh ------------------------------//
//------------------- pick_gui_pop.hh ------------------------------//
//------------------- pick_gui_pop.hh ------------------------------//

//              header file for the SegEditPop class
//                 derived from the SLDialog class

#ifndef _PICK_GUI_POP
#define _PICK_GUI_POP

#include "wproc.h"
#include "sl/sl_dialog.hh"

class PickGui;

class PickGuiPop : public SLDialog
{
//------------------ beginning of class--------------------------//
//------------------ beginning of class--------------------------//

public:

  PickGuiPop (SLDelay *contain, char *name, HelpCtx Hctx, void *cvmapp);
  PickGuiPop (Widget parent, char *name, HelpCtx Hctx , void *cvmapp);

  virtual ~PickGuiPop(void);
  int isActive();

protected:


private:

   PickGui *_pickgui;
   void setup(HelpCtx Hctx,void *cvmapp);
//--------------------- end of class ----------------------------//
//--------------------- end of class ----------------------------//
};

#endif

//------------------------ end --------------------------------//
//------------------------ end --------------------------------//

