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
//------------------- coord_edit_pop.hh ------------------------------//
//------------------- coord_edit_pop.hh ------------------------------//

//              header file for the CoordEditPop class
//                 derived from the SLDialog class

#ifndef _COORD_EDIT_POP_
#define _COORD_EDIT_POP_

#include <stdio.h>
#include "sl/sl_dialog.hh"
#include "coord_edit.hh"


class CoordEditPop : public SLDialog
{
//------------------ beginning of class--------------------------//

public:

  CoordEditPop (SLDelay *contain, char *name, HelpCtx Hctx,
   void *cddata);
  CoordEditPop (Widget parent, char *name, HelpCtx Hctx ,
   void *cddata);

  virtual ~CoordEditPop(void);

private:
  CoordEdit *_cdedit;

protected:
 Boolean okNotify() {
  if(_cdedit) {_cdedit->table_to_data(); return True;}
  else return False;
 }
 Boolean cancelNotify() {
  if(_cdedit) {_cdedit->data_to_table(); return True;}
  else return False;
 }

 enum PBTYPE {OK,CAN};
//--------------------- end of class ----------------------------//
} ;

#endif

//------------------------ end --------------------------------//

