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
//------------------- seg_edit_pop.hh ------------------------------//
//------------------- seg_edit_pop.hh ------------------------------//
//------------------- seg_edit_pop.hh ------------------------------//

//              header file for the SegEditPop class
//                 derived from the SLDialog class

#ifndef _SEG_EDIT_POP_
#define _SEG_EDIT_POP_

#include "sl/sl_dialog.hh"

class SegEdit;

class SegEditPop : public SLDialog
{
//------------------ beginning of class--------------------------//
//------------------ beginning of class--------------------------//

public:

  SegEditPop (SLDelay *contain, char *name, HelpCtx Hctx,
   void *vector_list,char *segment_name);
  SegEditPop (Widget parent, char *name, HelpCtx Hctx ,
   void *vector_list,char *segment_name);

  virtual ~SegEditPop(void);

private:

   SegEdit *_segedit;
//--------------------- end of class ----------------------------//
//--------------------- end of class ----------------------------//
} ;

#endif

//------------------------ end --------------------------------//
//------------------------ end --------------------------------//

