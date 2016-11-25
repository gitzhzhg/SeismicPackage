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

//------------------------ jd_save_pop.hh ----------------------------//
//------------------------ jd_save_pop.hh ----------------------------//
//------------------------ jd_save_pop.hh ----------------------------//

//                header file for the JdSavePop class
//                  derived from the SLDialog class
//                  derived from the FgInform class
//                         subdirectory fggui


#ifndef _JD_SAVE_POP_HH_
#define _JD_SAVE_POP_HH_

#include "sl/sl_dialog.hh"
#include "geom/fg_inform.hh"
#include "oprim/file_base.hh"


class JdSavePop : public SLDialog, public FgInform
{

//------------------------ data -------------------------------//
//------------------------ data -------------------------------//
//------------------------ data -------------------------------//

private:

  class JdFile          *_file;    // owned by this class.
  class SLFileChoice    *_choice;  // owned by this class.

//-------------------- functions --------------------------------//
//-------------------- functions --------------------------------//
//-------------------- functions --------------------------------//

public:

  JdSavePop(SLDelay *slparent, char *name, HelpCtx hctx,
                      class FieldGeometry *fg,
                      class FgControlPop  *dcp,
                      class ContainerList *clist = NULL);
  virtual ~JdSavePop();

  JdFile         *getFile         ()  const  { return _file; }
  SLFileChoice   *getFileChoice   ()  const  { return _choice; }

private:     // these override SLDialog.

  virtual Boolean preManageNotify();
  virtual Boolean okNotify();
  virtual void    applyNotify();
  virtual Boolean cancelNotify();

private:     // these override FgInform.

  virtual void finishedChanges(FieldGeometry *fg);


//---------------------- end of functions -----------------------//
//---------------------- end of functions -----------------------//
//---------------------- end of functions -----------------------//

} ;

#endif

//---------------------------- end --------------------------------//
//---------------------------- end --------------------------------//
//---------------------------- end --------------------------------//
