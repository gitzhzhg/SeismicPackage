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

//------------------------ survey_read_pop.hh ----------------------------//
//------------------------ survey_read_pop.hh ----------------------------//
//------------------------ survey_read_pop.hh ----------------------------//

//                header file for the SurveyReadPop class
//                  derived from the SLDialog class
//                         subdirectory fggui


#ifndef _SURVEY_READ_POP_HH_
#define _SURVEY_READ_POP_HH_

#include "sl/sl_dialog.hh"
#include "oprim/file_base.hh"
#include <X11/Intrinsic.h>


class SurveyReadPop : public SLDialog
{

//------------------------ data -------------------------------//
//------------------------ data -------------------------------//
//------------------------ data -------------------------------//

private:

  class FieldGeometry   *_fg;      // pointer to field geometry.
  class SurveyFile      *_file;    // owned by this class.
  class SLFileChoice    *_choice;  // owned by this class.

//-------------------- functions --------------------------------//
//-------------------- functions --------------------------------//
//-------------------- functions --------------------------------//

public:

  SurveyReadPop(SLDelay *slparent, char *name, HelpCtx hctx,
                          class FieldGeometry *fg,
                          class FgControlPop  *dcp,
                          class ContainerList *clist = NULL);
  virtual ~SurveyReadPop();

  FieldGeometry  *getFieldGeometry()  const  { return _fg; }
  SurveyFile     *getFile         ()  const  { return _file; }
  SLFileChoice   *getFileChoice   ()  const  { return _choice; }

private:     // these override SLDialog.

  virtual Boolean preManageNotify();
  virtual Boolean okNotify();
  virtual void    applyNotify();
  virtual Boolean cancelNotify();

//---------------------- end of functions -----------------------//
//---------------------- end of functions -----------------------//
//---------------------- end of functions -----------------------//

} ;

#endif

//---------------------------- end --------------------------------//
//---------------------------- end --------------------------------//
//---------------------------- end --------------------------------//
