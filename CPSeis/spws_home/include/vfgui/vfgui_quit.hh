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

//-------------------------- vfgui_quit.hh --------------------------//
//-------------------------- vfgui_quit.hh --------------------------//
//-------------------------- vfgui_quit.hh --------------------------//

//              header file for the VfguiQuit class
//                 derived from the SLQuit class
//                      subdirectory vfgui


//   This class manages the quitting of any SLApp program which works
//   with velocity file data.

//   The function askQuitQuestion() should be called when the user
//   chooses to quit the program.  This function will pop up a
//   question dialog box which will display information about the status
//   of the data (whether data needs saving) and will ask an appropriate
//   question.  Based upon the user's response, this class will either
//   pop up the dialog box which allows the user to save data, or will
//   save necessary backup files and call the closing() virtual function
//   in SLApp.  If the dialog box is popped up to save data, and the
//   user successfully presses OK to save the data, and if there is no
//   more data to be saved, this class will then call the closing()
//   function in SLApp.


#ifndef _VFGUI_QUIT_HH_
#define _VFGUI_QUIT_HH_

#include "sl/sl_quit.hh"


class VfguiQuit : public SLQuit
{

//------------------------ data -------------------------------//
//------------------------ data -------------------------------//
//------------------------ data -------------------------------//

private:

  class VfManager   *_manager;

//-------------------- functions --------------------------------//
//-------------------- functions --------------------------------//
//-------------------- functions --------------------------------//

public:

           VfguiQuit (SLApp *app, SLShellContainer *savepop,
                                               VfManager *manager);
  virtual ~VfguiQuit ();

private:  // overriding virtual functions.

  virtual int  virtualDataNeedsSaving ();
  virtual void virtualGetSaveInfo     (char *info);


//---------------------- end of functions -----------------------//
//---------------------- end of functions -----------------------//
//---------------------- end of functions -----------------------//

} ;

#endif

//---------------------------- end --------------------------------//
//---------------------------- end --------------------------------//
//---------------------------- end --------------------------------//
