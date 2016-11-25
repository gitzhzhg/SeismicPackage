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

//-------------------------- sl_quit.hh --------------------------//
//-------------------------- sl_quit.hh --------------------------//
//-------------------------- sl_quit.hh --------------------------//

//                header file for the SLQuit class
//                 derived from the SLDelay class
//                        subdirectory sl


//   This is a base class for classes which manage the quitting of
//   any SLApp program which works with data which might have to be
//   saved to a file before quitting.

//   The public function askQuitQuestion() should be called when the
//   user chooses to quit the program, either by pressing a Quit button,
//   or by choosing to close the application through the window manager.

//   The function askQuitQuestion() will find out whether there is
//   data which needs saving.
//   -----------
//   If no data needs saving, it will call the delayDelete() function
//   in SLApp.
//   -----------
//   If data does need saving, it will pop up a question dialog box which
//   will display information about the status of the data and will ask
//   an appropriate question.
//   -----------
//   Based upon the user's response, this class will either pop up the
//   dialog box which allows the user to save data, or will call the
//   delayDelete() function in SLApp.
//   -----------
//   If the dialog box is popped up to save data, and the user successfully
//   presses OK to save the data, and if there is no more data to be saved,
//   this class will then call the delayDelete() function in SLApp.

//   To derive from this class, it is only necessary to override one
//   or two virtual functions (see below).

//   Example of the question for the question dialog box:
//
//     |      You have data which has NOT been saved.     |
//     |                     ----                         |
//     |      (text supplied by virtualGetSaveInfo)       |
//     |                     ----                         |
//     |  Do you want to save your data before quitting?  |
//
//   If you override virtualGetSaveInfo, you will supply the portion
//   of the text indicated above in parentheses.  But if you instead
//   override virtualGetFullQuestion, you will supply the entire question.
//   If you override NEITHER function, only the first and last line
//   of the above question will appear.  If you override BOTH functions,
//   virtualGetSaveInfo will never get called since it is called from
//   the non-overridden virtualGetFullQuestion.



#ifndef _SL_QUIT_HH_
#define _SL_QUIT_HH_

#include "sl/sl_delay.hh"


class SLQuit : public SLDelay
{

//------------------------ data -------------------------------//
//------------------------ data -------------------------------//
//------------------------ data -------------------------------//

private:

  class SLApp            *_app;      // main application.
  class SLShellContainer *_savepop;  // dialog box which allows saving data.
  class SLQuestPop       *_quest;

//-------------------- functions --------------------------------//
//-------------------- functions --------------------------------//
//-------------------- functions --------------------------------//

public:

           SLQuit (SLApp *app, SLShellContainer *savepop);
  virtual ~SLQuit ();

  void  askQuitQuestion();

protected:   // virtual functions to override.

  virtual int  virtualDataNeedsSaving() = 0;
                // should return TRUE if data needs saving.
                // should return FALSE if data does not need saving.

  virtual void virtualGetSaveInfo(char* /*info*/) {}
                // returns auxiliary info about data which needs saving.
                // optional to override (preset to "").
                // called only when data needs saving.
                // see example above.

  virtual void virtualGetFullQuestion(char* question);
                // returns the entire question about data which needs saving.
                // optional to override.
                // called only when data needs saving.
                // see example above.

private:  // these override SLDelay.

  virtual WidgetClass topClass      ()      { return NULL; }
  virtual Boolean     isWidgetBased ()      { return FALSE; }
  virtual Boolean     notifyComplex (SLDelay *sender, int ident);

private:

  void  calledFromQuestionBox       (int ident);
  void  calledFromSavePop           (int ident);

//---------------------- end of functions -----------------------//
//---------------------- end of functions -----------------------//
//---------------------- end of functions -----------------------//

} ;

#endif

//---------------------------- end --------------------------------//
//---------------------------- end --------------------------------//
//---------------------------- end --------------------------------//
