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


//------------------------ sl_quest_pop.hh ---------------------//
//------------------------ sl_quest_pop.hh ---------------------//
//------------------------ sl_quest_pop.hh ---------------------//

//              header file for the SLQuestPop class
//            derived from the SLShellContainer class
//                        subdirectory sl

   //  This class is a question dialog box.  When popped up, it asks a
   //  question which takes a YES or NO answer.  When the YES or NO button
   //  is pressed, the dialog box automatically pops down.
   //  The specified name becomes the title of the popup.
   //  The virtual function answerYes is called if the YES button is pressed.
   //  The virtual function answerNo  is called if the NO  button is pressed.
   //  The virtual function learnQuestion returns the question to be asked.

   //  There are two ways to use this class:
   //
   //  (1) TRANSIENT: The popup should be created just when it is needed.
   //      It automatically pops up when created, and it automatically
   //      deletes itself when the question is answered and it popps down.
   //      The user must use constructor 1 or 2 for this option.  The
   //      makeAndManage() function should not be called.
   //
   //      The question to be displayed is an argument to the
   //      constructor.  This question cannot be NULL.  The virtual
   //      function learnQuestion is not used.
   //
   //      Normally, the popup should be created in a pushbutton trap.
   //
   //  (2) NON-TRANSIENT: The popup is created ahead of time and can
   //      be used repeatedly.  It is popped up whenever makeAndManage()
   //      is called.  The user must use constructor 3 or 4 for this
   //      option.
   //
   //      A pointer to the question to be displayed (in static memory)
   //      is returned by the virtual function learnQuestion.  If the
   //      returned question is NULL or "Y" or "N", the popup will
   //      not be popped up; the answerYes virtual function will be
   //      called if the returned question is "Y"; the answerNo virtual
   //      function will be called if the returned question is "N".
   //      Neither virtual function will be called if the returned
   //      question is NULL.
   //
   //      With this option, the popup can be registered with a
   //      pushbutton to be automatically managed when the pushbutton
   //      is pressed, and the pushbutton does not have to have a
   //      trap registered with it.


   // Another way to use this class:  Instead of registering traps
   // or overriding virtual functions, the user can call this
   // public function right after creating this object:
   //               setComplexNotify(SLDelay *obj).
   // Then this object will call the following virtual function in obj:
   //        notifyComplex(SLDelay*, TRUE) if answered YES
   //        notifyComplex(SLDelay*, FALSE) if answered NO


   //  Care should be taken to make sure that this popup does not
   //  disappear from the display before it is popped down by pressing
   //  its OK button.  See the implementation file for details.

   //  If a virtual function is not overridden, and the corresponding
   //  user function is not NULL, then the user function will be called
   //  from the non-overridden virtual function.  The user functions
   //  are provided by arguments to the constructor.



#ifndef _SL_QUEST_POP_HH_
#define _SL_QUEST_POP_HH_

#include "sl/sl_shell_container.hh"

typedef void  SLQuestAnswer  (void *user_data);
typedef char *SLLearnQuestion(void *user_data);


class SLQuestPop : public SLShellContainer
{

//------------------------ data --------------------------------//
//------------------------ data --------------------------------//
//------------------------ data --------------------------------//

private:

  int              _transient; // whether the object is transient.
  int         _default_button; // TRUE if default button is YES; FALSE if NO.
  SLLearnQuestion *_quest_fun; // user function to call to get question.
  SLQuestAnswer   *_yes_fun;   // user function to call if Yes button pressed.
  SLQuestAnswer   *_no_fun;    // user function to call if No  button pressed.
  void            *_user_data; // user data to pass to user functions.

//----------------------- functions ----------------------------//
//----------------------- functions ----------------------------//
//----------------------- functions ----------------------------//

public:  // use constructor 1 or 2 for transient popup.
         // use constructor 3 or 4 for non-transient popup.
         // call makeAndManage only if constructor 3 or 4 is used.

  SLQuestPop (SLDelay *slparent, char *name, const char *question,
                    int            default_button = FALSE,
                    SLQuestAnswer *yes_fun        = NULL,
                    SLQuestAnswer *no_fun         = NULL,
                    void          *user_data      = NULL);
  SLQuestPop (Widget    wparent, char *name, const char *question,
                    int            default_button = FALSE,
                    SLQuestAnswer *yes_fun        = NULL,
                    SLQuestAnswer *no_fun         = NULL,
                    void          *user_data      = NULL);

  SLQuestPop (SLDelay *slparent, char *name,
                    int              default_button = FALSE,
                    SLLearnQuestion *quest_fun      = NULL,
                    SLQuestAnswer   *yes_fun        = NULL,
                    SLQuestAnswer   *no_fun         = NULL,
                    void            *user_data      = NULL);
  SLQuestPop (Widget    wparent, char *name,
                    int              default_button = FALSE,
                    SLLearnQuestion *quest_fun      = NULL,
                    SLQuestAnswer   *yes_fun        = NULL,
                    SLQuestAnswer   *no_fun         = NULL,
                    void            *user_data      = NULL);

  virtual ~SLQuestPop();

protected:   // virtual functions to override.
             // if not overridden, the _yes_fun and _no_fun and
             //   _quest_fun functions will be called.

  virtual char *learnQuestion()
                    { char *question = NULL;
                      if(_quest_fun) question = _quest_fun(_user_data);
                      return question; }
  virtual void answerYes() { if(_yes_fun) _yes_fun(_user_data); }
  virtual void answerNo () { if( _no_fun)  _no_fun(_user_data); }

private:

  void           applyQuestion (const char *question);
  virtual void   closing       () {}
  virtual Widget make          (Widget p = NULL);
  virtual void   manage        ();
  static void    yesCallback   (Widget w, XtPointer data, XtPointer call);
  static void    noCallback    (Widget w, XtPointer data, XtPointer call);

//------------------------ end of functions -------------------------//
//------------------------ end of functions -------------------------//
//------------------------ end of functions -------------------------//

} ;

#endif

//---------------------------- end --------------------------------//
//---------------------------- end --------------------------------//
//---------------------------- end --------------------------------//
