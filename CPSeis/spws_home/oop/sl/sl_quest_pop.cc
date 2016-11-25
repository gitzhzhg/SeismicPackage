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

//---------------------- sl_quest_pop.cc -----------------------//
//---------------------- sl_quest_pop.cc -----------------------//
//---------------------- sl_quest_pop.cc -----------------------//

//          implementation file for the SLQuestPop class
//           derived from the SLShellContainer class
//                       subdirectory sl


     //  Care should be taken to make sure that this popup
     //  does not disappear from the display before it is
     //  popped down by pressing its YES or NO button; otherwise
     //  it will not be deleted and the user will not be able
     //  to use the application.  This can happen if the parent
     //  of this object is unmanaged immediately after this
     //  object is created, before returning to the event loop.
     //  To avoid this, either make sure that the parent is not
     //  unmanaged at such a time, or make the parent be the main
     //  window.  See the implementation file for SLErrorPop
     //  for more details.
 

#include "sl/sl_quest_pop.hh"
#include "sl/sl_prim.hh"
#include "wproc.h"
#include "cprim.h"
#include <Xm/MessageB.h>
#include <stdlib.h>
#include <iostream.h>
#include <assert.h>


//-------------- first set of constructors ------------------//
//-------------- first set of constructors ------------------//
//-------------- first set of constructors ------------------//

SLQuestPop::SLQuestPop(Widget wparent, char *name, const char *question,
                       int            default_button,
                       SLQuestAnswer *yes_fun,
                       SLQuestAnswer *no_fun,
                       void          *user_data)
     : SLShellContainer(wparent, name, NULL),
                 _transient           (TRUE),
                 _default_button      (default_button),
                 _quest_fun           (NULL),
                 _yes_fun             (yes_fun),
                 _no_fun              (no_fun),
                 _user_data           (user_data)
{
  make();
  applyQuestion(question);
  SLShellContainer::manage();
}


SLQuestPop::SLQuestPop(SLDelay *slparent, char *name, const char *question,
                       int            default_button,
                       SLQuestAnswer *yes_fun,
                       SLQuestAnswer *no_fun,
                       void          *user_data)
     : SLShellContainer(slparent, name, NULL),
                 _transient           (TRUE),
                 _default_button      (default_button),
                 _quest_fun           (NULL),
                 _yes_fun             (yes_fun),
                 _no_fun              (no_fun),
                 _user_data           (user_data)
{
  make();
  applyQuestion(question);
  SLShellContainer::manage();
}



//-------------- second set of constructors ------------------//
//-------------- second set of constructors ------------------//
//-------------- second set of constructors ------------------//

SLQuestPop::SLQuestPop(Widget wparent, char *name,
                       int              default_button,
                       SLLearnQuestion *quest_fun,
                       SLQuestAnswer   *yes_fun,
                       SLQuestAnswer   *no_fun,
                       void            *user_data)
     : SLShellContainer(wparent, name, NULL),
                 _transient           (FALSE),
                 _default_button      (default_button),
                 _quest_fun           (quest_fun),
                 _yes_fun             (yes_fun),
                 _no_fun              (no_fun),
                 _user_data           (user_data)
{
}


SLQuestPop::SLQuestPop(SLDelay *slparent, char *name,
                       int              default_button,
                       SLLearnQuestion *quest_fun,
                       SLQuestAnswer   *yes_fun,
                       SLQuestAnswer   *no_fun,
                       void            *user_data)
     : SLShellContainer(slparent, name, NULL),
                 _transient           (FALSE),
                 _default_button      (default_button),
                 _quest_fun           (quest_fun),
                 _yes_fun             (yes_fun),
                 _no_fun              (no_fun),
                 _user_data           (user_data)
{
}



//--------------- callbacks -------------------------------------//
//--------------- callbacks -------------------------------------//
//--------------- callbacks -------------------------------------//

void SLQuestPop::yesCallback(Widget w, XtPointer data, XtPointer /*call*/)
{
  SLQuestPop *pop = (SLQuestPop*)data;
  //// cout << "am in SLQuestPop YES callback" << endl;
  pop->answerYes();
  pop->callNotifyComplex(TRUE);
/*
  if(w == pop->W() && pop->_transient) delete pop;
  else pop->unmanage();
*/
  pop->unmanage();
//XSync(XtDisplay(w), FALSE);      // doesn't help
  XmUpdateDisplay(w);              // usually doesn't work (timing?)
//XSync(XtDisplay(w), FALSE);      // doesn't help
  if(w == pop->W() && pop->_transient) delete pop;
  SLPrim::updateEverything();
}



void SLQuestPop::noCallback(Widget w, XtPointer data, XtPointer /*call*/)
{
  SLQuestPop *pop = (SLQuestPop*)data;
  //// cout << "am in SLQuestPop NO callback" << endl;
  pop->answerNo();
  pop->callNotifyComplex(FALSE);
/*
  if(w == pop->W() && pop->_transient) delete pop;
  else pop->unmanage();
*/
  pop->unmanage();
  XmUpdateDisplay(w);
  if(w == pop->W() && pop->_transient) delete pop;
  SLPrim::updateEverything();
}



//--------------------- destructor ---------------------------//
//--------------------- destructor ---------------------------//
//--------------------- destructor ---------------------------//

SLQuestPop::~SLQuestPop()
{
  //// cout << "am in SLQuestPop destructor" << endl;
}



//--------------------- apply question ---------------------------//
//--------------------- apply question ---------------------------//
//--------------------- apply question ---------------------------//

        // private.

void SLQuestPop::applyQuestion(const char *question)
{
  assert(question);
  XmString string = XmStringCreateLtoR((char*)question,
                                       XmSTRING_DEFAULT_CHARSET);
  XtVaSetValues(topWidget(), XmNmessageString, string, NULL);
  XmStringFree(string);
}



//--------------------------- manage ----------------------------//
//--------------------------- manage ----------------------------//
//--------------------------- manage ----------------------------//

    // private.
    // should be called only if the second set of contructors is used.

void SLQuestPop::manage()
{
  assert(!_transient);
  assert(made());
  assert(!XtIsManaged(topWidget()));
  char *question = learnQuestion();
  if(!question) return;
  if     (strings_equal(question, "Y"))
                {
                answerYes();
                SLPrim::updateEverything();
                }
  else if(strings_equal(question, "N"))
                {
                answerNo();
                SLPrim::updateEverything();
                }
  else
                {
                applyQuestion(question);
                SLShellContainer::manage();
                }
}



//--------------------- fallback resources --------------------//
//--------------------- fallback resources --------------------//
//--------------------- fallback resources --------------------//


static String defres[]= {
      "*background:         goldenrod",
      "*foreground:         brown",
      "*messageAlignment:   ALIGNMENT_CENTER",
      "*dialogStyle:        DIALOG_FULL_APPLICATION_MODAL",
      "*okLabelString:      Yes",
      "*cancelLabelString:  No",
  //  "*defaultButtonType:  DIALOG_CANCEL_BUTTON",
      NULL };



//---------------------------- make -----------------------------//
//---------------------------- make -----------------------------//
//---------------------------- make -----------------------------//


Widget SLQuestPop::make(Widget p)
{
  if(!made())
       {
       Widget w = SLShellContainer::make(p);
       if(!w)
           {
           char fullname[200];
           get_full_name(wParent(), fullname);
           strcat(fullname, "*");
           strcat(fullname, (char*)instanceName());
           setDefRes(XtDisplay(wParent()), fullname, defres);
   /// the "*" above allows for the fact that there is the name of
   /// the shell widget between the name of the parent and the
   /// instance name.
/*
                            printf("%s%s\n", fullname, defres[0]);
*/
           Arg args[22];
           int i = 0;
           if(_default_button)
              XtSetArg(args[i], XmNdefaultButtonType, XmDIALOG_OK_BUTTON);
           else
              XtSetArg(args[i], XmNdefaultButtonType, XmDIALOG_CANCEL_BUTTON);
           i++;
           w= XmCreateQuestionDialog(wParent(), _name, args, i);
           setTopWidget(w);
           XtAddCallback(w, XmNokCallback,
                         (XtCallbackProc)yesCallback, (XtPointer)this);
           XtAddCallback(w, XmNcancelCallback,
                         (XtCallbackProc)noCallback, (XtPointer)this);
           Widget wh =XmMessageBoxGetChild(w, XmDIALOG_HELP_BUTTON);
           XtUnmanageChild(wh);
           Widget shell = get_shell_widget(w);
           XtVaSetValues(shell, XtNtitle, _name, NULL);
           }
       }
  return topWidget();
}


//-------------------------- end -------------------------------//
//-------------------------- end -------------------------------//
//-------------------------- end -------------------------------//
