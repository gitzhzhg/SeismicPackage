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

//-------------------------- sl_quit.cc ----------------------------//
//-------------------------- sl_quit.cc ----------------------------//
//-------------------------- sl_quit.cc ----------------------------//

//            implementation file for the SLQuit class
//                  derived from the SLDelay class
//                        subdirectory sl


#include "sl/sl_quit.hh"
#include "sl/sl_app.hh"
#include "sl/sl_shell_container.hh"
#include "sl/sl_quest_pop.hh"


     // there is no pseudo widget in this class.

//------------------------ constructor ----------------------------//
//------------------------ constructor ----------------------------//
//------------------------ constructor ----------------------------//


SLQuit::SLQuit (SLApp *app, SLShellContainer *savepop)
         : SLDelay(app, "sl_quit"),
              _app                      (app),
              _savepop                  (savepop),
              _quest                    (NULL)
{
  assert(_app && _savepop);
}



//---------------------- destructor -------------------------//
//---------------------- destructor -------------------------//
//---------------------- destructor -------------------------//

SLQuit::~SLQuit()
{
}



//----------------------- virtual get full question ---------------------//
//----------------------- virtual get full question ---------------------//
//----------------------- virtual get full question ---------------------//


void SLQuit::virtualGetFullQuestion(char *question)
{
  char info[8000];
  strcpy(info, "");
  virtualGetSaveInfo(info);
  strcpy(question, "You have data which\nhas NOT been saved.\n");
  strcat(question, "----\n");
  if(info[0] != '\0')
      {
      strcat(question, info);
      strcat(question, "----\n");
      }
  strcat(question, "Do you want to save your data\nbefore quitting?");
}



//---------------------- ask quit question ---------------------------//
//---------------------- ask quit question ---------------------------//
//---------------------- ask quit question ---------------------------//


void SLQuit::askQuitQuestion()
{
  int need = virtualDataNeedsSaving();
  if(need)
      {
      char question[9000];
      virtualGetFullQuestion(question);
/*
      char info    [8000];
      strcpy(question, "");
      strcpy(info    , "");
      virtualGetSaveInfo(info);
      strcpy(question, "You have data which\nhas NOT been saved.\n");
      strcat(question, "----\n");
      if(info[0] != '\0')
          {
          strcat(question, info);
          strcat(question, "----\n");
          }
      strcat(question, "Do you want to save your data\nbefore quitting?");
*/
      _quest = new SLQuestPop(_app, "Question", question, TRUE);
      _quest->setComplexNotify(this);
      }
  else
      {
/****
      char question[90];
      strcpy(question, "Are you sure you want\nto quit?");
      _quest = new SLQuestPop(_app, "Question", question, FALSE);
****/
      _app->delayDelete();
      }
/****
  _quest->setComplexNotify(this);
****/
}



//---------------- called from question box ------------------------//
//---------------- called from question box ------------------------//
//---------------- called from question box ------------------------//

        // ident will be TRUE or FALSE.

void SLQuit::calledFromQuestionBox(int ident)
{
  int need = virtualDataNeedsSaving();
  if(need)
      {
      if(ident)   // do save data instead of quitting.
          {
          _savepop->setComplexNotify(this);
          _savepop->makeAndManage();
          _savepop->setModal(SLShellContainer::FullAppModal);
          }
      else        // do not save data before quitting.
          {
          _app->delayDelete();
          }
      }
  else
      {
      if(ident)   // do quit the application.
          {
          _app->delayDelete();
          }
      else        // do not quit the application.
          {
          }
      }
  _quest->setComplexNotify(NULL);
  _quest = NULL;         // _quest will delete itself.
}



//----------------- called from save pop -------------------------------//
//----------------- called from save pop -------------------------------//
//----------------- called from save pop -------------------------------//

void SLQuit::calledFromSavePop(int ident)
{
  if(ident == FP_OK)   // do quit the application (data has been saved).
      {
      int need = virtualDataNeedsSaving();
      if(need == FALSE)
          {
          _app->delayDelete();
          }
      }
  else                 // do not quit the application.
      {
      }
  _savepop->setComplexNotify(NULL);
  _savepop->setModal(SLShellContainer::Modeless);
}



//--------------------- notify complex -----------------------//
//--------------------- notify complex -----------------------//
//--------------------- notify complex -----------------------//

      // private virtual function overriding SLDelay.
      // called from _quest   with ident == TRUE or FALSE.
      // called from _savepop with ident == FP_OK or something else.

Boolean SLQuit::notifyComplex(SLDelay *sender, int ident)
{
  if(sender == _quest)
      {
      calledFromQuestionBox(ident);
      }
  else if(sender == _savepop)
      {
      calledFromSavePop(ident);
      }
  return TRUE;
}



//--------------------------- end --------------------------------//
//--------------------------- end --------------------------------//
//--------------------------- end --------------------------------//
