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

//---------------------- slp_push.cc ----------------------------------//
//---------------------- slp_push.cc ----------------------------------//
//---------------------- slp_push.cc ----------------------------------//

//         implementation file for the SLpPush class
//                 derived from the SLpBase class
//                      subdirectory sl

#include "sl/slp_push.hh"
#include "sl/sl_shell_container.hh"
#include <Xm/PushB.h>
 

//-----------------constructor----------------------------------------//
//-----------------constructor----------------------------------------//
//-----------------constructor----------------------------------------//


SLpPush::SLpPush(SLDelay *slparent, char *name, long ident, char *label)
    : SLpBase(slparent, name, xmPushButtonWidgetClass, ident, _ACTIVE),
      _special_trap_action (_NORMAL_ACTION),
      _slpop               (NULL),
      _helpname            (NULL)
{
  createCvarResource();
  createSenseResource();
  setupCvarValue(label);
  if(slparent->topWidget()) make();
}


SLpPush::SLpPush(Widget wparent, char *name, long ident, char *label)
    : SLpBase(wparent, name, xmPushButtonWidgetClass, ident, _ACTIVE),
      _special_trap_action (_NORMAL_ACTION),
      _slpop               (NULL),
      _helpname            (NULL)
{
  createCvarResource();
  createSenseResource();
  setupCvarValue(label);
  make();
}


SLpPush::SLpPush(Widget w, long ident, char *label)
    : SLpBase(w, ident, _ACTIVE),
      _special_trap_action (_NORMAL_ACTION),
      _slpop               (NULL),
      _helpname            (NULL)
{
  createCvarResource();
  createSenseResource();
  setupCvarValue(label);
  make();
}


//-------------------- destructor --------------------------//
//-------------------- destructor --------------------------//
//-------------------- destructor --------------------------//

SLpPush::~SLpPush(void)
{
  if(_helpname) string_free(_helpname);
}



//----------------- other functions -------------------------//
//----------------- other functions -------------------------//
//----------------- other functions -------------------------//


void SLpPush::setCvarResource(void)
{
  setCompoundStringResource(XmNlabelString);
}


Boolean SLpPush::notify(SLPrim * /* gui */)
{
  if(_special_trap_action == _SHOW_HELP && getHelpCtx())
       {
       if(_helpname)
            {
            overview_help(_helpname, getHelpCtx());
            }
       else
            {
            char fullname[200];
            get_full_name (topWidget(), fullname);
            overview_help(fullname, getHelpCtx());
            }
       }
  else if(_special_trap_action == _MANAGE_SHELL && _slpop)
       {
/*
       if(_slpop->made() && XtIsManaged(_slpop->topWidget()))
           {
           Widget w = _slpop->W();
           Widget shell = get_shell_widget(w);
           XRaiseWindow(XtDisplay(shell), XtWindow(shell));
           }
       else
           {
           _slpop->makeAndManage();
           }
  ///  _slpop->makeAndManage();    /// replaced with above code 8/14/95
*/
       _slpop->makeAndManage();    /// changed back (no longer needed)
                                   ///   11/10/95
       }
  else if(_special_trap_action == _UNMANAGE_SHELL && _slpop)
       {
       _slpop->unmanage();
       }
  return TRUE;
}



//--------------------------- make -----------------------------//
//--------------------------- make -----------------------------//
//--------------------------- make -----------------------------//


Widget SLpPush::make(Widget p)
{
  if(!made())
     {
     Widget w = SLDelay::make(p);
     if(!w)
        {
        Arg args[22];
        int i = 0;
        w = XmCreatePushButton(wParent(), (char*)instanceName(), args, i);
        setTopWidget(w);
        XtManageChild(w);
        }
     setTraversal(_traversal);
     install_help();
     XtAddEventHandler (w, FocusChangeMask, FALSE,
                       (XtEventHandler)focusEventHandler, (XtPointer)this);
     XtAddCallback(w, XmNactivateCallback,
                        (XtCallbackProc)activateCallback, (XtPointer)this);
     if(updateSenseNeverActivated())
                          setupSenseValue(SLBase::sensitivity());
     updateSelf(TRUE);
     }
  return topWidget();
}



//------------------------ end ----------------------------------//
//------------------------ end ----------------------------------//
//------------------------ end ----------------------------------//
