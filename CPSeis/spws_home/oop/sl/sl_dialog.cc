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

//---------------------- sl_dialog.cc -----------------------//
//---------------------- sl_dialog.cc -----------------------//
//---------------------- sl_dialog.cc -----------------------//

//          implementation file for the SLDialog class
//           derived from the SLShellContainer class
//                       subdirectory sl


#include "sl/sl_dialog.hh"
#include "sl/sl_smart_form.hh"
#include "sl/sl_sep.hh"
#include "sl/slp_push.hh"
#include "sl/shell_stat_msg.hh"
#include <Xm/DialogS.h>
#include <Xm/Form.h>
#include <iostream.h>
#include <assert.h>
#include <X11/IntrinsicP.h> /* TEMP */

#define  NAME_OK       "ok"
#define  NAME_APPLY    "apply"
#define  NAME_CANCEL   "cancel"
#define  NAME_REMOVE   "remove"
#define  NAME_UNDO     "undo"
#define  NAME_RESET    "reset"
#define  NAME_KEYHELP  "keyhelp"
#define  NAME_HELP     "help"

/*
#define  LABEL_OK       "  OK  "
#define  LABEL_APPLY    "Apply"
#define  LABEL_CANCEL   "Cancel"
#define  LABEL_REMOVE   "Remove"
#define  LABEL_UNDO     " Undo "
#define  LABEL_RESET    "Reset"
#define  LABEL_KEYHELP  "Help on Keys"
#define  LABEL_HELP     " Help "
*/
#define  LABEL_OK       "OK"
#define  LABEL_APPLY    "Apply"
#define  LABEL_CANCEL   "Cancel"
#define  LABEL_REMOVE   "Remove"
#define  LABEL_UNDO     "Undo"
#define  LABEL_RESET    "Reset"
#define  LABEL_KEYHELP  "Help on Keys"
#define  LABEL_HELP     "Help"


/////////// NO LONGER:

   // NOTE:  This class will call this virtual function in a
   // registered SLDelay object:
   //
   //        int dummy = notifyComplex(this, button)
   //
   // under these circumstances:
   //
   //...  (1) Right after the APPLY button is pressed, with the
   //  ...       argument button set to FP_APPLY.
   //  (2...) Right after the CANCEL button is pressed, with the
   //      ...   argument button set to FP_CANCEL.
   //  (3) Ri...ght after the REMOVE button is pressed, with the
   //         a...rgument button set to FP_REMOVE.
   //  (4) Right ...after the APPLY button is pressed, with the
   //         argum...ent button set to FP_APPLY.
   //  (2) Right after this dialog box is successfully unmanaged,
   //         with the argument button set to the most recent button
   //         (FP_OK, FP_CANCEL, or FP_REMOVE) which was pressed
   //         prior to the unmanage.  Any other pressed buttons
   //         will be ignored for this purpose.  If the unmanage
   //         occurs sometime after FP_APPLY was pressed, or
   //         occurs before any of the above four buttons was
   //         pressed, the argument button will be set to FP_NONE.
   //
   //  Note that if FP_OK was pressed, but this dialog box chooses
   //  not to unmanage itself for any reason, eventually either
   //  FP_OK will be pressed successfully, or FP_APPLY or FP_CANCEL
   //  or FP_REMOVE will be pressed, triggering the call.
   //         
   // One important example of this use would be this:
   //   When the user wants to quit the application, and he responds
   //   with a YES when asked whether he wants to save his data,
   //   this class (which saves the data) can be called thusly:
   //          thisclass->setNotifyComplex(SLDelay *receiver);
   //          thisclass->setApplicationModal();
   //          thisclass->makeAndManage();
   //   Then, when the receiver gets this call:
   //           notifyComplex(SLDelay *thisclass, int ident)
   //   he will make these calls:
   //          thisclass->setNotifyComplex(NULL);
   //          thisclass->unsetApplicationModal();
   //   and then, only if ident == FP_OK, he will go ahead and quit
   //   the application.

   // This stuff will work only in these circumstances:
   //
   //  (1) The unmanage() virtual function in this class is not
   //         overridden.
   //  (2) The notify() virtual function in this class is not
   //         overridden.
   //  (3) The derived class does not register static functions
   //         as traps for the OK or APPLY or CANCEL or REMOVE buttons,
   //         but instead allows the okNotify() or applyNotify() or
   //         cancelNotify() or removeNotify() functions to be called
   //         (whether or not they are overridden).



//---------------------- constructors -----------------------//
//---------------------- constructors -----------------------//
//---------------------- constructors -----------------------//

SLDialog::SLDialog(Widget wparent, char *name,
                         const HelpCtx hctx,
                         const Boolean show_separator,
                         const int     num_colors,
                         const int     may_icon,
                         const int     screen_number)
 
     : SLShellContainer(wparent, name, hctx,
                        num_colors, may_icon, screen_number),
             _work            (NULL),
             _sep             (NULL),
             _bottom          (NULL),
             _title           (NULL),
             _button          (FP_NONE)    // not used.
{
  supportUnmadeDefaults(wparent);
  constructorHelper(show_separator);
}


SLDialog::SLDialog(SLDelay *slparent, char *name,
                         const HelpCtx hctx,
                         const Boolean show_separator,
                         const int     num_colors,
                         const int     may_icon,
                         const int     screen_number)
 
     : SLShellContainer(slparent, name, hctx,
                        num_colors, may_icon, screen_number),
             _work            (NULL),
             _sep             (NULL),
             _bottom          (NULL),
             _title           (NULL),
             _button          (FP_NONE)    // not used.
{
  if     (slparent->topWidget()) supportUnmadeDefaults(slparent->topWidget());
  else if(slparent->pW       ()) supportUnmadeDefaults(slparent->pW());
  constructorHelper(show_separator);
}


//------------------ constructor helper ------------------------//
//------------------ constructor helper ------------------------//
//------------------ constructor helper ------------------------//

void SLDialog::constructorHelper(const Boolean show_separator)
{
  if(show_separator) _sep = new SLSep      (this, "sep");
  _bottom                 = new SLSmartForm(this, "bottom");
  _bottom->showEvenSpacing();
           _attlist.add(_bottom, this,this,NULL,this   , 10,10,0,10);  
  if(_sep) _attlist.add(_sep   , this,this,NULL,_bottom, 10,10,0,10);  
}


//--------------------- destructor ---------------------------//
//--------------------- destructor ---------------------------//
//--------------------- destructor ---------------------------//

SLDialog::~SLDialog()
{
  if(_title) free(_title);
}


//--------------------- misc functions -----------------------//
//--------------------- misc functions -----------------------//
//--------------------- misc functions -----------------------//

void SLDialog::setTitle(char *title)
{
  if(made())
       {
       Widget shell = get_shell_widget(W());
       if(shell) XtVaSetValues(shell, XtNtitle, title, NULL);
       }
  else
       {
       if(_title) free(_title);
       _title = newstr(title);
       }
}


void SLDialog::setWorkArea(SLDelay *work)
{
  if(_work)
       {
       cout << "calling SLDialog::setWorkArea when work area is already set"
            << endl;
       assert(FALSE);
       }
  else if(!work)
       {
       cout << "calling SLDialog::setWorkArea with NULL argument"
            << endl;
       assert(FALSE);
       }
  else
       {
       _work = work;
       if(made()) _work->make();
       if(_sep) _attlist.add(_work, this,this,this,_sep   , 10,10,10,10);  
       else     _attlist.add(_work, this,this,this,_bottom, 10,10,10,10);  
       _attlist.tryAllAttachments();
       }
}


SLSmartForm *SLDialog::workArea()
{
  if(!_work)
       {
       _work = new SLSmartForm(this, "work");
       if(_sep) _attlist.add(_work, this,this,this,_sep   , 10,10,10,10);  
       else     _attlist.add(_work, this,this,this,_bottom, 10,10,10,10);  
       _attlist.tryAllAttachments();
       }
  return (SLSmartForm*)_work;
}
 

Boolean SLDialog::isManaged()
{
  return ( made() && XtIsManaged(topWidget()) );
}



//--------------- manage, unmanage, update ----------------------//
//--------------- manage, unmanage, update ----------------------//
//--------------- manage, unmanage, update ----------------------//

void SLDialog::manage()
{
/*
  if(isManaged())
      {
      Widget w = W();
      Widget shell = get_shell_widget(w);
      XRaiseWindow(XtDisplay(shell), XtWindow(shell));
      return;
      }
*/
  Boolean doit = preManageNotify();
  if(doit)
     {
     SLBase::manage();
     postManageNotify();
     }
}


void SLDialog::unmanage()
{
  if(!isManaged()) return;
  Boolean doit = preUnmanageNotify();
  if(doit)
     {
     SLBase::unmanage();
 //  XSync(XtDisplay(W()), FALSE);     // doesn't make any difference.
     XmUpdateDisplay(W());             // often doesn't work (timing?).
 //  XSync(XtDisplay(W()), FALSE);     // doesn't make any difference.
     postUnmanageNotify();
     }
}


void SLDialog::update()
{
  updateNotify();
  updateChildren();
}



//--------------------- notify -----------------------//
//--------------------- notify -----------------------//
//--------------------- notify -----------------------//

Boolean SLDialog::notify(SLPrim *gui)
{
  if(!gui) return TRUE;
  long ident = gui->id();
  long type  = gui->type();
  switch(type)
     {
     case SLPrim::_LONG  :
           return longNotify  (ident, gui->oldIvar(), gui->ivar());
     case SLPrim::_FLOAT :
           return floatNotify (ident, gui->oldFvar(), gui->fvar());
     case SLPrim::_DOUBLE:
           return doubleNotify(ident, gui->oldDvar(), gui->dvar());
     case SLPrim::_CHAR  :
           return charNotify  (ident, gui->oldCvar(), gui->cvar());
     default: break;
     }
  char *name = (char*)gui->instanceName();
  Boolean doit = FALSE;
  if     (strings_equal(name, NAME_OK    ))
               {
               doit = okNotify();
               }
  else if(strings_equal(name, NAME_APPLY ))
               {
               applyNotify();
               }
  else if(strings_equal(name, NAME_CANCEL))
               {
               doit = cancelNotify();
               }
  else if(strings_equal(name, NAME_REMOVE))
               {
               doit = removeNotify();
               }
  else if(strings_equal(name, NAME_UNDO  ))
               {
               undoNotify();
               }
  else if(strings_equal(name, NAME_RESET ))
               {
               resetNotify();
               }
  else
               {
               activateNotify(ident);
               }
  if(doit) unmanage();
  return TRUE;
}



//------------------- private add bottom push ---------------------//
//------------------- private add bottom push ---------------------//
//------------------- private add bottom push ---------------------//

SLpPush *SLDialog::privateAddBottomPush(char *name, char *label,
                                   long ident, AtrapFun *trap, void *data)
{
  SLpPush *gui = new SLpPush(_bottom, name, ident, label);
  if(trap) gui->setAtrap(trap, data);
  else     gui->setNotify(this);
  return gui;
}



//------------------- add bottom buttons ---------------------//
//------------------- add bottom buttons ---------------------//
//------------------- add bottom buttons ---------------------//

SLpPush *SLDialog::addBottomPush(char *name,
                                   long ident, AtrapFun *trap, void *data)
{
  return privateAddBottomPush(name, name, ident, trap, data);
}


SLpPush *SLDialog::addBottomOK(long ident, AtrapFun *trap, void *data)
{
  return privateAddBottomPush(NAME_OK, LABEL_OK, ident, trap, data);
}


SLpPush *SLDialog::addBottomApply(long ident, AtrapFun *trap, void *data)
{
  return privateAddBottomPush(NAME_APPLY, LABEL_APPLY, ident, trap, data);
}


SLpPush *SLDialog::addBottomCancel(long ident, AtrapFun *trap, void *data)
{
  return privateAddBottomPush(NAME_CANCEL, LABEL_CANCEL, ident, trap, data);
}


SLpPush *SLDialog::addBottomRemove(long ident, AtrapFun *trap, void *data)
{
  return privateAddBottomPush(NAME_REMOVE, LABEL_REMOVE, ident, trap, data);
}


SLpPush *SLDialog::addBottomUndo(long ident, AtrapFun *trap, void *data)
{
  return privateAddBottomPush(NAME_UNDO, LABEL_UNDO, ident, trap, data);
}


SLpPush *SLDialog::addBottomReset(long ident, AtrapFun *trap, void *data)
{
  return privateAddBottomPush(NAME_RESET, LABEL_RESET, ident, trap, data);
}


SLpPush *SLDialog::addBottomKeyhelp(char *helpname)
{
  SLpPush *gui = new SLpPush(_bottom, NAME_KEYHELP, 0, LABEL_KEYHELP);
  if(helpname) gui->showHelpWhenPressed(helpname);
  else         gui->showHelpWhenPressed("wbox_keyhelp");
  return gui;
}


SLpPush *SLDialog::addBottomHelp(char *helpname)
{
  SLpPush *gui = new SLpPush(_bottom, NAME_HELP, 0, LABEL_HELP);
  if(helpname) gui->showHelpWhenPressed(helpname);
  else         gui->showHelpWhenPressed(_name);
  return gui;
}



//---------------------------- make -----------------------------//
//---------------------------- make -----------------------------//
//---------------------------- make -----------------------------//

static String defres[]= {
    ".autoUnmanage:          False",
    ".resizePolicy:          RESIZE_NONE",
    ".horizontalSpacing:     4",
    ".verticalSpacing:       4",
    "*bottom*marginWidth:    8",
    "*bottom*ok.marginWidth: 16",
    NULL };


Widget SLDialog::make(Widget p)
{
  ShellStatMsg *bld_info = NULL;
  if(!made())
       {
       Widget w = SLShellContainer::make(p);
   //  ShellStatMsg bld_info(wParent(), "Building Popup...");
   //  bld_info = new ShellStatMsg(wParent(), "Building Popup...");
       char *message = newstrcat("Building ", _name, " Popup...", NULL);
       bld_info = new ShellStatMsg(wParent(), message);
       free(message);
       if(!w)
           {
           setDefaultResources(XtDisplay(wParent()), instanceName(), defres);

     /////  The following code sets the default shell title to _name.
           char *shellname, *defres2[2];
           shellname = newstrcat(_name, "_popup", NULL);
           defres2[0] = newstrcat(".title: ", _name, NULL);
           defres2[1] = NULL;
           setDefaultResources(XtDisplay(wParent()), shellname, defres2);
           free(shellname);
           free(defres2[0]);
     /////  The above code sets the default shell title to _name.

      //   w = XmCreateFormDialog(wParent(), _name, NULL, 0);
           w = creDialog(wParent(), topClass());

           setTopWidget(w);
           if(_title) { setTitle(_title); free(_title); _title = NULL; }
           }
       makeNotify();
       }
  makeChildren();
  _attlist.tryAllAttachments();
  if(bld_info) delete bld_info;
  return topWidget();
}


//-------------------------- end -------------------------------//
//-------------------------- end -------------------------------//
//-------------------------- end -------------------------------//
