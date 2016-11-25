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

//---------------------- prim_support.cc ---------------------------//
//---------------------- prim_support.cc ---------------------------//
//---------------------- prim_support.cc ---------------------------//

//        implementation file for the PrimSupport class
//   not derived from any class (except virtual derivations)
//                     subdirectory sl
//            this class should not be instantiated
//           to be inherited as a second inheritance


// Definitions:
//
//  A "GUI" is a graphical user interface component such as a
//         widget, or a windowless data field.
//
//  A "changeable" resource is any of the GUI resources inherited by
//         this class or derived classes.
//
//  An "interactive" resource is a resource which the user can change
//         by operating on the GUI.  This also would be a resource
//         which can be reset to a default.  The interactive resources
//         are a subset of the changeable resources.  GUIs which have
//         an interactive resource must have a type of _LONG, _FLOAT,
//         _DOUBLE, or _CHAR.
//
//  An "interactive" GUI is a GUI which responds to user interaction.
//         An interactive GUI can have ONE interactive resource (e.g.
//         a text widget or toggle button) or NO interactive resource
//         (e.g. a pushbutton).  An interactive GUI without an interactive
//         resource must have a type of _ACTIVE.


//  The type of the GUI must be specified as follows:
//  type of GUI   description of GUI
//  ------------  ------------------
//  == _INACTIVE  Does NOT respond to user interaction (e.g. label).
//  >= _ACTIVE    Interactive GUI (responds to user interaction).
//  == _ACTIVE    Interactive GUI which does NOT have a resource
//                  which changes as a result of user interaction
//                  (e.g. pushbutton).
//  >= _LONG      Interactive GUI which DOES have a resource which
//                  changes as a result of the user interaction
//                  (e.g. text widget, toggle button, scrollbar).
//                  The type must correspond to the type of the
//                  interactive resource (_LONG, _FLOAT, _DOUBLE,
//                  or _CHAR).


#include <string.h>
#include "sl/prim_support.hh"
#include <iostream.h>
#include "sl/family_list.hh"
#include "sl/sl_delay.hh"
 
FamilyList *PrimSupport::_family = new FamilyList();

//------------------- constructors -------------------------------//
//------------------- constructors -------------------------------//
//------------------- constructors -------------------------------//

PrimSupport::PrimSupport(long ident, long type)
         : ResourceList(),
           SenseGuiResource(),
            IvarGuiResource(),
            FvarGuiResource(),
            DvarGuiResource(),
            CvarGuiResource(),
     _ident         (ident),
     _type          (type ),
     _traptype      (_STATIC),
     _data          (NULL ),
     _focusindata   (NULL ),
     _focusoutdata  (NULL ),
     _focusintrap   (NULL ),
     _focusouttrap  (NULL ),
     _focusintrapf  (NULL ),
     _focusouttrapf (NULL )
{
  _t._atrap = NULL;
}



//------------------------ destructor ----------------------------//
//------------------------ destructor ----------------------------//
//------------------------ destructor ----------------------------//

PrimSupport::~PrimSupport(void)
{
}



//-------------- manage update functions -----------------//
//-------------- manage update functions -----------------//
//-------------- manage update functions -----------------//

void PrimSupport::addUpdateFunction(void(*fun)(void *data), void *data)
{
  _family->add(fun, data);
}


void PrimSupport::removeUpdateFunction(void(*fun)(void *data), void *data)
{
  _family->remove(fun, data);
}


void PrimSupport::callUpdateFunctions(void)
{
  _family->callFamily();
}


void PrimSupport::updateEverything(void)
{
//  callUpdateFunctions();
  SLDelay::updateAll();
  callUpdateFunctions();
}



//-------functions below might need to be overridden -----------//
//-------functions below might need to be overridden -----------//
//-------functions below might need to be overridden -----------//
//-------functions below might need to be overridden -----------//
//-------functions below might need to be overridden -----------//
//-------functions below might need to be overridden -----------//
//-------functions below might need to be overridden -----------//


//---------------------- update self -------------------------------//
//---------------------- update self -------------------------------//
//---------------------- update self -------------------------------//

// This method is to be called (with force == FALSE) from update(),
//    which then should call updateChildren().
// This method is also to be called (with force == FALSE) from
//    set...() methods, after the appropriate setup...() method
//    is called.
// This method is also to be called (with force == TRUE) from make().

// The public function update() is to be called whenever the
//    GUI resources are to be updated from application variables
//    or functions previously registered with setup...() calls.

// Updating is conditionally done if force == FALSE, and always otherwise.
//    Normally, force should be set to TRUE when the actual GUI is
//    first made (or if the GUI is not a widget and must be
//    re-exposed), and FALSE otherwise.

// The function update() is normally called from
//    a linked list when returning to the event loop.  This happens
//    automatically when returning from an event on any GUI (i.e.
//    within any of the trap functions in this class).  This also
//    happens when update() or updateAll() is called by the application.

//  Do the conditional resource setting here.
//  This function need not be overridden if each resource can
//    be set independently.
//  Resource settings may be done by code within this function if
//    desired, rather than calling the set... functions below.
//  Resource settings which are not independent may have to be made
//    simultaneously by code in this function, rather than calling
//    the set... functions below.

void PrimSupport::updateSelf(Boolean force)
{
  updatePacket(force);
  if(changedSense()) setSenseResource();
  if(changedIvar ()) setIvarResource ();
  if(changedFvar ()) setFvarResource ();
  if(changedDvar ()) setDvarResource ();
  if(changedCvar ()) setCvarResource ();
  //// optionally omit irrelevant lines above.
}



//-------functions beyond here do not need to be overridden ----------//
//-------functions beyond here do not need to be overridden ----------//
//-------functions beyond here do not need to be overridden ----------//
//-------functions beyond here do not need to be overridden ----------//
//-------functions beyond here do not need to be overridden ----------//
//-------functions beyond here do not need to be overridden ----------//
//-------functions beyond here do not need to be overridden ----------//


//--------- public functions to register traps ---------------------//
//--------- public functions to register traps ---------------------//
//--------- public functions to register traps ---------------------//

       // the argument can be a NULL to un-register the trap

static void whoops(char *msg)
      { cout << "calling " << msg << "() when not relevant" << endl; }

void PrimSupport::setFocusinTrap  (FocusFun *trap, void *data)
      { _focusintrap = trap; _focusindata = data; _focusintrapf = NULL; }

void PrimSupport::setFocusoutTrap (FocusFun *trap, void *data)
      { _focusouttrap = trap; _focusoutdata = data; _focusouttrapf = NULL; }

void PrimSupport::setFocusinTrap  (FocusFunF *trap)
      { _focusintrapf = trap; _focusintrap = NULL; }

void PrimSupport::setFocusoutTrap (FocusFunF *trap)
      { _focusouttrapf = trap; _focusouttrap = NULL; }


void PrimSupport::setNotify (SLDelay *gui)
      { _traptype = _NOTIFY; _t._notify = gui; }

void PrimSupport::setAtrap (AtrapFun *trap, void *data)
      { if(_type != _ACTIVE) { whoops("setAtrap"); return; }
        _traptype = _STATIC; _t._atrap = trap; _data = data; }

void PrimSupport::setItrap (ItrapFun *trap, void *data)
      { if(_type != _LONG) { whoops("setItrap"); return; }
        _traptype = _STATIC;  _t._itrap = trap; _data = data; }

void PrimSupport::setFtrap (FtrapFun *trap, void *data)
      { if(_type != _FLOAT) { whoops("setFtrap"); return; }
        _traptype = _STATIC;  _t._ftrap = trap; _data = data; }

void PrimSupport::setDtrap (DtrapFun *trap, void *data)
      { if(_type != _DOUBLE) { whoops("setDtrap"); return; }
        _traptype = _STATIC;  _t._dtrap = trap; _data = data; }

void PrimSupport::setCtrap (CtrapFun *trap, void *data)
      { if(_type != _CHAR) { whoops("setCtrap"); return; }
        _traptype = _STATIC;  _t._ctrap = trap; _data = data; }


void PrimSupport::setAtrap (AtrapFunF *trap)
      { if(_type != _ACTIVE) { whoops("setAtrap"); return; }
        _traptype = _FORTRAN; _t._atrapf = trap; }

void PrimSupport::setItrap (ItrapFunF *trap)
      { if(_type != _LONG) { whoops("setItrap"); return; }
        _traptype = _FORTRAN;  _t._itrapf = trap; }

void PrimSupport::setFtrap (FtrapFunF *trap)
      { if(_type != _FLOAT) { whoops("setftrap"); return; }
        _traptype = _FORTRAN;  _t._ftrapf = trap; }

void PrimSupport::setDtrap (DtrapFunF *trap)
      { if(_type != _DOUBLE) { whoops("setDtrap"); return; }
        _traptype = _FORTRAN;  _t._dtrapf = trap; }

void PrimSupport::setCtrap (CtrapFunF *trap)
      { if(_type != _CHAR) { whoops("setCtrap"); return; }
        _traptype = _FORTRAN;  _t._ctrapf = trap; }



//------------- reset interactive resources to defaults -------//
//------------- reset interactive resources to defaults -------//
//------------- reset interactive resources to defaults -------//

//   This function is to be called whenever the resource values
//       are to be reset from defaults (e.g. from the app-defaults
//       file).
//   This function is normally called from a linked list when the user
//       requests defaults to be loaded, overriding the current program
//       values.  This happens when resetAll() or reset() is
//       called by the program.
//   This function can also be called after this object is created
//       (whether or not the actual GUI has been made) if the default
//       resources are to override the preset program value.
//   Traps are called after the GUI has been reset, just as if the
//       user changed the GUI resource values interactively.
//   It would be best to call updateEverything() after all GUIs have
//       been reset.  This function only calls update() for this GUI
//       because it is assumed that many GUIs are being reset at the
//       same time, and it would be wasteful to call updateEverything()
//       over and over again for each GUI.


void PrimSupport::reloadSelf(Boolean do_method)
{
  if(!_allow_reload_default) return;
  switch(_type)
     {
     case _LONG  :  callItrap(ivarDefault(), do_method); break;
     case _FLOAT :  callFtrap(fvarDefault(), do_method); break;
     case _DOUBLE:  callDtrap(dvarDefault(), do_method); break;
     case _CHAR  :  callCtrap(cvarDefault(), do_method); break;
     default     :  return;
     }
  updateSelf();
}


void PrimSupport::reloadSystemSelf(Boolean do_method)
{
  if(!_allow_reload_system_default) return;
  switch(_type)
     {
     case _LONG  :  callItrap(ivarSystemDefault(), do_method); break;
     case _FLOAT :  callFtrap(fvarSystemDefault(), do_method); break;
     case _DOUBLE:  callDtrap(dvarSystemDefault(), do_method); break;
     case _CHAR  :  callCtrap(cvarSystemDefault(), do_method); break;
     default     :  return;
     }
  updateSelf();
}



//---------- protected functions to call focus traps ---------------//
//---------- protected functions to call focus traps ---------------//
//---------- protected functions to call focus traps ---------------//

// These are to be called whenever the GUI gets or loses the input focus.

void PrimSupport::callFocusinTrap(void)
{
  if(!_focusintrap && !_focusintrapf) return;
  long ident = id();
  if     (_focusintrap ) _focusintrap (_focusindata, ident);
  else if(_focusintrapf) _focusintrapf(             &ident);
  updateEverything();
}

void PrimSupport::callFocusoutTrap(void)
{
  if(!_focusouttrap && !_focusintrapf) return;
  long ident = id();
  if     (_focusouttrap ) _focusouttrap (_focusoutdata, ident);
  else if(_focusouttrapf) _focusouttrapf(              &ident);
  updateEverything();
}



//---------- protected function to call the activate trap ----------//
//---------- protected function to call the activate trap ----------//
//---------- protected function to call the activate trap ----------//

//  This is to be called when the GUI is activated.
//  Normally, this would be for a GUI which has no interactive
//    resource (e.g. a pushbutton).

void PrimSupport::callAtrap(void)
{
  long ident = id();
  if     (_traptype == _STATIC  && _t._atrap ) _t._atrap (_data, ident);
  else if(_traptype == _FORTRAN && _t._atrapf) _t._atrapf(      &ident);
  else if(_traptype == _NOTIFY  && _t._notify) trapHelper(_t._notify);
  else                                         trapHelper(NULL);
  updateEverything();
}


//---------- protected functions to call value-changed traps --------//
//---------- protected functions to call value-changed traps --------//
//---------- protected functions to call value-changed traps --------//

//  If a new value is obtained from user interaction, these
//  steps should be taken (normally from within a callback or
//  event handler):
//    (1) Get the new value from the GUI by calling one of these
//          functions (or by any other means):
//                 ivarResource()      fvarResource()
//                 dvarResource()      cvarResource()
//    (2) Pass this new value to one of the trap routines below.
//    (3) Call updateEverything();

//  If the new value is a default value, these steps should be taken
//  (normally from within the resetSelf() function):
//    (1) Get the new value by calling one of these functions (or
//          by any other means):
//                 ivarDefault()      fvarDefault()
//                 dvarDefault()      cvarDefault()
//    (2) Pass this new value to one of the trap routines below.
//    (3) Wait until all new values for all GUIs are reset, then
//          call updateEverything();

 
#define TRAP(callItrap, long2, ivar, setNewIvar,              \
                        _itrap, _itrapf, setIvarResource,     \
                        setupIvarValue, oldIvar)              \
void PrimSupport::callItrap(long2 value, Boolean do_method)   \
 {                                                            \
  setNewIvar(value);                                          \
  Boolean doit = TRUE;                                        \
  if(do_method)                                               \
     {                                                        \
     long  ident  = id();                                     \
     long2 oldvar = oldIvar();                                \
     long2 newvar = ivar();                                   \
     if     (_traptype == _STATIC  && _t._itrap )             \
                  _t._itrap (_data, ident,  oldvar,  newvar); \
     else if(_traptype == _FORTRAN && _t._itrapf)             \
                  _t._itrapf(      &ident, &oldvar, &newvar); \
     else if(_traptype == _NOTIFY  && _t._notify)             \
                  doit = trapHelper(_t._notify);              \
     else         doit = trapHelper(NULL);                    \
     }                                                        \
  setIvarResource();                                          \
  if(!doit) setupIvarValue(oldIvar());                        \
}


TRAP(callItrap, long  , ivar, setNewIvar, _itrap, _itrapf,
                     setIvarResource, setupIvarValue, oldIvar)
TRAP(callFtrap, float , fvar, setNewFvar, _ftrap, _ftrapf,
                     setFvarResource, setupFvarValue, oldFvar)
TRAP(callDtrap, double, dvar, setNewDvar, _dtrap, _dtrapf,
                     setDvarResource, setupDvarValue, oldDvar)


void PrimSupport::callCtrap(char *value, Boolean do_method)
{
  setNewCvar(value);
  Boolean doit = TRUE;
  if(do_method)
     {
     long  ident  = id();
     char *oldvar = newstr(oldCvar());
     char *newvar = newstr(cvar());
     if     (_traptype == _STATIC  && _t._atrap )
                  _t._ctrap (_data, ident, oldvar, newvar);
     else if(_traptype == _FORTRAN && _t._atrapf)
                  _t._ctrapf(      &ident, oldvar, newvar);
     else if(_traptype == _NOTIFY  && _t._notify)
                  doit = trapHelper(_t._notify);
     else         doit = trapHelper(NULL);
     if (oldvar) free(oldvar);
     if (newvar) free(newvar);
     }
  setCvarResource();
  if(!doit) setupCvarValue(oldCvar());
}



//---------------------------- end --------------------------------------//
//---------------------------- end --------------------------------------//
//---------------------------- end --------------------------------------//
