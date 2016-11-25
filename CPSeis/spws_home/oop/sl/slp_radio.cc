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

//---------------------- slp_radio.cc ----------------------------------//
//---------------------- slp_radio.cc ----------------------------------//
//---------------------- slp_radio.cc ----------------------------------//

//         implementation file for the SLpRadio class
//                derived from the SLpBase class
//                      subdirectory sl

//   Radio buttons are "related" (i.e. work with each other) if they
//     share the same pointer (_ipoint) to a user variable, or the
//     same update function (_ifun).
//   All related radio buttons with the same value of abs(_ident) will
//     be ON (depressed) or OFF (not depressed) together.
//   A radio button is ON if AbsoluteValue(_ident) == _ivar (copy of user
//     variable pointed to by _ipoint, or value returned by _ifun),
//     and OFF otherwise.

//   The usual way to work with a set of related radio buttons is:
//     To assign each one a different positive value of _ident if you
//       require one-of-many operation.
//     To assign each one a different negative value of _ident if you
//       require zero-OR-one-of-many operation.

//   Under user control:
//     If the user presses the button, all other related buttons (with
//       a different value of abs(_ident)) will be turned OFF.
//     If the user presses the button, and the button is OFF, it will
//       be turned ON.
//     If the user presses the button, and the button is ON, it will
//       remain ON if _ident >= 0, and turned OFF if _ident < 0.
//     If _ident == 0: This radio button will always be ON if all other
//       buttons are OFF.
//     In a trap, the user variable will always be reset either to
//       abs(_ident) or to zero.

//   Under program control:
//     Setting user variable to zero will turn OFF all buttons with
//       _ident != 0, and turn ON any buttons with _ident == 0.
//     Setting user variable to anything else will turn ON any buttons
//       with abs(_ident) == user variable, and turn OFF all other buttons.
//
//   If the program resets a button by using a set... or setup...
//   function, only that specific button will get changed.  To make 
//   everything work correctly:
//       The same pointer (_ipoint) or the same function (_ifun) must
//          be registered with all related buttons.
//       The function updateGuis() must be called if you are not in a
//          trap called by a class derived from GuiBase.

#include "sl/slp_radio.hh"
#include "sl/radio_list.hh"
#include "sl/psuedo_widget.hh"
#include "named_constants.h"
#include <Xm/ToggleB.h>
 


//---------------- constructors ----------------------------------//
//---------------- constructors ----------------------------------//
//---------------- constructors ----------------------------------//


SLpRadio::SLpRadio(SLDelay *slparent, char *name, long ident,
                        char *label, RadioList *radio_list)
    : SLpBase(slparent, name, xmToggleButtonWidgetClass, ident, _LONG),
            _radio_list (radio_list)
{
  createCvarResource();
  createIvarResource();
  createSenseResource(); 
  setupCvarValue(label);
  setNewIvar(ivarDefault());
  if(_radio_list) _radio_list->add(this);
  if(slparent->topWidget()) make();
}

SLpRadio::SLpRadio(Widget wparent, char *name, long ident,
                        char *label, RadioList *radio_list)
    : SLpBase(wparent, name, xmToggleButtonWidgetClass, ident, _LONG),
            _radio_list (radio_list)
{
  createCvarResource();
  createIvarResource();
  createSenseResource(); 
  setupCvarValue(label);
  setNewIvar(ivarDefault());
  if(_radio_list) _radio_list->add(this);
  make();
}



//-------------------- radio list going away -------------------//
//-------------------- radio list going away -------------------//
//-------------------- radio list going away -------------------//

          // to be called from RadioList destructor.

void SLpRadio::radioListGoingAway()
{
  _radio_list = NULL;
}



//------------------ destructor -------------------------//
//------------------ destructor -------------------------//
//------------------ destructor -------------------------//

SLpRadio::~SLpRadio(void)
{
  if(_radio_list) _radio_list->remove(this);
}



//------------ set and get resources ---------------------//
//------------ set and get resources ---------------------//
//------------ set and get resources ---------------------//


void SLpRadio::setIvarResource(void)
{
  if(!topWidget()) return;
  Boolean state = (ivar() == AbsoluteValue(_ident));
  XmToggleButtonSetState(topWidget(), state, FALSE);
}


void SLpRadio::setCvarResource(void)
{
  setCompoundStringResource(XmNlabelString);
}


long SLpRadio::ivarResource(void) const
{
  if(_ident > 0) return _ident;      // do not allow radio button to be off.
  if(!topWidget()) return 0;
  Boolean state = XmToggleButtonGetState(topWidget());
  if(state) return AbsoluteValue(_ident);
  return 0;
}


long SLpRadio::ivarDefault(void) const
{
  if(!pW()) return ivar();
  Boolean state = pW()->togDef();
  if(state) return AbsoluteValue(_ident);
  return 0;
}



//---------------------- make -----------------------------//
//---------------------- make -----------------------------//
//---------------------- make -----------------------------//


static String defres[]= {
    ".marginHeight:       0",
    ".marginTop:          0",
    ".marginBottom:       0",
    NULL };


Widget SLpRadio::make(Widget p)
{
  if(!made())
     {
     Widget w = SLDelay::make(p);
     if(!w)
        {
        setDefaultResources(XtDisplay(wParent()), instanceName(), defres);
        Arg args[5];
        int i = 0;
        XtSetArg(args[i], XmNindicatorType , XmONE_OF_MANY); i++;
        w = XmCreateToggleButton(wParent(), (char*)instanceName(), args, i);
        setTopWidget(w);
        XtManageChild(w);
        }
     setTraversal(_traversal);
     install_help();
     XtAddEventHandler(w, FocusChangeMask, FALSE,
                       (XtEventHandler)focusEventHandler, (XtPointer)this);
     XtAddCallback(w, XmNvalueChangedCallback,
                         (XtCallbackProc)integerCallback, (XtPointer)this);
     updateSelf(TRUE);
     }
  return topWidget();
}



//------------------------ end ----------------------------------//
//------------------------ end ----------------------------------//
//------------------------ end ----------------------------------//
