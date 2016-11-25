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

//---------------------- slp_toggle.cc ----------------------------------//
//---------------------- slp_toggle.cc ----------------------------------//
//---------------------- slp_toggle.cc ----------------------------------//

//         implementation file for the SLpToggle class
//                 derived from the SLpBase class
//                       subdirectory sl

#include "sl/slp_toggle.hh"
#include "sl/psuedo_widget.hh"
#include <Xm/ToggleB.h>


//-----------------constructor----------------------------------------//
//-----------------constructor----------------------------------------//
//-----------------constructor----------------------------------------//


SLpToggle::SLpToggle (SLDelay *slparent, char *name, long ident,
                                     char *label, Boolean radio_button)
    : SLpBase(slparent, name, xmToggleButtonWidgetClass, ident, _LONG),
        _radio_button (radio_button)
{
  createCvarResource(); 
  createIvarResource();
  createSenseResource();
  setupCvarValue(label);
  setNewIvar(ivarDefault());
  if(slparent->topWidget()) make();
}


SLpToggle::SLpToggle (Widget wparent, char *name, long ident,
                                     char *label, Boolean radio_button)
    : SLpBase(wparent, name, xmToggleButtonWidgetClass, ident, _LONG),
        _radio_button (radio_button)
{
  createCvarResource(); 
  createIvarResource();
  createSenseResource();
  setupCvarValue(label);
  setNewIvar(ivarDefault());
  make();
}


SLpToggle::SLpToggle (Widget w, long ident,
                                     char *label)
    : SLpBase(w, ident, _LONG),
        _radio_button (FALSE)
{
  createCvarResource(); 
  createIvarResource();
  createSenseResource();
  setupCvarValue(label);
  setNewIvar(ivarDefault());
  make();
}


//------------------ destructor -------------------------//
//------------------ destructor -------------------------//
//------------------ destructor -------------------------//

SLpToggle::~SLpToggle(void)
{
}



//------------ set and get resources ---------------------//
//------------ set and get resources ---------------------//
//------------ set and get resources ---------------------//


void SLpToggle::setIvarResource(void)
{
  if(!topWidget()) return;
  XmToggleButtonSetState(topWidget(), (int)ivar(), FALSE);
}


void SLpToggle::setCvarResource(void)
{
  setCompoundStringResource(XmNlabelString);
}


long SLpToggle::ivarResource(void) const
{
  if(!topWidget()) return 0;
  return XmToggleButtonGetState(topWidget());
}


long SLpToggle::ivarDefault(void) const
{
  if(pW()) return pW()->togDef();
  else     return ivar();
}



//------------------ make ------------------------------------//
//------------------ make ------------------------------------//
//------------------ make ------------------------------------//

static String defres[]= {
    ".marginHeight:       0",
    ".marginTop:          0",
    ".marginBottom:       0",
    NULL };


Widget SLpToggle::make(Widget p)
{
  if(!made())
     {
     Widget w = SLDelay::make(p);
     if(!w)
        {
        setDefaultResources(XtDisplay(wParent()), instanceName(), defres);
        Arg args[5];
        int i = 0;
        if(_radio_button)
           {
           XtSetArg(args[i], XmNindicatorType , XmONE_OF_MANY); i++;
           }
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
