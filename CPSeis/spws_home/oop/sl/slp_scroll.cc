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

//---------------------- slp_scroll.cc ----------------------------------//
//---------------------- slp_scroll.cc ----------------------------------//
//---------------------- slp_scroll.cc ----------------------------------//

//         implementation file for the SLpScroll class
//                 derived from the SLpBase class
//                       subdirectory sl

#include "sl/slp_scroll.hh"
#include "named_constants.h"
#include <Xm/ScrollBar.h>


//-----------------constructor----------------------------------------//
//-----------------constructor----------------------------------------//
//-----------------constructor----------------------------------------//


SLpScroll::SLpScroll (SLDelay *slparent, char *name, long ident,
       long direction)
    : SLpBase(slparent, name, xmScrollBarWidgetClass, ident, _LONG),
      IminGuiResource(),
      ImaxGuiResource(),
      SliderGuiResource(),
      IstepGuiResource(),
      IpageGuiResource(),
       _direction (direction)
{
  createSenseResource();
  createIvarResource();
  createIminResource();
  createImaxResource();
  createSliderResource();
  createIstepResource();
  createIpageResource();
  if(slparent->topWidget()) make();
}


SLpScroll::SLpScroll (Widget wparent, char *name, long ident,
       long direction)
    : SLpBase(wparent, name, xmScrollBarWidgetClass, ident, _LONG),
      IminGuiResource(),
      ImaxGuiResource(),
      SliderGuiResource(),
      IstepGuiResource(),
      IpageGuiResource(),
       _direction (direction)
{
  createSenseResource();
  createIvarResource();
  createIminResource();
  createImaxResource();
  createSliderResource();
  createIstepResource();
  createIpageResource();
  make();
}


SLpScroll::SLpScroll (Widget w, long ident)
    : SLpBase(w, ident, _LONG),
      IminGuiResource(),
      ImaxGuiResource(),
      SliderGuiResource(),
      IstepGuiResource(),
      IpageGuiResource(),
       _direction (_UNKNOWN)
{
  createSenseResource();
  createIvarResource();
  createIminResource();
  createImaxResource();
  createSliderResource();
  createIstepResource();
  createIpageResource();
  make();
}


//------------------ destructor -------------------------//
//------------------ destructor -------------------------//
//------------------ destructor -------------------------//

SLpScroll::~SLpScroll(void)
{
}


//------------------ update self -----------------------//
//------------------ update self -----------------------//
//------------------ update self -----------------------//


void SLpScroll::updateSelf(Boolean force)
{
  updatePacket(force);
  if(!topWidget()) return;
  if(changedIvar() || changedImin() || changedImax() ||
     changedSlider() || changedIstep() || changedIpage())
       {
       long value   = ivar  ();
       long minimum = imin  ();
       long maximum = imax  ();
       long slide   = slider();
       long step    = istep ();
       long page    = ipage ();
       maximum = MaximumValue(minimum, maximum);
       value   = ConstrainValue(value, minimum, maximum);
       slide   = ConstrainValue(slide, 1, maximum - value + 1);
       step    = MaximumValue(step, 1);
       page    = MaximumValue(page, step);
       XtVaSetValues(topWidget(), XmNvalue        , value,
                                  XmNminimum      , minimum,
                                  XmNmaximum      , maximum + 1,
                                  XmNsliderSize   , slide,
                                  XmNincrement    , step,
                                  XmNpageIncrement, page,
                                  NULL);
       }
}

 

//------------ set and get resources ---------------------//
//------------ set and get resources ---------------------//
//------------ set and get resources ---------------------//


void SLpScroll::setIvarResource(void)
{
  updateSelf(TRUE);
}


long SLpScroll::ivarResource(void) const
{
  if(!topWidget()) return 0;
  int value;
  XtVaGetValues(topWidget(), XmNvalue, &value, NULL);
  return value;
}



//-------------------- event handler --------------------------//
//-------------------- event handler --------------------------//
//-------------------- event handler --------------------------//


void SLpScroll::buttonUpEventHandler(Widget /* w */, XtPointer user,
                                     XEvent *event)
{
  if(event->type == ButtonRelease)
      {
      SLpScroll *gui = (SLpScroll*)user;
      gui->updateSelf(TRUE);
      }
}



//-------------------- make ------------------------------------//
//-------------------- make ------------------------------------//
//-------------------- make ------------------------------------//

static String defres[]= {
    ".orientation:         VERTICAL",
    ".traversalOn:         True",
    ".highlightThickness:  2",
    NULL };
 

Widget SLpScroll::make(Widget p)
{
  if(!made())
    {
    Widget w = SLDelay::make(p);
    if(!w)
       {
       setDefaultResources(XtDisplay(wParent()), instanceName(), defres);
       Arg args[5];
       int i = 0;
       if(_direction == _VERTICAL)
            { XtSetArg(args[i], XmNorientation, XmVERTICAL); i++; }
       else if(_direction == _HORIZONTAL)
            { XtSetArg(args[i], XmNorientation, XmHORIZONTAL); i++; }
       w = XmCreateScrollBar(wParent(), (char*)instanceName(), args, i);
       setTopWidget(w);
       XtManageChild(w);
       }
    setTraversal(_traversal);
    install_help();
    XtAddEventHandler(w, FocusChangeMask, FALSE,
                      (XtEventHandler)focusEventHandler, (XtPointer)this);
    XtAddEventHandler(w, ButtonReleaseMask, FALSE,
                   (XtEventHandler)buttonUpEventHandler, (XtPointer)this);
    XtAddCallback(w, XmNvalueChangedCallback,
                        (XtCallbackProc)integerCallback, (XtPointer)this);
    XtAddCallback(w, XmNdragCallback,
                        (XtCallbackProc)integerCallback, (XtPointer)this);
    updateSelf(TRUE);
    }
  return topWidget();
}



//------------------------ end ----------------------------------//
//------------------------ end ----------------------------------//
//------------------------ end ----------------------------------//
