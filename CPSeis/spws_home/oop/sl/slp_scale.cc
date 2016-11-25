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

//---------------------- slp_scale.cc ----------------------------------//
//---------------------- slp_scale.cc ----------------------------------//
//---------------------- slp_scale.cc ----------------------------------//

//         implementation file for the SLpScale class
//                 derived from the SLpBase class
//                       subdirectory sl

#include "sl/slp_scale.hh"
#include "sl/psuedo_widget.hh"
#include "named_constants.h"
#include <Xm/Scale.h>
 


//-----------------constructor----------------------------------------//
//-----------------constructor----------------------------------------//
//-----------------constructor----------------------------------------//


SLpScale::SLpScale (SLDelay *slparent, char *name, long ident, char *label,
       long direction, Boolean drag)
    : SLpBase(slparent, name, xmScaleWidgetClass, ident, _LONG),
      IminGuiResource(),
      ImaxGuiResource(),
      IpageGuiResource(),
       _direction (direction),
       _drag      (drag)
{
  createCvarResource();
  createIvarResource();
  createSenseResource();
  createIminResource();
  createImaxResource();
  createIpageResource();
  setupCvarValue(label);
  setNewIvar(ivarDefault());
  if(slparent->topWidget()) make();
}


SLpScale::SLpScale (Widget wparent, char *name, long ident, char *label,
       long direction, Boolean drag)
    : SLpBase(wparent, name, xmScaleWidgetClass, ident, _LONG),
      IminGuiResource(),
      ImaxGuiResource(),
      IpageGuiResource(),
       _direction (direction),
       _drag      (drag)
{
  createCvarResource();
  createIvarResource();
  createSenseResource();
  createIminResource();
  createImaxResource();
  createIpageResource();
  setupCvarValue(label);
  setNewIvar(ivarDefault());
  make();
}


SLpScale::SLpScale (Widget w, long ident, char *label, Boolean drag)
    : SLpBase(w, ident, _LONG),
      IminGuiResource(),
      ImaxGuiResource(),
      IpageGuiResource(),
       _direction (_UNKNOWN),
       _drag      (drag)
{
  createCvarResource();
  createIvarResource();
  createSenseResource();
  createIminResource();
  createImaxResource();
  createIpageResource();
  setupCvarValue(label);
  setNewIvar(ivarDefault());
  make();
}


//------------------ destructor -------------------------//
//------------------ destructor -------------------------//
//------------------ destructor -------------------------//

SLpScale::~SLpScale(void)
{
}


//------------------ update self -----------------------//
//------------------ update self -----------------------//
//------------------ update self -----------------------//


void SLpScale::updateSelf(Boolean force)
{
  updatePacket(force);
  if(!topWidget()) return;
  if(changedImin() || changedImax()  ||
     changedIvar() || changedSense() || changedIpage())
       {
       long minimum = imin ();
       long maximum = imax ();
       long value   = ivar ();
       long s       = sense();
       long page    = ipage ();
       maximum = MaximumValue(maximum, minimum);
       value   = ConstrainValue(value, minimum, maximum);
       page    = ConstrainValue(page, 1, maximum - minimum);
       if(maximum == minimum)
            {
            s = 0;
            minimum--;
            maximum++;
            page = 1;
            }
       SLBase::setSensitivity((Boolean)s);
       if(s) XtVaSetValues(topWidget(), XmNforeground, _foreground, NULL);
       else  XtVaSetValues(topWidget(), XmNforeground, _dim       , NULL);
       XtVaSetValues(topWidget(), XmNvalue        , value,
                                  XmNminimum      , minimum,
                                  XmNmaximum      , maximum,
                                  XmNscaleMultiple, page,
                                  NULL);
       }
  if(changedCvar()) setCompoundStringResource(XmNtitleString);
}

 

//------------ set and get resources ---------------------//
//------------ set and get resources ---------------------//
//------------ set and get resources ---------------------//


void SLpScale::setIvarResource(void)
{
  if(!topWidget()) return;
  long value = ivar();
  XtVaSetValues(topWidget(), XmNvalue, value, NULL);
}


long SLpScale::ivarResource(void) const
{
  if(!topWidget()) return 0;
  int value;
  XtVaGetValues(topWidget(), XmNvalue, &value, NULL);
  return value;
}


long SLpScale::ivarDefault(void) const
{
  if(pW()) return pW()->scaleDef();
  else     return ivar();
}



//-------------------- make ------------------------------------//
//-------------------- make ------------------------------------//
//-------------------- make ------------------------------------//

static String defres[]= {
    ".showValue:      TRUE",
    ".orientation:    HORIZONTAL",
    NULL };


Widget SLpScale::make(Widget p)
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
       w = XmCreateScale(wParent(), (char*)instanceName(), args, i);
       setTopWidget(w);
       XtManageChild(w);
       }
    XtVaGetValues(w, XmNforeground, &_foreground, NULL);
    _dim = getDimForeground();
    setTraversal(_traversal);
    install_help();
    XtAddEventHandler(w, FocusChangeMask, FALSE,
                      (XtEventHandler)focusEventHandler, (XtPointer)this);
    XtAddCallback(w, XmNvalueChangedCallback,
                        (XtCallbackProc)integerCallback, (XtPointer)this);
    if(_drag)
    XtAddCallback(w, XmNdragCallback,
                        (XtCallbackProc)integerCallback, (XtPointer)this);
    if(updateSenseNeverActivated())
                     setupSenseValue(SLBase::sensitivity());
    updateSelf(TRUE);
    }
  return topWidget();
}


//------------------------ end ----------------------------------//
//------------------------ end ----------------------------------//
//------------------------ end ----------------------------------//
