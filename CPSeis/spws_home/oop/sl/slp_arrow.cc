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

//---------------------- slp_arrow.cc ----------------------------------//
//---------------------- slp_arrow.cc ----------------------------------//
//---------------------- slp_arrow.cc ----------------------------------//

//         implementation file for the SLpArrow class
//                 derived from the SLpBase class
//                      subdirectory sl

#include "sl/slp_arrow.hh"
#include <Xm/ArrowB.h>
 

//-----------------constructor----------------------------------------//
//-----------------constructor----------------------------------------//
//-----------------constructor----------------------------------------//


SLpArrow::SLpArrow(SLDelay *slparent, char *name, long ident, long direction)
    : SLpBase(slparent, name, xmArrowButtonWidgetClass, ident, _ACTIVE),
      _direction (direction)
{
  createSenseResource();
  if(slparent->topWidget()) make();
}


SLpArrow::SLpArrow(Widget wparent, char *name, long ident, long direction)
    : SLpBase(wparent, name, xmArrowButtonWidgetClass, ident, _ACTIVE),
      _direction (direction)
{
  createSenseResource();
  make();
}


SLpArrow::SLpArrow(Widget w, long ident)
    : SLpBase(w, ident, _ACTIVE),
      _direction (_UNKNOWN)
{
  createSenseResource();
  make();
}


//-------------------- destructor --------------------------//
//-------------------- destructor --------------------------//
//-------------------- destructor --------------------------//

SLpArrow::~SLpArrow(void)
{
}



//----------------- other functions -------------------------//
//----------------- other functions -------------------------//
//----------------- other functions -------------------------//

void SLpArrow::setSenseResource(void)
{
  Widget w = topWidget();
  if(!w) return;
  SLBase::setSensitivity((Boolean)sense());
/*   removed for motif 1.2:
  if(sense()) XtVaSetValues(w, XmNforeground, _foreground, NULL);
  else        XtVaSetValues(w, XmNforeground, _background, NULL);
*/
}



//--------------------------- make -----------------------------//
//--------------------------- make -----------------------------//
//--------------------------- make -----------------------------//

static String defres[]= {
    ".shadowThickness:      0",
    ".width:               30",
    NULL };


Widget SLpArrow::make(Widget p)
{
  if(!made())
     {
     Widget w = SLDelay::make(p);
     if(!w)
        {
        setDefaultResources(XtDisplay(wParent()), instanceName(), defres);
        Arg args[5];
        int i = 0;
        if(_direction == _LEFT)
            { XtSetArg(args[i], XmNarrowDirection, XmARROW_LEFT); i++; }
        else if(_direction == _RIGHT)
            { XtSetArg(args[i], XmNarrowDirection, XmARROW_RIGHT); i++; }
        else if(_direction == _UP)
            { XtSetArg(args[i], XmNarrowDirection, XmARROW_UP); i++; }
        else if(_direction == _DOWN)
            { XtSetArg(args[i], XmNarrowDirection, XmARROW_DOWN); i++; }
        w = XmCreateArrowButton(wParent(), (char*)instanceName(), args, i);
        setTopWidget(w);
        XtManageChild(w);
        }
/* removed for motif 1.2:
     XtVaGetValues(w, XmNforeground, &_foreground, NULL);
     XtVaGetValues(w, XmNbackground, &_background, NULL);
*/
     setTraversal(_traversal);
     install_help();
     XtAddEventHandler (w, FocusChangeMask, FALSE,
                        (XtEventHandler)focusEventHandler, (XtPointer)this);
     XtAddCallback(w, XmNactivateCallback,
                         (XtCallbackProc)activateCallback, (XtPointer)this);
     updateSelf(TRUE);
     }
  return topWidget();
}



//------------------------ end ----------------------------------//
//------------------------ end ----------------------------------//
//------------------------ end ----------------------------------//
