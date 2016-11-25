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

//--------------------------- slp_base.cc --------------------------------//
//--------------------------- slp_base.cc --------------------------------//
//--------------------------- slp_base.cc --------------------------------//

//            implementation file for the SLpBase base class
//                    derived from the SLPrim class
//                         subdirectory sl


#include "sl/slp_base.hh"
#include <X11/StringDefs.h>
#include <Xm/Text.h>
 

char  *SLpBase::_textcopy = NULL;

//------------------- constructors -----------------------------//
//------------------- constructors -----------------------------//
//------------------- constructors -----------------------------//

SLpBase::SLpBase(SLDelay *slparent, char *name, WidgetClass wclass,
                                        long ident, long type)
     : SLPrim(slparent, name, ident, type),
       _wclass    (wclass),
       _traversal (True)
{
  if(_type >= _LONG)
    {
    if     (slparent->pW       ()) supportUnmadeDefaults(slparent->pW());
    else if(slparent->topWidget()) supportUnmadeDefaults(slparent->topWidget());
    }
}


SLpBase::SLpBase(Widget wparent, char *name, WidgetClass wclass,
                                        long ident, long type)
     : SLPrim(wparent, name, ident, type),
       _wclass    (wclass),
       _traversal (True)
{
  if(_type >= _LONG) supportUnmadeDefaults(wparent);
}


SLpBase::SLpBase(Widget w, long ident, long type)
     : SLPrim(w, ident, type),
       _wclass    (XtClass(w)),
       _traversal (True)
{
  setTopWidget(w);
  if(_type >= _LONG) supportUnmadeDefaults(XtParent(w));
}



//------------------------ destructor ----------------------------//
//------------------------ destructor ----------------------------//
//------------------------ destructor ----------------------------//

SLpBase::~SLpBase(void)
{
}


//-------functions beyond here do not need versions in derived classes------//
//-------functions beyond here do not need versions in derived classes------//
//-------functions beyond here do not need versions in derived classes------//
//-------functions beyond here do not need versions in derived classes------//
//-------functions beyond here do not need versions in derived classes------//
//-------functions beyond here do not need versions in derived classes------//
//-------functions beyond here do not need versions in derived classes------//


//---------------- set traversal on or off -------------------------//
//---------------- set traversal on or off -------------------------//
//---------------- set traversal on or off -------------------------//

void SLpBase::setTraversal(Boolean traversal)
{
  _traversal = traversal;
  if(topWidget())
      XtVaSetValues(topWidget(), XmNtraversalOn, _traversal, NULL);
}



//------------------- get custom resources ---------------------//
//------------------- get custom resources ---------------------//
//------------------- get custom resources ---------------------//

typedef struct _Custom
{
  Pixel dim, bright;
} Custom;

static Custom custom;

static XtResource resources[] = {
  { "dim_foreground",  "Dim_foreground",  XtRPixel, sizeof(Pixel),
               XtOffsetOf(Custom,dim),  XtRString, (caddr_t)"gray55" },
  { "bright_background",  "Bright_background",  XtRPixel, sizeof(Pixel),
               XtOffsetOf(Custom,bright),  XtRString, (caddr_t)"gray87" },
};


static void get_custom_resources(Widget w)
{
  static Boolean start = TRUE;
  if(start)
       {
       Widget toplevel = get_toplevel_shell(w);
       XtGetApplicationResources(toplevel, &custom,
                            resources, XtNumber(resources), NULL, 0);
       start = FALSE;
       }
}


Pixel SLpBase::getDimForeground   (void) 
{ get_custom_resources(topWidget()); return custom.dim   ; }

Pixel SLpBase::getBrightBackground(void)
{ get_custom_resources(topWidget()); return custom.bright; }



//------------ callbacks and event handlers -------------------------//
//------------ callbacks and event handlers -------------------------//
//------------ callbacks and event handlers -------------------------//


void SLpBase::focusEventHandler(Widget /* w */, XtPointer user, XEvent *event)
{
  SLpBase *gui = (SLpBase*)user;
  switch(event->type)
      {
      case FocusIn : gui->callFocusinTrap ();  break;
      case FocusOut: gui->callFocusoutTrap();  break;
      }
}


void SLpBase::activateCallback(Widget /* w */, XtPointer user, XtPointer call)
{
  SLpBase *gui = (SLpBase*)user;
  gui->setLastCallback((XmAnyCallbackStruct*)call);
  gui->callAtrap();
  gui->setLastCallback(NULL);
}


void SLpBase::integerCallback(Widget /* w */, XtPointer user, XtPointer call)
{                     
  SLpBase *gui = (SLpBase*)user;
  gui->setLastCallback((XmAnyCallbackStruct*)call);
  gui->callItrap(gui->ivarResource());
  gui->updateEverything();
  gui->setLastCallback(NULL);
}



//----------set and get compound string resource---------------------//
//----------set and get compound string resource---------------------//
//----------set and get compound string resource---------------------//
       // set... moves resource from the _cvar class to the widget.
       // get... gets and returns the resource from the widget.


void SLpBase::setCompoundStringResource(char *resname, Widget w)
{
  XmString string;

  if(!w) w = topWidget();
  if(!w) return;
  char *text = cvar();
  if(text == NULL) return;
  string = XmStringCreateLtoR(text, XmSTRING_DEFAULT_CHARSET);
  XtVaSetValues(w, resname, string, NULL);
  XmStringFree(string);

}


char *SLpBase::getCompoundStringResource(char *resname, Widget w)
{
  XmString string;
  char *text;

  if(!w) w = topWidget();
  if(!w) return NULL;
  XtVaGetValues(w, resname, &string, NULL);
  if(XmStringGetLtoR(string,XmSTRING_DEFAULT_CHARSET,&text) )
      {
      _textcopy = string_alloc(_textcopy, text, 0);
      XtFree(text);
      }
  else
      {
      _textcopy = string_free(_textcopy);
      }
  XmStringFree(string);
  return _textcopy;
}


//---------------------------- end -----------------------------------//
//---------------------------- end -----------------------------------//
//---------------------------- end -----------------------------------//
