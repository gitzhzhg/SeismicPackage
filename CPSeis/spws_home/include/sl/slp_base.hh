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

//-------------------------- slp_base.hh -----------------------------//
//-------------------------- slp_base.hh -----------------------------//
//-------------------------- slp_base.hh -----------------------------//

//               header file for the SLpBase base class
//                  derived from the SLPrim class
//                         subdirectory sl

//  This class serves as a base class for all primitive GUIs made
//  from Motif widgets.


#ifndef _SLP_BASE_HH_
#define _SLP_BASE_HH_

#include "sl/sl_prim.hh"


class SLpBase : public SLPrim
{

//------------------------- data ------------------------------//
//------------------------- data ------------------------------//
//------------------------- data ------------------------------//

protected:

  static char  *_textcopy;  // copy of text obtained from widget.
  WidgetClass   _wclass;    // pointer to widget class.
  Boolean       _traversal; // whether to set traversal on or off.
  
//-------- functions needing replacement in derived classes --------//
//-------- functions needing replacement in derived classes --------//
//-------- functions needing replacement in derived classes --------//

protected:

  SLpBase (SLDelay *slparent, char *name, WidgetClass wclass,
                                           long ident, long type);
  SLpBase (Widget    wparent, char *name, WidgetClass wclass,
                                           long ident, long type);
  SLpBase (Widget    w      ,              long ident, long type);

  virtual void setSenseResource (void)    // overrides SenseGuiResource
                     { SLBase::setSensitivity((Boolean)sense()); }

public:

  virtual  ~SLpBase  (void);

//------------------- other functions ------------------------//
//------------------- other functions ------------------------//
//------------------- other functions ------------------------//

public:

  virtual WidgetClass topClass() { return _wclass; }   // overrides SLDelay

  virtual void setSensitivity (Boolean sense)          // overrides SLBase
                { if(updateSenseNeverActivated()) setSense(sense); }
  virtual Boolean sensitivity() { return sense(); }    // overrides SLBase

  void setTraversal (Boolean traversal);

protected:

  Pixel getDimForeground    (void);
  Pixel getBrightBackground (void);

  static void focusEventHandler(Widget, XtPointer, XEvent*);
  static void activateCallback (Widget, XtPointer, XtPointer);
  static void integerCallback  (Widget, XtPointer, XtPointer);

  void  setCompoundStringResource (char *resname, Widget w = NULL);
  char *getCompoundStringResource (char *resname, Widget w = NULL);

//  XmAnyCallbackStruct *lastCallback() { return _callback; }

//----------------------- end of functions -----------------------//
//----------------------- end of functions -----------------------//
//----------------------- end of functions -----------------------//

};

#endif

//------------------------- end -------------------------------//
//------------------------- end -------------------------------//
//------------------------- end -------------------------------//
