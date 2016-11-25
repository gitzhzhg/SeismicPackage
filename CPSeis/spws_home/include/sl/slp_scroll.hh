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

//------------------------- slp_scroll.hh -------------------------------//
//------------------------- slp_scroll.hh -------------------------------//
//------------------------- slp_scroll.hh -------------------------------//

//               header file for the SLpScroll class
//                 derived from the SLpBase class
//                         subdirectory sl

#ifndef _SLP_SCROLL_HH_
#define _SLP_SCROLL_HH_

#include "sl/slp_base.hh"


class SLpScroll : public SLpBase,
                virtual public IminGuiResource,
                virtual public ImaxGuiResource,
                virtual public SliderGuiResource,
                virtual public IstepGuiResource,
                virtual public IpageGuiResource
{

//------------------------- data ------------------------------//
//------------------------- data ------------------------------//
//------------------------- data ------------------------------//

public:

  enum { _HORIZONTAL, _VERTICAL, _UNKNOWN };

private:

  long  _direction;                 // one of the above enums

//-------------------------- functions --------------------------//
//-------------------------- functions --------------------------//
//-------------------------- functions --------------------------//

public:

  SLpScroll (SLDelay *slparent, char *name, long ident = 0,
                                long direction = _UNKNOWN);

  SLpScroll (Widget wparent, char *name, long ident = 0,
                                long direction = _UNKNOWN);

  SLpScroll (Widget w, long ident = 0);

  virtual ~SLpScroll (void);
  virtual Widget make (Widget p = NULL); // overrides SLDelay

public:   // optional convenience functions

  long    scrollValue        (void)  const { return ivar(); }
  long    oldScrollValue     (void)  const { return oldIvar(); }
  void setScrollValue        (long x)      { setIvar(x); }
  void setupScrollValue      (long x)            { setupIvarValue(x); }
  void setupScrollPoint      (long *x)           { setupIvarPoint(x); }
  void setupScrollFun (long(*f)(void*), void *d) { setupIvarFun(f,d); }

private:

  virtual void updateSelf(Boolean force = FALSE); // overrides PrimSupport
  virtual void setIvarResource   (void);          // overrides PrimSupport
  virtual long    ivarResource   (void) const;    // overrides PrimSupport
  static void buttonUpEventHandler(Widget, XtPointer, XEvent*);

//---------------------- end of functions -----------------------//
//---------------------- end of functions -----------------------//
//---------------------- end of functions -----------------------//

} ;

#endif

//---------------------------- end --------------------------------------//
//---------------------------- end --------------------------------------//
//---------------------------- end --------------------------------------//
