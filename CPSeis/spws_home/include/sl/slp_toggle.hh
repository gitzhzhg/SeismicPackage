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


//------------------------- slp_toggle.hh -------------------------------//
//------------------------- slp_toggle.hh -------------------------------//
//------------------------- slp_toggle.hh -------------------------------//

//               header file for the SLpToggle class
//                 derived from the SLpBase class
//                         subdirectory sl

#ifndef _SLP_TOGGLE_HH_
#define _SLP_TOGGLE_HH_

#include "sl/slp_base.hh"


class SLpToggle : public SLpBase
{

private:     // data

  Boolean _radio_button;      // TRUE or FALSE

public:

  SLpToggle(SLDelay *slparent, char *name, long ident = 0,
            char *label = NULL, Boolean radio_button = FALSE);

  SLpToggle(Widget    wparent, char *name, long ident = 0,
            char *label = NULL, Boolean radio_button = FALSE);

  SLpToggle(Widget    w,                   long ident = 0,
            char *label = NULL);

  virtual ~SLpToggle (void);
  virtual Widget make (Widget p = NULL); // overrides SLDelay

public:   // optional convenience functions

  long    toggleValue        (void)  const { return ivar(); }
  long    oldToggleValue     (void)  const { return oldIvar(); }
  void setToggleSystemDefault(long x)      { setIvarSystemDefault(x); }
  void setToggleValue        (long x)      { setIvar(x); }
  void setupToggleValue      (long x)            { setupIvarValue(x); }
  void setupTogglePoint      (long *x)           { setupIvarPoint(x); }
  void setupToggleFun (long(*f)(void*), void *d) { setupIvarFun(f,d); }

private:

  virtual void setIvarResource (void);       // overrides PrimSupport
  virtual void setCvarResource (void);       // overrides PrimSupport
  virtual long    ivarResource (void) const; // overrides PrimSupport
  virtual long    ivarDefault  (void) const; // overrides PrimSupport
} ;


#endif

//---------------------------- end --------------------------------------//
//---------------------------- end --------------------------------------//
//---------------------------- end --------------------------------------//
