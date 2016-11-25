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

//-------------------------- sl_prim.hh -----------------------------//
//-------------------------- sl_prim.hh -----------------------------//
//-------------------------- sl_prim.hh -----------------------------//

//               header file for the SLPrim base class
//                  derived from the SLDelay class 
//                derived from the PrimSupport class 
//                         subdirectory sl

//  This class serves as a base class for all primitive GUIs,
//  either widgetless, or made from Motif widgets.
//  This class cannot be instantiated.

#ifndef _SL_PRIM_HH_
#define _SL_PRIM_HH_

#include "sl/sl_delay.hh"
#include "sl/prim_support.hh"

class SLPrim : public SLDelay, public PrimSupport
{

//------------------------- data ------------------------------//
//------------------------- data ------------------------------//
//------------------------- data ------------------------------//

protected:
                // no data
  
//------------------ constructors and destructor -------------------//
//------------------ constructors and destructor -------------------//
//------------------ constructors and destructor -------------------//

protected:

  SLPrim (SLDelay *slparent, char *name, long ident, long type);
  SLPrim (Widget    wparent, char *name, long ident, long type);
  SLPrim (Widget    w      ,             long ident, long type);
  XmAnyCallbackStruct *_callback;

public:

  virtual ~SLPrim (void);

//------------------- other functions ------------------------//
//------------------- other functions ------------------------//
//------------------- other functions ------------------------//
     // these functions do not need to be overridden

protected:             // this overrides PrimSupport

  virtual Boolean trapHelper (SLDelay *target);
  void setLastCallback(XmAnyCallbackStruct *cb) { _callback= cb;}

public:                // these all override SLDelay

  virtual void update(void)
             { updateSelf();
               updateChildren(); }
  virtual void reloadDefaults(Boolean do_method = TRUE)
             { reloadSelf(do_method);
               reloadChildrenDefaults(do_method); }
  virtual void reloadSystemDefaults(Boolean do_method = TRUE)
             { reloadSystemSelf(do_method);
               reloadChildrenSystemDefaults(do_method); }
  XmAnyCallbackStruct *lastCallback() { return _callback; }

//----------------------- end of functions -----------------------//
//----------------------- end of functions -----------------------//
//----------------------- end of functions -----------------------//

};

#endif

//------------------------- end -------------------------------//
//------------------------- end -------------------------------//
//------------------------- end -------------------------------//
