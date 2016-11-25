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

//-------------------------- sl_prim.cc -----------------------------//
//-------------------------- sl_prim.cc -----------------------------//
//-------------------------- sl_prim.cc -----------------------------//

//            implementation file for the SLPrim base class
//                  derived from the SLDelay class
//                derived from the PrimSupport class
//                         subdirectory sl


#include "sl/sl_prim.hh"


//------------------ constructors and destructor -------------------//
//------------------ constructors and destructor -------------------//
//------------------ constructors and destructor -------------------//


SLPrim::SLPrim(SLDelay *slparent, char *name, long ident, long type)
     : SLDelay(slparent, name),
       PrimSupport(ident, type), _callback(NULL)
{
}

SLPrim::SLPrim(Widget   wparent, char *name, long ident, long type)
     : SLDelay(wparent, name),
       PrimSupport(ident, type), _callback(NULL)
{
}

SLPrim::SLPrim(Widget   w      ,             long ident, long type)
     : SLDelay(XtParent(w), XtName(w)),
       PrimSupport(ident, type), _callback(NULL)
{
}

SLPrim::~SLPrim(void)
{
}


//------------------- trap helper ----------------------------//
//------------------- trap helper ----------------------------//
//------------------- trap helper ----------------------------//

Boolean SLPrim::trapHelper(SLDelay *target)
{
  Boolean doit = TRUE;
  if(target)          doit =     target->notify(this);
//else if(slParent()) doit = slParent()->notify(this);
  else if(slParent()) doit = ((SLDelay*)slParent())->notify(this);
  if(doit) doit = notify(NULL);
  return doit;
}


//------------------------- end -------------------------------//
//------------------------- end -------------------------------//
//------------------------- end -------------------------------//
