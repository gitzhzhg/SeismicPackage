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

//------------------------- slp_scale.hh -------------------------------//
//------------------------- slp_scale.hh -------------------------------//
//------------------------- slp_scale.hh -------------------------------//

//               header file for the SLpScale class
//                 derived from the SLpBase class
//                         subdirectory sl

#ifndef _SLP_SCALE_HH_
#define _SLP_SCALE_HH_

#include "sl/slp_base.hh"

class SL2Scale;

class SLpScale : public SLpBase,
                virtual public IminGuiResource,
                virtual public ImaxGuiResource,
                virtual public IpageGuiResource
{
  friend class SL2Scale;

//--------------------- data ----------------------------//
//--------------------- data ----------------------------//
//--------------------- data ----------------------------//

public:

  enum { _HORIZONTAL, _VERTICAL, _UNKNOWN };

private:

  long  _direction;              // one of the above enums 
  Boolean _drag;
  Pixel _foreground, _dim;

//--------------------- functions --------------------------//
//--------------------- functions --------------------------//
//--------------------- functions --------------------------//

public:

  SLpScale (SLDelay *slparent, char *name, long ident = 0,
            char *label = NULL, long direction = _UNKNOWN,
                                Boolean drag = FALSE);

  SLpScale (Widget    wparent, char *name, long ident = 0,
            char *label = NULL, long direction = _UNKNOWN,
                                Boolean drag = FALSE);

  SLpScale (Widget w, long ident = 0,
            char *label = NULL, Boolean drag = FALSE);

  virtual ~SLpScale (void);
  virtual Widget make (Widget p = NULL); // overrides SLDelay


public:   // optional convenience functions

  long    scaleValue        (void)  const { return ivar(); }
  long    oldScaleValue     (void)  const { return oldIvar(); }
  void setScaleSystemDefault(long x)      { setIvarSystemDefault(x); }
  void setScaleValue        (long x)      { setIvar(x); }
  void setupScaleValue      (long x)            { setupIvarValue(x); }
  void setupScalePoint      (long *x)           { setupIvarPoint(x); }
  void setupScaleFun (long(*f)(void*), void *d) { setupIvarFun(f,d); }

private:
 
  virtual void updateSelf(Boolean force = FALSE); // overrides PrimSupport
  virtual void setIvarResource   (void);          // overrides PrimSupport
  virtual long    ivarResource   (void) const;    // overrides PrimSupport
  virtual long    ivarDefault    (void) const;    // overrides PrimSupport

//----------------------- end of functions --------------------//
//----------------------- end of functions --------------------//
//----------------------- end of functions --------------------//

} ;

#endif

//---------------------------- end ---------------------------------//
//---------------------------- end ---------------------------------//
//---------------------------- end ---------------------------------//
