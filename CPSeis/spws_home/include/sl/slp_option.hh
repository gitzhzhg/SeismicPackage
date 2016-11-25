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


//--------------------- slp_option.hh ---------------------------//
//--------------------- slp_option.hh ---------------------------//
//--------------------- slp_option.hh ---------------------------//

//           header file for the SLpOption class
//                      subdirectory sl

#ifndef _SL_OPTION_HH_
#define _SL_OPTION_HH_

#include "sl/slp_base.hh"

class SLpPush;

class SLpOption : public SLpBase
{

private:

  Widget _woption;

public:

  SLpOption(SLDelay *slparent, char *name, long ident = 0, char *label = NULL);
  SLpOption(Widget    wparent, char *name, long ident = 0, char *label = NULL);

  virtual ~SLpOption (void);
  virtual Boolean isContainer () { return TRUE; }
  virtual Widget make (Widget p = NULL);  // overrides SLDelay

public:   // optional convenience functions

  long    optionValue        (void)  const { return ivar(); }
  long    oldOptionValue     (void)  const { return oldIvar(); }
  void setOptionSystemDefault(long x)      { setIvarSystemDefault(x); }
  void setOptionValue        (long x)      { setIvar(x); }
  void setupOptionValue      (long x)            { setupIvarValue(x); }
  void setupOptionPoint      (long *x)           { setupIvarPoint(x); }
  void setupOptionFun (long(*f)(void*), void *d) { setupIvarFun(f,d); }

public:     // add and manipulate children

  SLpPush *findOptionByIdent (long ident);
  SLpPush *addOption         (char *name, long ident, char *label = NULL);
  void     removeOption      (long ident);

  void    setOptionLabel        (long ident, char* value);
  void    setupOptionLabelValue (long ident, char* value);
  void    setupOptionLabelPoint (long ident, char *point, long nvar);
  void    setupOptionLabelFun   (long ident, char* (*fun)(void*), void *data);

  void    setOptionSense        (long ident, long value);
  void    setupOptionSenseValue (long ident, long value);
  void    setupOptionSensePoint (long ident, long *point);
  void    setupOptionSenseFun   (long ident, long (*fun)(void*), void *data);

  void    manageOption          (long ident);
  void    unmanageOption        (long ident);

private:

  virtual Boolean notify(SLPrim *gui);    // overrides SLDelay
  virtual void manage           (void);   // overrides SLDelay
  virtual void unmanage         (void);   // overrides SLDelay
  virtual void setCvarResource  (void);   // overrides PrimSupport
  virtual void setSenseResource (void);   // overrides SLpBase
  virtual void setIvarResource  (void);   // overrides PrimSupport

public:    // added 6/11/98.

     virtual Boolean notifyComplex (SLDelay*, int ident);

private:   // added 6/11/98.

    static void  doMapCallback (Widget, XtPointer, XEvent*);

} ;


#endif

//---------------------------- end --------------------------------------//
//---------------------------- end --------------------------------------//
//---------------------------- end --------------------------------------//
