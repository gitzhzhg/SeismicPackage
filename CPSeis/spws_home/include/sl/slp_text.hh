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


//------------------------- slp_text.hh -------------------------------//
//------------------------- slp_text.hh -------------------------------//
//------------------------- slp_text.hh -------------------------------//

//                 header file for the SLpText class
//                   derived from the SLpBase class
//                         subdirectory sl


//  This is a class for text widgets which display long, 
//  float, double, or char.


#ifndef _SLP_TEXT_HH_
#define _SLP_TEXT_HH_

#include "sl/slp_base.hh"


class SLpText : public SLpBase,
               virtual public IminGuiResource,
               virtual public ImaxGuiResource,
               virtual public FminGuiResource,
               virtual public FmaxGuiResource,
               virtual public DminGuiResource,
               virtual public DmaxGuiResource
{

//------------------------------- data --------------------------//
//------------------------------- data --------------------------//
//------------------------------- data --------------------------//

private:

  enum { _NORMAL_APPEARANCE, _LABEL_APPEARANCE, _FRAMED_LABEL_APPEARANCE };

  Boolean  _flush;              // whether to flush buffer when value changes.
  long     _show;               // one of the above enums
  long  _nchar, _ndec;

//------------------------------- functions ------------------------//
//------------------------------- functions ------------------------//
//------------------------------- functions ------------------------//

public:

  void flushWhenValueChanges(Boolean flush = TRUE) { _flush = flush; }

  void showNormalAppearance()      { _show = _NORMAL_APPEARANCE;
                                     setSenseResource(); }
  void showLabelAppearance()       { _show = _LABEL_APPEARANCE ;
                                     setSenseResource(); }
  void showFramedLabelAppearance() { _show = _FRAMED_LABEL_APPEARANCE;
                                     setSenseResource(); }

  SLpText (SLDelay *slparent, char *name, long ident = 0,
          long type = _CHAR, long nchar = 0, long ndec = 999);
  SLpText (Widget   wparent , char *name, long ident = 0,
          long type = _CHAR, long nchar = 0, long ndec = 999);
  SLpText (Widget   w,                    long ident = 0,
          long type = _CHAR, long nchar = 0, long ndec = 999);

  virtual ~SLpText (void);
  virtual Widget make  (Widget p = NULL); // overrides SLDelay

  static void displayMessage(void *data, char *msg);

private:

  void          constructorHelper();
  int           checkValue (int    ivar, int istat);
  float         checkValue (float  fvar, int istat);
  double        checkValue (double dvar, int istat);
  virtual void  setSenseResource(void);       // overrides PrimSupport
  virtual void  setIvarResource (void);       // overrides PrimSupport
  virtual void  setFvarResource (void);       // overrides PrimSupport
  virtual void  setDvarResource (void);       // overrides PrimSupport
  virtual void  setCvarResource (void);       // overrides PrimSupport
  virtual long     ivarResource (void) const; // overrides PrimSupport
  virtual float    fvarResource (void) const; // overrides PrimSupport
  virtual double   dvarResource (void) const; // overrides PrimSupport
  virtual char    *cvarResource (void) const; // overrides PrimSupport
  virtual long     ivarDefault  (void) const; // overrides PrimSupport
  virtual float    fvarDefault  (void) const; // overrides PrimSupport
  virtual double   dvarDefault  (void) const; // overrides PrimSupport
  virtual char    *cvarDefault  (void) const; // overrides PrimSupport
  static void tCallback(Widget w, XtPointer user, XtPointer call);
} ;


#endif

//---------------------------- end --------------------------------------//
//---------------------------- end --------------------------------------//
//---------------------------- end --------------------------------------//
