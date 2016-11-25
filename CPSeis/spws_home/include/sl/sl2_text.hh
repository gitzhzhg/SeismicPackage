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


//-------------------------- sl2_text.hh -------------------------------//
//-------------------------- sl2_text.hh -------------------------------//
//-------------------------- sl2_text.hh -------------------------------//

//             header file for the SL2Text class
//             derived from the SLSmartForm class
//                      subdirectory sl

#ifndef _SL2_TEXT_HH_
#define _SL2_TEXT_HH_

#include "sl/sl_smart_form.hh"
#include "sl/slp_label.hh"
#include "sl/slp_text.hh"


class SL2Text : public SLSmartForm
{

//--------------------------- data -------------------------------//
//--------------------------- data -------------------------------//
//--------------------------- data -------------------------------//

private:

  SLpLabel *_label;
  SLpText  *_text;

//------------------------- functions ----------------------------//
//------------------------- functions ----------------------------//
//------------------------- functions ----------------------------//

public:

  SL2Text (SLDelay *slparent, char *name, long ident = 0,
     char *label = NULL,
     long type = SLpText::_CHAR, long nchar = 0, long ndec = 999);
  SL2Text (Widget    wparent, char *name, long ident = 0,
     char *label = NULL,
     long type = SLpText::_CHAR, long nchar = 0, long ndec = 999);

  virtual ~SL2Text (void);

  SLpLabel *getSLpLabel(void) const { return _label; }
  SLpText  *getSLpText (void) const { return _text ; }

  virtual Widget make(Widget p = NULL);        // overrides SLSmartForm

  void showNormalAppearance     () { _text->showNormalAppearance     (); }
  void showLabelAppearance      () { _text->showLabelAppearance      (); }
  void showFramedLabelAppearance() { _text->showFramedLabelAppearance(); }

public:    // pass-thru functions

  void setNotify(SLDelay *target) { _text->setNotify(target); }

  void setFocusinTrap (FocusFun *trap, void *data)
                              { _text->setFocusinTrap (trap, data); }
  void setFocusoutTrap(FocusFun *trap, void *data)
                              { _text->setFocusoutTrap(trap, data); }

  void setItrap(ItrapFun *trap, void *data)
                              { _text->setItrap(trap, data); }
  void setFtrap(FtrapFun *trap, void *data)
                              { _text->setFtrap(trap, data); }
  void setDtrap(DtrapFun *trap, void *data)
                              { _text->setDtrap(trap, data); }
  void setCtrap(CtrapFun *trap, void *data)
                              { _text->setCtrap(trap, data); }

  void setFocusinTrap (FocusFunF *trap) { _text->setFocusinTrap (trap); }
  void setFocusoutTrap(FocusFunF *trap) { _text->setFocusoutTrap(trap); }

  void setItrap(ItrapFunF *trap) { _text->setItrap(trap); }
  void setFtrap(FtrapFunF *trap) { _text->setFtrap(trap); }
  void setDtrap(DtrapFunF *trap) { _text->setDtrap(trap); }
  void setCtrap(CtrapFunF *trap) { _text->setCtrap(trap); }

  char  *getLabel(void) const { return _label->cvar (); }
  long   getIvar (void) const { return _text ->ivar (); }
  float  getFvar (void) const { return _text ->fvar (); }
  double getDvar (void) const { return _text ->dvar (); }
  char  *getCvar (void) const { return _text ->cvar (); }
  long   getSense(void) const { return _label->sense(); }
  long   getImin (void) const { return _text ->imin (); }
  long   getImax (void) const { return _text ->imax (); }
  float  getFmin (void) const { return _text ->fmin (); }
  float  getFmax (void) const { return _text ->fmax (); }
  double getDmin (void) const { return _text ->dmin (); }
  double getDmax (void) const { return _text ->dmax (); }

  void  setLabel(char  *value) { _label->setCvar (value); }
  void  setIvar (long   value) { _text ->setIvar (value); }
  void  setFvar (float  value) { _text ->setFvar (value); }
  void  setDvar (double value) { _text ->setDvar (value); }
  void  setCvar (char  *value) { _text ->setCvar (value); }
  void  setSense(long   value) { _label->setSense(value);
                                 _text ->setSense(value); }
  void  setImin (long   value) { _text ->setImin (value); }
  void  setImax (long   value) { _text ->setImax (value); }
  void  setFmin (float  value) { _text ->setFmin (value); }
  void  setFmax (float  value) { _text ->setFmax (value); }
  void  setDmin (double value) { _text ->setDmin (value); }
  void  setDmax (double value) { _text ->setDmax (value); }

  void  setupLabelValue(char  *value) { _label->setupCvarValue (value); }
  void  setupIvarValue (long   value) { _text ->setupIvarValue (value); }
  void  setupFvarValue (float  value) { _text ->setupFvarValue (value); }
  void  setupDvarValue (double value) { _text ->setupDvarValue (value); }
  void  setupCvarValue (char  *value) { _text ->setupCvarValue (value); }
  void  setupSenseValue(long   value) { _label->setupSenseValue(value);
                                        _text ->setupSenseValue(value); }
  void  setupIminValue (long   value) { _text ->setupIminValue (value); }
  void  setupImaxValue (long   value) { _text ->setupImaxValue (value); }
  void  setupFminValue (float  value) { _text ->setupFminValue (value); }
  void  setupFmaxValue (float  value) { _text ->setupFmaxValue (value); }
  void  setupDminValue (double value) { _text ->setupDminValue (value); }
  void  setupDmaxValue (double value) { _text ->setupDmaxValue (value); }

  void  setupLabelPoint(char  *point, long nvar)
                               { _label->setupCvarPoint (point, nvar); }
  void  setupIvarPoint (long  *point) { _text ->setupIvarPoint (point); }
  void  setupFvarPoint (float *point) { _text ->setupFvarPoint (point); }
  void  setupDvarPoint (double*point) { _text ->setupDvarPoint (point); }
  void  setupCvarPoint (char  *point, long nvar)
                               { _text ->setupCvarPoint (point, nvar); }
  void  setupSensePoint(long  *point) { _label->setupSensePoint(point);
                                        _text ->setupSensePoint(point); }
  void  setupIminPoint (long  *point) { _text ->setupIminPoint (point); }
  void  setupImaxPoint (long  *point) { _text ->setupImaxPoint (point); }
  void  setupFminPoint (float *point) { _text ->setupFminPoint (point); }
  void  setupFmaxPoint (float *point) { _text ->setupFmaxPoint (point); }
  void  setupDminPoint (double*point) { _text ->setupDminPoint (point); }
  void  setupDmaxPoint (double*point) { _text ->setupDmaxPoint (point); }

  void  setupLabelFun(char *(*fun)(void*), void *data)
                             { _label->setupCvarFun (fun, data); }
  void  setupIvarFun (long  (*fun)(void*), void *data)
                             { _text ->setupIvarFun (fun, data); }
  void  setupFvarFun (float (*fun)(void*), void *data)
                             { _text ->setupFvarFun (fun, data); }
  void  setupDvarFun (double(*fun)(void*), void *data)
                             { _text ->setupDvarFun (fun, data); }
  void  setupCvarFun (char *(*fun)(void*), void *data)
                             { _text ->setupCvarFun (fun, data); }
  void  setupSenseFun(long  (*fun)(void*), void *data)
                             { _label->setupSenseFun(fun, data);
                               _text ->setupSenseFun(fun, data); }
  void  setupIminFun (long  (*fun)(void*), void *data)
                             { _text ->setupIminFun (fun, data); }
  void  setupImaxFun (long  (*fun)(void*), void *data)
                             { _text ->setupImaxFun (fun, data); }
  void  setupFminFun (float (*fun)(void*), void *data)
                             { _text ->setupFminFun (fun, data); }
  void  setupFmaxFun (float (*fun)(void*), void *data)
                             { _text ->setupFmaxFun (fun, data); }
  void  setupDminFun (double(*fun)(void*), void *data)
                             { _text ->setupDminFun (fun, data); }
  void  setupDmaxFun (double(*fun)(void*), void *data)
                             { _text ->setupDmaxFun (fun, data); }

public:    // alternative pass-thru functions

  void  setValue  (long   value) { setIvar (value); }
  void  setValue  (float  value) { setFvar (value); }
  void  setValue  (double value) { setDvar (value); }
  void  setValue  (char  *value) { setCvar (value); }
  void  setupValue(long   value) { setupIvarValue (value); }
  void  setupValue(float  value) { setupFvarValue (value); }
  void  setupValue(double value) { setupDvarValue (value); }
  void  setupValue(char  *value) { setupCvarValue (value); }
  void  setupPoint(long  *point) { setupIvarPoint (point); }
  void  setupPoint(float *point) { setupFvarPoint (point); }
  void  setupPoint(double*point) { setupDvarPoint (point); }
  void  setupPoint(char  *point, long nvar)
                                 { setupCvarPoint (point, nvar); }
  void  setupFun  (long  (*fun)(void*), void *data)
                                 { setupIvarFun (fun, data); }
  void  setupFun  (float (*fun)(void*), void *data)
                                 { setupFvarFun (fun, data); }
  void  setupFun  (double(*fun)(void*), void *data)
                                 { setupDvarFun (fun, data); }
  void  setupFun  (char *(*fun)(void*), void *data)
                                 { setupCvarFun (fun, data); }

//----------------------- end of functions ------------------------------//
//----------------------- end of functions ------------------------------//
//----------------------- end of functions ------------------------------//

} ;

#endif

//---------------------------- end --------------------------------------//
//---------------------------- end --------------------------------------//
//---------------------------- end --------------------------------------//
