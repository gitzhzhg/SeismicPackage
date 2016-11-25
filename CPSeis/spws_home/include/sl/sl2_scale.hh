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


//-------------------------- sl2_scale.hh -------------------------------//
//-------------------------- sl2_scale.hh -------------------------------//
//-------------------------- sl2_scale.hh -------------------------------//

//              header file for the SL2Scale class
//              derived from the SLSmartForm class
//                      subdirectory sl

#ifndef _SL2_SCALE_HH_
#define _SL2_SCALE_HH_

#include "sl/sl_smart_form.hh"
#include "sl/slp_scale.hh"
#include "sl/slp_arrow.hh"


class SL2Scale : public SLSmartForm
{

private:   // data

  SLpScale  *_scale;
  SLpArrow  *_left;
  SLpArrow  *_right;

public:    // functions

  SL2Scale (SLDelay *slparent, char *name, long ident = 0, char *label = NULL,
      long direction = SLpScale::_HORIZONTAL, Boolean drag = FALSE);
  SL2Scale (Widget    wparent, char *name, long ident = 0, char *label = NULL,
      long direction = SLpScale::_HORIZONTAL, Boolean drag = FALSE);

  virtual ~SL2Scale (void);

  SLpScale *getSLpScale      (void) const { return _scale; }
  SLpArrow *getSLpArrowLeft  (void) const { return _left ; }
  SLpArrow *getSLpArrowRight (void) const { return _right; }

private:

  void constructorHelper       (long ident, char *label,
                                  long direction, Boolean drag);
  static void arrowTrap        (void *data, long ident);
  static long updateLeftSense  (void *data);
  static long updateRightSense (void *data);

public:    // pass-thru functions

  void setNotify(SLDelay *target) { _scale->setNotify(target); }

  void setFocusinTrap (FocusFun *trap, void *data)
                              { _scale->setFocusinTrap (trap, data);
                                _left ->setFocusinTrap (trap, data);
                                _right->setFocusinTrap (trap, data); }
  void setFocusoutTrap(FocusFun *trap, void *data)
                              { _scale->setFocusoutTrap(trap, data);
                                _left ->setFocusoutTrap(trap, data);
                                _right->setFocusoutTrap(trap, data); }

  void setItrap(ItrapFun *trap, void *data)
                              { _scale->setItrap(trap, data); }

  void setFocusinTrap (FocusFunF *trap) { _scale->setFocusinTrap (trap);
                                          _left ->setFocusinTrap (trap);
                                          _right->setFocusinTrap (trap); }
  void setFocusoutTrap(FocusFunF *trap) { _scale->setFocusoutTrap(trap);
                                          _left ->setFocusoutTrap(trap);
                                          _right->setFocusoutTrap(trap); }

  void setItrap(ItrapFunF *trap) { _scale->setItrap(trap); }
 
  long  oldScaleValue (void) const { return _scale->oldIvar(); }
  long  oldIvar       (void) const { return _scale->oldIvar(); }

  char *cvar      (void) const { return _scale->cvar (); }
  char *label     (void) const { return _scale->cvar (); }
  long  ivar      (void) const { return _scale->ivar (); }
  long  scaleValue(void) const { return _scale->ivar (); }
  long  sense     (void) const { return _scale->sense(); }
  long  imin      (void) const { return _scale->imin (); }
  long  imax      (void) const { return _scale->imax (); }
  long  ipage     (void) const { return _scale->ipage(); }

  void setIvarSystemDefault (long x) { _scale->setIvarSystemDefault(x); }
  void setScaleSystemDefault(long x) { _scale->setIvarSystemDefault(x); }

  void  setCvar      (char *value) { _scale->setCvar (value); }
  void  setLabel     (char *value) { _scale->setCvar (value); }
  void  setIvar      (long  value) { _scale->setIvar (value); }
  void  setScaleValue(long  value) { _scale->setIvar (value); }
  void  setSense     (long  value) { _scale->setSense(value); }
  void  setImin      (long  value) { _scale->setImin (value); }
  void  setImax      (long  value) { _scale->setImax (value); }
  void  setIpage     (long  value) { _scale->setIpage(value); }

  void  setupCvarValue (char *value) { _scale->setupCvarValue (value); }
  void  setupLabelValue(char *value) { _scale->setupCvarValue (value); }
  void  setupIvarValue (long  value) { _scale->setupIvarValue (value); }
  void  setupScaleValue(long  value) { _scale->setupIvarValue (value); }
  void  setupSenseValue(long  value) { _scale->setupSenseValue(value); }
  void  setupIminValue (long  value) { _scale->setupIminValue (value); }
  void  setupImaxValue (long  value) { _scale->setupImaxValue (value); }
  void  setupIpageValue(long  value) { _scale->setupIpageValue(value); }

  void  setupCvarPoint (char *point, long nvar)
                               { _scale->setupCvarPoint (point, nvar); }
  void  setupLabelPoint(char *point, long nvar)
                               { _scale->setupCvarPoint (point, nvar); }
  void  setupIvarPoint (long *point) { _scale->setupIvarPoint (point); }
  void  setupScalePoint(long *point) { _scale->setupIvarPoint (point); }
  void  setupSensePoint(long *point) { _scale->setupSensePoint(point); }
  void  setupIminPoint (long *point) { _scale->setupIminPoint (point); }
  void  setupImaxPoint (long *point) { _scale->setupImaxPoint (point); }
  void  setupIpagePoint(long *point) { _scale->setupIpagePoint(point); }

  void  setupCvarFun (char *(*fun)(void*), void *data)
                             { _scale->setupCvarFun (fun, data); }
  void  setupLabelFun(char *(*fun)(void*), void *data)
                             { _scale->setupCvarFun (fun, data); }
  void  setupIvarFun (long  (*fun)(void*), void *data)
                             { _scale->setupIvarFun (fun, data); }
  void  setupScaleFun(long  (*fun)(void*), void *data)
                             { _scale->setupIvarFun (fun, data); }
  void  setupSenseFun(long  (*fun)(void*), void *data)
                             { _scale->setupSenseFun(fun, data); }
  void  setupIminFun (long  (*fun)(void*), void *data)
                             { _scale->setupIminFun (fun, data); }
  void  setupImaxFun (long  (*fun)(void*), void *data)
                             { _scale->setupImaxFun (fun, data); }
  void  setupIpageFun(long  (*fun)(void*), void *data)
                             { _scale->setupIpageFun(fun, data); }
} ;


#endif

//---------------------------- end --------------------------------------//
//---------------------------- end --------------------------------------//
//---------------------------- end --------------------------------------//
