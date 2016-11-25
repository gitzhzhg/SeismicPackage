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

//---------------------- radio_list.hh --------------------------//
//---------------------- radio_list.hh --------------------------//
//---------------------- radio_list.hh --------------------------//

//         header file for RadioElement and RadioList
//          derived from Element and BaseLinkedList
//                      subdirectory sl

//        supports linked lists of SLpRadio objects

//  This list should contain all radio button objects which work
//    together.
//  This is more than just a linked list of radio button objects.
//  It also has a number of pass-thru functions to set values
//    collectively in all radio buttons in the list.
//  This is a convenience class only.  The radio buttons can
//    be operated without using this class.  This class is a
//    convenience because the same function call would have to
//    be made on all related radio buttons for them to work
//    together.  These function calls set values, traps, etc.
//  The pass-thru functions are identical to the methods which would
//    be used on the radio buttons separately.


#ifndef _RADIO_LIST_HH_
#define _RADIO_LIST_HH_

#include <X11/Intrinsic.h>
#include <Xm/RowColumn.h>
#include "oprim/element.hh"
#include "oprim/ll_base.hh"
#include "sl/prim_support.hh"

class SLpRadio;
class SLDelay;


//--------------------- RadioElement ------------------------//
//--------------------- RadioElement ------------------------//
//--------------------- RadioElement ------------------------//

class RadioElement : public Element
{
private:

  SLpRadio *_gui;

  friend class RadioList;

  RadioElement    (SLpRadio *gui);
  ~RadioElement   () {}
  int operator == (void * const gui) const;
  void print      () const;
};


//-------------------- RadioList ----------------------//
//-------------------- RadioList ----------------------//
//-------------------- RadioList ----------------------//

class RadioList : public BaseLinkedList
{

public:   // the usual linked list methods

  RadioList         ();
  ~RadioList        ();
  void      add     (SLpRadio *gui);
  void      remove  (SLpRadio *gui);
  SLpRadio *find    (SLpRadio *gui);
  SLpRadio *top     ();
  SLpRadio *bottom  ();
  SLpRadio *next    ();
  SLpRadio *prev    ();
  SLpRadio *current ();

public:     // add and manipulate children

  SLpRadio *addRadio   (SLDelay *slparent,
                           char *name, long ident, char *label = NULL);
  SLpRadio *addRadio   (Widget    wparent,
                           char *name, long ident, char *label = NULL);

  void     removeRadio (long ident);

  long ivar();
  long oldIvar();

  void setIvar       (long x);
  void setupIvarValue(long x);
  void setupIvarPoint(long*x);
  void setupIvarFun(long(*fun)(void*), void*data);
  void setIvarSystemDefault(long x);

  void setFocusinTrap (FocusFun  *trap, void*data);
  void setFocusoutTrap(FocusFun  *trap, void*data);
  void setFocusinTrap (FocusFunF *trap);
  void setFocusoutTrap(FocusFunF *trap);
  void setNotify      (SLDelay *target);
  void setItrap       (ItrapFun  *trap, void*data);
  void setItrap       (ItrapFunF *trap);

  SLpRadio *findRadioByIdent (long ident);

  void    setLabel        (long ident, char* value);
  void    setupLabelValue (long ident, char* value);
  void    setupLabelPoint (long ident, char *point, long nvar);
  void    setupLabelFun   (long ident, char* (*fun)(void*), void *data);

  void    setSense        (long ident, long value);
  void    setupSenseValue (long ident, long value);
  void    setupSensePoint (long ident, long *point);
  void    setupSenseFun   (long ident, long (*fun)(void*), void *data);

  void    manageRadio     (long ident);
  void    unmanageRadio   (long ident);

public:   // alternatives (inline) to some methods above

  long    RadioValue        (void)        { return ivar(); }
  long    OldRadioValue     (void)        { return oldIvar(); }
  void setRadioSystemDefault(long x)      { setIvarSystemDefault(x); }
  void setRadioValue        (long x)      { setIvar(x); }
  void setupRadioValue      (long x)            { setupIvarValue(x); }
  void setupRadioPoint      (long *x)           { setupIvarPoint(x); }
  void setupRadioFun (long(*f)(void*), void *d) { setupIvarFun(f,d); }

};

#undef SHORTHAND

//--------------------- end of classes ----------------------//
//--------------------- end of classes ----------------------//
//--------------------- end of classes ----------------------//

#endif

//------------------------- end --------------------------//
//------------------------- end --------------------------//
//------------------------- end --------------------------//

