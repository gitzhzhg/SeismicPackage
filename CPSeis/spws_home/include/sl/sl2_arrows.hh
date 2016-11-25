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


//-------------------------- sl2_arrows.hh -------------------------------//
//-------------------------- sl2_arrows.hh -------------------------------//
//-------------------------- sl2_arrows.hh -------------------------------//

//            header file for the SL2Arrows class
//             derived from the SLSmartForm class
//                      subdirectory sl

    // This class creates a framed horizontal GUI containing
    // left and right arrows, and fields between the arrows.
    // The GUI looks like this:
    //
    //            < label num1 of num2 >
    //
    // The num1 and num2 fields contain integers.
    // The user can press the arrows to increment or decrement num1.
    // The user can type into num1.
    // num1 should be >= 1 and <= num2 (or 1 or 0 if num2 == 0).
    // num2 should be >= 1 (or 0).

    // This class can be used as-is by registering functions
    // with the object.  Alternatively, this class can be derived
    // from by overriding the virtual functions.


#ifndef _SL2_ARROWS_HH_
#define _SL2_ARROWS_HH_

#include "sl/sl_smart_form.hh"


class SL2Arrows : public SLSmartForm
{

//--------------------------- data -------------------------------//
//--------------------------- data -------------------------------//
//--------------------------- data -------------------------------//

private:

  typedef void  ArrowsLongTrap  (void *data, long value);
  typedef char *ArrowsCharFun   (void *data);
  typedef long  ArrowsLongFun   (void *data);

  class SLpArrow *_left;
  class SLpLabel *_label;
  class SLpText  *_num1;
  class SLpLabel *_of;
  class SLpText  *_num2;
  class SLpArrow *_right;

  ArrowsLongTrap  *_num1_trap;
  ArrowsCharFun   *_label_fun;
  ArrowsLongFun   *_num1_fun;
  ArrowsLongFun   *_num2_fun;
  ArrowsLongFun   *_sense_fun;
  
  void *_num1_tdata;
  void *_label_udata;
  void *_num1_udata;
  void *_num2_udata;
  void *_sense_udata;
  
//------------------------- functions ----------------------------//
//------------------------- functions ----------------------------//
//------------------------- functions ----------------------------//

public:

  SL2Arrows (SLDelay *slparent, char *name,
                       const char *label = NULL, long nchar = 0);
  SL2Arrows (Widget    wparent, char *name,
                       const char *label = NULL, long nchar = 0);

  virtual ~SL2Arrows (void);

  virtual Widget make(Widget p = NULL);        // overrides SLSmartForm

  void  registerNum1Trap    (ArrowsLongTrap *trap, void *data);
  void  registerLabelUpdate (ArrowsCharFun  *fun , void *data);
  void  registerNum1Update  (ArrowsLongFun  *fun , void *data);
  void  registerNum2Update  (ArrowsLongFun  *fun , void *data);
  void  registerSenseUpdate (ArrowsLongFun  *fun , void *data);

/*
protected:   // virtual functions to optionally override.

  virtual void  virtualNum1Trap    (long value);
  virtual char *virtualLabelUpdate ();
  virtual long  virtualNum1Update  ();
  virtual long  virtualNum2Update  ();
  virtual long  virtualSenseUpdate ();
*/

private:

  void constructorHelper (const char *label, long nchar);

  static void  staticLeftTrap   (void *data, long ident);
  static void  staticRightTrap  (void *data, long ident);
  static void  staticNum1Trap   (void *data, long ident,
                                      long oldvar, long newvar);

  static long  staticSenseFun        (void *data);
  static long  staticLeftSenseFun    (void *data);
  static long  staticRightSenseFun   (void *data);

//----------------------- end of functions ------------------------------//
//----------------------- end of functions ------------------------------//
//----------------------- end of functions ------------------------------//

} ;

#endif

//---------------------------- end --------------------------------------//
//---------------------------- end --------------------------------------//
//---------------------------- end --------------------------------------//
