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

//---------------------- sl2_arrows.cc ---------------------------------//
//---------------------- sl2_arrows.cc ---------------------------------//
//---------------------- sl2_arrows.cc ---------------------------------//

//       implementation file for the SL2Arrows class
//            derived from the SLSmartForm class
//                      subdirectory sl

#include "sl/sl2_arrows.hh"
#include "sl/slp_label.hh"
#include "sl/slp_text.hh"
#include "sl/slp_arrow.hh"
#include <Xm/Xm.h>


//---------------- constructors and destructor --------------------//
//---------------- constructors and destructor --------------------//
//---------------- constructors and destructor --------------------//


SL2Arrows::SL2Arrows (SLDelay *slparent, char *name,
                      const char *label, long nchar)
           : SLSmartForm(slparent, name, NULL, TRUE)
{
  constructorHelper(label, nchar);
}


SL2Arrows::SL2Arrows (Widget   wparent, char *name,
                      const char *label, long nchar)
           : SLSmartForm(wparent, name, NULL, TRUE)
{
  constructorHelper(label, nchar);
}


SL2Arrows::~SL2Arrows()
{
}



//---------------- constructor helper -------------------//
//---------------- constructor helper -------------------//
//---------------- constructor helper -------------------//

           // private.

void SL2Arrows::constructorHelper(const char *label, long nchar)
{
  _left  = new SLpArrow (this, "left" , 0, SLpArrow::_LEFT);
  _label = new SLpLabel (this, "label", 0, (char*)label);
  _num1  = new SLpText  (this, "num1" , 0, SLpText::_LONG, nchar);
  _of    = new SLpLabel (this, "of"   , 0, "of");
  _num2  = new SLpText  (this, "num2" , 0, SLpText::_LONG, nchar);
  _right = new SLpArrow (this, "right", 0, SLpArrow::_RIGHT);

  _num1_trap  = NULL;
  _label_fun  = NULL;
  _num1_fun   = NULL;
  _num2_fun   = NULL;
  _sense_fun  = NULL;

  _num1_tdata   = NULL;
  _label_udata  = NULL;
  _num1_udata   = NULL;
  _num2_udata   = NULL;
  _sense_udata  = NULL;

  _num2->showLabelAppearance();

     //             L     R     T     B
  attach(_left ,   this, NULL, this, this);
  attach(_label,  _left, NULL, this, this);
  attach(_num1 , _label, NULL, this, this);
  attach(_of   ,  _num1, NULL, this, this);
  attach(_num2 ,    _of, NULL, this, this);
  attach(_right,  _num2, this, this, this);

  _left ->setAtrap (staticLeftTrap , this);
  _num1 ->setItrap (staticNum1Trap , this);
  _right->setAtrap (staticRightTrap, this);

  _left ->setupSenseFun (staticLeftSenseFun , this);
  _label->setupSenseFun (staticSenseFun     , this);
  _num1 ->setupSenseFun (staticSenseFun     , this);
  _of   ->setupSenseFun (staticSenseFun     , this);
  _num2 ->setupSenseFun (staticSenseFun     , this);
  _right->setupSenseFun (staticRightSenseFun, this);
}



//-------------------------- make ----------------------------//
//-------------------------- make ----------------------------//
//-------------------------- make ----------------------------//

         // public.
         // this function needed only to remove tab group.

Widget SL2Arrows::make(Widget p)
{
  if(!made())
     {
     Widget w = SLSmartForm::make(p);
     XmRemoveTabGroup(w);     
     }
  makeChildren();
  return topWidget();
}



//----------- register traps and update functions ------------//
//----------- register traps and update functions ------------//
//----------- register traps and update functions ------------//

     // public.
     // register user-written functions to be called by this object.

void SL2Arrows::registerNum1Trap    (ArrowsLongTrap *trap, void *data)
{
  _num1_trap  = trap;
  _num1_tdata = data;
}


void SL2Arrows::registerLabelUpdate (ArrowsCharFun *fun, void *data)
{
  _label_fun   = fun;
  _label_udata = data;
  _label->setupCvarFun(_label_fun, _label_udata);
}


void SL2Arrows::registerNum1Update  (ArrowsLongFun *fun, void *data)
{
  _num1_fun   = fun;
  _num1_udata = data;
  _num1->setupIvarFun(_num1_fun, _num1_udata);
}


void SL2Arrows::registerNum2Update  (ArrowsLongFun *fun, void *data)
{
  _num2_fun   = fun;
  _num2_udata = data;
  _num2->setupIvarFun(_num2_fun, _num2_udata);
}


void SL2Arrows::registerSenseUpdate (ArrowsLongFun *fun, void *data)
{
  _sense_fun   = fun;
  _sense_udata = data;
}



//----------------------- static traps ------------------------//
//----------------------- static traps ------------------------//
//----------------------- static traps ------------------------//

          // private.

void SL2Arrows::staticLeftTrap(void *data, long /*ident*/)
{
  SL2Arrows *THIS = (SL2Arrows*)data;
  if(!THIS->_num1_fun) return;
  long value = THIS->_num1_fun(THIS->_num1_udata);
  staticNum1Trap(data, 0, value, value - 1);
}


void SL2Arrows::staticRightTrap(void *data, long /*ident*/)
{
  SL2Arrows *THIS = (SL2Arrows*)data;
  if(!THIS->_num1_fun) return;
  long value = THIS->_num1_fun(THIS->_num1_udata);
  staticNum1Trap(data, 0, value, value + 1);
}


void SL2Arrows::staticNum1Trap(void *data, long /*ident*/,
                               long /*oldvar*/, long newvar)
{
  SL2Arrows *THIS = (SL2Arrows*)data;
  if(newvar < 1) return;
  if(!THIS->_num1_trap) return;
  if(THIS->_num2_fun)
      {
      long max_value = THIS->_num2_fun(THIS->_num2_udata);
      if(newvar > max_value) return;
      }
  THIS->_num1_trap(THIS->_num1_tdata, newvar);
}



/*
//--------------- virtual functions to override ---------------//
//--------------- virtual functions to override ---------------//
//--------------- virtual functions to override ---------------//

     // protected.

void SL2Arrows::virtualNum1Trap(long value)
{
  if(
}
*/



//--------------- static sense update functions ----------------//
//--------------- static sense update functions ----------------//
//--------------- static sense update functions ----------------//

       // private.

long SL2Arrows::staticSenseFun(void *data)
{
  SL2Arrows *THIS = (SL2Arrows*)data;
  if(THIS->_num2_fun)
      {
      long max_value = THIS->_num2_fun(THIS->_num2_udata);
      if(max_value <= 1) return FALSE;
      }
  if(!THIS->_sense_fun) return TRUE;
  return THIS->_sense_fun(THIS->_sense_udata);
}



long SL2Arrows::staticLeftSenseFun(void *data)
{
  SL2Arrows *THIS = (SL2Arrows*)data;
  if(THIS->_num1_fun)
      {
      long value = THIS->_num1_fun(THIS->_num1_udata);
      if(value <= 1) return FALSE;
      }
  return staticSenseFun(data);
}



long SL2Arrows::staticRightSenseFun(void *data)
{
  SL2Arrows *THIS = (SL2Arrows*)data;
  if(THIS->_num1_fun && THIS->_num2_fun)
      {
      long value     = THIS->_num1_fun(THIS->_num1_udata);
      long max_value = THIS->_num2_fun(THIS->_num2_udata);
      if(value >= max_value) return FALSE;
      }
  return staticSenseFun(data);
}




//------------------------ end ----------------------------------//
//------------------------ end ----------------------------------//
//------------------------ end ----------------------------------//
