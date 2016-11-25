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

//---------------------- sl_range_select.cc -----------------------//
//---------------------- sl_range_select.cc -----------------------//
//---------------------- sl_range_select.cc -----------------------//

//         implementation file for the SLRangeSelect class
//              derived from the SLSmartForm class
//                       subdirectory sl


#include "sl/sl_range_select.hh"
#include "sl/slp_push.hh"
#include "sl/slp_text.hh"
#include "sl/sl2_scale.hh"
#include "named_constants.h"
#include <stdio.h>
//#include <iostream.h>
//#include "cprim.h"


//------------------- constructors and destructor -----------------//
//------------------- constructors and destructor -----------------//
//------------------- constructors and destructor -----------------//

SLRangeSelect::SLRangeSelect(SLDelay *slparent, char *name, HelpCtx hctx)
        : SLSmartForm(slparent, name, hctx)
{
  constructorHelper();
}


SLRangeSelect::SLRangeSelect(Widget wparent, char *name, HelpCtx hctx)
        : SLSmartForm(wparent, name, hctx)
{
  constructorHelper();
}


SLRangeSelect::~SLRangeSelect(void)
{
}


//--------------------- constructor helper ---------------------//
//--------------------- constructor helper ---------------------//
//--------------------- constructor helper ---------------------//

void SLRangeSelect::constructorHelper()
{
  _value   =    1;
  _minimum =    1;
  _maximum =    1;
  _page    =   10;
  _trap    = NULL;
  _data    = NULL;

  _scale   = new SL2Scale   (this, "scale", 0, "",
                                       SLpScale::_HORIZONTAL, TRUE);

  _control = new SLSmartForm(this, "control");
  _control->showEvenSpacing();

  _min     = new SLpPush(_control, "12345678<-Min" , -2);
  _prev    = new SLpPush(_control, "<-Prev", -1);
  _valtext = new SLpText(_control, "valtext", 0, SLpText::_LONG, 8);
  _next    = new SLpPush(_control, "Next->",  1);
  _max     = new SLpPush(_control, "Max->12345678" ,  2);

  _min    ->setAtrap      (pushTrap, this);
  _prev   ->setAtrap      (pushTrap, this);
  _next   ->setAtrap      (pushTrap, this);
  _max    ->setAtrap      (pushTrap, this);
  _valtext->setItrap      (scaleTrap, this);
  _valtext->setupSenseFun (senseFunValue, this);
  _min    ->setupSenseFun (senseFunLower, this);
  _prev   ->setupSenseFun (senseFunLower, this);
  _next   ->setupSenseFun (senseFunUpper, this);
  _max    ->setupSenseFun (senseFunUpper, this);
  _valtext->setupIvarPoint(&_value);

  _scale->setItrap(scaleTrap, this);
  _scale->setupScalePoint(&_value);
  _scale->setupIpageValue(_page);

  attach(_min    , NULL, NULL, _control, _control);
  attach(_prev   , NULL, NULL, _control, _control);
  attach(_valtext, NULL, NULL, _control, _control);
  attach(_next   , NULL, NULL, _control, _control);
  attach(_max    , NULL, NULL, _control, _control);

  attach(_control, this, this, this   ,  NULL,  0,  0,  0, 0);
  attach(_scale  , this, this, _control, this,  0,  0, 10, 0);
  if(made()) make();
  updateChildren();
}


//--------------------- set values --------------------------//
//--------------------- set values --------------------------//
//--------------------- set values --------------------------//

void SLRangeSelect::setValues(long value, long minimum, long maximum,
                               long page)
{
  _minimum = MinimumValue  (minimum, maximum);
  _maximum = MaximumValue  (minimum, maximum);
  _valtext->setupIminValue(_minimum);
  _valtext->setupImaxValue(_maximum);
  _scale  ->setupIminValue(_minimum);
  _scale  ->setupImaxValue(_maximum);
  _value   = ConstrainValue(value, _minimum, _maximum);
  if(page > 0)
       {
       _page = page;
       _scale->setupIpageValue(_page);
       }
  static char min_label[40];
  static char max_label[40];
  sprintf(min_label, "%d<-Min", _minimum);
  sprintf(max_label, "Max->%d", _maximum);
  _min->setupLabelValue(min_label);
  _max->setupLabelValue(max_label);
  updateChildren();
}


void SLRangeSelect::setValue(long value)
{
  setValues(value, _minimum, _maximum);
}


void SLRangeSelect::setMinMax(long minimum, long maximum)
{
  setValues(_value, minimum, maximum);
}


void SLRangeSelect::setPage(long page)
{
  setValues(_value, _minimum, _maximum, page);
}


void SLRangeSelect::setNoValues()
{
  setValues(1, 1, 1);
}



//-------------------------- traps ------------------------------//
//-------------------------- traps ------------------------------//
//-------------------------- traps ------------------------------//

void SLRangeSelect::scaleTrap(void *data, long /* ident  */,
                                          long /* oldvar */,
                                          long /* newvar */)
{
  SLRangeSelect *range = (SLRangeSelect*)data;
  if(range->_trap) range->_trap(range->_data, range->_value);
}


void SLRangeSelect::pushTrap(void *data, long ident)
{
  SLRangeSelect *range = (SLRangeSelect*)data;
  long last_first = range->_maximum - range->_page + 1;
  switch(ident)
       {
       case -2: range->_value =  range->_minimum;
                break;
       case -1: if(range->_value > last_first)
                     {
                     range->_value = last_first;
                     }
                else
                     {
                     range->_value -= range->_page;
                     }
                break;
       case  1: if(range->_value < last_first - range->_page)
                     {
                     range->_value += range->_page;
                     break;
                     }
                // fall thru to case 2
       case  2: if(range->_value < last_first)
                   range->_value = last_first;
                else
                   range->_value = range->_maximum;
                break;
       default: break;
       }
  range->_value = ConstrainValue(range->_value,
                                 range->_minimum, range->_maximum);
  if(range->_trap) range->_trap(range->_data, range->_value);
}


long SLRangeSelect::senseFunValue(void *data)
{
  SLRangeSelect *range = (SLRangeSelect*)data;
  return (range->_maximum > range->_minimum);
}


long SLRangeSelect::senseFunLower(void *data)
{
  SLRangeSelect *range = (SLRangeSelect*)data;
  return (range->_minimum < range->_value);
}


long SLRangeSelect::senseFunUpper(void *data)
{
  SLRangeSelect *range = (SLRangeSelect*)data;
  return (range->_maximum > range->_value);
}



//-------------------------- end -------------------------------//
//-------------------------- end -------------------------------//
//-------------------------- end -------------------------------//
