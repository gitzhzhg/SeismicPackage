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

//---------------------- radio_list.cc --------------------------//
//---------------------- radio_list.cc --------------------------//
//---------------------- radio_list.cc --------------------------//

//     implementation file for RadioElement and RadioList
//          derived from Element and BaseLinkedList
//                      subdirectory sl


#include <string.h>
#include <iostream.h>
#include "sl/radio_list.hh"
#include "sl/slp_radio.hh"
#include "named_constants.h"



//--------------------- RadioElement ------------------------//
//--------------------- RadioElement ------------------------//
//--------------------- RadioElement ------------------------//

RadioElement::RadioElement(SLpRadio *gui)
         : Element(),
               _gui(gui)
{
}


int RadioElement::operator ==(void * const gui) const
{
  return (int)((SLpRadio*)gui == _gui);
}


void RadioElement::print() const
{
  cout << " " << _gui << endl;
}



//-------------------- convenient macro ----------------------//
//-------------------- convenient macro ----------------------//
//-------------------- convenient macro ----------------------//

#define DESCEND_LIST                                        \
         for( SLpRadio *gui = top(); gui; gui = next() )


//-------------------- RadioList ----------------------//
//-------------------- RadioList ----------------------//
//-------------------- RadioList ----------------------//

RadioList::RadioList(void)
      : BaseLinkedList()
{
}


RadioList::~RadioList(void)
{
  DESCEND_LIST { gui->radioListGoingAway(); delete gui; }
}


void RadioList::add(SLpRadio *gui)
{
  Element *element = new RadioElement(gui);
  BaseLinkedList::add(element);
}


void RadioList::remove(SLpRadio *gui)
{
  BaseLinkedList::remove((void*)gui);
}


SLpRadio *RadioList::find(SLpRadio *gui)
{
  Element *element = BaseLinkedList::find((void*)gui);
  if(!element) return NULL;
  return ((RadioElement*)element)->_gui;
}


#define SHORTHAND(top)                       \
SLpRadio *RadioList::top()                   \
{                                            \
  Element *element = BaseLinkedList::top();  \
  if(!element) return NULL;                  \
  return ((RadioElement*)element)->_gui;     \
}

  SHORTHAND(top)
  SHORTHAND(bottom)
  SHORTHAND(next)
  SHORTHAND(prev)
  SHORTHAND(current)



//-------- create radio button and add it to linked list -------//
//-------- create radio button and add it to linked list -------//
//-------- create radio button and add it to linked list -------//

SLpRadio *RadioList::addRadio(SLDelay *slparent,
                                char *name, long ident, char *label)
{
  DESCEND_LIST { if(AbsoluteValue(gui->id()) ==
                    AbsoluteValue(ident)) cout <<
         "called addRadio with duplicate ident " << ident << " or " <<
                                                   -ident << endl; }
  SLpRadio *gui2 = new SLpRadio(slparent, name, ident, label, this);
  return gui2;
}


SLpRadio *RadioList::addRadio(Widget wparent,
                                char *name, long ident, char *label)
{
  DESCEND_LIST { if(AbsoluteValue(gui->id()) ==
                    AbsoluteValue(ident)) cout <<
         "called addRadio with duplicate ident " << ident << endl; }
  SLpRadio *gui2 = new SLpRadio(wparent, name, ident, label, this);
  return gui2;
}



//------- remove radio button from linked list and delete it ------//
//------- remove radio button from linked list and delete it ------//
//------- remove radio button from linked list and delete it ------//

void RadioList::removeRadio(long ident)
{
  SLpRadio *gui = findRadioByIdent(ident);
  if(gui) delete gui;
  else cout << "called removeRadio() with invalid ident" << endl;
}



//--------------------------- get value ------------------------------//
//--------------------------- get value ------------------------------//
//--------------------------- get value ------------------------------//

long RadioList::ivar()
{
/*
  SLpRadio *gui = top();
  if(gui) return gui->ivar();
*/
  DESCEND_LIST { if(gui->ivar()) return gui->ivar(); }
  return 0;
}


long RadioList::oldIvar()
{
/*
  SLpRadio *gui = top();
  if(gui) return gui->oldIvar();
*/
  DESCEND_LIST { if(gui->oldIvar()) return gui->oldIvar(); }
  return 0;
}


//------------ set and setup value on all radio buttons ------------//
//------------ set and setup value on all radio buttons ------------//
//------------ set and setup value on all radio buttons ------------//

void RadioList::setIvar(long x)
{
  DESCEND_LIST { gui->setIvar(x); }
}


void RadioList::setupIvarValue(long x)
{
  DESCEND_LIST { gui->setupIvarValue(x); }
}


void RadioList::setupIvarPoint(long*x)
{
  DESCEND_LIST { gui->setupIvarPoint(x); }
}


void RadioList::setupIvarFun(long(*fun)(void*), void*data)
{
  DESCEND_LIST { gui->setupIvarFun(fun, data); }
}


void RadioList::setIvarSystemDefault(long x)
{
  DESCEND_LIST { gui->setIvarSystemDefault(x); }
}


//---------------- set trap on all radio buttons -------------//
//---------------- set trap on all radio buttons -------------//
//---------------- set trap on all radio buttons -------------//

void RadioList::setFocusinTrap (FocusFun *trap, void *data)
{
  DESCEND_LIST { gui->setFocusinTrap(trap, data); }
}


void RadioList::setFocusoutTrap(FocusFun *trap, void *data)
{
  DESCEND_LIST { gui->setFocusoutTrap(trap, data); }
}


void RadioList::setFocusinTrap (FocusFunF *trap)
{
  DESCEND_LIST { gui->setFocusinTrap(trap); }
}


void RadioList::setFocusoutTrap(FocusFunF *trap)
{
  DESCEND_LIST { gui->setFocusoutTrap(trap); }
}


void RadioList::setNotify   (SLDelay *target)
{
  DESCEND_LIST { gui->setNotify(target); }
}


void RadioList::setItrap    (ItrapFun *trap, void *data)
{
  DESCEND_LIST { gui->setItrap(trap, data); }
}


void RadioList::setItrap    (ItrapFunF *trap)
{
  DESCEND_LIST { gui->setItrap(trap); }
}



//------------- find radio button by ident ------------------//
//------------- find radio button by ident ------------------//
//------------- find radio button by ident ------------------//

SLpRadio *RadioList::findRadioByIdent(long ident)
{
  DESCEND_LIST
       {
       if(gui->id() == ident) return gui;
       }
  return NULL;
}



//--------------- set and setup label (by ident) -------------//
//--------------- set and setup label (by ident) -------------//
//--------------- set and setup label (by ident) -------------//

void RadioList::setLabel(long ident, char* value)
{
  SLpRadio *gui = findRadioByIdent(ident);
  if(gui) gui->setCvar(value);
  else cout << "called RadioList::setLabel() with invalid ident" << endl;
}


void RadioList::setupLabelValue(long ident, char* value)
{
  SLpRadio *gui = findRadioByIdent(ident);
  if(gui) gui->setupCvarValue(value);
  else cout << "called RadioList::setupLabelValue() with invalid ident" << endl;
}


void RadioList::setupLabelPoint(long ident, char *point, long nvar)
{
  SLpRadio *gui = findRadioByIdent(ident);
  if(gui) gui->setupCvarPoint(point, nvar);
  else cout << "called RadioList::setupLabelPoint() with invalid ident" << endl;
}


void RadioList::setupLabelFun(long ident,
                                   char* (*fun)(void*), void *data)
{
  SLpRadio *gui = findRadioByIdent(ident);
  if(gui) gui->setupCvarFun(fun, data);
  else cout << "called RadioList::setupLabelFun() with invalid ident" << endl;
}


//--------------- set and setup sense (by ident) ------------//
//--------------- set and setup sense (by ident) ------------//
//--------------- set and setup sense (by ident) ------------//

void RadioList::setSense(long ident, long value)
{
  SLpRadio *gui = findRadioByIdent(ident);
  if(gui) gui->setSense(value);
  else cout << "called RadioList::setSense() with invalid ident" << endl;
}


void RadioList::setupSenseValue(long ident, long value)
{
  SLpRadio *gui = findRadioByIdent(ident);
  if(gui) gui->setupSenseValue(value);
  else cout << "called RadioList::setupSenseValue() with invalid ident" << endl;
}


void RadioList::setupSensePoint(long ident, long *point)
{
  SLpRadio *gui = findRadioByIdent(ident);
  if(gui) gui->setupSensePoint(point);
  else cout << "called RadioList::setupSensePoint() with invalid ident" << endl;
}


void RadioList::setupSenseFun(long ident,
                                   long (*fun)(void*), void *data)
{
  SLpRadio *gui = findRadioByIdent(ident);
  if(gui) gui->setupSenseFun(fun, data);
  else cout << "called RadioList::setupSenseFun() with invalid ident" << endl;
}


//---------- manage and unmanage (by ident) -----------------//
//---------- manage and unmanage (by ident) -----------------//
//---------- manage and unmanage (by ident) -----------------//

void RadioList::manageRadio(long ident)
{
  SLpRadio *gui = findRadioByIdent(ident);
  if(gui) gui->manage();
  else cout << "called manageRadio() with invalid ident" << endl;
}


void RadioList::unmanageRadio(long ident)
{
  SLpRadio *gui = findRadioByIdent(ident);
  if(gui) gui->unmanage();
  else cout << "called unmanageRadio() with invalid ident" << endl;
}



//------------------------- end --------------------------//
//------------------------- end --------------------------//
//------------------------- end --------------------------//

